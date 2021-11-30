library(shiny)
library(tidyverse)
library(tmap)
library(plotly)
library(leaflet)
library(DT)
library(shinyWidgets)
library(shinycssloaders)

# **************************************** outside app ****************************************

# ==================== read in data ====================
load(here::here("data/ida.RData"))
load(here::here("data/eda.RData"))

# --- set spinner colour to red
options(spinner.color = "#b22222")

# ========== include `bf_season` in data set
# create storage vector
bf_season <- numeric(nrow(ignition_rasterize_cluster_sf_month))

# start from group 1
group <- 1
current_year <- min(as.numeric(as.character(ignition_rasterize_cluster_sf_month$year))) # start from minimum year

# --- for loop
# •if following year & month == 10; group + 1
# -> i.e. group by bushfire season (10(Oct) to 3(March))
for(i in seq_along(ignition_rasterize_cluster_sf_month$year)){

    # group + 1; if its next year & month == 10 (October)
    if(current_year == ignition_rasterize_cluster_sf_month$year[i] - 1 & ignition_rasterize_cluster_sf_month$month[i]  == 10){
        group <- group + 1
        current_year <- current_year + 1
    }

    bf_season[i] <- group
}

ignition_rasterize_cluster_sf_month <- cbind(ignition_rasterize_cluster_sf_month, bf_season) %>%
    group_by(bf_season) %>%
    mutate(bf_season = paste0(min(year), "-", min(year + 1)), # add `bf_season` column
           id = as_factor(id)) %>% # coerce `id` to factor
    ungroup()

# **************************************** define UI ****************************************
ui <- fluidPage(

    # add app title
    tags$h1("Bushfire Risk Information",
            style = "font-family: Impact; color: #1e90ff; font-size: 60px",
            align = "center"),
    br(),

    sidebarLayout(
        # === column for user interactivity
        sidebarPanel(width = 2,
            # --- user; `month` checkbox buttons
            shinyWidgets::checkboxGroupButtons(inputId = "month_selected",
                                               label = "Month(s)",
                                               choiceNames = c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar"),
                                               choiceValues = ignition_rasterize_cluster_sf_month %>% distinct(month) %>% pull(month),
                                               selected = ignition_rasterize_cluster_sf_month %>% distinct(month) %>% pull(month),
                                               status = "danger",
                                               checkIcon = list(yes = icon("ok",
                                                                           lib = "glyphicon"),
                                                                no = icon("remove",
                                                                          lib = "glyphicon")),
                                               direction = "horizontal",
                                               size = "lg"), # large size

            shinyWidgets::checkboxGroupButtons(inputId = "bf_season_selected",
                                               label = "Bushfire season(s)",
                                               choices = ignition_rasterize_cluster_sf_month %>% distinct(bf_season) %>% pull(bf_season), # unique bushfire seasons
                                               selected = ignition_rasterize_cluster_sf_month %>% distinct(bf_season) %>% pull(bf_season),
                                               status = "danger",
                                               checkIcon = list(yes = icon("ok",
                                                                           lib = "glyphicon"),
                                                                no = icon("remove",
                                                                          lib = "glyphicon")),
                                               direction = "horizontal",
                                               size = "lg")
        ),

        # === column for leaflet map output
        mainPanel(width = 10,
            leaflet::leafletOutput("map",
                                   height = 500) %>%
                shinycssloaders::withSpinner()
        )
    ),
        br(), br(),

    # ========== DT::datatable ==========
    fluidRow(
        DT::DTOutput("datatable") %>%
            shinycssloaders::withSpinner()
    ),
    br(), br(),



    # ========== plotly ==========
    fluidRow(
        # === column for `fire_count` vs. `bf_season` bar plotly
        column(width = 4,
               plotly::plotlyOutput("fire_bf_season_plotly") %>%
                   shinycssloaders::withSpinner()
               ),

        column(width = 4,
               plotly::plotlyOutput("fire_month_plotly") %>%
                   shinycssloaders::withSpinner()
               ),

        column(width = 4,
               plotly::plotlyOutput("max_temp_plotly") %>%
                   shinycssloaders::withSpinner())
    )

)

# **************************************** define server ****************************************
server <- function(input, output) {

    # ==================== create leaflet map ====================

    # === create reactive data

    # --- for overlay groups (points)
    bf_season_2016_2017_sf <- shiny::reactive({
        cluster_16_21_sf %>%
            filter(bf_season == "2016-2017",
                   month %in% input$month_selected)
    })

    bf_season_2017_2018_sf <- shiny::reactive({
        cluster_16_21_sf %>%
            filter(bf_season == "2017-2018",
                   month %in% input$month_selected)
    })

    bf_season_2018_2019_sf <- shiny::reactive({
        cluster_16_21_sf %>%
            filter(bf_season == "2018-2019",
                   month %in% input$month_selected)
    })

    bf_season_2019_2020_sf <- shiny::reactive({
        cluster_16_21_sf %>%
            filter(bf_season == "2019-2020",
                   month %in% input$month_selected)
    })

    bf_season_2020_2021_sf <- shiny::reactive({
        cluster_16_21_sf %>%
            filter(bf_season == "2020-2021",
                   month %in% input$month_selected)
    })

    # --- for grid cell (polygons)
    ignition_rasterize_cluster_bf_season_rct <- shiny::reactive({
        ignition_rasterize_cluster_sf_month_temp <- ignition_rasterize_cluster_sf_month %>%
            # filter to user selected `bf_season` & `month`
            dplyr::filter(bf_season %in% input$bf_season_selected,
                          month %in% input$month_selected) %>%
            # sum up `fire_count`; for each `id` (grid cell)
            group_by(id) %>%
            summarise(fire_count = sum(fire_count))

        vic_raster_crop_sf %>% # `sf` object with `id` & `geometry`
            mutate(id = as_factor(id)) %>%
            # join with predicted data
            left_join(., ignition_rasterize_cluster_sf_month_temp)
    })



    # --- create leaflet map start
    output$map <- leaflet::renderLeaflet({

        # create base map
        base_map <- leaflet(options = leafletOptions(
            # set min & max zoom
            minZoom = 6.45,
            maxZoom = 11
        )) %>%
            # add provider Tiles
            addProviderTiles(provider = "OpenStreetMap",
                             group = "OSM") %>%
            addProviderTiles(provider = "CartoDB",
                             group = "Carto") %>%
            # set map to within Victoria bounding box
            leaflet::fitBounds(lng1 = 140.9617,
                               lng2 = 149.9763,
                               lat1 = -39.13396,
                               lat2 = -33.99605) %>%
            # set max bounds of map
            # disable dragging map; too far out of bounds
            setMaxBounds(lng1 = 140.9617 - 0.4,
                         lng2 = 149.9763 + 0.4,
                         lat1 = -39.13396 - 0.4,
                         lat2 = -33.99605 + 0.4)

        # --- palette for polygon data
        pal <- leaflet::colorNumeric(palette = "YlOrRd",
                                     domain = c(1:50), # scale
                                     reverse = F)


        base_map %>%
            # add outline of Victoria
            addPolylines(data = vic_map_sf,
                         color = "black",
                         weight = 3) %>%

            # add gridded cell
            addPolygons(data = ignition_rasterize_cluster_bf_season_rct(),
                        opacity = 0.1,
                        layerId = ~id,
                        fillOpacity = 0.4,
                        color = ~pal(fire_count), # fill by `fire_count`; using above palette
                        popup = ~paste0("<b> id: ", id, "</b>", "<br/>",
                                        "number of ignitions: ", fire_count)) %>% # click pop-up texts

            # --- add circle markers; each `bf_season` as a layer
            # 2016-2017
            addCircleMarkers(data = bf_season_2016_2017_sf(),
                             radius = 2,
                             fillOpacity = 0.01,
                             color = "grey",
                             group = "2016-2017") %>%
            # 2017-2018
            addCircleMarkers(data = bf_season_2017_2018_sf(),
                             radius = 2,
                             fillOpacity = 0.01,
                             color = "grey",
                             group = "2017-2018") %>%
            # 2018-2019
            addCircleMarkers(data = bf_season_2018_2019_sf(),
                             radius = 2,
                             fillOpacity = 0.01,
                             color = "grey",
                             group = "2018-2019") %>%
            # 2019-2020
            addCircleMarkers(data = bf_season_2019_2020_sf(),
                             radius = 2,
                             fillOpacity = 0.01,
                             color = "grey",
                             group = "2019-2020") %>%
            # 2020-2021
            addCircleMarkers(data = bf_season_2018_2019_sf(),
                             radius = 2,
                             fillOpacity = 0.01,
                             color = "grey",
                             group = "2020-2021") %>%

            # add user controls; for overlay groups
            addLayersControl(baseGroups = c("OSM", "Carto"),
                             overlayGroups = c("2016-2017", "2017-2018", "2018-2019", "2019-2020", "2020-2021"),
                             options = layersControlOptions(collapsed = FALSE)) %>%

            # uncheck groups
            hideGroup(c("2017-2018", "2018-2019", "2019-2020", "2020-2021")) %>%

            # add & customise legend
            addLegend(pal = pal,
                      values = c(1:50),
                      title = "ignition counts",
                      position = "bottomright") %>%

            # add Reset button
            leaflet.extras::addResetMapButton()

    })

    # --- df based on user click
    model_df_id <- shiny::eventReactive(input$map_shape_click, { # invalidated; each time; user click grid cell
        # based on user map click
        model_df3 %>%
            filter(id == input$map_shape_click$id)
    })

    # --- DT::datatable; based on cell clicked
    output$datatable <- DT::renderDT(
        model_df_id() %>%
            DT::datatable(options = list(scrollX = T,
                                         pageLength = 5)) %>%
            DT::formatRound(columns = 6:33,
                            digits = 3)
    )

    # --- plot `fire_count` vs. `bf_season`; bar plotly
    output$fire_bf_season_plotly <- plotly::renderPlotly({
        model_df_id() %>%
            # sum fires over bushfire season
            group_by(bf_season) %>%
            summarise(fire_count = sum(fire_count)) %>%
            # plot_ly
            plotly::plot_ly(x = ~bf_season,
                            y = ~fire_count,
                            type = "bar",
                            text = ~fire_count,
                            textposition = "auto") %>%
            plotly::layout(
                # plot title
                title = "number of fires ignitions in bushfire season",

                # x-axis
                xaxis = list(type = "category", # categorical variable
                             title = "bushfire season"), # axis title

                # y-axis
                yaxis = list(title = "number of fire ignitions",
                             type = "numeric",
                             range = c(0, 40))
            )
    })


    # --- plot `fire_count` vs. `month`; bar plotly
    output$fire_month_plotly <- plotly::renderPlotly({
        model_df_id() %>%
            group_by(bf_season,
                     month) %>%
            summarise(fire_count = sum(fire_count)) %>%
            # wide form (each `month` have own column)
            pivot_wider(names_from = month,
                        values_from = fire_count) %>%
            # plot
            plot_ly() %>%
            # add bar for each month
            add_trace(x = ~bf_season,
                      y = ~`10`,
                      name = "Oct",
                      text = ~`10`,
                      textposition = "outside") %>%
            add_trace(x = ~bf_season,
                      y = ~`11`,
                      name = "Nov",
                      text = ~`11`,
                      textposition = "outside") %>%
            add_trace(x = ~bf_season,
                      y = ~`12`,
                      name = "Dec",
                      text = ~`12`,
                      textposition = "outside") %>%
            add_trace(x = ~bf_season,
                      y = ~`1`,
                      name = "Jan",
                      text = ~`1`,
                      textposition = "outside") %>%
            add_trace(x = ~bf_season,
                      y = ~`2`,
                      name = "Feb",
                      text = ~`2`,
                      textposition = "outside") %>%
            add_trace(x = ~bf_season,
                      y = ~`3`,
                      name = "Mar",
                      text = ~`3`,
                      textposition = "outside") %>%
            plotly::layout(barmode = "group",
                           title = "fire ignitions against months in bushfire season",
                           yaxis = list(title = "number of ignitions",
                                        type = "numeric",
                                        range = c(0, 40)),
                           xaxis = list(type = "category",
                                        title = "bushfire season | months"),
                           bargap = 0.05,
                           bargroupgap = 0.25)
    })

    # --- plot
    output$max_temp_plotly <- plotly::renderPlotly({
        model_df_id() %>%
            # extract date
            mutate(date = paste(year, month, sep = "") %>% lubridate::ym(),
                   .after = "month") %>%
            plot_ly(x = ~date,
                    y = ~max_temp,
                    split = ~bf_season,
                    type = "scatter",
                    mode = "lines+markers") %>%
            plotly::layout(title = "max temperature over bushfire season", # title
                           # set tick labels
                           xaxis = list(tickformat = "%b\n%Y",
                                        ticklabelmode = "period",
                                        dtick = "M12"), # every 12 months
                           # legend
                           legend = list(title = list(text = "bushfire_season"))
            )
    })
}

# Run the application
shinyApp(ui = ui,
         server = server)
