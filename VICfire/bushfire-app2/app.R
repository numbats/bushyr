library(shiny)
library(tidyverse)
library(DT)
library(leaflet)
library(plotly)
library(raster)
library(tmap)
library(shinyalert)
library(shinycssloaders)
library(shinyWidgets)

# **************************************** outside app ****************************************

# ==================== read in data ====================
# load(here::here("data/ida.RData"))
# load(here::here("data/eda.RData"))

select <- dplyr::select

model_df3 <- readr::read_rds("model_df3.rds")
ignition_rasterize_cluster_sf_month <- readr::read_rds("ignition_rasterize_cluster_sf_month.rds") %>%
    arrange(year, month) # arrange by year, month; to run loop to compute `bf_season`
cluster_16_21_sf <- readr::read_rds("cluster_16_21_sf.rds")


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

    # --- use `shinyalert`; for information modal
    shinyalert::useShinyalert(),

    # --- set background colour
    shinyWidgets::setBackgroundColor(color = "#FFF5EE"),

    # add app title
    tags$h1("Bushfire Risk Historical Information",
            style = "font-family: Impact; color: #1e90ff; font-size: 60px",
            align = "center"),
    br(),

    shiny::fluidRow(
        # === column for user interactivity
        shiny::column(width = 2,

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
                      # --- user; `bf_season` checkbox buttons
                      shinyWidgets::checkboxGroupButtons(inputId = "bf_season_selected",
                                                         label = "Bushfire season(s)",
                                                         choices = ignition_rasterize_cluster_sf_month %>% distinct(bf_season) %>% pull(bf_season), # unique bushfire seasons
                                                         selected = c("2016-2017"),
                                                         status = "danger",
                                                         checkIcon = list(yes = icon("ok",
                                                                                     lib = "glyphicon"),
                                                                          no = icon("remove",
                                                                                    lib = "glyphicon")),
                                                         direction = "horizontal",
                                                         size = "lg"),
                      br(),

                      # --- information button
                      shinyWidgets::actionBttn(inputId = "info_bttn2",
                                               label = "",
                                               style = "material-circle",
                                               color = "danger",
                                               icon = icon("info")),

                      br(), br(), br(),
                      # --- download button
                      downloadBttn(outputId = "full_data_download",
                                   label = "download full data as csv"),


        ),

        # === main column for leaflet map output
        shiny::column(width = 8,
                      leaflet::leafletOutput("map",
                                             height = 1000) %>%
                          shinycssloaders::withSpinner(),

                      shiny::h2(shiny::textOutput(outputId = "cell_id_text3")),

                      br(), br(), br(),

                      # --- DT::datatable output ---
                      DT::DTOutput("datatable_hist") %>%
                          shinycssloaders::withSpinner()
        ),

        # === right column; for plotly outputs
        shiny::column(width = 2,
                      style = "max-height: 85vh; overflow-y: auto;", # add vertical scroll bar

                      shiny::h2(shiny::textOutput(outputId = "cell_id_text2")),

                      # `fire_count` vs. `bf_season`
                      plotly::plotlyOutput("fire_bf_season_plotly") %>%
                          shinycssloaders::withSpinner(),

                      br(),

                      # `fire_count` vs. `month`
                      plotly::plotlyOutput("fire_month_plotly") %>%
                          shinycssloaders::withSpinner(),

                      br(),

                      # `daily_rain`
                      plotly::plotlyOutput("daily_rain_plotly") %>%
                          shinycssloaders::withSpinner(),

                      br(),

                      # `max_temp`
                      plotly::plotlyOutput("max_temp_plotly") %>%
                          shinycssloaders::withSpinner(),

                      br(),

                      # `et_short_crop`
                      plotly::plotlyOutput("et_short_crop_plotly") %>%
                          shinycssloaders::withSpinner(),

                      br(),

                      # `radiation`
                      plotly::plotlyOutput("radiation_plotly") %>%
                          shinycssloaders::withSpinner(),

                      br(),

                      # `rh`
                      plotly::plotlyOutput("rh_plotly") %>%
                          shinycssloaders::withSpinner(),

                      br(),

                      # `si10`
                      plotly::plotlyOutput("si10_plotly") %>%
                          shinycssloaders::withSpinner(),

                      br(),

                      plotly::plotlyOutput("s0_pct_plotly") %>%
                          shinycssloaders::withSpinner(),


        )
    )
)

# **************************************** define server ****************************************
server <- function(input, output) {

    shiny::observeEvent(input$info_bttn2, {
        # show a modal; when button pressed
        shinyalert::shinyalert(title = "The bushfire risk historical information map shows the historical fire ignitions and weather/climate variables from 2016-2021. These are used in predicting bushfire risk.",
                               size = "m", # size of modal
                               showConfirmButton = T, # no confirm button
                               closeOnEsc = T,
                               closeOnClickOutside = T,
                               type = "info") # info logo
    })

    # ==================== create leaflet map ====================

    # === `vic_map_sf`: for outline of polygon in app
    # Victoria map (sfdf MULTIPOLYGON)
    vic_map_sf <- ozmaps::ozmap_states %>%
        filter(NAME == "Victoria")

    # --- project crs
    vic_map_sf <- sf::st_transform(vic_map_sf,
                                   crs = 4326)

    # === create `vic_raster` object *can be toggled
    vic_raster <- raster::brick(
        # no. of rows & columns (directly linked to resolution of grid cell)
        nrows = 20,
        ncols = 20,

        # bbox (bounding box of Victoria)
        xmn = 140.9617,
        xmx = 149.9763,
        ymn = -39.13396,
        ymx = -33.99605,

        # crs
        crs = 4326,

        # set `raster` values (rowwise)
        # vals = seq(from = 1, to = 400000, by = 1000)
    )

    # set values (`id` each cell)
    vic_raster <- vic_raster %>%
        raster::setValues(values = seq(from = 1, to = 400, by = 1))

    # --- mask raster; to only Victorian map

    # change vic_map_sf to `sp` object
    # *`raster::crop` & `raster::mask`; NOT compatible with `sf` yet; so; need; change to `sp`
    vic_map_sp <- as(vic_map_sf,
                     Class = "Spatial")

    # mask (*think: crop to polygon shape) raster; to only Victorian map (`vic_map_sp`)
    vic_raster_crop <- vic_raster %>%
        raster::mask(mask = vic_map_sp)

    # convert `raster` -> `spdf` -> `sf`; to conduct spatial join
    vic_raster_crop_sf <- vic_raster_crop %>%
        setValues(1:400) %>% # set id values *values required to convert to `spdf`
        as(., "SpatialPolygonsDataFrame") %>%
        sf::st_as_sf() %>%
        rename(id = layer) # rename `layer` to `id`

    # === create reactive data

    # --- for overlay groups (points)
    sf::st_crs(cluster_16_21_sf) <- 4326 # project crs

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
            minZoom = 7.49,
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
                         weight = 3.5) %>%

            # add gridded cell
            addPolygons(data = ignition_rasterize_cluster_bf_season_rct(),
                        opacity = 0.1,
                        layerId = ~id,
                        fillOpacity = 0.4,
                        color = ~pal(fire_count), # fill by `fire_count`; using above palette
                        # click pop-up texts
                        popup = ~paste0("<h3 style='font-size:5; color:#1E90FF'><b> id: ", id, "</b></h3>", "<br/>",
                                        "<h4 style='font-size:5'><b>number of historical ignitions </b>", "= ", fire_count, "</h3>", "<br/>")) %>%

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


    output$cell_id_text2 <- shiny::renderText({

        click <- input$map_shape_click

        # if user hasn't clicked
        if(is.null(click)){
            paste("click on grid cell to show plots")
        }
        # if (user clicked on grid cell); print
        else{
            paste("Observed values across variable values for cell", input$map_shape_click$id)
        }
    })

    output$cell_id_text3 <- shiny::renderText({

        click <- input$map_shape_click

        # if user hasn't clicked
        if(is.null(click)){
            paste("click on grid cell to show table")
        }
        # if (user clicked on grid cell); print
        else{
            ""
        }
    })

    # --- DT::datatable; based on cell clicked
    output$datatable_hist <- DT::renderDT(
        model_df_id() %>%
            DT::datatable(options = list(scrollX = T, # scroll horizontal
                                         pageLength = 5, #
                                         dom = "Bfrtip", # dom options
                                         buttons = c("csv", "excel")), # include `csv` & `excel` buttons; allow user; download data
                          extensions = "Buttons", # add buttons
                          caption = htmltools::tags$caption(tags$h3(style = 'caption-side: top; text-align: left;',
                                                                    paste("cell", input$map_shape_click$id, "Historical Information (Tabular)")))) %>%
            DT::formatRound(columns = 6:33,
                            digits = 3)
    )

    # --- full data *to download
    output$full_data_download <- downloadHandler(
        filename = function(){
            paste0("data-", Sys.Date(), ".csv")
        },

        content = function(con){
            write.csv(model_df3, con)
        }
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
                            textposition = "auto",
                            hovertemplate = paste("bushfire season: %{x}",
                                                  "<br>fire ignitions: %{y}")) %>%
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
            summarise(fire_count = sum(fire_count, na.rm = T)) %>%
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
                      textposition = "outside",
                      hovertemplate = paste("bushfire season: %{x}",
                                            "<br>month: Oct",
                                            "<br>fire ignitions: %{y}")) %>%
            add_trace(x = ~bf_season,
                      y = ~`11`,
                      name = "Nov",
                      text = ~`11`,
                      textposition = "outside",
                      hovertemplate = paste("bushfire season: %{x}",
                                            "<br>month: Nov",
                                            "<br>fire ignitions: %{y}")) %>%
            add_trace(x = ~bf_season,
                      y = ~`12`,
                      name = "Dec",
                      text = ~`12`,
                      textposition = "outside",
                      hovertemplate = paste("bushfire season: %{x}",
                                            "<br>month: Dec",
                                            "<br>fire ignitions: %{y}")) %>%
            add_trace(x = ~bf_season,
                      y = ~`1`,
                      name = "Jan",
                      text = ~`1`,
                      textposition = "outside",
                      hovertemplate = paste("bushfire season: %{x}",
                                            "<br>month: Jan",
                                            "<br>fire ignitions: %{y}")) %>%
            add_trace(x = ~bf_season,
                      y = ~`2`,
                      name = "Feb",
                      text = ~`2`,
                      textposition = "outside",
                      hovertemplate = paste("bushfire season: %{x}",
                                            "<br>month: Feb",
                                            "<br>fire ignitions: %{y}")) %>%
            add_trace(x = ~bf_season,
                      y = ~`3`,
                      name = "Mar",
                      text = ~`3`,
                      textposition = "outside",
                      hovertemplate = paste("bushfire season: %{x}",
                                            "<br>month: Mar",
                                            "<br> fire ignitions: %{y}")) %>%
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

    # --- plot variable values
    # `daily_rain`
    output$daily_rain_plotly <- plotly::renderPlotly({
        model_df_id() %>%
            # extract date
            mutate(date = paste(year, month, sep = "") %>% lubridate::ym(),
                   .after = "month") %>%
            plot_ly(x = ~date,
                    y = ~daily_rain,
                    split = ~bf_season,
                    type = "scatter",
                    mode = "lines+markers",
                    hovertemplate = paste("bushfire season = %{x|%b}", # show month
                                          "<br>daily rain(mm): %{y:.2f}")) %>% # value in 2d.p.
            plotly::layout(title = "daily rain over bushfire seasons", # title
                           # set tick labels
                           xaxis = list(tickformat = "%Y",
                                        ticklabelmode = "period",
                                        dtick = "M12"), # every 12 months
                           # legend
                           legend = list(title = list(text = "bushfire_season"))
            )
    })

    # `max_temp`
    output$max_temp_plotly <- plotly::renderPlotly({
        model_df_id() %>%
            # extract date
            mutate(date = paste(year, month, sep = "") %>% lubridate::ym(),
                   .after = "month") %>%
            plot_ly(x = ~date,
                    y = ~max_temp,
                    split = ~bf_season,
                    type = "scatter",
                    mode = "lines+markers",
                    hovertemplate = paste("bushfire season = %{x|%b}", # show month
                                          "<br>max temperature(°C): %{y:.2f}")) %>% # value in 2d.p.
            plotly::layout(title = "max temperature over bushfire seasons", # title
                           # set tick labels
                           xaxis = list(tickformat = "%Y",
                                        ticklabelmode = "period",
                                        dtick = "M12"), # every 12 months
                           # legend
                           legend = list(title = list(text = "bushfire_season"))
            )
    })

    # `et_short_crop`
    output$et_short_crop_plotly <- plotly::renderPlotly({
        model_df_id() %>%
            # extract date
            mutate(date = paste(year, month, sep = "") %>% lubridate::ym(),
                   .after = "month") %>%
            plot_ly(x = ~date,
                    y = ~et_short_crop,
                    split = ~bf_season,
                    type = "scatter",
                    mode = "lines+markers",
                    hovertemplate = paste("bushfire season = %{x|%b}", # show month
                                          "<br>evapotranspiration rate(mm): %{y:.2f}")) %>% # value in 2d.p.
            plotly::layout(title = "evapotranspiration over bushfire seasons", # title
                           # set tick labels
                           xaxis = list(tickformat = "%Y",
                                        ticklabelmode = "period",
                                        dtick = "M12"), # every 12 months
                           # legend
                           legend = list(title = list(text = "bushfire_season"))
            )
    })

    # radiation
    output$radiation_plotly <- plotly::renderPlotly({
        model_df_id() %>%
            # extract date
            mutate(date = paste(year, month, sep = "") %>% lubridate::ym(),
                   .after = "month") %>%
            plot_ly(x = ~date,
                    y = ~radiation,
                    split = ~bf_season,
                    type = "scatter",
                    mode = "lines+markers",
                    hovertemplate = paste("bushfire season = %{x|%b}", # show month
                                          "<br>radiation(MJ/m^2): %{y:.2f}")) %>% # value in 2d.p.
            plotly::layout(title = "radiation over bushfire seasons", # title
                           # set tick labels
                           xaxis = list(tickformat = "%Y",
                                        ticklabelmode = "period",
                                        dtick = "M12"), # every 12 months
                           # legend
                           legend = list(title = list(text = "bushfire_season"))
            )
    })

    # radiation
    output$rh_plotly <- plotly::renderPlotly({
        model_df_id() %>%
            # extract date
            mutate(date = paste(year, month, sep = "") %>% lubridate::ym(),
                   .after = "month") %>%
            plot_ly(x = ~date,
                    y = ~rh,
                    split = ~bf_season,
                    type = "scatter",
                    mode = "lines+markers",
                    hovertemplate = paste("bushfire season = %{x|%b}", # show month
                                          "<br>relative humidity: %{y:.2f}")) %>% # value in 2d.p.
            plotly::layout(title = "relative humidity over bushfire seasons", # title
                           # set tick labels
                           xaxis = list(tickformat = "%Y",
                                        ticklabelmode = "period",
                                        dtick = "M12"), # every 12 months
                           # legend
                           legend = list(title = list(text = "bushfire_season"))
            )
    })

    output$rh_plotly <- plotly::renderPlotly({
        model_df_id() %>%
            # extract date
            mutate(date = paste(year, month, sep = "") %>% lubridate::ym(),
                   .after = "month") %>%
            plot_ly(x = ~date,
                    y = ~rh,
                    split = ~bf_season,
                    type = "scatter",
                    mode = "lines+markers",
                    hovertemplate = paste("bushfire season = %{x|%b}", # show month
                                          "<br>relative humidity(%): %{y:.2f}")) %>% # value in 2d.p.
            plotly::layout(title = "relative humidity over bushfire seasons", # title
                           # set tick labels
                           xaxis = list(tickformat = "%Y",
                                        ticklabelmode = "period",
                                        dtick = "M12"), # every 12 months
                           # legend
                           legend = list(title = list(text = "bushfire_season"))
            )
    })


    output$si10_plotly <- plotly::renderPlotly({
        model_df_id() %>%
            # extract date
            mutate(date = paste(year, month, sep = "") %>% lubridate::ym(),
                   .after = "month") %>%
            plot_ly(x = ~date,
                    y = ~si10,
                    split = ~bf_season,
                    type = "scatter",
                    mode = "lines+markers",
                    hovertemplate = paste("bushfire season = %{x|%b}", # show month
                                          "<br>10m wind speed(m/s): %{y:.2f}")) %>% # value in 2d.p.
            plotly::layout(title = "10m wind speed over bushfire seasons", # title
                           # set tick labels
                           xaxis = list(tickformat = "%Y",
                                        ticklabelmode = "period",
                                        dtick = "M12"), # every 12 months
                           # legend
                           legend = list(title = list(text = "bushfire_season"))
            )
    })

    output$s0_pct_plotly <- plotly::renderPlotly({
        model_df_id() %>%
            # extract date
            mutate(date = paste(year, month, sep = "") %>% lubridate::ym(),
                   .after = "month") %>%
            plot_ly(x = ~date,
                    y = ~s0_pct,
                    split = ~bf_season,
                    type = "scatter",
                    mode = "lines+markers",
                    hovertemplate = paste("bushfire season = %{x|%b}", # show month
                                          "<br>surface soil moisture(%): %{y:.2f}")) %>% # value in 2d.p.
            plotly::layout(title = "upper surface soil moisture over bushfire seasons", # title
                           # set tick labels
                           xaxis = list(tickformat = "%Y",
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
