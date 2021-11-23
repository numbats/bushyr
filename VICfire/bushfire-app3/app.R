library(shiny)
library(tidyverse)
library(tmap)
library(plotly)
library(leaflet)
library(DT)
library(shinyWidgets)
library(shinycssloaders)
library(ranger)

# **************************************** outside app ****************************************

ignition_rasterize_cluster_bf_season <- ignition_rasterize_cluster_bf_season %>%
  mutate(id = as_factor(id))


# ==================== read in data ====================
load(here::here("data/ida.RData"))
load(here::here("data/eda.RData"))
model <- readRDS(here::here("data/rfmodel_final.rds"))
model_df2_low_avg_high <- read_csv(here::here("data/model_df2_low_avg_high.csv"))

# --- set spinner colour to red
options(spinner.color = "#b22222")



# **************************************** define UI ****************************************
ui <- fluidPage(

  shinyWidgets::setBackgroundColor(color = "ghostwhite"),

  # add app title
  tags$h1("Bushfire Risk Predictions",
          style = "font-family: Impact; color: #1e90ff; font-size: 60px",
          align = "center"),
  br(),

  sidebarLayout(
    # === column for user interactivity
    sidebarPanel(width = 2,

                 # --- user; choose month
                 radioGroupButtons(
                   inputId = "month_chosen",
                   label = "Choose month",
                   choiceNames = c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar"), # values shown to users
                   choiceValues = c(10, 11, 12, 1, 2, 3), # values internal
                   selected = 12,
                   status = "danger"
                 ),

                 # --- user; toggle variable values

                 # `daily_rain`
                 shinyWidgets::sliderTextInput(inputId = "daily_rain_slider",
                                               label = "toggle daily rain",
                                               choices = -100:100,
                                               selected = 0,
                                               post = "%"),

                 # `et_short_crop`
                 shinyWidgets::sliderTextInput(inputId = "et_short_crop_slider",
                                               label = "toggle evapotranspiration rate",
                                               choices = -100:100,
                                               selected = 0,
                                               post = "%"),

                 # `max_temp`
                 shinyWidgets::sliderTextInput(inputId = "max_temp_slider",
                                               label = "toggle max temperature",
                                               choices = -100:100,
                                               selected = 0,
                                               post = "%"),
                 # `radiation`
                 shinyWidgets::sliderTextInput(inputId = "radiation_slider",
                                               label = "toggle radiation",
                                               choices = -100:100,
                                               selected = 0,
                                               post = "%"),

                 # `rh`
                 shinyWidgets::sliderTextInput(inputId = "rh_slider",
                                               label = "toggle relative humidity",
                                               choices = -100:100,
                                               selected = 0,
                                               post = "%"),

                 # `si10`
                 shinyWidgets::sliderTextInput(inputId = "si10_slider",
                                               label = "toggle 10m wind speed",
                                               choices = -100:100,
                                               selected = 0,
                                               post = "%"),

                 # `s0_pct`
                 shinyWidgets::sliderTextInput(inputId = "s0_pct_slider",
                                               label = "toggle surface soil moisture",
                                               choices = -100:100,
                                               selected = 0,
                                               post = "%"),
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
      shinycssloaders::withSpinner(),

    shiny::textOutput("test")
  ),
  br(), br(),



  # ========== plotly ==========
  fluidRow(
    column(width = 4,
           plotly::plotlyOutput("daily_rain_plotly") %>%
             shinycssloaders::withSpinner()),

    column(width = 4,
           plotly::plotlyOutput("max_temp_plotly") %>%
             shinycssloaders::withSpinner()),

    column(width = 4,
           plotly::plotlyOutput("et_short_crop_plotly") %>%
             shinycssloaders::withSpinner())

  )

)

# **************************************** define server ****************************************
server <- function(input, output) {

  # --- based on user selected month; create reactive dfs for each bushfire season for circle marker overlay groups
  cluster_16_sf <- shiny::reactive({
    cluster_16_21_sf %>%
      filter(year == 2016,
             month == input$month_chosen)
  })

  cluster_17_sf <- shiny::reactive({
    cluster_16_21_sf %>%
      filter(year == 2017,
             month == input$month_chosen)
  })

  cluster_18_sf <- shiny::reactive({
    cluster_16_21_sf %>%
      filter(year == 2018,
             month == input$month_chosen)
  })

  cluster_19_sf <- shiny::reactive({
    cluster_16_21_sf %>%
      filter(year == 2019,
             month == input$month_chosen)
  })

  cluster_20_sf <- shiny::reactive({
    cluster_16_21_sf %>%
      filter(year == 2020,
             month == input$month_chosen)
  })

  cluster_21_sf <- shiny::reactive({
    cluster_16_21_sf %>%
      filter(year == 2021,
             month == input$month_chosen)
  })

  # --- based on user selected month; predict avg
  model_df_pred <- shiny::reactive({

    model_df2_temp <- model_df2 %>%
      na.omit() %>%
      # select variables; user toggle
      select(id, year, month, daily_rain:s0_pct,
             -lai_hv, -lai_lv) %>% # most values; same throughout the years (no need toggle)
      pivot_longer(cols = daily_rain:s0_pct,
                   values_to = "value",
                   names_to = "var") %>%
      # for each id & variable
      group_by(id, var, month) %>%
      mutate(z = (value - mean(value, na.rm = T)) / sd(value, na.rm = T), # compute z RV
             mean = mean(value, na.rm = T), # compute mean
             sd = sd(value, na.rm = T)) # compute sd

    # --- temper variables based on z-scores
    # 1% = 0.1 sd -> 100% = 10sd

    # `daily_rain`
    model_df2_temp$value[model_df2_temp$var == "daily_rain"] <- model_df2_temp %>%
      filter(var == "daily_rain") %>%
      mutate(z = z + (input$daily_rain_slider * 0.1)) %>% # change z values; according to user toggle
      mutate(z_updated = (z * sd) + mean) %>%
      pull(z_updated)

    # `et_short_crop`
    model_df2_temp$value[model_df2_temp$var == "et_short_crop"] <- model_df2_temp %>%
      filter(var == "et_short_crop") %>%
      mutate(z = z + (input$et_short_crop_slider * 0.1)) %>% # change z values; according to user toggle// *0.1 because user chooses in %
      mutate(z_updated = (z * sd) + mean) %>%
      pull(z_updated)

    # `max_temp`
    model_df2_temp$value[model_df2_temp$var == "max_temp"] <- model_df2_temp %>%
      filter(var == "max_temp") %>%
      mutate(z = z + (input$max_temp_slider * 0.1)) %>% # change z values; according to user toggle// *0.1 because user chooses in %
      mutate(z_updated = (z * sd) + mean) %>%
      pull(z_updated)

    # `radiation`
    model_df2_temp$value[model_df2_temp$var == "radiation"] <- model_df2_temp %>%
      filter(var == "radiation") %>%
      mutate(z = z + (input$radiation_slider * 0.1)) %>% # change z values; according to user toggle// *0.1 because user chooses in %
      mutate(z_updated = (z * sd) + mean) %>%
      pull(z_updated)

    # `rh`
    model_df2_temp$value[model_df2_temp$var == "rh"] <- model_df2_temp %>%
      filter(var == "rh") %>%
      mutate(z = z + (input$rh_slider * 0.1)) %>% # change z values; according to user toggle// *0.1 because user chooses in %
      mutate(z_updated = (z * sd) + mean) %>%
      pull(z_updated)


    # `si10`
    model_df2_temp$value[model_df2_temp$var == "si10"] <- model_df2_temp %>%
      filter(var == "si10") %>%
      mutate(z = z + (input$si10_slider * 0.1)) %>% # change z values; according to user toggle// *0.1 because user chooses in %
      mutate(z_updated = (z * sd) + mean) %>%
      pull(z_updated)

    # `s0_pct`
    model_df2_temp$value[model_df2_temp$var == "s0_pct"] <- model_df2_temp %>%
      filter(var == "s0_pct") %>%
      mutate(z = z + (input$s0_pct_slider * 0.1)) %>% # change z values; according to user toggle// *0.1 because user chooses in %
      mutate(z_updated = (z * sd) + mean) %>%
      pull(z_updated)

    # --- recreate `model_df2` with tampered variables
    model_df2_temp2 <- model_df2_temp %>%
      select(id, year, month, var, value) %>%
      pivot_wider(names_from = var,
                  values_from = value) %>%
      ungroup() %>%
      # compute 1st lag
      mutate(across(.cols = daily_rain:s0_pct,
                    .fns = ~lag(.x),
                    .names = "{.col}_1")) %>%
      # compute 2nd lag
      mutate(across(.cols = daily_rain:s0_pct,
                    .fns = ~lag(.x,
                                n = 2),
                    .names = "{.col}_2")) %>%
      na.omit() %>%
      left_join(., model_df2 %>% na.omit() %>% select(id, year, month, # keys
                                                      forest, fire_count, x, y, lai_lv, lai_lv_1, lai_lv_2, lai_hv, lai_hv_1, lai_hv_2), # variables to join
                by = c("id", "year", "month")) %>%
      relocate(fire_count, x, y,
               .after = "year") %>%
      filter(month == input$month_chosen) %>%
      # make predictions with `rf` model
      mutate(pred = predict(model, .)$predictions,
             .after = "fire_count") %>%
      group_by(id) %>%
      summarise(avg_pred = mean(pred)) %>%
      ungroup() %>%
      mutate(total = sum(avg_pred)) %>%
      mutate(avg_pred_prop = avg_pred / total,
             .after = "avg_pred")

    # extract `id` & `geometry` column (i.e. polygon for each grid cell)
    vic_raster_crop_sf %>%
      mutate(id = as_factor(id)) %>%
      # join with predicted data
      left_join(., model_df2_temp2)
  })



  # ===== create leaflet map =====

  # --- palette intervals; based on user selected month
  pal_intervals <- shiny::reactive({
    classInt::classIntervals(model_df_pred()$avg_pred_prop,
                             n = 5,
                             style = "pretty")
  })

  output$map <- leaflet::renderLeaflet({

    # ---  palette create
    pal <- leaflet::colorBin(palette = "YlOrRd",
                             domain = pal_intervals()$brks, # palette intervals; based on user selected month
                             bins = 5,
                             pretty = T,
                             reverse = F)

    # --- create base map
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
    pal <- leaflet::colorBin(palette = "YlOrRd",
                             domain = pal_intervals()$brks,
                             bins = 5,
                             pretty = T,
                             reverse = F)


    base_map %>%
      # add outline of Victoria
      addPolylines(data = vic_map_sf,
                   color = "black",
                   weight = 3) %>%

      # add gridded cell
      addPolygons(data = model_df_pred(),
                  opacity = 0.1,
                  layerId = ~id,
                  fillOpacity = 0.4,
                  color = ~pal(avg_pred_prop), # fill by  avg. predicted proportions; using above palette
                  popup = ~paste0("<b> id: ", id, "</b>", "<br/>",
                                  "number of ignitions: ", round(avg_pred_prop, 3))) %>% # click pop-up texts

      # --- add circle markers; each `bf_season` as a layer
      # 2016
      addCircleMarkers(data = cluster_16_sf(),
                       radius = 2,
                       fillOpacity = 0.01,
                       color = "grey",
                       group = "2016") %>%
      # 2017
      addCircleMarkers(data = cluster_17_sf(),
                       radius = 2,
                       fillOpacity = 0.01,
                       color = "grey",
                       group = "2017") %>%
      # 2018
      addCircleMarkers(data = cluster_18_sf(),
                       radius = 2,
                       fillOpacity = 0.01,
                       color = "grey",
                       group = "2018") %>%
      # 2019
      addCircleMarkers(data = cluster_19_sf(),
                       radius = 2,
                       fillOpacity = 0.01,
                       color = "grey",
                       group = "2019") %>%
      # 2020
      addCircleMarkers(data = cluster_20_sf(),
                       radius = 2,
                       fillOpacity = 0.01,
                       color = "grey",
                       group = "2020") %>%
      # 2021
      addCircleMarkers(data = cluster_21_sf(),
                       radius = 2,
                       fillOpacity = 0.01,
                       color = "grey",
                       group = "2021") %>%

      # add user controls; for overlay groups
      addLayersControl(baseGroups = c("OSM", "Carto"),
                       overlayGroups = c("2016", "2017", "2018", "2019", "2020", "2021"),
                       options = layersControlOptions(collapsed = FALSE)) %>%

      # uncheck groups
      hideGroup(c("2017", "2018", "2019", "2020", "2021")) %>%

      # add & customise legend
      addLegend(pal = pal,
                values = pal_intervals()$brks,
                title = "predicted avg. proportion",
                position = "bottomright") %>%

      # add Reset button
      leaflet.extras::addResetMapButton()

  })

  # --- df based on user click
  model_df_id <- shiny::eventReactive(input$map_shape_click, { # invalidated; each time; user click grid cell
    # based on user map click

    model_df2_low_avg_high %>% # variable values; low = -10sd // avg.// high = +10 sd
      filter(id == input$map_shape_click$id,
             month == input$month_chosen)
  })

  # --- DT::datatable; based on cell clicked
  output$datatable <- DT::renderDT(
    model_df_pred() %>%
      sf::st_set_geometry(NULL) %>% # drop geometry column
      DT::datatable(options = list(scrollX = T,
                                   pageLength = 5)) %>%
      DT::formatRound(columns = 2:4,
                      digits = 3)
  )


  # --- plot variables of low = -10sd// avg.// high = +10sd

  # `daily_rain`
  output$daily_rain_plotly <- plotly::renderPlotly({
    # --- e.g. plotly
    model_df_id() %>%
      ungroup() %>%
      mutate(month = factor(month,
                            levels = c(10, 11, 12, 1, 2, 3))) %>%
      # add high & low filled
      plot_ly(data = .,
              x = ~year,
              y = ~daily_rain_high,
              type = "scatter",
              mode = "lines",
              line = list(color = 'transparent'),
              showlegend = FALSE,
              name = 'High') %>%
      add_trace(x = ~year,
                y = ~daily_rain_low,
                type = 'scatter',
                mode = 'lines',
                fill = 'tonexty',
                fillcolor='rgba(0,100,80,0.2)',
                line = list(color = 'transparent'),
                showlegend = FALSE,
                name = 'Low') %>%
      # add average line + markers
      add_trace(x = ~year,
                y = ~daily_rain_avg,
                type = 'scatter',
                mode = 'lines+markers',
                line = list(color='rgb(0,100,80)'),
                name = 'Average') %>%
      layout(yaxis = list(title = "daily_rain"))
  })

  # `max_temp`
  output$max_temp_plotly <- plotly::renderPlotly({
    # --- e.g. plotly
    model_df_id() %>%
      ungroup() %>%
      mutate(month = factor(month,
                            levels = c(10, 11, 12, 1, 2, 3))) %>%
      # add high & low filled
      plot_ly(data = .,
              x = ~year,
              y = ~max_temp_high,
              type = "scatter",
              mode = "lines",
              line = list(color = 'transparent'),
              showlegend = FALSE,
              name = 'High') %>%
      add_trace(x = ~year,
                y = ~max_temp_low,
                type = 'scatter',
                mode = 'lines',
                fill = 'tonexty',
                fillcolor='rgba(0,100,80,0.2)',
                line = list(color = 'transparent'),
                showlegend = FALSE,
                name = 'Low') %>%
      # add average line + markers
      add_trace(x = ~year,
                y = ~max_temp_avg,
                type = 'scatter',
                mode = 'lines+markers',
                line = list(color='rgb(0,100,80)'),
                name = 'Average') %>%
      layout(yaxis = list(title = "max_temp"))
  })

  output$et_short_crop_plotly <- plotly::renderPlotly({
    # --- e.g. plotly
    model_df_id() %>%
      ungroup() %>%
      mutate(month = factor(month,
                            levels = c(10, 11, 12, 1, 2, 3))) %>%
      # add high & low filled
      plot_ly(data = .,
              x = ~year,
              y = ~et_short_crop_high,
              type = "scatter",
              mode = "lines",
              line = list(color = 'transparent'),
              showlegend = FALSE,
              name = 'High') %>%
      add_trace(x = ~year,
                y = ~et_short_crop_low,
                type = 'scatter',
                mode = 'lines',
                fill = 'tonexty',
                fillcolor='rgba(0,100,80,0.2)',
                line = list(color = 'transparent'),
                showlegend = FALSE,
                name = 'Low') %>%
      # add average line + markers
      add_trace(x = ~year,
                y = ~et_short_crop_avg,
                type = 'scatter',
                mode = 'lines+markers',
                line = list(color='rgb(0,100,80)'),
                name = 'Average') %>%
      layout(yaxis = list(title = "et_short_crop"))
  })


}

# Run the application
shinyApp(ui = ui,
         server = server)
