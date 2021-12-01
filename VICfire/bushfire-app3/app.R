library(shiny)
library(tidyverse)
library(DT)
library(leaflet)
library(plotly)
library(ranger)
library(raster)
library(tmap)
library(shinycssloaders)
library(shinyWidgets)

# **************************************** outside app ****************************************


# ==================== read in data ====================
# load(here::here("data/ida.RData"))
# load(here::here("data/eda.RData"))

# model_df2_low_avg_high <- read_csv(here::here("data/model_df2_low_avg_high.csv"))

# --- df; with predictors & response; for modelling
# *note: na.omit() later; to remove grid cells (`id`) not in Victoria
model_df2 <- readr::read_rds("model_df2.rds")

# --- df; with {predictors}_low: -4 sd from mean// {predictors}_avg// {predictors}_high: +4 sd from mean; for plotlys
model_df2_low_avg_high <- readr::read_rds("model_df2_low_avg_high.rds")

# --- df; with fire ignitions; from 2016 to 2021; for overlaying on map
cluster_16_21_sf <- readr::read_rds("cluster_16_21_sf.rds")

# --- `ranger` model object; for predictions
model <- readr::read_rds("rfmodel_final.rds")


# **************************************** define UI ****************************************

# --- set spinner colour to red
options(spinner.color = "#b22222")

ui <- fluidPage(

  # --- use shinyalert; for information modal
  shinyalert::useShinyalert(),

  # --- set background colour
  shinyWidgets::setBackgroundColor(color = "#FFF5EE"),

  # --- app title
  tags$h1("Bushfire Risk Predictions",
          style = "font-family: Impact; color: #1e90ff; font-size: 60px",
          align = "center"),
  br(),

  shiny::fluidRow(
    # === side column for user interactivity
    shiny::column(width = 2,

                  # --- user; choose month
                  radioGroupButtons(
                    inputId = "month_chosen",
                    label = tags$h2("Toggle month & variable values"),
                    choiceNames = c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar"), # values shown to users
                    choiceValues = c(10, 11, 12, 1, 2, 3), # values internal
                    size = "lg",
                    selected = 12,
                    status = "danger"
                  ),

                  # --- user; toggle variable values

                  # `daily_rain`
                  shinyWidgets::sliderTextInput(inputId = "daily_rain_slider",
                                                label = tags$h4("Daily rain"),
                                                choices = -100:100,
                                                selected = 0,
                                                post = "%"),

                  # `max_temp`
                  shinyWidgets::sliderTextInput(inputId = "max_temp_slider",
                                                label = tags$h4("Max temperature"),
                                                choices = -100:100,
                                                selected = 0,
                                                post = "%"),


                  # `et_short_crop`
                  shinyWidgets::sliderTextInput(inputId = "et_short_crop_slider",
                                                label = tags$h4("Evapotranspiration rate"),
                                                choices = -100:100,
                                                selected = 0,
                                                post = "%"),

                  # `radiation`
                  shinyWidgets::sliderTextInput(inputId = "radiation_slider",
                                                label = tags$h4("Radiation"),
                                                choices = -100:100,
                                                selected = 0,
                                                post = "%"),

                  # `rh`
                  shinyWidgets::sliderTextInput(inputId = "rh_slider",
                                                label = tags$h4("Relative humidity"),
                                                choices = -100:100,
                                                selected = 0,
                                                post = "%"),

                  # `si10`
                  shinyWidgets::sliderTextInput(inputId = "si10_slider",
                                                label = tags$h4("10m wind speed"),
                                                choices = -100:100,
                                                selected = 0,
                                                post = "%"),

                  # `s0_pct`
                  shinyWidgets::sliderTextInput(inputId = "s0_pct_slider",
                                                label = tags$h4("Surface soil moisture"),
                                                choices = -100:100,
                                                selected = 0,
                                                post = "%"),

                  # --- apply button
                  shinyWidgets::actionBttn(inputId = "apply_bttn",
                                           label = "Apply",
                                           style = "fill",
                                           color = "danger",
                                           icon = icon("mouse")),

                  # --- reset button
                  shinyWidgets::actionBttn(inputId = "reset_bttn",
                                           label = "Reset",
                                           style = "fill",
                                           color = "danger",
                                           icon = icon("undo")),

                  br(), br(), br(),


                  # --- information button
                  shinyWidgets::actionBttn(inputId = "info_bttn",
                                           label = "",
                                           style = "material-circle",
                                           color = "danger",
                                           icon = icon("info"))


    ),

    # === main column; for leaflet map
    shiny::column(width = 8,
                  # leaflet map
                  leaflet::leafletOutput("map",
                                         height = 1000) %>%
                    shinycssloaders::withSpinner(),

                  br(), br(),

                  DT::DTOutput("datatable") %>%
                    shinycssloaders::withSpinner()

    ),

    # === right column; for plotly outputs
    shiny::column(width = 2,
                  style = "max-height: 85vh; overflow-y: auto;", # add vertical scroll bar

                  shiny::h2(shiny::textOutput(outputId = "cell_id_text")),

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

                  # `s0_pct`
                  plotly::plotlyOutput("s0_pct_plotly") %>%
                    shinycssloaders::withSpinner(),
    ),

  ),
  br(), br()
)

# **************************************** define server ****************************************
server <- function(input, output, session) {

  shiny::observeEvent(input$info_bttn, {
    # show a modal; when button pressed
    shinyalert::shinyalert(title = "Information- This is a bushfire risk prediction map",
                           size = "m", # size of modal
                           showConfirmButton = T, # no confirm button
                           closeOnEsc = T,
                           closeOnClickOutside = T,
                           type = "info") # info logo
  })

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

  # --- based on user selected month; predict avg. *to extract baseline
  model_df_pred_baseline <- shiny::reactive({

    model_df2_temp <- model_df2 %>%
      na.omit() %>%
      # select variables; user toggle
      dplyr::select(id, year, month, daily_rain:s0_pct,
                    -lai_hv, -lai_lv) %>% # most values; same throughout the years (no need toggle)
      pivot_longer(cols = daily_rain:s0_pct,
                   values_to = "value",
                   names_to = "var") %>%
      # for each id & variable
      group_by(id, var, month) %>%
      mutate(z = (value - mean(value, na.rm = T)) / sd(value, na.rm = T), # compute z RV
             mean = mean(value, na.rm = T), # compute mean
             sd = sd(value, na.rm = T)) # compute sd

    # --- recreate `model_df2` with variables values
    model_df2_temp2 <- model_df2_temp %>%
      dplyr::select(id, year, month, var, value) %>%
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
      # join with full data set (with untempered variables)
      na.omit() %>%
      left_join(., model_df2 %>% na.omit() %>% dplyr::select(id, year, month, # keys
                                                             forest, fire_count, x, y, lai_lv, lai_lv_1, lai_lv_2, lai_hv, lai_hv_1, lai_hv_2), # variables to join
                by = c("id", "year", "month")) %>%
      relocate(fire_count, x, y,
               .after = "year") %>%
      # filter to month chosen by user
      filter(month == input$month_chosen) %>%
      # make predictions with `rf` model
      mutate(pred = predict(model, .)$predictions,
             .after = "fire_count") %>%
      group_by(id) %>%
      summarise(avg_pred = mean(pred)) %>%
      ungroup() %>%
      mutate(total = sum(avg_pred)) %>% # baseline total
      mutate(avg_pred_prop = avg_pred / total,
             .after = "avg_pred")

  }) # fire event at start up

  # --- based on user selected month & toggled variables; create `model_df2`
  model_df_user <- shiny::eventReactive(input$apply_bttn, {

    model_df2_temp <- model_df2 %>%
      na.omit() %>%
      # select variables; user toggle
      dplyr::select(id, year, month, daily_rain:s0_pct,
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
      mutate(z = z + (input$daily_rain_slider * 0.04)) %>% # change z values; according to user toggle
      mutate(z_updated = pmax((z * sd) + mean, 0)) %>% # value cannot be < 0
      pull(z_updated)

    # `et_short_crop`
    model_df2_temp$value[model_df2_temp$var == "et_short_crop"] <- model_df2_temp %>%
      filter(var == "et_short_crop") %>%
      mutate(z = z + (input$et_short_crop_slider * 0.04)) %>% # change z values; according to user toggle// *0.1 because user chooses in %
      mutate(z_updated = (z * sd) + mean) %>%
      pull(z_updated)

    # `max_temp`
    model_df2_temp$value[model_df2_temp$var == "max_temp"] <- model_df2_temp %>%
      filter(var == "max_temp") %>%
      mutate(z = z + (input$max_temp_slider * 0.04)) %>% # change z values; according to user toggle// *0.1 because user chooses in %
      mutate(z_updated = (z * sd) + mean) %>%
      pull(z_updated)

    # `radiation`
    model_df2_temp$value[model_df2_temp$var == "radiation"] <- model_df2_temp %>%
      filter(var == "radiation") %>%
      mutate(z = z + (input$radiation_slider * 0.04)) %>% # change z values; according to user toggle// *0.1 because user chooses in %
      mutate(z_updated = pmax((z * sd) + mean, 0)) %>% # value cannot be < 0
      pull(z_updated)

    # `rh`
    model_df2_temp$value[model_df2_temp$var == "rh"] <- model_df2_temp %>%
      filter(var == "rh") %>%
      mutate(z = z + (input$rh_slider * 0.04)) %>% # change z values; according to user toggle// *0.1 because user chooses in %
      mutate(z_updated = pmax((z * sd) + mean, 0)) %>% # value cannot be < 0
      pull(z_updated)


    # `si10`
    model_df2_temp$value[model_df2_temp$var == "si10"] <- model_df2_temp %>%
      filter(var == "si10") %>%
      mutate(z = z + (input$si10_slider * 0.04)) %>% # change z values; according to user toggle// *0.1 because user chooses in %
      mutate(z_updated = pmax((z * sd) + mean, 0)) %>% # value cannot be < 0
      pull(z_updated)

    # `s0_pct`
    model_df2_temp$value[model_df2_temp$var == "s0_pct"] <- model_df2_temp %>%
      filter(var == "s0_pct") %>%
      mutate(z = z + (input$s0_pct_slider * 0.04)) %>% # change z values; according to user toggle// *0.1 because user chooses in %
      mutate(z_updated = pmax((z * sd) + mean, 0)) %>% # value cannot be < 0
      pull(z_updated)

    model_df2_temp

    }, ignoreNULL = F) # fire up at start of app

  # --- recreate `model_df2` with tampered variables(only if apply button ran)- join lag variables & untempered variables
  model_df_user2 <- reactive({

    model_df_user() %>%
      dplyr::select(id, year, month, var, value) %>%
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
      # join with full data set (with untempered variables)
      na.omit() %>%
      left_join(., model_df2 %>% na.omit() %>% dplyr::select(id, year, month, # keys
                                                             forest, fire_count, x, y, lai_lv, lai_lv_1, lai_lv_2, lai_hv, lai_hv_1, lai_hv_2), # variables to join
                by = c("id", "year", "month")) %>%
      relocate(fire_count, x, y,
               .after = "year") %>%
      # filter to month chosen by user
      filter(month == input$month_chosen)
  })

  # --- with recreated `model_df2`; run predictions
  model_df_pred <- reactive({

    # make predictions with `rf` model
    model_df2_temp2 <- model_df_user2() %>%
      mutate(pred = predict(model, .)$predictions,
             .after = "fire_count") %>%
      group_by(id) %>%
      summarise(avg_pred = mean(pred)) %>%
      ungroup() %>%
      mutate(total = sum(avg_pred)) %>% # baseline total
      mutate(avg_pred_prop = avg_pred / total,
             .after = "avg_pred")

    model_df2_temp2$total <- model_df_pred_baseline()$total

    # extract `id` & `geometry` column (i.e. polygon for each grid cell)
    vic_raster_crop_sf %>%
      mutate(id = as_factor(id)) %>%

      # join with predicted data
      left_join(., model_df2_temp2)
  })

  # --- reset button clicked; update sliders to 0
  shiny::observeEvent(input$reset_bttn, {

    # `daily_rain`
    shinyWidgets::updateSliderTextInput(session = session,
                                        inputId = c("daily_rain_slider"),
                                        selected = 0)

    # `et_short_crop`
    shinyWidgets::updateSliderTextInput(session = session,
                                        inputId = c("et_short_crop_slider"),
                                        selected = 0)

    # `max_temp`
    shinyWidgets::updateSliderTextInput(session = session,
                                        inputId = c("max_temp_slider"),
                                        selected = 0)

    # `radiation`
    shinyWidgets::updateSliderTextInput(session = session,
                                        inputId = c("radiation_slider"),
                                        selected = 0)

    # `rh`
    shinyWidgets::updateSliderTextInput(session = session,
                                        inputId = c("rh_slider"),
                                        selected = 0)

    # `si10`
    shinyWidgets::updateSliderTextInput(session = session,
                                        inputId = c("si10_slider"),
                                        selected = 0)

    # `s0_pct`
    shinyWidgets::updateSliderTextInput(session = session,
                                        inputId = c("s0_pct_slider"),
                                        selected = 0)
  })



  # ===== create leaflet map =====

  # --- `vic_map_sf`: for outline of polygon in app
  # Victoria map (sfdf MULTIPOLYGON)
  vic_map_sf <- ozmaps::ozmap_states %>%
    filter(NAME == "Victoria")

  # --- project crs
  vic_map_sf <- sf::st_transform(vic_map_sf,
                                 crs = 4326)

  # --- create `vic_raster` object *to be toggled
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

  # --- set values (`id` each cell)
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
      leaflet::setMaxBounds(lng1 = 140.9617 - 0.4,
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
                   weight = 3.5) %>%

      # add gridded cell
      addPolygons(data = model_df_pred(),
                  opacity = 0.1,
                  layerId = ~id,
                  fillOpacity = 0.4,
                  color = ~pal(avg_pred_prop), # fill by  avg. predicted proportions; using above palette
                  popup = ~paste0("<h3 style='font-size:5; color:#1E90FF'><b> id: ", id, "</b></h3>", "<br/>",
                                  "<h3 style='font-size:5'><b>predicted fire ignitions baseline total for month </b>", input$month_chosen, "= ", round(total, 2), "</h3>", "<br/>",
                                  # "<h3 style='font-size:5'><b>number of predicted ignitions: </b>", round(avg_pred_prop, 3), "</h3>",
                                  "<h3 style='font-size:5'><b>If there were 1,000 fire ignitions, ", round(1000*(avg_pred/total), 2), " is predicted to occur in this grid cell </h3>")) %>% # click pop-up texts

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
      addLegend(pal = pal, # palette
                values = pal_intervals()$brks, # palette breaks
                title = "predicted avg. proportion",
                position = "bottomright") %>%

      # add Reset button
      leaflet.extras::addResetMapButton()

  })

  # --- df based on user click
  model_df_id <- shiny::eventReactive(input$map_shape_click, { # invalidated; each time; user click grid cell
    # based on user map click

    model_df2_low_avg_high %>% # variable values; low = -4sd // avg.// high = +4 sd
      filter(id == input$map_shape_click$id,
             month == input$month_chosen)
  })

  # --- `renderText`; for plotly column; show cell id picked
  output$cell_id_text <- shiny::renderText({

    click <- input$map_shape_click

    # if user hasn't clicked
    if(is.null(click)){
      paste("click on grid cell to produce plots")
    }
    # else (user clicked on grid cell); print
    else{
      paste("Average & used monthly variable values for cell", input$map_shape_click$id)
    }
  })


  # --- DT::datatable; based on cell clicked
  output$datatable <- DT::renderDT(
    model_df_pred() %>%
      sf::st_set_geometry(NULL) %>% # drop geometry column
      rename(`cell id` = id,
             `average pred.` = avg_pred,
             `average pred. proportion` = avg_pred_prop) %>%
      na.omit() %>%
      DT::datatable(caption = htmltools::tags$caption(tags$h3(style = 'caption-side: top; text-align: left;',
                                                              "Predictions (Tabular)")),
                    options = list(scrollX = T, # scroll horizontal
                                   pageLength = 5,
                                   dom = "Bfrtip", # dom options
                                   buttons = c("csv", "excel")),
                    extensions = "Buttons") %>%
      DT::formatRound(columns = 2:4,
                      digits = 3)
  )


  # --- plot variables of low = -10sd// avg.// high = +10sd

  # `daily_rain` plotly
  output$daily_rain_plotly <- plotly::renderPlotly({

    req(input$map_shape_click) # require user to click a grid cell

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
                name = 'Average',
                showlegend = T) %>%
      # add toggled user selection
      add_trace(x = ~year,
                y = ~daily_rain,
                type = "scatter",
                mode = "line+markers",
                line = list(color = "#ff69b4"),
                name = "used",
                showlegend = T,
                data = model_df_user2() %>% filter(id == input$map_shape_click$id, # filter to user clicked cell
                                                   month == input$month_chosen)) %>%  # filter to month_chosen by user
      layout(yaxis = list(title = "daily rain (mm)"), # y-axis label
             legend = list(x = 0.43, y = 10)) # add legends
  })

  # `max_temp` plotly
  output$max_temp_plotly <- plotly::renderPlotly({

    req(input$map_shape_click) # require user to click a grid cell

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
      # add toggled user selection
      add_trace(x = ~year,
                y = ~max_temp,
                type = "scatter",
                mode = "line+markers",
                line = list(color = "#ff69b4"),
                name = "tampered values",
                data = model_df_user2() %>% filter(id == input$map_shape_click$id, # filter to user clicked cell
                                                   month == input$month_chosen)) %>%  # filter to month_chosen by user
      layout(yaxis = list(title = "max temp (°C)")) # y-axis label
  })

  # `et_short_crop` plotly
  output$et_short_crop_plotly <- plotly::renderPlotly({

    req(input$map_shape_click) # require user to click a grid cell

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
      # add toggled user selection
      add_trace(x = ~year,
                y = ~et_short_crop,
                type = "scatter",
                mode = "line+markers",
                line = list(color = "#ff69b4"),
                name = "tampered values",
                data = model_df_user2() %>% filter(id == input$map_shape_click$id, # filter to user clicked cell
                                                   month == input$month_chosen)) %>%  # filter to month_chosen by user
      layout(yaxis = list(title = "evapotranspiration (mm)")) # y-axis label
  })

  output$radiation_plotly <- plotly::renderPlotly({

    req(input$map_shape_click) # require user to click a grid cell

    model_df_id() %>%
      ungroup() %>%
      mutate(month = factor(month,
                            levels = c(10, 11, 12, 1, 2, 3))) %>%
      # add high & low filled
      plot_ly(data = .,
              x = ~year,
              y = ~radiation_high,
              type = "scatter",
              mode = "lines",
              line = list(color = 'transparent'),
              showlegend = FALSE,
              name = 'High') %>%
      add_trace(x = ~year,
                y = ~radiation_low,
                type = 'scatter',
                mode = 'lines',
                fill = 'tonexty',
                fillcolor='rgba(0,100,80,0.2)',
                line = list(color = 'transparent'),
                showlegend = FALSE,
                name = 'Low') %>%
      # add average line + markers
      add_trace(x = ~year,
                y = ~radiation_avg,
                type = 'scatter',
                mode = 'lines+markers',
                line = list(color='rgb(0,100,80)'),
                name = 'Average') %>%
      # add toggled user selection
      add_trace(x = ~year,
                y = ~radiation,
                type = "scatter",
                mode = "line+markers",
                line = list(color = "#ff69b4"),
                name = "tampered values",
                data = model_df_user2() %>% filter(id == input$map_shape_click$id, # filter to user clicked cell
                                                   month == input$month_chosen)) %>%  # filter to month_chosen by user
      layout(yaxis = list(title = "radiation (MJ/m^2)")) # y-axis label
  })

  output$rh_plotly <- plotly::renderPlotly({

    req(input$map_shape_click) # require user to click a grid cell

    model_df_id() %>%
      ungroup() %>%
      mutate(month = factor(month,
                            levels = c(10, 11, 12, 1, 2, 3))) %>%
      # add high & low filled
      plot_ly(data = .,
              x = ~year,
              y = ~rh_high,
              type = "scatter",
              mode = "lines",
              line = list(color = 'transparent'),
              showlegend = FALSE,
              name = 'High') %>%
      add_trace(x = ~year,
                y = ~rh_low,
                type = 'scatter',
                mode = 'lines',
                fill = 'tonexty',
                fillcolor='rgba(0,100,80,0.2)',
                line = list(color = 'transparent'),
                showlegend = FALSE,
                name = 'Low') %>%
      # add average line + markers
      add_trace(x = ~year,
                y = ~rh_avg,
                type = 'scatter',
                mode = 'lines+markers',
                line = list(color='rgb(0,100,80)'),
                name = 'Average') %>%
      # add toggled user selection
      add_trace(x = ~year,
                y = ~rh,
                type = "scatter",
                mode = "line+markers",
                line = list(color = "#ff69b4"),
                name = "tampered values",
                data = model_df_user2() %>% filter(id == input$map_shape_click$id, # filter to user clicked cell
                                                   month == input$month_chosen)) %>%  # filter to month_chosen by user
      layout(yaxis = list(title = "relative humidity (%)")) # yaxis label
  })

  output$si10_plotly <- plotly::renderPlotly({

    req(input$map_shape_click) # require user to click a grid cell

    model_df_id() %>%
      ungroup() %>%
      mutate(month = factor(month,
                            levels = c(10, 11, 12, 1, 2, 3))) %>%
      # add high & low filled
      plot_ly(data = .,
              x = ~year,
              y = ~si10_high,
              type = "scatter",
              mode = "lines",
              line = list(color = 'transparent'),
              showlegend = FALSE,
              name = 'High') %>%
      add_trace(x = ~year,
                y = ~si10_low,
                type = 'scatter',
                mode = 'lines',
                fill = 'tonexty',
                fillcolor='rgba(0,100,80,0.2)',
                line = list(color = 'transparent'),
                showlegend = FALSE,
                name = 'Low') %>%
      # add average line + markers
      add_trace(x = ~year,
                y = ~si10_avg,
                type = 'scatter',
                mode = 'lines+markers',
                line = list(color='rgb(0,100,80)'),
                name = 'Average') %>%
      # add toggled user selection
      add_trace(x = ~year,
                y = ~si10,
                type = "scatter",
                mode = "line+markers",
                line = list(color = "#ff69b4"),
                name = "tampered values",
                data = model_df_user2() %>% filter(id == input$map_shape_click$id, # filter to user clicked cell
                                                   month == input$month_chosen)) %>%  # filter to month_chosen by user
      layout(yaxis = list(title = "10m wind speed (m/s)")) # yaxis label
  })

  output$s0_pct_plotly <- plotly::renderPlotly({

    req(input$map_shape_click) # require user to click a grid cell

    model_df_id() %>%
      ungroup() %>%
      mutate(month = factor(month,
                            levels = c(10, 11, 12, 1, 2, 3))) %>%
      # add high & low filled
      plot_ly(data = .,
              x = ~year,
              y = ~s0_pct_high,
              type = "scatter",
              mode = "lines",
              line = list(color = 'transparent'),
              showlegend = FALSE,
              name = 'High') %>%
      add_trace(x = ~year,
                y = ~s0_pct_low,
                type = 'scatter',
                mode = 'lines',
                fill = 'tonexty',
                fillcolor='rgba(0,100,80,0.2)',
                line = list(color = 'transparent'),
                showlegend = FALSE,
                name = 'Low') %>%
      # add average line + markers
      add_trace(x = ~year,
                y = ~s0_pct_avg,
                type = 'scatter',
                mode = 'lines+markers',
                line = list(color='rgb(0,100,80)'),
                name = 'Average') %>%
      # add toggled user selection
      add_trace(x = ~year,
                y = ~s0_pct,
                type = "scatter",
                mode = "line+markers",
                line = list(color = "#ff69b4"),
                name = "tampered values",
                data = model_df_user2() %>% filter(id == input$map_shape_click$id, # filter to user clicked cell
                                                   month == input$month_chosen)) %>%  # filter to month_chosen by user
      layout(yaxis = list(title = "surface soil moisture (% full)")) # yaxis label
  })




}

# Run the application
shinyApp(ui = ui,
         server = server)
