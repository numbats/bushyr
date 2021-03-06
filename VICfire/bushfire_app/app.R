library(shiny)
library(tidyverse)
library(tmap)
library(shinyWidgets)
library(shinycssloaders)

# **************************************** outside app ****************************************

# --- set spinner colour to red
options(spinner.color = "#b22222")

# ==================== read in data ====================

# --- read in combined historical & satellite fire ignition data
load(here::here("data/cause_join_df"))

# --- convert fire ignition join data; into into `sf` object
cause_join_sf <- cause_join_df %>%
  sf::st_as_sf(coords = c("lon", "lat"))

# ==================== create spatial grid `RasterBrick`; cropped to Victoria map ====================

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
  crs = 4326
)

# === Victoria map (sfdf MULTIPOLYGON)
vic_map_sf <- ozmaps::ozmap_states %>%
  filter(NAME == "Victoria")

# --- project crs
vic_map_sf <- sf::st_transform(vic_map_sf,
                               crs = 4326)

# --- mask raster; to only Victorian map
# * `raster` package; NOT compatible with `sf` yet; so; need; change to `sp`
vic_map_sp <- as(vic_map_sf,
                 Class = "Spatial")

# mask (*think: crop to polygon shape) raster; to only Victorian map (`vic_map_sp`)
vic_raster_crop <- vic_raster %>%
  raster::mask(mask = vic_map_sp)


# **************************************** define UI ****************************************
ui <- fluidPage(
  # --- output map
  tmapOutput("map") %>%
    shinycssloaders::withSpinner(type = 7),

  # --- user; select year(s)
  shinyWidgets::checkboxGroupButtons(
    inputId = "selected_years",
    label = "Select Years",
    choices = c(2000:2020), # year choices
    selected = c(2019, 2020), # default to 2019 & 2020
    status = "danger"
  ),

  # --- user; select season(s)
  shinyWidgets::checkboxGroupButtons(
    inputId = "selected_seasons",
    label = "Select Seasons",
    choices = c("summer", "autumn", "winter", "spring"), # year choices
    selected = c("summer", "autumn", "winter", "spring"), # default to all seasons
    status = "danger"
  ),

  shinyWidgets::checkboxGroupButtons(
    inputId = "selected_causes",
    label = "Select Causes",
    choices = c("lightning", "burning_off", "accident", "arson"), # `cause` choices
    selected = c("lightning", "burning_off", "accident", "arson"), # default to all causes
    status = "danger"
  ),

  # --- run simulation; when user click button
  shiny::actionButton(inputId = "run_sim",
                      label = "Run simulation!")
)

# **************************************** define server ****************************************
server <- function(input, output) {

  # ==================== Initial Ignition grid ====================
  # values of  ignition grid cells
  # •`fire_count`: count of no. of fire ignitions; per grid cell
  # •`fire_prop`: # ignitions in grid cell / # total no. of ignitions

  vic_raster_crop_values_join <- shiny::reactive({
    # --- user-specified years
    cause_join_sf <- cause_join_sf %>%
      filter(year %in% input$selected_years,
             season %in% input$selected_seasons,
             cause %in% input$selected_causes)

    # --- convert from `sf` -> `spdf` -> `SpatialPoints`; to be able; use `raster::rasterize` (count; no. of pts.; in each raster cell)

    # convert from `sf` -> `spdf`
    cause_join_spdf <- as(cause_join_sf,
                          Class = "Spatial")

    # convert from `spdf` to `SpatialPoints`
    cause_join_sp <- as(cause_join_spdf,
                        "SpatialPoints")

    # --- count; no. of points in each grid cell
    ignition_join_rasterize <- raster::rasterize(x = cause_join_sp,
                                                 y = vic_raster_crop,
                                                 fun = "count")
    # --- compute columns; in each grid cell
    # •`id`: id for grid cell (rowwise)
    # •`fire_count`: no. of ignition points; per grid cell
    # •`fire_prop`: # no. of points / # total no. of points
    # *returns matrix

    ignition_raster_values_join <- raster::getValues(ignition_join_rasterize) %>%
      tibble(fire_count = .) %>%
      replace_na(list(fire_count = 0)) %>%  # replace NA (cells w/o fire ignitions) with 0
      mutate(id = 1:nrow(.), # cell id
             .before = fire_count) %>%
      mutate(fire_prop = fire_count / sum(fire_count)) %>% # no. of fire ignitions in a cell / total no. of fire ignitions
      as.matrix() # turn intro matrix (each column = 1 layer in `RasterBrick`)

    # --- set computed values; in `vic_raster_crop` (`RasterBrick` object)
    raster::setValues(x = vic_raster_crop,
                      values = ignition_raster_values_join)
  })


  # ==================== run simulation ====================
  sim_count_raster_sf <- eventReactive(input$run_sim,
                                       {
                                         # --- frequency distribution; to draw no. of bushfires
                                         # based on user selection of `year` and `seasons`
                                         freq_dist_year_season <- cause_join_sf %>%
                                           count(year) # broadest group

                                         # --- create empty storage matrix
                                         sim_count <- matrix(NA,
                                                             nrow = 0,
                                                             ncol = 3)

                                         # --- run for loop: simulate random points (bushfire ignitions); in `raster` cell; return coordinates (matrix)
                                         for(i in 1:1000){
                                           # simulate random points; in `raster` cells
                                           sim <- enmSdm::sampleRast(x = vic_raster_crop_values_join()$fire_count, # `raster` object
                                                                     # *** draw no. of points; from frequency distribution ***
                                                                     n = sample(freq_dist_year_season$n,
                                                                                size = 1), # 1 number
                                                                     replace = T, # w replacement
                                                                     prob = T) # sample cells with probabilities proportional to cell value (fire_count)

                                           # include 3rd column: simulation iteration number
                                           sim <- cbind(sim, rep(i, nrow(sim)))

                                           # store results in `sim_count`
                                           sim_count <- rbind(sim_count, sim)
                                         }

                                         # create `SpatialPoints` object to store simulation points;
                                         # so; able; `rasterize` (count no. of points in each cell)
                                         sim_count_sp <- sim_count %>%
                                           as_tibble() %>%
                                           rename(lon = x,
                                                  lat = y,
                                                  sim_id = V3) %>%
                                           select(-sim_id) %>% # cannot have `sim_id` when `rasterize`
                                           # convert to `sp` object
                                           sp::SpatialPoints()


                                         # --- count number of points(bushfire ignitions); in each cell
                                         sim_count_raster <- raster::rasterize(x = sim_count_sp,# `SpatialPoints` object
                                                                               y = vic_raster_crop, # `Raster` object
                                                                               fun = "count")

                                         # --- convert from `raster` -> `spdf` -> `sf`; to plot in `tmap`
                                         # hovering over variable; can only work with sf polygon (grid cells) using `tm_polygons`
                                         # doesn't work with `tm_raster`
                                         sim_count_raster_sp <- as(sim_count_raster, "SpatialPolygonsDataFrame")

                                         sf::st_as_sf(sim_count_raster_sp) %>%
                                           rename(fire_count = layer) %>%
                                           mutate(fire_count = fire_count / 1000) # divide `fire_count`; by no. of simulations ∴ output represent no. of bushfires in the number of months in `season` chosen by user
                                       })


  output$map <- tmap::renderTmap({
    # plot `raster`; fill by counts
    tmap::tm_basemap(leaflet::providers$Esri.WorldTopoMap) +
    tmap::tm_shape(sim_count_raster_sf()) + # simulation result; after clicking run simuatlion button
      tmap::tm_polygons(alpha = 0.5,
                        col = "fire_count",
                        title = "Number of simulated fire ignitions") +
      # include outline of victoria map
      tmap::tm_shape(vic_map_sf) +
      tmap::tm_borders(lwd = 2)
  })

}


# **************************************** run app. ****************************************
shinyApp(ui = ui,
         server = server)
