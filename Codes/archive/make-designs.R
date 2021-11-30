# /*=================================================*/
#' # Preparation
# /*=================================================*/

library(here)
library(tmap)
library(sp)
library(sf)
library(agricolae)
library(lwgeom)
library(measurements)
library(raster)
library(data.table)
library(tidyverse)
library(scam)

#--- source functions ---#
source(here("Codes", "functions.R"))

#--- github ---#
source("https://raw.githubusercontent.com/brittanikedge/DIFM/main/Functions.R")

# /*----------------------------------*/
#' ## Field data
# /*----------------------------------*/
# You can create your own field and ab-line
ffy <- "Wendte_LaueLib80_2020"

#--- field boundary ---#
field <- ffy %>%
  file.path(here("Data"), ., "Raw/boundary.shp") %>%
  st_read() %>%
  st_make_valid() %>%
  st_transform_utm()

# /*=================================================*/
#' # Create polygons (plots, harvester, applicator, planter)
# /*=================================================*/
# /*----------------------------------*/
#' ## Set parameters
# /*----------------------------------*/
square_field <- field %>%
  st_bbox() %>%
  st_as_sfc() %>%
  st_as_sf()

wf_bbox <- st_bbox(square_field)
starting_point <- c(wf_bbox["xmax"] + 100, wf_bbox["ymin"] - 100)

# use 60 ft design with 30 ft yield monitor
subplot_length <- conv_unit(10, "ft", "m")
polygon_width <- conv_unit(60, "ft", "m")


# /*----------------------------------*/
#' ## Create polygons
# /*----------------------------------*/
# group: strip id
# basic_plot_id: ids for the small polygons
# plot_id: plot id

ab_line <- rbind(
  c(wf_bbox["xmin"], wf_bbox["ymin"]),
  c(wf_bbox["xmin"], wf_bbox["ymax"])
) %>%
  st_linestring()

all_grids <- make_trial_grids(square_field, ab_line, starting_point, polygon_width)

# ggplot(all_grids) +
#   geom_sf()

rates_ls <- seq(80, 240, by = 40)

design <- assign_rates(all_grids, rates_ls, merge = TRUE)

# assume Mitscherlich-Baule functional form

mb_function <- function(x){
  y = 300*(1 - exp(-.015*(50 + x)))
  return(y)}

 # mb_metric <- function(x){
 #  y = 9180*(1 - exp(-0.0288 *(50.6952  + x)))
 #  return(y)}

# graph yield response function
base <- ggplot() +
  xlim(0, 300) +
  geom_function(fun = mb_function)

target_map <- ggplot(design) +
  geom_sf(aes(fill = factor(rate))) +
  scale_fill_viridis_d()

# add yield to target map
design <- design %>%
  data.table() %>%
  .[, yield := mb_function(rate)] %>%
  st_as_sf(.)

harvester <- make_trial_grids(square_field, ab_line, starting_point, conv_unit(30, "ft", "m"))

# normal polygons when no misalignment
# ggplot() +
#   geom_sf(data = design, aes(fill = factor(rate))) +
#   geom_sf(data = harvester, color = "red", fill = NA, size = 0.3) +
#   scale_fill_viridis_d()

##### Case 1: Misalignment due to shift of harvester #####
harvester_shifted <- st_shift(harvester, c(conv_unit(15, "ft", "m"), 0)) %>%
  st_set_crs(st_crs(harvester)) %>%
  mutate(yield_area = as.numeric(st_area(.))) %>%
  mutate(yield_id = row_number())

yield <- harvester_shifted

# ggplot() +
#   geom_sf(data = design, aes(fill = factor(rate))) +
#   geom_sf(data = yield, color = "red", fill = NA, size = 0.3) +
#   scale_fill_viridis_d()

## Use the trial grid to represent what happened on the field
## So we take the area-weighted mean of nitrogen and yield inside the harvest grid
area_weighted_n <- st_intersection(
  dplyr::select(yield, yield_id, yield_area),
  dplyr::select(design, c(rate, yield))
) %>%
  #--- percentage overlapped ---#
  mutate(sub_pct = as.numeric(st_area(.)) / yield_area) %>%
  data.table() %>%
  #--- total sub_pct by yield polygon ---#
  .[, tot_sub_pct_n := sum(sub_pct), by = yield_id] %>% 
  #--- calculate sub_pct-weighted mean of applied rate ---#
  .[, wm_n_rate := sum(sub_pct * rate) / sum(sub_pct), by = yield_id] %>%
  .[, wm_y_rate := sum(sub_pct * yield) / sum(sub_pct), by = yield_id]

## Merge with the intersected data by yield_id
yield_combined <- left_join(yield, area_weighted_n, by = "yield_id") %>%
  filter(!is.na(rate)) %>%
  filter(!is.na(yield)) 

## graph with the real yield response and the yield curve estimated from the harvester data
## limit the graph to around the trial rates

scam <- scam(wm_y_rate ~ s(wm_n_rate, bs = "micv"), data = yield_combined)

pred_data <- as.data.frame(seq(min(rates_ls), max(rates_ls), by = 1)) %>%
  dplyr::rename("wm_n_rate" = "seq(min(rates_ls), max(rates_ls), by = 1)") %>%
  cbind(predict.scam(scam, .), .) %>%
  rename("y_hat" = "predict.scam(scam, .)")

ggplot(pred_data, aes(wm_n_rate, y_hat)) +
  geom_smooth(method = "loess", se = FALSE) +
  xlim(70, 250) +
  geom_function(fun = mb_function)

##### Case 2: Differing Ablines for application and harvest #####
# now change the angle of the abline for the harvest
ab_tilted <- st_tilt(ab_line, 10)[[5]]
harvester_tilted <- make_trial_grids(square_field, ab_tilted[[1]], starting_point, conv_unit(30, "ft", "m"))

ggplot(data = design) +
  geom_sf(data = harvester_tilted, color = "red", fill = NA, size = 0.3) +
  scale_fill_viridis_d()

ggplot(data = design) +
  geom_sf(data = harvester_tilted, color = "red", fill = NA, size = 0.3) +
  scale_fill_viridis_d()

