# /*=================================================*/
#' # Preparation
# /*=================================================*/



expand.grid(overlap_degree, mechanism, yield_function)

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

#--- source functions ---#
source(here("Codes", "functions.R"))

#--- github ---#
source("https://raw.githubusercontent.com/brittanikedge/DIFM/main/Functions.R")

##### Trial Design #####
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

target_map <- ggplot(design) +
  geom_sf(aes(fill = factor(rate)), lwd = 0) +
  scale_fill_viridis_d() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())



##### Setting up functional form of yield response #####

mb_function <- function(x){
  y = 350*(1 - exp(-.012*(50 + x)))
  return(y)}

mb_function_low <- function(x){
  y = 350*(1 - exp(-.009*(50 + x)))
  return(y)}

mb_function_high <- function(x){
  y = 350*(1 - exp(-.015*(50 + x)))
  return(y)}

# graph yield response function
yield_response_base <- ggplot() +
  xlim(0, 300) +
  geom_function(fun = mb_function)

yield_response_low <- ggplot() +
  xlim(0, 300) +
  geom_function(fun = mb_function_low)

yield_response_high <- ggplot() +
  xlim(0, 300) +
  geom_function(fun = mb_function_high)

# add yield to target map
design <- design %>%
  data.table() %>%
  .[, yield_b := mb_function(rate) + rnorm(nrow(design), mean = 0, sd = 5)] %>%
  .[, yield_h := mb_function_high(rate) + rnorm(nrow(design), mean = 0, sd = 5)] %>%
  .[, yield_l := mb_function_low(rate) + rnorm(nrow(design), mean = 0, sd = 5)] %>%
  st_as_sf(.)


##### Generate Yield Data #####
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

misalignment_map <- ggplot() +
  geom_sf(data = design, aes(fill = factor(rate)), lwd = 0) +
  geom_sf(data = yield, color = "black", fill = NA, size = 0.2) +
  scale_fill_viridis_d() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

## Use the trial grid to represent what happened on the field
## So we take the area-weighted mean of nitrogen and yield inside the harvest grid
area_weighted_n <- st_intersection(
  dplyr::select(yield, yield_id, yield_area),
  dplyr::select(design, c(rate, yield_b, yield_h, yield_l))
) %>%
  #--- percentage overlapped ---#
  mutate(sub_pct = as.numeric(st_area(.)) / yield_area) %>%
  data.table() %>%
  #--- total sub_pct by yield polygon ---#
  .[, tot_sub_pct_n := sum(sub_pct), by = yield_id] %>% 
  #--- calculate sub_pct-weighted mean of applied rate ---#
  .[, wm_n_rate := sum(sub_pct * rate) / sum(sub_pct), by = yield_id] %>%
  .[, wm_y_b := sum(sub_pct * yield_b) / sum(sub_pct), by = yield_id] %>%
  .[, wm_y_h := sum(sub_pct * yield_h) / sum(sub_pct), by = yield_id] %>%
  .[, wm_y_l := sum(sub_pct * yield_l) / sum(sub_pct), by = yield_id]

## Merge with the intersected data by yield_id
yield_combined <- left_join(yield, area_weighted_n, by = "yield_id") %>%
  filter(!is.na(rate)) %>%
  filter(!is.na(yield_b)) 

## graph with the real yield response and the yield curve estimated from the harvester data
## limit the graph to around the trial rates

yield_estimate_b <- ggplot(yield_combined, aes(wm_n_rate, wm_y_b)) +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4, bs = "tp"), se = FALSE) +
  xlim(70, 250) +
  geom_function(fun = mb_function) +
  xlab("Nitrogen Rate") +
  ylab("Yield (bu/ac)")

yield_estimate_h <- ggplot(yield_combined, aes(wm_n_rate, wm_y_h)) +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4, bs = "tp"), se = FALSE) +
  xlim(70, 250) +
  geom_function(fun = mb_function_high) +
  xlab("Nitrogen Rate") +
  ylab("Yield (bu/ac)")

yield_estimate_l <- ggplot(yield_combined, aes(wm_n_rate, wm_y_l)) +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4, bs = "tp"), se = FALSE) +
  xlim(70, 250) +
  geom_function(fun = mb_function_low) +
  xlab("Nitrogen Rate") +
  ylab("Yield (bu/ac)")

save.image(here("Writing", "misalignment.RData"))

## graph with a

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

