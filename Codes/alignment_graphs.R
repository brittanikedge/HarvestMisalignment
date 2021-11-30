# /*=================================================*/
#' # Preparation
# /*=================================================*/

library(here)
library(future.apply)
library(tmap)
library(sp)
library(sf)
library(agricolae)
library(lwgeom)
library(scam)
library(measurements)
library(raster)
library(data.table)
library(tidyverse)
library(gdata)
library(mgcv)
library(MonteCarlo)
library(parallel)
library(gstat)
library(mapproj)

#--- source functions ---#
source(here("Codes", "functions new.R"))

#--- github ---#
source("https://raw.githubusercontent.com/brittanikedge/DIFM/main/Functions.R")

#--- setup ---#

trial_grids <- readRDS("Results/trial_grids.rds")
harvester_polygons <- readRDS("Results/harvester_polygons.rds")

rates_ls <- seq(80, 280, by = 40)
exp_design <- assign_rates(
  filter(trial_grids, td_grid_id != "headland"), 
  rates_ls
) %>% 
  data.table() %>% 
  .[, .(td_grid_id, rate)]

trial_design <- merge(trial_grids, exp_design, by = "td_grid_id")

xlim <- c(352640 - 60, 352640 + 60)
ylim <- c(4337720 - 100, 4337720 + 100)

##### Angle and Misalignment #####
alignment_case <- c("angle", "mis-alignment")
error_degree <- c(0, 10, 30)

### alignment maps ###

map_data <- expand.grid(
  alignment_case = alignment_case,
  error_degree = error_degree
) %>% 
  tibble() %>% 
  mutate(
    coverage_harvest = mclapply(
      seq_len(nrow(.)),
      function(i) {
        get_harvest(
          harvester_polygons,
          .$alignment_case[[i]],
          .$error_degree[[i]]
        )
      },
      mc.cores = 3
    )
  )

map_data_for_plot <- map_data %>%
  rowwise() %>% 
  mutate(coverage_harvest = list(
    data.table(coverage_harvest) %>% 
      .[, alignment_case := alignment_case] %>% 
      .[, error_degree := error_degree]
  )) %>% 
  pluck("coverage_harvest") %>% 
  rbindlist() %>% 
  st_as_sf()

alignment_map <- ggplot(map_data_for_plot) +
  geom_sf(data = trial_design, aes(fill = rate), lwd = 0) +
  geom_sf(fill = NA, size = 0.2, color = "black") +
  coord_sf(xlim = xlim, 
           ylim = ylim,
           datum = st_crs(trial_grids)) +
  facet_grid(alignment_case ~ error_degree) +
  scale_colour_distiller(palette = "Blues", 
                         type = "seq",
                         direction = 1,
                         guide = "colourbar",
                         aesthetics = c("fill")) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank())

saveRDS(alignment_map, file = here("Results", "alignment_map.rds"))

### individual examples for harvest misalignment types

alignment_case <- c("angle")
error_degree <- c(10)

### alignment maps ###
harvest <- get_harvest(harvester_polygons, "angle", 10)


angle_map <- ggplot(harvest) +
  geom_sf(data = trial_design, aes(fill = rate), lwd = 0) +
  geom_sf(fill = NA, size = 0.2, color = "black") +
  coord_sf(xlim = xlim, 
           ylim = ylim,
           datum = st_crs(trial_grids)) +
  scale_colour_distiller(palette = "Blues", 
                         type = "seq",
                         direction = 1,
                         guide = "colourbar",
                         aesthetics = c("fill")) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank())
saveRDS(angle_map, file = here("Results", "angle_map.rds"))

### alignment maps ###
harvest <- get_harvest(harvester_polygons, "mis-alignment", 15)

shift_map <- ggplot(harvest) +
  geom_sf(data = trial_design, aes(fill = rate), lwd = 0) +
  geom_sf(fill = NA, size = 0.2, color = "black") +
  coord_sf(xlim = xlim, 
           ylim = ylim,
           datum = st_crs(trial_grids)) +
  scale_colour_distiller(palette = "Blues", 
                         type = "seq",
                         direction = 1,
                         guide = "colourbar",
                         aesthetics = c("fill")) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank())
saveRDS(shift_map, file = here("Results", "shift_map.rds"))

##### Mismatch #####

boundary <- st_read(here("Data/Wendte_LaueLib80_2020/Raw", "boundary.shp")) %>%
  st_transform_utm()

abline <- st_read(here("Data/Wendte_LaueLib80_2020/Raw", "ab-line.shp")) %>%
  st_transform_utm()

# perhaps these are not in feet but meters
trial_grids <- make_trial_grids(field = boundary,
                                ab_line = abline,
                                plot_width = 18.288,
                                cell_height = 30,
                                headland_length = 0) %>%
  mutate(td_grid_id = paste0(strip_id, "_", cell_id))

rates_ls <- seq(80, 280, by = 40)
exp_design <- assign_rates(
  filter(trial_grids, td_grid_id != "headland"), 
  rates_ls
) %>% 
  data.table() %>% 
  .[, .(td_grid_id, rate)]

trial_design <- merge(trial_grids, exp_design, by = "td_grid_id")

harvest_grids <- make_trial_grids(field = boundary,
                                  ab_line = abline,
                                  plot_width = 18.288,
                                  cell_height = 2,
                                  headland_length = 0) %>%
  mutate(td_grid_id = paste0(strip_id, "_", cell_id))

alignment_case <- c("mismatch")
error_degree <- c(0.75, 1, 1.25)

### alignment maps ###

map_data <- expand.grid(
  alignment_case = alignment_case,
  error_degree = error_degree
) %>% 
  tibble() %>% 
  mutate(
    coverage_harvest = mclapply(
      seq_len(nrow(.)),
      function(i) {
        get_harvest(
          harvester_polygons,
          .$alignment_case[[i]],
          .$error_degree[[i]]
        )
      },
      mc.cores = 3
    )
  )

map_data_for_plot <- map_data %>%
  rowwise() %>% 
  mutate(coverage_harvest = list(
    data.table(coverage_harvest) %>% 
      .[, alignment_case := alignment_case] %>% 
      .[, error_degree := error_degree]
  )) %>% 
  pluck("coverage_harvest") %>% 
  rbindlist() %>% 
  st_as_sf()

alignment_mismatch <- ggplot(map_data_for_plot) +
  geom_sf(data = trial_design, aes(fill = rate), lwd = 0) +
  geom_sf(fill = NA, size = 0.2, color = "black") +
  coord_sf(xlim = xlim, 
           ylim = ylim,
           datum = st_crs(trial_grids)) +
  facet_grid(alignment_case ~ error_degree) +
  scale_colour_distiller(palette = "Blues", 
                         type = "seq",
                         direction = 1,
                         guide = "colourbar",
                         aesthetics = c("fill")) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank())
saveRDS(alignment_mismatch, file = here("Results", "alignment_map_mismatch.rds"))

### alignment maps ###
harvest <- get_harvest(harvester_polygons, "mismatch", .75)

mismatch_map <- ggplot(harvest) +
  geom_sf(data = trial_design, aes(fill = rate), lwd = 0) +
  geom_sf(fill = NA, size = 0.2, color = "black") +
  coord_sf(xlim = xlim, 
           ylim = ylim,
           datum = st_crs(trial_grids)) +
  scale_colour_distiller(palette = "Blues", 
                         type = "seq",
                         direction = 1,
                         guide = "colourbar",
                         aesthetics = c("fill")) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank())
saveRDS(mismatch_map, file = here("Results", "mismatch_map.rds"))

