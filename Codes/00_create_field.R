#/*=================================================*/
#' # Trial grids
#/*=================================================*/
ffy <- "Wendte_LaueLib80_2020"

#--- field boundary ---#
temp_field <- ffy %>% 
  here("Data", ., "Raw/boundary.shp") %>% 
  st_read() %>% 
  st_transform_utm()  

wf_bbox <- temp_field %>% 
  st_bbox() 

field <- wf_bbox %>% 
  st_as_sfc() %>% 
  st_as_sf() %>% 
  st_set_crs(st_crs(temp_field)) %>% 
  rename(geometry = x)

ab_line_field <- rbind(
  c(wf_bbox["xmin"], wf_bbox["ymin"]),
  c(wf_bbox["xmin"], wf_bbox["ymax"])
) %>%
st_linestring() %>% 
list(.) %>% 
st_as_sfc() %>% 
st_set_crs(st_crs(temp_field))

experiment_plots <- make_trial_grids(
  field = field, 
  #--- by default uses the first one ---#
  ab_line = ab_line[1, ], 
  plot_width =  conv_unit(60, "ft", "m"), 
  cell_height = conv_unit(30, "ft", "m"),
  headland_length = 120
) %>% 
mutate(td_grid_id := paste0(strip_id, "_", cell_id))

experiment_plots_dissolved <- experiment_plots %>% 
  st_snap_to_grid(size = 0.0001) %>%
  st_make_valid() %>% 
  summarize(plot_id = min(plot_id))

headland <- st_difference(field, experiment_plots_dissolved) %>%
  dplyr::select(geometry) %>% 
  mutate(
    plot_id = - 999,
    strip_id = - 999,
    group_in_strip = - 999,
    type = "headland",
    cell_id = - 999,
    td_grid_id = "headland"
  ) %>% 
  relocate(names(experiment_plots))

trial_desisn <- rbind(
  experiment_plots,
  headland
)

saveRDS(trial_desisn, "Results/trial_grids.rds")

#/*----------------------------------*/
#' ## Make expanded grid
#/*----------------------------------*/
temp_field <- ffy %>% 
  here("Data", ., "Raw/boundary.shp") %>% 
  st_read() %>% 
  st_transform_utm() %>% 
  st_buffer(conv_unit(990, "ft", "m")) 

wf_bbox <- temp_field %>% 
  st_bbox() 

field <- wf_bbox %>% 
  st_as_sfc() %>% 
  st_as_sf() %>% 
  st_set_crs(st_crs(temp_field))

harvester_polygons <- make_trial_grids(
  field = field, 
  #--- by default uses the first one ---#
  ab_line = ab_line_field[1, ], 
  plot_width =  conv_unit(60, "ft", "m"), 
  cell_height = conv_unit(30, "ft", "m"),
  headland_length = 0
) %>% 
mutate(unique_id_expanded := paste0(strip_id, "_", cell_id))

saveRDS(harvester_polygons, "Results/harvester_polygons.rds")

# ggplot() +
#   geom_sf(data = harvester_polygons, col = "blue") +
#   geom_sf(data = experiment_plots, col = "red") 

