#### Design trial grids ####
trial_grid_design <- function(plot_width){
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
polygon_width <- conv_unit(plot_width, "ft", "m")


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
return(all_grids)
}

##### Generate Yield Data #####
make_harvest<- function(harvester_width, case, error){
ffy <- "Wendte_LaueLib80_2020"

#--- field boundary ---#
field <- ffy %>%
  file.path(here("Data"), ., "Raw/boundary.shp") %>%
  st_read() %>%
  st_make_valid() %>%
  st_transform_utm()

square_field <- field %>%
  st_bbox() %>%
  st_as_sfc() %>%
  st_as_sf()

wf_bbox <- st_bbox(square_field)
starting_point <- c(wf_bbox["xmax"] + 100, wf_bbox["ymin"] - 100)

subplot_length <- conv_unit(10, "ft", "m")

ab_line <- rbind(
  c(wf_bbox["xmin"], wf_bbox["ymin"]),
  c(wf_bbox["xmin"], wf_bbox["ymax"])
) %>%
  st_linestring()
harvester <- make_trial_grids(square_field, ab_line, starting_point, conv_unit(harvester_width, "ft", "m"))
# written to handle any case #

if (case == "misalignment"){
  harvester <- st_shift(harvester, c(conv_unit(error, "ft", "m"), 0)) %>%
    st_set_crs(st_crs(harvester)) %>%
    mutate(yield_area = as.numeric(st_area(.))) %>%
    mutate(yield_id = row_number())
  
  yield <- harvester
}

if (case == "angle"){
  ab_tilted <- st_tilt(ab_line, error)[[5]]
  harvester <- make_trial_grids(square_field, ab_tilted[[1]], starting_point, conv_unit(harvester_width, "ft", "m")) %>%
    mutate(yield_area = as.numeric(st_area(.))) %>%
    mutate(yield_id = row_number())
  
  yield <- harvester
}
return(yield)
}

i <- 1
##### Monte Carlo Simulation #####
function_opt_n_est <- function(i){
  source(here("Codes", "functions.R"))
  print(paste0("Replication", i))
#### Design the trial
design <- assign_rates(design_grid, rates_ls, merge = TRUE)
# add yield to target map
design <- design %>%
  data.table() %>%
  .[, yield := y_function(rate) + rnorm(nrow(design), mean = 0, sd = 5)] %>%
  st_as_sf(.)
#### make analysis data ####

## Use the trial grid to represent what happened on the field
## So we take the area-weighted mean of nitrogen and yield inside the harvest grid
area_weighted_n <- st_intersection(
  dplyr::select(poly_yield, yield_id, yield_area),
  dplyr::select(design, c(rate, yield))
) %>%
  #--- percentage overlapped ---#
  mutate(sub_pct = as.numeric(st_area(.)) / yield_area) %>%
  data.table() %>%
  #--- total sub_pct by yield polygon ---#
  .[, tot_sub_pct_n := sum(sub_pct), by = yield_id] %>% 
  #--- calculate sub_pct-weighted mean of applied rate ---#
  .[, wm_n_rate := sum(sub_pct * rate) / sum(sub_pct), by = yield_id] %>%
  .[, wm_y := sum(sub_pct * yield) / sum(sub_pct), by = yield_id]

## Merge with the intersected data by yield_id
yield_combined <- left_join(poly_yield, area_weighted_n, by = "yield_id") %>%
  filter(!is.na(rate)) %>%
  filter(!is.na(yield)) 

## Estimate gam and find profit maximizing rate
scam <- scam(wm_y ~ s(wm_n_rate, bs = "micv"), data = yield_combined)

opt_n_est <- as.data.frame(seq(min(rates_ls), max(rates_ls), by = 1)) %>%
  dplyr::rename("wm_n_rate" = "seq(min(rates_ls), max(rates_ls), by = 1)") %>%
  cbind(predict.scam(scam, .), .) %>%
  rename("y_hat" = "predict.scam(scam, .)") %>%
  data.table() %>%
  .[, p_hat := y_hat*c_price - wm_n_rate*n_price] %>%
  .[, .SD[which.max(p_hat)]] %>%
  dplyr::select(wm_n_rate)
return(opt_n_est)
}

# design_grid <- result_table$design_grid[1][[1]]
# poly_yield <- result_table$poly_yield[1][[1]]
# y_function <- result_table$y_function[1][[1]]
# n_sims <- 2
# function_sim(design_grid, poly_yield, y_function, n_sims)
  
function_sim <- function(design_grid, poly_yield, y_function, n_sims){
print(paste0(n_sims, "simulations")) 
cl <-  parallel::makeCluster(3, setup_strategy = "sequential")

clusterExport(cl, varlist = c("design_grid", "poly_yield", "y_function", "rates_ls", "c_price", "n_price"), envir = .GlobalEnv)

clusterEvalQ(cl, c(library(sf), library(dplyr), library(data.table), library(here), library(mgcv), library(scam)))
list <- parSapply(cl, 1:n_sims, function_opt_n_est)
stopCluster(cl)

# 204 is the optimal rate for this yield function
opt_n_estimates <- as.data.frame(unlist(list)) %>%
  rename("opt_n_est" = "unlist(list)")
return(opt_n_estimates)
}
