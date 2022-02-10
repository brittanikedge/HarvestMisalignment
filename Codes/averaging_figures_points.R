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
library(R.utils)

#--- source functions ---#
source(here("Codes", "functions new.R"))

#--- github ---#
source("https://raw.githubusercontent.com/brittanikedge/DIFM/main/Functions.R")

#### Mismatch Seed Responses ####

trial_grids <- readRDS("Data/Wendte_LaueLib80_2020/Raw/trial-grids-mismatch.rds")
trial_grids <- dplyr::rename(trial_grids, "td_grid_id" = "td_grd_", "strip_id" = "strip_d")

harvester_polygons <- readRDS("Data/Wendte_LaueLib80_2020/Raw/harvester-polygons-mismatch.rds")

# keep the same setup for the machine, treatments and prices

n_price <- .5
s_price <- 4.50 
# c_price <- 5.00

c_price <- c(5.62)
# ys_function <- c("low_response", "middle_response", "high_response")
# max_dev <- c(20, 30, 40, 100)

ys_function <- c("seed_response")
max_dev <- c(2, 4, 6, 100)

rate_types <- c("rates_low", "rates_center", "rates_high")

alignment_case <- c("mismatch")
error_degree <- c(.75, 1, 1.25)

#/*----------------------------------*/
#' ## Create field data cases with coverage pct data
#/*----------------------------------*/
field_data <- expand.grid(
  alignment_case = alignment_case,
  error_degree = error_degree
) %>% 
  tibble() %>% 
  mutate(
    coverage_pct_data = mclapply(
      seq_len(nrow(.)),
      function(i) {
        get_coverage_pct(
          harvester_polygons,
          .$alignment_case[[i]],
          .$error_degree[[i]]
        )
      },
      mc.cores = 3
    )
  )

#/*----------------------------------*/
#' ## Combine with yield function types
#/*----------------------------------*/
data_cases <- tibble(expand.grid(ys_type = ys_function,
                                 rate_types =  rate_types,
                                 c_price = c_price,
                                 max_dev = max_dev)) %>% 
  rowwise() %>% 
  #--- find the optimal N for each ys function ---#
  mutate(
    opt_n = optimize(
      function(x) gen_profit(ys_type, x, c_price, n_price, s_price), 
      interval = c(0, 300), 
      maximum = TRUE
    )$maximum
  ) %>% 
  mutate(
    rates_ls = list(gen_rates(ys_type, x, c_price, n_price, s_price, rate_types))
  ) %>% 
  ungroup() %>% 
  #--- expand grid with field_data ---#
  expand_grid_df(., field_data) %>% 
  data.table()

all_results <- data_cases %>%
  mutate(
    sim_data = mclapply(
      seq_len(nrow(.)),
      function(x) {
        get_sim_data(
          trial_design = trial_grids,
          coverage_data = .$coverage_pct_data[[x]],
          ys_type = .$ys_type[[x]],
          error_degree = .$error_degree[[x]],
          rates_ls = .$rates_ls[[x]],
          opt_n = .$opt_n[[x]]
        )
      },
      mc.cores = 3
    )
  )

clean_results <- data_cases %>%
  mutate(
    clean_data = mclapply(
      seq_len(nrow(.)),
      function(x) {
        get_cleaned_data(
          trial_design = trial_grids,
          coverage_data = .$coverage_pct_data[[x]],
          ys_type = .$ys_type[[x]],
          error_degree = .$error_degree[[x]],
          rates_ls = .$rates_ls[[x]],
          opt_n = .$opt_n[[x]],
          max_dev = .$max_dev[[x]]
        )
      },
      mc.cores = 3
    )
  )

final_results_clean <- clean_results %>%
  dplyr::select(- coverage_pct_data) %>%
  unnest(clean_data) %>%
  data.table() %>%
  .[, alignment_case := as.character(alignment_case)]

final_results <- all_results %>%
  dplyr::select(- coverage_pct_data) %>%
  unnest(sim_data) %>%
  data.table() %>%
  .[, alignment_case := as.character(alignment_case)]

# gen_prof_diff(error = data_cases$error_degree[[1]],
#               yield_type = data_cases$ys_type[[1]],
#               case = data_cases$alignment_case[[1]],
#               rates = data_cases$rate_types[[1]],
#               crop_price = data_cases$c_price[[1]],
#               dev_max = data_cases$max_dev[[1]],
#               opt_n = data_cases$opt_n[[1]])

profit_data <- data_cases %>% 
  .[, alignment_case := as.character(alignment_case)] %>%
  rowwise() %>% 
  mutate(
    profit = list(gen_prof_diff(error = error_degree, yield_type = ys_type, case = alignment_case, rates = rate_types, crop_price = c_price, dev_max = max_dev, opt_n = opt_n))
  ) %>% 
  ungroup() %>% 
  data.table()  %>%
  unnest(profit)

final_results <- final_results_clean

det_est_data <- data_cases %>% 
  .[, alignment_case := as.character(alignment_case)] %>%
  rowwise() %>% 
  mutate(
    model_est_data = list(get_est_data(yield_type = ys_type, error = error_degree, case = alignment_case, rates = rate_types, crop_price = c_price, dev_max = max_dev, opt_n = opt_n)),
    # results = list(gen_prof_diff(yield_type = ys_type, error = error_degree, case = alignment_case, rates = rate_types, crop_price = c_price))
  ) %>% 
  ungroup() %>% 
  data.table()  %>%
  unnest(model_est_data)

saveRDS(final_results, here("Results", "deterministic_clean_data_mismatch_seed.rds"))
saveRDS(det_est_data, here("Results", "deterministic_clean_est_mismatch_seed.rds"))
saveRDS(profit_data, here("Results", "deterministic_clean_profit_mismatch_seed.rds"))

final_results <- all_results %>%
  dplyr::select(- coverage_pct_data) %>%
  unnest(sim_data) %>%
  data.table() %>%
  .[, alignment_case := as.character(alignment_case)]

det_est_data <- data_cases %>% 
  .[, alignment_case := as.character(alignment_case)] %>%
  rowwise() %>% 
  mutate(
    model_est_data = list(get_est_data(yield_type = ys_type, error = error_degree, case = alignment_case, rates = rate_types, crop_price = c_price, dev_max = max_dev)),
    # results = list(gen_prof_diff(yield_type = ys_type, error = error_degree, case = alignment_case, rates = rate_types, crop_price = c_price))
  ) %>% 
  ungroup() %>% 
  data.table()  %>%
  unnest(model_est_data)

saveRDS(final_results, here("Results", "deterministic_data_mismatch_seed.rds"))
saveRDS(det_est_data, here("Results", "deterministic_est_mismatch_seed.rds"))

#### Mismatch Nitrogen Responses ####
trial_grids <- readRDS("Data/Wendte_LaueLib80_2020/Raw/trial-grids-mismatch.rds")
trial_grids <- dplyr::rename(trial_grids, "td_grid_id" = "td_grd_", "strip_id" = "strip_d")

harvester_polygons <- readRDS("Data/Wendte_LaueLib80_2020/Raw/harvester-polygons-mismatch.rds")

n_price <- .5
s_price <- 4.50 
c_price <- c(5.62)
rate_types <- c("rates_low", "rates_center", "rates_high")
alignment_case <- c("mismatch")
error_degree <- c(.75, 1, 1.25)

ys_function <- c("low_response", "middle_response", "high_response")
max_dev <- c(20, 30, 40, 100)

#/*----------------------------------*/
#' ## Create field data cases with coverage pct data
#/*----------------------------------*/
field_data <- expand.grid(
  alignment_case = alignment_case,
  error_degree = error_degree
) %>% 
  tibble() %>% 
  mutate(
    coverage_pct_data = mclapply(
      seq_len(nrow(.)),
      function(i) {
        get_coverage_pct(
          harvester_polygons,
          .$alignment_case[[i]],
          .$error_degree[[i]]
        )
      },
      mc.cores = 3
    )
  )

#/*----------------------------------*/
#' ## Combine with yield function types
#/*----------------------------------*/
data_cases <- tibble(expand.grid(ys_type = ys_function,
                                 rate_types =  rate_types,
                                 c_price = c_price,
                                 max_dev = max_dev)) %>% 
  rowwise() %>% 
  #--- find the optimal N for each ys function ---#
  mutate(
    opt_n = optimize(
      function(x) gen_profit(ys_type, x, c_price, n_price, s_price), 
      interval = c(0, 300), 
      maximum = TRUE
    )$maximum
  ) %>% 
  mutate(
    rates_ls = list(gen_rates(ys_type, x, c_price, n_price, s_price, rate_types))
  ) %>% 
  ungroup() %>% 
  #--- expand grid with field_data ---#
  expand_grid_df(., field_data) %>% 
  data.table()

all_results <- data_cases %>%
  mutate(
    sim_data = mclapply(
      seq_len(nrow(.)),
      function(x) {
        get_sim_data(
          trial_design = trial_grids,
          coverage_data = .$coverage_pct_data[[x]],
          ys_type = .$ys_type[[x]],
          error_degree = .$error_degree[[x]],
          rates_ls = .$rates_ls[[x]],
          opt_n = .$opt_n[[x]]
        )
      },
      mc.cores = 3
    )
  )

clean_results <- data_cases %>%
  mutate(
    clean_data = mclapply(
      seq_len(nrow(.)),
      function(x) {
        get_cleaned_data(
          trial_design = trial_grids,
          coverage_data = .$coverage_pct_data[[x]],
          ys_type = .$ys_type[[x]],
          error_degree = .$error_degree[[x]],
          rates_ls = .$rates_ls[[x]],
          opt_n = .$opt_n[[x]],
          max_dev = .$max_dev[[x]]
        )
      },
      mc.cores = 3
    )
  )


final_results_clean <- clean_results %>%
  dplyr::select(- coverage_pct_data) %>%
  unnest(clean_data) %>%
  data.table() %>%
  .[, alignment_case := as.character(alignment_case)]

final_results <- all_results %>%
  dplyr::select(- coverage_pct_data) %>%
  unnest(sim_data) %>%
  data.table() %>%
  .[, alignment_case := as.character(alignment_case)]

profit_data <- data_cases %>% 
  .[, alignment_case := as.character(alignment_case)] %>%
  rowwise() %>% 
  mutate(
    profit = list(gen_prof_diff(yield_type = ys_type, error = error_degree, case = alignment_case, rates = rate_types, crop_price = c_price, dev_max = max_dev, opt_n = opt_n))
  ) 

problem_data <- profit_data[26,] %>% 
  ungroup() %>% 
  data.table()  %>%
  unnest(profit)

profit_data <- profit_data[-26,] %>% 
  ungroup() %>% 
  data.table()  %>%
  unnest(profit)

final_results <- final_results_clean

det_est_data <- data_cases %>% 
  .[, alignment_case := as.character(alignment_case)] %>%
  rowwise() %>% 
  mutate(
    model_est_data = list(get_est_data(yield_type = ys_type, error = error_degree, case = alignment_case, rates = rate_types, crop_price = c_price, dev_max = max_dev, opt_n = opt_n)),
    # results = list(gen_prof_diff(yield_type = ys_type, error = error_degree, case = alignment_case, rates = rate_types, crop_price = c_price))
  ) %>% 
  ungroup() %>% 
  data.table()  %>%
  unnest(model_est_data)

saveRDS(final_results, here("Results", "deterministic_clean_data_mismatch_nitrogen.rds"))
saveRDS(det_est_data, here("Results", "deterministic_clean_est_mismatch_nitrogen.rds"))
saveRDS(profit_data, here("Results", "deterministic_clean_profit_mismatch_nitrogen.rds"))

final_results <- all_results %>%
  dplyr::select(- coverage_pct_data) %>%
  unnest(sim_data) %>%
  data.table() %>%
  .[, alignment_case := as.character(alignment_case)]

det_est_data <- data_cases %>% 
  .[, alignment_case := as.character(alignment_case)] %>%
  rowwise() %>% 
  mutate(
    model_est_data = list(get_est_data(yield_type = ys_type, error = error_degree, case = alignment_case, rates = rate_types, crop_price = c_price, dev_max = max_dev)),
    # results = list(gen_prof_diff(yield_type = ys_type, error = error_degree, case = alignment_case, rates = rate_types, crop_price = c_price))
  ) %>% 
  ungroup() %>% 
  data.table()  %>%
  unnest(model_est_data)


saveRDS(final_results, here("Results", "deterministic_data_mismatch_nitrogen.rds"))
saveRDS(det_est_data, here("Results", "deterministic_est_mismatch_nitrogen.rds"))


#### Angle and Shift Seed Response ####

trial_grids <- readRDS("Results/trial_grids.rds")
harvester_polygons <- readRDS("Results/harvester_polygons.rds")

# keep the same setup for the machine, treatments and prices

n_price <- .5
s_price <- 4.50 
c_price <- c(5.62)
rate_types <- c("rates_low", "rates_center", "rates_high")
alignment_case <- c("angle", "mis-alignment")
error_degree <- c(0, 10, 30)

ys_function <- c("seed_response")
max_dev <- c(2, 3, 4, 5, 6, 100)

#/*----------------------------------*/
#' ## Create field data cases with coverage pct data
#/*----------------------------------*/
field_data <- expand.grid(
  alignment_case = alignment_case,
  error_degree = error_degree
) %>% 
  tibble() %>% 
  mutate(
    coverage_pct_data = mclapply(
      seq_len(nrow(.)),
      function(i) {
        get_coverage_pct(
          harvester_polygons,
          .$alignment_case[[i]],
          .$error_degree[[i]]
        )
      },
      mc.cores = 3
    )
  )

#/*----------------------------------*/
#' ## Combine with yield function types
#/*----------------------------------*/
data_cases <- tibble(expand.grid(ys_type = ys_function,
                                 rate_types =  rate_types,
                                 c_price = c_price,
                                 max_dev = max_dev)) %>% 
  rowwise() %>% 
  #--- find the optimal N for each ys function ---#
  mutate(
    opt_n = optimize(
      function(x) gen_profit(ys_type, x, c_price, n_price, s_price), 
      interval = c(0, 300), 
      maximum = TRUE
    )$maximum
  ) %>% 
  mutate(
    rates_ls = list(gen_rates(ys_type, x, c_price, n_price, s_price, rate_types))
  ) %>% 
  ungroup() %>% 
  #--- expand grid with field_data ---#
  expand_grid_df(., field_data) %>% 
  data.table()

all_results <- data_cases %>%
  mutate(
    sim_data = mclapply(
      seq_len(nrow(.)),
      function(x) {
        get_sim_data(
          trial_design = trial_grids,
          coverage_data = .$coverage_pct_data[[x]],
          ys_type = .$ys_type[[x]],
          error_degree = .$error_degree[[x]],
          rates_ls = .$rates_ls[[x]],
          opt_n = .$opt_n[[x]]
        )
      },
      mc.cores = 3
    )
  )

clean_results <- data_cases %>%
  mutate(
    clean_data = mclapply(
      seq_len(nrow(.)),
      function(x) {
        get_cleaned_data(
          trial_design = trial_grids,
          coverage_data = .$coverage_pct_data[[x]],
          ys_type = .$ys_type[[x]],
          error_degree = .$error_degree[[x]],
          rates_ls = .$rates_ls[[x]],
          opt_n = .$opt_n[[x]],
          max_dev = .$max_dev[[x]]
        )
      },
      mc.cores = 3
    )
  )

final_results_clean <- clean_results %>%
  dplyr::select(- coverage_pct_data) %>%
  unnest(clean_data) %>%
  data.table() %>%
  .[, alignment_case := as.character(alignment_case)]

final_results <- all_results %>%
  dplyr::select(- coverage_pct_data) %>%
  unnest(sim_data) %>%
  data.table() %>%
  .[, alignment_case := as.character(alignment_case)]

profit_data <- data_cases %>% 
  .[, alignment_case := as.character(alignment_case)] %>%
  rowwise() %>% 
  mutate(
    profit = list(gen_prof_diff(yield_type = ys_type, error = error_degree, case = alignment_case, rates = rate_types, crop_price = c_price, dev_max = max_dev, opt_n = opt_n))
  ) %>% 
  ungroup() %>% 
  data.table()  %>%
  unnest(profit)

final_results <- clean_results %>%
  dplyr::select(- coverage_pct_data) %>%
  unnest(clean_data) %>%
  data.table() %>%
  .[, alignment_case := as.character(alignment_case)]

det_est_data <- data_cases %>% 
  .[, alignment_case := as.character(alignment_case)] %>%
  rowwise() %>% 
  mutate(
    model_est_data = list(get_est_data(yield_type = ys_type, error = error_degree, case = alignment_case, rates = rate_types, crop_price = c_price, dev_max = max_dev, opt_n = opt_n)),
    # results = list(gen_prof_diff(yield_type = ys_type, error = error_degree, case = alignment_case, rates = rate_types, crop_price = c_price))
  ) %>% 
  ungroup() %>% 
  data.table()  %>%
  unnest(model_est_data)

saveRDS(final_results, here("Results", "deterministic_clean_data_others_seed.rds"))
saveRDS(det_est_data, here("Results", "deterministic_clean_est_others_seed.rds"))
saveRDS(profit_data, here("Results", "deterministic_clean_profit_others_seed.rds"))

final_results <- all_results %>%
  dplyr::select(- coverage_pct_data) %>%
  unnest(sim_data) %>%
  data.table() %>%
  .[, alignment_case := as.character(alignment_case)]

det_est_data <- data_cases %>% 
  .[, alignment_case := as.character(alignment_case)] %>%
  rowwise() %>% 
  mutate(
    model_est_data = list(get_est_data(yield_type = ys_type, error = error_degree, case = alignment_case, rates = rate_types, crop_price = c_price, dev_max = max_dev)),
    # results = list(gen_prof_diff(yield_type = ys_type, error = error_degree, case = alignment_case, rates = rate_types, crop_price = c_price))
  ) %>% 
  ungroup() %>% 
  data.table()  %>%
  unnest(model_est_data)

saveRDS(final_results, here("Results", "deterministic_data_others_seed.rds"))
saveRDS(det_est_data, here("Results", "deterministic_est_others_seed.rds"))

##### Angle and Shift Nitrogen Responses #####
trial_grids <- readRDS("Results/trial_grids.rds")
harvester_polygons <- readRDS("Results/harvester_polygons.rds")

n_price <- .5
s_price <- 4.50 
c_price <- c(5.62)

rate_types <- c("rates_low", "rates_center", "rates_high")
alignment_case <- c("angle", "mis-alignment")
error_degree <- c(0, 10, 30)

ys_function <- c("low_response", "middle_response", "high_response")
max_dev <- c(20, 30, 40, 100)

#/*----------------------------------*/
#' ## Create field data cases with coverage pct data
#/*----------------------------------*/
field_data <- expand.grid(
  alignment_case = alignment_case,
  error_degree = error_degree
) %>% 
  tibble() %>% 
  mutate(
    coverage_pct_data = mclapply(
      seq_len(nrow(.)),
      function(i) {
        get_coverage_pct(
          harvester_polygons,
          .$alignment_case[[i]],
          .$error_degree[[i]]
        )
      },
      mc.cores = 3
    )
  )


#/*----------------------------------*/
#' ## Combine with yield function types
#/*----------------------------------*/
data_cases <- tibble(expand.grid(ys_type = ys_function,
                                 rate_types =  rate_types,
                                 c_price = c_price,
                                 max_dev = max_dev)) %>% 
  rowwise() %>% 
  #--- find the optimal N for each ys function ---#
  mutate(
    opt_n = optimize(
      function(x) gen_profit(ys_type, x, c_price, n_price, s_price), 
      interval = c(0, 300), 
      maximum = TRUE
    )$maximum
  ) %>% 
  mutate(
    rates_ls = list(gen_rates(ys_type, x, c_price, n_price, s_price, rate_types))
  ) %>% 
  ungroup() %>% 
  #--- expand grid with field_data ---#
  expand_grid_df(., field_data) %>% 
  data.table()

all_results <- data_cases %>%
  mutate(
    sim_data = mclapply(
      seq_len(nrow(.)),
      function(x) {
        get_sim_data(
          trial_design = trial_grids,
          coverage_data = .$coverage_pct_data[[x]],
          ys_type = .$ys_type[[x]],
          error_degree = .$error_degree[[x]],
          rates_ls = .$rates_ls[[x]],
          opt_n = .$opt_n[[x]]
        )
      },
      mc.cores = 3
    )
  )

clean_results <- data_cases %>%
  mutate(
    clean_data = mclapply(
      seq_len(nrow(.)),
      function(x) {
        get_cleaned_data(
          trial_design = trial_grids,
          coverage_data = .$coverage_pct_data[[x]],
          ys_type = .$ys_type[[x]],
          error_degree = .$error_degree[[x]],
          rates_ls = .$rates_ls[[x]],
          opt_n = .$opt_n[[x]],
          max_dev = .$max_dev[[x]]
        )
      },
      mc.cores = 3
    )
  )

final_results_clean <- clean_results %>%
  dplyr::select(- coverage_pct_data) %>%
  unnest(clean_data) %>%
  data.table() %>%
  .[, alignment_case := as.character(alignment_case)]

final_results <- all_results %>%
  dplyr::select(- coverage_pct_data) %>%
  unnest(sim_data) %>%
  data.table() %>%
  .[, alignment_case := as.character(alignment_case)]

profit_data <- data_cases %>% 
  .[, alignment_case := as.character(alignment_case)] %>%
  rowwise() %>% 
  mutate(
    profit = list(gen_prof_diff(yield_type = ys_type, error = error_degree, case = alignment_case, rates = rate_types, crop_price = c_price, dev_max = max_dev, opt_n = opt_n))
  ) %>% 
  ungroup() %>% 
  data.table()  %>%
  unnest(profit)

profit_data$clean_diff <- sapply(1:nrow(profit_data),
       FUN = function(i){
         profit_diff = ifelse(is.null(profit_data$clean_diff[[i]]) == TRUE, NA, profit_data$clean_diff[[i]])
         return(profit_diff)
       })

profit_data$clean_diff <- sapply(1:nrow(profit_data),
                                 FUN = function(i){
                                   profit_diff = ifelse(is.null(profit_data$clean_diff[[i]]) == TRUE, NA, profit_data$clean_diff[[i]])
                                   return(profit_diff)
                                 })
profit_data$profit_loss_clean <- sapply(1:nrow(profit_data),
                                 FUN = function(i){
                                   profit_diff = ifelse(is.null(profit_data$profit_loss_clean[[i]]) == TRUE, NA, profit_data$profit_loss_clean[[i]])
                                   return(profit_diff)
                                 })
profit_data$perc_clean <- sapply(1:nrow(profit_data),
                                        FUN = function(i){
                                          profit_diff = ifelse(is.null(profit_data$perc_clean[[i]]) == TRUE, NA, profit_data$perc_clean[[i]])
                                          return(profit_diff)
                                        })

final_results <- final_results_clean

det_est_data <- data_cases %>% 
  .[, alignment_case := as.character(alignment_case)] %>%
  rowwise() %>% 
  mutate(
    model_est_data = list(get_est_data(yield_type = ys_type, error = error_degree, case = alignment_case, rates = rate_types, crop_price = c_price, dev_max = max_dev, opt_n = opt_n)),
    # results = list(gen_prof_diff(yield_type = ys_type, error = error_degree, case = alignment_case, rates = rate_types, crop_price = c_price))
  ) %>% 
  ungroup() %>% 
  data.table()  %>%
  unnest(model_est_data)

saveRDS(final_results, here("Results", "deterministic_clean_data_others_nitrogen.rds"))
saveRDS(det_est_data, here("Results", "deterministic_clean_est_others_nitrogen.rds"))
saveRDS(profit_data, here("Results", "deterministic_clean_profit_others_nitrogen.rds"))


final_results <- all_results %>%
  dplyr::select(- coverage_pct_data) %>%
  unnest(sim_data) %>%
  data.table() %>%
  .[, alignment_case := as.character(alignment_case)]

det_est_data <- data_cases %>% 
  .[, alignment_case := as.character(alignment_case)] %>%
  rowwise() %>% 
  mutate(
    model_est_data = list(get_est_data(yield_type = ys_type, error = error_degree, case = alignment_case, rates = rate_types, crop_price = c_price, dev_max = max_dev)),
    # results = list(gen_prof_diff(yield_type = ys_type, error = error_degree, case = alignment_case, rates = rate_types, crop_price = c_price))
  ) %>% 
  ungroup() %>% 
  data.table()  %>%
  unnest(model_est_data)

saveRDS(final_results, here("Results", "deterministic_data_others_nitrogen.rds"))
saveRDS(det_est_data, here("Results", "deterministic_est_others_nitrogen.rds"))

