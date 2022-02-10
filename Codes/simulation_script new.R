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
library(dplyr)
library(R.utils)

#--- source functions ---#
source(here("Codes", "functions new.R"))

#--- github ---#
source("https://raw.githubusercontent.com/brittanikedge/DIFM/main/Functions.R")

boundary <- st_read(here("Data/Wendte_LaueLib80_2020/Raw", "boundary.shp")) %>%
  st_transform_utm()

abline <- st_read(here("Data/Wendte_LaueLib80_2020/Raw", "ab-line.shp")) %>%
  st_transform_utm()
#
# # perhaps these are not in feet but meters
# trial_grids <- make_trial_grids(field = boundary,
#                                    ab_line = abline,
#                                    plot_width = 18.288,
#                                    cell_height = 2,
#                                    headland_length = 0) %>%
#   mutate(td_grid_id = paste0(strip_id, "_", cell_id))
# # saveRDS(trial_grids, "Data/Wendte_LaueLib80_2020/Raw/trial-grids-mismatch.rds")

# harvest_grids <- make_trial_grids(field = boundary,
#                                 ab_line = abline,
#                                 plot_width = 18.288,
#                                 cell_height = 2,
#                                 headland_length = 0) %>%
#   mutate(td_grid_id = paste0(strip_id, "_", cell_id))
# saveRDS(harvest_grids, "Data/Wendte_LaueLib80_2020/Raw/harvester-polygons-mismatch.rds")

trial_grids <- readRDS("Data/Wendte_LaueLib80_2020/Raw/trial-grids-mismatch.rds")
trial_grids <- dplyr::rename(trial_grids, "td_grid_id" = "td_grd_", "strip_id" = "strip_d")
harvester_polygons <- readRDS("Data/Wendte_LaueLib80_2020/Raw/harvester-polygons-mismatch.rds")

# trial_grids <- readRDS("Results/trial_grids.rds")
# harvester_polygons <- readRDS("Results/harvester_polygons.rds")

##### Parameters #####
# keep the same setup for the machine, treatments and prices

n_price <- .5
s_price <- 4.50 

c_price <- c(5.62)
# ys_function <- c("low_response", "middle_response", "high_response", "seed_response")
ys_function <- c("low_response", "middle_response", "high_response")
# ys_function <- c("low_response")
# ys_function <- c("middle_response")
# ys_function <- c("high_response")
# ys_function <- c("seed_response")

rate_types <- c("rates_low", "rates_center", "rates_high")

# alignment_case <- c("angle", "mis-alignment")
# error_degree <- c(0, 10, 30)
# max_dev <- c(2, 4, 6)
max_dev <- c(20, 30, 40)
error_degree <- c(.75, 1, 1.25)
alignment_case <- c("mismatch")

#/*----------------------------------*/
#' ## Create field data cases with coverage pct data
#/*----------------------------------*/

    # get_coverage_pct(
    #   harvester_polygons,
    #   field_data$alignment_case[[1]],
    #   field_data$error_degree[[1]]
    # )

  
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

# checking the weights #
# perc_data <- field_data %>%
#   unnest(coverage_pct_data) %>%
#   data.table() %>%
#   .[, alignment_case := as.character(alignment_case)]
# 
# ggplot(data = subset(perc_data)) +
#   geom_histogram(aes(x = pct)) +
#   facet_grid(error_degree ~ alignment_case)

# get_coverage_pct(
#   harvester_polygons,
#   field_data$alignment_case[[1]],
#   field_data$error_degree[[1]]
# )

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

# subset <- data_cases[7,]
# subset <- data_cases[10,]
# 
#   hist(subset$coverage_pct_data[[1]]$pct)
#   hist(subset$coverage_pct_data[[1]]$pct)

#/*=================================================*/
#' # Simulations
#/*=================================================*/
# test2 <- run_sim(
# num_iterations = 10,
# trial_design = trial_grids,
# coverage_data = data_cases$coverage_pct_data[[46]],
# ys_type = data_cases$ys_type[[46]],
# error_degree = data_cases$error_degree[[46]],
# rates_ls = data_cases$rates_ls[[46]],
# opt_n = data_cases$opt_n[[46]],
# c_price = data_cases$c_price[[46]],
# max_dev = 3
# )

all_results1 <- data_cases[1:20,] %>%
  mutate(
    results = mclapply(
      seq_len(nrow(.)),
      function(x) {
        run_sim(
          num_iterations = 100,
          trial_design = trial_grids,
          coverage_data = .$coverage_pct_data[[x]],
          ys_type = .$ys_type[[x]],
          error_degree = .$error_degree[[x]],
          rates_ls = .$rates_ls[[x]],
          opt_n = .$opt_n[[x]],
          c_price = .$c_price[[x]],
          max_dev = .$max_dev[[x]]
         )
      },
      mc.cores = 3
    ) 
  )
saveRDS(all_results1, file = here("Results", paste0("results_", paste0(alignment_case, collapse = "_"), "20sd_group1", ".rds")))

all_results2 <- data_cases[21:40,] %>%
  mutate(
    results = mclapply(
      seq_len(nrow(.)),
      function(x) {
        run_sim(
          num_iterations = 100,
          trial_design = trial_grids,
          coverage_data = .$coverage_pct_data[[x]],
          ys_type = .$ys_type[[x]],
          error_degree = .$error_degree[[x]],
          rates_ls = .$rates_ls[[x]],
          opt_n = .$opt_n[[x]],
          c_price = .$c_price[[x]],
          max_dev = .$max_dev[[x]]
        )
      },
      mc.cores = 3
    ) 
  )
saveRDS(all_results2, file = here("Results", paste0("results_", paste0(alignment_case, collapse = "_"), "20sd_group2", ".rds")))

all_results3 <- data_cases[41:60,] %>%
  mutate(
    results = mclapply(
      seq_len(nrow(.)),
      function(x) {
        run_sim(
          num_iterations = 100,
          trial_design = trial_grids,
          coverage_data = .$coverage_pct_data[[x]],
          ys_type = .$ys_type[[x]],
          error_degree = .$error_degree[[x]],
          rates_ls = .$rates_ls[[x]],
          opt_n = .$opt_n[[x]],
          c_price = .$c_price[[x]],
          max_dev = .$max_dev[[x]]
        )
      },
      mc.cores = 3
    ) 
  )
saveRDS(all_results3, file = here("Results", paste0("results_", paste0(alignment_case, collapse = "_"), "20sd_group3", ".rds")))

all_results4 <- data_cases[61:81,] %>%
  mutate(
    results = mclapply(
      seq_len(nrow(.)),
      function(x) {
        run_sim(
          num_iterations = 100,
          trial_design = trial_grids,
          coverage_data = .$coverage_pct_data[[x]],
          ys_type = .$ys_type[[x]],
          error_degree = .$error_degree[[x]],
          rates_ls = .$rates_ls[[x]],
          opt_n = .$opt_n[[x]],
          c_price = .$c_price[[x]],
          max_dev = .$max_dev[[x]]
        )
      },
      mc.cores = 3
    ) 
  )
saveRDS(all_results4, file = here("Results", paste0("results_", paste0(alignment_case, collapse = "_"), "20sd_group4", ".rds")))

all_results <- rbind(all_results1, all_results2, all_results3, all_results4)

final_results <- all_results %>% 
  dplyr::select(- coverage_pct_data) %>% 
  unnest(results) %>% 
  data.table() %>% 
  .[, alignment_case := as.character(alignment_case)]

final_results$profit_diff <- unlist(final_results$profit_diff)
final_results$profit_diff_clean <- unlist(final_results$profit_diff_clean)
final_results$no_obs_clean <- unlist(final_results$no_obs_clean)
final_results$no_obs_all <- unlist(final_results$no_obs_all)
final_results$rate_clean <- unlist(final_results$rate_clean)
final_results$rate <- unlist(final_results$rate)

# mean(as.numeric(final_results$profit_diff_clean), na.rm = TRUE)
# sum(ifelse(final_results$profit_diff_clean == " ", 1, 0))

# # 
# sum(ifelse((is.na(final_results$profit_diff_clean) == TRUE | is.na(final_results$profit_diff) == TRUE), 1, 0))/nrow(final_results)
# 
# hist(final_results$profit_diff)
# hist(final_results$profit_diff_clean)
# final_results$profit_diff <- ifelse(final_results_test$profit_diff == " ", NA, final_results_test$profit_diff)
# final_results_test$profit_diff_clean <- ifelse(final_results_test$profit_diff_clean == " ", NA, final_results_test$profit_diff_clean)
final_results$diff <- as.numeric(final_results$profit_diff) - as.numeric(final_results$profit_diff_clean)


# saveRDS(final_results, file = here("Results", paste0("results_", paste0(alignment_case, collapse = "_"), "20sd", ".rds")))
saveRDS(final_results, file = here("Results", paste0("results_", paste0(alignment_case, collapse = "_"), "20sd", ".rds")))

final_results[,
              .(rate = round(mean(rate, na.rm = TRUE)), opt_n = round(mean(opt_n)), diff = round(mean(diff, na.rm = TRUE), 2)),
              by = .(ys_type, error_degree, alignment_case, rate_types, max_dev)
]

final_results_no_xy[,
              .(rate = round(mean(rate, na.rm = TRUE)), opt_n = round(mean(opt_n)), diff = round(mean(diff, na.rm = TRUE), 2)),
              by = .(ys_type, error_degree, alignment_case, rate_types, max_dev)
]

final_results_10[,
                    .(rate = round(mean(rate, na.rm = TRUE)), opt_n = round(mean(opt_n)), diff = round(mean(diff, na.rm = TRUE), 2)),
                    by = .(ys_type, error_degree, alignment_case, rate_types, max_dev)
]

final_results_w_xy[,
                   .(rate = round(mean(rate, na.rm = TRUE)), opt_n = round(mean(opt_n)), profit_diff = round(mean(profit_diff_clean, na.rm = TRUE), 2)),
                   by = .(ys_type, error_degree, alignment_case, rate_types, max_dev)
]

final_results_no_xy[,
                    .(rate = round(mean(rate, na.rm = TRUE)), opt_n = round(mean(opt_n)), profit_diff = round(mean(profit_diff_clean, na.rm = TRUE), 2)),
                    by = .(ys_type, error_degree, alignment_case, rate_types, max_dev)
]


# ggplot(data = subset(mean_data, error_degree == 30 & alignment_case == "mis-alignment" ), aes(x = ys_type, y=profit_diff)) +
#   geom_bar(stat="identity") +
#   facet_grid(rate_types ~ c_price) +
#   scale_x_discrete(limits = y_functions, labels = c('Seed','High','Mid', 'Low')) +
#   xlab("Yield Function") + ylab("Profit Loss")
# 
# ggplot(data = subset(mean_data, error_degree == 10 & alignment_case == "angle" ), aes(x = ys_type, y=profit_diff)) +
#   geom_bar(stat="identity") +
#   facet_grid(rate_types ~ c_price) +
#   scale_x_discrete(limits = y_functions, labels = c('Seed','High','Mid', 'Low')) +
#   xlab("Yield Function") + ylab("Profit Loss")

# ggplot(data = final_results) +
#   geom_density(aes(x = rate, fill = factor(error_degree)), alpha = 0.3) +
#   facet_grid(ys_type ~ alignment_case, scale = "free")
# ggsave(
#   here("Results", paste0("density_rate_", paste0(alignment_case, collapse = "_"), ".jpg"))
# )
#' 

ggplot(data = final_results_w_xy) +
  geom_histogram(aes(x = profit_diff), alpha = 0.5) +
  geom_histogram(aes(x = profit_diff_clean, fill = factor(max_dev)), alpha = 0.5) +
  facet_grid(rate_types ~ error_degree, scale = "free")

ggplot(data = final_results_no_xy) +
  geom_histogram(aes(x = profit_diff), alpha = 0.5) +
  geom_histogram(aes(x = profit_diff_clean, fill = factor(max_dev)), alpha = 0.5) +
  facet_grid(rate_types ~ error_degree, scale = "free")

ggplot(data = subset(final_results, ys_type == "seed_response" & alignment_case == "mismatch" & error_degree != 1)) +
  geom_histogram(aes(x = profit_diff, fill = factor(error_degree)), alpha = 0.5) +
  facet_grid( ~ rate_types, scale = "free")
quantile(subset(final_results, error_degree == 0.75 & rate_types == "rates_high")$profit_diff, 0.5, na.rm = TRUE)

ggplot(data = subset(final_results, ys_type == "seed_response" & alignment_case == "mismatch" & error_degree != 1)) +
  geom_histogram(aes(x = diff, fill = factor(max_dev)), alpha = 0.5) +
  facet_grid(rate_types ~ error_degree, scale = "free")
quantile(subset(final_results, error_degree == 0.75 & rate_types == "rates_high")$diff, 0.5, na.rm = TRUE)

seed_results_others <- readRDS(file = here("Results", paste0("results_", paste0(c("angle", "mis-alignment"), collapse = "_"), "20sd_seed", ".rds")))

ggplot(data = subset(final_results, ys_type == "seed_response" & alignment_case == "mismatch" & error_degree != 1)) +
  geom_histogram(aes(x = profit_diff), alpha = 0.5) +
  geom_histogram(aes(x = profit_diff_clean, fill = factor(max_dev)), alpha = 0.5) +
  facet_grid(rate_types ~ error_degree, scale = "free")

ggplot(data = subset(final_results, ys_type == "seed_response" & alignment_case == "mismatch" & error_degree != 1)) +
  geom_histogram(aes(x = profit_diff, fill = factor(error_degree)), alpha = 0.5) +
  facet_grid( ~ rate_types, scale = "free")
quantile(subset(final_results, error_degree == 0.75 & rate_types == "rates_high")$profit_diff, 0.5, na.rm = TRUE)

ggplot(data = subset(final_results, ys_type == "seed_response" & alignment_case == "mismatch" & error_degree != 1)) +
  geom_histogram(aes(x = diff, fill = factor(max_dev)), alpha = 0.5) +
  facet_grid(rate_types ~ error_degree, scale = "free")
quantile(subset(final_results, error_degree == 0.75 & rate_types == "rates_high")$diff, 0.5, na.rm = TRUE)

ggplot(data = subset(final_results, ys_type == "seed_response" & alignment_case == "mis-alignment" & error_degree != 0)) +
  geom_density(aes(x = rate, fill = factor(error_degree)), alpha = 0.3) +
  facet_grid(rate_types ~ c_price, scale = "free")


ggplot(data = subset(final_results, ys_type == "high_response" & alignment_case == "mis-alignment" & error_degree != 0)) +
  geom_density(aes(x = profit_diff, fill = factor(error_degree)), alpha = 0.3) +
  facet_grid(rate_types ~ c_price, scale = "free")

ggplot(data = subset(final_results, ys_type == "high_response" & alignment_case == "mis-alignment" & error_degree != 0)) +
  geom_density(aes(x = rate, fill = factor(error_degree)), alpha = 0.3) +
  facet_grid(rate_types ~ c_price, scale = "free")

mismatch_results <- readRDS(here("Results", paste0("results_", paste0("mismatch", collapse = "_"), "20sd", ".rds")))
mean_data_mismatch <- mismatch_results[, 
                           .(rate = round(mean(rate, na.rm = TRUE)), opt_n = round(mean(opt_n)), profit_diff = round(mean(profit_diff, na.rm = TRUE), 2)), 
                           by = .(ys_type, error_degree, alignment_case, rate_types, c_price)
]

ggplot(data = subset(mismatch_results,  c_price == 5.62)) +
  geom_histogram(aes(x = profit_diff, fill = factor(error_degree)), alpha = 0.5) +
  geom_vline(data = subset(mean_data_mismatch, c_price == 5.62), 
             aes(xintercept = profit_diff, color = factor(error_degree)), linetype="dashed") +
  facet_grid(rate_types ~ ys_type, scale = "free")

quantile(subset(mismatch_results, 
                error_degree == .75 & c_price == 5.62 & ys_type == "seed_response")$profit_diff,
         0.1)

ggplot(data = subset(mismatch_results, c_price == 5.62)) +
  geom_histogram(aes(x = rate, fill = factor(error_degree)), alpha = 0.5) +
  geom_vline(data = subset(mean_data_mismatch, c_price == 5.62), 
             aes(xintercept = rate, color = factor(error_degree)), linetype="dashed") +
  geom_vline(data = subset(data_cases, c_price == 5.62), 
             aes(xintercept = opt_n), color = "Black") +
  facet_grid(rate_types ~ ys_type, scale = "free")

other_results <- readRDS(here("Results", paste0("results_", paste0(c("angle", "mis-alignment"), collapse = "_"), "20sd", ".rds")))

mean_data <- other_results[, 
                           .(rate = round(mean(rate, na.rm = TRUE)), opt_n = round(mean(opt_n)), profit_diff = round(mean(profit_diff, na.rm = TRUE), 2)), 
                           by = .(ys_type, error_degree, alignment_case, rate_types, c_price)
]
ggplot(data = subset(other_results, alignment_case == "angle"  & c_price == 5.62), color = error_degree) +
  geom_histogram(aes(x = profit_diff, fill = factor(error_degree)), alpha = 0.5) +
  geom_vline(data = subset(mean_data, alignment_case == "angle"  & c_price == 5.62), 
             aes(xintercept = profit_diff, color = factor(error_degree)), linetype="dashed") +
  facet_grid(rate_types ~ ys_type, scale = "free")

ggplot(data = subset(other_results, alignment_case == "mis-alignment" & c_price == 5.62)) +
  geom_histogram(aes(x = profit_diff, fill = factor(error_degree)), alpha = 0.5) +
  geom_vline(data = subset(mean_data, alignment_case == "mis-alignment"  & c_price == 5.62), 
             aes(xintercept = profit_diff, color = factor(error_degree)), linetype="dashed") +
  facet_grid(rate_types ~ ys_type, scale = "free")

ggplot(data = subset(other_results, alignment_case == "mis-alignment"  & c_price == 5.62)) +
  geom_histogram(aes(x = rate, fill = factor(error_degree)), alpha = 0.5) +
  geom_vline(data = subset(mean_data,  alignment_case == "mis-alignment"  & c_price == 5.62), 
             aes(xintercept = rate, color = factor(error_degree)), linetype="dashed") +
  geom_vline(data = subset(data_cases, alignment_case == "mis-alignment"  & c_price == 5.62), 
             aes(xintercept = opt_n), color = "Black") +
  facet_grid(rate_types ~ ys_type, scale = "free")

quantile(subset(other_results, 
                error_degree == 30 & c_price == 5.62 & ys_type == "seed_response" & alignment_case == "mis-alignment" & rate_types == "rates_center")$profit_diff,
         0.1)
quantile(subset(other_results, 
                error_degree == 30 & c_price == 5.62 & ys_type == "seed_response" & alignment_case == "mis-alignment" & rate_types == "rates_high")$profit_diff,
         0.1)
quantile(subset(other_results, 
                error_degree == 30 & c_price == 5.62 & ys_type == "seed_response" & alignment_case == "mis-alignment" & rate_types == "rates_low")$profit_diff,
         0.1)


mean_data_mismatch <- mismatch_results[, 
                           .(rate = round(mean(rate, na.rm = TRUE)), opt_n = round(mean(opt_n)), profit_diff = round(mean(profit_diff, na.rm = TRUE), 2)), 
                           by = .(ys_type, error_degree, alignment_case, rate_types, c_price)
]

ggplot(data = subset(mean_data_mismatch, error_degree == 0.75 & alignment_case == "mismatch" ), aes(x = ys_type, y=profit_diff)) +
  geom_bar(stat="identity") +
  facet_grid(rate_types ~ c_price) +
  scale_x_discrete(limits = y_functions, labels = c('Seed','High','Mid', 'Low')) +
  xlab("Yield Function") + ylab("Profit Loss")

ggplot(data = subset(mean_data_mismatch, error_degree == 1.25 & alignment_case == "mismatch" ), aes(x = ys_type, y=profit_diff)) +
  geom_bar(stat="identity") +
  facet_grid(rate_types ~ c_price) +
  scale_x_discrete(limits = y_functions, labels = c('Seed','High','Mid', 'Low')) +
  xlab("Yield Function") + ylab("Profit Loss")
#' 
#' 
#' 
#' #/*----------------------------------*/
#' #' ## Graphs of weights and misalignments
#' #/*----------------------------------*/
#' 
perc_data <- data_cases %>%
  dplyr::select(- c(ys_type, opt_n, rates_ls)) %>%
  unnest(coverage_pct_data) %>%
  data.table() %>%
  .[, alignment_case := as.character(alignment_case)]

ggplot(data = perc_data) +
  geom_histogram(aes(x = pct)) +
  facet_grid(error_degree ~ alignment_case)
#' 
#' ggsave(
#'   "perc_hml_mismatch.jpg"
#' )
