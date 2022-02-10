#### creating datasets ####
trial_grids <- readRDS("Results/trial_grids.rds")
harvester_polygons <- readRDS("Results/harvester_polygons.rds")

n_price <- .5
s_price <- 4.50 
c_price <- c(5.62)

rate_types <- c("rates_center")
alignment_case <- c("angle")
error_degree <- c(0, 10)

ys_function <- c("high_response")
max_dev <- c(100)

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
data_0 <- all_results$sim_data[[1]]
data_10 <- all_results$sim_data[[2]]
hist(data_10$tot_pct)

min(data_0$rate, na.rm = TRUE)
max(data_0$rate, na.rm = TRUE)

min(data_10$rate, na.rm = TRUE)
max(data_10$rate, na.rm = TRUE)

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
# est_data_0 <- subset(det_est_data, error_degree == 0)
# est_data_10 <- subset(det_est_data, error_degree == 10)
# min(est_data_0$rate, na.rm = TRUE)
# min(est_data_10$rate, na.rm = TRUE)


saveRDS(final_results, here("Results", "deterministic_clean_data_others_nitrogen.rds"))
saveRDS(det_est_data, here("Results", "deterministic_clean_est_others_nitrogen.rds"))
saveRDS(profit_data, here("Results", "deterministic_clean_profit_others_nitrogen.rds"))


f_high <- function(x) {
  y = 250 * (1 - exp(-.035 * (20 + x)))
  return(y)
}

ggplot(data = subset(final_results, ys_type == "high_response" & alignment_case == "angle" & error_degree == 10 & rate_types == "rates_center"), aes(y = yield, x = rate)) +
  geom_point() +
  geom_function(fun = f_high) +
  geom_line(data = subset(det_est_data, ys_type == "high_response" & alignment_case == "angle" & error_degree == 10 & rate_types == "rates_center"), aes(y = yield, x = rate), color = "Red") +
  # facet_grid( ~ max_dev) +
  xlim(40, 182)
no_error_data <- subset(det_est_data, ys_type == "high_response" & alignment_case == "angle" & error_degree == 0 & rate_types == "rates_center")[1,]

#### conclusions about issue ####
# The problem was coming from observations without complete coverage in the yield polygon
# so some values did not lie on the line of linear combinations
# Now only the observations with 0.99 of the yield polygon covered are included