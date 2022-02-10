num_iterations = 5
trial_design = trial_grids
coverage_data = data_cases$coverage_pct_data[[21]]
ys_type = data_cases$ys_type[[21]]
error_degree = data_cases$error_degree[[39]]
rates_ls = data_cases$rates_ls[[39]]
opt_n = data_cases$opt_n[[39]]
c_price = data_cases$c_price[[39]]
max_dev = data_cases$max_dev[[39]]

time_test <- function(trial_design, coverage_data, ys_type, error_degree, rates_ls, opt_n, c_price, max_dev){
start_time <- Sys.time()
### make the data ###
exp_design <- assign_rates(
  filter(trial_design, td_grid_id != "headland"), 
  rates_ls
) %>% 
  data.table() %>% 
  .[, .(td_grid_id, rate)]

#--- need to add spatially correlated errors ---#
if (ys_type == "seed_response"){
  design_whole <- exp_design %>% 
    # .[, yield := gen_yield(ys_type, rate)]
    .[, yield := gen_yield(ys_type, rate) + rnorm(nrow(.), sd = 20)] %>%
    .[, rate := rate + rnorm(nrow(.), sd = 1)]
}else{
  design_whole <- exp_design %>% 
    # .[, yield := gen_yield(ys_type, rate)]
    .[, yield := gen_yield(ys_type, rate) + rnorm(nrow(.), sd = 20)] %>%
    .[, rate := rate + rnorm(nrow(.), sd = 7)]
  
}

### Add spatial errors to yield on trial grid ### 
sp_range <- 400

xy <- dplyr::select(trial_design, td_grid_id) %>%
  cbind(., st_coordinates(st_centroid(.))) %>%
  st_drop_geometry() %>%
  data.table()

#--- m_error ---#
m_error <- gen_coefs(
  mean = 0,
  psill = 0.828,
  range = sp_range,
  coef_name = "m_error_uncorrelated",
  nsim = 1,
  xy = xy
)

design_whole <- merge(design_whole, m_error, by = "td_grid_id")

design_whole <- design_whole %>% 
  .[, yield := yield + m_error_uncorrelated]
end_time <- Sys.time()
time_datasetup <- end_time - start_time

#### Data Processing ####
start_time <- Sys.time()
if(ys_type == "seed_response"){
  good_guys <- design_whole[coverage_data, on = "td_grid_id"] %>% 
    .[, tot_sub_pct := sum(pct), by = harvester_id] %>%
    .[, input_weighted := sum(pct * rate), by = harvester_id] %>%
    .[, dev_input_rate := sum(abs(pct * (rate - input_weighted) / tot_sub_pct)), by = harvester_id] %>%
    filter(dev_input_rate < max_dev) %>%
    select(c(harvester_id)) %>%
    unique(by = "harvester_id")
}else{
  good_guys <- design_whole[coverage_data, on = "td_grid_id"] %>% 
    .[, tot_sub_pct := sum(pct), by = harvester_id] %>%
    .[, input_weighted := sum(pct * rate), by = harvester_id] %>%
    .[, dev_input_rate := sum(abs(pct * (rate - input_weighted) / tot_sub_pct)), by = harvester_id] %>%
    filter(dev_input_rate < max_dev) %>%
    select(c(harvester_id)) %>%
    unique(by = "harvester_id")
}

good_coverage_data  <- filter(coverage_data, harvester_id %in% good_guys[, harvester_id])

if(error_degree == 1 | error_degree == 0){
  reg_data_clean <- design_whole[good_coverage_data, on = "td_grid_id"] %>% 
    .[, .SD[which.max(pct)], by=harvester_id] %>%
    mutate(yield = gen_yield(ys_type, rate))
}else{
  reg_data_clean <- design_whole[good_coverage_data, on = "td_grid_id"] %>% 
    .[, .(
      rate = sum(pct * rate), 
      yield = sum(pct * yield),
      X = mean(X),
      Y = mean (Y)),
      by = harvester_id] 
}

if(error_degree == 1 | error_degree == 0){
  reg_data <- design_whole[coverage_data, on = "td_grid_id"] %>%
    .[, .SD[which.max(pct)], by = harvester_id] %>%
    mutate(yield = gen_yield(ys_type, rate))
}else{
  reg_data <- design_whole[coverage_data, on = "td_grid_id"] %>%
    .[, .(
      rate = sum(pct * rate),
      yield = sum(pct * yield),
      X = mean(X),
      Y = mean (Y)
    ), by = harvester_id]
}

obs_clean <- nrow(reg_data_clean)
obs_total <- nrow(reg_data)
end_time <- Sys.time()
time_regdata <- end_time - start_time

##### Estimations 
## cleaned dataset ##
start_time <- Sys.time()
if (ys_type != "seed_response"){
  scam_res_clean <- try(withTimeout(scam(yield ~ s(rate, k = 4, bs = "micv"), data = reg_data_clean, optimizer =  "nlm.fd"),
                                    timeout = 60,
                                    onTimeout = "silent"))
}else{
  scam_res_clean <- try(withTimeout(scam(yield ~ s(rate, k = 4, bs = "cv"), data = reg_data_clean, optimizer =  "nlm.fd"),
                                    timeout = 60,
                                    onTimeout = "silent"))
}

## full dataset
if (ys_type != "seed_response"){
  scam_res <- try(withTimeout(scam(yield ~ s(rate, k = 4, bs = "micv"), data = reg_data, optimizer =  "nlm.fd"),
                              timeout = 60,
                              onTimeout = "silent"))
}else{
  scam_res <- try(withTimeout(scam(yield ~ s(rate, k = 4, bs = "cv"), data = reg_data, optimizer =  "nlm.fd"),
                              timeout = 60,
                              onTimeout = "silent"))
}
end_time <- Sys.time()
time_scam <- end_time - start_time

start_time <- Sys.time()
## now estimating the optimal nitrogen  
if("scam" %in% class(scam_res) == TRUE & "scam" %in% class(scam_res_clean) == TRUE){
  if(ys_type == "seed_response") {
    opt_n_est <- data.table(
      rate = seq(quantile(reg_data$rate, probs = .025, na.rm = TRUE),
                 quantile(reg_data$rate, probs = .975, na.rm = TRUE),
                 by = 1)
    ) %>% 
      .[, X := median(reg_data$X)] %>% 
      .[, Y := median(reg_data$Y)] %>% 
      .[, y_hat := predict(scam_res, newdata = .)] %>% 
      .[, p_hat := y_hat * c_price - rate * s_price] %>%
      .[, .SD[which.max(p_hat)]] %>% 
      .[, .(rate)]
    
    opt_n_est_clean <- data.table(
      rate = seq(quantile(reg_data_clean$rate, probs = .025, na.rm = TRUE),
                 quantile(reg_data_clean$rate, probs = .975, na.rm = TRUE),
                 by = 1)
    ) %>% 
      .[, X := median(reg_data_clean$X)] %>% 
      .[, Y := median(reg_data_clean$Y)] %>% 
      .[, y_hat := predict(scam_res_clean, newdata = .)] %>% 
      .[, p_hat := y_hat * c_price - rate * s_price] %>%
      .[, .SD[which.max(p_hat)]] %>% 
      .[, .(rate)]
  }else{
    opt_n_est <- data.table(
      rate = seq(quantile(reg_data$rate, probs = .025, na.rm = TRUE),
                 quantile(reg_data$rate, probs = .975, na.rm = TRUE),
                 by = 1)
    ) %>% 
      .[, X := median(reg_data$X)] %>% 
      .[, Y := median(reg_data$Y)] %>% 
      .[, y_hat := predict(scam_res, newdata = .)] %>% 
      .[, p_hat := y_hat * c_price - rate * n_price] %>%
      .[, .SD[which.max(p_hat)]] %>% 
      .[, .(rate)]
    
    opt_n_est_clean <- data.table(
      rate = seq(quantile(reg_data_clean$rate, probs = .025, na.rm = TRUE),
                 quantile(reg_data_clean$rate, probs = .975, na.rm = TRUE),
                 by = 1)
    ) %>% 
      .[, X := median(reg_data_clean$X)] %>% 
      .[, Y := median(reg_data_clean$Y)] %>% 
      .[, y_hat := predict(scam_res_clean, newdata = .)] %>% 
      .[, p_hat := y_hat * c_price - rate * n_price] %>%
      .[, .SD[which.max(p_hat)]] %>% 
      .[, .(rate)]
  }
  
  prof_est <- gen_profit(ys_type, opt_n_est, c_price, n_price, s_price)
  prof_opt <- gen_profit(ys_type, opt_n, c_price, n_price, s_price)
  profit_diff <- prof_opt - prof_est
  
  prof_est <- gen_profit(ys_type, opt_n_est_clean, c_price, n_price, s_price)
  prof_opt <- gen_profit(ys_type, opt_n, c_price, n_price, s_price)
  profit_diff_clean <- prof_opt - prof_est
  
}else if("scam" %in% class(scam_res) == TRUE & "scam" %in% class(scam_res_clean) == FALSE){
  if(ys_type == "seed_response") {
    opt_n_est <- data.table(
      rate = seq(quantile(reg_data$rate, probs = .025, na.rm = TRUE),
                 quantile(reg_data$rate, probs = .975, na.rm = TRUE),
                 by = 1)
    ) %>% 
      .[, X := median(reg_data$X)] %>% 
      .[, Y := median(reg_data$Y)] %>% 
      .[, y_hat := predict(scam_res, newdata = .)] %>% 
      .[, p_hat := y_hat * c_price - rate * s_price] %>%
      .[, .SD[which.max(p_hat)]] %>% 
      .[, .(rate)]
    
    prof_est <- gen_profit(ys_type, opt_n_est, c_price, n_price, s_price)
    prof_opt <- gen_profit(ys_type, opt_n, c_price, n_price, s_price)
    profit_diff <- prof_opt - prof_est
    
    opt_n_est_clean <- NA
    profit_diff_clean <- NA
    
  }else{
    opt_n_est <- data.table(
      rate = seq(quantile(reg_data$rate, probs = .025, na.rm = TRUE),
                 quantile(reg_data$rate, probs = .975, na.rm = TRUE),
                 by = 1)
    ) %>% 
      .[, X := median(reg_data$X)] %>% 
      .[, Y := median(reg_data$Y)] %>% 
      .[, y_hat := predict(scam_res, newdata = .)] %>% 
      .[, p_hat := y_hat * c_price - rate * n_price] %>%
      .[, .SD[which.max(p_hat)]] %>% 
      .[, .(rate)]
  }
  prof_est <- gen_profit(ys_type, opt_n_est, c_price, n_price, s_price)
  prof_opt <- gen_profit(ys_type, opt_n, c_price, n_price, s_price)
  profit_diff <- prof_opt - prof_est
  
  opt_n_est_clean <- NA
  profit_diff_clean <- NA
}else{
  opt_n_est <- NA
  profit_diff <- NA
  opt_n_est_clean <- NA
  profit_diff_clean <- NA
}
end_time <- Sys.time()
time_opt <- end_time - start_time

total_time <- time_datasetup + time_regdata + time_scam + time_opt

results <- as.data.frame(matrix(c(total_time, time_datasetup, time_regdata, time_scam, time_opt), ncol = 5, nrow = 1)) %>%
  setNames(., c("total time", "data setup time", "processing time", "scam est time", "optimal calc time"))

return(results)
}

time_results <- data_cases %>%
  mutate(
    time_test = mclapply(
      seq_len(nrow(.)),
      function(x) {
        time_test(
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

time_test_results <- time_results %>% 
  dplyr::select(- coverage_pct_data) %>% 
  unnest(time_test) %>% 
  data.table() %>% 
  .[, alignment_case := as.character(alignment_case)]

mean(time_test_results$`total time`)
(((2.685875 * 300000) / 60) / 60) / 24

time_test_results$perc_scam <- time_test_results$`scam est time`/time_test_results$`total time`
mean(time_test_results$perc_scam)

mean(time_test_results$`data setup time`)
sd(time_test_results$`data setup time`)

mean(time_test_results$`processing time`)
sd(time_test_results$`processing time`)

mean(time_test_results$`scam est time`)
sd(time_test_results$`scam est time`)

mean(time_test_results$`optimal calc time`)
sd(time_test_results$`optimal calc time`)
