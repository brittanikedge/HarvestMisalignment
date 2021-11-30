det_est_data_others <- det_est_data
final_results_others <- final_results

# changing yield response types

high_data <- ggplot(subset(final_results_others, ys_type == "high_response" & c_price == 5.62 & alignment_case == "mis-alignment" & error_degree == 30 & rate_types == "rates_center"), aes(rate, yield)) +
  geom_count() +
  geom_line(data = subset(det_est_data_others, ys_type == "high_response" & c_price == 5.62 & alignment_case == "mis-alignment" & error_degree == 30 & rate_types == "rates_center"), aes(rate, yield), color = "Blue") +
  geom_function(fun = f_high) +
  xlab("Input Rate") +
  ylab("Yield") + 
  ylim(230, 250) +
  theme(legend.position="none")

seed_data <- ggplot(subset(final_results_others, ys_type == "seed_response" & c_price == 5.62 & alignment_case == "mis-alignment" & error_degree == 30 & rate_types == "rates_center"), aes(rate, yield)) +
  geom_count() +
  geom_line(data = subset(det_est_data_others, ys_type == "seed_response" & c_price == 5.62 & alignment_case == "mis-alignment" & error_degree == 30 & rate_types == "rates_center"), aes(rate, yield), color = "Blue") +
  geom_function(fun = f_seed) +
  xlab("Input Rate") +
  ylab("Yield") + 
  theme(legend.position="none")


f_low <- function(x){
  y = 250 * (1 - exp(-.009 * (160 + x)))
  return(y)
}

low_data <- ggplot(subset(final_results_others, ys_type == "low_response" & c_price == 5.62 & alignment_case == "mis-alignment" & error_degree == 30 & rate_types == "rates_center"), aes(rate, yield)) +
  geom_count() +
  geom_line(data = subset(det_est_data_others, ys_type == "low_response" & c_price == 5.62 & alignment_case == "mis-alignment" & error_degree == 30 & rate_types == "rates_center"), aes(rate, yield), color = "Blue") +
  geom_function(fun = f_low) +
  xlab("Input Rate") +
  ylab("Yield") 


f_mid <- function(x){
  y = 250 * (1 - exp(-.02 * (45 + x)))
  return(y)
}

mid_data <- ggplot(subset(final_results_others, ys_type == "middle_response" & c_price == 5.62 & alignment_case == "mis-alignment" & error_degree == 30 & rate_types == "rates_center"), aes(rate, yield)) +
  geom_count() +
  geom_line(data = subset(det_est_data_others, ys_type == "middle_response" & c_price == 5.62 & alignment_case == "mis-alignment" & error_degree == 30 & rate_types == "rates_center"), aes(rate, yield), color = "Blue") +
  geom_function(fun = f_mid) +
  xlab("Input Rate") +
  ylab("Yield") + 
  theme(legend.position="none")

  (seed_data | high_data | mid_data | low_data)


# just change rates
# use mis-alignment 30-foot high-response and change rates

ggplot(subset(final_results_others, ys_type == "high_response" & c_price == 5.62 & alignment_case == "mis-alignment" & error_degree == 30), aes(rate, yield)) +
  geom_count() +
  geom_line(data = subset(det_est_data_others, ys_type == "high_response" & c_price == 5.62 & alignment_case == "mis-alignment" & error_degree == 30), aes(rate, yield), color = "Blue") +
  geom_function(fun = f_high) +
  facet_grid(cols = vars(rate_types)) +
  ylim(230, 250) +
  xlab("Input Rate") +
  ylab("Yield") 


# change error levels

ggplot(subset(final_results_others, rate_types == "rates_center" & ys_type == "high_response" & c_price == 5.62 & alignment_case == "mis-alignment"), aes(rate, yield)) +
  geom_count() +
  geom_line(data = subset(det_est_data_others, rate_types == "rates_center" & ys_type == "high_response" & c_price == 5.62 & alignment_case == "mis-alignment"), aes(rate, yield), color = "Blue") +
  geom_function(fun = f_high) +
  facet_grid(cols = vars(error_degree)) +
  xlab("Input Rate") +
  ylab("Yield") 


# change misalignment type
# need to add misalignment types together 
data_mismatch <- subset(det_est_data, rate_types == "rates_center" & ys_type == "high_response" & c_price == 5.62 & error_degree == 0.75)
results_mismatch <- subset(final_results, rate_types == "rates_center" & ys_type == "high_response" & c_price == 5.62 & error_degree == 0.75)

data_others <- subset(det_est_data_others, rate_types == "rates_center" & ys_type == "high_response" & c_price == 5.62 & error_degree == 30)
results_others <- subset(final_results_others, rate_types == "rates_center" & ys_type == "high_response" & c_price == 5.62 & error_degree == 30)

data_all <- rbind(data_others, data_mismatch)
results_all <- rbind(results_others, results_mismatch)

ggplot(results_all, aes(rate, yield)) +
  geom_count() +
  geom_line(data = data_all, aes(rate, yield), color = "Blue") +
  geom_function(fun = f_high) +
  facet_grid(cols = vars(alignment_case)) +
  ylim(230, 250) +
  xlab("Input Rate") +
  ylab("Yield") 

