f_high <- function(x) {
  y = 250 * (1 - exp(-.035 * (20 + x)))
  return(y)
}

f_seed <- function(x) {
  y = 150 + 5.2*x - 0.068*(x^2)
  return(y)
}

final_results <- readRDS(here("Results", "deterministic_clean_data_others_seed.rds"))
det_est_data <- readRDS(here("Results", "deterministic_clean_est_others_seed.rds"))
profit_data <- readRDS(here("Results", "deterministic_clean_profit_others_seed.rds"))

ggplot(data = subset(profit_data, alignment_case == "mis-alignment"), aes(x = error_degree, y = as.numeric(profit_loss_clean), fill = as.factor(max_dev))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(rate_types ~ ys_type)

ggplot(data = profit_data, aes(x = error_degree, y = as.numeric(profit_diff), fill = as.factor(max_dev))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(rate_types ~ ys_type)

ggplot(data = subset(final_results, alignment_case == "mis-alignment" & error_degree == 30), aes(y = yield, x = rate)) +
  geom_count() +
  geom_function(fun = f_seed) +
  geom_line(data = subset(det_est_data, alignment_case == "mis-alignment" & error_degree == 30 & ys_type == "seed_response"), aes(y = yield, x = rate), color = "Red") +
  facet_grid(rate_types ~ max_dev)

# --- #   # --- # # --- # # --- # # --- #
##### Others Nitrogen Graphs #####
# --- #   # --- # # --- # # --- # # --- #

final_results <- readRDS(here("Results", "deterministic_clean_data_others_nitrogen.rds"))
det_est_data <- readRDS(here("Results", "deterministic_clean_est_others_nitrogen.rds"))
profit_data <- readRDS(here("Results", "deterministic_clean_profit_others_nitrogen.rds"))
hist(as.numeric(profit_data$profit_diff))

ggplot(data = subset(profit_data, alignment_case == "mis-alignment"), aes(x = error_degree, y = as.numeric(clean_diff), fill = as.factor(max_dev))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(rate_types ~ ys_type)
hist(as.numeric(profit_data$profit_diff))

ggplot(data = subset(profit_data, alignment_case == "mis-alignment"), aes(x = error_degree, y = as.numeric(profit_loss_clean), fill = as.factor(max_dev))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(rate_types ~ ys_type)

ggplot(data = subset(profit_data, alignment_case == "angle"), aes(x = error_degree, y = as.numeric(profit_loss_clean), fill = as.factor(max_dev))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(rate_types ~ ys_type)


ggplot(data = subset(final_results, ys_type == "high_response" & alignment_case == "mis-alignment" & error_degree == 30), aes(y = yield, x = rate)) +
  geom_count() +
  geom_function(fun = f_high) +
  geom_line(data = subset(det_est_data, ys_type == "high_response" & alignment_case == "mis-alignment" & error_degree == 30), aes(y = yield, x = rate), color = "Red") +
  facet_grid(rate_types ~ max_dev)

# --- #   # --- # # --- # # --- # # --- #
##### Mismatch Seed Graphs #####
# --- #   # --- # # --- # # --- # # --- #

final_results <- readRDS(here("Results", "deterministic_clean_data_mismatch_seed.rds"))
det_est_data <- readRDS(here("Results", "deterministic_clean_est_mismatch_seed.rds"))
profit_data <- readRDS(here("Results", "deterministic_clean_profit_mismatch_seed.rds"))

ggplot(data = profit_data, aes(x = error_degree, y = as.numeric(clean_diff), fill = as.factor(max_dev))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(rate_types ~ ys_type)

ggplot(data = profit_data, aes(x = error_degree, y = as.numeric(profit_loss_clean), fill = as.factor(max_dev))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(rate_types ~ ys_type)

ggplot(data = profit_data, aes(x = error_degree, y = as.numeric(profit_loss), fill = as.factor(max_dev))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(rate_types ~ ys_type)

ggplot(data = subset(final_results, error_degree == 0.75 & ys_type == "seed_response"), aes(y = yield, x = rate)) +
  geom_count() +
  geom_function(fun = f_seed) +
  geom_line(data = subset(det_est_data, error_degree == 0.75 & ys_type == "seed_response"), aes(y = yield, x = rate), color = "Red") +
  facet_grid(rate_types ~ max_dev)
hist(as.numeric(profit_data$profit_diff))

ggplot(data = subset(final_results_clean, alignment_case == "mis-alingment" & error_degree == 30), aes(x = yield, y = input_rate)) +
  geom_count() +
  # geom_line(data = subset(det_est_data, alignment_case == "mis-alingment" & error_degree == 30), aes(x = yield, y = input_rate)) +
  facet_grid(rate_types ~ max_dev)

ggplot(data = subset(final_results_clean, alignment_case == "mis-alingment" & error_degree == 10), aes(x = yield, y = input_rate)) +
  geom_count() +
  # geom_line(data = subset(det_est_data, alignment_case == "mis-alingment" & error_degree == 30), aes(x = yield, y = input_rate)) +
  facet_grid(rate_types ~ max_dev)

# --- #   # --- # # --- # # --- # # --- #
##### Mismatch Nitrogen Graphs #####
# --- #   # --- # # --- # # --- # # --- #

final_results <- readRDS(here("Results", "deterministic_clean_data_mismatch_nitrogen.rds"))
det_est_data <- readRDS(here("Results", "deterministic_clean_est_mismatch_nitrogen.rds"))
profit_data <- readRDS(here("Results", "deterministic_clean_profit_mismatch_nitrogen.rds"))

ggplot(data = profit_data, aes(x = error_degree, y = as.numeric(profit_diff), fill = as.factor(max_dev))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(rate_types ~ ys_type)
ggsave()

ggplot(data = profit_data, aes(x = error_degree, y = as.numeric(profit_loss_clean), fill = as.factor(max_dev))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(rate_types ~ ys_type)
ggsave()

ggplot(data = subset(final_results, error_degree == 0.75 & ys_type == "high_response"), aes(y = yield, x = rate)) +
  geom_count() +
  geom_function(fun = f_high) +
  geom_line(data = subset(det_est_data, error_degree == 0.75 & ys_type == "high_response"), aes(y = yield, x = rate)) +
  facet_grid(rate_types ~ max_dev)

ggplot(data = subset(final_results, error_degree == 1.25 & ys_type == "high_response"), aes(y = yield, x = rate)) +
  geom_count() +
  geom_function(fun = f_high) +
  geom_line(data = subset(det_est_data, error_degree == 1.25 & ys_type == "high_response"), aes(y = yield, x = rate)) +
  facet_grid(rate_types ~ max_dev)

