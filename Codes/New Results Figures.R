# --- #   # --- # # --- # # --- # # --- #
##### Others Seed Graphs #####
# --- #   # --- # # --- # # --- # # --- #

seed_results_others <- readRDS(file = here("Results", paste0("results_", paste0(c("angle", "mis-alignment"), collapse = "_"), "20sd_seed", ".rds")))

seed_results_others <- na.omit(seed_results_others)
seed_results_others$profit_diff <- unlist(seed_results_others$profit_diff)
seed_results_others$profit_diff_clean <- unlist(seed_results_others$profit_diff_clean)
seed_results_others$no_obs_clean <- unlist(seed_results_others$no_obs_clean)
seed_results_others$no_obs_all <- unlist(seed_results_others$no_obs_all)
seed_results_others$rate_clean <- unlist(seed_results_others$rate_clean)
seed_results_others$rate <- unlist(seed_results_others$rate)
seed_results_others$diff <- seed_results_others$profit_diff - seed_results_others$profit_diff_clean

# seed_results_others %>%
mean_table <- seed_results_others[ ,list(mean_obs = mean(no_obs_clean/no_obs_all)), by = list(error_degree, ys_type, alignment_case, max_dev, rate_types)]

# ggplot(data = subset(seed_results_others, ys_type == "seed_response"  & error_degree == 30)) +
#   geom_histogram(aes(x = profit_diff), alpha = 0.5, binwidth = 1) +
#   facet_grid( ~ rate_types, scale = "free")

ggplot(data = subset(seed_results_others, alignment_case == "mis-alignment" & ys_type == "seed_response"  & error_degree != 0)) +
  geom_histogram(aes(x = profit_diff), alpha = 0.5, binwidth = 1) +
  geom_histogram(aes(x = profit_diff_clean, fill = factor(max_dev)), alpha = 0.5, binwidth = 1) +
  facet_grid(error_degree ~ rate_types, scale = "free")

ggplot(data = subset(seed_results_others, alignment_case == "mis-alignment" & ys_type == "seed_response" & error_degree != 0)) +
  geom_histogram(aes(x = profit_diff, fill = factor(error_degree)), alpha = 0.5, binwidth = 1) +
  facet_grid(error_degree ~ rate_types, scale = "free")

ggplot(data = subset(seed_results_others, alignment_case == "mis-alignment" & ys_type == "seed_response" & error_degree != 0)) +
  geom_histogram(aes(x = diff, fill = factor(max_dev)), alpha = 0.5, binwidth = 1) +
  facet_grid(rate_types ~ error_degree, scale = "free")

ggplot(data = subset(seed_results_others, alignment_case == "mis-alignment" & ys_type == "seed_response" & error_degree == 30)) +
  geom_histogram(aes(x = diff), alpha = 0.5, binwidth = 1) +
  geom_text(y = 0.5, aes(x = 0, label = paste0("No = ", mean_obs)),
            data = subset(mean_table, alignment_case == "mis-alignment" & ys_type == "seed_response" & error_degree == 30)) +
  facet_grid(rate_types ~ max_dev, scale = "free") +
  xlim(-40, 20)

# --- #   # --- # # --- # # --- # # --- #
##### Mismatch Seed Graphs #####
# --- #   # --- # # --- # # --- # # --- #
seed_results_mismatch <- readRDS(file = here("Results", paste0("results_", paste0(c("mismatch"), collapse = "_"), "20sd_seed", ".rds")))
seed_results_mismatch <- na.omit(seed_results_mismatch)
seed_results_mismatch$profit_diff <- unlist(seed_results_mismatch$profit_diff)
seed_results_mismatch$profit_diff_clean <- unlist(seed_results_mismatch$profit_diff_clean)
seed_results_mismatch$no_obs_clean <- unlist(seed_results_mismatch$no_obs_clean)
seed_results_mismatch$no_obs_all <- unlist(seed_results_mismatch$no_obs_all)
seed_results_mismatch$rate_clean <- unlist(seed_results_mismatch$rate_clean)
seed_results_mismatch$rate <- unlist(seed_results_mismatch$rate)
seed_results_mismatch$diff <- seed_results_mismatch$profit_diff - seed_results_mismatch$profit_diff_clean

mean_table_mismatch <- seed_results_mismatch[ ,list(mean_obs = mean(no_obs_clean/no_obs_all)), by = list(error_degree, ys_type, alignment_case, max_dev, rate_types)]

ggplot(data = subset(seed_results_mismatch, ys_type == "seed_response"  & error_degree != 1)) +
  geom_histogram(aes(x = profit_diff), alpha = 0.5, binwidth = 1) +
  geom_histogram(aes(x = profit_diff_clean, fill = factor(max_dev)), alpha = 0.5, binwidth = 1) +
  facet_grid(error_degree ~ rate_types, scale = "free")

ggplot(data = subset(seed_results_mismatch, ys_type == "seed_response" & error_degree != 1)) +
  geom_histogram(aes(x = profit_diff, fill = factor(error_degree)), alpha = 0.5, binwidth = 1) +
  facet_grid(error_degree ~ rate_types, scale = "free")

ggplot(data = subset(seed_results_mismatch, ys_type == "seed_response" & error_degree != 1)) +
  geom_histogram(aes(x = diff, fill = factor(max_dev)), alpha = 0.5, binwidth = 1) +
  facet_grid(rate_types ~ error_degree, scale = "free")

ggplot(data = subset(seed_results_mismatch,  ys_type == "seed_response" & error_degree == 0.75)) +
  geom_histogram(aes(x = diff), alpha = 0.5, binwidth = 1) +
  geom_text(y = 0.5, aes(x = 0, label = paste0("No = ", mean_obs)),
            data = subset(mean_table_mismatch,  ys_type == "seed_response" & error_degree == 0.75)) +
  facet_grid(rate_types ~ max_dev, scale = "free") +
  xlim(-40, 20)

# --- #   # --- # # --- # # --- # # --- #
##### Mismatch Nitrogen Graphs #####
# --- #   # --- # # --- # # --- # # --- #

results_mismatch <- readRDS(file = here("Results", paste0("results_", paste0(c("mismatch"), collapse = "_"), "20sd", ".rds")))
results_mismatch <- na.omit(results_mismatch)
results_mismatch$profit_diff <- unlist(results_mismatch$profit_diff)
results_mismatch$profit_diff_clean <- unlist(results_mismatch$profit_diff_clean)
results_mismatch$no_obs_clean <- unlist(results_mismatch$no_obs_clean)
results_mismatch$no_obs_all <- unlist(results_mismatch$no_obs_all)
results_mismatch$rate_clean <- unlist(results_mismatch$rate_clean)
results_mismatch$rate <- unlist(results_mismatch$rate)
results_mismatch$diff <- results_mismatch$profit_diff - results_mismatch$profit_diff_clean

mean_table <- results_mismatch[ ,list(mean_obs = mean(no_obs_clean/no_obs_all)), by = list(error_degree, ys_type, alignment_case, max_dev, rate_types)]

# ggplot(data = subset(seed_results_others, ys_type == "seed_response"  & error_degree == 30)) +
#   geom_histogram(aes(x = profit_diff), alpha = 0.5, binwidth = 1) +
#   facet_grid( ~ rate_types, scale = "free")

ggplot(data = subset(results_mismatch, ys_type == "high_response"  & error_degree != 1)) +
  geom_histogram(aes(x = profit_diff), alpha = 0.5, binwidth = 1) +
  geom_histogram(aes(x = profit_diff_clean, fill = factor(max_dev)), alpha = 0.5, binwidth = 1) +
  facet_grid(error_degree ~ rate_types, scale = "free")

ggplot(data = subset(results_mismatch, ys_type == "high_response" & error_degree != 1)) +
  geom_histogram(aes(x = profit_diff, fill = factor(error_degree)), alpha = 0.5, binwidth = 1) +
  facet_grid(error_degree ~ rate_types, scale = "free")

ggplot(data = subset(results_mismatch, ys_type == "high_response" & error_degree != 1)) +
  geom_histogram(aes(x = diff, fill = factor(max_dev)), alpha = 0.5, binwidth = 1) +
  facet_grid(rate_types ~ error_degree, scale = "free")

ggplot(data = subset(results_mismatch,  ys_type == "high_response" & error_degree == 0.75)) +
  geom_histogram(aes(x = diff), alpha = 0.5, binwidth = 1) +
  geom_text(y = 0.5, aes(x = 0, label = paste0("No = ", mean_obs)),
            data = subset(mean_table, ys_type == "high_response" & error_degree == 0.75)) +
  facet_grid(rate_types ~ max_dev, scale = "free") 

ggplot(data = subset(results_mismatch,  ys_type == "low_response" & error_degree == 0.75)) +
  geom_histogram(aes(x = diff), alpha = 0.5, binwidth = 1) +
  geom_text(y = 0.5, aes(x = 0, label = paste0("No = ", mean_obs)),
            data = subset(mean_table, ys_type == "low_response" & error_degree == 0.75)) +
  facet_grid(rate_types ~ max_dev, scale = "free") 

# --- #   # --- # # --- # # --- # # --- #
##### Others Nitrogen Graphs #####
# --- #   # --- # # --- # # --- # # --- #

results_others <- readRDS(file = here("Results", paste0("results_", paste0(c("angle", "mis-alignment"), collapse = "_"), "20sd", ".rds")))
results_others <- na.omit(results_others)
results_others$profit_diff <- unlist(results_others$profit_diff)
results_others$profit_diff_clean <- unlist(results_others$profit_diff_clean)
results_others$no_obs_clean <- unlist(results_others$no_obs_clean)
results_others$no_obs_all <- unlist(results_others$no_obs_all)
results_others$rate_clean <- unlist(results_others$rate_clean)
results_others$rate <- unlist(results_others$rate)
results_others$diff <- results_others$profit_diff - results_others$profit_diff_clean

mean_table <- results_others[ ,list(mean_obs = mean(no_obs_clean/no_obs_all)), by = list(error_degree, ys_type, alignment_case, max_dev, rate_types)]

ggplot(data = subset(results_others, ys_type == "high_response"  & error_degree != 0)) +
  geom_histogram(aes(x = profit_diff), alpha = 0.5, binwidth = 1) +
  geom_histogram(aes(x = profit_diff_clean, fill = factor(max_dev)), alpha = 0.5, binwidth = 1) +
  facet_grid(error_degree ~ rate_types, scale = "free")

ggplot(data = subset(results_others, ys_type == "high_response" & error_degree != 0)) +
  geom_histogram(aes(x = diff, fill = factor(max_dev)), alpha = 0.5, binwidth = 1) +
  facet_grid(rate_types ~ error_degree, scale = "free")

ggplot(data = subset(results_others, alignment_case == "mis-alignment" & ys_type == "high_response" & error_degree == 30)) +
  geom_histogram(aes(x = diff), alpha = 0.5, binwidth = 1) +
  geom_text(y = 0.5, aes(x = 0, label = paste0("No = ", mean_obs)),
            data = subset(mean_table, alignment_case == "mis-alignment" & ys_type == "high_response" & error_degree == 30)) +
  facet_grid(rate_types ~ max_dev, scale = "free") +
  xlim(-40, 20)


ggplot(data = subset(results_others, max_dev == 30 & ys_type == "high_response" & error_degree != 0)) +
  geom_histogram(aes(x = diff), alpha = 0.5, binwidth = 1) +
  facet_grid(rate_types ~ error_degree, scale = "free")

ggplot(data = subset(results_others, max_dev == 15 & ys_type == "high_response" & error_degree != 0)) +
  geom_histogram(aes(x = diff), alpha = 0.5, binwidth = 1) +
  facet_grid(rate_types ~ error_degree, scale = "free")

ggplot(data = subset(results_others, ys_type == "high_response" & error_degree != 0)) +
  geom_histogram(aes(x = profit_diff), alpha = 0.5, binwidth = 1) +
  facet_grid(rate_types ~ error_degree, scale = "free")

