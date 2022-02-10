# --- #   # --- # # --- # # --- # # --- #
##### Mismatch Nitrogen Graphs #####
# --- #   # --- # # --- # # --- # # --- #

results_mismatch <- readRDS(file = here("Results", paste0("results_", paste0(c("mismatch"), collapse = "_"), "20sd", ".rds")))
results_mismatch <- na.omit(results_mismatch)
results_mismatch$profit_diff <- as.numeric(unlist(results_mismatch$profit_diff))
results_mismatch$profit_diff_clean <- as.numeric(unlist(results_mismatch$profit_diff_clean))
results_mismatch$no_obs_clean <- as.numeric(unlist(results_mismatch$no_obs_clean))
results_mismatch$no_obs_all <- as.numeric(unlist(results_mismatch$no_obs_all))
results_mismatch$rate_clean <- as.numeric(unlist(results_mismatch$rate_clean))
results_mismatch$rate <- as.numeric(unlist(results_mismatch$rate))
results_mismatch$diff <- as.numeric(unlist(results_mismatch$diff))
results_mismatch$rate_diff <- results_mismatch$rate_clean - results_mismatch$opt_n
results_mismatch$rate_diff_raw <- results_mismatch$rate - results_mismatch$opt_n
results_mismatch$rate_diff_clean <- results_mismatch$rate_clean - results_mismatch$rate 

levels(results_mismatch$rate_types) <- c("Low Rates", "Centered Rates", "High Rates")
levels(results_mismatch$ys_type) <- c("Low Curvature", "Middle Curvature", "High Curvature")


mean_table <- results_mismatch[ ,list(mean_obs = mean(no_obs_clean/no_obs_all)), by = list(error_degree, ys_type, alignment_case, max_dev, rate_types)]
results_mismatch_means <- results_mismatch[ ,list(mean_diff = mean(diff)), by = list(error_degree, ys_type, alignment_case, max_dev, rate_types)]
results_mismatch_means_rate <- results_mismatch[ ,list(mean_rate_diff = mean(rate_diff_cleaning)), by = list(error_degree, ys_type, alignment_case, max_dev, rate_types)]
results_mismatch_means_profit_diff <- results_mismatch[ ,list(mean_diff = mean(profit_diff)), by = list(error_degree, ys_type, alignment_case, max_dev, rate_types)]
results_mismatch_means_profit_diff_clean <- results_mismatch[ ,list(mean_diff = mean(profit_diff_clean)), by = list(error_degree, ys_type, alignment_case, max_dev, rate_types)]

results_mismatch_means_raw <- results_mismatch[ ,list(mean_diff = mean(profit_diff)), by = list(error_degree, ys_type, alignment_case, rate_types)]

results_mismatch_means_rate_raw <- results_mismatch[ ,list(mean_rate_diff_raw = mean(rate_diff_raw)), by = list(error_degree, ys_type, alignment_case, rate_types, max_dev)]

results_mismatch_means_rate_clean <- results_mismatch[ ,list(mean_rate_clean = mean(rate_clean)), by = list(error_degree, ys_type, alignment_case, rate_types, max_dev)]
results_mismatch_means_rate <- results_mismatch[ ,list(mean_rate = mean(rate)), by = list(error_degree, ys_type, alignment_case, rate_types, max_dev)]


ggplot(data = subset(results_mismatch_means_profit_diff_clean, error_degree != 1), aes(x = as.factor(error_degree), y = as.numeric(mean_diff), fill = as.factor(max_dev))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(rate_types ~ ys_type) + 
  xlab("Error Level") +
  ylab("Profit Difference from Optimal") + 
  guides(fill = guide_legend(title = "Maximum Deviation \nParameter"))

### No cleaning Profit Losses
ggplot(data = subset(results_mismatch_means_raw, error_degree != 1), aes(x = as.factor(error_degree), y = as.numeric(mean_diff), fill = as.factor(ys_type))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(~ rate_types) +
  xlab("Error Level") +
  ylab("Profit Difference from Optimal") + 
  guides(fill = guide_legend(title = "Yield Response"))
  
### No cleaning Optimal Rates 
ggplot(data = subset(results_mismatch_means_rate_raw, error_degree != 1), aes(x = as.factor(error_degree), y = as.numeric(mean_rate_diff), fill = as.factor(ys_type))) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Error Level") +
  ylab("Difference between Estimated and True Optimal Rate") + 
  facet_grid(~ rate_types) +
  guides(fill = guide_legend(title = "Yield Response"))

ggplot(data = subset(results_mismatch, error_degree != 1)) +
  geom_density(aes(x = diff, fill = factor(error_degree)), alpha = 0.7, binwidth = 1) +
  facet_grid(rate_types ~ ys_type, scale = "free") +
  xlab("Profit Difference from Optimal") +
  ylab("Density") +
  guides(fill = guide_legend(title = "Error Level"))
  
## With Cleaning Graphs
ggplot(data = subset(results_mismatch_means, error_degree != 1), aes(x = as.factor(error_degree), y = as.numeric(mean_diff), fill = as.factor(max_dev))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(rate_types ~ ys_type) + 
  xlab("Error Level") +
  ylab("Profit Difference from Optimal") + 
  guides(fill = guide_legend(title = "Maximum Deviation \nParameter"))

ggplot(data = subset(results_mismatch_means_rate, error_degree != 1), aes(x = as.factor(error_degree), y = as.numeric(mean_rate_diff), fill = as.factor(max_dev))) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Error Level") +
  ylab("Difference between Clean and Raw Estimated Optimal Input") + 
  facet_grid(rate_types ~ ys_type) +
  guides(fill = guide_legend(title = "Maximum Deviation \nParameter"))

# mismatch profit difference density 
ggplot(data = subset(results_mismatch, error_degree == 0.75)) +
  geom_density(aes(x = diff, fill = factor(max_dev)), alpha = 0.7, binwidth = 1) +
  xlab("Density") +
  ylab("Profit Difference from Cleaning") + 
  facet_grid(ys_type ~ rate_types, scale = "free") +
  guides(fill = guide_legend(title = "Maximum Deviation \nParameter"))

# mismatch profit difference density 
ggplot(data = subset(results_mismatch,  error_degree == 1.25)) +
  geom_density(aes(x = diff, fill = factor(max_dev)), alpha = 0.7, binwidth = 1) +
  xlab("Density") +
  ylab("Profit Difference from Cleaning") + 
  facet_grid(ys_type ~ rate_types, scale = "free") +
  guides(fill = guide_legend(title = "Maximum Deviation \nParameter"))

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
results_others$profit_diff <- as.numeric(unlist(results_others$profit_diff))
results_others$profit_diff_clean <- as.numeric(unlist(results_others$profit_diff_clean))
results_others$no_obs_clean <- as.numeric(unlist(results_others$no_obs_clean))
results_others$no_obs_all <- as.numeric(unlist(results_others$no_obs_all))
results_others$rate_clean <- as.numeric(unlist(results_others$rate_clean))
results_others$rate <- as.numeric(unlist(results_others$rate))
results_others$diff <- as.numeric(unlist(results_others$diff))
results_others$rate_diff_cleaning <- results_others$rate_clean - results_others$rate


levels(results_others$rate_types) <- c("Low Rates", "Centered Rates", "High Rates")
levels(results_others$ys_type) <- c("Low Curvature", "Middle Curvature", "High Curvature")

mean_table <- results_others[ ,list(mean_obs = mean(no_obs_clean/no_obs_all)), by = list(error_degree, ys_type, alignment_case, max_dev, rate_types)]
results_others_means <- results_others[ ,list(mean_diff = mean(diff)), by = list(error_degree, ys_type, alignment_case, max_dev, rate_types)]
results_others_means_rate <- results_others[ ,list(mean_rate_diff = mean(rate_diff_cleaning)), by = list(error_degree, ys_type, alignment_case, max_dev, rate_types)]

results_others_means_rate_clean <- results_others[ ,list(mean_rate_clean = mean(rate_clean)), by = list(error_degree, ys_type, alignment_case, rate_types, max_dev)]
results_others_means_rate <- results_others[ ,list(mean_rate = mean(rate)), by = list(error_degree, ys_type, alignment_case, rate_types, max_dev)]


results_others_means_profit_clean <- results_others[ ,list(profit_diff_clean = mean(profit_diff_clean)), by = list(error_degree, ys_type, alignment_case, rate_types, max_dev)]
results_others_means_profit <- results_others[ ,list(profit_diff = mean(profit_diff)), by = list(error_degree, ys_type, alignment_case, rate_types, max_dev)]


##### No cleaning Graphs #####
results_others_means_raw <- results_others[ ,list(mean_diff = mean(profit_diff)), by = list(error_degree, ys_type, alignment_case, rate_types)]
results_others_means_rate_raw <- results_others[ ,list(mean_rate_diff = mean(rate_diff_raw)), by = list(error_degree, ys_type, alignment_case, rate_types)]

### No cleaning Profit Losses Angle
ggplot(data = subset(results_others_means_raw, alignment_case == "angle" & error_degree != 0), aes(x = as.factor(error_degree), y = as.numeric(mean_diff), fill = as.factor(ys_type))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(~ rate_types) +
  xlab("Error Level") +
  ylab("Profit Difference from Optimal") + 
  guides(fill = guide_legend(title = "Yield Response"))

ggplot(data = subset(results_others, alignment_case == "angle" & error_degree != 0)) +
  geom_density(aes(x = profit_diff, fill = factor(error_degree)), alpha = 0.7, binwidth = 1) +
  facet_grid(rate_types ~ ys_type, scale = "free") +
  xlab("Profit Difference from Optimal") +
  ylab("Density") +
  guides(fill = guide_legend(title = "Error Level"))


### No cleaning Optimal Rates Angle
ggplot(data = subset(results_others_means_rate_raw, alignment_case == "angle" & error_degree != 0), aes(x = as.factor(error_degree), y = as.numeric(mean_rate_diff), fill = as.factor(ys_type))) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Error Level") +
  ylab("Difference between Estimated and True Optimal Rate") + 
  facet_grid(~ rate_types) +
  guides(fill = guide_legend(title = "Yield Response"))

ggplot(data = subset(results_others, alignment_case == "angle" & error_degree != 0)) +
  geom_density(aes(x = rate_diff_raw, fill = factor(error_degree)), alpha = 0.7, binwidth = 1) +
  facet_grid(rate_types ~ ys_type, scale = "free") +
  xlab("Difference between Clean and Raw Estimated Optimal Rate") +
  ylab("Density") +
  guides(fill = guide_legend(title = "Error Level"))

### No cleaning Profit Losses
ggplot(data = subset(results_others_means_raw, alignment_case == "mis-alignment" & error_degree != 0), aes(x = as.factor(error_degree), y = as.numeric(mean_diff), fill = as.factor(ys_type))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(~ rate_types) +
  xlab("Error Level") +
  ylab("Profit Difference from Optimal") + 
  guides(fill = guide_legend(title = "Yield Response"))

ggplot(data = subset(results_others, alignment_case == "mis-alignment" & error_degree != 0)) +
  geom_density(aes(x = profit_diff, fill = factor(ys_type)), alpha = 0.7, binwidth = 1) +
  facet_grid(rate_types ~ error_degree, scale = "free") +
  xlab("Profit Difference from Optimal") +
  ylab("Density") +
  guides(fill = guide_legend(title = "Yield Response"))

### No cleaning Optimal Rates 
ggplot(data = subset(results_others_means_rate_raw, alignment_case == "mis-alignment" & error_degree != 0), aes(x = as.factor(error_degree), y = as.numeric(mean_rate_diff), fill = as.factor(ys_type))) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Error Level") +
  ylab("Difference between Estimated and True Optimal Rate") + 
  facet_grid(~ rate_types) +
  guides(fill = guide_legend(title = "Yield Response"))

ggplot(data = subset(results_others, alignment_case == "mis-alignment" & error_degree != 0)) +
  geom_density(aes(x = rate_diff_raw, fill = factor(ys_type)), alpha = 0.7, binwidth = 1) +
  facet_grid(rate_types ~ error_degree, scale = "free") +
  xlab("Difference between Estimated and True Optimal Rate") +
  ylab("Density") +
  guides(fill = guide_legend(title = "Yield Response"))

##### Cleaning Graphs #####

# misalignment profit difference 
ggplot(data = subset(results_others_means, error_degree != 0 & alignment_case == "mis-alignment"), aes(x = as.factor(error_degree), y = as.numeric(mean_diff), fill = as.factor(max_dev))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(rate_types ~ ys_type) +
  xlab("Error Level") +
  ylab("Profit Difference from Optimal") + 
  guides(fill = guide_legend(title = "Maximum Deviation \nParameter"))

# misalignment rate difference
ggplot(data = subset(results_others_means_rate, error_degree != 0 & alignment_case == "mis-alignment"), aes(x = as.factor(error_degree), y = as.numeric(mean_rate_diff), fill = as.factor(max_dev))) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Error Level") +
  ylab("Difference between Clean and Raw Estimated Optimal Input") + 
  facet_grid(rate_types ~ ys_type) +
  guides(fill = guide_legend(title = "Maximum Deviation \nParameter"))

# angle profit difference
ggplot(data = subset(results_others_means, error_degree != 0 & alignment_case == "angle"), aes(x = as.factor(error_degree), y = as.numeric(mean_diff), fill = as.factor(max_dev))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(rate_types ~ ys_type) +
  xlab("Error Level") +
  ylab("Profit Difference from Optimal") + 
  guides(fill = guide_legend(title = "Maximum Deviation \nParameter"))

# angle rate difference
ggplot(data = subset(results_others_means_rate, error_degree != 0 & alignment_case == "angle"), aes(x = as.factor(error_degree), y = as.numeric(mean_rate_diff), fill = as.factor(max_dev))) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Error Level") +
  ylab("Difference between Clean and Raw Estimated Optimal Input") + 
  facet_grid(rate_types ~ ys_type) +
  guides(fill = guide_legend(title = "Maximum Deviation \nParameter"))

# shift profit difference density 
ggplot(data = subset(results_others, alignment_case == "mis-alignment" & error_degree == 30)) +
  geom_density(aes(x = diff, fill = factor(max_dev)), alpha = 0.7, binwidth = 1) +
  xlab("Density") +
  ylab("Profit Difference from Cleaning") + 
  facet_grid(ys_type ~ rate_types, scale = "free") +
  guides(fill = guide_legend(title = "Maximum Deviation \nParameter"))

# shift profit difference density 
ggplot(data = subset(results_others, alignment_case == "mis-alignment" & error_degree == 10)) +
  geom_density(aes(x = diff, fill = factor(max_dev)), alpha = 0.7, binwidth = 1) +
  xlab("Density") +
  ylab("Profit Difference from Cleaning") + 
  facet_grid(ys_type ~ rate_types, scale = "free") +
  guides(fill = guide_legend(title = "Maximum Deviation \nParameter"))

# angle profit difference density 
ggplot(data = subset(results_others, alignment_case == "angle" & error_degree == 30)) +
  geom_density(aes(x = diff, fill = factor(max_dev)), alpha = 0.7, binwidth = 1) +
  xlab("Density") +
  ylab("Profit Difference from Cleaning") + 
  facet_grid(ys_type ~ rate_types, scale = "free") +
  guides(fill = guide_legend(title = "Maximum Deviation \nParameter"))

# angle profit difference density 
ggplot(data = subset(results_others, alignment_case == "angle" & error_degree == 10)) +
  geom_density(aes(x = diff, fill = factor(max_dev)), alpha = 0.7, binwidth = 1) +
  xlab("Density") +
  ylab("Profit Difference from Cleaning") + 
  facet_grid(ys_type ~ rate_types, scale = "free") +
  guides(fill = guide_legend(title = "Maximum Deviation \nParameter"))




