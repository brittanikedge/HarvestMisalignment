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

f_high <- function(x) {
  y = 250 * (1 - exp(-.035 * (20 + x)))
  return(y)
}

f_seed <- function(x) {
  y = 150 + 5.2*x - 0.068*(x^2)
  return(y)
}

#### Others Seed Graphs ####
final_results <- readRDS(here("Results", "deterministic_clean_data_others_seed.rds"))
det_est_data <- readRDS(here("Results", "deterministic_clean_est_others_seed.rds"))
profit_data <- readRDS(here("Results", "deterministic_clean_profit_others_seed.rds"))

ggplot(data = subset(profit_data, alignment_case == "mis-alignment"), aes(x = error_degree, y = as.numeric(profit_loss_clean), fill = as.factor(max_dev))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(rate_types ~ ys_type)

ggplot(data = subset(profit_data, alignment_case == "angle"), aes(x = error_degree, y = as.numeric(profit_loss_clean), fill = as.factor(max_dev))) +
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

## check on angle data

ggplot(data = subset(final_results, ys_type == "high_response" & alignment_case == "angle" & error_degree == 30 & rate_types == "rates_center"), aes(y = yield, x = rate)) +
  geom_point() +
  geom_point(data = subset(det_est_data, ys_type == "high_response" & alignment_case == "angle" & error_degree == 0 & rate_types == "rates_center"), aes(y = yield, x = rate), color = "Blue") +
  geom_function(fun = f_high) +
  geom_line(data = subset(det_est_data, ys_type == "high_response" & alignment_case == "angle" & error_degree == 30 & rate_types == "rates_center"), aes(y = yield, x = rate), color = "Red") +
  facet_grid( ~ max_dev) +
  xlim(40, 182)

ggplot(data = subset(profit_data, alignment_case == "mis-alignment"), aes(x = error_degree, y = as.numeric(profit_loss_clean), fill = as.factor(max_dev))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(rate_types ~ ys_type)

ggplot(data = subset(profit_data, alignment_case == "angle"), aes(x = error_degree, y = as.numeric(profit_loss_clean), fill = as.factor(max_dev))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(rate_types ~ ys_type)

ggplot(data = subset(profit_data, alignment_case == "mis-alignment"), aes(x = error_degree, y = as.numeric(rate_est_clean), fill = as.factor(max_dev))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(rate_types ~ ys_type)

ggplot(data = subset(profit_data, alignment_case == "mis-alignment"), aes(x = error_degree, y = as.numeric(profit_loss_clean), fill = as.factor(max_dev))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(rate_types ~ ys_type)

ggplot(data = subset(profit_data, alignment_case == "angle"), aes(x = error_degree, y = as.numeric(profit_loss_clean), fill = as.factor(max_dev))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(rate_types ~ ys_type)

ggplot(data = subset(final_results, ys_type == "high_response" & alignment_case == "mis-alignment" & error_degree == 30), aes(y = yield, x = rate)) +
  geom_count() +
  geom_function(fun = f_high) +
  geom_line(data = subset(det_est_data, ys_type == "high_response" & alignment_case == "mis-alignment" & error_degree == 30), aes(y = yield, x = rate), color = "Blue") +
  facet_grid(rate_types ~ max_dev)

ggplot(data = subset(final_results, ys_type == "high_response" & alignment_case == "mis-alignment" & error_degree == 10), aes(y = yield, x = rate)) +
  geom_count() +
  geom_function(fun = f_high) +
  geom_line(data = subset(det_est_data, ys_type == "high_response" & alignment_case == "mis-alignment" & error_degree == 10), aes(y = yield, x = rate), color = "Blue") +
  facet_grid(rate_types ~ max_dev)

ggplot(data = subset(final_results, ys_type == "high_response" & alignment_case == "angle" & error_degree == 30), aes(y = yield, x = rate)) +
  geom_point() +
  geom_function(fun = f_high) +
  geom_line(data = subset(det_est_data, ys_type == "high_response" & alignment_case == "angle" & error_degree == 30), aes(y = yield, x = rate), color = "Blue") +
  facet_grid(rate_types ~ max_dev)

ggplot(data = subset(final_results, ys_type == "high_response" & alignment_case == "angle" & error_degree == 10), aes(y = yield, x = rate)) +
  geom_point() +
  geom_function(fun = f_high) +
  geom_line(data = subset(det_est_data, ys_type == "high_response" & alignment_case == "angle" & error_degree == 10), aes(y = yield, x = rate), color = "Red") +
  facet_grid(rate_types ~ max_dev)

results_others_means <- results_others[ ,list(mean_diff = mean(diff)), by = list(error_degree, ys_type, alignment_case, max_dev, rate_types)]

## graph for cleaning example ##
results_others_mean_perc <- profit_data[ ,list(perc_data = mean(perc_clean)), by = list(error_degree, ys_type, alignment_case, max_dev, rate_types)]

ggplot(data = subset(final_results, rate_types == "rates_center" & ys_type == "high_response" & alignment_case == "mis-alignment" & error_degree == 30), aes(y = yield, x = rate)) +
  geom_count() +
  geom_function(fun = f_high) +
  geom_line(data = subset(det_est_data, rate_types == "rates_center" & ys_type == "high_response" & alignment_case == "mis-alignment" & error_degree == 30), aes(y = yield, x = rate), color = "Blue") +
  geom_label(y = 225, aes(x = 75, label = paste0("Perc Data = ", perc_clean)),
            data = subset(profit_data, rate_types == "rates_center" & alignment_case == "mis-alignment" & ys_type == "high_response" & error_degree == 30)) +
  facet_grid( ~ max_dev)

# --- #   # --- # # --- # # --- # # --- #
##### Mismatch Seed Graphs #####
# --- #   # --- # # --- # # --- # # --- #

final_results <- readRDS(here("Results", "deterministic_clean_data_mismatch_seed.rds"))
det_est_data <- readRDS(here("Results", "deterministic_clean_est_mismatch_seed.rds"))
profit_data <- readRDS(here("Results", "deterministic_clean_profit_mismatch_seed.rds"))

ggplot(data = profit_data, aes(x = error_degree, y = as.numeric(profit_loss_clean), fill = as.factor(max_dev))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(rate_types ~ ys_type)

ggplot(data = subset(final_results, error_degree == 0.75 & ys_type == "seed_response"), aes(y = yield, x = rate)) +
  geom_count() +
  geom_function(fun = f_seed) +
  geom_line(data = subset(det_est_data, error_degree == 0.75 & ys_type == "seed_response"), aes(y = yield, x = rate), color = "Blue") +
  facet_grid(rate_types ~ max_dev)
hist(as.numeric(profit_data$profit_diff))


# --- #   # --- # # --- # # --- # # --- #
##### Mismatch Nitrogen Graphs #####
# --- #   # --- # # --- # # --- # # --- #

final_results <- readRDS(here("Results", "deterministic_clean_data_mismatch_nitrogen.rds"))
det_est_data <- readRDS(here("Results", "deterministic_clean_est_mismatch_nitrogen.rds"))
profit_data <- readRDS(here("Results", "deterministic_clean_profit_mismatch_nitrogen.rds"))

ggplot(data = profit_data, aes(x = error_degree, y = as.numeric(profit_loss_clean), fill = as.factor(max_dev))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(rate_types ~ ys_type)
# ggsave()

ggplot(data = subset(final_results, rate_types == "rates_center" & error_degree == 0.75 & ys_type == "high_response"), aes(y = yield, x = rate)) +
  geom_count() +
  geom_function(fun = f_high) +
  geom_line(data = subset(det_est_data,  rate_types == "rates_center" & error_degree == 0.75 & ys_type == "high_response"), aes(y = yield, x = rate), color = "Blue") +
  facet_grid(cols = vars(max_dev))

ggplot(data = subset(final_results, rate_types == "rates_center" & error_degree == 1.25 & ys_type == "high_response"), aes(y = yield, x = rate)) +
  geom_count() +
  geom_function(fun = f_high) +
  geom_line(data = subset(det_est_data, rate_types == "rates_center" & error_degree == 1.25 & ys_type == "high_response"), aes(y = yield, x = rate), color = "Blue") +
  facet_grid(cols = vars(max_dev))

## graph for cleaning example ##
results_mismatch_mean_perc <- profit_data[ ,list(perc_data = mean(perc_clean)), by = list(error_degree, ys_type, alignment_case, max_dev, rate_types)]

ggplot(data = subset(final_results, rate_types == "rates_center" & ys_type == "high_response" & error_degree == 0.75), aes(y = yield, x = rate)) +
  geom_count() +
  geom_function(fun = f_high) +
  geom_line(data = subset(det_est_data, rate_types == "rates_center" & ys_type == "high_response" & error_degree == 0.75), aes(y = yield, x = rate), color = "Blue") +
  geom_label(y = 225, aes(x = 75, label = paste0("Perc Data = ", perc_clean)),
             data = subset(profit_data, rate_types == "rates_center" & ys_type == "high_response" & error_degree == 0.75)) +
  facet_grid( ~ max_dev)

final_results <- readRDS(here("Results", "deterministic_clean_data_mismatch_seed.rds"))
det_est_data <- readRDS(here("Results", "deterministic_clean_est_mismatch_seed.rds"))
profit_data <- readRDS(here("Results", "deterministic_clean_profit_mismatch_seed.rds"))

ggplot(data = profit_data, aes(x = error_degree, y = as.numeric(profit_loss_clean), fill = as.factor(max_dev))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(rate_types ~ ys_type)
ggsave()

ggplot(data = subset(final_results, error_degree == 0.75), aes(y = yield, x = rate)) +
  geom_count() +
  geom_function(fun = f_seed) +
  geom_line(data = subset(det_est_data, error_degree == 0.75), aes(y = yield, x = rate)) +
  facet_grid(rate_types ~ max_dev)

ggplot(data = subset(final_results, error_degree == 1.25), aes(y = yield, x = rate)) +
  geom_count() +
  geom_function(fun = f_seed) +
  geom_line(data = subset(det_est_data, error_degree == 1.25), aes(y = yield, x = rate)) +
  facet_grid(rate_types ~ max_dev)

