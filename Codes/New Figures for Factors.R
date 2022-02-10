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
library(patchwork)

#--- source functions ---#
source(here("Codes", "functions new.R"))

#--- github ---#
source("https://raw.githubusercontent.com/brittanikedge/DIFM/main/Functions.R")

f_high <- function(x) {
  y = 250 * (1 - exp(-.035 * (20 + x)))
  return(y)
}

f_low <- function(x){
  y = 250 * (1 - exp(-.009 * (160 + x)))
  return(y)
}

f_mid <- function(x){
  y = 250 * (1 - exp(-.02 * (45 + x)))
  return(y)
}

final_results_mismatch_nitrogen <- readRDS(here("Results", "deterministic_clean_data_mismatch_nitrogen.rds"))
det_est_data_mismatch_nitrogen <- readRDS(here("Results", "deterministic_clean_est_mismatch_nitrogen.rds"))
final_results_mismatch_nitrogen <- subset(final_results_mismatch_nitrogen, max_dev == 100 & rate_types == "rates_center" & ys_type == "high_response" & c_price == 5.62 & error_degree == 0.75)
det_est_data_mismatch_nitrogen <- subset(det_est_data_mismatch_nitrogen, max_dev == 100 & rate_types == "rates_center" & ys_type == "high_response" & c_price == 5.62 & error_degree == 0.75)
final_results_mismatch_nitrogen <- final_results_mismatch_nitrogen[,1:11]
det_est_data_mismatch_nitrogen <- det_est_data_mismatch_nitrogen[,1:11]

final_results_others_nitrogen <- readRDS(here("Results", "deterministic_clean_data_others_nitrogen.rds"))
det_est_data_others_nitrogen <- readRDS(here("Results", "deterministic_clean_est_others_nitrogen.rds"))

# changing yield response types
high_data <- ggplot(data = subset(final_results_others_nitrogen, max_dev == 100 & ys_type == "high_response" & alignment_case == "mis-alignment" & error_degree == 30 & rate_types == "rates_center"), aes(y = yield, x = rate)) +
  geom_count() +
  geom_function(fun = f_high) +
  geom_line(data = subset(det_est_data_others_nitrogen, max_dev == 100 & ys_type == "high_response" & alignment_case == "mis-alignment" & error_degree == 30 & rate_types == "rates_center"), aes(y = yield, x = rate), color = "Red") +
  xlab("Input Rate") +
  ylab("Yield") +
  xlim(60, 260) +
  theme(legend.position="none") +
  ylim(230, 252)

mid_data <- ggplot(data = subset(final_results_others_nitrogen, max_dev == 100 & ys_type == "middle_response" & alignment_case == "mis-alignment" & error_degree == 30 & rate_types == "rates_center"), aes(y = yield, x = rate)) +
  geom_count() +
  geom_function(fun = f_mid) +
  geom_line(data = subset(det_est_data_others_nitrogen, max_dev == 100 & ys_type == "middle_response" & alignment_case == "mis-alignment" & error_degree == 30 & rate_types == "rates_center"), aes(y = yield, x = rate), color = "Red") +
  xlab("Input Rate") +
  ylab("Yield") +
  xlim(60, 260) +
  theme(legend.position="none") +
  ylim(230, 252)

low_data <- ggplot(data = subset(final_results_others_nitrogen, max_dev == 100 & ys_type == "low_response" & alignment_case == "mis-alignment" & error_degree == 30 & rate_types == "rates_center"), aes(y = yield, x = rate)) +
  geom_count() +
  geom_function(fun = f_low) +
  geom_line(data = subset(det_est_data_others_nitrogen, max_dev == 100 & ys_type == "low_response" & alignment_case == "mis-alignment" & error_degree == 30 & rate_types == "rates_center"), aes(y = yield, x = rate), color = "Red") +
  xlab("Input Rate") +
  ylab("Yield") +
  xlim(60, 260) +
  ylim(230, 252) +
  guides(size = guide_legend(title = "Count"))

(high_data | mid_data | low_data)

# change misalignment type
# need to add misalignment types together 
results_others <- subset(final_results_others_nitrogen, max_dev == 100 & rate_types == "rates_center" & ys_type == "high_response" & c_price == 5.62 & error_degree == 30)
results_others <- results_others %>%
  dplyr::select(colnames(final_results_mismatch_nitrogen))
data_others <- subset(det_est_data_others_nitrogen, max_dev == 100 & rate_types == "rates_center" & ys_type == "high_response" & c_price == 5.62 & error_degree == 30)

data_all <- rbind(data_others, det_est_data_mismatch_nitrogen)
results_all <- rbind(results_others, final_results_mismatch_nitrogen)

data_all$alignment_case <- as.factor(data_all$alignment_case)
levels(data_all$alignment_case) <- c("Heading Difference", "Parallel Shift", "Incompatible Machinery")

results_all$alignment_case <- as.factor(results_all$alignment_case)
levels(results_all$alignment_case) <- c("Heading Difference", "Parallel Shift", "Incompatible Machinery")

ggplot(results_all, aes(rate, yield)) +
  geom_count() +
  geom_line(data = data_all, aes(rate, yield), color = "Red") +
  geom_function(fun = f_high) +
  facet_grid(cols = vars(alignment_case)) +
  xlab("Input Rate") +
  ylab("Yield") +
  guides(size = guide_legend(title = "Count"))

# change error levels

ggplot(subset(final_results_others_nitrogen, max_dev == 100 & rate_types == "rates_center" & ys_type == "high_response" & c_price == 5.62 & alignment_case == "mis-alignment"), aes(rate, yield)) +
  geom_count() +
  geom_line(data = subset(det_est_data_others_nitrogen, max_dev == 100 & rate_types == "rates_center" & ys_type == "high_response" & c_price == 5.62 & alignment_case == "mis-alignment"), aes(rate, yield), color = "Red") +
  geom_function(fun = f_high) +
  facet_grid(cols = vars(error_degree)) +
  xlab("Input Rate") +
  ylab("Yield") +
  guides(size = guide_legend(title = "Count"))

# just change rates
# use mis-alignment 30-foot high-response and change rates
# final_results_others_nitrogen$rate_types <- as.factor(final_results_others_nitrogen$alignment_case)
levels(final_results_others_nitrogen$rate_types) <- c("Low Rates", "Centered Rates", "High Rates")

# det_est_data_others_nitrogen$rate_types <- as.factor(det_est_data_others_nitrogen$alignment_case)
levels(det_est_data_others_nitrogen$rate_types) <- c("Low Rates", "Centered Rates", "High Rates")

ggplot(subset(final_results_others_nitrogen, max_dev == 100 & ys_type == "high_response" & c_price == 5.62 & alignment_case == "mis-alignment" & error_degree == 30), aes(rate, yield)) +
  geom_count() +
  geom_line(data = subset(det_est_data_others_nitrogen, max_dev == 100 & ys_type == "high_response" & c_price == 5.62 & alignment_case == "mis-alignment" & error_degree == 30), aes(rate, yield), color = "Red") +
  geom_function(fun = f_high) +
  facet_grid(cols = vars(rate_types)) +
  # ylim(230, 250) +
  xlab("Input Rate") +
  ylab("Yield") +
  guides(size = guide_legend(title = "Count"))

# change max_dev 

ggplot(subset(final_results_others_nitrogen, error_degree == 30 & rate_types == "Centered Rates" & ys_type == "high_response"  & alignment_case == "mis-alignment"), aes(rate, yield)) +
  geom_count() +
  geom_line(data = subset(det_est_data_others_nitrogen, error_degree == 30 & rate_types == "Centered Rates" & ys_type == "high_response" & alignment_case == "mis-alignment"), aes(rate, yield), color = "Red") +
  geom_function(fun = f_high) +
  facet_grid(cols = vars(max_dev)) +
  xlab("Input Rate") +
  ylab("Yield") +
  guides(size = guide_legend(title = "Count"))


