# /*=================================================*/
#' # Preparation
# /*=================================================*/

library(here)
library(tmap)
library(sp)
library(sf)
library(agricolae)
library(lwgeom)
library(measurements)
library(raster)
library(data.table)
library(tidyverse)
library(gdata)
library(mgcv)
library(MonteCarlo)

#--- source functions ---#
source(here("Codes", "functions.R"))
source(here("Codes", "simulation_functions.R"))

#--- github ---#
source("https://raw.githubusercontent.com/brittanikedge/DIFM/main/Functions.R")


results <- readRDS(here("Codes", "sim_results.rds")) %>% 
  unnest(sim_estimates) %>%
  as.data.frame()

merge(results, function_data, by = y_function)
results
p <- ggplot(subset(results, case == "angle"), aes(opt_n_est)) +
  geom_vline(xintercept = opt_n) +
  geom_histogram() + 
  xlab("Estimated Optimal Nitrogen") +
  ylab("Frequency") +
  facet_grid(vars(error), vars(y_function))

# Use vars() to supply variables from the dataset:
p + facet_grid(rows = vars(drv))plot_width <- 60
harvester_width <- 30
rates_ls <- seq(80, 240, by = 40)
n_price <- .5
c_price <- 4.00
subplot_length <- 10

##### Functions #####
y_function_low <- function(x){
  y = 200*(1 - exp(-.006*(140 + x)))
  return(y)}
# graph yield response function
ggplot() +
  xlim(0, 300) +
  geom_function(fun = y_function_low)

p_function_low <- function(x){
  y = (200*(1 - exp(-.006*(140 + x)))*c_price) - x*n_price
  return(y)}
ggplot() +
  xlim(0, 300) +
  geom_function(fun = p_function_low)
optimize(p_function_low, interval=c(50, 300), maximum=TRUE)$maximum

y_function_mid <- function(x){
  y = 280*(1 - exp(-.012*(70 + x)))
  return(y)}
# graph yield response function
ggplot() +
  xlim(0, 300) +
  geom_function(fun = y_function_mid)

p_function_mid <- function(x){
  y = (280*(1 - exp(-.012*(70 + x)))*c_price) - x*n_price
  return(y)}
ggplot() +
  xlim(0, 300) +
  geom_function(fun = p_function_mid)
optimize(p_function_mid, interval=c(50, 300), maximum=TRUE)$maximum

y_function_high <- function(x){
  y = 300*(1 - exp(-.02*(100 + x)))
  return(y)}
# graph yield response function
ggplot() +
  xlim(0, 300) +
  geom_function(fun = y_function_high)

p_function_high <- function(x){
  y = (300*(1 - exp(-.02*(100 + x)))*c_price) - x*n_price
  return(y)}
ggplot() +
  xlim(0, 300) +
  geom_function(fun = p_function_high)
optimize(p_function_high, interval=c(50, 300), maximum=TRUE)$maximum

results$hist_n_est[[1]] # angle 5
results$hist_n_est[[2]] # misalingment 5
results$hist_n_est[[3]] # angle 10
results$hist_n_est[[4]] # misalingment 10
results$hist_n_est[[5]] # angle 15
results$hist_n_est[[6]] # misalignment 15
results$hist_n_est[[7]] # angle 20
results$hist_n_est[[8]] # misalingment 20

results$hist_n_est[[9]] # angle 5
results$hist_n_est[[10]] # misalignment 5
results$hist_n_est[[11]] # angle 10
results$hist_n_est[[12]] # misalingment 10
results$hist_n_est[[13]] # angle 15
results$hist_n_est[[14]] # misalingment 15
results$hist_n_est[[15]] # angle 20
results$hist_n_est[[16]] # misalingment 20

results$hist_n_est[[17]] # angle 5
results$hist_n_est[[18]] # misalignment 5
results$hist_n_est[[19]] # angle 10
results$hist_n_est[[20]] # misalignment 10
results$hist_n_est[[21]] # angle 15
results$hist_n_est[[22]] # misalignment 15
results$hist_n_est[[23]] # angle 20
results$hist_n_est[[24]] # misalignment 20
