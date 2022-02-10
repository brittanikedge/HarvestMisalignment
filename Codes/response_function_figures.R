library(ggplot2)

f_high <- function(x){
  y = 250 * (1 - exp(-.035 * (20 + x)))
  return(y)
}

response_high <- ggplot() +
  xlim(0, 200) +
  ylim(100, 255) +
  geom_function(fun = f_high) +
  xlab("Input Rate") +
  ylab("Yield") 
response_high
# saveRDS(response_high, "~/Box/Machine_Misalignment/Results/response_high.rds")


f_mid <- function(x){
  y = 250 * (1 - exp(-.02 * (45 + x)))
  return(y)
}

response_mid <- ggplot() +
  xlim(0, 200) +
  ylim(100, 255) +
  geom_function(fun = f_mid) +
  xlab("Input Rate") +
  ylab("Yield") 
response_mid
# saveRDS(response_mid, "~/Box/Machine_Misalignment/Results/response_mid.rds")

f_low <- function(x){
  y = 250 * (1 - exp(-.009 * (160 + x)))
  return(y)
}

response_low <- ggplot() +
  xlim(0, 200) +
  ylim(100, 255) +
  geom_function(fun = f_low) +
  xlab("Input Rate") +
  ylab("Yield") 
response_low
# saveRDS(response_low, "~/Box/Machine_Misalignment/Results/response_low.rds")

f_seed <- function(x){
  y = 150 + 5.2*x - 0.068*(x^2)
  return(y)
}

response_seed <- ggplot() +
  xlim(0, 60) +
  ylim(100, 255) +
  geom_function(fun = f_seed) +
  xlab("Input Rate") +
  ylab("Yield") 
response_seed
# saveRDS(response_seed, "~/Box/Machine_Misalignment/Results/response_seed.rds")

library(patchwork)
(response_low + response_mid + response_high) 
