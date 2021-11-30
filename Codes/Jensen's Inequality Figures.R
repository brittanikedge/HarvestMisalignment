# Jensen's Inequality Figures #

soy_function <- function(x){
  yield <- 250 * (1 - exp(-.035 * (20 + x)))
  return(yield)
}

r1 <- 30
r2 <- 100

r3 <- 120
r4 <- 190

gap_data <- data.frame(X = c(.5*r1 + .5*r2, 
                             .5*r1 + .5*r2,
                             .3*r1 + .7*r2,
                             .3*r1 + .7*r2,
                             .5*r3 + .5*r4,
                             .5*r3 + .5*r4),
                       Y = c(.5*soy_function(r1) + .5*soy_function(r2),
                             soy_function(.5*r1 + .5*r2),
                             .3*soy_function(r1) + .7*soy_function(r2),
                             soy_function(.3*r1 + .7*r2),
                             .5*soy_function(r3) + .5*soy_function(r4),
                             soy_function(.5*r3 + .5*r4)),
                       pair = c(1, 1, 2, 2, 3, 3))

length_data <- data.frame(length = c(soy_function(.5*r1 + .5*r2) - (.5*soy_function(r1) + .5*soy_function(r2)),
                                     soy_function(.3*r1 + .7*r2) - (.3*soy_function(r1) + .7*soy_function(r2)),
                                     soy_function(.5*r3 + .5*r4) - (.5*soy_function(r3) + .5*soy_function(r4))),
                          X = c(.5*r1 + .5*r2,
                                .3*r1 + .7*r2,
                                .5*r3 + .5*r4),
                          Y = c(255,
                                255,
                                255),
                          pair = c(1, 2, 3))

label_data <- data.frame(label = c("A",
                                   "B",
                                   "C"),
                         X = c(.5*r1 + .5*r2,
                               .3*r1 + .7*r2,
                               .5*r3 + .5*r4),
                         Y = c(max(soy_function(r1), soy_function(r2)) + 5,
                               max(soy_function(r1), soy_function(r2)) + 5,
                               max(soy_function(r3), soy_function(r4)) + 5))

new_data <- data.frame(X = c(r1, r2, r1, r2),
                       Y = c(soy_function(r1), soy_function(r2),  soy_function(r1),  soy_function(r2)),
                       pair = c(1, 1, 2, 2))

base <- ggplot(new_data, aes(X, Y)) 

weighting1 <- base + geom_function(fun = soy_function) +
  geom_point(aes(x = r1, y = soy_function(r1))) +
  geom_point(aes(x = r2, y = soy_function(r2))) +
  geom_point(aes(x = .5*r1 + .5*r2, y = .5*soy_function(r1) + .5*soy_function(r2)), shape = 0) +
  geom_point(aes(x = .5*r1 + .5*r2, y = soy_function(.5*r1 + .5*r2)), shape = 0) +
  geom_line(linetype = "dashed", aes (group = pair)) +
  geom_line(data = subset(gap_data, pair == 1), linetype = "dotted", aes (group = pair)) +
  geom_label(data = subset(length_data, pair == 1),
             aes(label = round(length, 4)),
             size = 8) +
  xlim(0, 200) +
  ylim(175, 260) +
  labs( y="Yield", x = "Seeding Rate (thousands/acre)")

weighting2 <- base + geom_function(fun = soy_function) +
  geom_point(aes(x = r1, y = soy_function(r1))) +
  geom_point(aes(x = r2, y = soy_function(r2))) +
  geom_point(aes(x = .3*r1 + .7*r2, y = .3*soy_function(r1) + .7*soy_function(r2)), shape = 5) +
  geom_point(aes(x = .3*r1 + .7*r2, y = soy_function(.3*r1 + .7*r2)), shape = 5) +
  geom_line(linetype = "dashed", aes (group = pair)) +
  geom_line(data = subset(gap_data, pair == 2), linetype = "dotted", aes (group = pair), linetype = "dashed") +
  geom_label(data = subset(length_data, pair == 2),
             aes(label = round(length, 4)),
             size = 8) +
  xlim(0, 200) +
  ylim(175, 260) +
  labs( y="Yield", x = "Seeding Rate (thousands/acre)")

new_data <- data.frame(X = c(r3, r4),
                       Y = c(soy_function(r3), soy_function(r4)),
                       pair = c(3, 3))
base <- ggplot(new_data, aes(X, Y)) 

weighting3 <- base + geom_function(fun = soy_function) +
  geom_point(aes(x = r3, y = soy_function(r3))) +
  geom_point(aes(x = r4, y = soy_function(r4))) +
  geom_point(aes(x = .5*r3 + .5*r4, y = .5*soy_function(r3) + .5*soy_function(r4)), shape = 15) +
  geom_point(aes(x = .5*r3 + .5*r4, y = soy_function(.5*r3 + .5*r4)), shape = 15) +
  geom_line(linetype = "dashed", aes (group = pair)) +
  geom_line(data = subset(gap_data, pair == 3), linetype = "dotted", aes (group = pair)) +
  geom_label(data = subset(length_data, pair == 3),
             aes(label = round(length, 4)),
             size = 8) +
  xlim(0, 200) +
  ylim(175, 260) +
  labs( y="Yield", x = "Seeding Rate (thousands/acre)")

(weighting1 | weighting2 | weighting3)

