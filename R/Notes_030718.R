library(tidyverse)
library(modelr)
library(ggplot2)


options(na.action = na.warn)

ggplot(sim1, aes(x,y)) +
  geom_point()

ggplot(sim1, aes(x,y)) +
  geom_point()

models <- tibble(
  a1 = runif(250, -20, 40)
  ,a2 = runif(250, -5, 5)
)

ggplot(sim1, aes(x,y)) + geom_abline(aes(intercept = a1, slope = a2)
                                     , data = models, alpha = .25) +
  geom_point() +
  theme_bw()

model1 <- function(a, data){
  a[1] + data$x * a[2]
}

model1(c(7, 1.5), sim1)

measure_distance <- function(mod, data){
  diff <- data$y - model1(mod, data)
  sqrt(mean(diff^2))
}

measure_distance(c(7, 1.5), sim1)

sim1_dist <- function(intercept, slope){
  measure_distance(c(intercept, slope), sim1)
}

models <- models %>%
  mutate(
    dist = purrr::map2_dbl(a1, a2, sim1_dist)
  )

ggplot(sim1, aes(x,y)) +
  geom_point() +
  geom_abline(
    aes(intercept = a1, slope = a2, color = -dist),
    data = filter(models, rank(dist) <= 10)
  )

ggplot(models, aes(a1, a2)) +
  geom_point(data = filter(models, rank(dist) <= 10)
             ,size = 4, color = 'red') +
  # line above circles top ten models in red
  geom_point(aes(color = -dist) +
               theme_bw()
 #aes changes aesthetics

 grid <- expand.grid(
   a1 = seq(-5, 20, length = 25)
   , a2 = seq(1, 3, length = 25)
 ) %>%
   mutate(
     dist = purrr::map2_dbl(a1, a2, sim1_dist)
   )

 ggplot(grid, aes(a1, a2)) +
   geom_point(data = filter(grid, rank(dist) <= 10)
              , size = 4, color = 'red') +
   geom_point(aes(color = -dist))

 ggplot(sim1, aes(x,y)) +
   geom_point() +
   geom_abline(intercept = a1, slope = a2, color = -dist
               , data = filter(grid, rank(dist) <= 10))

 best <- optim(c(0,0), measure_distance(data = sim1))

 ggplot(sim1, aes(x,y)) +
   geom_point() +
   abline(intercept = best$par[1], slope = best$par[2])

 sim1_mod <- lm(y~x, data = sim1)
 # line above is the regression

 sim1_mod$coefficients

 ggplot(sim1, aes(x,y)) +
   geom_point() +
   geom_abline(intercept = best$par[1], slope = best$par[2], color = "blue") +
   geom_abline(intercept = best$par[1], slope = coeffs[2], color = "green")

 grid <- sim1 %>% data_grid(x) %>%
   add_predictions(sim1_mod)

 ggplot(sim1, aes(x)) +
   geom_point(aes(y = y)) +
   geom_line(aes(y = pred), data = grid,
             color = 'red', size = 1)

 sim1 <- sim1 %>%
   add_residuals(sim1_mod) %>%
   add_predictions(sim1_mod)

 ggplot(sim1, aes(resid)) +
   geom_freqpoly(binwidth = 0.5)


 ggplot(sim1, aes(x, resid)) +
   geom_ref_line(h = 0) +
   geom_point()

 df <- tribble(
   ~y, ~x1, ~x2,
   4, 2, 5,
   5, 1, 6
 )

 modle_matrix(df, y ~ x1 - 1)

 lm(y ~ x-1, data = sim1)

 model_matrix(df, y ~ x1 + x2)

 df <- tribble(
   ~sex, ~response
   , "male", 1,
   "female", 2,
   "male", 1
 )

 model_matrix(df, response ~ sex)

 ggplot(sim1) +
   geom_point(aes(x, y))

 mod2 <- lm(y ~ x, data = sim2)


 grid <- sim2 %>%
   data_grid(x) %>%
   add_predictions(mod2)

 ggplot(sim2, aes(x)) +
   geom_point(aes(y = y)) +
   geom_point(data = grid, aes(y = pred), color = 'red')


 mod1 <- lm(y ~ x1 + x2, data = sim3)
 mod2 <- lm(y ~ x1 * x2, data = sim3)

 grid <- sim3 %>%
   data_grid(x1, x2) %>%
   gather_predictions(mod1, mod2)

 ggplot(sim3, aes(x1, y, color = x2)) +
   geom_point() +
   geom_line(data = grid, aes(y = pred)) +
   facet_wrap(~model)

 sim3 <- sim3 %>%
   gather_residuals(mod1, mod2)

 ggplot(sim3, aes(x1, resid, color = x2)) +
   geom_point() +
   facet_grid(model ~ x2)


 mod1 <- lm(y ~ x1 + x2, data = sim4)
 mod2 <- lm(y ~x1 * x2, data = sim4)

 grid <- sim4 %>%
   data_grid(
     x1 = seq_range(x1, 5),
     x2 = seq_range(x2, 5)
   ) %>%
   gather_predictions(mod1, mod2)

 seq_range(c(.0123, .924343), n = 5, pretty = TRUE)

 x1 <- rcauchy(100)

 seq_range(x1, n = 5, trim = .25)

 ggplot(grid, aes(x1, x2)) +
   geom_tile(aes(fill = pred)) +
   facet_wrap(~model)

 ggplot(grid, aes(x1, pred, color = x2, group = x2)) +
   geom_line() +
   facet_wrap(~model)

 df <- tribble(
   ~y, ~x,
   1, 1,
   2, 2,
   3, 3
 )

 model_matrix(df, y ~ x^2 + x)

 model_matrix(df, y ~ I(x^2) + x)

 sim5 <- tibble(
   x = seq(0, 3.5*pi, length = 50),
   y = 5*sin(x) + rnorm(length(x))
 )

 ggplot(sim5, aes(x, y)) +
   geom_point()

library(splines)

 mod1 <- lm(y ~ ns(x, 1), data = sim5)
 mod2 <- lm(y ~ ns(x, 2), data = sim5)
 mod3 <- lm(y ~ ns(x, 3), data = sim5)
 mod4 <- lm(y ~ ns(x, 4), data = sim5)
 mod5 <- lm(y ~ ns(x, 5), data = sim5)
# shift + option + up (down) is useful here
# if you don't change the blue numbers then you dont
# fit a spline to the  data

 grid <- sim5 %>%
   data_grid(x = seq_range(x, n = 50, expand = .1)) %>%
   gather_predictions(mod1, mod2, mod3, mod4, mod5, .pred = "y")
 #by default it wants to call the column pred, change it to y

 ggplot(sim5, aes(x,y)) +
   geom_point() +
   geom_line(data = grid, color = "red") +
   facet_wrap(~model)









































