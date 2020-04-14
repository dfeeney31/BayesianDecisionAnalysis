rm(list=ls())

library('tidyverse')
library('rstan')
library('recipes')

data("Duncan", package = "carData")

duncan_lm <- lm(prestige ~ type + income + education, data = Duncan)
plot(Duncan$prestige, duncan_lm$fitted.values)
