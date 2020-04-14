# Chapter 2 in BDA course exercises #
rm(list=ls())
#' ## Probability of a girl birth given placenta previa (BDA3 p. 37).
#' 
#' 437 girls and 543 boys have been observed. Calculate and plot the
#' posterior distribution of the proportion of girls $\theta$, using
#' uniform prior on $\theta$.

library(ggplot2)
library(tidyverse)
#posterior is beta(438,544)
df1 <- data.frame(theta = seq(0.375, 0.525, 0.001))  #set up even spacing 
a <- 438
b <- 544
# dbeta comutes the posterior density
df1$p <- dbeta(df1$theta,a,b)

#' compute also 95% central interval
# seq creates evenly spaced values from 2.5% quantile
# to 97.5% quantile (i.e., 95% central interval)
# qbeta computes the value for a given quantile given parameters a and b
df2 <- data.frame(theta = seq(qbeta(0.025, a, b), qbeta(0.975, a, b), length.out = 100))
# compute the posterior density
df2$p <- dbeta(df2$theta, a, b)

#' Plot posterior (Beta(438,544))
#' and 48.8% line for population average
ggplot(mapping = aes(theta, p)) +
  geom_line(data = df1) +
  # Add a layer of colorized 95% posterior interval
  geom_area(data = df2, aes(fill='1')) +
  # Add the proportion of girl babies in general population
  geom_vline(xintercept = 0.488, linetype='dotted') +
  # Decorate the plot a little
  labs(title='Uniform prior -> Posterior is Beta(438,544)', y = '') +
  scale_y_continuous(expand = c(0, 0.1), breaks = NULL) +
  scale_fill_manual(values = 'lightblue', labels = '95% posterior interval') +
  theme(legend.position = 'bottom', legend.title = element_blank())

#' Posterior with Beta(1,1), ie. uniform prior
df1$pu <- dbeta(df1$theta, a+1, b+1)
#' 3 different choices for priors
#'
#' - Beta(0.488\*2,(1-0.488)\*2)
#' - Beta(0.488\*20,(1-0.488)\*20)
#' - Beta(0.488\*200,(1-0.488)\*200)
n <- c(2, 20, 200) # prior counts
apr <- 0.488 # prior ratio of success


# helperf returns the prior ratio of successes for given number of prior observations,
# number of observed successes and failures and a data
# frame with values of theta, a new data frame with prior and posterior
# values evaluated at points theta.
helperf <- function(n, apr, a, b, df)
  cbind(df, pr = dbeta(df$theta, n*apr, n*(1-apr)), po = dbeta(df$theta, n*apr + a, n*(1-apr) + b), n = n)
# lapply function over prior counts n and gather results into key-value pairs.
df2 <- lapply(n, helperf, apr, a, b, df1) %>% do.call(rbind, args = .) %>%
  gather(grp, p, -c(theta, n), factor_key = T)
# add correct labels for plotting
df2$title <- factor(paste0('alpha/(alpha+beta)=0.488, alpha+beta=',df2$n))
levels(df2$grp) <- c('Posterior with unif prior', 'Prior', 'Posterior')

#' Plot distributions
ggplot(data = df2) +
  geom_line(aes(theta, p, color = grp)) +
  geom_vline(xintercept = 0.488, linetype = 'dotted') +
  facet_wrap(~title, ncol = 1) +
  labs(x = '', y = '') +
  scale_y_continuous(breaks = NULL) +
  theme(legend.position = 'bottom', legend.title = element_blank())
