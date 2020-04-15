### Richard Mcelreath lecture examples ###
#lecture 2: simualting proportion of earth covered in water vs land. 
library("rethinking")
#chapter 2. d in dbinom is density, rbinom is random, and pbindom is probabilities
# W ~ Binom(N,p) where N tosses and p is prob of a success. and W is number of 'successes' or water
# p ~ Uniform(0,1) where p may be drawn from the prior which is uniform over [0,1]
# next step is calculating the posterior (what is value of p given what we have observed and our prior)
# posterior is proporational to product of prior and probability of the data
# Grid approximation: most params are continuous, at a particular param value p', multiply
# the prior probability of p' with the liklihood at p'. Repeat for each value in grid to get approx posterior
# 
rm(list=ls())
dbinom(6, size = 9, prob = 0.5) #Relative number of ways to get 6 'successes' in 9 trials with a probability of 0.5
dbinom(6, size = 9, prob = 0.2) #much lower with p of 0.2
dbinom(6, size = 9, prob = 0.8) #greater with 0.8
dbinom(6, size = 9, prob = 0.9) #and low again with 0.9 since most trials would give success

#
p_grid <- seq(0,1, length.out = 100)
prior <- rep(1,100)
liklihood <-dbinom(6, size = 9, prob = p_grid)
unstd.posterior <- prior * liklihood
posterior <- unstd.posterior / sum(unstd.posterior)
plot(unstd.posterior)
plot(posterior)

#different prior
prior <- ifelse( p_grid < 0.5 , 0 , 1 )
posterior2 <- prior * liklihood
plot(prior)
plot(posterior2)
#or
prior <- exp( -5*abs( p_grid - 0.5 ) )
posterior3 <- prior * liklihood
plot(posterior3)
#Quadratic approximation: the region near the peak of a posterior is well defined as a Gaussian function
# with mean and std. Log(Gaussian) is a parabola (and therefore the name, Quadratic). This is done in 2
# steps in R: 1 find the peak of the posterior and 2 etimate the curvature around the peak. quap does this
library(rethinking)
globe.qa <- quap(
  alist(
    W ~ dbinom(W+L, p), #Binomial liklihood
    p ~ dunif(0,1) #uniform prior
  ),
  data = list(W=6,L=3)
)
precis(globe.qa)
# provide a formula and list of data to quap. Provides mean of posterior and 89% interval
# quadratic approx is similar to maximum liklihood estimate and SE with a lot of data and a uniform prior
# errors with Hessians (square matrix w/ 2nd derivatives). In this case, it is 2nd derivative of log of posterior
# probability w.r.t parameters. A parabola does not have derivates beyond the 2nd, so this is sufficient to 
# characterize the posterior. 
## MCMC algorithm Markov Chain Monte Carlo
n_samples <- 1000 
p <- rep( NA , n_samples )
p[1] <- 0.5
W <- 6
L <- 3
for ( i in 2:n_samples ) {
  p_new <- rnorm( 1 , p[i-1] , 0.1 )
  if ( p_new < 0 ) p_new <- abs( p_new )
  if ( p_new > 1 ) p_new <- 2 - p_new
  q0 <- dbinom( W , W+L , p[i-1] )
  q1 <- dbinom( W , W+L , p_new )
}
dens( p , xlim=c(0,1) )
curve( dbeta( x , W+1 , L+1 ) , lty=2 , add=TRUE )
# HW 1
rm(list=ls())
set.seed(5280)
p_grid <- seq(0, 1, length.out = 1000)
prob_p <- rep(1,1000)
prob_data <- dbinom(8, size = 15, prob = p_grid)
posterior <- prob_data * p_grid
posterior <- posterior / sum(posterior)
plot(p_grid)
plot(prob_p)
plot(posterior)
samples <- sample(p_grid, prob=posterior, size = 1e4, replace=TRUE) #generates the samples based on the observed data 
#posterior and the prior (which was unifirm in this case)
mean(samples) #Mean of the posterior distribution

#2 Use a prior that is 0 below 0.5
p_grid<- seq(0,1,length.out = 1000) #same as before
prior <-c(rep(0,500),rep(1,500))
prior_data <- dbinom(8, size = 15, prob=p_grid)

#verify the prior
plot(prior_prob)
plot(prior)

posterior <- prob_data * prior
plot(posterior)
posterior <- posterior / sum(posterior)
samples2 <- sample(p_grid, prob = posterior, size = 1e4, replace = TRUE)
plot(samples2)

dens(samples, xlab='p', xlim=c(0,1), ylim=c(0,6))
dens(samples2, add = TRUE, lty=2)
abline(v=0.7, col='red')
PI(samples, prob = 0.98)
PI(samples2, prob = 0.98)

### how can we get a 99% CI of p to only be 0.05 wide? ##

