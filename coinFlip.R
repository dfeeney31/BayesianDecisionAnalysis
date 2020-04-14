rm(list=ls())
set.seed(5280)
par(mfrow=c(1,1)) 
#Approximate Bayesian Computation, simple but very slow!
###  Bayesian A testing example ###
observed_data <- 6 #6 of 16 people responded yes
observed_data2 <- 10 #10 of 16 said yes in condition B

# Number of random draws from the prior
n_draws <- 10000
x <- seq(0,1,length = n_draws)

prior <- runif(n_draws,0,1) # Here you sample n_draws draws from the prior  
prior2 <- dbeta(x, 3,5)

hist(prior) # It's always good to eyeball the prior to make sure it looks ok
plot(x, dbeta(x,3,5), ylab = "density", type='l', col=4)

# Here you define the generative model
# Defining the generative model
gen_model <- function(rate) {
  subscribers <- rbinom(1, size = 16, prob = rate)
  subscribers
}

# Here you simulate data using the parameters from the prior and the 
# generative model
sim_data <- rep(NA, n_draws)
sim_data2 <- rep(NA, n_draws)
for(i in 1:n_draws) {
  sim_data[i] <- gen_model(prior[i])
  sim_data2[i] <- gen_model(prior2[i])
}
hist(sim_data)
hist(sim_data2)

#filter data that matches observed data
posterior <- prior[sim_data == observed_data] 
hist(posterior)
# Now you can summarize the posterior, where a common summary is to take the mean
# or the median posterior, and perhaps a 95% quantile interval.
median(posterior)
quantile(posterior, c(0.025, 0.975))

#A/B
library("rstan")
setwd('C:/Users/Daniel.Feeney/Documents/BayesianDecisionAnalysis')
schools_dat <- list(J = 8, 
                    y = c(28,  8, -3,  7, -1,  1, 18, 12),
                    sigma = c(15, 10, 16, 11,  9, 11, 10, 18))
fit <- stan(file = '8schools.stan', data = schools_dat)

################################
library("rstan")
data_list <- list(nA = 16, nB = 16, sA = 6, sB = 10)

# Compiling and producing posterior samples from the model.
stan_samples <- stan(file = 'swedishfish.stan', data = data_list)
# Plotting and summarizing the posterior distribution
plot(stan_samples) #with beta distribution (3,25), CI_level is 0.8

# Export the samples to a data.frame for easier handling.
posterior <- as.data.frame(stan_samples)
sum(posterior$rate_diff > 0) / length(posterior$rate_diff) #Rate B is higher than A with an 84.3% probabiliY
# calculating the estimated posterior profit using method A (or B)
# a cost of 30 kr + the average profit per sent out add
profitA <- -30 + posterior$rateA * 1000 
profitB <- -300 + posterior$rateB * 1000 
hist(profitA)
hist(profitB)
hist(profitA - profitB)
expected_profit_diff <- mean(profitA - profitB)
abline(v = expected_profit_diff, col = "red", lwd =2)


