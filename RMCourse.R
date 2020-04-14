### Richard Mcelreath lecture examples ###
#lecture 2: simualting proportion of earth covered in water vs land. 
library("rethinking")
p_grid <- seq(0, 1, length.out = 1000)
prob_p <- rep(1,1000)
prob_data <- dbinom(6, size = 9, prob = p_grid)
posterior <- prob_data * p_grid
posterior <- posterior / sum(posterior)
plot(p_grid)
plot(prob_p)
plot(posterior)

samples <- sample(p_grid, prob=posterior, size = 1e4, replace=TRUE)
plot(samples)
hist(samples)
