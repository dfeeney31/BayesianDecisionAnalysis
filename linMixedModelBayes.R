### linear mixed models with a Bayesian interpretation ## 
# from tutorial https://www.r-bloggers.com/bayesian-linear-mixed-models-random-intercepts-slopes-and-missing-data/ #
library(brms)
library(tidyverse)
library(haven) #Data lib. 405 children elementary school measured over 4 periods on different reading/math scores
setwd('C:/Users/Daniel.Feeney/Documents/BayesianDecisionAnalysis')
curran_dat <- read_sav("CurranLong.sav") %>%
  select(id, occasion, read, homecog) %>%
  filter(complete.cases(.))
curran_dat

#First model, population level intercept and random intercept for each participant.
## using brms, family is where we select prior for our outcome variable (reading score) & we use Gaussian in this case. Next line we define model
## formula. Just like lmer, 1|id allows for a random intercept for each participant. Specify priors. This first is pp-level intercept, 
## we keep it wide (0.5 sigma). next prior is for the standard dev of random effects. We use Cauchy since SD can only be positive and same for sigma
## which is overall variability.
## The rest of the code runs MCMC w/ 2k chained simulations 

read1 <- brm(data = curran_dat,
             family = gaussian,
             formula = read ~ 1 + (1 | id),
             prior = c(prior(normal(0, 10), class = Intercept),
                       prior(cauchy(0, 1), class = sd),
                       prior(cauchy(0, 1), class = sigma)),
             iter = 2000, warmup = 1000, chains = 4, cores = 4,
             control = list(adapt_delta = .975, max_treedepth = 20),
             seed = 190831)
print(read1)
# population level intervcept is 4.11 with a Bayesian credible interval of 4.01-4.21. 
# Estimate of SD(intercept) is 0.54, meaning there is high interpersonal variability
plot(read1)
#look at model against random sample
# each estimate is drawn towards population level mean. With no random intercept for id, they would all be on the same line

# now let's add a random effect for assessment. Adding a second random effect (1|occasion)
read2 <- brm(data = curran_dat,
             family = gaussian,
             read ~ 1 + (1 | id) + (1 | occasion),
             prior = c(prior(normal(0, 10), class = Intercept),
                       prior(cauchy(0, 1), class = sd),
                       prior(cauchy(0, 1), class = sigma)),
             iter = 2000, warmup = 1000, chains = 4, cores = 4,
             control = list(adapt_delta = .975, max_treedepth = 20),
             seed = 190831)
print(read2) #we have significantly reduced the overall sigma, which is the goal of these models by resolving overall sigma into cluster-level variance
plot(read2)
#we see a general trend for students to improve over time
curran_dat %>%
  ggplot(aes(x = occasion, y = read, group = id)) +
  geom_line(size = .75, alpha = .20) +
  labs(x = "Assessment Period",
       y = "Reading Ability") +
  theme_minimal(base_size = 16)

# fixed effects model for occasion and random for id. beta needs a new prior so we choose normal (0,1) (this isbeta effect for occasion on reading)
read3 <- brm(data = curran_dat,
             family = gaussian,
             read ~ 1 + occasion + (1 | id),
             prior = c(prior(normal(0, 10), class = Intercept),
                       prior(normal(0, 1), class = b),
                       prior(cauchy(0, 1), class = sd),
                       prior(cauchy(0, 1), class = sigma)),
             iter = 2000, warmup = 1000, chains = 4, cores = 4,
             control = list(adapt_delta = .975, max_treedepth = 20),
             seed = 190831)
print(read3)
plot(read3)

# lastly, model a random slope for each subject and random intercept. A random slope model also has a random intercept, but now, 
# the slope for time on reading ability will be different for each participant: '(1 + occasion | id)'.
# add a prior for correlation btwn intercept and beta for this occasion. We use Cholesky Distribution. More above 1, the more
# speculative the model is of a strong correlation

read4 <- brm(data = curran_dat,
             family = gaussian,
             read ~ 1 + occasion + (1 + occasion | id), #fixed effect of occasion, random interecept and random slope for each subject
             prior = c(prior(normal(0, 10), class = Intercept),
                       prior(normal(0, 1), class = b),
                       prior(cauchy(0, 1), class = sd),
                       prior(cauchy(0, 1), class = sigma),
                       prior(lkj_corr_cholesky(1.5), class = cor)),
             iter = 2000, warmup = 1000, chains = 4, cores = 4,
             control = list(adapt_delta = .975, max_treedepth = 20),
             seed = 190831)
print(read4)
plot(read4)

library(lme4)
lmer(curran_dat$read ~ curran_dat$occasion + (1 + curran_dat$occasion | curran_dat$id))

# Try on some pilot data
dat <- read.csv('C:/Users/Daniel.Feeney/Dropbox (Boa)/Endurance Protocol Trail Run/Outdoor_Protocol_March2020/Master.csv')
dat$RateNorm <- (dat$RateTotal / 9.81) / 70
dat <- subset(dat, dat$Subject != 'SH') #removing the incomplete case
dat <- as_tibble(dat)
dat$PrePost <- factor(dat$PrePost, c('pre','post')) #reorder factors to plot pre before post

lmer(dat$RateNorm ~ dat$Config + (1+ dat$Config|dat$Subject))

runmod <- brm(data = dat,
             family = gaussian,
             RateNorm ~ Config + (1 + Config | Subject), #fixed effect of occasion, random interecept and random slope for each subject
             prior = c(prior(normal(0, 10), class = Intercept),
                       prior(normal(0, 1), class = b),
                       prior(cauchy(0, 1), class = sd),
                       prior(cauchy(0, 1), class = sigma),
                       prior(lkj_corr_cholesky(1.5), class = cor)),
             iter = 2000, warmup = 1000, chains = 4, cores = 4,
             control = list(adapt_delta = .975, max_treedepth = 20),
             seed = 190831)
print(runmod)
plot(runmod)
