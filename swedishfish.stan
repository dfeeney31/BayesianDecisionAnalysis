//
// Stan model for Swedish Fish example

// The input data is a vector 'y' of length 'N'.
data {
  //  Number of trials
  int nA;
  int nB;
  // Number of successes These parameters are defined in the R data sent to Stan.
  int sA;
  int sB;
}

parameters {
  real<lower=0, upper=1> rateA;
  real<lower=0, upper=1> rateB;
}

model {
  rateA ~ beta(3,25);
  rateB ~ beta(3,25);
  sA ~ binomial(nA, rateA);
  sB ~ binomial(nB, rateB); 
}

generated quantities {
  real rate_diff;
  rate_diff = rateB - rateA;
}


