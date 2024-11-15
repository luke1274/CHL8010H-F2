m <- 1000 # number of simulations
alpha1 <- 0:2
results_unadj = numeric(3)
results_adj = numeric(3)
for (j in 1:3){
  type1_err_unadj = numeric(m)
  type1_err_adj = numeric(m)
  for (i in 1:m) {
    set.seed(2024+i)
    alpha <- 0.05
    n <- 500 # sample size
    pz <- 0.2 # probability of Z = 1
    alpha0 <- 0 # logit probability of x = 1 in non-smokers (z = 0)
    alpha2 <- 2
    beta0 <- -3 # logit prob of y = 1 in non-coffee drinkers (x = 0) and non-smokers (z = 0)
    beta1 <- 0
    beta2 <- 2
    ## generate confounder Z from a binomial distribution
    z <- rbinom(n, size = 1, prob = pz)
    ## compute probability of observing X = 1 from the inverse logit function
    px <- exp(alpha0 + alpha1[j] * z) / (1 + exp(alpha0 + alpha1[j] * z))
    ## randomly generate binary variable X from the above probability
    x <- rbinom(n, size = 1, prob = px)
    ## randomly generate binary variable Y from the inverse logistic function
    py <- exp(beta0 + beta1 * x + beta2 * z) / (1 + exp(beta0 + beta1 * x + beta2 * z))
    y <- rbinom(n, size = 1, prob = py)
    ## combine three random variables into a data frame
    dat <- data.frame(lung = y, coffee = x, smoke = z)
    ## fit unadjusted logistic regression model
    unadj.mod <- glm(lung ~ coffee, data = dat, family = "binomial")
    unadj.coef <- summary(unadj.mod)$coef
    unadj_p <- unadj.coef[2,4]
    ## fit adjusted logistic regression model
    adj.mod <- glm(lung ~ coffee + smoke, data = dat, family = "binomial")
    adj.coef <- summary(adj.mod)$coef
    adj_p <- adj.coef[2,4]
    
    type1_err_unadj[i] <- as.numeric(unadj_p < alpha)
    type1_err_adj[i] <- as.numeric(adj_p < alpha)
  }
  results_unadj[j] <- mean(type1_err_unadj)
  results_adj[j] <- mean(type1_err_adj)
}
results_table <- data.frame(
  "α1" = alpha1_values,
  "Adjusted Type I Error" = results_adj,
  "Unadjusted Type I Error" = results_unadj
  
)
