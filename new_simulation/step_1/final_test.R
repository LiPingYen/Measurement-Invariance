library(lavaan)
library(dplyr)

# parameters setting
obs_n <- 5000
ind_n <- 16
eta_n <- 1
seed <- 123
nu <- matrix(rep(1, ind_n),
             nrow = ind_n,
             ncol = 1,
             byrow = TRUE)
first_lam <- 0.9
lambda <- matrix(
  rep(first_lam, ind_n),
  nrow = ind_n,
  ncol = 1,
  byrow = TRUE
)
epsilon_mean <- 0
epsilon_sd <- 0.6
eta_mean <- 0
eta_sd <- 1

# function for generating all observation and indicators and eta
gen_obs <-
  function(ind_n,
           eta_n,
           nu,
           eta_mean,
           eta_sd,
           epsilon_mean,
           epsilon_sd,
           lambda) {
    eta_value <-
      rnorm(eta_n, mean = eta_mean, sd = eta_sd)
    epsilon_value <-
      rnorm(ind_n, mean = epsilon_mean , sd = epsilon_sd)
    epsilon <-  matrix(epsilon_value, nrow = ind_n)
    eta <- matrix(eta_value, nrow = eta_n)
    y <- nu + lambda %*% eta + epsilon
    c(y, eta)
  }

gen_all <-
  function(seed,
           obs_n,
           ind_n,
           eta_n,
           nu,
           eta_mean,
           eta_sd,
           epsilon_mean,
           epsilon_sd,
           lambda) {
    set.seed(seed)
    y_all_eta <- matrix(replicate(
      obs_n,
      gen_obs(
        ind_n = ind_n,
        eta_n = eta_n,
        nu = nu,
        eta_mean = eta_mean,
        eta_sd = eta_sd,
        epsilon_mean = epsilon_mean,
        epsilon_sd = epsilon_sd,
        lambda = lambda
      )
    ),
    ncol = ind_n + 1,
    byrow = TRUE)
    y_all_eta <- data.frame(y_all_eta)
    colnames(y_all_eta) <- c(paste0("x", 1:ind_n), "factor_score")
    y_all_eta
  }

# generate all sample

ind_eta <- gen_all(
  seed = seed,
  obs_n = obs_n,
  ind_n = ind_n,
  eta_n = eta_n,
  nu = nu,
  eta_mean = eta_mean,
  eta_sd = eta_sd,
  epsilon_mean = epsilon_mean,
  epsilon_sd = epsilon_sd,
  lambda = lambda
)

# cfa

model <-
  paste0(
    "factor",
    eta_n,
    "=~",
    first_lam,
    "*x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+x15+x16"
  )

fit <- cfa(data = ind_eta[, c(1:ind_n)], model = model)
summary(fit)

# cor

pred_factor_score <- predict(fit)
true_factor_score <- ind_eta[, 6]
sum_score <- apply(ind_eta[, 1:5], 1, sum)
outcome <- data.frame(pred_factor_score, true_factor_score, sum_score)
cor(outcome)
