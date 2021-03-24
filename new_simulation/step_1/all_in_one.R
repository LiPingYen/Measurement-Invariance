library(lavaan)
library(dplyr)
library(corrplot)

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

# run parameters settings

# cfa model selection
if (ind_n == 8) {
  model <-
    paste0("factor", eta_n, "=~", first_lam, "*x1+x2+x3+x4+x5+x6+x7+x8")
} else{
  model <-
    paste0(
      "factor",
      eta_n,
      "=~",
      first_lam,
      "*x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+x15+x16"
    )
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
fit <- cfa(data = ind_eta[, c(1:ind_n)], model = model)

# cor
pred_factor_score <- predict(fit)
colnames(pred_factor_score) <- "predict_factor_score"
true_factor_score <- ind_eta$factor_score
sum_score <- apply(ind_eta[, 1:ind_n], 1, sum)
outcome <-
  data.frame(pred_factor_score, true_factor_score, sum_score)
cor(outcome)

# plot
corrplot(cor(outcome), method="number")
