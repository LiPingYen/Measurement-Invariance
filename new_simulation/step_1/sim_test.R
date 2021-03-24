library(lavaan)
library(dplyr)

# parameters setting
obn <- 5000
ind_n <- 5
eta_n <- 1
seed <- 123
nu <- matrix(c(1,
               1,
               1,
               1,
               1),
             nrow = ind_n,
             ncol = 1,
             byrow = TRUE)
lambda <- matrix(
  c(0.8,
    0.8,
    0.8,
    0.8,
    0.8),
  nrow = ind_n,
  ncol = 1,
  byrow = TRUE
)
epsilon_mean <- 0
epsilon_sd <- 0.6
eta_mean <- 0
eta_sd <- 1

########################### don't run ####
eta_value <-
  rnorm(eta_n, mean = eta_mean, sd = eta_sd)
epsilon_value <-
  rnorm(ind_n, mean = epsilon_mean , sd = epsilon_sd)
epsilon <-  matrix(epsilon_value, nrow = 5)
eta <- matrix(eta_value, nrow = 5)
y <- nu + lambda %*% eta + epsilon
y <- data.frame(t(y))
##########################################

gen_y <-
  function(ind_n,
           eta_n,
           nu,
           eta_mean,
           eta_sd,
           epsilon_mean,
           epsilon_sd,
           lambda,
           eta) {
    eta_value <-
      rnorm(eta_n, mean = eta_mean, sd = eta_sd)
    epsilon_value <-
      rnorm(ind_n, mean = epsilon_mean , sd = epsilon_sd)
    epsilon <-  matrix(epsilon_value, nrow = ind_n)
    eta <- matrix(eta_value, nrow = eta_n)
    y <- nu + lambda %*% eta + epsilon
    c(y, eta)
  }

# generate one observation
gen_y(
  ind_n = ind_n,
  eta_n = eta_n,
  nu = nu,
  eta_mean = eta_mean,
  eta_sd = eta_sd,
  epsilon_mean = epsilon_mean,
  epsilon_sd = epsilon_sd,
  lambda = lambda
)

# generate all observation
set.seed(seed)
y_all_eta <- matrix(replicate(
  obn,
  gen_y(
    ind_n = ind_n,
    eta_n = eta_n,
    nu = nu,
    eta_mean = eta_mean,
    eta_sd = eta_sd,
    epsilon_mean = epsilon_mean,
    epsilon_sd = epsilon_sd,
    lambda = lambda
  )
), ncol = ind_n + 1, byrow = TRUE)
colnames(y_all_eta) <- c(paste0("x",1:ind_n),"factor_score")
y_all <- data.frame(y_all_eta[, c(1:5)])
colnames(y_all) <- c(paste0("x",1:ind_n))
orin_factor_score <- data.frame(y_all_eta[, 6])
colnames(orin_factor_score) <- "factor_score"

# cfa

md <- '
fac1=~0.8*X1+X2+X3+X4+X5
'

fit <- cfa(data = y_all, model = md)

summary(fit)

pred_factor_score <- predict(fit)

# correlation
cor(orin_factor_score, pred_factor_score)

# mean square
sum((pred_factor_score - orin_factor_score) ^ 2) / obn


# sum all the score and cor with eta
sum_score <- apply(y_all, 1, sum)

cor(sum_score, orin_factor_score)
