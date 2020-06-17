library(dplyr)
library(MASS)
library(mvtnorm)
library(lavaan)
library(semTools)
library(simsem)


####generate population####
##variable=6
#first group
lambda1 <- matrix(c(0.7, 0.9, 0.5, 0.6, 0.8, 0.3), nrow = 6)
phi1 <- 1
theta1 <- diag(c(0.8, 1.3, 0.4, 0.5, 0.9, 0.2))
pop_co_ma1 <- lambda1 %*% phi1 %*% t(lambda1) + theta1

#second group
##con:low, LD: small
lambda2_1 <- matrix(c(0.6, 0.77, 0.5, 0.6, 0.8, 0.3), nrow = 6)
phi2 <- 1.3
theta2_1 <- diag(c(0.8, 1.3, 0.4, 0.5, 0.9, 0.2))
pop_co_ma2_1 <- lambda2_1 %*% phi2 %*% t(lambda2_1) + theta2_1

pop_co_ma1_1 <- lambda1 %*% phi1 %*% t(lambda1) + theta2_1

##con:low, LD: medium
lambda2_2 <- matrix(c(0.5, 0.64, 0.5, 0.6, 0.8, 0.3), nrow = 6)
phi2 <- 1.3
theta2_2 <- diag(c(0.7, 1.3, 0.4, 0.5, 0.9, 0.2))
pop_co_ma2_2 <- lambda2_2 %*% phi2 %*% t(lambda2_2) + theta2_2

pop_co_ma1_2 <- lambda1 %*% phi1 %*% t(lambda1) + theta2_2

##con:low, LD: large
lambda2_3 <- matrix(c(0.4, 0.51, 0.5, 0.6, 0.8, 0.3), nrow = 6)
phi2 <- 1.3
theta2_3 <- diag(c(0.7, 1.2, 0.4, 0.5, 0.9, 0.2))
pop_co_ma2_3 <- lambda2_3 %*% phi2 %*% t(lambda2_3) + theta2_3

pop_co_ma1_3 <- lambda1 %*% phi1 %*% t(lambda1) + theta2_3

##con:high, LD: small
lambda2_4 <- matrix(c(0.6, 0.77, 0.5, 0.51, 0.69, 0.3), nrow = 6)
phi2 <- 1.3
theta2_4 <- diag(c(0.8, 1.3, 0.4, 0.6, 0.9, 0.2))
pop_co_ma2_4 <- lambda2_4 %*% phi2 %*% t(lambda2_4) + theta2_4

pop_co_ma1_4 <- lambda1 %*% phi1 %*% t(lambda1) + theta2_4

##con:high, LD: medium
lambda2_5 <- matrix(c(0.5, 0.64, 0.5, 0.43, 0.57, 0.3), nrow = 6)
phi2 <- 1.3
theta2_5 <- diag(c(0.7, 1.3, 0.4, 0.5, 0.9, 0.2))
pop_co_ma2_5 <- lambda2_5 %*% phi2 %*% t(lambda2_5) + theta2_5

pop_co_ma1_5 <- lambda1 %*% phi1 %*% t(lambda1) + theta2_5

##con:high, LD: large
lambda2_6 <- matrix(c(0.4, 0.51, 0.5, 0.34, 0.46, 0.3), nrow = 6)
phi2 <- 1.3
theta2_6 <- diag(c(0.7, 1.2, 0.4, 0.5, 0.9, 0.2))
pop_co_ma2_6 <- lambda2_6 %*% phi2 %*% t(lambda2_6) + theta2_6

pop_co_ma1_6 <- lambda1 %*% phi1 %*% t(lambda1) + theta2_6

##con:high, LD: large, mixed
lambda2_7 <- matrix(c(0.4, 0.51, 0.5, 0.86, 1.14, 0.3), nrow = 6)
phi2 <- 1.3
theta2_7 <- diag(c(0.7, 1.2, 0.4, 1.1, 1.8, 0.2))
pop_co_ma2_7 <- lambda2_7 %*% phi2 %*% t(lambda2_7) + theta2_7

pop_co_ma1_7 <- lambda1 %*% phi1 %*% t(lambda1) + theta2_7

##con:high, LD: large, nonpropotional
lambda2_8 <- matrix(c(0.25, 0.65, 0.5, 0.45, 0.45, 0.3), nrow = 6)
phi2 <- 1.3
theta2_8 <- diag(c(0.5, 1.2, 0.4, 0.4, 0.9, 0.2))
pop_co_ma2_8 <- lambda2_8 %*% phi2 %*% t(lambda2_8) + theta2_8

pop_co_ma1_8 <- lambda1 %*% phi1 %*% t(lambda1) + theta2_8


####generate data####
# number of observations to simulate
nobs = 200
# Cholesky decomposition
L1_1 = chol(pop_co_ma1_1)
nvars = dim(L1_1)[1]
r1_1 = t(L1_1) %*% matrix(rnorm(nvars * nobs), nrow = nvars, ncol = nobs)
r1_1 = t(r1_1)
r1_1_dta = as.data.frame(r1_1)

####generate data with package mvtnorm::rmvnorm####
##con:low, LD: small
set.seed(66)
dta1_1 <-
  data.frame(rmvnorm(100, rep(0, nrow(pop_co_ma1_1)), sigma = pop_co_ma1_1, method =
                       "chol"))

set.seed(66)
dta2_1 <-
  data.frame(rmvnorm(100, rep(0, nrow(pop_co_ma2_1)), sigma = pop_co_ma2_1, method =
                       "chol"))

##con:low, LD: medium
set.seed(66)
dta1_2 <-
  data.frame(rmvnorm(100, rep(0, nrow(pop_co_ma1_2)), sigma = pop_co_ma1_2, method =
                       "chol"))

set.seed(66)
dta2_2 <-
  data.frame(rmvnorm(100, rep(0, nrow(pop_co_ma2_2)), sigma = pop_co_ma2_2, method =
                       "chol"))

##con:low, LD: large
set.seed(66)
dta1_3 <-
  data.frame(rmvnorm(100, rep(0, nrow(pop_co_ma1_3)), sigma = pop_co_ma1_3, method =
                       "chol"))

set.seed(66)
dta2_3 <-
  data.frame(rmvnorm(100, rep(0, nrow(pop_co_ma2_3)), sigma = pop_co_ma2_3, method =
                       "chol"))

##con:high, LD: small
set.seed(66)
dta1_4 <-
  data.frame(rmvnorm(100, rep(0, nrow(pop_co_ma1_4)), sigma = pop_co_ma1_4, method =
                       "chol"))

set.seed(66)
dta2_4 <-
  data.frame(rmvnorm(100, rep(0, nrow(pop_co_ma2_4)), sigma = pop_co_ma2_4, method =
                       "chol"))

##con:high, LD: medium
set.seed(66)
dta1_5 <-
  data.frame(rmvnorm(100, rep(0, nrow(pop_co_ma1_5)), sigma = pop_co_ma1_5, method =
                       "chol"))

set.seed(66)
dta2_5 <-
  data.frame(rmvnorm(100, rep(0, nrow(pop_co_ma2_5)), sigma = pop_co_ma2_5, method =
                       "chol"))

##con:high, LD: large
set.seed(66)
dta1_6 <-
  data.frame(rmvnorm(100, rep(0, nrow(pop_co_ma1_6)), sigma = pop_co_ma1_6, method =
                       "chol"))

set.seed(66)
dta2_6 <-
  data.frame(rmvnorm(100, rep(0, nrow(pop_co_ma2_6)), sigma = pop_co_ma2_6, method =
                       "chol"))

##con:high, LD: large, mixed
set.seed(66)
dta1_7 <-
  data.frame(rmvnorm(100, rep(0, nrow(pop_co_ma1_7)), sigma = pop_co_ma1_7, method =
                       "chol"))

set.seed(66)
dta2_7 <-
  data.frame(rmvnorm(100, rep(0, nrow(pop_co_ma2_7)), sigma = pop_co_ma2_7, method =
                       "chol"))

##con:high, LD: large, nonpropotional
set.seed(66)
dta1_8 <-
  data.frame(rmvnorm(100, rep(0, nrow(pop_co_ma1_8)), sigma = pop_co_ma1_8, method =
                       "chol"))

set.seed(66)
dta2_8 <-
  data.frame(rmvnorm(100, rep(0, nrow(pop_co_ma2_8)), sigma = pop_co_ma2_8, method =
                       "chol"))

####arange data####
##con:low, LD: small
dta1 <- rbind(dta1_1, dta2_1) %>%
  rename(
    v1 = X1,
    v2 = X2,
    v3 = X3,
    v4 = X4,
    v5 = X5,
    v6 = X6
  ) %>%
  mutate(group = c(rep(1, 100), rep(2, 100)))

##con:low, LD: medium
dta2 <- rbind(dta1_2, dta2_2) %>%
  rename(
    v1 = X1,
    v2 = X2,
    v3 = X3,
    v4 = X4,
    v5 = X5,
    v6 = X6
  ) %>%
  mutate(group = c(rep(1, 100), rep(2, 100)))

##con:low, LD: large
dta3 <- rbind(dta1_3, dta2_3) %>%
  rename(
    v1 = X1,
    v2 = X2,
    v3 = X3,
    v4 = X4,
    v5 = X5,
    v6 = X6
  ) %>%
  mutate(group = c(rep(1, 100), rep(2, 100)))

##con:high, LD: small
dta4 <- rbind(dta1_4, dta2_4) %>%
  rename(
    v1 = X1,
    v2 = X2,
    v3 = X3,
    v4 = X4,
    v5 = X5,
    v6 = X6
  ) %>%
  mutate(group = c(rep(1, 100), rep(2, 100)))

##con:high, LD: medium
dta5 <- rbind(dta1_5, dta2_5) %>%
  rename(
    v1 = X1,
    v2 = X2,
    v3 = X3,
    v4 = X4,
    v5 = X5,
    v6 = X6
  ) %>%
  mutate(group = c(rep(1, 100), rep(2, 100)))

##con:high, LD: large
dta6 <- rbind(dta1_6, dta2_6) %>%
  rename(
    v1 = X1,
    v2 = X2,
    v3 = X3,
    v4 = X4,
    v5 = X5,
    v6 = X6
  ) %>%
  mutate(group = c(rep(1, 100), rep(2, 100)))

##con:high, LD: large, mixed
dta7 <- rbind(dta1_7, dta2_7) %>%
  rename(
    v1 = X1,
    v2 = X2,
    v3 = X3,
    v4 = X4,
    v5 = X5,
    v6 = X6
  ) %>%
  mutate(group = c(rep(1, 100), rep(2, 100)))

##con:high, LD: large, nonpropotional
dta8 <- rbind(dta1_8, dta2_8) %>%
  rename(
    v1 = X1,
    v2 = X2,
    v3 = X3,
    v4 = X4,
    v5 = X5,
    v6 = X6
  ) %>%
  mutate(group = c(rep(1, 100), rep(2, 100)))

####Confirmatory analysis####
md <- "
fac1=~NA*v1+v2+v3+v4+v5+v6
fac1~~c(1,NA)*fac1
"

##con:low, LD: small
lav_fit_base1 <-
  cfa(
    model = md,
    data = dta1,
    group = "group",
    std.lv = TRUE,
    group.equal = c("loadings")
  )
summary(lav_fit_base1,
        standardized = TRUE,
        fit.measures = TRUE)


##con:low, LD: medium
lav_fit_base2 <-
  cfa(
    model = md,
    data = dta2,
    group = "group",
    std.lv = TRUE,
    group.equal = c("loadings")
  )
summary(lav_fit_base1,
        standardized = TRUE,
        fit.measures = TRUE)


##con:low, LD: large
lav_fit_base3 <-
  cfa(
    model = md,
    data = dta3,
    group = "group",
    std.lv = TRUE,
    group.equal = c("loadings")
  )
summary(lav_fit_base3,
        standardized = TRUE,
        fit.measures = TRUE)


##con:high, LD: small
lav_fit_base4 <-
  cfa(
    model = md,
    data = dta4,
    group = "group",
    std.lv = TRUE,
    group.equal = c("loadings")
  )
summary(lav_fit_base4,
        standardized = TRUE,
        fit.measures = TRUE)


##con:high, LD: medium
lav_fit_base5 <-
  cfa(
    model = md,
    data = dta5,
    group = "group",
    std.lv = TRUE,
    group.equal = c("loadings")
  )
summary(lav_fit_base5,
        standardized = TRUE,
        fit.measures = TRUE)


##con:high, LD: large
lav_fit_base6 <-
  cfa(
    model = md,
    data = dta6,
    group = "group",
    std.lv = TRUE,
    group.equal = c("loadings")
  )
summary(lav_fit_base6,
        standardized = TRUE,
        fit.measures = TRUE)


##con:high, LD: large, mixed
lav_fit_base7 <-
  cfa(
    model = md,
    data = dta7,
    group = "group",
    std.lv = TRUE,
    group.equal = c("loadings")
  )
summary(lav_fit_base7,
        standardized = TRUE,
        fit.measures = TRUE)


##con:high, LD: large, nonpropotional
lav_fit_base8 <-
  cfa(
    model = md,
    data = dta8,
    group = "group",
    std.lv = TRUE,
    group.equal = c("loadings")
  )
summary(lav_fit_base8,
        standardized = TRUE,
        fit.measures = TRUE)
