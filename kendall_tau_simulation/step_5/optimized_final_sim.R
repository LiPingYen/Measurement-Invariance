library(lavaan)
library(dplyr, warn.conflicts = FALSE)
library(ggplot2)
library(pcaPP)
library(tictoc)
library(dtplyr)
library(latex2exp)
library(ggpubr)
library(dqrng)
options(dplyr.summarise.inform = FALSE)


# parameters setting ------------------------------------------------------


seed <- 123
rep <- 500
obs_n <- 100
eta_n <- 1
ind_n <- c(5, 10, 15)
lambda_value <-
  list(c(0.5, 0.3, 0.4, 0.4, 0.4),
       c(0.6, 0.4, 0.5, 0.5, 0.5),
       c(0.7, 0.5, 0.6, 0.6, 0.6))
lambda_label <- c("small", "medium", "large")
epsilon_mean <- 0
eta_mean <- 0
eta_sd <- 1
nu1_value <- 0
nu2_value <- c(0, 0.1, 0.3, 0.5)
non_inv_label <- c("none", "small", "medium", "large")
pror_non_inv_label <- c("0.2", "0.4")


# simulation function -----------------------------------------------------


sim <-
  function(seed,
           rep,
           obs_n,
           eta_n,
           ind_n,
           lambda_value,
           nu_value,
           epsilon_mean,
           eta_mean,
           eta_sd) {
    tic.clearlog()
    tic("total")
    set.seed(seed)
    n <- 1
    run_time <- 0
    all_outcome <- vector(mode = "list", length = rep)
    while (n <= rep) {
      a <- 1
      stop <-  FALSE
      sub_outcome <- vector(mode = "list", length = 72)
      for (i in 1:3) {
        for (j in 1:3) {
          for (p in 1:4) {
            for (q in 1:2) {
              lambda <- matrix(
                rep(lambda_value[[j]], i),
                nrow = ind_n[i],
                ncol = 1,
                byrow = TRUE
              )
              epsilon_sd <- sqrt(1 - lambda ^ 2)
              epsilon <- matrix(ncol = 1,
                                nrow = ind_n[i],
                                byrow = TRUE)
              nu1 <- matrix(
                rep(nu1_value, ind_n[i]),
                nrow = ind_n[i],
                ncol = 1,
                byrow = TRUE
              )
              dta1 <- matrix(ncol = ind_n[i] + 2,
                             nrow = obs_n,
                             byrow = TRUE)
              for (k in seq_len(obs_n)) {
                eta_value <-
                  dqrnorm(eta_n, mean = eta_mean, sd = eta_sd)
                eta <- matrix(
                  eta_value,
                  nrow = eta_n,
                  ncol = 1,
                  byrow = TRUE
                )
                for (m in seq_len(ind_n[i])) {
                  epsilon[m] <-
                    dqrnorm(1, mean = epsilon_mean, sd = epsilon_sd[m, 1])
                }
                y <- nu1 + lambda %*% eta + epsilon
                dta1[k, ] <- c(y, eta, 1)
              }
              if (q == 1) {
                nu2 <- matrix(
                  rep(c(0, 0, 0, 0, nu2_value[p]), i),
                  nrow = ind_n[i],
                  ncol = 1,
                  byrow = TRUE
                )
              } else{
                nu2 <- matrix(
                  rep(c(
                    0, 0, 0, nu2_value[p], nu2_value[p]
                  ), i),
                  nrow = ind_n[i],
                  ncol = 1,
                  byrow = TRUE
                )
              }
              dta2 <- matrix(ncol = ind_n[i] + 2,
                             nrow = obs_n,
                             byrow = TRUE)
              for (k in seq_len(obs_n)) {
                eta_value <-
                  dqrnorm(eta_n, mean = eta_mean, sd = eta_sd)
                eta <- matrix(
                  eta_value,
                  nrow = eta_n,
                  ncol = 1,
                  byrow = TRUE
                )
                for (m in seq_len(ind_n[i])) {
                  epsilon[m] <-
                    dqrnorm(1, mean = epsilon_mean, sd = epsilon_sd[m, 1])
                }
                y <- nu2 + lambda %*% eta + epsilon
                dta2[k, ] <- c(y, eta, 2)
              }
              dta_all <- rbind(dta1, dta2)
              colnames(dta_all) <-
                c(paste0("x", 1:ind_n[i]),
                  "true_factor_score",
                  "group")
              colnames(dta1) <-
                c(paste0("x", 1:ind_n[i]),
                  "true_factor_score",
                  "group")
              colnames(dta2) <-
                c(paste0("x", 1:ind_n[i]),
                  "true_factor_score",
                  "group")
              if (i == 1) {
                if (p == 1) {
                  model_all <-
                    paste0(
                      "factor",
                      eta_n,
                      "=~",
                      lambda_value[[j]][1],
                      "*x1+lm2*x2+lm3*x3+lm4*x4+lm5*x5",
                      "
                    factor",
                    eta_n,
                    "~c(0,NA)*1",
                    "
                    x1~tau1*1
                    x2~tau2*1
                    x3~tau3*1
                    x4~tau4*1
                    x5~tau5*1"
                    )
                  if (q == 1) {
                    dta_inv <- dta_all[, -5]
                    model_inv <- paste0(
                      "factor",
                      eta_n,
                      "=~",
                      lambda_value[[j]][1],
                      "*x1+lm2*x2+lm3*x3+lm4*x4",
                      "
                    factor",
                    eta_n,
                    "~c(0,NA)*1",
                    "
                    x1~tau1*1
                    x2~tau2*1
                    x3~tau3*1
                    x4~tau4*1"
                    )
                  } else{
                    dta_inv <- dta_all[, -c(4, 5)]
                    model_inv <- paste0(
                      "factor",
                      eta_n,
                      "=~",
                      lambda_value[[j]][1],
                      "*x1+lm2*x2+lm3*x3",
                      "
                    factor",
                    eta_n,
                    "~c(0,NA)*1",
                    "
                    x1~tau1*1
                    x2~tau2*1
                    x3~tau3*1"
                    )
                  }
                } else if (q == 1) {
                  model_all <-
                    paste0(
                      "factor",
                      eta_n,
                      "=~",
                      lambda_value[[j]][1],
                      "*x1+lm2*x2+lm3*x3+lm4*x4+lm5*x5",
                      "
                    factor",
                    eta_n,
                    "~c(0,NA)*1",
                    "
                    x1~tau1*1
                    x2~tau2*1
                    x3~tau3*1
                    x4~tau4*1
                    x5~c(tau51,tau52)*1"
                    )
                  dta_inv <- dta_all[, -which(nu2 != 0)]
                  model_inv <- paste0(
                    "factor",
                    eta_n,
                    "=~",
                    lambda_value[[j]][1],
                    "*x1+lm2*x2+lm3*x3+lm4*x4",
                    "
                    factor",
                    eta_n,
                    "~c(0,NA)*1",
                    "
                    x1~tau1*1
                    x2~tau2*1
                    x3~tau3*1
                    x4~tau4*1"
                  )
                } else{
                  model_all <-
                    paste0(
                      "factor",
                      eta_n,
                      "=~",
                      lambda_value[[j]][1],
                      "*x1+lm2*x2+lm3*x3+lm4*x4+lm5*x5",
                      "
                    factor",
                    eta_n,
                    "~c(0,NA)*1",
                    "
                    x1~tau1*1
                    x2~tau2*1
                    x3~tau3*1
                    x4~c(tau41,tau42)*1
                    x5~c(tau51,tau52)*1"
                    )
                  dta_inv <- dta_all[, -which(nu2 != 0)]
                  model_inv <- paste0(
                    "factor",
                    eta_n,
                    "=~",
                    lambda_value[[j]][1],
                    "*x1+lm2*x2+lm3*x3",
                    "
                    factor",
                    eta_n,
                    "~c(0,NA)*1",
                    "
                    x1~tau1*1
                    x2~tau2*1
                    x3~tau3*1"
                  )
                }
              } else if (i == 2) {
                if (p == 1) {
                  model_all <-
                    paste0(
                      "factor",
                      eta_n,
                      "=~",
                      lambda_value[[j]][1],
                      "*x1+lm2*x2+lm3*x3+lm4*x4+lm5*x5+lm6*x6+lm7*x7+lm8*x8+lm9*x9+lm10*x10",
                      "
                    factor",
                    eta_n,
                    "~c(0,NA)*1",
                    "
                    x1~tau1*1
                    x2~tau2*1
                    x3~tau3*1
                    x4~tau4*1
                    x5~tau5*1
                    x6~tau6*1
                    x7~tau7*1
                    x8~tau8*1
                    x9~tau9*1
                    x10~tau10*1"
                    )
                  if (q == 1) {
                    dta_inv <- dta_all[, -c(5, 10)]
                    model_inv <-
                      paste0(
                        "factor",
                        eta_n,
                        "=~",
                        lambda_value[[j]][1],
                        "*x1+lm2*x2+lm3*x3+lm4*x4+lm6*x6+lm7*x7+lm8*x8+lm9*x9",
                        "
                    factor",
                    eta_n,
                    "~c(0,NA)*1",
                    "
                    x1~tau1*1
                    x2~tau2*1
                    x3~tau3*1
                    x4~tau4*1
                    x6~tau6*1
                    x7~tau7*1
                    x8~tau8*1
                    x9~tau9*1"
                      )
                  } else{
                    dta_inv <- dta_all[, -c(4, 5, 9, 10)]
                    model_inv <-
                      paste0(
                        "factor",
                        eta_n,
                        "=~",
                        lambda_value[[j]][1],
                        "*x1+lm2*x2+lm3*x3+lm6*x6+lm7*x7+lm8*x8",
                        "
                    factor",
                    eta_n,
                    "~c(0,NA)*1",
                    "
                    x1~tau1*1
                    x2~tau2*1
                    x3~tau3*1
                    x6~tau6*1
                    x7~tau7*1
                    x8~tau8*1"
                      )
                  }
                } else if (q == 1) {
                  model_all <-
                    paste0(
                      "factor",
                      eta_n,
                      "=~",
                      lambda_value[[j]][1],
                      "*x1+lm2*x2+lm3*x3+lm4*x4+lm5*x5+lm6*x6+lm7*x7+lm8*x8+lm9*x9+lm10*x10",
                      "
                    factor",
                    eta_n,
                    "~c(0,NA)*1",
                    "
                    x1~tau1*1
                    x2~tau2*1
                    x3~tau3*1
                    x4~tau4*1
                    x5~c(tau51,tau52)*1
                    x6~tau6*1
                    x7~tau7*1
                    x8~tau8*1
                    x9~tau9*1
                    x10~c(tau101,tau102)*1"
                    )
                  dta_inv <- dta_all[, -which(nu2 != 0)]
                  model_inv <-
                    paste0(
                      "factor",
                      eta_n,
                      "=~",
                      lambda_value[[j]][1],
                      "*x1+lm2*x2+lm3*x3+lm4*x4+lm6*x6+lm7*x7+lm8*x8+lm9*x9",
                      "
                    factor",
                    eta_n,
                    "~c(0,NA)*1",
                    "
                    x1~tau1*1
                    x2~tau2*1
                    x3~tau3*1
                    x4~tau4*1
                    x6~tau6*1
                    x7~tau7*1
                    x8~tau8*1
                    x9~tau9*1"
                    )
                } else{
                  model_all <-
                    paste0(
                      "factor",
                      eta_n,
                      "=~",
                      lambda_value[[j]][1],
                      "*x1+lm2*x2+lm3*x3+lm4*x4+lm5*x5+lm6*x6+lm7*x7+lm8*x8+lm9*x9+lm10*x10",
                      "
                    factor",
                    eta_n,
                    "~c(0,NA)*1",
                    "
                    x1~tau1*1
                    x2~tau2*1
                    x3~tau3*1
                    x4~c(tau41,tau42)*1
                    x5~c(tau51,tau52)*1
                    x6~tau6*1
                    x7~tau7*1
                    x8~tau8*1
                    x9~c(tau91,tau92)*1
                    x10~c(tau101,tau102)*1"
                    )
                  dta_inv <- dta_all[, -which(nu2 != 0)]
                  model_inv <-
                    paste0(
                      "factor",
                      eta_n,
                      "=~",
                      lambda_value[[j]][1],
                      "*x1+lm2*x2+lm3*x3+lm6*x6+lm7*x7+lm8*x8",
                      "
                    factor",
                    eta_n,
                    "~c(0,NA)*1",
                    "
                    x1~tau1*1
                    x2~tau2*1
                    x3~tau3*1
                    x6~tau6*1
                    x7~tau7*1
                    x8~tau8*1"
                    )
                }
              } else{
                if (p == 1) {
                  model_all <-
                    paste0(
                      "factor",
                      eta_n,
                      "=~",
                      lambda_value[[j]][1],
                      "*x1+lm2*x2+lm3*x3+lm4*x4+lm5*x5+lm6*x6+lm7*x7+lm8*x8+lm9*x9+lm10*x10+lm11*x11+lm12*x12+lm13*x13+lm14*x14+lm15*x15",
                      "
                    factor",
                    eta_n,
                    "~c(0,NA)*1",
                    "
                    x1~tau1*1
                    x2~tau2*1
                    x3~tau3*1
                    x4~tau4*1
                    x5~tau5*1
                    x6~tau6*1
                    x7~tau7*1
                    x8~tau8*1
                    x9~tau9*1
                    x10~tau10*1
                    x11~tau11*1
                    x12~tau12*1
                    x13~tau13*1
                    x14~tau14*1
                    x15~tau15*1"
                    )
                  if (q == 1) {
                    dta_inv <- dta_all[, -c(5, 10, 15)]
                    model_inv <-
                      paste0(
                        "factor",
                        eta_n,
                        "=~",
                        lambda_value[[j]][1],
                        "*x1+lm2*x2+lm3*x3+lm4*x4+lm6*x6+lm7*x7+lm8*x8+lm9*x9+lm11*x11+lm12*x12+lm13*x13+lm14*x14",
                        "
                    factor",
                    eta_n,
                    "~c(0,NA)*1",
                    "
                    x1~tau1*1
                    x2~tau2*1
                    x3~tau3*1
                    x4~tau4*1
                    x6~tau6*1
                    x7~tau7*1
                    x8~tau8*1
                    x9~tau9*1
                    x11~tau11*1
                    x12~tau12*1
                    x13~tau13*1
                    x14~tau14*1"
                      )
                  } else{
                    dta_inv <- dta_all[, -c(4, 5, 9, 10, 14, 15)]
                    model_inv <-
                      paste0(
                        "factor",
                        eta_n,
                        "=~",
                        lambda_value[[j]][1],
                        "*x1+lm2*x2+lm3*x3+lm6*x6+lm7*x7+lm8*x8+lm11*x11+lm12*x12+lm13*x13",
                        "
                    factor",
                    eta_n,
                    "~c(0,NA)*1",
                    "
                    x1~tau1*1
                    x2~tau2*1
                    x3~tau3*1
                    x6~tau6*1
                    x7~tau7*1
                    x8~tau8*1
                    x11~tau11*1
                    x12~tau12*1
                    x13~tau13*1"
                      )
                  }
                } else if (q == 1) {
                  model_all <-
                    paste0(
                      "factor",
                      eta_n,
                      "=~",
                      lambda_value[[j]][1],
                      "*x1+lm2*x2+lm3*x3+lm4*x4+lm5*x5+lm6*x6+lm7*x7+lm8*x8+lm9*x9+lm10*x10+lm11*x11+lm12*x12+lm13*x13+lm14*x14+lm15*x15",
                      "
                    factor",
                    eta_n,
                    "~c(0,NA)*1",
                    "
                    x1~tau1*1
                    x2~tau2*1
                    x3~tau3*1
                    x4~tau4*1
                    x5~c(tau51,tau52)*1
                    x6~tau6*1
                    x7~tau7*1
                    x8~tau8*1
                    x9~tau9*1
                    x10~c(tau101,tau102)*1
                    x11~tau11*1
                    x12~tau12*1
                    x13~tau13*1
                    x14~tau14*1
                    x15~c(tau151,tau152)*1"
                    )
                  dta_inv <- dta_all[, -which(nu2 != 0)]
                  model_inv <-
                    paste0(
                      "factor",
                      eta_n,
                      "=~",
                      lambda_value[[j]][1],
                      "*x1+lm2*x2+lm3*x3+lm4*x4+lm6*x6+lm7*x7+lm8*x8+lm9*x9+lm11*x11+lm12*x12+lm13*x13+lm14*x14",
                      "
                    factor",
                    eta_n,
                    "~c(0,NA)*1",
                    "
                    x1~tau1*1
                    x2~tau2*1
                    x3~tau3*1
                    x4~tau4*1
                    x6~tau6*1
                    x7~tau7*1
                    x8~tau8*1
                    x9~tau9*1
                    x11~tau11*1
                    x12~tau12*1
                    x13~tau13*1
                    x14~tau14*1"
                    )
                } else{
                  model_all <-
                    paste0(
                      "factor",
                      eta_n,
                      "=~",
                      lambda_value[[j]][1],
                      "*x1+lm2*x2+lm3*x3+lm4*x4+lm5*x5+lm6*x6+lm7*x7+lm8*x8+lm9*x9+lm10*x10+lm11*x11+lm12*x12+lm13*x13+lm14*x14+lm15*x15",
                      "
                    factor",
                    eta_n,
                    "~c(0,NA)*1",
                    "
                    x1~tau1*1
                    x2~tau2*1
                    x3~tau3*1
                    x4~c(tau41,tau42)*1
                    x5~c(tau51,tau52)*1
                    x6~tau6*1
                    x7~tau7*1
                    x8~tau8*1
                    x9~c(tau91,tau92)*1
                    x10~c(tau101,tau102)*1
                    x11~tau11*1
                    x12~tau12*1
                    x13~tau13*1
                    x14~c(tau141,tau142)*1
                    x15~c(tau151,tau152)*1"
                    )
                  dta_inv <- dta_all[, -which(nu2 != 0)]
                  model_inv <-
                    paste0(
                      "factor",
                      eta_n,
                      "=~",
                      lambda_value[[j]][1],
                      "*x1+lm2*x2+lm3*x3+lm6*x6+lm7*x7+lm8*x8+lm11*x11+lm12*x12+lm13*x13",
                      "
                    factor",
                    eta_n,
                    "~c(0,NA)*1",
                    "
                    x1~tau1*1
                    x2~tau2*1
                    x3~tau3*1
                    x6~tau6*1
                    x7~tau7*1
                    x8~tau8*1
                    x11~tau11*1
                    x12~tau12*1
                    x13~tau13*1"
                    )
                }
              }
              
              fit_all <-
                cfa(data = dta_all[, -(ncol(dta_all) - 1)],
                    model = model_all,
                    group = "group")
              fit_inv <-
                cfa(data = dta_inv[, -(ncol(dta_inv) - 1)],
                    model = model_inv,
                    group = "group")
              if (lavInspect(fit_all, "converged") == FALSE |
                  lavInspect(fit_inv, "converged") == FALSE) {
                run_time <- run_time + 1
                stop <-  TRUE
                break
              } else if (lavInspect(fit_all, "post.check") == FALSE |
                         lavInspect(fit_inv, "post.check") == FALSE) {
                run_time <- run_time + 1
                stop <-  TRUE
                break
              } else{
                run_time <- run_time + 1
                pred_factor_score_all <-
                  c(predict(fit_all)$`1`, predict(fit_all)$`2`)
                pred_factor_score_1 <- c(predict(fit_all)$`1`)
                pred_factor_score_2 <- c(predict(fit_all)$`2`)
                true_factor_score_all <- dta_all[, ind_n[i] + 1]
                true_factor_score_1 <- dta1[, ind_n[i] + 1]
                true_factor_score_2 <- dta2[, ind_n[i] + 1]
                sum_score_all <-
                  apply(dta_all[, 1:ind_n[i]], 1, sum)
                sum_score_1 <- apply(dta1[, 1:ind_n[i]], 1, sum)
                sum_score_2 <- apply(dta2[, 1:ind_n[i]], 1, sum)
                outcome_all <-
                  data.frame(pred_factor_score_all,
                             true_factor_score_all,
                             sum_score_all)
                pearson_cor_all <- cor(outcome_all)
                kendall_cor_all <-
                  cor.fk(outcome_all)
                outcome_1 <-
                  data.frame(pred_factor_score_1,
                             true_factor_score_1,
                             sum_score_1)
                kendall_cor_1 <- cor.fk(outcome_1)
                outcome_2 <-
                  data.frame(pred_factor_score_2,
                             true_factor_score_2,
                             sum_score_2)
                kendall_cor_2 <- cor.fk(outcome_2)
                kendall_cor_between <-
                  (
                    choose(obs_n * 2, 2) * kendall_cor_all - choose(obs_n, 2) * kendall_cor_1 -
                      choose(obs_n, 2) * kendall_cor_2
                  ) / (obs_n * obs_n)
                kendall_cor_1_2 <-
                  (kendall_cor_1 + kendall_cor_2) / 2
                colnames(kendall_cor_between) <-
                  c(
                    "pred_factor_score_betw",
                    "true_factor_score_betw",
                    "sum_score_betw"
                  )
                colnames(kendall_cor_1_2) <-
                  c("pred_factor_score_1_2",
                    "true_factor_score_1_2",
                    "sum_score_1_2")
                rownames(kendall_cor_between) <-
                  c(
                    "pred_factor_score_betw",
                    "true_factor_score_betw",
                    "sum_score_betw"
                  )
                rownames(kendall_cor_1_2) <-
                  c("pred_factor_score_1_2",
                    "true_factor_score_1_2",
                    "sum_score_betw")
                
                pred_factor_score_all_inv <-
                  c(predict(fit_inv)$`1`, predict(fit_inv)$`2`)
                pred_factor_score_1_inv <- c(predict(fit_inv)$`1`)
                pred_factor_score_2_inv <- c(predict(fit_inv)$`2`)
                true_factor_score_all_inv <-
                  dta_inv[, (ncol(dta_inv) - 1)]
                true_factor_score_1_inv <-
                  dta_inv[1:obs_n, (ncol(dta_inv) - 1)]
                true_factor_score_2_inv <-
                  dta_inv[(obs_n + 1):nrow(dta_inv), (ncol(dta_inv) - 1)]
                sum_score_all_inv <-
                  apply(dta_inv[, 1:(ncol(dta_inv) - 2)], 1, sum)
                sum_score_1_inv <-
                  apply(dta_inv[1:obs_n, 1:(ncol(dta_inv) - 2)], 1, sum)
                sum_score_2_inv <-
                  apply(dta_inv[(obs_n + 1):nrow(dta_inv), 1:(ncol(dta_inv) - 2)], 1, sum)
                outcome_all_inv <-
                  data.frame(
                    pred_factor_score_all_inv,
                    true_factor_score_all_inv,
                    sum_score_all_inv
                  )
                pearson_cor_all_inv <- cor(outcome_all_inv)
                kendall_cor_all_inv <-
                  cor.fk(outcome_all_inv)
                outcome_1_inv <-
                  data.frame(pred_factor_score_1_inv,
                             true_factor_score_1_inv,
                             sum_score_1_inv)
                kendall_cor_1_inv <-
                  cor.fk(outcome_1_inv)
                outcome_2_inv <-
                  data.frame(pred_factor_score_2_inv,
                             true_factor_score_2_inv,
                             sum_score_2_inv)
                kendall_cor_2_inv <-
                  cor.fk(outcome_2_inv)
                kendall_cor_between_inv <-
                  (
                    choose(obs_n * 2, 2) * kendall_cor_all_inv - choose(obs_n, 2) * kendall_cor_1_inv -
                      choose(obs_n, 2) * kendall_cor_2_inv
                  ) / (obs_n * obs_n)
                kendall_cor_1_2_inv <-
                  (kendall_cor_1_inv + kendall_cor_2_inv) / 2
                colnames(kendall_cor_between_inv) <-
                  c(
                    "pred_factor_score_betw",
                    "true_factor_score_betw",
                    "sum_score_betw"
                  )
                colnames(kendall_cor_1_2_inv) <-
                  c("pred_factor_score_1_2",
                    "true_factor_score_1_2",
                    "sum_score_1_2")
                rownames(kendall_cor_between_inv) <-
                  c(
                    "pred_factor_score_betw",
                    "true_factor_score_betw",
                    "sum_score_betw"
                  )
                rownames(kendall_cor_1_2_inv) <-
                  c("pred_factor_score_1_2",
                    "true_factor_score_1_2",
                    "sum_score_1_2")
                
                sub_outcome[[a]] <-
                  data.frame(
                    cor_type = c(
                      "predict_true_betw",
                      "true_sum_betw",
                      "predict_true_betw_inv",
                      "true_sum_betw_inv"
                    ),
                    estimate = c(
                      kendall_cor_between[2, 1],
                      kendall_cor_between[3, 2],
                      kendall_cor_between_inv[2, 1],
                      kendall_cor_between_inv[3, 2]
                    ),
                    runtime = rep(run_time, 4)
                  )
                run_time <- 0
                a <- a + 1
              }
            }
            if (stop == TRUE) {
              break
            }
          }
          if (stop == TRUE) {
            break
          }
        }
        if (stop == TRUE) {
          break
        }
      }
      if (stop == TRUE) {
        sub_outcome <- NULL
        next
      }
      all_outcome[[n]] <- do.call(rbind, sub_outcome)
      sub_outcome <- NULL
      if (n %% 10 == 0) {
        print(paste0("Loop:", n))
      }
      n <- n + 1
    }
    cor_outcome <- do.call(rbind, all_outcome)
    indicator_n <-
      rep(c(rep(5, 96), rep(10, 96), rep(15, 96)), rep)
    factor_loading <-
      rep(rep(c(
        rep("small", 32), rep("medium", 32), rep("large", 32)
      ), 3), rep)
    non_effect <-
      rep(rep(c(
        rep("none", 8),
        rep("small", 8),
        rep("medium", 8),
        rep("large", 8)
      ), 9), rep)
    non_proportion <-
      rep(c(rep(c(
        rep("0.2", 4), rep("0.4", 4)
      ), 36)), rep)
    outcome_all <-
      data.frame(indicator_n,
                 factor_loading,
                 non_effect,
                 non_proportion,
                 cor_outcome) %>%
      rename(
        Indicator = indicator_n,
        Loading = factor_loading,
        Effect = non_effect,
        Proportion = non_proportion,
        Type = cor_type
      )
    outcome_all$Indicator <-
      factor(
        outcome_all$Indicator,
        levels = c("5", "10", "15"),
        labels = c("5", "10", "15")
      )
    outcome_all$Loading <-
      factor(
        outcome_all$Loading,
        levels = c("small", "medium", "large"),
        labels = c("small", "medium", "large")
      )
    outcome_all$Effect <-
      factor(
        outcome_all$Effect,
        levels = c("none", "small", "medium", "large"),
        labels = c("none", "small", "medium", "large")
      )
    outcome_all$Proportion <-
      factor(
        outcome_all$Proportion,
        levels = c("0.2", "0.4"),
        labels = c("0.2", "0.4")
      )
    outcome_all$Type <-
      factor(
        outcome_all$Type,
        levels = c(
          "predict_true_betw",
          "true_sum_betw",
          "predict_true_betw_inv",
          "true_sum_betw_inv"
        ),
        labels = c(
          "predict true between",
          "true sum between",
          "predict true between invariance",
          "true sum between invariance"
        )
      )
    outcome_summary <-
      outcome_all[, -ncol(outcome_all)] %>%
      lazy_dt() %>%
      group_by(Indicator,
               Loading,
               Effect,
               Proportion,
               Type) %>%
      summarise(
        mean_est = mean(estimate),
        lower = (mean(estimate) - 1.96 * sd(estimate)),
        upper = (mean(estimate) + 1.96 * sd(estimate))
      ) %>% as.data.frame()
    toc(log = TRUE, quiet = TRUE)
    log.txt <- tic.log(format = TRUE)
    list(outcome_summary, outcome_all, log.txt)
  }


# all outcome -------------------------------------------------------------


outcome_list <- sim(
  seed = seed,
  rep = rep,
  obs_n = obs_n,
  eta_n = eta_n,
  ind_n = ind_n,
  lambda_value = lambda_value,
  nu_value = nu_value,
  epsilon_mean = epsilon_mean,
  eta_mean = eta_mean,
  eta_sd = eta_sd
)


# plot --------------------------------------------------------------------


outcome_proportion_1 <- outcome_list[[1]] %>%
  filter(Proportion == 0.2)
outcome_proportion_2 <- outcome_list[[1]] %>%
  filter(Proportion == 0.4)

labels <-
  unname(TeX(
    c(
      "$\\tau_{\\hat{\\eta}\\eta}$ (All items)",
      "$\\tau_{x \\eta}$ (All items)",
      "$\\tau_{\\hat{\\eta}\\eta}$ (Fair items)",
      "$\\tau_{x \\eta}$ (Fair items)"
    )
  ))
p1 <- ggplot(outcome_proportion_1,
             aes(Effect, mean_est,
                 group = Type)) +
  coord_cartesian(ylim = c(0.3, 0.9)) +
  geom_point(
    aes(shape = Type, colour = Type),
    position = position_dodge(width = .7),
    size = 1,
    show.legend = c(colour = TRUE)
  ) +
  scale_shape_manual(values = c(17, 15, 4, 3), labels = labels) +
  scale_colour_manual(values = c("#F8766D", "#619CFF", "#00BA38", "#E69F00"),
                      labels = labels) +
  scale_x_discrete(labels = c("None", "Small", "Medium", "Large")) +
  geom_linerange(
    aes(
      colour = Type,
      ymin = lower,
      ymax = upper
    ),
    position = position_dodge(width = .7),
    size = .5,
    show.legend = c(colour = FALSE)
  ) +
  labs(
    x = "Effect",
    y = "Parameter Estimate",
    title = TeX(r"(Kendall's $\tau$ between groups)", bold = TRUE),
    subtitle = "Proportion = 0.2"
  ) +
  scale_y_continuous(breaks = c(0.4, 0.6, 0.8)) +
  facet_grid(
    Loading ~ Indicator,
    labeller = labeller(
      .cols = label_both,
      Loading = as_labeller(c(
        `small` = "Small",
        `medium` = "Medium",
        `large` = "Large"
      ),default = label_both)
    )
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 35),
    plot.subtitle = element_text(size = 15),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    strip.background = element_rect(fill = "white", color = "white", size = 1),
    panel.grid.major = element_line(color = "gray"),
    panel.grid.minor = element_line(color = "gray"),
    strip.text.x = element_text(size = 10),
    strip.text.y = element_text(angle = -90, size = 10),
    legend.position = "bottom",
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 11)
  )

p2 <- ggplot(outcome_proportion_2,
             aes(Effect, mean_est,
                 group = Type)) +
  coord_cartesian(ylim = c(0.3, 0.9)) +
  geom_point(
    aes(shape = Type, colour = Type),
    position = position_dodge(width = .7),
    size = 1,
    show.legend = c(colour = TRUE)
  ) +
  scale_shape_manual(values = c(17, 15, 4, 3), labels = labels) +
  scale_colour_manual(values = c("#F8766D", "#619CFF", "#00BA38", "#E69F00"),
                      labels = labels) +
  scale_x_discrete(labels = c("None", "Small", "Medium", "Large")) +
  geom_linerange(
    aes(
      colour = Type,
      ymin = lower,
      ymax = upper
    ),
    position = position_dodge(width = .7),
    size = .5,
    show.legend = c(colour = FALSE)
  ) +
  labs(x = "Effect", y = "Parameter Estimate",  subtitle = "Proportion = 0.4") +
  scale_y_continuous(breaks = c(0.4, 0.6, 0.8)) +
  facet_grid(
    Loading ~ Indicator,
    labeller = labeller(
      .cols = label_both,
      Loading = as_labeller(c(
        `small` = "Small",
        `medium` = "Medium",
        `large` = "Large"
      ),default = label_both)
    )
  ) +
  theme_bw() +
  theme(
    plot.subtitle = element_text(size = 15),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    strip.background = element_rect(fill = "white", color = "white", size = 1),
    panel.grid.major = element_line(color = "gray"),
    panel.grid.minor = element_line(color = "gray"),
    strip.text.x = element_text(size = 10),
    strip.text.y = element_text(angle = -90, size = 10),
    legend.position = "bottom",
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 11)
  )


final_plot <- ggarrange(
  p1 + rremove("xlab") + rremove("ylab"),
  p2 +  rremove("ylab"),
  nrow = 2,
  common.legend = TRUE,
  legend = "bottom"
) %>%
  annotate_figure(left = text_grob("Parameter Estimate",
                                   size = 15,
                                   rot = 90))

ggsave(
  final_plot,
  filename = paste0("simulation_outcome_plot_", rep, ".pdf"),
  path = "../outcome/plot",
  width = 21,
  height = 27,
  units = "cm",
  device = "pdf",
  dpi = 400
)

save.image(file=paste0("../outcome/env/env_rep",rep,"_obsn",obs_n,".RData"))
