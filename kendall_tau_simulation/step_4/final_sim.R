# rep_list is stored with data and outcome_list
# includes only detection of converge.
# remain the condition which non_effect is equal to 0 and
# non_proportion is equal to 0.5
# reorganize plots

library(lavaan)
library(dplyr)
library(ggplot2)
library(pcaPP)
library(progress)
# library(testit) # detect error and warning # not needed in this and after simulation step

# parameters setting
seed <- 123
rep <- 30
obs_n <- 400
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

# simulation fuction
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
    pb <- progress_bar$new(
      format = "  Processing [:bar] :percent in :elapsedfull  loop: :current",
      complete = "=",
      incomplete = "-",
      current = ">",
      total = rep,
      clear = FALSE,
      width = 60
    )
    set.seed(seed)
    a <- 1
    n <- 1
    run_time <- 0
    cor_outcome <- data.frame()
    while (n <= rep) {
      pb$tick()
      stop <-  FALSE
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
              for (k in 1:obs_n) {
                eta_value <-
                  rnorm(eta_n, mean = eta_mean, sd = eta_sd)
                eta <- matrix(
                  eta_value,
                  nrow = eta_n,
                  ncol = 1,
                  byrow = TRUE
                )
                for (m in 1:ind_n[i]) {
                  epsilon[m] <-
                    rnorm(1, mean = epsilon_mean, sd = epsilon_sd[m, 1])
                }
                y <- nu1 + lambda %*% eta + epsilon
                dta1[k,] <- c(y, eta, 1)
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
              for (k in 1:obs_n) {
                eta_value <-
                  rnorm(eta_n, mean = eta_mean, sd = eta_sd)
                eta <- matrix(
                  eta_value,
                  nrow = eta_n,
                  ncol = 1,
                  byrow = TRUE
                )
                for (m in 1:ind_n[i]) {
                  epsilon[m] <-
                    rnorm(1, mean = epsilon_mean, sd = epsilon_sd[m, 1])
                }
                y <- nu2 + lambda %*% eta + epsilon
                dta2[k,] <- c(y, eta, 2)
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
                
                cor_all <-
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
                if (a == 1) {
                  cor_outcome <- rbind(NULL, cor_all)
                  a <- a + 1
                } else{
                  cor_outcome <- rbind(cor_outcome, cor_all)
                }
              }
            }
          }
        }
        if (stop == TRUE)
          break
      }
      if (stop == TRUE) {
        next
      }
      n <- n + 1
    }
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
        Indicator_N = indicator_n,
        Factor_Loading = factor_loading,
        Noninvariance_Effect = non_effect,
        Noninvariance_Proportion = non_proportion,
        Correlation_Type = cor_type
      )
    outcome_all$Indicator_N <-
      factor(
        outcome_all$Indicator_N,
        levels = c("5", "10", "15"),
        labels = c("5", "10", "15")
      )
    outcome_all$Factor_Loading <-
      factor(
        outcome_all$Factor_Loading,
        levels = c("small", "medium", "large"),
        labels = c("small", "medium", "large")
      )
    outcome_all$Noninvariance_Effect <-
      factor(
        outcome_all$Noninvariance_Effect,
        levels = c("none", "small", "medium", "large"),
        labels = c("none", "small", "medium", "large")
      )
    outcome_all$Noninvariance_Proportion <-
      factor(
        outcome_all$Noninvariance_Proportion,
        levels = c("0.2", "0.4"),
        labels = c("0.2", "0.4")
      )
    outcome_all$Correlation_Type <-
      factor(
        outcome_all$Correlation_Type,
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
      group_by(Indicator_N,
               Factor_Loading,
               Noninvariance_Effect,
               Noninvariance_Proportion,
               Correlation_Type) %>%
      summarise(
        mean_est = mean(estimate),
        lower = min(estimate),
        upper = max(estimate)
      ) 
    list(outcome_summary, outcome_all)
  }

# all outcome
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

# plot

p1 <- ggplot(outcome_list[[1]],
             aes(Indicator_N, mean_est,
                 group = Correlation_Type)) +
  geom_hline(
    yintercept = 0.5,
    linetype = 1,
    colour = "grey80",
    size = 0.3
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  geom_point(
    aes(shape = Correlation_Type, colour = Correlation_Type),
    position = position_dodge(width = .7),
    size = 1,
    show.legend = c(colour = TRUE)
  ) +
  scale_shape_manual(values = c(17, 15, 4, 3)) +
  scale_colour_manual(values = c("#F8766D", "#619CFF", "#00BA38", "#E69F00")) +
  scale_x_discrete(labels = c("5", "10", "15")) +
  geom_linerange(
    aes(
      colour = Correlation_Type,
      ymin = lower,
      ymax = upper
    ),
    position = position_dodge(width = .7),
    size = .5,
    show.legend = c(colour = FALSE)
  ) +
  labs(x = "Indicator Number", y = "Parameter Estimate", title = "Kendall Tau between groups") +
  scale_y_continuous(breaks = c(0.2, 0.4, 0.6, 0.8)) +
  facet_grid(
    Factor_Loading + Noninvariance_Proportion ~ Noninvariance_Effect,
    labeller = labeller(.rows = label_both, .cols = label_both)
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20),
    strip.background = element_rect(fill = "white", color = "white", size = 1),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_line(color = "white"),
    strip.text.x = element_text(size = 10),
    strip.text.y = element_text(angle = -90, size = 7),
    legend.position = "bottom"
  )

p1
