# rep_list is stored with data and outcome_list
# includes only detection of converge.
# remain the condition which non_effect is equal to 0 and
# non_proportion is equal to 0.5
# reorganize plots

library(lavaan)
library(dplyr)
library(ggplot2)
library(plotly)
# library(testit) # detect error and warning # not needed in this and after simulation step

# parameters setting
seed <- 123
rep <- 10
obs_n <- 1000
eta_n <- 1
ind_n <- c(5, 10, 15)
lambda_value <- list(c(0.7, 0.5, 0.6, 0.6, 0.6),
                     c(0.6, 0.4, 0.5, 0.5, 0.5),
                     c(0.5, 0.3, 0.4, 0.4, 0.4))
lambda_label <- c("large", "medium", "small")
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
    set.seed(seed)
    a <- 1
    n <- 1
    cor_outcome <- data.frame()
    while (n <= rep) {
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
              if (i == 1) {
                if (p == 1) {
                  model <-
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
                } else if (q == 1) {
                  model <-
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
                } else{
                  model <-
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
                }
              } else if (i == 2) {
                if (p == 1) {
                  model <-
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
                } else if (q == 1) {
                  model <-
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
                } else{
                  model <-
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
                }
              } else{
                if (p == 1) {
                  model <-
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
                } else if (q == 1) {
                  model <-
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
                } else{
                  model <-
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
                }
              }
              
              fit <-
                cfa(data = dta_all[, c(1:ind_n[i], ind_n[i] + 2)],
                    model = model,
                    group = "group")
              if (lavInspect(fit, "converged") == FALSE) {
                stop <-  TRUE
                break
              } else if (lavInspect(fit, "post.check") == FALSE) {
                stop <-  TRUE
                break
              } else{
                pred_factor_score <- c(predict(fit)$`1`, predict(fit)$`2`)
                true_factor_score <- dta_all[, ind_n[i] + 1]
                sum_score <- apply(dta_all[, 1:ind_n[i]], 1, sum)
                outcome <-
                  data.frame(pred_factor_score,
                             true_factor_score,
                             sum_score)
                pearson_cor <- cor(outcome)
                kendall_cor <- cor(outcome, method = "kendall")
                cor_all <-
                  data.frame(
                    pearson_predict_true = pearson_cor[2, 1],
                    kendall_predict_true =  kendall_cor[2, 1],
                    pearson_predict_sum = pearson_cor[3, 1],
                    kendall_predict_sum = kendall_cor[3, 1],
                    pearson_true_sum = pearson_cor[3, 2],
                    kendall_true_sum = kendall_cor[3, 2]
                  )
                rownames(cor_all) <-
                  paste0(
                    "ind_",
                    ind_n[i],
                    "_loading_",
                    lambda_label[j],
                    "_effect_",
                    non_inv_label[p],
                    "_propotion_",
                    pror_non_inv_label[q]
                  )
                
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
      rep(c(rep(5, 24), rep(10, 24), rep(15, 24)), rep)
    factor_loading <-
      rep(rep(c(
        rep("large", 8), rep("medium", 8), rep("small", 8)
      ), 3), rep)
    non_effect <-
      rep(rep(c(
        rep("none", 2),
        rep("small", 2),
        rep("medium", 2),
        rep("large", 2)
      ), 9), rep)
    non_propotion <- rep(c(rep(c("0.2", "0.4"), 36)), rep)
    outcome_summary <-
      data.frame(indicator_n,
                 factor_loading,
                 non_effect,
                 non_propotion,
                 cor_outcome)
    rownames(outcome_summary) <- 1:dim(outcome_summary)[1]
    outcome_summary$non_effect <-
      factor(
        outcome_summary$non_effect,
        levels = c("none", "small", "medium", "large"),
        labels = c("none", "small", "medium", "large")
      )
    outcome_summary$non_propotion <-
      factor(
        outcome_summary$non_propotion,
        levels = c("0.2", "0.4"),
        labels = c("0.2", "0.4")
      )
    outcome_summary %>% group_by(indicator_n,
                                 factor_loading,
                                 non_effect,
                                 non_propotion) %>%
      summarise(
        pearson_predict_true = mean(pearson_predict_true),
        kendall_predict_true = mean(kendall_predict_true),
        pearson_predict_sum = mean(pearson_predict_sum),
        kendall_predict_sum = mean(kendall_predict_sum),
        pearson_true_sum = mean(pearson_true_sum),
        kendall_true_sum = mean(kendall_true_sum)
      )
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
p1 <-
  ggplot(outcome_list,
         aes(x = indicator_n,
             y = pearson_predict_true,
             color = non_effect)) +
  geom_line() + geom_point() + facet_grid(factor_loading ~ non_propotion) +
  scale_x_continuous(n.breaks = 3, name = "indicator number") +
  scale_y_continuous(limits = c(0.5, 1), name = "correlation coefficient") +
  labs(title = "Pearson correlation between true factor score and predicted factor score")
p1 %>% ggplotly()

p2 <-
  ggplot(outcome_list,
         aes(x = indicator_n,
             y = kendall_predict_true,
             color = non_effect)) +
  geom_line() + geom_point() + facet_grid(factor_loading ~ non_propotion) +
  scale_x_continuous(n.breaks = 3, name = "indicator number") +
  scale_y_continuous(limits = c(0.4, 1), name = "correlation coefficient") +
  labs(title = "kendall correlation between true factor score and predicted factor score")
p2 %>% ggplotly()

p3 <-
  ggplot(outcome_list,
         aes(x = indicator_n,
             y = pearson_predict_sum,
             color = non_effect)) +
  geom_line() + geom_point() + facet_grid(factor_loading ~ non_propotion) +
  scale_x_continuous(n.breaks = 3, name = "indicator number") +
  scale_y_continuous(limits = c(0.5, 1), name = "correlation coefficient") +
  labs(title = "Pearson correlation between predicted factor score and sum score")
p3 %>% ggplotly()

p4 <-
  ggplot(outcome_list,
         aes(x = indicator_n,
             y = kendall_predict_sum,
             color = non_effect)) +
  geom_line() + geom_point() + facet_grid(factor_loading ~ non_propotion) +
  scale_x_continuous(n.breaks = 3, name = "indicator number") +
  scale_y_continuous(limits = c(0.5, 1), name = "correlation coefficient") +
  labs(title = "kendall correlation between predicted factor score and sum score")
p4 %>% ggplotly()

p5 <-
  ggplot(outcome_list,
         aes(x = indicator_n,
             y = pearson_true_sum,
             color = non_effect)) +
  geom_line() + geom_point() + facet_grid(factor_loading ~ non_propotion) +
  scale_x_continuous(n.breaks = 3, name = "indicator number") +
  scale_y_continuous(limits = c(0.5, 1), name = "correlation coefficient") +
  labs(title = "pearson correlation between true factor score and sum score")
p5 %>% ggplotly()

p6 <-
  ggplot(outcome_list,
         aes(x = indicator_n,
             y = kendall_true_sum,
             color = non_effect)) +
  geom_line() + geom_point() + facet_grid(factor_loading ~ non_propotion) +
  scale_x_continuous(n.breaks = 3, name = "indicator number") +
  scale_y_continuous(limits = c(0.4, 1), name = "correlation coefficient") +
  labs(title = "kendall correlation between true factor score and sum score")
p6 %>% ggplotly()
