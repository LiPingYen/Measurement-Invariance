library(lavaan)
library(dplyr)
library(testit) # detect error and warning

# parameters setting
seed <- 123
rep <- 100
obs_n <- 200
eta_n <- 1
ind_n <- c(4, 8, 12)
lambda_value <- list(c(0.7, 0.5, 0.6, 0.6),
                     c(0.6, 0.4, 0.5, 0.5),
                     c(0.5, 0.3, 0.4, 0.4))
lambda_label <- c("large", "medium", "small")
nu_value <- 0
epsilon_mean <- 0
eta_mean <- 0
eta_sd <- 1

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
    b <- 1
    m <- 1
    rep_list <- vector(length = rep, mode = "list")
    warning_fit <- vector(mode = "list")
    warning_data <- vector(mode = "list")
    warning_model <- vector(mode = "list")
    no_converge <- vector(mode = "list")
    while (m <= rep) {
      cor_outcome <- data.frame()
      stop <-  FALSE
      for (i in 1:3) {
        for (j in 1:3) {
          nu <- matrix(
            rep(nu_value, ind_n[i]),
            nrow = ind_n[i],
            ncol = 1,
            byrow = TRUE
          )
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
          
          dta1 <- matrix(ncol = ind_n[i] + 2,
                         nrow = obs_n,
                         byrow = TRUE)
          for (k in 1:obs_n) {
            eta_value <-
              rnorm(eta_n, mean = eta_mean, sd = eta_sd)
            eta <- matrix(eta_value,
                          nrow = eta_n,
                          ncol = 1,
                          byrow = TRUE)
            for (n in 1:ind_n[i]) {
              epsilon[n] <-
                rnorm(1, mean = epsilon_mean, sd = epsilon_sd[n, 1])
            }
            y <- nu + lambda %*% eta + epsilon
            dta1[k, ] <- c(y, eta, 1)
          }
          dta2 <- matrix(ncol = ind_n[i] + 2,
                         nrow = obs_n,
                         byrow = TRUE)
          for (k in 1:obs_n) {
            eta_value <-
              rnorm(eta_n, mean = eta_mean, sd = eta_sd)
            eta <- matrix(eta_value,
                          nrow = eta_n,
                          ncol = 1,
                          byrow = TRUE)
            for (n in 1:ind_n[i]) {
              epsilon[n] <-
                rnorm(1, mean = epsilon_mean, sd = epsilon_sd[n, 1])
            }
            y <- nu + lambda %*% eta + epsilon
            dta2[k, ] <- c(y, eta, 2)
          }
          dta_all <- rbind(dta1, dta2)
          colnames(dta_all) <-
            c(paste0("x", 1:ind_n[i]), "true_factor_score", "group")
          if (i == 1) {
            model <-
              paste0("factor",
                     eta_n,
                     "=~",
                     lambda_value[[j]][1],
                     "*x1+x2+x3+x4")
          } else if (i == 2) {
            model <-
              paste0("factor",
                     eta_n,
                     "=~",
                     lambda_value[[j]][1],
                     "*x1+x2+x3+x4+x5+x6+x7+x8")
          } else{
            model <-
              paste0(
                "factor",
                eta_n,
                "=~",
                lambda_value[[j]][1],
                "*x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12"
              )
          }
          
          fit <-
            cfa(data = dta_all[, c(1:ind_n[i], ind_n[i] + 2)],
                model = model,
                group = "group")
          if (lavInspect(fit, "converged") == FALSE) {
            warning_fit[[b]] <- fit
            warning_data[[b]] <- dta_all
            warning_model[[b]] <- model
            no_converge[[b]] <- lavInspect(fit, "converged")
            b <- b + 1
            stop <-  TRUE
            break
          } else if (lavInspect(fit, "post.check") == FALSE) {
            warning_fit[[b]] <- fit
            warning_data[[b]] <- dta_all
            warning_model[[b]] <- model
            b <- b + 1
            stop <-  TRUE
            break
          } else if (has_warning(cfa(
            data = dta_all[, c(1:ind_n[i], ind_n[i] + 2)],
            model = model,
            group = "group"
          ))) {
            warning_fit[[b]] <- fit
            warning_data[[b]] <- dta_all
            warning_model[[b]] <- model
            b <- b + 1
            stop <-  TRUE
            break
          }
          else{
            pred_factor_score <- c(predict(fit)$`1`, predict(fit)$`2`)
            true_factor_score <- dta_all[, ind_n[i] + 1]
            sum_score <- apply(dta_all[, 1:ind_n[i]], 1, sum)
            outcome <-
              data.frame(pred_factor_score, true_factor_score, sum_score)
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
              paste0("ind_", ind_n[i], "_loading_", lambda_label[j])
            if (a == 1) {
              cor_outcome <- rbind(NULL, cor_all)
              a <- a + 1
            } else{
              cor_outcome <- rbind(cor_outcome, cor_all)
            }
          }
        }
        if (stop == TRUE)
          break
      }
      if (stop == TRUE) {
        next
      }
      rep_list[[m]] <- cor_outcome
      m <- m + 1
    }
    outcome_list <- list(rep_list,
                         warning_fit,
                         warning_data,
                         warning_model,
                         no_converge)
    outcome_list
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

sum_matrix <- matrix(ncol = 6, nrow = 9)

a <- c()
for (j in 1:9) {
  for (k in 1:6) {
    for (i in 1:rep) {
      a[i] <- outcome_list[[1]][[i]][j, k]
    }
    sum_matrix[j, k] <- mean(a)
  }
}
colnames(sum_matrix) <- names(outcome_list[[1]][[1]])
rownames(sum_matrix) <- row.names(outcome_list[[1]][[1]])

indicator_n = c(rep(4, 3), rep(8, 3), rep(12, 3))
factor_loading = rep(c("large", "medium", "small"), 3)
outcome_summary <- data.frame(indicator_n,factor_loading,sum_matrix)
rownames(outcome_summary) <- 1:9
outcome_summary

