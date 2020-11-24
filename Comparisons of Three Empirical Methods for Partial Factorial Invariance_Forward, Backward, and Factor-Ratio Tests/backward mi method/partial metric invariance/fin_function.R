library(lavaan)
library(mvtnorm)
library(dplyr)
library(parallel)
library(data.table)
library(compare)
library(stringr)


# generate data from multivariate normal distribution ---------------------


#create all data once(final picked)(insanely fast)
gen_dta <-
  function(nobs,
           reps,
           la1,
           phi1,
           th1,
           tau1,
           fac_mean1,
           la2,
           phi2,
           th2,
           tau2,
           fac_mean2) {
    em_list <- vector(length = reps, mode = "list")
    mclapply(em_list, function(x) {
      pop_co_ma1 <- la1 %*% phi1 %*% t(la1) + th1
      pop_co_ma2 <- la2 %*% phi2 %*% t(la2) + th2
      pop_mean1 <- tau1 + la1 %*% fac_mean1
      pop_mean2 <- tau2 + la2 %*% fac_mean2
      dta1 <-
        data.frame(rmvnorm(
          nobs,
          mean = pop_mean1,
          sigma = pop_co_ma1,
          method = "chol"
        ))#rep(mean,)mean要帶公式,tau+lamda*latent mean
      dta2 <-
        data.frame(rmvnorm(
          nobs,
          mean = pop_mean2,
          sigma = pop_co_ma2,
          method = "chol"
        ))
      rbind(dta1, dta2) %>%
        mutate(group = c(rep(1, nobs), rep(2, nobs)))
    }, mc.cores = 1)
  }


# 產出noninvariant的變項，使用平行運算 ------------------------------------------------


det_non_v <- function(md, dta) {
  mclapply(dta, function(x) {
    fit <- cfa(
      model = md,
      data = x,
      group = "group",
      group.equal = c("loadings", "intercepts")
    )
    lavp <-
      lavTestScore(fit)$uni %>% subset(!lhs %in% c(".p15.", ".p16.", ".p17.", ".p18.", ".p19.", ".p20.")) %>% arrange(p.value) #移除tau恆等的那列
    lavp <- lavp[1,]
    fre_va <- vector()
    non_int_each <- vector()
    converge <- vector()
    n <- 1
    while (lavp[, 6] < p_value) {
      non_int_each[n] <- lavp$lhs
      X <- str_extract_all(lavp$lhs, "(\\d)+")[[1]]
      fre_va[n] <- paste0("fac1=~X", X)
      fit_i <-
        cfa(
          model = md,
          data = x,
          group = "group",
          group.equal = c("loadings", "intercepts"),
          group.partial = fre_va
        )
      converge[n] <- lavInspect(fit_i, what = "converged")
      lavp <-
        lavTestScore(fit_i)$uni %>% subset(!lhs %in% c(".p15.", ".p16.", ".p17.", ".p18.", ".p19.", ".p20.")) %>% arrange(p.value)
      lavp <- lavp[1,]
      n = n + 1
    }
    conv <- ifelse(all(converge == TRUE), 1, 0)
    list(non_int_each, converge, conv)
  }, mc.cores = 12)
}


#convergence rate


conv_rate <-
  function(non_v_li) {
    mean(sapply(lapply(non_v_li, function(x) {
      x[[3]]
    }), function(y) {
      y
    }))
  }


# perfect recovery rate:completely detects non-invariant variable ---------


#non_con: non-invariant variable will be showed on list
det_non <- function(det_list, non_con) {
  sapply(det_list, function(x) {
    ifelse(compare(x, non_con, ignoreOrder = TRUE)$result, 1, 0)
  })
}


# model-level Type I error (only for baseline model) ----------------------


det_tyi <- function(det_list) {
  sapply(det_list, function(x) {
    ifelse(any(x %in% c(".p2.", ".p3.", ".p4.", ".p5.", ".p6.")), 1, 0)
  })
}


# model-level Type I error ------------------------------------------------


det_tyi <- function(det_list) {
  sapply(det_list, function(x) {
    ifelse(any(x %in% c(".p3.", ".p5.", ".p6.")), 1, 0)
  })
}


# model-level Type II error -----------------------------------------------


det_tyii <- function(det_list) {
  sapply(det_list, function(x) {
    ifelse(any(x %in% ".p2."), ifelse(any(x %in% ".p4."), 0, 1), 1)
  })
}