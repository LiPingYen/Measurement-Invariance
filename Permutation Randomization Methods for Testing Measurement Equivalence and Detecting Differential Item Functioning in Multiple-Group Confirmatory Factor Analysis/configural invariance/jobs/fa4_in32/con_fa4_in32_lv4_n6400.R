#configural invariance
#indicator=32, factor= 4

library(semTools)
library(lavaan)
library(mvtnorm)
library(dplyr)
library(parallel)

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
    }, mc.cores = 5)
  }


# generate traditional AFIs ------------------------------------------------

trad_afi <- function(data, model , null_model , AFIs) {
  mclapply(data, function(x) {
    base_fit <- lavaan(
      data = x,
      model = null_model,
      group = "group",
      std.lv = TRUE
    )
    fit <- cfa(
      data = x,
      model = model,
      group = "group",
      std.lv = TRUE
    )
    fitmeasures(fit, fit.measures = AFIs, baseline.model = base_fit)
  }, mc.cores = 5)
}


# generate permuted AFI ---------------------------------------------------

per_afi <-
  function(data,
           seed,
           uncon_md,
           con_md,
           npermu,
           AFIs,
           moreAFIs) {
    mclapply(data, function(x) {
      fit_null <-
        lavaan(
          model = uncon_md,
          data = x,
          group = "group",
          std.lv = TRUE
        )
      fit_config <-
        cfa(
          model = con_md,
          data = x,
          std.lv = TRUE,
          group = "group"
        )
      set.seed(seed, "L'Ecuyer-CMRG")
      permuteMeasEq(
        nPermute = npermu,
        con = fit_config,
        AFIs = AFIs,
        moreAFIs = moreAFIs,
        null = fit_null,
        parallelType = "multicore",
        ncpus = 1,
        showProgress = TRUE
      )
    }, mc.cores = 5)
  }


#omnibus reject H0 rate (permutation) -----------------------------------------------------

prr <- function(data, pvalue) {
  chi <- mean(sapply(data, function(x) {
    ifelse(x@AFI.pval["chisq"] >= pvalue, 0, 1)
  }))
  cfi <- mean(sapply(data, function(x) {
    ifelse(x@AFI.pval["cfi"] >= pvalue, 0, 1)
  }))
  mfi <- mean(sapply(data, function(x) {
    ifelse(x@AFI.pval["mfi"] >= pvalue, 0, 1)
  }))
  rmsea <- mean(sapply(data, function(x) {
    ifelse(x@AFI.pval["rmsea"] >= pvalue, 0, 1)
  }))
  srmr <- mean(sapply(data, function(x) {
    ifelse(x@AFI.pval["srmr"] >= pvalue, 0, 1)
  }))
  data.frame(chi, cfi, mfi, rmsea, srmr, row.names = "reject_rate")
}


# omnibus reject H0 rate (traditional AFIs) -------------------------------

trr <- function(data) {
  chi <- mean(sapply(data, function(x) {
    ifelse(x[1] >= 0.05, 0, 1)
  }))
  cfi_95 <- mean(sapply(data, function(x) {
    ifelse(x[2] >= 0.95, 0, 1)
  }))
  cfi_90 <- mean(sapply(data, function(x) {
    ifelse(x[2] >= 0.9, 0, 1)
  }))
  mfi <- mean(sapply(data, function(x) {
    ifelse(x[3] >= 0.9, 0, 1)
  }))
  rmsea_05 <- mean(sapply(data, function(x) {
    ifelse(x[4] >= 0.05, 1, 0)
  }))
  rmsea_08 <- mean(sapply(data, function(x) {
    ifelse(x[4] >= 0.08, 1, 0)
  }))
  srmr <- mean(sapply(data, function(x) {
    ifelse(x[5] >= 0.08, 1, 0)
  }))
  data.frame(chi, cfi_95, cfi_90, mfi, rmsea_05, rmsea_08, srmr, row.names = "reject_rate")
}


#parameter setting
reps = 2000
nobs = 6400
pvalue = 0.05
n_factor = 4
n_indicator = 32
seed <- sample(1:100000, 1)
npermu <- 200
myAFIs_tra <- c("pvalue", "cfi", "mfi", "rmsea", "srmr")
myAFIs_per <- c("chisq", "cfi", "mfi", "rmsea", "srmr")
moreAFIs_per <- NULL # c("gammaHat","gammaHat.scaled")

#model
#null model
null_md <-
  c(
    paste0("X", 1:32, " ~~ c(psi", 1:32, ",psi", 1:32, ")*X", 1:32),
    paste0("X", 1:32, " ~ c(tau", 1:32, ", tau", 1:32, ")*1")
  )

#configural invariance model
md_conf <- '
fac1=~X1+X2+X3+X4+X17+X18+X19+X20
fac2=~X5+X6+X7+X8+X21+X22+X23+X24
fac3=~X9+X10+X11+X12+X25+X26+X27+X28
fac4=~X13+X14+X15+X16+X29+X30+X31+X32
'

#group1
lambda1 <- matrix(c(0.54,-0.03,-0.02,-0.11,
                    0.62,0.02,-0.03,-0.03,
                    0.6,-0.04,-0.03,0,
                    0.61,0,-0.01,0.08,
                    0.04,0.61,0.07,0,
                    -0.06,0.41,-0.03,0.04,
                    -0.08,0.6,0.07,0.06,
                    -0.02,0.57,0.05,-0.03,
                    0.07,-0.01,0.65,0,
                    -0.01,-0.03,0.43,-0.02,
                    -0.04,0.06,0.65,0.03,
                    0.04,0,0.24,0.05,
                    -0.02,-0.02,-0.01,0.51,
                    0,-0.13,-0.03,0.53,
                    0,0.03,-0.01,0.45,
                    0,0.03,0.02,0.53), nrow = 16, ncol = n_factor, byrow = TRUE)
lambda1 <- rbind(lambda1, lambda1)

phi1 <- matrix(
  c(1, 0.3, 0.3, 0.3,
    0.3, 1, 0.3, 0.3,
    0.3, 0.3, 1, 0.3,
    0.3, 0.3, 0.3, 1),
  nrow = n_factor,
  ncol = n_factor,
  byrow = TRUE
)
theta1 <- diag(rep(1, n_indicator))
tau1 <- matrix(rep(0, n_indicator), nrow = n_indicator)
fac_mean1 = matrix(rep(0, n_factor), nrow = n_factor)

#group2
#level4
lambda2 <- matrix(c(0.54,0.7,-0.02,-0.11,
                    0.62,0.02,-0.03,-0.03,
                    0.6,-0.04,-0.03,0,
                    0.61,0,-0.01,0.08,
                    0.7,0.61,0.07,0,
                    -0.06,0.41,-0.03,0.04,
                    -0.08,0.6,0.07,0.06,
                    -0.02,0.57,0.05,-0.03,
                    0.07,-0.01,0.65,0,
                    -0.01,-0.03,0.43,-0.02,
                    -0.04,0.06,0.65,0.03,
                    0.04,0,0.24,0.05,
                    -0.02,-0.02,-0.01,0.51,
                    0,-0.13,-0.03,0.53,
                    0,0.03,-0.01,0.45,
                    0,0.03,0.02,0.53), nrow = 16, ncol=n_factor, byrow = TRUE)
lambda2 <- rbind(lambda2, lambda2)

phi2 <- matrix(
  c(1, 0.3, 0.3, 0.3,
    0.3, 1, 0.3, 0.3,
    0.3, 0.3, 1, 0.3,
    0.3, 0.3, 0.3, 1),
  nrow = n_factor,
  ncol = n_factor,
  byrow = TRUE
)
theta2 <- diag(rep(1, n_indicator))
theta2[7, 2] <- 0.2
theta2[2, 7] <- 0.2
theta2[8, 4] <- 0.2
theta2[4, 8] <- 0.2
tau2 <- matrix(rep(0, n_indicator), nrow = n_indicator)
fac_mean2 = matrix(rep(0, n_factor), nrow = n_factor)

#generate population
set.seed(seed)
dta <- gen_dta(
  reps = reps,
  nobs = nobs,
  la1 = lambda1,
  la2 = lambda2,
  phi1 = phi1,
  phi2 = phi2,
  th1 = theta1,
  th2 = theta2,
  tau1 = tau1,
  tau2 = tau2,
  fac_mean1 = fac_mean1,
  fac_mean2 = fac_mean2
)

# testing configural measurement invariance (traditional AFIs) ------------

tra_afi_list <-
  trad_afi(
    data = dta,
    model = md_conf,
    null_model = null_md ,
    AFIs = myAFIs_tra
  )

# testing configural measurement invariance (permutation)-------------------------------

per_afi_list <- per_afi(
  data = dta,
  seed = seed,
  uncon_md = null_md,
  con_md = md_conf,
  npermu = npermu,
  AFIs = myAFIs_per,
  moreAFIs = moreAFIs_per
)

# omnibus reject H0 rate (traditional AFIs) -------------------------------

tra_rej_rate <- trr(data = tra_afi_list)

# omnibus reject H0 rate (permutation) -----------------------------------

per_rej_rate <- prr(data = per_afi_list, pvalue = pvalue)

# remove the population data and permuated AFI list -----------------------

rm(dta, per_afi_list)