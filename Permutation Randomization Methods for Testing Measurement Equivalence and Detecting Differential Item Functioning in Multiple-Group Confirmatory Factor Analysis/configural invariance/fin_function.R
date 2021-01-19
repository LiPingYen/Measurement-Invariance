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
    }, mc.cores = 12)
  }



# generate traditional AFIs ------------------------------------------------

trad_afi<-function(data,model,AFIs){
  mclapply(data,function(x){
    fit<-cfa(
      data = x,
      model = model,
      group = "group",
      std.lv = TRUE
    )
    fitmeasures(fit,fit.measures = AFIs)
  }, mc.cores = 12)
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
        AFIs = myAFIs,
        moreAFIs = moreAFIs,
        null = fit_null,
        parallelType = "multicore",
        ncpus = 1,
        showProgress = TRUE
      )
    }, mc.cores = 12)
  }


#omnibus reject H0 rate (permutation) -----------------------------------------------------

per_rej_rate<-function(data,pvalue){
  chi<-mean(sapply(data,function(x){
    ifelse(x@AFI.pval["chisq"]>=pvalue,0,1)
  }))
  cfi<-mean(sapply(data,function(x){
    ifelse(x@AFI.pval["cfi"]>=pvalue,0,1)
  }))
  mfi<-mean(sapply(data,function(x){
    ifelse(x@AFI.pval["mfi"]>=pvalue,0,1)
  }))
  rmsea<-mean(sapply(data,function(x){
    ifelse(x@AFI.pval["rmsea"]>=pvalue,0,1)
  }))
  srmr<-mean(sapply(data,function(x){
    ifelse(x@AFI.pval["srmr"]>=pvalue,0,1)
  }))
  data.frame(chi,cfi,mfi,rmsea,srmr, row.names = "reject_rate")
}


# omnibus reject H0 rate (traditional AFIs) -------------------------------

tra_rej_rate<-function(data){
  chi<-mean(sapply(data,function(x){
    ifelse(x[1]>=0.05,0,1)
  }))
  cfi<-mean(sapply(data,function(x){
    ifelse(x[2]>=0.95,0,1)
  }))
  mfi<-mean(sapply(data,function(x){
    ifelse(x[3]>=0.9,0,1)
  }))
  rmsea<-mean(sapply(data,function(x){
    ifelse(x[4]>=0.06,1,0)
  }))
  srmr<-mean(sapply(data,function(x){
    ifelse(x[5]>=0.08,1,0)
  }))
  data.frame(chi,cfi,mfi,rmsea,srmr, row.names = "reject_rate")
}