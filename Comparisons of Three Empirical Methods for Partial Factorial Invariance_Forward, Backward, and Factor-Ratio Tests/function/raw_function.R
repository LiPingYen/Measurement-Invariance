library(lavaan)
library(dplyr)
library(parallel)
library(data.table)
library(compare)

####generate data from multivariate normal distribution####
gen_dta<-function(nobs,md1,md2){
  dta_1<-simulateData(model =md1,model.type = "cfa",return.type = "data.frame",sample.nobs =nobs)
  dta_2<-simulateData(model =md2,model.type = "cfa",return.type = "data.frame",sample.nobs =nobs)
  dta<-rbind(dta_1,dta_2)%>%
    mutate(group=c(rep(1,nobs),rep(2,nobs)))
  dta
}

####create lambda dataframe from result of cfa####
gen_lam<-function(data,model){
  mclapply(data,function(x){
    fit<-cfa(data=x,model=model,group="group")
    lam_g1<-parameterEstimates(fit)[1:6,7]
    lam_g2<-parameterEstimates(fit)[21:26,7]
    data.frame(lambda_g1=lam_g1,lambda_g2=lam_g2,v=c("v1","v2","v3","v4","v5","v6"))
  })
}

####check variable is non-invariant or not####
check_non<-function(data,con.int){
  v1_lam<-data%>%filter(.,v=="v1")
  v2_lam<-data%>%filter(.,v=="v2")
  v3_lam<-data%>%filter(.,v=="v3")
  v4_lam<-data%>%filter(.,v=="v4")
  v5_lam<-data%>%filter(.,v=="v5")
  v6_lam<-data%>%filter(.,v=="v6")
  ci1<-t.test(v1_lam$lambda_g1,v1_lam$lambda_g2,alternative = "two.sided",paired = TRUE,conf.level = con.int)$conf.int
  ci2<-t.test(v2_lam$lambda_g1,v2_lam$lambda_g2,alternative = "two.sided",paired = TRUE,conf.level = con.int)$conf.int
  ci3<-t.test(v3_lam$lambda_g1,v3_lam$lambda_g2,alternative = "two.sided",paired = TRUE,conf.level = con.int)$conf.int
  ci4<-t.test(v4_lam$lambda_g1,v4_lam$lambda_g2,alternative = "two.sided",paired = TRUE,conf.level = con.int)$conf.int
  ci5<-t.test(v5_lam$lambda_g1,v5_lam$lambda_g2,alternative = "two.sided",paired = TRUE,conf.level = con.int)$conf.int
  ci6<-t.test(v6_lam$lambda_g1,v6_lam$lambda_g2,alternative = "two.sided",paired = TRUE,conf.level = con.int)$conf.int
  dta_inv<-c(prod(ci1)>=0,prod(ci2)>=0,prod(ci3)>=0,prod(ci4)>=0,prod(ci5)>=0,prod(ci6)>=0)
  dta_inv
}

####perfect recovery rate:completely detects non-invariant variable####
#combine generating data and checking non-invariant variable together
detnon_list<-function(reps,nobs,md1,md2,testmd,con.int){
  dta_list<-replicate(n=reps,gen_dta(nobs=nobs,md1=md1,md2 =md2),simplify = FALSE)
  lam_dta_list<-gen_lam(data=dta_list,model = mdconf)
  lam_dta<-rbindlist(lam_dta_list)
  ch_non<-check_non(data = lam_dta,con.int = con.int)
  ch_non
}
#non_con: non-invariant variable enter TRUE, NA enter NA, invariant enter FALSE 
det_non<-function(det_list,non_con){
  sapply(det_list,function(x){
    non_inv<-vector()
    ifelse(compare(x,non_con)$result,non_inv<-1,non_inv<-0)
    non_inv})
}
