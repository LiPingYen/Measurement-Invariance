library(dplyr)
library(MASS)
library(mvtnorm)
library(lavaan)
library(semTools)
library(simsem)
library(stringr)
library(reshape2)
library(compare)
library(parallel)
library(future)


####產生population資料####
gen_dta_v6<-function(nobs,la1,phi1,la2,phi2,th2){
  pop_co_ma1<-la1%*%phi1%*%t(la1)+th2
  pop_co_ma2<-la2%*%phi2%*%t(la2)+th2
  dta_1<-data.frame(rmvnorm(nobs,rep(0,nrow(pop_co_ma1)),sigma = pop_co_ma1,method ="chol"))
  dta_2<-data.frame(rmvnorm(nobs,rep(0,nrow(pop_co_ma2)),sigma = pop_co_ma2,method ="chol"))
  dta<-rbind(dta_1,dta_2)%>%
    rename(v1=X1,v2=X2,v3=X3,v4=X4,v5=X5,v6=X6)%>%
    mutate(group=c(rep(1,nobs),rep(2,nobs)))
  dta
}

gen_dta_v12<-function(nobs,la1,phi1,la2,phi2,th2){
  pop_co_ma1<-la1%*%phi1%*%t(la1)+th2
  pop_co_ma2<-la2%*%phi2%*%t(la2)+th2
  dta_1<-data.frame(rmvnorm(nobs,rep(0,nrow(pop_co_ma1)),sigma = pop_co_ma1,method ="chol"))
  dta_2<-data.frame(rmvnorm(nobs,rep(0,nrow(pop_co_ma2)),sigma = pop_co_ma2,method ="chol"))
  dta<-rbind(dta_1,dta_2)%>%
    rename(v1=X1,v2=X2,v3=X3,v4=X4,v5=X5,v6=X6,v7=X7,v8=X8,v9=X9,v10=X10,v11=X11,v12=X12)%>%
    mutate(group=c(rep(1,nobs),rep(2,nobs)))
  dta
}

####使用apply系列運算####
#產生所有資料的rmsea
rmfun_pl<-function(md,dta){
  sapply(dta,function(x){
    fit<-cfa(model = md,data = x,group = "group",group.equal=c("loadings"))
  fitMeasures(fit,"rmsea")})
}

#產生所有資料的rmsea，使用平行分析
cpu.cores <- availableCores()
cl <- makeCluster(cpu.cores,type = "FORK")
rmfun_papl_test<-function(md,dta){
  parSapply(cl=cl,dta,function(x){
    fit<-cfa(model = md,data = x,group = "group",group.equal=c("loadings"))
    fitMeasures(fit,"rmsea")})
}
stopCluster(cl)
showConnections() 

#產出noninvariant的變項
det_non_pl<-function(md,dta){
  lapply(dta,function(x){
    fit<-cfa(model=md,data=x,group="group",group.equal=c("loadings"))
    lavp<-lavTestScore(fit,cumulative = TRUE)$cumulative[1,]
    fre_va<-vector()
    non_int_each<-vector()
    n<-1
    while(lavp[,6]<.05){
      non_int_each[n]<-lavp$lhs
      v<-str_extract_all(lavp$lhs, "(\\d)+")[[1]]
      fre_va[n]<-paste0("fac1=~v",v)
      fit_i<-cfa(model=md,data=x,group="group",group.equal=c("loadings"),group.partial=fre_va)
      lavp<-lavTestScore(fit_i,cumulative = TRUE)$cumulative[1,]
      n=n+1
    }
    non_int_each
  })
}

#產出noninvariant的變項，使用平行分析
non_var_papl<-function(md,dta){
  mclapply(dta,function(x){
    fit<-cfa(model=md,data=x,group="group",group.equal=c("loadings"))
    lavp<-lavTestScore(fit,cumulative = TRUE)$cumulative[1,]
    fre_va<-vector()
    non_int_each<-vector()
    n<-1
    while(lavp[,6]<.05){
      non_int_each[n]<-lavp$lhs
      v<-str_extract_all(lavp$lhs, "(\\d)+")[[1]]
      fre_va[n]<-paste0("fac1=~v",v)
      fit_i<-cfa(model=md,data=x,group="group",group.equal=c("loadings"),group.partial=fre_va)
      lavp<-lavTestScore(fit_i,cumulative = TRUE)$cumulative[1,]
      n=n+1
    }
    non_int_each
  })
}

#偵測模型中noninvariant variable 和資料中的有沒有相符,perfect recovery rate
det_non_pl<-function(non_var,non_con){
  sapply(non_var,function(x){
    non_inv<-vector()
    ifelse(compare(x,non_con,ignoreOrder = TRUE)$result,non_inv<-1,non_inv<-0)
  non_inv})
}  

##true detection
true_det_pl<-function(non_var,non_con){
  a<-mclapply(non_var,function(x){
    x%in%non_con
  })
  b<-mclapply(a,function(x){
    table(x)
  })
  names(b)<-NULL
  c<-melt(b)
  c$L1 <- factor(c$L1, seq_along(tab))
  d<-dcast(c, L1 ~ x, fill = 0, drop = FALSE)
  colnames(d)<-c("L1","F","T")
  count(d,F,T)%>%
    mutate(freq=n/reps)
}


#false detection
fal_det_pl<-function(non_var,inv_con){
  a<-mclapply(non_var,function(x){
    x%in%inv_con
  })
  b<-mclapply(a,function(x){
    table(x)
  })
  names(b)<-NULL
  c<-melt(b)
  c$L1 <- factor(c$L1, seq_along(tab))
  d<-dcast(c, L1 ~ x, fill = 0, drop = FALSE)
  colnames(d)<-c("L1","F","T")
  count(d,F,T)%>%
    mutate(freq=n/reps)
}



####使用迴圈####
#產生rmsea
rmfun<-function(md,dta){
  lav_fit<-cfa(model=md,data=dta,group="group",group.equal=c("loadings"))
  rmsea<-fitMeasures(lav_fit,"rmsea")
  rmsea
}

#產生所有資料的rmsea
get_rm<-function(md,dta,reps){
  output<-vector()
  for(i in 1:reps){
    rmsea<-rmfun(md,dta =dta[[i]])
    output[[i]]<-rmsea
  }
  return(output)
}


#產出noninvariant的變項
det_non<-function(md,dta,reps){
  non_inv_all<-vector("list",length = reps)
  for(i in 1:reps){
    fit<-cfa(model=md,data=dta[[i]],group="group",group.equal=c("loadings"))
    lavp<-lavTestScore(fit,cumulative = TRUE)$cumulative[1,]
    fre_va<-vector()
    non_int_each<-vector()
    n<-1
    while(lavp[,6]<.05){
      non_int_each[n]<-lavp$lhs
      v<-str_extract_all(lavp$lhs, "(\\d)+")[[1]]
      fre_va[n]<-paste0("fac1=~v",v)
      fit_i<-cfa(model=md,data=dta[[i]],group="group",group.equal=c("loadings"),group.partial=fre_va)
      lavp<-lavTestScore(fit_i,cumulative = TRUE)$cumulative[1,]
      n=n+1
    }
    non_inv_all[[i]]<-non_int_each
  }
  return(non_inv_all)
}



#偵測模型中noninvariant variable 和資料中的有沒有相符,perfect recovery rate
det_non_all<-function(reps,non_var,non_con){
  non_inv<-vector()
  for(i in 1:reps){
    ifelse(compare(non_var[[i]],non_con,ignoreOrder = TRUE)$result,non_inv[i]<-1,non_inv[i]<-0)
  }
  return(non_inv)
}

#true detection
true_det<-function(reps,non_var,non_con){
  true_de<-vector("list",length = reps)
  for(i in 1:reps){
    true_de[[i]]<-non_var[[i]]%in%non_con
  }
  tab<-vector("list",length = reps)
  for (j in 1:reps) {
    tab[[j]]<-table(true_de[[j]])
  }
  a<-melt(tab)
  a$L1 <- factor(a$L1, seq_along(tab))
  b<-dcast(a, L1 ~ Var1, fill = 0, drop = FALSE)
  colnames(b)<-c("L1","F","T")
  count(b,F,T)%>%
    mutate(freq=n/reps)
}

#false detection
fal_det<-function(reps,non_var,inv_con){
  fal_de<-vector("list",length = reps)
  for(i in 1:reps){
    fal_de[[i]]<-non_var[[i]]%in%inv_con
  }
  tab<-vector("list",length = reps)
  for (j in 1:reps) {
    tab[[j]]<-table(fal_de[[j]])
  }
  a<-melt(tab)
  a$L1 <- factor(a$L1, seq_along(tab))
  b<-dcast(a, L1 ~ Var1, fill = 0, drop = FALSE)
  colnames(b)<-c("L1","F","T")
  count(b,F,T)%>%
    mutate(freq=n/reps)
}

##註true_det和fal_det為相反，例：true_det:1F0T=fal_det:0F1Ts
