---
title: "forward linear"
author: "Yudong Yang"
date: "2018/11/14"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## R forward linear

```{r}
library(leaps)
library(readxl)
my_data <- list()
mse_lf<-list()
for (j in 1:8) {
  my_data[[j]] <- read_excel(paste("Store",j+1,"-1.xlsx",sep = ""))
  #delet null value

  Store = na.omit(my_data[[j]])
  Store[c(99:122)] = log(Store[c(99:122)])
  for (i in 1:24){
    Store[i*4+2] = log(Store[i*4+2])
  }
  
 
  #SELECT TRAIN DATA & TEST DATA
  xtrain=(Store$Random=='Train')
  xtest=(Store$Random=='Test')
  Store.test = Store[!xtrain,]

  attach(Store)
  depenvar=c(colnames(Store)[99:122])
  
  mse_f = rep(NA,24)
  for(i in 1:24){
    f=as.formula(paste(depenvar[i],"~",paste(colnames(Store)[3:98],collapse="+")))
    regfit.fwd = regsubsets(f,Store,nvmax=40, method="forward",subset=xtrain)
    reg.summaryf=summary(regfit.fwd)
    num_f=which.min(reg.summaryf$bic)
    coef(regfit.fwd,num_f)
    names(coef(regfit.fwd,num_f))[2:length(coef(regfit.fwd,num_f))]
    f_f=as.formula(paste(depenvar[i],"~",paste(names(coef(regfit.fwd,num_f))[2:length(coef(regfit.fwd,num_f))], collapse='+' )))
    forward.fit=lm(f_f, Store,subset=xtrain)
    forward.pred=predict(forward.fit,Store.test)
    mse_f[i]=mean(((exp(forward.pred)-exp(get(depenvar[i])[(nrow(Store)-nrow(Store.test)+1):nrow(Store)]))^2))
  }
  mse_lf[[j]] = mse_f
}
mse_lf

```

