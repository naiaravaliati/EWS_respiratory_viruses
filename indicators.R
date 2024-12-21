#Coefficient of variation
coefficient_of_variation <- function(x) {
  sd(x)/mean(x)
}

#Index of Dispersion
index_of_dispersion <- function(x) {
  sd(x)*sd(x)/mean(x)
}

#Empirical entropy
empirical_entropy <- function(xs){
  nbins=5
  range.xs   <- max(xs)-min(xs)
  width.bins <- range.xs/nbins
  xs <- xs %>% cut(breaks=nbins) %>% as.numeric()
  epps <- (table(xs) / length(xs)) %>%  as.vector()
  -sum(epps * (log(epps) - log(width.bins)))
} 

#Kolmogorov Smirnov normality test
kolmogorov_smirnov <- function(xs) {
  meanxs = mean(xs)
  sdxs = sd(xs)
  as.numeric(ks.test(xs,
                     "pnorm",
                     mean=meanxs,
                     sd=sdxs)$statistic[1])
}

#Anderson-Darling normality test
anderson_darling <- function(xx) {
  if (length(xx)<8) {xs = rep(10,9)}
  else {xs = xx}
  as.numeric(ad.test(xs)$statistic)
}

#Auto-Correlation lag 1 normality test
acf1 <- function(xs) {
  acf(xs,1,plot=FALSE)$acf[2]
}

#PCA
pca_comp1 <- function(xs) {
  datapca <- summary(princomp(xs))
  datapca$sdev
}

#EVI
evi_func <- function(xs,lag=7) {
  cases = mova(cases = xs)
  roll = rollsd(cases = cases, lag_t = lag)
  ev = evi(rollsd = roll)
  return(ev)
}

#cEVI
cevi_func <- function(xs,lag=7,cut=0.1) {
  cases = mova(cases = xs)
  cev = cEVI_fun(cases=cases,lag_n=lag,c_n=cut)
  return(cev)
}

#Verify Rt
Rt_status <- function(cases,mean_si,std_si) {
  res_parametric_si <- estimate_R(cases, 
                                  method="parametric_si",
                                  config = make_config(list(
                                  mean_si = mean_si, #OK 
                                  std_si = std_si))) #Check
  
  Rt <- res_parametric_si$R$`Mean(R)`
  
  status=rep(0,length(cases))
  status[1:lag]=0
  for (i in lag:(length(cases)-lag)){
    if (Rt[i]>1)
    {status[i]=1}
    else
    {status[i]=0}
  }
  return(status)
  
}

#Verify increasing trend of cases using Kendall's tau
kendall_status <- function(cases,lag=7) {
  status=rep(0,length(cases))
  status[1:lag]=0
  for (i in lag:(length(cases)-lag)){
    kend <- kendall(cases[(i-min((i-1),lag)):(i+min(i,(lag-1)))])
    slope <- kend[2]
    pvalue <- kend[4]
    if (is.na(slope)) {
      slope=0
      pvalue=1e10
      }
    if (is.na(pvalue)) {
      slope=0
      pvalue=1e10
      }
    if ((slope>0) && (pvalue<0.05))
    {status[i]=1}
    else
    {status[i]=0}
  }
  return(status)
}

#Verify increasing trend of cases using Kendall's tau
kendall_status_all <- function(cases,lag=7) {
  status=rep(0,length(cases))
  status[1:lag]=0
  for (i in lag:(length(cases)-lag)){
    kend <- kendall(cases[(i-min((i-1),lag)):(i+min(i,(lag-1)))])
    slope <- kend[2]
    pvalue <- kend[4]
    if (is.na(slope)) {
      slope=0
      pvalue=1e10
    }
    if (is.na(pvalue)) {
      slope=0
      pvalue=1e10
    }
    if ((slope>0) && (pvalue<0.05))
    {status[i]=1}
    else if ((slope<0) && (pvalue<0.05))
    {status[i]=-1}
    else
    {status[i]=0}
  }
  return(status)
}

#Verify increasing trend of cases using Kendall's tau
epidemic_status_all <- function(cases,lag=7) {
  status=rep(0,length(cases))
  status[1:lag]=0
  for (i in lag:(length(cases)-lag)){
    kend <- kendall(cases[(i-min((i-1),lag)):(i+min(i,(lag-1)))])
    slope <- kend[2]
    pvalue <- kend[4]
    if (is.na(slope)) {
      slope=0
      pvalue=1e10
    }
    if (is.na(pvalue)) {
      slope=0
      pvalue=1e10
    }
    if ((slope>0) && (pvalue<0.05))
    {status[i]=1}
    else if ((slope<0) && (pvalue<0.05))
    {status[i]=-1}
    else
    {status[i]=0}
  }
  return(status)
}

#Verify increasing trend of cases using % of increasing cases
r_status <- function(cases, r=0.2) {
  ratio = 1/(1+r)
  w_s=7
  status=rep(0,length(cases))
  status[1]=0
  for (i in 2:(length(cases)-w_s)){
    if (mean(cases[(i-min((i-1),w_s)):(i-1)])<=ratio*mean((cases[i:(i+min(i,(w_s-1)))])))
    {status[i]=1}
    else
    {status[i]=0}
  }
  return(status)
}

calc_all_ews <- function(xs,widths) {
  
  notif_srag_covid0 <- xs %>%
    mutate(freqmean = rollapply(frequency,width=14,FUN=mean,fill=0,align='right')) %>%
    mutate(lastweekmean = rollapply(frequency,width=7,FUN=mean,fill=0,align='right')) %>%
    mutate(EE = rollapply(freqmean,width=widths$EE,FUN=empirical_entropy,fill=0,align='right')) %>%
    mutate(CV = rollapply(freqmean,width=widths$CV,FUN=coefficient_of_variation,fill=0,align='right')) %>%
    mutate(SKEW = rollapply(freqmean,width=widths$SKEW,FUN=skewness,fill=0,align='right')) %>%
    mutate(KURT = rollapply(freqmean,width=widths$KURT,FUN=kurtosis,fill=0,align='right')) %>%
    mutate(ID = rollapply(freqmean,width=widths$ID,FUN=index_of_dispersion,fill=0,align='right')) %>%
    mutate(ACF = rollapply(freqmean,width=widths$ACF,FUN=acf1,fill=0,align='right'))
  
  is.na(notif_srag_covid0)<-sapply(notif_srag_covid0, is.infinite)
  notif_srag_covid0[is.na(notif_srag_covid0)]<-0
  
  #Principal Component Analysis
  PCA.analysis <- princomp(scale(notif_srag_covid0[,c("EE","CV","SKEW","KURT","ID","ACF")]))
  notif_srag_covid0$PCA1 <- PCA.analysis$scores[,1]
  notif_srag_covid0$PCA12 <- PCA.analysis$scores[,1]+PCA.analysis$scores[,2]
  notif_srag_covid0$PCA123 <- PCA.analysis$scores[,1]+PCA.analysis$scores[,2]+PCA.analysis$scores[,3]
  notif_srag_covid0[is.na(notif_srag_covid0)] <- 0
  
  return(notif_srag_covid0)
}

calc_all_ews_evi <- function(xs,lags,cut_cEVI) {
  
  notif_srag_covid0 <- xs
  notif_srag_covid0$EVI <- evi_func(notif_srag_covid0$frequency,lag=lags$EVI) 
  notif_srag_covid0$cEVI <- cevi_func(notif_srag_covid0$frequency,lag=lags$cEVI,cut=cut_cEVI) 
  
  return(notif_srag_covid0)
}

calc_all_status <- function(xs,widths) {
  
  notif_srag_covid0 <- xs
  
  #Calc cases
  Rtstatus <- Rt_status(cases=notif_srag_covid0$frequency,mean_si=5.2,std_si=1.2)
  kstatus <- kendall_status(cases=notif_srag_covid0$frequency,lag=7)
  rstatus <- r_status(cases=notif_srag_covid0$frequency,r=0.2)
  r01status <- r_status(cases=notif_srag_covid0$frequency,r=0.1)
  
  notif_srag_covid0$RRt <- Rtstatus
  notif_srag_covid0$RRt_status <- Rtstatus
  notif_srag_covid0$RRt_status[notif_srag_covid0$RRt_status==1] <- "Increasing"
  notif_srag_covid0$RRt_status[notif_srag_covid0$RRt_status==0] <- "Non-increasing"
  
  notif_srag_covid0$kendall <- kstatus
  notif_srag_covid0$kendall_status <- kstatus
  notif_srag_covid0$kendall_status[notif_srag_covid0$kendall_status==1] <- "Increasing"
  notif_srag_covid0$kendall_status[notif_srag_covid0$kendall_status==0] <- "Non-increasing"
  
  notif_srag_covid0$r02 <- rstatus
  notif_srag_covid0$r02_status <- rstatus
  notif_srag_covid0$r02_status[notif_srag_covid0$r02_status==1] <- "Increasing"
  notif_srag_covid0$r02_status[notif_srag_covid0$r02_status==0] <- "Non-increasing"
  
  notif_srag_covid0$r01 <- r01status
  notif_srag_covid0$r01_status <- r01status
  notif_srag_covid0$r01_status[notif_srag_covid0$r01_status==1] <- "Increasing"
  notif_srag_covid0$r01_status[notif_srag_covid0$r01_status==0] <- "Non-increasing"
  
  return(notif_srag_covid0)
}

