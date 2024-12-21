#Build Se Sp table and calculate statistics
stat_table <- function(cases, indic, status) {
  w_s <- 7
  sens = length(which(indic == 1 & status == 1))/length(which(status == 1))
  spec = length(which(indic == 0 & status == 0))/length(which(status == 0))
  ppv = length(which(indic == 1 & status == 1))/length(which(indic == 1))
  npv = length(which(indic == 0 & status == 0))/length(which(indic == 0))
  sens[is.nan(sens)] <- 0
  spec[is.nan(spec)] <- 0
  sens[is.nan(sens)] <- 0
  spec[is.nan(spec)] <- 0
  testsin = length(which(indic == 1))/(length(cases) - w_s)
  prev = length(which(status == 1))/(length(cases) - w_s)
  stat_table <- list(sens = sens, spec = spec, testsin = testsin,
                     prev = prev, ppv=ppv, npv=npv)
  return(stat_table)
}

#Build Se Sp table and calculate statistics
indic_status <- function(cases, lastweekmean, ind, method="EVI", cut=0.01) {
  
  w_s = 7
  
  indic = rep(NA, length(cases))
  
  if(method=="EVI"){
    w_s = 7
    indic <- rep(0,length(cases))
    indic[which(ind >= cut)] <- 1
    indic[is.na(indic)] <- 0
  }
  
  if(method=="EVI2"){
    w_s = 7
    indic <- rep(0,length(cases))
    indic[which((ind >= cut) & (cases > lastweekmean))] <- 1
    indic[is.na(indic)] <- 0
  }
  
  if(method=="cEVI"){
    indic <- rep(0,length(cases))
    indic[which((ind == 1) & (!is.na(ind)))] <- 1
    indic[is.na(indic)] <- 0
  }
  
  if(method=="cEVI2"){
    indic <- rep(0,length(cases))
    indic[which((ind == 1) & (!is.na(ind)) & (cases > lastweekmean))] <- 1
    indic[is.na(indic)] <- 0
  }
  
  
  if(method=="cEVI-"){
    cevi <- ind$cev
    evi <- ind$ev
    
    indic <- rep(0,length(cases))
    indic[which((cevi == 1) & (!is.na(cevi)) & (evi>=cut))] <- 1
    indic[is.na(indic)] <- 0
  }
  
  if(method=="cEVI-2"){
    cevi <- ind$cev
    evi <- ind$ev
    indic <- rep(0,length(cases))
    indic[which((cevi == 1) & (!is.na(cevi)) & (evi>=cut) & 
                  (cases > lastweekmean))] <- 1
    indic[is.na(indic)] <- 0
  }
  
  if(method=="cEVI+"){
    cevi <- ind$cev
    evi <- ind$ev
    
    indic <- rep(0,length(cases))
    indic[which(((!is.na(cevi) & cevi == 1) | (evi >= cut)))] <- 1
    indic[is.na(indic)] <- 0
  }
  
  if(method=="cEVI+2"){
    cevi <- ind$cev
    evi <- ind$ev
    
    indic <- rep(0,length(cases))
    indic[which(( (!is.na(cevi) & cevi == 1) | (evi >= cut) ) &
                  (cases > lastweekmean) )] <- 1
    indic[is.na(indic)] <- 0
  }
  
  if(method=="CV" | method=="EE" | method=="SKEW" | 
     method=="KURT" | method=="ID" | method=="KS" |
     method=="PCA" | method=="AD" | method=="ACF"){
    w_s <- 7
    indic <- rep(0,length(cases))
    scaled_ind <- scale(ind)
    indic[which(scaled_ind[w_s:(length(cases)-w_s)] >= cut)] <- 1
    indic[is.na(indic)] <- 0
  }
  
  if(method=="CV2" | method=="EE2" | method=="SKEW2" | 
     method=="KURT2" | method=="ID2" | method=="KS2" |
     method=="PCA2" | method=="AD2" | method=="ACF2"){
    w_s <- 7
    indic <- rep(0,length(cases))
    scaled_ind <- scale(ind)
    indic[which( (scaled_ind[w_s:(length(cases)-w_s)] >= cut) &
                   (cases > lastweekmean) )] <- 1
    indic[is.na(indic)] <- 0
  }
  
  if(method=="PCA-lag"){
    lagg <- 1
    w_s <- 7
    indic <- rep(0,length(cases))
    scaled_ind <- lag(scale(ind),lagg)
    indic[which(scaled_ind >= cut)] <- 1
    indic[is.na(indic)] <- 0
  }
  
  if(method=="PCA-lag2"){
    lagg <- 1
    w_s <- 7
    indic <- rep(0,length(cases))
    scaled_ind <- lag(scale(ind),lagg)
    indic[which( (scaled_ind[w_s:(length(cases)-w_s)] >= cut) &
                   (cases > lastweekmean) )] <- 1
    indic[is.na(indic)] <- 0
  }
  
  return(indic)
}

AUC_all <- function(xs,lags,istatus) {
  
  notif_srag_covid0 <- xs
  ROC0 <- ROC_all(notif_srag_covid0,lags,istatus)
  
  AUC0 <- data.frame(ROC0$CV_fpr[1])
  AUC0$CV <- AUC(x=c(ROC0$CV_fpr,0,1),y=c(ROC0$CV_tpr,0,1))
  AUC0$EE <- AUC(x=c(ROC0$EE_fpr,0,1),y=c(ROC0$EE_tpr,0,1))
  AUC0$ACF <- AUC(x=c(ROC0$ACF_fpr,0,1),y=c(ROC0$ACF_tpr,0,1))
  AUC0$SKEW <- AUC(x=c(ROC0$SKEW_fpr,0,1),y=c(ROC0$SKEW_tpr,0,1))
  AUC0$KURT <- AUC(x=c(ROC0$KURT_fpr,0,1),y=c(ROC0$KURT_tpr,0,1))
  AUC0$ID <- AUC(x=c(ROC0$ID_fpr,0,1),y=c(ROC0$ID_tpr,0,1))
  AUC0$PCA1 <- AUC(x=c(ROC0$PCA1_fpr,0,1),y=c(ROC0$PCA1_tpr,0,1))
  AUC0$PCA12 <- AUC(x=c(ROC0$PCA12_fpr,0,1),y=c(ROC0$PCA12_tpr,0,1))
  AUC0$PCA123 <- AUC(x=c(ROC0$PCA123_fpr,0,1),y=c(ROC0$PCA123_tpr,0,1))
  AUC0$PCA1lag <- AUC(x=c(ROC0$PCA1lag_fpr,0,1),y=c(ROC0$PCA1lag_tpr,0,1))
  
  return(AUC0)
}

AUC_all_cevi <- function(xs,lags,istatus) {
  
  notif_srag_covid0 <- xs
  ROC1 <- ROC_all_cevi(notif_srag_covid0,lags,istatus)
  
  AUC0$EVI <- AUC(x=c(ROC1$EVI_fpr,0,1),y=c(ROC1$EVI_tpr,0,1))
  AUC0$cEVI <- AUC(x=c(ROC1$cEVI_fpr,0,1),y=c(ROC1$cEVI_tpr,0,1))
  AUC0$cEVIm <- AUC(x=c(ROC1$cEVIm_fpr,0,1),y=c(ROC1$cEVIm_tpr,0,1))
  AUC0$cEVIp <- AUC(x=c(ROC1$cEVIp_fpr,0,1),y=c(ROC1$cEVIp_tpr,0,1))
  
  return(AUC0)
}

ROC_all <- function(xs,lags,istatus) {
  
  notif_srag_covid0 <- xs
  notif_srag_covid0 <- notif_srag_covid0 %>%
    mutate(CV = scale(CV)) %>%
    mutate(EE = scale(EE)) %>%
    mutate(SKEW = scale(SKEW)) %>%
    mutate(KURT = scale(KURT)) %>%
    mutate(ID = scale(ID)) %>%
    mutate(ACF = scale(ACF)) %>%
    mutate(PCA1 = scale(PCA1)) %>%
    mutate(PCA12 = scale(PCA12)) %>%
    mutate(PCA123 = scale(PCA123))
  
  cuts = seq(from = 0.0, to = 1.0, by = 0.01)
  
  ROC0 <- data.frame(cuts)
  ROC0 <- ROC0 %>%
    mutate(CV_tpr = 0) %>%
    mutate(CV_fpr = 0) %>%
    mutate(EE_tpr = 0) %>%
    mutate(EE_fpr = 0) %>%
    mutate(ID_tpr = 0) %>%
    mutate(ID_fpr = 0) %>%
    mutate(KS_tpr = 0) %>%
    mutate(KS_fpr = 0) %>%
    mutate(AD_tpr = 0) %>%
    mutate(AD_fpr = 0) %>%
    mutate(ACF_tpr = 0) %>%
    mutate(ACF_fpr = 0) %>%
    mutate(SKEW_tpr = 0) %>%
    mutate(SKEW_fpr = 0) %>%
    mutate(KURT_tpr = 0) %>%
    mutate(KURT_fpr = 0) %>%
    mutate(PCA1_tpr = 0) %>%
    mutate(PCA1_fpr = 0) %>%
    mutate(PCA12_tpr = 0) %>%
    mutate(PCA12_fpr = 0) %>%
    mutate(PCA123_tpr = 0) %>%
    mutate(PCA123_fpr = 0) %>%
    mutate(PCA1lag_tpr = 0) %>%
    mutate(PCA1lag_fpr = 0) %>%
    mutate(CV_ppv = 0) %>%
    mutate(CV_npv = 0) %>%
    mutate(EE_ppv = 0) %>%
    mutate(EE_npv = 0) %>%
    mutate(ID_ppv = 0) %>%
    mutate(ID_npv = 0) %>%
    mutate(KS_ppv = 0) %>%
    mutate(KS_npv = 0) %>%
    mutate(AD_ppv = 0) %>%
    mutate(AD_npv = 0) %>%
    mutate(ACF_ppv = 0) %>%
    mutate(ACF_npv = 0) %>%
    mutate(SKEW_ppv = 0) %>%
    mutate(SKEW_npv = 0) %>%
    mutate(KURT_ppv = 0) %>%
    mutate(KURT_npv = 0) %>%
    mutate(PCA1_ppv = 0) %>%
    mutate(PCA1_npv = 0) %>%
    mutate(PCA12_ppv = 0) %>%
    mutate(PCA12_npv = 0) %>%
    mutate(PCA123_ppv = 0) %>%
    mutate(PCA123_npv = 0) %>%
    mutate(PCA1lag_ppv = 0) %>%
    mutate(PCA1lag_npv = 0)

  for (i in 1:length(cuts)){
    
    #CV
    minn = min(notif_srag_covid0$CV,na.rm=TRUE)
    maxx = max(notif_srag_covid0$CV,na.rm=TRUE)
    cutt = minn + (i-1)*(maxx-minn)/length(cuts)
    indicstatus <- indic_status(cases=notif_srag_covid0$frequency,
                                lastweekmean=notif_srag_covid0$lastweekmean,
                                ind=notif_srag_covid0$CV, 
                                method="CV", cut=cutt)
    stat <- stat_table(cases=notif_srag_covid0$frequency, 
                       indic=indicstatus, 
                       status=istatus)
    ROC0$CV_fpr[i] <- 1-stat$spec
    ROC0$CV_tpr[i] <- stat$sens
    ROC0$CV_ppv[i] <- stat$ppv
    ROC0$CV_npv[i] <- stat$npv
    
    #EE
    minn = min(notif_srag_covid0$EE,na.rm=TRUE)
    maxx = max(notif_srag_covid0$EE,na.rm=TRUE)
    cutt = minn + (i-1)*(maxx-minn)/length(cuts)
    indicstatus <- indic_status(cases=notif_srag_covid0$frequency,
                                lastweekmean=notif_srag_covid0$lastweekmean, 
                                ind=notif_srag_covid0$EE, 
                                method="EE", cut=cutt)
    stat <- stat_table(cases=notif_srag_covid0$frequency, 
                       indic=indicstatus, 
                       status=istatus)
    ROC0$EE_fpr[i] <- 1-stat$spec
    ROC0$EE_tpr[i] <- stat$sens
    ROC0$EE_ppv[i] <- stat$ppv
    ROC0$EE_npv[i] <- stat$npv
    
    #SKEW
    minn = min(notif_srag_covid0$SKEW,na.rm=TRUE)
    maxx = max(notif_srag_covid0$SKEW,na.rm=TRUE)
    cutt = minn + (i-1)*(maxx-minn)/length(cuts)
    indicstatus <- indic_status(cases=notif_srag_covid0$frequency, 
                                lastweekmean=notif_srag_covid0$lastweekmean,
                                ind=notif_srag_covid0$SKEW, 
                                method="SKEW", cut=cutt)
    stat <- stat_table(cases=notif_srag_covid0$frequency, 
                       indic=indicstatus, 
                       status=istatus)
    ROC0$SKEW_fpr[i] <- 1-stat$spec
    ROC0$SKEW_tpr[i] <- stat$sens
    ROC0$SKEW_ppv[i] <- stat$ppv
    ROC0$SKEW_npv[i] <- stat$npv
    
    #KURT
    minn = min(notif_srag_covid0$KURT,na.rm=TRUE)
    maxx = max(notif_srag_covid0$KURT,na.rm=TRUE)
    cutt = minn + (i-1)*(maxx-minn)/length(cuts)
    indicstatus <- indic_status(cases=notif_srag_covid0$frequency, 
                                lastweekmean=notif_srag_covid0$lastweekmean,
                                ind=notif_srag_covid0$KURT, 
                                method="KURT", cut=cutt)
    stat <- stat_table(cases=notif_srag_covid0$frequency, 
                       indic=indicstatus, 
                       status=istatus)
    ROC0$KURT_fpr[i] <- 1-stat$spec
    ROC0$KURT_tpr[i] <- stat$sens
    ROC0$KURT_ppv[i] <- stat$ppv
    ROC0$KURT_npv[i] <- stat$npv
    
    #ACF
    minn = min(notif_srag_covid0$ACF,na.rm=TRUE)
    maxx = max(notif_srag_covid0$ACF,na.rm=TRUE)
    cutt = minn + (i-1)*(maxx-minn)/length(cuts)
    indicstatus <- indic_status(cases=notif_srag_covid0$frequency, 
                                lastweekmean=notif_srag_covid0$lastweekmean,
                                ind=notif_srag_covid0$ACF, 
                                method="ACF", cut=cutt)
    stat <- stat_table(cases=notif_srag_covid0$frequency, 
                       indic=indicstatus, 
                       status=istatus)
    ROC0$ACF_fpr[i] <- 1-stat$spec
    ROC0$ACF_tpr[i] <- stat$sens
    ROC0$ACF_ppv[i] <- stat$ppv
    ROC0$ACF_npv[i] <- stat$npv
    
    #ID
    minn = min(notif_srag_covid0$ID,na.rm=TRUE)
    maxx = max(notif_srag_covid0$ID,na.rm=TRUE)
    cutt = minn + (i-1)*(maxx-minn)/length(cuts)
    indicstatus <- indic_status(cases=notif_srag_covid0$frequency, 
                                lastweekmean=notif_srag_covid0$lastweekmean,
                                ind=notif_srag_covid0$ID, 
                                method="ID", cut=cutt)
    stat <- stat_table(cases=notif_srag_covid0$frequency, 
                       indic=indicstatus, 
                       status=istatus)
    ROC0$ID_fpr[i] <- 1-stat$spec
    ROC0$ID_tpr[i] <- stat$sens
    ROC0$ID_ppv[i] <- stat$ppv
    ROC0$ID_npv[i] <- stat$npv
    
    #PCA
    minn = min(notif_srag_covid0$PCA1,na.rm=TRUE)
    maxx = max(notif_srag_covid0$PCA1,na.rm=TRUE)
    cutt = minn + (i-1)*(maxx-minn)/length(cuts)
    indicstatus <- indic_status(cases=notif_srag_covid0$frequency, 
                                lastweekmean=notif_srag_covid0$lastweekmean,
                                ind=notif_srag_covid0$PCA1, 
                                method="PCA", cut=cutt)
    stat <- stat_table(cases=notif_srag_covid0$frequency, 
                       indic=indicstatus, 
                       status=istatus)
    ROC0$PCA1_fpr[i] <- 1-stat$spec
    ROC0$PCA1_tpr[i] <- stat$sens
    ROC0$PCA1_ppv[i] <- stat$ppv
    ROC0$PCA1_npv[i] <- stat$npv
    
    minn = min(notif_srag_covid0$PCA12,na.rm=TRUE)
    maxx = max(notif_srag_covid0$PCA12,na.rm=TRUE)
    cutt = minn + (i-1)*(maxx-minn)/length(cuts)
    indicstatus <- indic_status(cases=notif_srag_covid0$frequency, 
                                lastweekmean=notif_srag_covid0$lastweekmean,
                                ind=notif_srag_covid0$PCA12, 
                                method="PCA", cut=cutt)
    stat <- stat_table(cases=notif_srag_covid0$frequency, 
                       indic=indicstatus, 
                       status=istatus)
    ROC0$PCA12_fpr[i] <- 1-stat$spec
    ROC0$PCA12_tpr[i] <- stat$sens
    ROC0$PCA12_ppv[i] <- stat$ppv
    ROC0$PCA12_npv[i] <- stat$npv
    
    minn = min(notif_srag_covid0$PCA123,na.rm=TRUE)
    maxx = max(notif_srag_covid0$PCA123,na.rm=TRUE)
    cutt = minn + (i-1)*(maxx-minn)/length(cuts)
    indicstatus <- indic_status(cases=notif_srag_covid0$frequency, 
                                lastweekmean=notif_srag_covid0$lastweekmean,
                                ind=notif_srag_covid0$PCA123, 
                                method="PCA", cut=cutt)
    stat <- stat_table(cases=notif_srag_covid0$frequency, 
                       indic=indicstatus, 
                       status=istatus)
    ROC0$PCA123_fpr[i] <- 1-stat$spec
    ROC0$PCA123_tpr[i] <- stat$sens
    ROC0$PCA123_ppv[i] <- stat$ppv
    ROC0$PCA123_npv[i] <- stat$npv
    
    minn = min(notif_srag_covid0$PCA1,na.rm=TRUE)
    maxx = max(notif_srag_covid0$PCA1,na.rm=TRUE)
    cutt = minn + (i-1)*(maxx-minn)/length(cuts)
    indicstatus <- indic_status(cases=notif_srag_covid0$frequency, 
                                lastweekmean=notif_srag_covid0$lastweekmean,
                                ind=notif_srag_covid0$PCA1, 
                                method="PCA-lag", cut=cutt)
    stat <- stat_table(cases=notif_srag_covid0$frequency, 
                       indic=indicstatus, 
                       status=istatus)
    ROC0$PCA1lag_fpr[i] <- 1-stat$spec
    ROC0$PCA1lag_tpr[i] <- stat$sens
    ROC0$PCA1lag_ppv[i] <- stat$ppv
    ROC0$PCA1lag_npv[i] <- stat$npv
    
  }
  
  return(ROC0)
}

ROC_all_2 <- function(xs,lags,istatus) {
  
  notif_srag_covid0 <- xs
  
  notif_srag_covid0 <- notif_srag_covid0 %>%
    mutate(CV = scale(CV)) %>%
    mutate(EE = scale(EE)) %>%
    mutate(SKEW = scale(SKEW)) %>%
    mutate(KURT = scale(KURT)) %>%
    mutate(ID = scale(ID)) %>%
    mutate(ACF = scale(ACF)) %>%
    mutate(PCA1 = scale(PCA1)) %>%
    mutate(PCA12 = scale(PCA12)) %>%
    mutate(PCA123 = scale(PCA123))
  
  cuts = seq(from = 0.0, to = 1.0, by = 0.01)
  
  ROC0 <- data.frame(cuts)
  ROC0 <- ROC0 %>%
    mutate(CV_tpr = 0) %>%
    mutate(CV_fpr = 0) %>%
    mutate(EE_tpr = 0) %>%
    mutate(EE_fpr = 0) %>%
    mutate(ID_tpr = 0) %>%
    mutate(ID_fpr = 0) %>%
    mutate(KS_tpr = 0) %>%
    mutate(KS_fpr = 0) %>%
    mutate(AD_tpr = 0) %>%
    mutate(AD_fpr = 0) %>%
    mutate(ACF_tpr = 0) %>%
    mutate(ACF_fpr = 0) %>%
    mutate(SKEW_tpr = 0) %>%
    mutate(SKEW_fpr = 0) %>%
    mutate(KURT_tpr = 0) %>%
    mutate(KURT_fpr = 0) %>%
    mutate(PCA1_tpr = 0) %>%
    mutate(PCA1_fpr = 0) %>%
    mutate(PCA12_tpr = 0) %>%
    mutate(PCA12_fpr = 0) %>%
    mutate(PCA123_tpr = 0) %>%
    mutate(PCA123_fpr = 0) %>%
    mutate(PCA1lag_tpr = 0) %>%
    mutate(PCA1lag_fpr = 0) %>%
    mutate(CV_ppv = 0) %>%
    mutate(CV_npv = 0) %>%
    mutate(EE_ppv = 0) %>%
    mutate(EE_npv = 0) %>%
    mutate(ID_ppv = 0) %>%
    mutate(ID_npv = 0) %>%
    mutate(KS_ppv = 0) %>%
    mutate(KS_npv = 0) %>%
    mutate(AD_ppv = 0) %>%
    mutate(AD_npv = 0) %>%
    mutate(ACF_ppv = 0) %>%
    mutate(ACF_npv = 0) %>%
    mutate(SKEW_ppv = 0) %>%
    mutate(SKEW_npv = 0) %>%
    mutate(KURT_ppv = 0) %>%
    mutate(KURT_npv = 0) %>%
    mutate(PCA1_ppv = 0) %>%
    mutate(PCA1_npv = 0) %>%
    mutate(PCA12_ppv = 0) %>%
    mutate(PCA12_npv = 0) %>%
    mutate(PCA123_ppv = 0) %>%
    mutate(PCA123_npv = 0) %>%
    mutate(PCA1lag_ppv = 0) %>%
    mutate(PCA1lag_npv = 0)
  
  for (i in 1:length(cuts)){
    
    #CV
    minn = min(notif_srag_covid0$CV,na.rm=TRUE)
    maxx = max(notif_srag_covid0$CV,na.rm=TRUE)
    cutt = minn + (i-1)*(maxx-minn)/length(cuts)
    indicstatus <- indic_status(cases=notif_srag_covid0$frequency,
                                lastweekmean=notif_srag_covid0$lastweekmean,
                                ind=notif_srag_covid0$CV, 
                                method="CV2", cut=cutt)
    stat <- stat_table(cases=notif_srag_covid0$frequency, 
                       indic=indicstatus, 
                       status=istatus)
    ROC0$CV_fpr[i] <- 1-stat$spec
    ROC0$CV_tpr[i] <- stat$sens
    ROC0$CV_ppv[i] <- stat$ppv
    ROC0$CV_npv[i] <- stat$npv
    
    #EE
    minn = min(notif_srag_covid0$EE,na.rm=TRUE)
    maxx = max(notif_srag_covid0$EE,na.rm=TRUE)
    cutt = minn + (i-1)*(maxx-minn)/length(cuts)
    indicstatus <- indic_status(cases=notif_srag_covid0$frequency,
                                lastweekmean=notif_srag_covid0$lastweekmean, 
                                ind=notif_srag_covid0$EE, 
                                method="EE2", cut=cutt)
    stat <- stat_table(cases=notif_srag_covid0$frequency, 
                       indic=indicstatus, 
                       status=istatus)
    ROC0$EE_fpr[i] <- 1-stat$spec
    ROC0$EE_tpr[i] <- stat$sens
    ROC0$EE_ppv[i] <- stat$ppv
    ROC0$EE_npv[i] <- stat$npv
    
    #SKEW
    minn = min(notif_srag_covid0$SKEW,na.rm=TRUE)
    maxx = max(notif_srag_covid0$SKEW,na.rm=TRUE)
    cutt = minn + (i-1)*(maxx-minn)/length(cuts)
    indicstatus <- indic_status(cases=notif_srag_covid0$frequency, 
                                lastweekmean=notif_srag_covid0$lastweekmean,
                                ind=notif_srag_covid0$SKEW, 
                                method="SKEW2", cut=cutt)
    stat <- stat_table(cases=notif_srag_covid0$frequency, 
                       indic=indicstatus, 
                       status=istatus)
    ROC0$SKEW_fpr[i] <- 1-stat$spec
    ROC0$SKEW_tpr[i] <- stat$sens
    ROC0$SKEW_ppv[i] <- stat$ppv
    ROC0$SKEW_npv[i] <- stat$npv
    
    #KURT
    minn = min(notif_srag_covid0$KURT,na.rm=TRUE)
    maxx = max(notif_srag_covid0$KURT,na.rm=TRUE)
    cutt = minn + (i-1)*(maxx-minn)/length(cuts)
    indicstatus <- indic_status(cases=notif_srag_covid0$frequency, 
                                lastweekmean=notif_srag_covid0$lastweekmean,
                                ind=notif_srag_covid0$KURT, 
                                method="KURT2", cut=cutt)
    stat <- stat_table(cases=notif_srag_covid0$frequency, 
                       indic=indicstatus, 
                       status=istatus)
    ROC0$KURT_fpr[i] <- 1-stat$spec
    ROC0$KURT_tpr[i] <- stat$sens
    ROC0$KURT_ppv[i] <- stat$ppv
    ROC0$KURT_npv[i] <- stat$npv
    
    #ACF
    minn = min(notif_srag_covid0$ACF,na.rm=TRUE)
    maxx = max(notif_srag_covid0$ACF,na.rm=TRUE)
    cutt = minn + (i-1)*(maxx-minn)/length(cuts)
    indicstatus <- indic_status(cases=notif_srag_covid0$frequency, 
                                lastweekmean=notif_srag_covid0$lastweekmean,
                                ind=notif_srag_covid0$ACF, 
                                method="ACF2", cut=cutt)
    stat <- stat_table(cases=notif_srag_covid0$frequency, 
                       indic=indicstatus, 
                       status=istatus)
    ROC0$ACF_fpr[i] <- 1-stat$spec
    ROC0$ACF_tpr[i] <- stat$sens
    ROC0$ACF_ppv[i] <- stat$ppv
    ROC0$ACF_npv[i] <- stat$npv
    
    #ID
    minn = min(notif_srag_covid0$ID,na.rm=TRUE)
    maxx = max(notif_srag_covid0$ID,na.rm=TRUE)
    cutt = minn + (i-1)*(maxx-minn)/length(cuts)
    indicstatus <- indic_status(cases=notif_srag_covid0$frequency, 
                                lastweekmean=notif_srag_covid0$lastweekmean,
                                ind=notif_srag_covid0$ID, 
                                method="ID2", cut=cutt)
    stat <- stat_table(cases=notif_srag_covid0$frequency, 
                       indic=indicstatus, 
                       status=istatus)
    ROC0$ID_fpr[i] <- 1-stat$spec
    ROC0$ID_tpr[i] <- stat$sens
    ROC0$ID_ppv[i] <- stat$ppv
    ROC0$ID_npv[i] <- stat$npv
    
    #PCA
    minn = min(notif_srag_covid0$PCA1,na.rm=TRUE)
    maxx = max(notif_srag_covid0$PCA1,na.rm=TRUE)
    cutt = minn + (i-1)*(maxx-minn)/length(cuts)
    indicstatus <- indic_status(cases=notif_srag_covid0$frequency, 
                                lastweekmean=notif_srag_covid0$lastweekmean,
                                ind=notif_srag_covid0$PCA1, 
                                method="PCA2", cut=cutt)
    stat <- stat_table(cases=notif_srag_covid0$frequency, 
                       indic=indicstatus, 
                       status=istatus)
    ROC0$PCA1_fpr[i] <- 1-stat$spec
    ROC0$PCA1_tpr[i] <- stat$sens
    ROC0$PCA1_ppv[i] <- stat$ppv
    ROC0$PCA1_npv[i] <- stat$npv
    
    minn = min(notif_srag_covid0$PCA12,na.rm=TRUE)
    maxx = max(notif_srag_covid0$PCA12,na.rm=TRUE)
    cutt = minn + (i-1)*(maxx-minn)/length(cuts)
    indicstatus <- indic_status(cases=notif_srag_covid0$frequency, 
                                lastweekmean=notif_srag_covid0$lastweekmean,
                                ind=notif_srag_covid0$PCA12, 
                                method="PCA2", cut=cutt)
    stat <- stat_table(cases=notif_srag_covid0$frequency, 
                       indic=indicstatus, 
                       status=istatus)
    ROC0$PCA12_fpr[i] <- 1-stat$spec
    ROC0$PCA12_tpr[i] <- stat$sens
    ROC0$PCA12_ppv[i] <- stat$ppv
    ROC0$PCA12_npv[i] <- stat$npv
    
    minn = min(notif_srag_covid0$PCA123,na.rm=TRUE)
    maxx = max(notif_srag_covid0$PCA123,na.rm=TRUE)
    cutt = minn + (i-1)*(maxx-minn)/length(cuts)
    indicstatus <- indic_status(cases=notif_srag_covid0$frequency, 
                                lastweekmean=notif_srag_covid0$lastweekmean,
                                ind=notif_srag_covid0$PCA123, 
                                method="PCA2", cut=cutt)
    stat <- stat_table(cases=notif_srag_covid0$frequency, 
                       indic=indicstatus, 
                       status=istatus)
    ROC0$PCA123_fpr[i] <- 1-stat$spec
    ROC0$PCA123_tpr[i] <- stat$sens
    ROC0$PCA123_ppv[i] <- stat$ppv
    ROC0$PCA123_npv[i] <- stat$npv
    
    minn = min(notif_srag_covid0$PCA1,na.rm=TRUE)
    maxx = max(notif_srag_covid0$PCA1,na.rm=TRUE)
    cutt = minn + (i-1)*(maxx-minn)/length(cuts)
    indicstatus <- indic_status(cases=notif_srag_covid0$frequency, 
                                lastweekmean=notif_srag_covid0$lastweekmean,
                                ind=notif_srag_covid0$PCA1, 
                                method="PCA-lag2", cut=cutt)
    stat <- stat_table(cases=notif_srag_covid0$frequency, 
                       indic=indicstatus, 
                       status=istatus)
    ROC0$PCA1lag_fpr[i] <- 1-stat$spec
    ROC0$PCA1lag_tpr[i] <- stat$sens
    ROC0$PCA1lag_ppv[i] <- stat$ppv
    ROC0$PCA1lag_npv[i] <- stat$npv
    
  }
  
  return(ROC0)
}

ROC_all_cevi <- function(xs,lags,istatus) {
  
  notif_srag_covid0 <- xs
  
  cuts = seq(from = 0.0, to = 1.0, by = 0.01)
  
  ROC0 <- data.frame(cuts)
  ROC0 <- ROC0 %>%
    mutate(EVI_tpr = 0) %>%
    mutate(EVI_fpr = 0) %>%
    mutate(cEVI_tpr = 0) %>%
    mutate(cEVI_fpr = 0) %>%
    mutate(cEVIp_tpr = 0) %>%
    mutate(cEVIp_fpr = 0) %>%
    mutate(cEVIm_tpr = 0) %>%
    mutate(cEVIm_fpr = 0) %>%
    mutate(EVI_ppv = 0) %>%
    mutate(EVI_npv = 0) %>%
    mutate(cEVI_ppv = 0) %>%
    mutate(cEVI_npv = 0) %>%
    mutate(cEVIp_ppv = 0) %>%
    mutate(cEVIp_npv = 0) %>%
    mutate(cEVIm_ppv = 0) %>%
    mutate(cEVIm_npv = 0)
  
  for (i in 1:length(cuts)){
    
    notif_srag_covid0$cEVI <- cevi_func(notif_srag_covid0$frequency,lag=lags$cEVI,cut=cuts[i]) 
    
    #EVI
    minn = min(notif_srag_covid0$EVI,na.rm=TRUE)
    maxx = max(notif_srag_covid0$EVI,na.rm=TRUE)
    cutt = minn + (i-1)*(maxx-minn)/length(cuts)
    indicstatus <- indic_status(cases=notif_srag_covid0$frequency, 
                                lastweekmean=notif_srag_covid0$lastweekmean,
                                ind=notif_srag_covid0$EVI, 
                                method="EVI", cut=cutt)
    stat <- stat_table(cases=notif_srag_covid0$frequency, 
                       indic=indicstatus, 
                       status=istatus)
    ROC0$EVI_fpr[i] <- 1-stat$spec
    ROC0$EVI_tpr[i] <- stat$sens
    ROC0$EVI_ppv[i] <- stat$ppv
    ROC0$EVI_npv[i] <- stat$npv
    
    #cEVI
    minn = min(notif_srag_covid0$EVI,na.rm=TRUE)
    maxx = max(notif_srag_covid0$EVI,na.rm=TRUE)
    cutt = minn + (i-1)*(maxx-minn)/length(cuts)
    notif_srag_covid0$cEVI <- notif_srag_covid0$cEVI
    indicstatus <- indic_status(cases=notif_srag_covid0$frequency, 
                                lastweekmean=notif_srag_covid0$lastweekmean,
                                ind=notif_srag_covid0$cEVI, 
                                method="cEVI", cut=cutt)
    stat <- stat_table(cases=notif_srag_covid0$frequency, 
                       indic=indicstatus, 
                       status=istatus)
    ROC0$cEVI_fpr[i] <- 1-stat$spec
    ROC0$cEVI_tpr[i] <- stat$sens
    ROC0$cEVI_ppv[i] <- stat$ppv
    ROC0$cEVI_npv[i] <- stat$npv
    
    #cEVI+
    minn = min(notif_srag_covid0$EVI,na.rm=TRUE)
    maxx = max(notif_srag_covid0$EVI,na.rm=TRUE)
    cutt = minn + (i-1)*(maxx-minn)/length(cuts)
    ev <- notif_srag_covid0$EVI
    cev <- notif_srag_covid0$cEVI
    indicstatus <- indic_status(cases=notif_srag_covid0$frequency, 
                                lastweekmean=notif_srag_covid0$lastweekmean,
                                ind=data.frame(ev,cev), 
                                method="cEVI+", cut=cutt)
    stat <- stat_table(cases=notif_srag_covid0$frequency, 
                       indic=indicstatus, 
                       status=istatus)
    ROC0$cEVIp_fpr[i] <- 1-stat$spec
    ROC0$cEVIp_tpr[i] <- stat$sens
    ROC0$cEVIp_ppv[i] <- stat$ppv
    ROC0$cEVIp_npv[i] <- stat$npv
    
    #cEVI-
    minn = min(notif_srag_covid0$EVI,na.rm=TRUE)
    maxx = max(notif_srag_covid0$EVI,na.rm=TRUE)
    cutt = minn + (i-1)*(maxx-minn)/length(cuts)
    ev <- notif_srag_covid0$EVI
    cev <- notif_srag_covid0$cEVI
    indicstatus <- indic_status(cases=notif_srag_covid0$frequency, 
                                lastweekmean=notif_srag_covid0$lastweekmean,
                                ind=data.frame(ev,cev), 
                                method="cEVI-", cut=cutt)
    stat <- stat_table(cases=notif_srag_covid0$frequency, 
                       indic=indicstatus, 
                       status=istatus)
    ROC0$cEVIm_fpr[i] <- 1-stat$spec
    ROC0$cEVIm_tpr[i] <- stat$sens
    ROC0$cEVIm_ppv[i] <- stat$ppv
    ROC0$cEVIm_npv[i] <- stat$npv
    
  }
  
  return(ROC0)
}

ROC_all_cevi_2 <- function(xs,lags,istatus) {
  
  notif_srag_covid0 <- xs
  
  cuts = seq(from = 0.0, to = 1.0, by = 0.01)
  
  ROC0 <- data.frame(cuts)
  ROC0 <- ROC0 %>%
    mutate(EVI_tpr = 0) %>%
    mutate(EVI_fpr = 0) %>%
    mutate(cEVI_tpr = 0) %>%
    mutate(cEVI_fpr = 0) %>%
    mutate(cEVIp_tpr = 0) %>%
    mutate(cEVIp_fpr = 0) %>%
    mutate(cEVIm_tpr = 0) %>%
    mutate(cEVIm_fpr = 0) %>%
    mutate(EVI_ppv = 0) %>%
    mutate(EVI_npv = 0) %>%
    mutate(cEVI_ppv = 0) %>%
    mutate(cEVI_npv = 0) %>%
    mutate(cEVIp_ppv = 0) %>%
    mutate(cEVIp_npv = 0) %>%
    mutate(cEVIm_ppv = 0) %>%
    mutate(cEVIm_npv = 0)
  
  for (i in 1:length(cuts)){
    
    notif_srag_covid0$cEVI <- cevi_func(notif_srag_covid0$frequency,lag=lags$cEVI,cut=cuts[i]) 
    
    #EVI
    minn = min(notif_srag_covid0$EVI,na.rm=TRUE)
    maxx = max(notif_srag_covid0$EVI,na.rm=TRUE)
    cutt = minn + (i-1)*(maxx-minn)/length(cuts)
    indicstatus <- indic_status(cases=notif_srag_covid0$frequency, 
                                lastweekmean=notif_srag_covid0$lastweekmean,
                                ind=notif_srag_covid0$EVI, 
                                method="EVI2", cut=cutt)
    stat <- stat_table(cases=notif_srag_covid0$frequency, 
                       indic=indicstatus, 
                       status=istatus)
    ROC0$EVI_fpr[i] <- 1-stat$spec
    ROC0$EVI_tpr[i] <- stat$sens
    ROC0$EVI_ppv[i] <- stat$ppv
    ROC0$EVI_npv[i] <- stat$npv
    
    #cEVI
    minn = min(notif_srag_covid0$EVI,na.rm=TRUE)
    maxx = max(notif_srag_covid0$EVI,na.rm=TRUE)
    cutt = minn + (i-1)*(maxx-minn)/length(cuts)
    notif_srag_covid0$cEVI <- notif_srag_covid0$cEVI
    indicstatus <- indic_status(cases=notif_srag_covid0$frequency, 
                                lastweekmean=notif_srag_covid0$lastweekmean,
                                ind=notif_srag_covid0$cEVI, 
                                method="cEVI2", cut=cutt)
    stat <- stat_table(cases=notif_srag_covid0$frequency, 
                       indic=indicstatus, 
                       status=istatus)
    ROC0$cEVI_fpr[i] <- 1-stat$spec
    ROC0$cEVI_tpr[i] <- stat$sens
    ROC0$cEVI_ppv[i] <- stat$ppv
    ROC0$cEVI_npv[i] <- stat$npv
    
    #cEVI+
    minn = min(notif_srag_covid0$EVI,na.rm=TRUE)
    maxx = max(notif_srag_covid0$EVI,na.rm=TRUE)
    cutt = minn + (i-1)*(maxx-minn)/length(cuts)
    ev <- notif_srag_covid0$EVI
    cev <- notif_srag_covid0$cEVI
    indicstatus <- indic_status(cases=notif_srag_covid0$frequency, 
                                lastweekmean=notif_srag_covid0$lastweekmean,
                                ind=data.frame(ev,cev), 
                                method="cEVI+2", cut=cutt)
    stat <- stat_table(cases=notif_srag_covid0$frequency, 
                       indic=indicstatus, 
                       status=istatus)
    ROC0$cEVIp_fpr[i] <- 1-stat$spec
    ROC0$cEVIp_tpr[i] <- stat$sens
    ROC0$cEVIp_ppv[i] <- stat$ppv
    ROC0$cEVIp_npv[i] <- stat$npv
    
    #cEVI-
    minn = min(notif_srag_covid0$EVI,na.rm=TRUE)
    maxx = max(notif_srag_covid0$EVI,na.rm=TRUE)
    cutt = minn + (i-1)*(maxx-minn)/length(cuts)
    ev <- notif_srag_covid0$EVI
    cev <- notif_srag_covid0$cEVI
    indicstatus <- indic_status(cases=notif_srag_covid0$frequency, 
                                lastweekmean=notif_srag_covid0$lastweekmean,
                                ind=data.frame(ev,cev), 
                                method="cEVI-2", cut=cutt)
    stat <- stat_table(cases=notif_srag_covid0$frequency, 
                       indic=indicstatus, 
                       status=istatus)
    ROC0$cEVIm_fpr[i] <- 1-stat$spec
    ROC0$cEVIm_tpr[i] <- stat$sens
    ROC0$cEVIm_ppv[i] <- stat$ppv
    ROC0$cEVIm_npv[i] <- stat$npv
    
  }
  
  return(ROC0)
}

table2_build <- function(xs,xs2,xs3,xs4,disease) {
  
  ROCcurve <- xs
  ROCcurve_c <- xs2
  ROCcurve2 <- xs3
  ROCcurve_c2 <- xs4
  
  #Find max Youden values
  CV <- data.frame(1)
  CV$EWS <- "CV"
  CV$Se <- ROCcurve$CV_tpr[which.max(ROCcurve$CV_tpr+(1-ROCcurve$CV_fpr)-1)]
  CV$Sp <- 1-ROCcurve$CV_fpr[which.max(ROCcurve$CV_tpr+(1-ROCcurve$CV_fpr)-1)]
  CV$PPV <- ROCcurve$CV_ppv[which.max(ROCcurve$CV_tpr+(1-ROCcurve$CV_fpr)-1)]
  CV$NPV <- ROCcurve$CV_npv[which.max(ROCcurve$CV_tpr+(1-ROCcurve$CV_fpr)-1)]
  CV$AUC <- AUC(x=c(ROCcurve$CV_fpr,0,1),y=c(ROCcurve$CV_tpr,0,1))
  
  CV2 <- data.frame(1)
  CV2$EWS <- "CV2"
  CV2$Se <- ROCcurve2$CV_tpr[which.max(ROCcurve2$CV_tpr+(1-ROCcurve2$CV_fpr)-1)]
  CV2$Sp <- 1-ROCcurve2$CV_fpr[which.max(ROCcurve2$CV_tpr+(1-ROCcurve2$CV_fpr)-1)]
  CV2$PPV <- ROCcurve2$CV_ppv[which.max(ROCcurve2$CV_tpr+(1-ROCcurve2$CV_fpr)-1)]
  CV2$NPV <- ROCcurve2$CV_npv[which.max(ROCcurve2$CV_tpr+(1-ROCcurve2$CV_fpr)-1)]
  CV2$AUC <- AUC(x=c(ROCcurve2$CV_fpr,0,1),y=c(ROCcurve2$CV_tpr,0,1))
  
  ACF <- data.frame(1)
  ACF$EWS <- "ACF"
  ACF$Se <- ROCcurve$ACF_tpr[which.max(ROCcurve$ACF_tpr+(1-ROCcurve$ACF_fpr)-1)]
  ACF$Sp <- 1-ROCcurve$ACF_fpr[which.max(ROCcurve$ACF_tpr+(1-ROCcurve$ACF_fpr)-1)]
  ACF$PPV <- ROCcurve$ACF_ppv[which.max(ROCcurve$ACF_tpr+(1-ROCcurve$ACF_fpr)-1)]
  ACF$NPV <- ROCcurve$ACF_npv[which.max(ROCcurve$ACF_tpr+(1-ROCcurve$ACF_fpr)-1)]
  ACF$AUC <- AUC(x=c(ROCcurve$ACF_fpr,0,1),y=c(ROCcurve$ACF_tpr,0,1))
  
  ACF2 <- data.frame(1)
  ACF2$EWS <- "ACF2"
  ACF2$Se <- ROCcurve2$ACF_tpr[which.max(ROCcurve2$ACF_tpr+(1-ROCcurve2$ACF_fpr)-1)]
  ACF2$Sp <- 1-ROCcurve2$ACF_fpr[which.max(ROCcurve2$ACF_tpr+(1-ROCcurve2$ACF_fpr)-1)]
  ACF2$PPV <- ROCcurve2$ACF_ppv[which.max(ROCcurve2$ACF_tpr+(1-ROCcurve2$ACF_fpr)-1)]
  ACF2$NPV <- ROCcurve2$ACF_npv[which.max(ROCcurve2$ACF_tpr+(1-ROCcurve2$ACF_fpr)-1)]
  ACF2$AUC <- AUC(x=c(ROCcurve2$ACF_fpr,0,1),y=c(ROCcurve2$ACF_tpr,0,1))
  
  EE <- data.frame(1)
  EE$EWS <- "EE"
  EE$Se <- ROCcurve$EE_tpr[which.max(ROCcurve$EE_tpr+(1-ROCcurve$EE_fpr)-1)]
  EE$Sp <- 1-ROCcurve$EE_fpr[which.max(ROCcurve$EE_tpr+(1-ROCcurve$EE_fpr)-1)]
  EE$PPV <- ROCcurve$EE_ppv[which.max(ROCcurve$EE_tpr+(1-ROCcurve$EE_fpr)-1)]
  EE$NPV <- ROCcurve$EE_npv[which.max(ROCcurve$EE_tpr+(1-ROCcurve$EE_fpr)-1)]
  EE$AUC <- AUC(x=c(ROCcurve$EE_fpr,0,1),y=c(ROCcurve$EE_tpr,0,1))
  
  EE2 <- data.frame(1)
  EE2$EWS <- "EE2"
  EE2$Se <- ROCcurve2$EE_tpr[which.max(ROCcurve2$EE_tpr+(1-ROCcurve2$EE_fpr)-1)]
  EE2$Sp <- 1-ROCcurve2$EE_fpr[which.max(ROCcurve2$EE_tpr+(1-ROCcurve2$EE_fpr)-1)]
  EE2$PPV <- ROCcurve2$EE_ppv[which.max(ROCcurve2$EE_tpr+(1-ROCcurve2$EE_fpr)-1)]
  EE2$NPV <- ROCcurve2$EE_npv[which.max(ROCcurve2$EE_tpr+(1-ROCcurve2$EE_fpr)-1)]
  EE2$AUC <- AUC(x=c(ROCcurve2$EE_fpr,0,1),y=c(ROCcurve2$EE_tpr,0,1))
  
  SKEW <- data.frame(1)
  SKEW$EWS <- "SKEW"
  SKEW$Se <- ROCcurve$SKEW_tpr[which.max(ROCcurve$SKEW_tpr+(1-ROCcurve$SKEW_fpr)-1)]
  SKEW$Sp <- 1-ROCcurve$SKEW_fpr[which.max(ROCcurve$SKEW_tpr+(1-ROCcurve$SKEW_fpr)-1)]
  SKEW$PPV <- ROCcurve$SKEW_ppv[which.max(ROCcurve$SKEW_tpr+(1-ROCcurve$SKEW_fpr)-1)]
  SKEW$NPV <- ROCcurve$SKEW_npv[which.max(ROCcurve$SKEW_tpr+(1-ROCcurve$SKEW_fpr)-1)]
  SKEW$AUC <- AUC(x=c(ROCcurve$SKEW_fpr,0,1),y=c(ROCcurve$SKEW_tpr,0,1))
  
  SKEW2 <- data.frame(1)
  SKEW2$EWS <- "SKEW2"
  SKEW2$Se <- ROCcurve2$SKEW_tpr[which.max(ROCcurve2$SKEW_tpr+(1-ROCcurve2$SKEW_fpr)-1)]
  SKEW2$Sp <- 1-ROCcurve2$SKEW_fpr[which.max(ROCcurve2$SKEW_tpr+(1-ROCcurve2$SKEW_fpr)-1)]
  SKEW2$PPV <- ROCcurve2$SKEW_ppv[which.max(ROCcurve2$SKEW_tpr+(1-ROCcurve2$SKEW_fpr)-1)]
  SKEW2$NPV <- ROCcurve2$SKEW_npv[which.max(ROCcurve2$SKEW_tpr+(1-ROCcurve2$SKEW_fpr)-1)]
  SKEW2$AUC <- AUC(x=c(ROCcurve2$SKEW_fpr,0,1),y=c(ROCcurve2$SKEW_tpr,0,1))
  
  KURT <- data.frame(1)
  KURT$EWS <- "KURT"
  KURT$Se <- ROCcurve$KURT_tpr[which.max(ROCcurve$KURT_tpr+(1-ROCcurve$KURT_fpr)-1)]
  KURT$Sp <- 1-ROCcurve$KURT_fpr[which.max(ROCcurve$KURT_tpr+(1-ROCcurve$KURT_fpr)-1)]
  KURT$PPV <- ROCcurve$KURT_ppv[which.max(ROCcurve$KURT_tpr+(1-ROCcurve$KURT_fpr)-1)]
  KURT$NPV <- ROCcurve$KURT_npv[which.max(ROCcurve$KURT_tpr+(1-ROCcurve$KURT_fpr)-1)]
  KURT$AUC <- AUC(x=c(ROCcurve$KURT_fpr,0,1),y=c(ROCcurve$KURT_tpr,0,1))
  
  KURT2 <- data.frame(1)
  KURT2$EWS <- "KURT2"
  KURT2$Se <- ROCcurve2$KURT_tpr[which.max(ROCcurve2$KURT_tpr+(1-ROCcurve2$KURT_fpr)-1)]
  KURT2$Sp <- 1-ROCcurve2$KURT_fpr[which.max(ROCcurve2$KURT_tpr+(1-ROCcurve2$KURT_fpr)-1)]
  KURT2$PPV <- ROCcurve2$KURT_ppv[which.max(ROCcurve2$KURT_tpr+(1-ROCcurve2$KURT_fpr)-1)]
  KURT2$NPV <- ROCcurve2$KURT_npv[which.max(ROCcurve2$KURT_tpr+(1-ROCcurve2$KURT_fpr)-1)]
  KURT2$AUC <- AUC(x=c(ROCcurve2$KURT_fpr,0,1),y=c(ROCcurve2$KURT_tpr,0,1))
  
  ID <- data.frame(1)
  ID$EWS <- "ID"
  ID$Se <- ROCcurve$ID_tpr[which.max(ROCcurve$ID_tpr+(1-ROCcurve$ID_fpr)-1)]
  ID$Sp <- 1-ROCcurve$ID_fpr[which.max(ROCcurve$ID_tpr+(1-ROCcurve$ID_fpr)-1)]
  ID$PPV <- ROCcurve$ID_ppv[which.max(ROCcurve$ID_tpr+(1-ROCcurve$ID_fpr)-1)]
  ID$NPV <- ROCcurve$ID_npv[which.max(ROCcurve$ID_tpr+(1-ROCcurve$ID_fpr)-1)]
  ID$AUC <- AUC(x=c(ROCcurve$ID_fpr,0,1),y=c(ROCcurve$ID_tpr,0,1))
  
  ID2 <- data.frame(1)
  ID2$EWS <- "ID2"
  ID2$Se <- ROCcurve2$ID_tpr[which.max(ROCcurve2$ID_tpr+(1-ROCcurve2$ID_fpr)-1)]
  ID2$Sp <- 1-ROCcurve2$ID_fpr[which.max(ROCcurve2$ID_tpr+(1-ROCcurve2$ID_fpr)-1)]
  ID2$PPV <- ROCcurve2$ID_ppv[which.max(ROCcurve2$ID_tpr+(1-ROCcurve2$ID_fpr)-1)]
  ID2$NPV <- ROCcurve2$ID_npv[which.max(ROCcurve2$ID_tpr+(1-ROCcurve2$ID_fpr)-1)]
  ID2$AUC <- AUC(x=c(ROCcurve2$ID_fpr,0,1),y=c(ROCcurve2$ID_tpr,0,1))
  
  PCA1 <- data.frame(1)
  PCA1$EWS <- "PCA1"
  PCA1$Se <- ROCcurve$PCA1_tpr[which.max(ROCcurve$PCA1_tpr+(1-ROCcurve$PCA1_fpr)-1)]
  PCA1$Sp <- 1-ROCcurve$PCA1_fpr[which.max(ROCcurve$PCA1_tpr+(1-ROCcurve$PCA1_fpr)-1)]
  PCA1$PPV <- ROCcurve$PCA1_ppv[which.max(ROCcurve$PCA1_tpr+(1-ROCcurve$PCA1_fpr)-1)]
  PCA1$NPV <- ROCcurve$PCA1_npv[which.max(ROCcurve$PCA1_tpr+(1-ROCcurve$PCA1_fpr)-1)]
  PCA1$AUC <- AUC(x=c(ROCcurve$PCA1_fpr,0,1),y=c(ROCcurve$PCA1_tpr,0,1))
  
  PCA1_2 <- data.frame(1)
  PCA1_2$EWS <- "PCA1-2"
  PCA1_2$Se <- ROCcurve2$PCA1_tpr[which.max(ROCcurve2$PCA1_tpr+(1-ROCcurve2$PCA1_fpr)-1)]
  PCA1_2$Sp <- 1-ROCcurve2$PCA1_fpr[which.max(ROCcurve2$PCA1_tpr+(1-ROCcurve2$PCA1_fpr)-1)]
  PCA1_2$PPV <- ROCcurve2$PCA1_ppv[which.max(ROCcurve2$PCA1_tpr+(1-ROCcurve2$PCA1_fpr)-1)]
  PCA1_2$NPV <- ROCcurve2$PCA1_npv[which.max(ROCcurve2$PCA1_tpr+(1-ROCcurve2$PCA1_fpr)-1)]
  PCA1_2$AUC <- AUC(x=c(ROCcurve2$PCA1_fpr,0,1),y=c(ROCcurve2$PCA1_tpr,0,1))
  
  PCA12 <- data.frame(1)
  PCA12$EWS <- "PCA12"
  PCA12$Se <- ROCcurve$PCA12_tpr[which.max(ROCcurve$PCA12_tpr+(1-ROCcurve$PCA12_fpr)-1)]
  PCA12$Sp <- 1-ROCcurve$PCA12_fpr[which.max(ROCcurve$PCA12_tpr+(1-ROCcurve$PCA12_fpr)-1)]
  PCA12$PPV <- ROCcurve$PCA12_ppv[which.max(ROCcurve$PCA12_tpr+(1-ROCcurve$PCA12_fpr)-1)]
  PCA12$NPV <- ROCcurve$PCA12_npv[which.max(ROCcurve$PCA12_tpr+(1-ROCcurve$PCA12_fpr)-1)]
  PCA12$AUC <- AUC(x=c(ROCcurve$PCA12_fpr,0,1),y=c(ROCcurve$PCA12_tpr,0,1))
  
  PCA12_2 <- data.frame(1)
  PCA12_2$EWS <- "PCA12-2"
  PCA12_2$Se <- ROCcurve2$PCA12_tpr[which.max(ROCcurve2$PCA12_tpr+(1-ROCcurve2$PCA12_fpr)-1)]
  PCA12_2$Sp <- 1-ROCcurve2$PCA12_fpr[which.max(ROCcurve2$PCA12_tpr+(1-ROCcurve2$PCA12_fpr)-1)]
  PCA12_2$PPV <- ROCcurve2$PCA12_ppv[which.max(ROCcurve2$PCA12_tpr+(1-ROCcurve2$PCA12_fpr)-1)]
  PCA12_2$NPV <- ROCcurve2$PCA12_npv[which.max(ROCcurve2$PCA12_tpr+(1-ROCcurve2$PCA12_fpr)-1)]
  PCA12_2$AUC <- AUC(x=c(ROCcurve2$PCA12_fpr,0,1),y=c(ROCcurve2$PCA12_tpr,0,1))
  
  PCA123 <- data.frame(1)
  PCA123$EWS <- "PCA123"
  PCA123$Se <- ROCcurve$PCA123_tpr[which.max(ROCcurve$PCA123_tpr+(1-ROCcurve$PCA123_fpr)-1)]
  PCA123$Sp <- 1-ROCcurve$PCA123_fpr[which.max(ROCcurve$PCA123_tpr+(1-ROCcurve$PCA123_fpr)-1)]
  PCA123$PPV <- ROCcurve$PCA123_ppv[which.max(ROCcurve$PCA123_tpr+(1-ROCcurve$PCA123_fpr)-1)]
  PCA123$NPV <- ROCcurve$PCA123_npv[which.max(ROCcurve$PCA123_tpr+(1-ROCcurve$PCA123_fpr)-1)]
  PCA123$AUC <- AUC(x=c(ROCcurve$PCA123_fpr,0,1),y=c(ROCcurve$PCA123_tpr,0,1))
  
  EVI <- data.frame(1)
  EVI$EWS <- "EVI"
  EVI$Se <- ROCcurve_c$EVI_tpr[which.max(ROCcurve_c$EVI_tpr+(1-ROCcurve_c$EVI_fpr)-1)]
  EVI$Sp <- 1-ROCcurve_c$EVI_fpr[which.max(ROCcurve_c$EVI_tpr+(1-ROCcurve_c$EVI_fpr)-1)]
  EVI$PPV <- ROCcurve_c$EVI_ppv[which.max(ROCcurve_c$EVI_tpr+(1-ROCcurve_c$EVI_fpr)-1)]
  EVI$NPV <- ROCcurve_c$EVI_npv[which.max(ROCcurve_c$EVI_tpr+(1-ROCcurve_c$EVI_fpr)-1)]
  EVI$AUC <- AUC(x=c(ROCcurve_c$EVI_fpr,0,1),y=c(ROCcurve_c$EVI_tpr,0,1))
  
  cEVI <- data.frame(1)
  cEVI$EWS <- "cEVI"
  cEVI$Se <- ROCcurve_c$cEVI_tpr[which.max(ROCcurve_c$cEVI_tpr+(1-ROCcurve_c$cEVI_fpr)-1)]
  cEVI$Sp <- 1-ROCcurve_c$cEVI_fpr[which.max(ROCcurve_c$cEVI_tpr+(1-ROCcurve_c$cEVI_fpr)-1)]
  cEVI$PPV <- ROCcurve_c$cEVI_ppv[which.max(ROCcurve_c$cEVI_tpr+(1-ROCcurve_c$cEVI_fpr)-1)]
  cEVI$NPV <- ROCcurve_c$cEVI_npv[which.max(ROCcurve_c$cEVI_tpr+(1-ROCcurve_c$cEVI_fpr)-1)]
  cEVI$AUC <- AUC(x=c(ROCcurve_c$cEVI_fpr,0,1),y=c(ROCcurve_c$cEVI_tpr,0,1))
  
  cEVIm <- data.frame(1)
  cEVIm$EWS <- "cEVI-"
  cEVIm$Se <- ROCcurve_c$cEVIm_tpr[which.max(ROCcurve_c$cEVIm_tpr+(1-ROCcurve_c$cEVIm_fpr)-1)]
  cEVIm$Sp <- 1-ROCcurve_c$cEVIm_fpr[which.max(ROCcurve_c$cEVIm_tpr+(1-ROCcurve_c$cEVIm_fpr)-1)]
  cEVIm$PPV <- ROCcurve_c$cEVIm_ppv[which.max(ROCcurve_c$cEVIm_tpr+(1-ROCcurve_c$cEVIm_fpr)-1)]
  cEVIm$NPV <- ROCcurve_c$cEVIm_npv[which.max(ROCcurve_c$cEVIm_tpr+(1-ROCcurve_c$cEVIm_fpr)-1)]
  cEVIm$AUC <- AUC(x=c(ROCcurve_c$cEVIm_fpr,0,1),y=c(ROCcurve_c$cEVIm_tpr,0,1))
  
  cEVIp <- data.frame(1)
  cEVIp$EWS <- "cEVI+"
  cEVIp$Se <- ROCcurve_c$cEVIp_tpr[which.max(ROCcurve_c$cEVIp_tpr+(1-ROCcurve_c$cEVIp_fpr)-1)]
  cEVIp$Sp <- 1-ROCcurve_c$cEVIp_fpr[which.max(ROCcurve_c$cEVIp_tpr+(1-ROCcurve_c$cEVIp_fpr)-1)]
  cEVIp$PPV <- ROCcurve_c$cEVIp_ppv[which.max(ROCcurve_c$cEVIp_tpr+(1-ROCcurve_c$cEVIp_fpr)-1)]
  cEVIp$NPV <- ROCcurve_c$cEVIp_npv[which.max(ROCcurve_c$cEVIp_tpr+(1-ROCcurve_c$cEVIp_fpr)-1)]
  cEVIp$AUC <- AUC(x=c(ROCcurve_c$cEVIp_fpr,0,1),y=c(ROCcurve_c$cEVIp_tpr,0,1))
  
  EVI2 <- data.frame(1)
  EVI2$EWS <- "EVI2"
  EVI2$Se <- ROCcurve_c2$EVI_tpr[which.max(ROCcurve_c2$EVI_tpr+(1-ROCcurve_c2$EVI_fpr)-1)]
  EVI2$Sp <- 1-ROCcurve_c2$EVI_fpr[which.max(ROCcurve_c2$EVI_tpr+(1-ROCcurve_c2$EVI_fpr)-1)]
  EVI2$PPV <- ROCcurve_c2$EVI_ppv[which.max(ROCcurve_c2$EVI_tpr+(1-ROCcurve_c2$EVI_fpr)-1)]
  EVI2$NPV <- ROCcurve_c2$EVI_npv[which.max(ROCcurve_c2$EVI_tpr+(1-ROCcurve_c2$EVI_fpr)-1)]
  EVI2$AUC <- AUC(x=c(ROCcurve_c2$EVI_fpr,0,1),y=c(ROCcurve_c2$EVI_tpr,0,1))
  
  cEVI2 <- data.frame(1)
  cEVI2$EWS <- "cEVI2"
  cEVI2$Se <- ROCcurve_c2$cEVI_tpr[which.max(ROCcurve_c2$cEVI_tpr+(1-ROCcurve_c2$cEVI_fpr)-1)]
  cEVI2$Sp <- 1-ROCcurve_c2$cEVI_fpr[which.max(ROCcurve_c2$cEVI_tpr+(1-ROCcurve_c2$cEVI_fpr)-1)]
  cEVI2$PPV <- ROCcurve_c2$cEVI_ppv[which.max(ROCcurve_c2$cEVI_tpr+(1-ROCcurve_c2$cEVI_fpr)-1)]
  cEVI2$NPV <- ROCcurve_c2$cEVI_npv[which.max(ROCcurve_c2$cEVI_tpr+(1-ROCcurve_c2$cEVI_fpr)-1)]
  cEVI2$AUC <- AUC(x=c(ROCcurve_c2$cEVI_fpr,0,1),y=c(ROCcurve_c2$cEVI_tpr,0,1))
  
  cEVIm2 <- data.frame(1)
  cEVIm2$EWS <- "cEVI-2"
  cEVIm2$Se <- ROCcurve_c2$cEVIm_tpr[which.max(ROCcurve_c2$cEVIm_tpr+(1-ROCcurve_c2$cEVIm_fpr)-1)]
  cEVIm2$Sp <- 1-ROCcurve_c2$cEVIm_fpr[which.max(ROCcurve_c2$cEVIm_tpr+(1-ROCcurve_c2$cEVIm_fpr)-1)]
  cEVIm2$PPV <- ROCcurve_c2$cEVIm_ppv[which.max(ROCcurve_c2$cEVIm_tpr+(1-ROCcurve_c2$cEVIm_fpr)-1)]
  cEVIm2$NPV <- ROCcurve_c2$cEVIm_npv[which.max(ROCcurve_c2$cEVIm_tpr+(1-ROCcurve_c2$cEVIm_fpr)-1)]
  cEVIm2$AUC <- AUC(x=c(ROCcurve_c2$cEVIm_fpr,0,1),y=c(ROCcurve_c2$cEVIm_tpr,0,1))
  
  cEVIp2 <- data.frame(1)
  cEVIp2$EWS <- "cEVI+2"
  cEVIp2$Se <- ROCcurve_c2$cEVIp_tpr[which.max(ROCcurve_c2$cEVIp_tpr+(1-ROCcurve_c2$cEVIp_fpr)-1)]
  cEVIp2$Sp <- 1-ROCcurve_c2$cEVIp_fpr[which.max(ROCcurve_c2$cEVIp_tpr+(1-ROCcurve_c2$cEVIp_fpr)-1)]
  cEVIp2$PPV <- ROCcurve_c2$cEVIp_ppv[which.max(ROCcurve_c2$cEVIp_tpr+(1-ROCcurve_c2$cEVIp_fpr)-1)]
  cEVIp2$NPV <- ROCcurve_c2$cEVIp_npv[which.max(ROCcurve_c2$cEVIp_tpr+(1-ROCcurve_c2$cEVIp_fpr)-1)]
  cEVIp2$AUC <- AUC(x=c(ROCcurve_c2$cEVIp_fpr,0,1),y=c(ROCcurve_c2$cEVIp_tpr,0,1))
  
  table1 <- rbind(CV,CV2,EE,EE2,SKEW,SKEW2,KURT,KURT2,ID,ID2,
                  ACF,ACF2,PCA1,PCA1_2,PCA12,PCA12_2,
                  EVI,EVI2,cEVI,cEVI2,cEVIm,cEVIm2,cEVIp,cEVIp2)
  
  write.table(table1,c(paste("./table1_",disease,".csv",sep="")), row.names = FALSE, sep=";")
  
  return(table1)
}

