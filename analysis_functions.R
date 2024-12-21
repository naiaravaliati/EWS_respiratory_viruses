source("indicators.R")
source("statistics_custom.R")

notif_analysis <- function(name) {
  
  if (!exists('dados_srag')){
  dados_srag_2009 <- read_csv2("././INFLUD09.csv",col_names = TRUE)
  dados_srag_2010 <- read_csv2("././INFLUD10.csv",col_names = TRUE)
  dados_srag_2011 <- read_csv2("././INFLUD11.csv",col_names = TRUE)
  dados_srag_2012 <- read_csv2("././INFLUD12.csv",col_names = TRUE)
  dados_srag_2013 <- read_csv2("././INFLUD13.csv",col_names = TRUE)
  dados_srag_2014 <- read_csv2("././INFLUD14.csv",col_names = TRUE)
  dados_srag_2015 <- read_csv2("././INFLUD15.csv",col_names = TRUE)
  dados_srag_2016 <- read_csv2("././INFLUD16.csv",col_names = TRUE)
  dados_srag_2017 <- read_csv2("././INFLUD17.csv",col_names = TRUE)
  dados_srag_2018 <- read_csv2("././INFLUD18.csv",col_names = TRUE)
  dados_srag_2019 <- read_csv2("././INFLUD19.csv",col_names = TRUE)
  dados_srag_2020 <- read_csv2("././INFLUD20.csv",col_names = TRUE)
  dados_srag_2021 <- read_csv2("././INFLUD21.csv",col_names = TRUE)
  dados_srag_2022 <- read_csv2("././INFLUD22.csv",col_names = TRUE)
  dados_srag_2023 <- read_csv2("././INFLUD23.csv",col_names = TRUE)
  dados_srag_2024 <- read_csv2("././INFLUD24.csv",col_names = TRUE)
  
  dados_srag_2009 <- dados_srag_2009 %>% dplyr::select(DT_NOTIFIC,DT_SIN_PRI,CLASSI_FIN) 
  dados_srag_2010 <- dados_srag_2010 %>% dplyr::select(DT_NOTIFIC,DT_SIN_PRI,CLASSI_FIN) 
  dados_srag_2011 <- dados_srag_2011 %>% dplyr::select(DT_NOTIFIC,DT_SIN_PRI,CLASSI_FIN) 
  dados_srag_2012 <- dados_srag_2012 %>% dplyr::select(DT_NOTIFIC,DT_SIN_PRI,CLASSI_FIN) 
  dados_srag_2013 <- dados_srag_2013 %>% dplyr::select(DT_NOTIFIC,DT_SIN_PRI,CLASSI_FIN) 
  dados_srag_2014 <- dados_srag_2014 %>% dplyr::select(DT_NOTIFIC,DT_SIN_PRI,CLASSI_FIN) 
  dados_srag_2015 <- dados_srag_2015 %>% dplyr::select(DT_NOTIFIC,DT_SIN_PRI,CLASSI_FIN) 
  dados_srag_2016 <- dados_srag_2016 %>% dplyr::select(DT_NOTIFIC,DT_SIN_PRI,CLASSI_FIN) 
  dados_srag_2017 <- dados_srag_2017 %>% dplyr::select(DT_NOTIFIC,DT_SIN_PRI,CLASSI_FIN) 
  dados_srag_2018 <- dados_srag_2018 %>% dplyr::select(DT_NOTIFIC,DT_SIN_PRI,CLASSI_FIN) 
  dados_srag_2019 <- dados_srag_2019 %>% dplyr::select(DT_NOTIFIC,DT_SIN_PRI,CLASSI_FIN) 
  dados_srag_2020 <- dados_srag_2020 %>% dplyr::select(DT_NOTIFIC,DT_SIN_PRI,CLASSI_FIN) 
  dados_srag_2021 <- dados_srag_2021 %>% dplyr::select(DT_NOTIFIC,DT_SIN_PRI,CLASSI_FIN)
  dados_srag_2022 <- dados_srag_2022 %>% dplyr::select(DT_NOTIFIC,DT_SIN_PRI,CLASSI_FIN)
  dados_srag_2023 <- dados_srag_2023 %>% dplyr::select(DT_NOTIFIC,DT_SIN_PRI,CLASSI_FIN)
  dados_srag_2024 <- dados_srag_2024 %>% dplyr::select(DT_NOTIFIC,DT_SIN_PRI,CLASSI_FIN)
  
  dados_srag <- rbind(dados_srag_2009,dados_srag_2010,dados_srag_2011,dados_srag_2012,
                      dados_srag_2013,dados_srag_2014,dados_srag_2015,dados_srag_2016,
                      dados_srag_2017,dados_srag_2018,dados_srag_2019,dados_srag_2020,
                      dados_srag_2021,dados_srag_2022,dados_srag_2023,dados_srag_2024)
  
  save(dados_srag,file="srag_data_2009_2024.RData")
  }
  
  #Filtrate COVID-19
  dados_srag_covid <- dados_srag %>% filter(CLASSI_FIN==5) 
  
  #Filtrate Influenza
  dados_srag_influenza <- dados_srag %>% filter(CLASSI_FIN==1) 
  
  #Filtrate Other Respiratory Viruses
  dados_srag_outro <- dados_srag %>% filter(CLASSI_FIN==2) 
  
  rm(dados_srag_2009,dados_srag_2010,dados_srag_2011,dados_srag_2012,
     dados_srag_2013,dados_srag_2014,dados_srag_2015,dados_srag_2016,
     dados_srag_2017,dados_srag_2018,dados_srag_2019,dados_srag_2020,
     dados_srag_2021,dados_srag_2022,dados_srag_2023,dados_srag_2024)
  
  dados_srag_covid$date <- format(as.Date(dados_srag_covid$DT_SIN_PRI, format="%d/%m/%Y"), "%Y-%m-%d")
  dados_srag_influenza$date <- format(as.Date(dados_srag_influenza$DT_SIN_PRI, format="%d/%m/%Y"), "%Y-%m-%d")
  dados_srag_outro$date <- format(as.Date(dados_srag_outro$DT_SIN_PRI, format="%d/%m/%Y"), "%Y-%m-%d")
  
  #Filter the dates for the study
  dados_srag_covid <- dados_srag_covid %>% filter(date<"2024-09-15")
  dados_srag_influenza <- dados_srag_influenza %>% filter(date<"2024-09-15")
  dados_srag_outro <- dados_srag_outro %>% filter(date<"2024-09-15")
  
  #Calculate notifications per day - COVID
  notif_srag <- dados_srag_covid %>% group_by(date) %>% summarise(frequency = n(), .groups = 'drop')
  notif_srag <- notif_srag %>% 
    mutate(date = as.Date(date)) %>%
    mutate(year = year(date)) %>%
    mutate(yearweek = yearweek(date)) %>%
    mutate(yearmonth = yearmonth(date)) %>%
    mutate(week = week(date)) %>%
    mutate(time = year + (week-1)/52.0) %>%
    filter(date >= as.Date("2020-01-01")) %>%
    mutate(freqsum = cumsum(frequency))
  notif_srag_covid <- notif_srag
  
  #Calculate notifications per day - Influenza
  notif_srag_influenza <- dados_srag_influenza %>% group_by(date) %>% summarise(frequency = n(), .groups = 'drop')
  notif_srag_influenza <- notif_srag_influenza %>% 
    mutate(date = as.Date(date)) %>%
    mutate(year = year(date)) %>%
    mutate(yearweek = yearweek(date)) %>%
    mutate(yearmonth = yearmonth(date)) %>%
    mutate(week = week(date)) %>%
    mutate(time = year + (week-1)/52.0) %>%
    mutate(freqsum = cumsum(frequency))
  
  #Calculate notifications per day - ORVs
  notif_srag_outro <- dados_srag_outro %>% group_by(date) %>% summarise(frequency = n(), .groups = 'drop')
  notif_srag_outro <- notif_srag_outro %>% 
    mutate(date = as.Date(date)) %>%
    mutate(year = year(date)) %>%
    mutate(yearweek = yearweek(date)) %>%
    mutate(yearmonth = yearmonth(date)) %>%
    mutate(week = week(date)) %>%
    mutate(time = year + (week-1)/52.0) %>%
    mutate(freqsum = cumsum(frequency))
  
  #Check number of cumulative number of cases at the end of series
  tail(notif_srag_covid$freqsum,1)
  tail(notif_srag_influenza$freqsum,1)
  tail(notif_srag_outro$freqsum,1)
  
  #Apply virus names
  notif_srag_covid$virus <- "SARS-CoV-2"
  notif_srag_influenza$virus <- "Influenza"
  notif_srag_outro$virus <- "ORVs"
  
  #Calculate EWS and ITI
  notif_srag_covid <- calc_all_ews(notif_srag_covid,lags)
  notif_srag_covid <- calc_all_status(notif_srag_covid,lags)
  
  notif_srag_influenza <- calc_all_ews(notif_srag_influenza,lags)
  notif_srag_influenza <- calc_all_status(notif_srag_influenza,lags)
  
  notif_srag_outro <- calc_all_ews(notif_srag_outro,lags)
  notif_srag_outro <- calc_all_status(notif_srag_outro,lags)
  
  #Mean frequency
  notif_srag_covid <- notif_srag_covid %>%
    mutate(freqmean = rollapply(frequency,width=14,FUN=mean,fill=0,align='right')) %>%
    mutate(freqmean2 = rollapply(frequency,width=28,FUN=mean,fill=0,align='right'))
  notif_srag_influenza <- notif_srag_influenza %>%
    mutate(freqmean = rollapply(frequency,width=14,FUN=mean,fill=0,align='right')) %>%
    mutate(freqmean2 = rollapply(frequency,width=28,FUN=mean,fill=0,align='right'))
  notif_srag_outro <- notif_srag_outro %>%
    mutate(freqmean = rollapply(frequency,width=14,FUN=mean,fill=0,align='right')) %>%
    mutate(freqmean2 = rollapply(frequency,width=28,FUN=mean,fill=0,align='right'))
  
  #Get viruses together
  notif_srag_all <- rbind(notif_srag_covid,notif_srag_influenza,notif_srag_outro)
 
  #Save data
  write.table(notif_srag_covid,c(paste("./notif_srag_covid_",disease_name,".csv",sep="")), row.names = FALSE, sep=";")
  write.table(notif_srag_influenza,c(paste("./notif_srag_influenza_",disease_name,".csv",sep="")), row.names = FALSE, sep=";")
  write.table(notif_srag_outro,c(paste("./notif_srag_outro_",disease_name,".csv",sep="")), row.names = FALSE, sep=";")
  write.table(notif_srag_all,c(paste("./notif_srag_all_",disease_name,".csv",sep="")), row.names = FALSE, sep=";")
  
  dproc1 <- notif_srag_all  %>%
    group_by(yearmonth, virus) %>%
    summarise(n = mean(freqmean)) %>%
    mutate(percentage = n / sum(n))
  
  dproc1 <- dproc1 %>%
    ungroup() %>%
    drop_na() %>%
    complete(virus, yearmonth, fill = list(`n()` = 0))
  
  dproc1[is.na(dproc1)] <- 0
  
  #Grafico de proporcao por regiao
  dproc1 %>%
    ggplot(aes(x=yearmonth, y=percentage, fill = virus)) + 
    geom_area(alpha=0.75, linewidth=0.5, colour="black") +
    theme_bw(base_size = 12) +
    scale_fill_manual(values = cbbPalette) + 
    xlab("Date (Year Month)") +
    ylab("Proportion of cases") + ylim(c(0.0,1.0)) + 
    labs(fill = "Virus") +
    theme(legend.position="right",
          axis.text=element_text(size=22),
          legend.text=element_text(size=22),
          axis.title=element_text(size=22,face="bold") ) +
    coord_cartesian(expand = FALSE) -> gproportion
  gproportion
  gname <- c(paste("Figures/Proportion_all_",name,"_week.png",sep=""))
  ggsave(gname, plot = gproportion, dpi = 300, width=18, height=7)
  
  #EWSs
  notif_srag <- notif_srag %>%
    mutate(freqmean = rollapply(frequency,width=14,FUN=mean,fill=0,align='right')) %>%
    mutate(lastweekmean = rollapply(frequency,width=7,FUN=mean,fill=0,align='right')) %>%
    mutate(EE = rollapply(freqmean,width=14,FUN=empirical_entropy,fill=0,align='right')) %>%
    mutate(CV = rollapply(freqmean,width=14,FUN=coefficient_of_variation,fill=0,align='right')) %>%
    mutate(SKEW = rollapply(freqmean,width=14,FUN=skewness,fill=0,align='right')) %>%
    mutate(KURT = rollapply(freqmean,width=14,FUN=kurtosis,fill=0,align='right')) %>%
    mutate(ID = rollapply(freqmean,width=14,FUN=index_of_dispersion,fill=0,align='right')) %>%
    mutate(KS = rollapply(freqmean,width=14,FUN=kolmogorov_smirnov,fill=0,align='right')) %>%
    mutate(ACF = rollapply(freqmean,width=14,FUN=acf1,fill=0,align='right'))
  
  notif_srag_influenza <- notif_srag_influenza %>%
    mutate(freqmean = rollapply(frequency,width=14,FUN=mean,fill=0,align='right')) %>%
    mutate(lastweekmean = rollapply(frequency,width=7,FUN=mean,fill=0,align='right')) %>%
    mutate(EE = rollapply(freqmean,width=14,FUN=empirical_entropy,fill=0,align='right')) %>%
    mutate(CV = rollapply(freqmean,width=14,FUN=coefficient_of_variation,fill=0,align='right')) %>%
    mutate(SKEW = rollapply(freqmean,width=14,FUN=skewness,fill=0,align='right')) %>%
    mutate(KURT = rollapply(freqmean,width=14,FUN=kurtosis,fill=0,align='right')) %>%
    mutate(ID = rollapply(freqmean,width=14,FUN=index_of_dispersion,fill=0,align='right')) %>%
    mutate(KS = rollapply(freqmean,width=14,FUN=kolmogorov_smirnov,fill=0,align='right')) %>%
    mutate(ACF = rollapply(freqmean,width=14,FUN=acf1,fill=0,align='right'))
  
  notif_srag_outro <- notif_srag_outro %>%
    mutate(freqmean = rollapply(frequency,width=14,FUN=mean,fill=0,align='right')) %>%
    mutate(lastweekmean = rollapply(frequency,width=7,FUN=mean,fill=0,align='right')) %>%
    mutate(EE = rollapply(freqmean,width=14,FUN=empirical_entropy,fill=0,align='right')) %>%
    mutate(CV = rollapply(freqmean,width=14,FUN=coefficient_of_variation,fill=0,align='right')) %>%
    mutate(SKEW = rollapply(freqmean,width=14,FUN=skewness,fill=0,align='right')) %>%
    mutate(KURT = rollapply(freqmean,width=14,FUN=kurtosis,fill=0,align='right')) %>%
    mutate(ID = rollapply(freqmean,width=14,FUN=index_of_dispersion,fill=0,align='right')) %>%
    mutate(KS = rollapply(freqmean,width=14,FUN=kolmogorov_smirnov,fill=0,align='right')) %>%
    mutate(ACF = rollapply(freqmean,width=14,FUN=acf1,fill=0,align='right'))
    
  #Principal Component Analysis
  PCA.analysis <- princomp(scale(notif_srag[,c("EE","CV","SKEW","KURT","ID","KS")]))
  notif_srag$waku1 <- PCA.analysis$scores[,1]
  notif_srag$waku12 <- PCA.analysis$scores[,1]+PCA.analysis$scores[,2]
  notif_srag$waku123 <- PCA.analysis$scores[,1]+PCA.analysis$scores[,2]+PCA.analysis$scores[,3]
  
  PCA.analysis <- princomp(scale(notif_srag[,c("EE","CV","SKEW","KURT","ID","ACF")]))
  notif_srag$PCA1 <- PCA.analysis$scores[,1]
  notif_srag$PCA12 <- PCA.analysis$scores[,1]+PCA.analysis$scores[,2]
  notif_srag$PCA123 <- PCA.analysis$scores[,1]+PCA.analysis$scores[,2]+PCA.analysis$scores[,3]
  
  loadings <- PCA.analysis$loadings[,1]
  
  PCA.analysis_influenza <- princomp(scale(notif_srag_influenza[,c("EE","CV","SKEW","KURT","ID","ACF")]))
  notif_srag_influenza$PCA1 <- PCA.analysis_influenza$scores[,1]
  notif_srag_influenza$PCA12 <- PCA.analysis_influenza$scores[,1]+PCA.analysis_influenza$scores[,2]
  notif_srag_influenza$PCA123 <- PCA.analysis_influenza$scores[,1]+PCA.analysis_influenza$scores[,2]+PCA.analysis_influenza$scores[,3]
  
  loadings_influenza <- PCA.analysis_influenza$loadings[,1]
  
  PCA.analysis_outro <- princomp(scale(notif_srag_outro[,c("EE","CV","SKEW","KURT","ID","ACF")]))
  notif_srag_outro$PCA1 <- PCA.analysis_outro$scores[,1]
  notif_srag_outro$PCA12 <- PCA.analysis_outro$scores[,1]+PCA.analysis_outro$scores[,2]
  notif_srag_outro$PCA123 <- PCA.analysis_outro$scores[,1]+PCA.analysis_outro$scores[,2]+PCA.analysis_outro$scores[,3]
  
  loadings_outro <- PCA.analysis_outro$loadings[,1]
  
  notif_srag <- notif_srag %>%
    mutate(freqmean2 = rollapply(frequency,width=28,FUN=mean,fill=0,align='right'))
  
  maxf <- max(notif_srag$CV)
  
  #Figure of notification time series
  gg_cases_covid <- ggplot(notif_srag, aes(date,freqmean)) +
    geom_line(data=notif_srag_covid,aes(y=freqmean,color=paste("Influenza",sep="")),na.rm=TRUE, lwd=1.5) +
    theme_bw() + xlab("Date") + ylab("SARS cases") +
    theme(legend.justification = c(1, 1), legend.position=c(0.4,0.9),
          legend.text=element_text(size=14),
          legend.title=element_blank(),  axis.text=element_text(size=14),
          axis.title=element_text(size=14,face="bold") ) +
    xlim(as.Date("2009-01-01"),as.Date("2024-12-01")) +
    scale_color_manual(values = cbbPalette[3])
  gg_cases_covid
  
  gg_cases_influenza <- ggplot(notif_srag, aes(date,freqmean)) +
    geom_line(data=notif_srag_influenza,aes(y=freqmean,color=paste("Influenza",sep="")),na.rm=TRUE, lwd=1.5) +
    theme_bw() + xlab("Date") + ylab("SARS cases") +
    theme(legend.justification = c(1, 1), legend.position=c(0.4,0.9),
          legend.text=element_text(size=14),
          legend.title=element_blank(),  axis.text=element_text(size=14),
          axis.title=element_text(size=14,face="bold") ) +
    xlim(as.Date("2009-01-01"),as.Date("2024-12-01")) +
    scale_color_manual(values = cbbPalette[2])
  gg_cases_influenza
  
  gg_cases_outro <- ggplot(notif_srag, aes(date,freqmean)) +
    geom_line(data=notif_srag_outro,aes(y=freqmean,color=paste("ORVs",sep="")),na.rm=TRUE, lwd=1.5) +
    theme_bw() + xlab("Date") + ylab("SARS cases") +
    theme(legend.justification = c(1, 1), legend.position=c(0.4,0.9),
          legend.text=element_text(size=14),
          legend.title=element_blank(),  axis.text=element_text(size=14),
          axis.title=element_text(size=14,face="bold") ) +
    xlim(as.Date("2009-01-01"),as.Date("2024-12-01")) +
    scale_color_manual(values = cbbPalette[1])
  gg_cases_outro
  
  gg_srag <- ggarrange(gg_cases_covid, gg_cases_influenza, gg_cases_outro + rremove("x.text"),
            ncol = 1, nrow = 3)
  
  gg_name <- c(paste("Figures/New_cases_SARS_",name,"_daily.png",sep=""))
  ggsave(gg_name, plot = gg_srag, dpi = 300, width=7, height=7)
  
  notif_srag$PCA1_scaled <- scale(notif_srag$PCA1)
  notif_srag$EE_scaled <- scale(notif_srag$EE)
  notif_srag$KURT_scaled <- scale(notif_srag$KURT)
  notif_srag$CV_scaled <- scale(notif_srag$CV)
  notif_srag$ID_scaled <- scale(notif_srag$ID)
  notif_srag$EVI <- evi_func(notif_srag$freqmean,lag=14)
  notif_srag$EVI_scaled <- scale(notif_srag$EVI)
  
  coeff <- max(notif_srag$freqmean)/max(notif_srag$PCA1_scaled)
  coeff1 <- max(notif_srag$freqmean)/max(notif_srag$CV_scaled)
  coeff2 <- max(notif_srag$freqmean)/max(notif_srag$ID_scaled,na.rm=TRUE)
  gg_cases <- ggplot(notif_srag, aes(date,PCA1_scaled)) +
    geom_line(aes(y=freqmean,color=paste("SARS (COVID-19)",sep="")),na.rm=TRUE, lwd=1.5) +
    geom_line(aes(y=PCA1_scaled*coeff,color=paste("PCA1",sep="")),na.rm=TRUE, lwd=1.5) +
    geom_line(aes(y=CV_scaled*coeff1,color=paste("CV",sep="")),na.rm=TRUE, lwd=1.5) +
    geom_line(aes(y=ID_scaled*coeff2,color=paste("ID",sep="")),na.rm=TRUE, lwd=1.5) +
    scale_y_continuous(sec.axis = sec_axis(~./coeff, name="Scaled indicator"),
                       limits=(c(-3000,9500))) +
    theme_bw() + xlab("Date") + ylab("Daily new cases") + #ylim(c(-100,6000)) +
    theme(legend.justification = c(1, 1), legend.position=c(0.9,0.9),
          legend.text=element_text(size=12),
          legend.title=element_blank(),  axis.text=element_text(size=14),
          axis.title=element_text(size=14,face="bold") ) +
    xlim(as.Date("2021-11-01"),as.Date("2022-09-01")) +
    scale_color_manual(values = cbbPalette)
  gg_cases
  gg_name <- c(paste("Figures/New_cases_COVID_",name,"_daily.png",sep=""))
  ggsave(gg_name, plot = gg_cases, dpi = 300, width=7, height=7)
  
  notif_srag_influenza$PCA1_scaled <- scale(notif_srag_influenza$PCA1)
  notif_srag_influenza$KURT_scaled <- scale(notif_srag_influenza$KURT)
  notif_srag_influenza$CV_scaled <- scale(notif_srag_influenza$CV)
  notif_srag_influenza$ID_scaled <- scale(notif_srag_influenza$ID)
  
  coeffi <- max(notif_srag_influenza$freqmean)/max(notif_srag_influenza$PCA1_scaled)
  gg_cases_influenza <- ggplot(notif_srag_influenza, aes(date,PCA1_scaled)) +
    geom_line(aes(y=freqmean,color=paste("SARS (Influenza)",sep="")),na.rm=TRUE, lwd=1.5) +
    geom_line(aes(y=PCA1_scaled*coeffi,color=paste("PCA1",sep="")),na.rm=TRUE, lwd=1.5) +
    geom_line(aes(y=CV_scaled*coeffi,color=paste("CV",sep="")),na.rm=TRUE, lwd=1.5) +
    geom_line(aes(y=ID_scaled*coeffi,color=paste("ID",sep="")),na.rm=TRUE, lwd=1.5) +
    scale_y_continuous(sec.axis = sec_axis(~./coeffi, name="Scaled indicator"),
                       limits = c(-200,2500)) +
    theme_bw() + xlab("Date") + ylab("Daily new cases") +
    theme(legend.justification = c(1, 1), legend.position=c(0.9,0.9),
          legend.text=element_text(size=14),
          legend.title=element_blank(),  axis.text=element_text(size=14),
          axis.title=element_text(size=14,face="bold") ) +
    xlim(as.Date("2009-06-01"),as.Date("2009-12-31")) +
    scale_color_manual(values = cbbPalette)
  gg_cases_influenza
  gg_name <- c(paste("Figures/New_cases_Influenza_",name,"_daily.png",sep=""))
  ggsave(gg_name, plot = gg_cases_influenza, dpi = 300, width=7, height=7)
  
  
  #Scree plots
  loadings <- PCA.analysis$loadings[,1]
  gg_scree <- fviz_eig(PCA.analysis, addlabels = TRUE)
  scree_val <- get_eig(PCA.analysis)
  scree_val$Dimensions <- c(1,2,3,4,5,6)
  scree_val$VariancePercent <- scree_val$variance.percent
  
  gg_scree <- ggplot(scree_val, aes(Dimensions,VariancePercent)) +
    geom_col(aes(y=VariancePercent), na.rm=TRUE, lwd=1.5, 
             color="black", fill=cbbPalette[1]) +
    theme_bw() + xlab("Dimensions") + ylab("Percentage of explained variances") + 
    theme(legend.justification = c(1, 1), legend.position=c(0.9,0.9),
          legend.text=element_text(size=14),
          legend.title=element_blank(),  axis.text=element_text(size=14),
          axis.title=element_text(size=14,face="bold") ) +
    geom_text(aes(label=paste(round(scree_val$VariancePercent,2),"%")), 
              position = position_dodge(width = .9), 
              family = "Times New Roman",
              vjust = -.5, size = 5) + ylim(0,35) + 
    scale_x_continuous(breaks = round(seq(1, 6, by = 1),1))
  gg_scree
  gg_name <- c(paste("Figures/Scree_Plot_COVID_",name,"_daily.png",sep=""))
  ggsave(gg_name, plot = gg_scree, dpi = 300, width=7, height=7)
  
  loadings <- PCA.analysis_influenza$loadings[,1]
  gg_scree_influenza <- fviz_eig(PCA.analysis_influenza, addlabels = TRUE)
  gg_scree_influenza
  gg_name <- c(paste("Figures/Scree_Plot_Influenza_",name,"_daily.png",sep=""))
  ggsave(gg_name, plot = gg_scree, dpi = 300, width=7, height=7)
  
  loadings <- PCA.analysis_outro$loadings[,1]
  gg_scree_outro <- fviz_eig(PCA.analysis_outro, addlabels = TRUE)
  gg_scree_outro
  gg_name <- c(paste("Figures/Scree_Plot_ORVs_",name,"_daily.png",sep=""))
  ggsave(gg_name, plot = gg_scree, dpi = 300, width=7, height=7)
  
  #Load plot
  gg_load <- fviz_pca_var(PCA.analysis, col.var = "cos2",
               gradient.cols = c(cbbPalette[1], cbbPalette[2], cbbPalette[3]),
               repel = TRUE)
  gg_load
  gg_name <- c(paste("Figures/Loadings_COVID_",name,"_daily.png",sep=""))
  ggsave(gg_name, plot = gg_load, dpi = 300, width=7, height=7)
  
  gg_load_influenza <- fviz_pca_var(PCA.analysis_influenza, col.var = "cos2",
                          gradient.cols = c(cbbPalette[1], cbbPalette[2], cbbPalette[3]),
                          repel = TRUE)
  gg_load_influenza
  gg_name <- c(paste("Figures/Loadings_Influenza_",name,"_daily.png",sep=""))
  ggsave(gg_name, plot = gg_load_influenza, dpi = 300, width=7, height=7)
  
  gg_load_outro <- fviz_pca_var(PCA.analysis_outro, col.var = "cos2",
                          gradient.cols = c(cbbPalette[1], cbbPalette[2], cbbPalette[3]),
                          repel = TRUE)
  gg_load_outro
  gg_name <- c(paste("Figures/Loadings_ORVs_",name,"_daily.png",sep=""))
  ggsave(gg_name, plot = gg_load_outro, dpi = 300, width=7, height=7)
  
  
  ggg <- ggarrange(gg_cases + theme(legend.position=c(0.9,0.9),
                                    legend.text=element_text(size=10)), 
                   gg_cases_influenza + theme(legend.position=c(0.9,0.9),
                                              legend.text=element_text(size=10)),
                   gg_scree, gg_load,
            ncol = 2, nrow = 2, labels=c("A","B","C","D"))
  ggg
  gg_name <- c(paste("Figures/PCA_",name,"_daily.png",sep=""))
  ggsave(gg_name, plot = ggg, dpi = 300, width=10, height=10)

  return(list(covid = notif_srag_covid,
              influenza = notif_srag_influenza,
              outro = notif_srag_outro))
}

ROC_AUC_analysis <- function(notif_srag,name,disease_name) {
  
  lags <- data.frame(1)
  lags$CV <- 14
  lags$EE <- 14
  lags$SKEW <- 14
  lags$KURT <- 14
  lags$ID <- 14
  lags$ACF <- 14
  
  notif_srag <- calc_all_ews(notif_srag,lags)
  notif_srag <- calc_all_status(notif_srag,lags)
  
  notif_srag <- notif_srag %>%
    mutate(freqmean = rollapply(frequency,width=14,FUN=mean,fill=0,align='right')) %>%
    mutate(lastweekmean = rollapply(frequency,width=7,FUN=mean,fill=0,align='right'))
  
  maxf <- max(notif_srag$freqmean)
  gg_kendall <- ggplot(notif_srag, aes(date,frequency)) +
    theme_bw() + xlab("Date") + ylab("Daily new cases") +
    theme(legend.justification = c(1, 1), legend.position="bottom",
          legend.text=element_text(size=14),
          legend.title=element_blank(),  axis.text=element_text(size=14),
          axis.title=element_text(size=14,face="bold") ) +
    geom_tile(aes(x=date,y=frequency*0+maxf/2,fill=kendall_status),
              height=maxf,alpha=0.5,na.rm=TRUE) +
    geom_line(aes(y=freqmean), na.rm=TRUE, lwd=1.5, color=cbbPalette[3]) +
    scale_fill_manual(values = c(cbbPalette[1],"white"))
  gg_kendall
  gg_name <- c(paste("Figures/Trends_kendall_",disease_name,"_",name,"_daily.png",sep=""))
  ggsave(gg_name, plot = gg_kendall, dpi = 300, width=7, height=7)
  
  gg_r01 <- ggplot(notif_srag, aes(date,frequency)) +
    theme_bw() + xlab("Date") + ylab("Daily new cases") +
    theme(legend.justification = c(1, 1), legend.position="bottom",
          legend.text=element_text(size=14),
          legend.title=element_blank(),  axis.text=element_text(size=14),
          axis.title=element_text(size=14,face="bold") ) +
    geom_tile(aes(x=date,y=frequency*0+maxf/2,fill=r01_status),
              height=maxf,alpha=0.5,na.rm=TRUE) +
    geom_line(aes(y=freqmean), na.rm=TRUE, lwd=1.5, color=cbbPalette[3]) +
    scale_fill_manual(values = c(cbbPalette[1],"white"))
  gg_r01
  gg_name <- c(paste("Figures/Trends_r01_",disease_name,"_",name,"_daily.png",sep=""))
  ggsave(gg_name, plot = gg_r01, dpi = 300, width=7, height=7)
  
  gg_r02 <- ggplot(notif_srag, aes(date,frequency)) +
    theme_bw() + xlab("Date") + ylab("Daily new cases") +
    theme(legend.justification = c(1, 1), legend.position="bottom",
          legend.text=element_text(size=14),
          legend.title=element_blank(),  axis.text=element_text(size=14),
          axis.title=element_text(size=14,face="bold") ) +
    geom_tile(aes(x=date,y=frequency*0+maxf/2,fill=r02_status),
              height=maxf,alpha=0.5,na.rm=TRUE) +
    geom_line(aes(y=freqmean), na.rm=TRUE, lwd=1.5, color=cbbPalette[3]) +
    scale_fill_manual(values = c(cbbPalette[1],"white"))
  gg_r02
  gg_name <- c(paste("Figures/Trends_r02_",disease_name,"_",name,"_daily.png",sep=""))
  ggsave(gg_name, plot = gg_r02, dpi = 300, width=7, height=7)
  
  gg_Rt <- ggplot(notif_srag, aes(date,frequency)) +
    theme_bw() + xlab("Date") + ylab("Daily new cases") +
    theme(legend.justification = c(1, 1), legend.position="bottom",
          legend.text=element_text(size=14),
          legend.title=element_blank(),  axis.text=element_text(size=14),
          axis.title=element_text(size=14,face="bold") ) +
    geom_tile(aes(x=date,y=frequency*0+maxf/2,fill=RRt_status),
              height=maxf,alpha=0.5,na.rm=TRUE) +
    geom_line(aes(y=freqmean), na.rm=TRUE, lwd=1.5, color=cbbPalette[3]) +
    scale_fill_manual(values = c(cbbPalette[1],"white"))
  gg_Rt
  gg_name <- c(paste("Figures/Trends_Rt_",disease_name,"_",name,"_daily.png",sep=""))
  ggsave(gg_name, plot = gg_Rt, dpi = 300, width=7, height=7)
  
  gg_trends <- ggarrange(gg_kendall + ylab("Cases"), 
                       gg_r02 + ylab("Cases"), 
                       gg_r01 + ylab("Cases"),
                       gg_Rt + ylab("Cases"),
                       labels=c("A","B","C","D"), 
                       ncol = 1, nrow = 4)
  gg_trends
  gg_name <- c(paste("Figures/Trends_all_",disease_name,"_",name,"_daily.png",sep=""))
  ggsave(gg_name, plot = gg_trends, dpi = 300, width=10, height=10)
  
  
  ### AUC OPTIMIZATION
  #AUC optimization new --------------------------------------------------
  print("Begin PCA AUC optimization")
  widths <- seq(3,80,1)
  AUC1 <- data.frame(widths)
  lags <- data.frame(1)
  lags$CV <- 14
  lags$EE <- 14
  lags$SKEW <- 14
  lags$KURT <- 14
  lags$ID <- 14
  lags$ACF <- 14
  lags$EVI <- 14
  lags$cEVI <- 14
  
  kstatus <- kendall_status(cases=notif_srag$frequency,lag=14)
  #rstatus <- r_status(cases=notif_srag$frequency,r=0.2)
  #r01status <- r_status(cases=notif_srag$frequency,r=0.1)
  
  for (j in 1:length(widths)){
    
    lags <- data.frame(1)
    lags$CV <- widths[j]
    lags$EE <- widths[j]
    lags$SKEW <- widths[j]
    lags$KURT <- widths[j]
    lags$ID <- widths[j]
    lags$ACF <- widths[j]
    lags$EVI <- widths[j]
    lags$cEVI <- widths[j]
    
    notif_srag <- calc_all_ews(notif_srag,lags)
    notif_srag$EVI <- evi_func(notif_srag$frequency,lag=lags$EVI) 
    
    AUC00 <- AUC_all(notif_srag,lags,kstatus)
    AUC1$CV[j] <- AUC00$CV
    AUC1$EE[j] <- AUC00$EE 
    AUC1$ACF[j] <- AUC00$ACF 
    AUC1$SKEW[j] <- AUC00$SKEW
    AUC1$KURT[j] <- AUC00$KURT
    AUC1$ID[j] <- AUC00$ID
    AUC1$PCA1[j] <- AUC00$PCA1
    AUC1$PCA12[j] <- AUC00$PCA12
    AUC1$PCA123[j] <- AUC00$PCA123
    AUC1$PCA1lag[j] <- AUC00$PCA1lag 
    
    print(j)
  }
  
  AUC0 <- AUC1
  
  #AUC optimization new EVI --------------------------------------------------
  print("Begin EVI AUC optimization")
  widths <- seq(3,80,1)
  AUC1 <- data.frame(widths)
  lags <- data.frame(1)
  lags$CV <- 14
  lags$EE <- 14
  lags$SKEW <- 14
  lags$KURT <- 14
  lags$ID <- 14
  lags$ACF <- 14
  lags$EVI <- 14
  lags$cEVI <- 14
  
  for (j in 1:length(widths)){
    
    lags <- data.frame(1)
    lags$CV <- widths[j]
    lags$EE <- widths[j]
    lags$SKEW <- widths[j]
    lags$KURT <- widths[j]
    lags$ID <- widths[j]
    lags$ACF <- widths[j]
    lags$EVI <- widths[j]
    lags$cEVI <- widths[j]
    
    notif_srag$EVI <- evi_func(notif_srag$frequency,lag=lags$EVI) 
    
    AUC00 <- AUC_all_cevi(notif_srag,lags,kstatus)
    AUC1$EVI[j] <- AUC00$EVI
    AUC1$cEVI[j] <- AUC00$cEVI
    AUC1$cEVIp[j] <- AUC00$cEVIp
    AUC1$cEVIm[j] <- AUC00$cEVIm
    
    print(j)
  }
  
  gg_AUC <- ggplot(AUC0, aes(widths,CV)) +
    geom_line(aes(x=widths, y=CV,color=paste("CV",sep="")),na.rm=TRUE,lwd=1.5) +
    geom_line(aes(x=widths, y=EE,color=paste("EE",sep="")),na.rm=TRUE,lwd=1.5) +
    geom_line(aes(x=widths, y=ACF,color=paste("ACF",sep="")),na.rm=TRUE,lwd=1.5) +
    geom_line(aes(x=widths, y=SKEW,color=paste("SKEW",sep="")),na.rm=TRUE,lwd=1.5) +
    geom_line(aes(x=widths, y=KURT,color=paste("KURT",sep="")),na.rm=TRUE,lwd=1.5) +
    geom_line(aes(x=widths, y=ID,color=paste("ID",sep="")),na.rm=TRUE,lwd=1.5) +
    geom_line(data=AUC1,aes(x=widths, y=EVI,color=paste("EVI",sep="")),na.rm=TRUE,lwd=1.5) +
    geom_line(data=AUC1,aes(x=widths, y=cEVI,color=paste("cEVI",sep="")),na.rm=TRUE,lwd=1.5) +
    theme_bw() + xlab("Moving window size (days)") + ylab("AUC") +
    theme(legend.justification = c(1, 1), legend.position="bottom",
          legend.text=element_text(size=14),
          legend.title=element_blank(),  axis.text=element_text(size=14),
          axis.title=element_text(size=14,face="bold") ) +
    scale_color_manual(values = cbbPalette)
  gg_AUC
  gg_name <- c(paste("Figures/AUC_PCA_",disease_name,"_",name,".png",sep=""))
  ggsave(gg_name, plot = gg_AUC, dpi = 300, width=7, height=7)
  
  gg_AUC0E <- ggplot(AUC1, aes(widths,CV)) +
    geom_line(data=AUC0,aes(x=widths, y=PCA1,color=paste("PCA1",sep="")),na.rm=TRUE,lwd=1.5) +
    geom_line(data=AUC0,aes(x=widths, y=PCA12,color=paste("PCA12",sep="")),na.rm=TRUE,lwd=1.5) +
    geom_line(data=AUC0,aes(x=widths, y=PCA123,color=paste("PCA123",sep="")),na.rm=TRUE,lwd=1.5) +
    geom_line(aes(x=widths, y=cEVIm,color=paste("cEVI-",sep="")),na.rm=TRUE,lwd=1.5) +
    geom_line(aes(x=widths, y=cEVIp,color=paste("cEVI+",sep="")),na.rm=TRUE,lwd=1.5) +
    theme_bw() + xlab("Moving window size (days)") + ylab("AUC") +
    theme(legend.justification = c(1, 1), legend.position="bottom",
          legend.text=element_text(size=14),
          legend.title=element_blank(),  axis.text=element_text(size=14),
          axis.title=element_text(size=14,face="bold") ) +
    scale_color_manual(values = cbbPalette)
  gg_AUC0E
  gg_name <- c(paste("Figures/AUC_EVI_",disease_name,"_",name,".png",sep=""))
  ggsave(gg_name, plot = gg_AUC0E, dpi = 300, width=7, height=7)
  
  ggg <- ggarrange(gg_AUC + theme(legend.position="right",
                                  legend.text=element_text(size=14),  
                                  axis.text=element_text(size=14),
                                  axis.title=element_text(size=14,face="bold")), 
                   gg_AUC0E + theme(legend.position="right",
                                    legend.text=element_text(size=14),  
                                    axis.text=element_text(size=14),
                                    axis.title=element_text(size=14,face="bold")),
                   ncol = 1, nrow = 2, labels=c("A","B"))
  ggg
  gg_name <- c(paste("Figures/AUC_optim_",name,"_daily.png",sep=""))
  ggsave(gg_name, plot = ggg, dpi = 300, width=10, height=10)
  
  #Max index of each
  optim_lags <- lags
  optim_lags$CV <- which.max(AUC0$CV)+min(widths)-1
  optim_lags$EE <- which.max(AUC0$EE)+min(widths)-1
  optim_lags$SKEW <- which.max(AUC0$SKEW)+min(widths)-1
  optim_lags$KURT <- which.max(AUC0$KURT)+min(widths)-1
  optim_lags$ID <- which.max(AUC0$ID)+min(widths)-1
  optim_lags$ACF <- which.max(AUC0$ACF)+min(widths)-1
  optim_lags$EVI <- which.max(AUC1$EVI)+min(widths)-1
  optim_lags$cEVI <- which.max(AUC1$cEVI)+min(widths)-1
  optim_lags$cEVIm <- which.max(AUC1$cEVIm)+min(widths)-1
  optim_lags$cEVIp <- which.max(AUC1$cEVIp)+min(widths)-1
  
  notif_srag <- notif_srag %>%
    mutate(freqmean = rollapply(frequency,width=14,FUN=mean,fill=0,align='right')) %>%
    mutate(lastweekmean = rollapply(frequency,width=6,FUN=mean,fill=0,align='right')) %>%
    mutate(EE = rollapply(freqmean,width=optim_lags$EE,FUN=empirical_entropy,fill=0,align='right')) %>%
    mutate(CV = rollapply(freqmean,width=optim_lags$CV,FUN=coefficient_of_variation,fill=0,align='right')) %>%
    mutate(SKEW = rollapply(freqmean,width=optim_lags$SKEW,FUN=skewness,fill=0,align='right')) %>%
    mutate(KURT = rollapply(freqmean,width=optim_lags$KURT,FUN=kurtosis,fill=0,align='right')) %>%
    mutate(ID = rollapply(freqmean,width=optim_lags$ID,FUN=index_of_dispersion,fill=0,align='right')) %>%
    mutate(ACF = rollapply(freqmean,width=optim_lags$ACF,FUN=acf1,fill=0,align='right'))
  
  notif_srag$EVI <- evi_func(notif_srag$frequency,lag=optim_lags$EVI)
  notif_srag$cEVI <- cevi_func(notif_srag$frequency,lag=optim_lags$cEVI,cut=0)
  
  is.na(notif_srag)<-sapply(notif_srag, is.infinite)
  notif_srag[is.na(notif_srag)]<-0
  
  #Principal Component Analysis
  PCA.analysis <- princomp(scale(notif_srag[,c("EE","CV","SKEW","KURT","ID","ACF")]))
  notif_srag$PCA1 <- PCA.analysis$scores[,1]
  notif_srag$PCA12 <- PCA.analysis$scores[,1]+PCA.analysis$scores[,2]
  notif_srag$PCA123 <- PCA.analysis$scores[,1]+PCA.analysis$scores[,2]+PCA.analysis$scores[,3]
  notif_srag[is.na(notif_srag)] <- 0
  
  #Scree plots
  loadings <- PCA.analysis$loadings[,1]
  gg_scree <- fviz_eig(PCA.analysis, addlabels = TRUE)
  gg_scree
  gg_name <- c(paste("Figures/Scree_Plot_optimized_",disease_name,"_",name,"_daily.png",sep=""))
  ggsave(gg_name, plot = gg_scree, dpi = 300, width=7, height=7)
  
  #Load plot
  gg_load <- fviz_pca_var(PCA.analysis, col.var = "cos2",
                          gradient.cols = c(cbbPalette[1], cbbPalette[2], cbbPalette[3]),
                          repel = TRUE)
  gg_load
  gg_name <- c(paste("Figures/Loadings_optimized_",disease_name,"_",name,"_daily.png",sep=""))
  ggsave(gg_name, plot = gg_load, dpi = 300, width=7, height=7)
  
  ### ROC curve with optimized lags
  print("Begin ROC curve with optimized lags")
  ROCcurve <- ROC_all(notif_srag,optim_lags,kstatus)
  ROCcurve2 <- ROC_all_2(notif_srag,optim_lags,kstatus)
  ROCcurve_evi <- ROC_all_cevi(notif_srag,optim_lags,kstatus)
  
  gg_ROC0 <- ggplot(ROCcurve, aes(CV_fpr,CV_tpr)) +
    geom_line(aes(x=CV_fpr,y=CV_tpr,color=paste("CV",sep="")),na.rm=TRUE,lwd=1.5) +
    geom_line(aes(x=EE_fpr,y=EE_tpr,color=paste("EE",sep="")),na.rm=TRUE,lwd=1.5) +
    geom_line(aes(x=ACF_fpr,y=ACF_tpr,color=paste("ACF",sep="")),na.rm=TRUE,lwd=1.5) +
    geom_line(aes(x=SKEW_fpr,y=SKEW_tpr,color=paste("SKEW",sep="")),na.rm=TRUE,lwd=1.5) +
    geom_line(aes(x=KURT_fpr,y=KURT_tpr,color=paste("KURT",sep="")),na.rm=TRUE,lwd=1.5) +
    geom_line(aes(x=ID_fpr,y=ID_tpr,color=paste("ID",sep="")),na.rm=TRUE,lwd=1.5) +
    geom_line(data=ROCcurve_evi,aes(x=EVI_fpr,y=EVI_tpr,color=paste("EVI",sep="")),na.rm=TRUE,lwd=1.5) +
    geom_line(data=ROCcurve_evi,aes(x=cEVI_fpr,y=cEVI_tpr,color=paste("cEVI",sep="")),na.rm=TRUE,lwd=1.5) +
    geom_abline(slope=1, linetype = "dashed", color="Black",lwd=1.5) +
    xlim(c(0,1)) + ylim(c(0,1)) +
    theme_bw() + xlab("False Positive Rate (1-Specificity)") + ylab("True Positive Rate (Sensitivity)") +
    theme(legend.justification = c(1, 1), legend.position="bottom",
          legend.text=element_text(size=14),
          legend.title=element_blank(),  axis.text=element_text(size=14),
          axis.title=element_text(size=14,face="bold") ) +
    scale_color_manual(values = cbbPalette)
  gg_ROC0
  gg_name <- c(paste("Figures/ROC0_optimized_",disease_name,"_",name,".png",sep=""))
  ggsave(gg_name, plot = gg_ROC0, dpi = 300, width=7, height=7)
  
  
  gg_ROC <- ggplot(ROCcurve, aes(CV_fpr,CV_tpr)) +
    geom_line(aes(x=PCA1_fpr,y=PCA1_tpr,color=paste("PCA1",sep="")),na.rm=TRUE,lwd=1.5) +
    geom_line(aes(x=PCA12_fpr,y=PCA12_tpr,color=paste("PCA12",sep="")),na.rm=TRUE,lwd=1.5) +
    geom_line(aes(x=PCA123_fpr,y=PCA123_tpr,color=paste("PCA123",sep="")),na.rm=TRUE,lwd=1.5) +
    geom_abline(slope=1, linetype = "dashed", color="Black",lwd=1.5) +
    xlim(c(0,1)) + ylim(c(0,1)) +
    theme_bw() + xlab("False Positive Rate (1-Specificity)") + ylab("True Positive Rate (Sensitivity)") +
    theme(legend.justification = c(1, 1), legend.position="bottom",
          legend.text=element_text(size=14),
          legend.title=element_blank(),  axis.text=element_text(size=14),
          axis.title=element_text(size=14,face="bold") ) +
    scale_color_manual(values = cbbPalette)
  gg_ROC
  gg_name <- c(paste("Figures/ROC_optimized_",disease_name,"_",name,".png",sep=""))
  ggsave(gg_name, plot = gg_ROC, dpi = 300, width=7, height=7)
  
  ggg <- ggarrange(gg_ROC0 + theme(legend.position="right",
                                  legend.text=element_text(size=14),  
                                  axis.text=element_text(size=14),
                                  axis.title=element_text(size=14,face="bold")), 
                   gg_ROC + theme(legend.position="right",
                                    legend.text=element_text(size=14),  
                                    axis.text=element_text(size=14),
                                    axis.title=element_text(size=14,face="bold")),
                   ncol = 1, nrow = 2, labels=c("A","B"))
  ggg
  gg_name <- c(paste("Figures/ROC_optim_",name,"_daily.png",sep=""))
  ggsave(gg_name, plot = ggg, dpi = 300, width=10, height=10)
  
  ## Max Youden index and table
  print("Get max youden and output statistics")
  ROCcurve <- ROC_all(notif_srag,optim_lags,kstatus)
  ROCcurve_evi <- ROC_all_cevi(notif_srag,optim_lags,kstatus)
  ROCcurve2 <- ROC_all_2(notif_srag,optim_lags,kstatus)
  ROCcurve_evi2 <- ROC_all_cevi_2(notif_srag,optim_lags,kstatus)
  table1 <- table2_build(ROCcurve,ROCcurve_evi,ROCcurve2,ROCcurve_evi2,disease_name)
  
}

