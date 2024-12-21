find_phases <- function(notif_srag,name,disease_name) {

#Run cEVI
notif_srag <- notif_srag %>%
  mutate(freqmean2 = rollapply(frequency,width=28*2,FUN=mean,fill=0,align='right'))
notif_srag$cEVI <- cEVI_fun(notif_srag$freqmean2,c_n=0.5,lag_n=4)
notif_srag$cEVI[is.na(notif_srag$cEVI)] <- 0
notif_srag$cEVI_status <- notif_srag$cEVI
notif_srag$cases_1 <- notif_srag$freqmean2*notif_srag$cEVI_status
notif_srag$cases_1[notif_srag$cases_1 == 0] <- NA
notif_srag$cases_0 <- notif_srag$freqmean2*(1-notif_srag$cEVI_status)
notif_srag$cases_0[notif_srag$cases_0 == 0] <- NA

#Find transitions
range <- 7
transitions <- 0
transition_dates <- notif_srag$date[1]
transition_dates_begin <- notif_srag$date[1]
transition_dates_end <- notif_srag$date[1]
transition_is <- 1

for (i in 8:length(notif_srag$cEVI)) {
  if ( any(is.na(notif_srag$cases_0[(i-(1+range)):(i-1)]))==FALSE & 
       any(is.na(notif_srag$cases_1[i:(i+range)]))==FALSE ) {
    print(paste("transition to begin cases at",notif_srag$date[i]," ",i))
    transitions <- append(transitions,1)
    transition_dates <- append(transition_dates,notif_srag$date[i])
    transition_dates_begin <- append(transition_dates_begin,notif_srag$date[i])
    transition_is <- append(transition_is,i)
  }
  else if ( any(is.na(notif_srag$cases_1[(i-(1+range)):(i-1)]))==FALSE & 
            any(is.na(notif_srag$cases_0[i:(i+range)]))==FALSE ) {
    print(paste("transition to end cases at",notif_srag$date[i]," ",i))
    transitions <- append(transitions,0)
    transition_dates <- append(transition_dates,notif_srag$date[i])
    transition_dates_end <- append(transition_dates_end,notif_srag$date[i])
  }
}

transition_dates_begin <- append(transition_dates_begin,max(notif_srag$date))
transition_is <- append(transition_is,length(notif_srag$cEVI))

#transitions
#transition_dates
#transition_dates_begin
#transition_is

peak_mean_old <- 0
peak_sd_old <- 0
state_old <- "Disease-Free"
notif_srag$new_status <- "Disease-Free"
flag <- 0
flag2 <- 0

for (i in 1:(length(transition_dates_begin)-1)) {
  
  #Calculate areas
  notif_srag_peak <- notif_srag %>%
    filter(between(date, as.Date(transition_dates_begin[i]), as.Date(transition_dates_begin[i+1])))
  peak_sum <- sum(notif_srag_peak$freqmean2)
  peak_mean <- mean(notif_srag_peak$freqmean2)
  peak_sd <- sd(notif_srag_peak$freqmean2)
  peak_max <- max(notif_srag_peak$freqmean2)
  
  print(c(peak_mean,peak_mean+1*peak_sd,peak_mean-1*peak_sd))
  
  print(paste("From",as.Date(transition_dates_begin[i]),"to",as.Date(transition_dates_begin[i+1])))
  
  #Check new peak
  r <- 1
  
  if (state_old=="Decreasing Endemic") {r<-0.25}
  else {r<-1}
  
  if (flag>2){r<-2}
  if (flag2>2){r<-2}
  
  if (peak_mean > peak_mean_old+r*peak_sd_old){
    print("maior")
    state <- "Epidemic"
    #notif_srag$new_status <- "Epidemic"
    flag <- 0
    flag2 <- 0
  }
  else if (peak_mean < peak_mean_old-r*peak_sd_old){
    print("diminuiu")
    state <- "Decreasing Endemic"
    #notif_srag$new_status <- "Decreasing Endemic"
    flag <- flag + 1
    flag2 <- 0
  }
  else {
    print("no meio")
    state <- "Sustained Endemic"
    #notif_srag$new_status <- "Sustained Endemic"
    flag <- 0
    flag2 <- flag2+1
  }
  
  peak_mean_old <- peak_mean
  peak_sd_old <- peak_sd
  state_old <- state
  
  notif_srag$new_status[transition_is[i]:transition_is[i+1]] <- state
  
}
notif_srag$new_status[1:20] <- "Disease-Free"
#notif_srag$status <- "Decreasing Endemic"
#notif_srag$status[notif_srag$date<"2021-12-14"] <- "Epidemic"
##notif_srag$status[notif_srag$date<"2022-09-23"] <- "Decreasing Endemic"
#notif_srag$status[notif_srag$date>"2023-02-01"] <- "Sustained Endemic"
#notif_srag$status[notif_srag$freqmean==0] <- "Disease-Free"

maxf <- max(notif_srag$freqmean)
gg_cases_covid <- ggplot(notif_srag, aes(date,freqmean2)) +
  geom_tile(aes(x=lag(date,28/2),y=lead(freqmean2*0+maxf/2,28*2),fill=new_status),
            height=maxf,alpha=0.35,na.rm=TRUE) +
  geom_line(data=notif_srag,aes(y=freqmean,color=paste("Notification data",sep="")),na.rm=TRUE, lwd=1.5) +
  
  ##geom_point(data=peaks,aes(x=date,y=freq),color="red") +
  
  #geom_point(data=notif_srag,aes(y=cases_1,color=paste("cEVI=1",sep="")),na.rm=TRUE, lwd=1.5) +
  #geom_point(data=notif_srag,aes(y=cases_0,color=paste("cEVI=0",sep="")),na.rm=TRUE, lwd=1.5) +
  #geom_vline(xintercept = as.numeric(as.Date(transition_dates_begin))) +
  
  ##geom_point(data=vals,aes(x=date,y=freq),color="blue") +
  ##geom_line(data=notif_srag,aes(y=lead(freqmean2,14),color=paste("Model",sep="")),na.rm=TRUE, lwd=1.5) +
  ##geom_line(data=notif_srag,aes(y=scale(cEVI),color=paste("Model",sep="")),na.rm=TRUE, lwd=1.5) +
  
theme_bw() + xlab("Date") + ylab("SARS cases") +
  theme(legend.justification = c(1, 1), #legend.position=c(0.8,0.9),
        legend.position="bottom",
        legend.text=element_text(size=12),
        legend.title=element_blank(),  axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold") ) +
  #xlim(as.Date("2023-02-01"),as.Date("2024-12-01")) +
  #ylim(c(0,500)) +
  scale_fill_manual(values = c(cbbPalette[2],cbbPalette[5],cbbPalette[7],
                               cbbPalette[3])) +
  scale_color_manual(values = c("black",cbbPalette[3])) + 
  guides(fill = guide_legend(nrow = 2))
gg_cases_covid

gg_name <- c(paste("Figures/SARS_classification_",name,"_weekly.png",sep=""))
ggsave(gg_name, plot = gg_cases_covid, dpi = 300, width=7, height=7)
}
