#Libraries
library(outbreakinfo)
library(lubridate)
library(dplyr)
library(ggplot2)
library(sf)
library(patchwork)
library(tidyverse)
library(tsibble)
library(stringr)
library(data.table)
library(epikit)
library(cowplot)
library(ggpubr)
library(zoo)
library(runner)
library(moments)
library(stats)
library(factoextra)
library(sars)
library(EVI)
#kendall function
library(spatialEco)
library(zyp)
#AD test
library(nortest)
#AUC function
library(DescTools)
#Rt function
library(EpiEstim)

Sys.setlocale("LC_ALL", "English")

# The palette with black:
cbbPalette <- c("#56B4E9", "#009E73", "#CC79A7","#000000", 
                "#E69F00", "#F0E442", "#0072B2", "#D55E00", 
                "#999999")


### DATABASE ORGANIZATION

#1) The following databases need to be downloaded (csv files in links):
#https://opendatasus.saude.gov.br/dataset/srag-2009-2012
#https://opendatasus.saude.gov.br/dataset/srag-2013-2018
#https://opendatasus.saude.gov.br/dataset/srag-2019
#https://opendatasus.saude.gov.br/dataset/srag-2020
#https://opendatasus.saude.gov.br/dataset/srag-2021-a-2024

#2) The downloaded csv files correspond each to an year of SRAG data in Brazil

#3) The files need to be in the same folder as in this script, and 
#are named "INFLUDXX.csv", where "XX" are the last two digits of a given year,
#so the data for 2017 is named "INFLUD17.csv"

#Load custom functions
source("phases_classification.R")
source("analysis_functions.R")
source("indicators.R")
source("statistics_custom.R")

### GET NOTIFICATION DATA FOR COUNTRY
name <- "Brazil"
notif_analysis(name)

### NOTIFICATION SERIES WITH TRUE INCREASES
disease_name <- "COVID"
ROC_AUC_analysis(notif_srag_covid,name,disease_name)
  
disease_name <- "Influenza"
notif_srag_influenza2020p <- notif_srag_influenza %>% filter(year>=2020) 
ROC_AUC_analysis(notif_srag_influenza2020p,name,disease_name)

disease_name <- "ORVs"
notif_srag_outro2020p <- notif_srag_outro %>% filter(year>=2020) 
ROC_AUC_analysis(notif_srag_outro2020p,name,disease_name)

### Brazil epidemic phases
disease_name <- "COVID"
find_phases(notif_srag_covid,name,disease_name)
