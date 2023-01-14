rm(list=ls())
library(tidyverse)
library(ggplot2)
library(readxl)
library(dplyr)
library(reshape2)
library(ggcorrplot)
library(readxl)
library(plyr)
library(dplyr)
library(xtable)
library(knitr)
library(egg)
library(rportfolio)
library(PerformanceAnalytics)
#install.packages("rportfolio")
rm(list=ls())

STOXX600_timeserie <- read.csv("C:/Users/Hadrien Venance/Desktop/Thesis/Data/STOXX600_timeseries_new_dataset.csv")
ESG_timeserie <- read.csv("C:/Users/Hadrien Venance/Desktop/Thesis/Data/ESG_leaders_highPrice_timeseries.csv", na.strings="")
nonESG_timeserie <- read.csv("C:/Users/Hadrien Venance/Desktop/Thesis/Data/ESG_laggards_highPrice_timeseries.csv", na.strings="")

#only take data until 31st May 2020
#STOXX600_timeserie <- STOXX600_timeserie[STOXX600_timeserie$Date<="2020-05-31",]

ESG <- read_excel("C:/Users/Hadrien Venance/Desktop/Thesis/Data/ESG_leaders_highPrice.xlsx")
nonESG<- read_excel("C:/Users/Hadrien Venance/Desktop/Thesis/Data/ESG_laggards_highPrice.xlsx")

names(nonESG)[names(nonESG) == 'Identifier (RIC)'] <- 'Identifier'
names(ESG)[names(ESG) == 'Identifier (RIC)'] <- 'Identifier'

colnames(ESG)
colnames(nonESG)
colnames(ESG) <- gsub("\r", "", colnames(ESG))
colnames(nonESG) <- gsub("\r", "", colnames(nonESG))

names(ESG)[names(ESG) == 'ROE Total Equity %  - 2019\n(FY-2)'] <- 'ROE Total Equity % - 2019\n(FY-2)'


### look at industries best-in-class ###
# different industries in the dataset: 
industries_ESG <- unique(ESG$`ICB Industry name`)
industries_nonESG <-  unique(nonESG$`ICB Industry name`) 
# we don't have any industries for the sector "Energy" in nonESG
nrow(ESG[which(ESG$`ICB Industry name`== "Energy"),])

## for tables in part 2.3 sample matching ##
names_ESG <- colnames(ptf_ESG_timeserie)
industry_ESG <- ESG %>% 
  filter(
    `Identifier` %in% names_ESG)

names_nonESG <- colnames(ptf_nonESG_timeserie)
industry_nonESG <- nonESG %>% 
  filter(
    `Identifier` %in% names_nonESG)



#tech: 
tech <- bind_rows(ESG[which(ESG$`ICB Industry name`== "Technology"),], nonESG[which(nonESG$`ICB Industry name`== "Technology"),])
#decide which ESG companies will make up the peer group:
tech_ESG <- tech[2,]
#decide which nonESG companies will make up the peer group --> BASED ON Industry, Leverage, and Country
tech_nonESG <- tech[c(5,7,9),]
#join them
tech_peer <- bind_rows(tech_ESG,tech_nonESG)
tech_peer
#keep only columns for benchmarking 
colnames(tech_peer)
tech_bench <- tech_peer[,c(2,4,8,11,17,20,24)]
tech_bench[,-c(1,2,7)] <- tech_bench[,-c(1,2,7)]*100
names(tech_bench)[names(tech_bench) == 'ROE Total Equity % - 2019\n(FY-2)'] <- 'ROE %'
names(tech_bench)[names(tech_bench) == '52 Week Total Return\n(-2CY)'] <- 'Total Return %'
#names(tech_bench)[names(tech_bench) == 'Operating Margin - % - Y2019\n(FY-2)'] <- 'Operating Margin %'
names(tech_bench)[names(tech_bench) == 'ROA Total Assets, Percent - 2019\n(1D, FY-2)'] <- 'ROA Total Assets %'
kable(tech_bench, "latex")
#keep col for DYNAMIC benchmarking --> t-test of means
#ROA

#Op margin 


tech_names <- tech_bench$`Company Name`
colnames(ESG_timeserie) <- ESG_timeserie[1,]
ESG_timeserie <- ESG_timeserie[-c(1:2),]
#only take data until 31st May 2020
ESG_timeserie <- ESG_timeserie[ESG_timeserie$Instrument<="2020-05-31",]
tech_ESG_timeserie <- data_frame(ESG_timeserie$Instrument, ESG_timeserie$SAPG.DE)
names(tech_ESG_timeserie)[names(tech_ESG_timeserie) == 'ESG_timeserie$Instrument'] <- 'date'
names(tech_ESG_timeserie)[names(tech_ESG_timeserie) == 'ESG_timeserie$SAPG.DE'] <- 'SAP SE'
tech_ESG_timeserie %>% select(date, everything()) %>%
  arrange(date = as.Date(date, "%d/%m/%Y"))
tech_ESG_timeserie$`SAP SE` <- as.numeric(tech_ESG_timeserie$`SAP SE`)
#create rtrns based on price
tech_ESG_timeserie <- tech_ESG_timeserie %>% mutate(`SAP SE`=100*log(`SAP SE`/lag(`SAP SE`)))

colnames(nonESG_timeserie) <- nonESG_timeserie[1,]
nonESG_timeserie <- nonESG_timeserie[-c(1:2),]
#only take data until 31st May 2020
nonESG_timeserie <- nonESG_timeserie[nonESG_timeserie$Instrument<="2020-05-31",]
tech_nonESG_timeserie <- data.frame(nonESG_timeserie$Instrument, nonESG_timeserie$A1OS.DE, nonESG_timeserie$SMHNn.DE, nonESG_timeserie$PSAGn.DE)
names(tech_nonESG_timeserie)[names(tech_nonESG_timeserie) == 'nonESG_timeserie.Instrument'] <- 'date'
names(tech_nonESG_timeserie)[names(tech_nonESG_timeserie) == 'nonESG_timeserie.A1OS.DE'] <- 'All for One Group SE'
names(tech_nonESG_timeserie)[names(tech_nonESG_timeserie) == 'nonESG_timeserie.SMHNn.DE'] <- 'SUESS MicroTec SE'
names(tech_nonESG_timeserie)[names(tech_nonESG_timeserie) == 'nonESG_timeserie.PSAGn.DE'] <- 'PSI Software AG'
tech_nonESG_timeserie %>% select(date, everything()) %>%
  arrange(date = as.Date(date, "%d/%m/%Y"))
tech_nonESG_timeserie$`All for One Group SE` <- as.numeric(tech_nonESG_timeserie$`All for One Group SE`)
tech_nonESG_timeserie$`SUESS MicroTec SE` <- as.numeric(tech_nonESG_timeserie$`SUESS MicroTec SE`)
tech_nonESG_timeserie$`PSI Software AG` <- as.numeric(tech_nonESG_timeserie$`PSI Software AG`)
#create rtrns based on price
colnames(tech_nonESG_timeserie)
tech_nonESG_timeserie <- tech_nonESG_timeserie %>% mutate(`All for One Group SE`=100*log(`All for One Group SE`/lag(`All for One Group SE`)))
tech_nonESG_timeserie <- tech_nonESG_timeserie %>% mutate(`SUESS MicroTec SE`=100*log(`SUESS MicroTec SE`/lag(`SUESS MicroTec SE`)))
tech_nonESG_timeserie <- tech_nonESG_timeserie %>% mutate(`PSI Software AG`=100*log(`PSI Software AG`/lag(`PSI Software AG`)))

#merge the 2dfs
tech_merged <- merge(tech_ESG_timeserie, tech_nonESG_timeserie, by="date")
tech_merged$date <- as.Date(tech_merged$date)
#separated lineplot:
p1 <- ggplot(na.omit(tech_merged), aes(x=date, group=1)) + 
  geom_line(aes(y = `SAP SE`), color = "#93c572") + ylim(-25,20) + 
  scale_x_date(date_labels = "%m-%Y")
p2 <- ggplot(na.omit(tech_merged), aes(x=date, group=1)) + 
  geom_line(aes(y = `All for One Group SE`), color="#FFA07A") + ylim(-25,20) + 
  scale_x_date(date_labels = "%m-%Y")
p3 <- ggplot(na.omit(tech_merged), aes(x=date, group=1)) + 
  geom_line(aes(y = `SUESS MicroTec SE`), color="#FF6347") + ylim(-25,20) + 
  scale_x_date(date_labels = "%m-%Y")
p4 <- ggplot(na.omit(tech_merged), aes(x=date, group=1)) + 
  geom_line(aes(y = `PSI Software AG`), color="#800000") + ylim(-25,20) + 
  scale_x_date(date_labels = "%m-%Y")
ggarrange(p1, p2, p3, p4, ncol = 2)
# Histogram with kernel density
h1 <- ggplot(na.omit(tech_merged), aes(x = `SAP SE`)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white") +
  geom_density(lwd = 1, colour = 3,
               fill = 3, alpha = 0.25)
h2 <- ggplot(na.omit(tech_merged), aes(x = `All for One Group SE`)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 2, fill = "white") +
  geom_density(lwd = 1, colour = 2,
               fill = 2, alpha = 0.25)
h3 <- ggplot(na.omit(tech_merged), aes(x = `SUESS MicroTec SE`)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 2, fill = "white") +
  geom_density(lwd = 1, colour = 2,
               fill = 2, alpha = 0.25)
h4 <- ggplot(na.omit(tech_merged), aes(x = `PSI Software AG`)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 2, fill = "white") +
  geom_density(lwd = 1, colour = 2,
               fill = 2, alpha = 0.25)
ggarrange(h1, h2, h3, h4, ncol = 2)
#table with std, mean rtrn, sharpe ratio, etc
STOXX600_timeserie <- read.csv("C:/Users/Hadrien Venance/Desktop/Thesis/Data/STOXX600_timeseries_new_dataset.csv")
tech_mean <- c(mean(tech_merged$`SAP SE`, na.rm = T),mean(tech_merged$`All for One Group SE`, na.rm = T),
               mean(tech_merged$`SUESS MicroTec SE`, na.rm = T),mean(tech_merged$`PSI Software AG`, na.rm = T))
tech_std <- c(sd(tech_merged$`SAP SE`, na.rm = T),sd(tech_merged$`All for One Group SE`, na.rm = T),
               sd(tech_merged$`SUESS MicroTec SE`, na.rm = T),sd(tech_merged$`PSI Software AG`, na.rm = T))
#excess ret by substracting ret from STOXX600
STOXX600_timeserie$`rf_STOXX600` <- 100*log(STOXX600_timeserie$`Close.Price`/lag(STOXX600_timeserie$`Close.Price`))
tech_excess_ret <- tech_merged[,2:5] - STOXX600_timeserie$rf_STOXX600
tech_sharpe <- c(round(mean(tech_excess_ret$`SAP SE`, na.rm = T)/sd(tech_excess_ret$`SAP SE`, na.rm = T),3),round(mean(tech_excess_ret$`All for One Group SE`, na.rm = T)/sd(tech_excess_ret$`All for One Group SE`, na.rm = T),3),
                 round(mean(tech_excess_ret$`SUESS MicroTec SE`, na.rm = T)/sd(tech_excess_ret$`SUESS MicroTec SE`, na.rm = T),3),round(mean(tech_excess_ret$`PSI Software AG`, na.rm = T)/sd(tech_excess_ret$`PSI Software AG`, na.rm = T),3))
names(STOXX600_timeserie)[names(STOXX600_timeserie) == 'Date'] <- 'date'
STOXX600_timeserie$date <- as.Date(STOXX600_timeserie$date)
tech_merged <- merge(tech_merged, STOXX600_timeserie, by='date')
tech_merged[,-1] <- sapply(tech_merged[,-1], as.numeric)
tech_merged$date <- as.Date(tech_merged$date)
tech_xts <- xts(tech_merged[,-1], order.by=as.Date(tech_merged[,1]))
tech_xts <- tech_xts[,-6] #remove Instrument column only w/ NAs
tech_xts <-  tech_xts[complete.cases(tech_xts)]
tech_infoRatio <- c(InformationRatio(tech_xts[,"SAP SE",drop= FALSE]/100, tech_xts[,"rf_STOXX600", drop =FALSE]/100),InformationRatio(tech_xts[,"All for One Group SE",drop= FALSE]/100, tech_xts[,"rf_STOXX600", drop =FALSE]/100),
                    InformationRatio(tech_xts[,"SUESS MicroTec SE",drop= FALSE]/100, tech_xts[,"rf_STOXX600", drop =FALSE]/100),InformationRatio(tech_xts[,"PSI Software AG",drop= FALSE]/100, tech_xts[,"rf_STOXX600", drop =FALSE]/100))
tech_perf <- data.frame(tech_mean, tech_std, tech_sharpe, tech_infoRatio)
kable(tech_perf, "latex")

# VaR methods
# Hist simulations
#Kernel estimations
health <- bind_rows(ESG[which(ESG$`ICB Industry name`== "Health Care"),], nonESG[which(nonESG$`ICB Industry name`== "Health Care"),])
#decide which ESG companies will make up the peer group:
health_ESG <- health[c(1,3),]
#decide which nonESG companies will make up the peer group --> BASED ON Industry, Leverage, and Country
health_nonESG <- health[c(13,17),]
#join them
health_peer <- bind_rows(health_ESG,health_nonESG)
health_peer
#keep only columns for benchmarking 
colnames(health_peer)
health_bench <- health_peer[,c(2,4,8,11,17,20,24)]
health_bench[,-c(1,2,7)] <- health_bench[,-c(1,2,7)]*100
names(health_bench)[names(health_bench) == 'ROE Total Equity % - 2019\n(FY-2)'] <- 'ROE %'
names(health_bench)[names(health_bench) == '52 Week Total Return\n(-2CY)'] <- 'Total Return %'
names(health_bench)[names(health_bench) == 'Operating Margin - % - Y2019\n(FY-2)'] <- 'Operating Margin %'
names(health_bench)[names(health_bench) == 'ROA Total Assets, Percent - 2019\n(1D, FY-2)'] <- 'ROA Total Assets %'
kable(health_bench, "latex")
#keep col for DYNAMIC benchmarking --> t-test of means
#ROA

# + ADD DRAWDOWNSSS



#health:

#Op margin 

health_names <- health_bench$`Company Name`
health_ESG_timeserie <- data_frame(ESG_timeserie$Instrument, ESG_timeserie$ROG.S, ESG_timeserie$LONN.S)
names(health_ESG_timeserie)[names(health_ESG_timeserie) == 'ESG_timeserie$Instrument'] <- 'date'
names(health_ESG_timeserie)[names(health_ESG_timeserie) == 'ESG_timeserie$ROG.S'] <- 'Roche Holding AG'
names(health_ESG_timeserie)[names(health_ESG_timeserie) == 'ESG_timeserie$LONN.S'] <- 'Lonza Group AG'
health_ESG_timeserie %>% select(date, everything()) %>%
  arrange(date = as.Date(date, "%d/%m/%Y"))
health_ESG_timeserie$`Roche Holding AG` <- as.numeric(health_ESG_timeserie$`Roche Holding AG`)
health_ESG_timeserie$`Lonza Group AG` <- as.numeric(health_ESG_timeserie$`Lonza Group AG`)
#create rtrns based on price
health_ESG_timeserie <- health_ESG_timeserie %>% mutate(`Roche Holding AG`=100*log(`Roche Holding AG`/lag(`Roche Holding AG`)))
health_ESG_timeserie <- health_ESG_timeserie %>% mutate(`Lonza Group AG`=100*log(`Lonza Group AG`/lag(`Lonza Group AG`)))

health_nonESG_timeserie <- data.frame(nonESG_timeserie$Instrument, nonESG_timeserie$CLTN.S, nonESG_timeserie$COPN.S)
names(health_nonESG_timeserie)[names(health_nonESG_timeserie) == 'nonESG_timeserie.Instrument'] <- 'date'
names(health_nonESG_timeserie)[names(health_nonESG_timeserie) == 'nonESG_timeserie.CLTN.S'] <- 'Coltene Holding AG'
names(health_nonESG_timeserie)[names(health_nonESG_timeserie) == 'nonESG_timeserie.COPN.S'] <- 'Cosmo Pharmaceuticals NV'
health_nonESG_timeserie %>% select(date, everything()) %>%
  arrange(date = as.Date(date, "%d/%m/%Y"))
health_nonESG_timeserie$`Coltene Holding AG` <- as.numeric(health_nonESG_timeserie$`Coltene Holding AG`)
health_nonESG_timeserie$`Cosmo Pharmaceuticals NV` <- as.numeric(health_nonESG_timeserie$`Cosmo Pharmaceuticals NV`)
#create rtrns based on price
colnames(health_nonESG_timeserie)
health_nonESG_timeserie <- health_nonESG_timeserie %>% mutate(`Coltene Holding AG`=100*log(`Coltene Holding AG`/lag(`Coltene Holding AG`)))
health_nonESG_timeserie <- health_nonESG_timeserie %>% mutate(`Cosmo Pharmaceuticals NV`=100*log(`Cosmo Pharmaceuticals NV`/lag(`Cosmo Pharmaceuticals NV`)))

#merge the 2dfs
health_merged <- merge(health_ESG_timeserie, health_nonESG_timeserie, by="date")
health_merged$date <- as.Date(health_merged$date)
#separated lineplot:
p1 <- ggplot(na.omit(health_merged), aes(x=date, group=1)) + 
  geom_line(aes(y = `Roche Holding AG`), color = "#93c572") + ylim(-15,15) +
  scale_x_date(date_labels = "%m-%Y")
p2 <- ggplot(na.omit(health_merged), aes(x=date, group=1)) + 
  geom_line(aes(y = `Lonza Group AG`), color="#6B8E23")  + ylim(-15,15) +
  scale_x_date(date_labels = "%m-%Y")
p3 <- ggplot(na.omit(health_merged), aes(x=date, group=1)) + 
  geom_line(aes(y = `Coltene Holding AG`), color="#FF6347") + ylim(-15,15) +
  scale_x_date(date_labels = "%m-%Y")
p4 <- ggplot(na.omit(health_merged), aes(x=date, group=1)) + 
  geom_line(aes(y = `Cosmo Pharmaceuticals NV`), color="#800000")  + ylim(-15,15) +
  scale_x_date(date_labels = "%m-%Y")
ggarrange(p1, p2, p3, p4, ncol = 2)
# Histogram with kernel density
h1 <- ggplot(na.omit(health_merged), aes(x = `Roche Holding AG`)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white") +
  geom_density(lwd = 1, colour = 3,
               fill = 3, alpha = 0.25)
h2 <- ggplot(na.omit(health_merged), aes(x = `Lonza Group AG`)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white") +
  geom_density(lwd = 1, colour = 3,
               fill = 3, alpha = 0.25)
h3 <- ggplot(na.omit(health_merged), aes(x = `Coltene Holding AG`)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 2, fill = "white") +
  geom_density(lwd = 1, colour = 2,
               fill = 2, alpha = 0.25)
h4 <- ggplot(na.omit(health_merged), aes(x = `Cosmo Pharmaceuticals NV`)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 2, fill = "white") +
  geom_density(lwd = 1, colour = 2,
               fill = 2, alpha = 0.25)
ggarrange(h1, h2, h3, h4, ncol = 2)
#table with std, mean rtrn, sharpe ratio, etc
STOXX600_timeserie <- read.csv("C:/Users/Hadrien Venance/Desktop/Thesis/Data/STOXX600_timeseries_new_dataset.csv")
health_mean <- c(mean(health_merged$`Roche Holding AG`, na.rm = T),mean(health_merged$`Lonza Group AG`, na.rm = T),
               mean(health_merged$`Coltene Holding AG`, na.rm = T),mean(health_merged$`Cosmo Pharmaceuticals NV`, na.rm = T))
health_std <- c(sd(health_merged$`Roche Holding AG`, na.rm = T),sd(health_merged$`Lonza Group AG`, na.rm = T),
              sd(health_merged$`Coltene Holding AG`, na.rm = T),sd(health_merged$`Cosmo Pharmaceuticals NV`, na.rm = T))
#excess ret by substracting ret from STOXX600
STOXX600_timeserie$`rf_STOXX600` <- 100*log(STOXX600_timeserie$`Close.Price`/lag(STOXX600_timeserie$`Close.Price`))
health_excess_ret <- health_merged[,2:5] - STOXX600_timeserie$rf_STOXX600
health_sharpe <- c(round(mean(health_excess_ret$`Roche Holding AG`, na.rm = T)/sd(health_excess_ret$`Roche Holding AG`, na.rm = T),3),round(mean(health_excess_ret$`Lonza Group AG`, na.rm = T)/sd(health_excess_ret$`Lonza Group AG`, na.rm = T),3),
                 round(mean(health_excess_ret$`Coltene Holding AG`, na.rm = T)/sd(health_excess_ret$`Coltene Holding AG`, na.rm = T),3),round(mean(health_excess_ret$`Cosmo Pharmaceuticals NV`, na.rm = T)/sd(health_excess_ret$`Cosmo Pharmaceuticals NV`, na.rm = T),3))
names(STOXX600_timeserie)[names(STOXX600_timeserie) == 'Date'] <- 'date'
STOXX600_timeserie$date <- as.Date(STOXX600_timeserie$date)
health_merged <- merge(health_merged, STOXX600_timeserie, by='date')
health_merged[,-1] <- sapply(health_merged[,-1], as.numeric)
health_merged$date <- as.Date(health_merged$date)
health_xts <- xts(health_merged[,-1], order.by=as.Date(health_merged[,1]))
health_xts <- health_xts[,-6] #remove Instrument column only w/ NAs
health_xts <-  health_xts[complete.cases(health_xts)]
health_infoRatio <- c(InformationRatio(health_xts[,"Roche Holding AG",drop= FALSE]/100, health_xts[,"rf_STOXX600", drop =FALSE]/100),InformationRatio(health_xts[,"Lonza Group AG",drop= FALSE]/100, health_xts[,"rf_STOXX600", drop =FALSE]/100),
                    InformationRatio(health_xts[,"Coltene Holding AG",drop= FALSE]/100, health_xts[,"rf_STOXX600", drop =FALSE]/100),InformationRatio(health_xts[,"Cosmo Pharmaceuticals NV",drop= FALSE]/100, health_xts[,"rf_STOXX600", drop =FALSE]/100))
health_perf <- data.frame(health_mean, health_std, health_sharpe, health_infoRatio)
kable(health_perf, "latex")
#table with different VaR methods
#`Roche Holding AG` <- quantile(health_ESG_timeserie$`Roche Holding AG`,c(0.95,0.99), na.rm = T)
#`Lonza Group AG`<- quantile(health_ESG_timeserie$`Lonza Group AG`,c(0.95,0.99), na.rm = T)
#`Coltene Holding AG` <- quantile(health_nonESG_timeserie$`Coltene Holding AG`, c(0.95,0.99), na.rm = T)
#`Cosmo Pharmaceuticals NV` <- quantile(health_nonESG_timeserie$`Cosmo Pharmaceuticals NV`, c(0.95,0.99), na.rm = T)
# Kernel estimations 
#for (i in 1:T){
#VaR=z[i]
#x=NULL
#Minobj=NULL
#for (j in 1:T) {
 # x[j]=pnorm((z[j]-VaR)/h)}
#Minobj=(sum(x)/T-alpha)^2
#if (Minobj<l) {
 # l=Minobj
  #NonParamVaR=VaR}
#}
#print(NonParamVaR)
# + ADD DRAWDOWNSSS




#industrials:
industrials <- bind_rows(ESG[which(ESG$`ICB Industry name`== "Industrials"),], nonESG[which(nonESG$`ICB Industry name`== "Industrials"),])
#decide which ESG companies will make up the peer group:
industrials_ESG <- industrials[c(4,6),]
#decide which nonESG companies will make up the peer group --> BASED ON Industry, Leverage, and Country
industrials_nonESG <- industrials[c(52,57,61),]
#join them
industrials_peer <- bind_rows(industrials_ESG,industrials_nonESG)
industrials_peer
#keep only columns for benchmarking 
colnames(industrials_peer)
industrials_bench <- industrials_peer[,c(2,4,8,11,17,20,24)]
industrials_bench[,-c(1,2,7)] <- industrials_bench[,-c(1,2,7)]*100
names(industrials_bench)[names(industrials_bench) == 'ROE Total Equity % - 2019\n(FY-2)'] <- 'ROE %'
names(industrials_bench)[names(industrials_bench) == '52 Week Total Return\n(-2CY)'] <- 'Total Return %'
names(industrials_bench)[names(industrials_bench) == 'Operating Margin - % - Y2019\n(FY-2)'] <- 'Operating Margin %'
names(industrials_bench)[names(industrials_bench) == 'ROA Total Assets, Percent - 2019\n(1D, FY-2)'] <- 'ROA Total Assets %'
kable(industrials_bench, "latex")
#keep col for DYNAMIC benchmarking --> t-test of means
#ROA

#Op margin 


industrials_names <- industrials_bench$`Company Name`
industrials_ESG_timeserie <- data_frame(ESG_timeserie$Instrument, ESG_timeserie$MNDI.L, ESG_timeserie$ALFA.ST)
names(industrials_ESG_timeserie)[names(industrials_ESG_timeserie) == 'ESG_timeserie$Instrument'] <- 'date'
names(industrials_ESG_timeserie)[names(industrials_ESG_timeserie) == 'ESG_timeserie$MNDI.L'] <- 'Mondi PLC'
names(industrials_ESG_timeserie)[names(industrials_ESG_timeserie) == 'ESG_timeserie$ALFA.ST'] <- 'Alfa Laval AB'
industrials_ESG_timeserie %>% select(date, everything()) %>%
  arrange(date = as.Date(date, "%d/%m/%Y"))
industrials_ESG_timeserie$`Mondi PLC` <- as.numeric(industrials_ESG_timeserie$`Mondi PLC`)
industrials_ESG_timeserie$`Alfa Laval AB` <- as.numeric(industrials_ESG_timeserie$`Alfa Laval AB`)
#create rtrns based on price
industrials_ESG_timeserie <- industrials_ESG_timeserie %>% mutate(`Mondi PLC`=100*log(`Mondi PLC`/lag(`Mondi PLC`)))
industrials_ESG_timeserie <- industrials_ESG_timeserie %>% mutate(`Alfa Laval AB`=100*log(`Alfa Laval AB`/lag(`Alfa Laval AB`)))

industrials_nonESG_timeserie <- data.frame(nonESG_timeserie$Instrument, nonESG_timeserie$JDG.L, nonESG_timeserie$MRLM.L, 
                                           nonESG_timeserie$JETPAK.ST)
names(industrials_nonESG_timeserie)[names(industrials_nonESG_timeserie) == 'nonESG_timeserie.Instrument'] <- 'date'
names(industrials_nonESG_timeserie)[names(industrials_nonESG_timeserie) == 'nonESG_timeserie.JDG.L'] <- 'Judges Scientific PLC'
names(industrials_nonESG_timeserie)[names(industrials_nonESG_timeserie) == 'nonESG_timeserie.MRLM.L'] <- 'Marlowe PLC'
names(industrials_nonESG_timeserie)[names(industrials_nonESG_timeserie) == 'nonESG_timeserie.JETPAK.ST'] <- 'Jetpak Top Holding AB (publ)'
industrials_nonESG_timeserie %>% select(date, everything()) %>%
  arrange(date = as.Date(date, "%d/%m/%Y"))
industrials_nonESG_timeserie$`Judges Scientific PLC` <- as.numeric(industrials_nonESG_timeserie$`Judges Scientific PLC`)
industrials_nonESG_timeserie$`Marlowe PLC` <- as.numeric(industrials_nonESG_timeserie$`Marlowe PLC`)
industrials_nonESG_timeserie$`Jetpak Top Holding AB (publ)` <- as.numeric(industrials_nonESG_timeserie$`Jetpak Top Holding AB (publ)`)
#create rtrns based on price
colnames(industrials_nonESG_timeserie)
industrials_nonESG_timeserie <- industrials_nonESG_timeserie %>% mutate(`Judges Scientific PLC`=100*log(`Judges Scientific PLC`/lag(`Judges Scientific PLC`)))
industrials_nonESG_timeserie <- industrials_nonESG_timeserie %>% mutate(`Marlowe PLC`=100*log(`Marlowe PLC`/lag(`Marlowe PLC`)))
industrials_nonESG_timeserie <- industrials_nonESG_timeserie %>% mutate(`Jetpak Top Holding AB (publ)`=100*log(`Jetpak Top Holding AB (publ)`/lag(`Jetpak Top Holding AB (publ)`)))

#merge the 2dfs
industrials_merged <- merge(industrials_ESG_timeserie, industrials_nonESG_timeserie, by="date")
industrials_merged$date <- as.Date(industrials_merged$date)
#separated lineplot: 
p1 <- ggplot(na.omit(industrials_merged), aes(x=date, group=1)) + 
  geom_line(aes(y = `Mondi PLC`), color = "#93c572") + ylim(-17,15) +
  scale_x_date(date_labels = "%m-%Y")
p11 <- ggplot(na.omit(industrials_merged), aes(x=date, group=1)) + 
  geom_line(aes(y = `Alfa Laval AB`), color = "#6B8E23") + ylim(-17,15) +
  scale_x_date(date_labels = "%m-%Y")
p2 <- ggplot(na.omit(industrials_merged), aes(x=date, group=1)) + 
  geom_line(aes(y = `Judges Scientific PLC`), color="#FFA07A") + ylim(-17,15) +
  scale_x_date(date_labels = "%m-%Y")
p3 <- ggplot(na.omit(industrials_merged), aes(x=date, group=1)) + 
  geom_line(aes(y = `Marlowe PLC`), color="#FF6347") + ylim(-17,15) +
  scale_x_date(date_labels = "%m-%Y")
p4 <- ggplot(na.omit(industrials_merged), aes(x=date, group=1)) + 
  geom_line(aes(y = `Jetpak Top Holding AB (publ)`), color="#800000") +  ylim(-17,15) +
  scale_x_date(date_labels = "%m-%Y")
ggarrange(p1, p11, p2, p3, p4, ncol = 2)
# Histogram with kernel density
h1 <- ggplot(na.omit(industrials_merged), aes(x = `Mondi PLC`)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white") +
  geom_density(lwd = 1, colour = 3,
               fill = 3, alpha = 0.25)
h11 <- ggplot(na.omit(industrials_merged), aes(x = `Alfa Laval AB`)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white") +
  geom_density(lwd = 1, colour = 3,
               fill = 3, alpha = 0.25)
h2 <- ggplot(na.omit(industrials_merged), aes(x = `Judges Scientific PLC`)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 2, fill = "white") +
  geom_density(lwd = 1, colour = 2,
               fill = 2, alpha = 0.25)
h3 <- ggplot(na.omit(industrials_merged), aes(x = `Marlowe PLC`)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 2, fill = "white") +
  geom_density(lwd = 1, colour = 2,
               fill = 2, alpha = 0.25)
h4 <- ggplot(na.omit(industrials_merged), aes(x = `Jetpak Top Holding AB (publ)`)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 2, fill = "white") +
  geom_density(lwd = 1, colour = 2,
               fill = 2, alpha = 0.25)
ggarrange(h1, h11, h2, h3, h4, ncol = 2)
#table with std, mean rtrn, sharpe ratio, etc
STOXX600_timeserie <- read.csv("C:/Users/Hadrien Venance/Desktop/Thesis/Data/STOXX600_timeseries_new_dataset.csv")
industrials_mean <- c(mean(industrials_merged$`Mondi PLC`, na.rm = T),mean(industrials_merged$`Alfa Laval AB`, na.rm = T),
                 mean(industrials_merged$`Judges Scientific PLC`, na.rm = T),mean(industrials_merged$`Marlowe PLC`, na.rm = T),
                 mean(industrials_merged$`Jetpak Top Holding AB (publ)`, na.rm = T))
industrials_std <- c(sd(industrials_merged$`Mondi PLC`, na.rm = T),sd(industrials_merged$`Alfa Laval AB`, na.rm = T),
                sd(industrials_merged$`Judges Scientific PLC`, na.rm = T),sd(industrials_merged$`Marlowe PLC`, na.rm = T),
                sd(industrials_merged$`Jetpak Top Holding AB (publ)`, na.rm = T))
#excess ret by substracting ret from STOXX600
STOXX600_timeserie$`rf_STOXX600` <- 100*log(STOXX600_timeserie$`Close.Price`/lag(STOXX600_timeserie$`Close.Price`))
industrials_excess_ret <- industrials_merged[,2:6] - STOXX600_timeserie$rf_STOXX600
industrials_sharpe <- c(round(mean(industrials_excess_ret$`Mondi PLC`, na.rm = T)/sd(industrials_excess_ret$`Mondi PLC`, na.rm = T),3),round(mean(industrials_excess_ret$`Alfa Laval AB`, na.rm = T)/sd(industrials_excess_ret$`Alfa Laval AB`, na.rm = T),3),
                   round(mean(industrials_excess_ret$`Judges Scientific PLC`, na.rm = T)/sd(industrials_excess_ret$`Judges Scientific PLC`, na.rm = T),3),round(mean(industrials_excess_ret$`Marlowe PLC`, na.rm = T)/sd(industrials_excess_ret$`Marlowe PLC`, na.rm = T),3),
                   round(mean(industrials_excess_ret$`Jetpak Top Holding AB (publ)`, na.rm = T)/sd(industrials_excess_ret$`Jetpak Top Holding AB (publ)`, na.rm = T),3))
names(STOXX600_timeserie)[names(STOXX600_timeserie) == 'Date'] <- 'date'
STOXX600_timeserie$date <- as.Date(STOXX600_timeserie$date)
industrials_merged <- merge(industrials_merged, STOXX600_timeserie, by='date')
industrials_merged[,-1] <- sapply(industrials_merged[,-1], as.numeric)
industrials_merged$date <- as.Date(industrials_merged$date)
industrials_xts <- xts(industrials_merged[,-1], order.by=as.Date(industrials_merged[,1]))
industrials_xts <- industrials_xts[,-7] #remove Instrument column only w/ NAs
industrials_xts <-  industrials_xts[complete.cases(industrials_xts)]
industrials_infoRatio <- c(InformationRatio(industrials_xts[,"Mondi PLC",drop= FALSE]/100, industrials_xts[,"rf_STOXX600", drop =FALSE]/100),InformationRatio(industrials_xts[,"Alfa Laval AB",drop= FALSE]/100, industrials_xts[,"rf_STOXX600", drop =FALSE]/100),
                      InformationRatio(industrials_xts[,"Judges Scientific PLC",drop= FALSE]/100, industrials_xts[,"rf_STOXX600", drop =FALSE]/100),InformationRatio(industrials_xts[,"Marlowe PLC",drop= FALSE]/100, industrials_xts[,"rf_STOXX600", drop =FALSE]/100),
                      InformationRatio(industrials_xts[,"Jetpak Top Holding AB (publ)",drop= FALSE]/100, industrials_xts[,"rf_STOXX600", drop =FALSE]/100))
industrials_perf <- data.frame(industrials_mean, industrials_std, industrials_sharpe, industrials_infoRatio)
kable(industrials_perf, "latex")



#financials:
financials <- bind_rows(ESG[which(ESG$`ICB Industry name`== "Financials"),], nonESG[which(nonESG$`ICB Industry name`== "Financials"),])
#decide which ESG companies will make up the peer group:
financials_ESG <- financials[c(1,2,3),]
#decide which nonESG companies will make up the peer group --> BASED ON Industry, Leverage, and Country
financials_nonESG <- financials[c(45,37,20,46),]
#join them
financials_peer <- bind_rows(financials_ESG,financials_nonESG)
financials_peer
#keep only columns for benchmarking 
colnames(financials_peer)
financials_bench <- financials_peer[,c(2,4,8,11,17,20,24)]
financials_bench[,-c(1,2,7)] <- financials_bench[,-c(1,2,7)]*100
names(financials_bench)[names(financials_bench) == 'ROE Total Equity % - 2019\n(FY-2)'] <- 'ROE %'
names(financials_bench)[names(financials_bench) == '52 Week Total Return\n(-2CY)'] <- 'Total Return %'
names(financials_bench)[names(financials_bench) == 'Operating Margin - % - Y2019\n(FY-2)'] <- 'Operating Margin %'
names(financials_bench)[names(financials_bench) == 'ROA Total Assets, Percent - 2019\n(1D, FY-2)'] <- 'ROA Total Assets %'
kable(financials_bench, "latex")
#keep col for DYNAMIC benchmarking --> t-test of means
#ROA

#Op margin 


financials_names <- financials_bench$`Company Name`
financials_ESG_timeserie <- data_frame(ESG_timeserie$Instrument, ESG_timeserie$GASI.MI, ESG_timeserie$ALVG.DE, ESG_timeserie$ZURN.S)
names(financials_ESG_timeserie)[names(financials_ESG_timeserie) == 'ESG_timeserie$Instrument'] <- 'date'
names(financials_ESG_timeserie)[names(financials_ESG_timeserie) == 'ESG_timeserie$GASI.MI'] <- 'Assicurazioni Generali SpA'
names(financials_ESG_timeserie)[names(financials_ESG_timeserie) == 'ESG_timeserie$ALVG.DE'] <- 'Allianz SE'
names(financials_ESG_timeserie)[names(financials_ESG_timeserie) == 'ESG_timeserie$ZURN.S'] <- 'Zurich Insurance Group AG'
financials_ESG_timeserie %>% select(date, everything()) %>%
  arrange(date = as.Date(date, "%d/%m/%Y"))
financials_ESG_timeserie$`Assicurazioni Generali SpA` <- as.numeric(financials_ESG_timeserie$`Assicurazioni Generali SpA`)
financials_ESG_timeserie$`Allianz SE` <- as.numeric(financials_ESG_timeserie$`Allianz SE`)
financials_ESG_timeserie$`Zurich Insurance Group AG` <- as.numeric(financials_ESG_timeserie$`Zurich Insurance Group AG`)
#create rtrns based on price
financials_ESG_timeserie <- financials_ESG_timeserie %>% mutate(`Assicurazioni Generali SpA`=100*log(`Assicurazioni Generali SpA`/lag(`Assicurazioni Generali SpA`)))
financials_ESG_timeserie <- financials_ESG_timeserie %>% mutate(`Allianz SE`=100*log(`Allianz SE`/lag(`Allianz SE`)))
financials_ESG_timeserie <- financials_ESG_timeserie %>% mutate(`Zurich Insurance Group AG`=100*log(`Zurich Insurance Group AG`/lag(`Zurich Insurance Group AG`)))

financials_nonESG_timeserie <- data.frame(nonESG_timeserie$Instrument, nonESG_timeserie$TIP.MI, nonESG_timeserie$FTKn.DE, nonESG_timeserie$SQN.S, nonESG_timeserie$PEHN.S)
names(financials_nonESG_timeserie)[names(financials_nonESG_timeserie) == 'nonESG_timeserie.Instrument'] <- 'date'
names(financials_nonESG_timeserie)[names(financials_nonESG_timeserie) == 'nonESG_timeserie.TIP.MI'] <- 'Tamburi Investment Partners SpA'
names(financials_nonESG_timeserie)[names(financials_nonESG_timeserie) == 'nonESG_timeserie.FTKn.DE'] <- 'flatexDEGIRO AG'
names(financials_nonESG_timeserie)[names(financials_nonESG_timeserie) == 'nonESG_timeserie.SQN.S'] <- 'Swissquote Group Holding SA'
names(financials_nonESG_timeserie)[names(financials_nonESG_timeserie) == 'nonESG_timeserie.PEHN.S'] <- 'Private Equity Holding AG'
financials_nonESG_timeserie %>% select(date, everything()) %>%
  arrange(date = as.Date(date, "%d/%m/%Y"))
financials_nonESG_timeserie$`Tamburi Investment Partners SpA` <- as.numeric(financials_nonESG_timeserie$`Tamburi Investment Partners SpA`)
financials_nonESG_timeserie$`flatexDEGIRO AG` <- as.numeric(financials_nonESG_timeserie$`flatexDEGIRO AG`)
financials_nonESG_timeserie$`Swissquote Group Holding SA` <- as.numeric(financials_nonESG_timeserie$`Swissquote Group Holding SA`)
financials_nonESG_timeserie$`Private Equity Holding AG` <- as.numeric(financials_nonESG_timeserie$`Private Equity Holding AG`)
#create rtrns based on price
colnames(financials_nonESG_timeserie)
financials_nonESG_timeserie <- financials_nonESG_timeserie %>% mutate(`Tamburi Investment Partners SpA`=100*log(`Tamburi Investment Partners SpA`/lag(`Tamburi Investment Partners SpA`)))
financials_nonESG_timeserie <- financials_nonESG_timeserie %>% mutate(`flatexDEGIRO AG`=100*log(`flatexDEGIRO AG`/lag(`flatexDEGIRO AG`)))
financials_nonESG_timeserie <- financials_nonESG_timeserie %>% mutate(`Swissquote Group Holding SA`=100*log(`Swissquote Group Holding SA`/lag(`Swissquote Group Holding SA`)))
financials_nonESG_timeserie <- financials_nonESG_timeserie %>% mutate(`Private Equity Holding AG`=100*log(`Private Equity Holding AG`/lag(`Private Equity Holding AG`)))

#merge the 2dfs
financials_merged <- merge(financials_ESG_timeserie, financials_nonESG_timeserie, by="date")
financials_merged$date <- as.Date(financials_merged$date)
#separated lineplot:
p1 <- ggplot(na.omit(financials_merged), aes(x=date, group=1)) + 
  geom_line(aes(y = `Assicurazioni Generali SpA`), color = "#93c572") + ylim(-17,17)+
  scale_x_date(date_labels = "%m-%Y")
p11 <- ggplot(na.omit(financials_merged), aes(x=date, group=1)) + 
  geom_line(aes(y = `Allianz SE`), color = "#6B8E23") + ylim(-17,17)+
  scale_x_date(date_labels = "%m-%Y")
p111 <- ggplot(na.omit(financials_merged), aes(x=date, group=1)) + 
  geom_line(aes(y = `Zurich Insurance Group AG`), color = "#00FA9A") +  ylim(-17,17)+
  scale_x_date(date_labels = "%m-%Y")
p2 <- ggplot(na.omit(financials_merged), aes(x=date, group=1)) + 
  geom_line(aes(y = `Tamburi Investment Partners SpA`), color="#FFA07A") +  ylim(-17,17)+
  scale_x_date(date_labels = "%m-%Y")
p3 <- ggplot(na.omit(financials_merged), aes(x=date, group=1)) + 
  geom_line(aes(y = `flatexDEGIRO AG`), color="#FF6347") +   ylim(-17,17)+
  scale_x_date(date_labels = "%m-%Y")
p4 <- ggplot(na.omit(financials_merged), aes(x=date, group=1)) + 
  geom_line(aes(y = `Swissquote Group Holding SA`), color="#800000") +  ylim(-17,17)+
  scale_x_date(date_labels = "%m-%Y")
p44 <- ggplot(na.omit(financials_merged), aes(x=date, group=1)) + 
  geom_line(aes(y = `Private Equity Holding AG`), color="#FA8072") + ylim(-17,17)+
  scale_x_date(date_labels = "%m-%Y")
ggarrange(p1, p11, p111, p2, p3, p4, p44, ncol = 2)
# Histogram with kernel density
h1 <- ggplot(na.omit(financials_merged), aes(x = `Assicurazioni Generali SpA`)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white") +
  geom_density(lwd = 1, colour = 3,
               fill = 3, alpha = 0.25)
h11 <- ggplot(na.omit(financials_merged), aes(x = `Allianz SE`)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white") +
  geom_density(lwd = 1, colour = 3,
               fill = 3, alpha = 0.25)
h111 <- ggplot(na.omit(financials_merged), aes(x = `Zurich Insurance Group AG`)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white") +
  geom_density(lwd = 1, colour = 3,
               fill = 3, alpha = 0.25)
h2 <- ggplot(na.omit(financials_merged), aes(x = `Tamburi Investment Partners SpA`)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 2, fill = "white") +
  geom_density(lwd = 1, colour = 2,
               fill = 2, alpha = 0.25)
h3 <- ggplot(na.omit(financials_merged), aes(x = `flatexDEGIRO AG`)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 2, fill = "white") +
  geom_density(lwd = 1, colour = 2,
               fill = 2, alpha = 0.25)
h4 <- ggplot(na.omit(financials_merged), aes(x = `Swissquote Group Holding SA`)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 2, fill = "white") +
  geom_density(lwd = 1, colour = 2,
               fill = 2, alpha = 0.25)
h44 <- ggplot(na.omit(financials_merged), aes(x = `Private Equity Holding AG`)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 2, fill = "white") +
  geom_density(lwd = 1, colour = 2,
               fill = 2, alpha = 0.25)
ggarrange(h1, h11,h111, h2, h3, h4, h44, ncol = 2)
#table with std, mean rtrn, sharpe ratio, etc
STOXX600_timeserie <- read.csv("C:/Users/Hadrien Venance/Desktop/Thesis/Data/STOXX600_timeseries_new_dataset.csv")
financials_mean <- c(mean(financials_merged$`Assicurazioni Generali SpA`, na.rm = T),mean(financials_merged$`Allianz SE`, na.rm = T),
                      mean(financials_merged$`Zurich Insurance Group AG`, na.rm = T),mean(financials_merged$`Tamburi Investment Partners SpA`, na.rm = T),
                      mean(financials_merged$`flatexDEGIRO AG`, na.rm = T), mean(financials_merged$`Swissquote Group Holding SA`, na.rm = T),
                     mean(financials_merged$`Private Equity Holding AG`, na.rm = T))
financials_std <- c(sd(financials_merged$`Assicurazioni Generali SpA`, na.rm = T),sd(financials_merged$`Allianz SE`, na.rm = T),
                     sd(financials_merged$`Zurich Insurance Group AG`, na.rm = T),sd(financials_merged$`Tamburi Investment Partners SpA`, na.rm = T),
                     sd(financials_merged$`flatexDEGIRO AG`, na.rm = T), sd(financials_merged$`Swissquote Group Holding SA`, na.rm = T),
                    sd(financials_merged$`Private Equity Holding AG`, na.rm = T))
#excess ret by substracting ret from STOXX600
STOXX600_timeserie$`rf_STOXX600` <- 100*log(STOXX600_timeserie$`Close.Price`/lag(STOXX600_timeserie$`Close.Price`))
financials_excess_ret <- financials_merged[,2:8] - STOXX600_timeserie$rf_STOXX600
financials_sharpe <- c(round(mean(financials_excess_ret$`Assicurazioni Generali SpA`, na.rm = T)/sd(financials_excess_ret$`Assicurazioni Generali SpA`, na.rm = T),3),round(mean(financials_excess_ret$`Allianz SE`, na.rm = T)/sd(financials_excess_ret$`Allianz SE`, na.rm = T),3),
                        round(mean(financials_excess_ret$`Zurich Insurance Group AG`, na.rm = T)/sd(financials_excess_ret$`Zurich Insurance Group AG`, na.rm = T),3),round(mean(financials_excess_ret$`Tamburi Investment Partners SpA`, na.rm = T)/sd(financials_excess_ret$`Tamburi Investment Partners SpA`, na.rm = T),3),
                        round(mean(financials_excess_ret$`flatexDEGIRO AG`, na.rm = T)/sd(financials_excess_ret$`flatexDEGIRO AG`, na.rm = T),3),
                       round(mean(financials_excess_ret$`Swissquote Group Holding SA`, na.rm = T)/sd(financials_excess_ret$`Swissquote Group Holding SA`, na.rm = T),3),
                       round(mean(financials_excess_ret$`Private Equity Holding AG`, na.rm = T)/sd(financials_excess_ret$`Private Equity Holding AG`, na.rm = T),3))
names(STOXX600_timeserie)[names(STOXX600_timeserie) == 'Date'] <- 'date'
STOXX600_timeserie$date <- as.Date(STOXX600_timeserie$date)
financials_merged <- merge(financials_merged, STOXX600_timeserie, by='date')
financials_merged[,-1] <- sapply(financials_merged[,-1], as.numeric)
financials_merged$date <- as.Date(financials_merged$date)
financials_xts <- xts(financials_merged[,-1], order.by=as.Date(financials_merged[,1]))
financials_xts <- financials_xts[,-9] #remove Instrument column only w/ NAs
financials_xts <-  financials_xts[complete.cases(financials_xts)]
financials_infoRatio <- c(InformationRatio(financials_xts[,"Assicurazioni Generali SpA",drop= FALSE]/100, financials_xts[,"rf_STOXX600", drop =FALSE]/100),InformationRatio(financials_xts[,"Allianz SE",drop= FALSE]/100, financials_xts[,"rf_STOXX600", drop =FALSE]/100),
                           InformationRatio(financials_xts[,"Zurich Insurance Group AG",drop= FALSE]/100, financials_xts[,"rf_STOXX600", drop =FALSE]/100),InformationRatio(financials_xts[,"Tamburi Investment Partners SpA",drop= FALSE]/100, financials_xts[,"rf_STOXX600", drop =FALSE]/100),
                           InformationRatio(financials_xts[,"flatexDEGIRO AG",drop= FALSE]/100, financials_xts[,"rf_STOXX600", drop =FALSE]/100), InformationRatio(financials_xts[,"Swissquote Group Holding SA",drop= FALSE]/100, financials_xts[,"rf_STOXX600", drop =FALSE]/100),
                          InformationRatio(financials_xts[,"Private Equity Holding AG",drop= FALSE]/100, financials_xts[,"rf_STOXX600", drop =FALSE]/100))
financials_perf <- data.frame(financials_mean, financials_std, financials_sharpe, financials_infoRatio)
kable(financials_perf, "latex")




#utilities:
utilities <- bind_rows(ESG[which(ESG$`ICB Industry name`== "Utilities"),], nonESG[which(nonESG$`ICB Industry name`== "Utilities"),])
#decide which ESG companies will make up the peer group:
utilities_ESG <- utilities[c(2,3,4),]
#decide which nonESG companies will make up the peer group --> BASED ON Industry, Leverage, and Country
utilities_nonESG <- utilities[6,]
#join them
utilities_peer <- bind_rows(utilities_ESG,utilities_nonESG)
utilities_peer
#keep only columns for benchmarking 
colnames(utilities_peer)
utilities_bench <- utilities_peer[,c(2,4,8,11,17,20,24)]
utilities_bench[,-c(1,2,7)] <- utilities_bench[,-c(1,2,7)]*100
names(utilities_bench)[names(utilities_bench) == 'ROE Total Equity % - 2019\n(FY-2)'] <- 'ROE %'
names(utilities_bench)[names(utilities_bench) == '52 Week Total Return\n(-2CY)'] <- 'Total Return %'
names(utilities_bench)[names(utilities_bench) == 'Operating Margin - % - Y2019\n(FY-2)'] <- 'Operating Margin %'
names(utilities_bench)[names(utilities_bench) == 'ROA Total Assets, Percent - 2019\n(1D, FY-2)'] <- 'ROA Total Assets %'
kable(utilities_bench, "latex")
#keep col for DYNAMIC benchmarking --> t-test of means
#ROA

#Op margin 

utilities_names <- utilities_bench$`Company Name`
utilities_ESG_timeserie <- data_frame(ESG_timeserie$Instrument, ESG_timeserie$IBE.MC, ESG_timeserie$NTGY.MC, ESG_timeserie$REDE.MC)
names(utilities_ESG_timeserie)[names(utilities_ESG_timeserie) == 'ESG_timeserie$Instrument'] <- 'date'
names(utilities_ESG_timeserie)[names(utilities_ESG_timeserie) == 'ESG_timeserie$IBE.MC'] <- 'Iberdrola SA'
names(utilities_ESG_timeserie)[names(utilities_ESG_timeserie) == 'ESG_timeserie$NTGY.MC'] <- 'Naturgy Energy Group SA'
names(utilities_ESG_timeserie)[names(utilities_ESG_timeserie) == 'ESG_timeserie$REDE.MC'] <- 'Red Electrica Corporacion SA'
utilities_ESG_timeserie %>% select(date, everything()) %>%
  arrange(date = as.Date(date, "%d/%m/%Y"))
utilities_ESG_timeserie$`Iberdrola SA` <- as.numeric(utilities_ESG_timeserie$`Iberdrola SA`)
utilities_ESG_timeserie$`Naturgy Energy Group SA` <- as.numeric(utilities_ESG_timeserie$`Naturgy Energy Group SA`)
utilities_ESG_timeserie$`Red Electrica Corporacion SA` <- as.numeric(utilities_ESG_timeserie$`Red Electrica Corporacion SA`)
#create rtrns based on price
utilities_ESG_timeserie <- utilities_ESG_timeserie %>% mutate(`Iberdrola SA`=100*log(`Iberdrola SA`/lag(`Iberdrola SA`)))
utilities_ESG_timeserie <- utilities_ESG_timeserie %>% mutate(`Naturgy Energy Group SA`=100*log(`Naturgy Energy Group SA`/lag(`Naturgy Energy Group SA`)))
utilities_ESG_timeserie <- utilities_ESG_timeserie %>% mutate(`Red Electrica Corporacion SA`=100*log(`Red Electrica Corporacion SA`/lag(`Red Electrica Corporacion SA`)))

utilities_nonESG_timeserie <- data.frame(nonESG_timeserie$Instrument, nonESG_timeserie$SLRS.MC)
names(utilities_nonESG_timeserie)[names(utilities_nonESG_timeserie) == 'nonESG_timeserie.Instrument'] <- 'date'
names(utilities_nonESG_timeserie)[names(utilities_nonESG_timeserie) == 'nonESG_timeserie.SLRS.MC'] <- 'Solaria Energia y Medio Ambiente SA'
utilities_nonESG_timeserie %>% select(date, everything()) %>%
  arrange(date = as.Date(date, "%d/%m/%Y"))
utilities_nonESG_timeserie$`Solaria Energia y Medio Ambiente SA` <- as.numeric(utilities_nonESG_timeserie$`Solaria Energia y Medio Ambiente SA`)
#create rtrns based on price
colnames(utilities_nonESG_timeserie)
utilities_nonESG_timeserie <- utilities_nonESG_timeserie %>% mutate(`Solaria Energia y Medio Ambiente SA`=100*log(`Solaria Energia y Medio Ambiente SA`/lag(`Solaria Energia y Medio Ambiente SA`)))

#merge the 2dfs
utilities_merged <- merge(utilities_ESG_timeserie, utilities_nonESG_timeserie, by="date")
utilities_merged$date <- as.Date(utilities_merged$date)
#separated lineplot:
p1 <- ggplot(na.omit(utilities_merged), aes(x=date, group=1)) + 
  geom_line(aes(y = `Iberdrola SA`), color = "#93c572") + ylim(-25,15)+
  scale_x_date(date_labels = "%m-%Y")
p11 <- ggplot(na.omit(utilities_merged), aes(x=date, group=1)) + 
  geom_line(aes(y = `Naturgy Energy Group SA`), color = "#6B8E23") + ylim(-25,15)+
  scale_x_date(date_labels = "%m-%Y")
p111 <- ggplot(na.omit(utilities_merged), aes(x=date, group=1)) + 
  geom_line(aes(y = `Red Electrica Corporacion SA`), color = "#00FA9A") + ylim(-25,15)+
  scale_x_date(date_labels = "%m-%Y")
p2 <- ggplot(na.omit(utilities_merged), aes(x=date, group=1)) + 
  geom_line(aes(y = `Solaria Energia y Medio Ambiente SA`), color="#FFA07A") + ylim(-25,15)+
  scale_x_date(date_labels = "%m-%Y")
ggarrange(p1, p11, p111, p2, ncol = 2)
# Histogram with kernel density
h1 <- ggplot(na.omit(utilities_merged), aes(x = `Iberdrola SA`)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white") +
  geom_density(lwd = 1, colour = 3,
               fill = 3, alpha = 0.25)
h11 <- ggplot(na.omit(utilities_merged), aes(x = `Naturgy Energy Group SA`)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white") +
  geom_density(lwd = 1, colour = 3,
               fill = 3, alpha = 0.25)
h111 <- ggplot(na.omit(utilities_merged), aes(x = `Red Electrica Corporacion SA`)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white") +
  geom_density(lwd = 1, colour = 3,
               fill = 3, alpha = 0.25)
h2 <- ggplot(na.omit(utilities_merged), aes(x = `Solaria Energia y Medio Ambiente SA`)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 2, fill = "white") +
  geom_density(lwd = 1, colour = 2,
               fill = 2, alpha = 0.25)
ggarrange(h1, h11, h111, h2, ncol = 2)
#table with std, mean rtrn, sharpe ratio, etc
STOXX600_timeserie <- read.csv("C:/Users/Hadrien Venance/Desktop/Thesis/Data/STOXX600_timeseries_new_dataset.csv")
utilities_mean <- c(mean(utilities_merged$`Iberdrola SA`, na.rm = T),mean(utilities_merged$`Naturgy Energy Group SA`, na.rm = T),
               mean(utilities_merged$`Red Electrica Corporacion SA`, na.rm = T),mean(utilities_merged$`Solaria Energia y Medio Ambiente SA`, na.rm = T))
utilities_std <- c(sd(utilities_merged$`Iberdrola SA`, na.rm = T),sd(utilities_merged$`Naturgy Energy Group SA`, na.rm = T),
              sd(utilities_merged$`Red Electrica Corporacion SA`, na.rm = T),sd(utilities_merged$`Solaria Energia y Medio Ambiente SA`, na.rm = T))
#excess ret by substracting ret from STOXX600
STOXX600_timeserie$`rf_STOXX600` <- 100*log(STOXX600_timeserie$`Close.Price`/lag(STOXX600_timeserie$`Close.Price`))
utilities_excess_ret <- utilities_merged[,2:5] - STOXX600_timeserie$rf_STOXX600
utilities_sharpe <- c(round(mean(utilities_excess_ret$`Iberdrola SA`, na.rm = T)/sd(utilities_excess_ret$`Iberdrola SA`, na.rm = T),3),round(mean(utilities_excess_ret$`Naturgy Energy Group SA`, na.rm = T)/sd(utilities_excess_ret$`Naturgy Energy Group SA`, na.rm = T),3),
                 round(mean(utilities_excess_ret$`Red Electrica Corporacion SA`, na.rm = T)/sd(utilities_excess_ret$`Red Electrica Corporacion SA`, na.rm = T),3),round(mean(utilities_excess_ret$`Solaria Energia y Medio Ambiente SA`, na.rm = T)/sd(utilities_excess_ret$`Solaria Energia y Medio Ambiente SA`, na.rm = T),3))
names(STOXX600_timeserie)[names(STOXX600_timeserie) == 'Date'] <- 'date'
STOXX600_timeserie$date <- as.Date(STOXX600_timeserie$date)
utilities_merged <- merge(utilities_merged, STOXX600_timeserie, by='date')
utilities_merged[,-1] <- sapply(utilities_merged[,-1], as.numeric)
utilities_merged$date <- as.Date(utilities_merged$date)
utilities_xts <- xts(utilities_merged[,-1], order.by=as.Date(utilities_merged[,1]))
utilities_xts <- utilities_xts[,-6] #remove Instrument column only w/ NAs
utilities_xts <-  utilities_xts[complete.cases(utilities_xts)]
utilities_infoRatio <- c(InformationRatio(utilities_xts[,"Iberdrola SA",drop= FALSE]/100, utilities_xts[,"rf_STOXX600", drop =FALSE]/100),InformationRatio(utilities_xts[,"Naturgy Energy Group SA",drop= FALSE]/100, utilities_xts[,"rf_STOXX600", drop =FALSE]/100),
                    InformationRatio(utilities_xts[,"Red Electrica Corporacion SA",drop= FALSE]/100, utilities_xts[,"rf_STOXX600", drop =FALSE]/100),InformationRatio(utilities_xts[,"Solaria Energia y Medio Ambiente SA",drop= FALSE]/100, utilities_xts[,"rf_STOXX600", drop =FALSE]/100))
utilities_perf <- data.frame(utilities_mean, utilities_std, utilities_sharpe, utilities_infoRatio)
kable(utilities_perf, "latex")



#estate:
estate <- bind_rows(ESG[which(ESG$`ICB Industry name`== "Real Estate"),], nonESG[which(nonESG$`ICB Industry name`== "Real Estate"),])
#decide which ESG companies will make up the peer group:
estate_ESG <- estate[c(3,9),]
#decide which nonESG companies will make up the peer group --> BASED ON Industry, Leverage, and Country
estate_nonESG <- estate[c(16),]
#join them
estate_peer <- bind_rows(estate_ESG,estate_nonESG)
estate_peer
#keep only columns for benchmarking 
colnames(estate_peer)
estate_bench <- estate_peer[,c(2,4,8,11,17,20,24)]
estate_bench[,-c(1,2,4,7)] <- estate_bench[,-c(1,2,4,7)]*100
names(estate_bench)[names(estate_bench) == 'ROE Total Equity % - 2019\n(FY-2)'] <- 'ROE %'
names(estate_bench)[names(estate_bench) == '52 Week Total Return\n(-2CY)'] <- 'Total Return %'
names(estate_bench)[names(estate_bench) == 'Operating Margin - % - Y2019\n(FY-2)'] <- 'Operating Margin %'
names(estate_bench)[names(estate_bench) == 'ROA Total Assets, Percent - 2019\n(1D, FY-2)'] <- 'ROA Total Assets %'
kable(estate_bench, "latex")
#keep col for DYNAMIC benchmarking --> t-test of means
#ROA

#Op margin 

estate_names <- estate_bench$`Company Name`
colnames(ESG_timeserie) <- ESG_timeserie[1,]
ESG_timeserie <- ESG_timeserie[-c(1:2),]
estate_ESG_timeserie <- data_frame(ESG_timeserie$Instrument, ESG_timeserie$SAPG.DE)
names(estate_ESG_timeserie)[names(estate_ESG_timeserie) == 'ESG_timeserie$Instrument'] <- 'date'
names(estate_ESG_timeserie)[names(estate_ESG_timeserie) == 'ESG_timeserie$SAPG.DE'] <- 'SAP SE'
estate_ESG_timeserie %>% select(date, everything()) %>%
  arrange(date = as.Date(date, "%d/%m/%Y"))
estate_ESG_timeserie$`SAP SE` <- as.numeric(estate_ESG_timeserie$`SAP SE`)
#create rtrns based on price
estate_ESG_timeserie <- estate_ESG_timeserie %>% mutate(`SAP SE`=100*log(`SAP SE`/lag(`SAP SE`)))

colnames(nonESG_timeserie) <- nonESG_timeserie[1,]
nonESG_timeserie <- nonESG_timeserie[-c(1:2),]
estate_nonESG_timeserie <- data.frame(nonESG_timeserie$Instrument, nonESG_timeserie$A1OS.DE, nonESG_timeserie$SMHNn.DE, nonESG_timeserie$PSAGn.DE)
names(estate_nonESG_timeserie)[names(estate_nonESG_timeserie) == 'nonESG_timeserie.Instrument'] <- 'date'
names(estate_nonESG_timeserie)[names(estate_nonESG_timeserie) == 'nonESG_timeserie.A1OS.DE'] <- 'All for One Group SE'
names(estate_nonESG_timeserie)[names(estate_nonESG_timeserie) == 'nonESG_timeserie.SMHNn.DE'] <- 'SUESS MicroTec SE'
names(estate_nonESG_timeserie)[names(estate_nonESG_timeserie) == 'nonESG_timeserie.PSAGn.DE'] <- 'PSI Software AG'
estate_nonESG_timeserie %>% select(date, everything()) %>%
  arrange(date = as.Date(date, "%d/%m/%Y"))
estate_nonESG_timeserie$`All for One Group SE` <- as.numeric(estate_nonESG_timeserie$`All for One Group SE`)
estate_nonESG_timeserie$`SUESS MicroTec SE` <- as.numeric(estate_nonESG_timeserie$`SUESS MicroTec SE`)
estate_nonESG_timeserie$`PSI Software AG` <- as.numeric(estate_nonESG_timeserie$`PSI Software AG`)
#create rtrns based on price
colnames(estate_nonESG_timeserie)
estate_nonESG_timeserie <- estate_nonESG_timeserie %>% mutate(`All for One Group SE`=100*log(`All for One Group SE`/lag(`All for One Group SE`)))
estate_nonESG_timeserie <- estate_nonESG_timeserie %>% mutate(`SUESS MicroTec SE`=100*log(`SUESS MicroTec SE`/lag(`SUESS MicroTec SE`)))
estate_nonESG_timeserie <- estate_nonESG_timeserie %>% mutate(`PSI Software AG`=100*log(`PSI Software AG`/lag(`PSI Software AG`)))

#merge the 2dfs
estate_merged <- merge(estate_ESG_timeserie, estate_nonESG_timeserie, by="date")
estate_merged$date <- as.Date(estate_merged$date)
#separated lineplot:
p1 <- ggplot(na.omit(estate_merged), aes(x=date, group=1)) + 
  geom_line(aes(y = `SAP SE`), color = "#93c572") + ylim(-0.25,0.2) + 
  scale_x_date(date_labels = "%m-%Y")
p2 <- ggplot(na.omit(estate_merged), aes(x=date, group=1)) + 
  geom_line(aes(y = `All for One Group SE`), color="#FFA07A") + ylim(-0.25,0.2) + 
  scale_x_date(date_labels = "%m-%Y")
p3 <- ggplot(na.omit(estate_merged), aes(x=date, group=1)) + 
  geom_line(aes(y = `SUESS MicroTec SE`), color="#FF6347") + ylim(-0.25,0.2) + 
  scale_x_date(date_labels = "%m-%Y")
p4 <- ggplot(na.omit(estate_merged), aes(x=date, group=1)) + 
  geom_line(aes(y = `PSI Software AG`), color="#800000") + ylim(-0.25,0.2) + 
  scale_x_date(date_labels = "%m-%Y")
ggarrange(p1, p2, p3, p4, ncol = 2)
# Histogram with kernel density
h1 <- ggplot(na.omit(estate_merged), aes(x = `SAP SE`)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white") +
  geom_density(lwd = 1, colour = 3,
               fill = 3, alpha = 0.25)
h2 <- ggplot(na.omit(estate_merged), aes(x = `All for One Group SE`)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 2, fill = "white") +
  geom_density(lwd = 1, colour = 2,
               fill = 2, alpha = 0.25)
h3 <- ggplot(na.omit(estate_merged), aes(x = `SUESS MicroTec SE`)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 2, fill = "white") +
  geom_density(lwd = 1, colour = 2,
               fill = 2, alpha = 0.25)
h4 <- ggplot(na.omit(estate_merged), aes(x = `PSI Software AG`)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 2, fill = "white") +
  geom_density(lwd = 1, colour = 2,
               fill = 2, alpha = 0.25)
ggarrange(h1, h2, h3, h4, ncol = 2)



#staples:
staples <- bind_rows(ESG[which(ESG$`ICB Industry name`== "Consumer Staples"),], nonESG[which(nonESG$`ICB Industry name`== "Consumer Staples"),])
#decide which ESG companies will make up the peer group:
staples_ESG <- staples[c(2,3),]
#decide which nonESG companies will make up the peer group --> BASED ON Industry, Leverage, and Country
staples_nonESG <- staples[c(11,12),]
#join them
staples_peer <- bind_rows(staples_ESG,staples_nonESG)
staples_peer
#keep only columns for benchmarking 
colnames(staples_peer)
staples_bench <- staples_peer[,c(2,4,8,11,17,20,24)]
staples_bench[,-c(1,2,4,7)] <- staples_bench[,-c(1,2,4,7)]*100
names(staples_bench)[names(staples_bench) == 'ROE Total Equity % - 2019\n(FY-2)'] <- 'ROE %'
names(staples_bench)[names(staples_bench) == '52 Week Total Return\n(-2CY)'] <- 'Total Return %'
names(staples_bench)[names(staples_bench) == 'Operating Margin - % - Y2019\n(FY-2)'] <- 'Operating Margin %'
names(staples_bench)[names(staples_bench) == 'ROA Total Assets, Percent - 2019\n(1D, FY-2)'] <- 'ROA Total Assets %'
kable(staples_bench, "latex")
#keep col for DYNAMIC benchmarking --> t-test of means
#ROA

#Op margin 


staples_names <- staples_bench$`Company Name`
colnames(ESG_timeserie) <- ESG_timeserie[1,]
ESG_timeserie <- ESG_timeserie[-c(1:2),]
staples_ESG_timeserie <- data_frame(ESG_timeserie$Instrument, ESG_timeserie$SAPG.DE)
names(staples_ESG_timeserie)[names(staples_ESG_timeserie) == 'ESG_timeserie$Instrument'] <- 'date'
names(staples_ESG_timeserie)[names(staples_ESG_timeserie) == 'ESG_timeserie$SAPG.DE'] <- 'SAP SE'
staples_ESG_timeserie %>% select(date, everything()) %>%
  arrange(date = as.Date(date, "%d/%m/%Y"))
staples_ESG_timeserie$`SAP SE` <- as.numeric(staples_ESG_timeserie$`SAP SE`)
#create rtrns based on price
staples_ESG_timeserie <- staples_ESG_timeserie %>% mutate(`SAP SE`=100*log(`SAP SE`/lag(`SAP SE`)))

staples_nonESG_timeserie <- data.frame(nonESG_timeserie$Instrument, nonESG_timeserie$A1OS.DE, nonESG_timeserie$SMHNn.DE, nonESG_timeserie$PSAGn.DE)
names(staples_nonESG_timeserie)[names(staples_nonESG_timeserie) == 'nonESG_timeserie.Instrument'] <- 'date'
names(staples_nonESG_timeserie)[names(staples_nonESG_timeserie) == 'nonESG_timeserie.A1OS.DE'] <- 'All for One Group SE'
names(staples_nonESG_timeserie)[names(staples_nonESG_timeserie) == 'nonESG_timeserie.SMHNn.DE'] <- 'SUESS MicroTec SE'
names(staples_nonESG_timeserie)[names(staples_nonESG_timeserie) == 'nonESG_timeserie.PSAGn.DE'] <- 'PSI Software AG'
staples_nonESG_timeserie %>% select(date, everything()) %>%
  arrange(date = as.Date(date, "%d/%m/%Y"))
staples_nonESG_timeserie$`All for One Group SE` <- as.numeric(staples_nonESG_timeserie$`All for One Group SE`)
staples_nonESG_timeserie$`SUESS MicroTec SE` <- as.numeric(staples_nonESG_timeserie$`SUESS MicroTec SE`)
staples_nonESG_timeserie$`PSI Software AG` <- as.numeric(staples_nonESG_timeserie$`PSI Software AG`)
#create rtrns based on price
colnames(staples_nonESG_timeserie)
staples_nonESG_timeserie <- staples_nonESG_timeserie %>% mutate(`All for One Group SE`=100*log(`All for One Group SE`/lag(`All for One Group SE`)))
staples_nonESG_timeserie <- staples_nonESG_timeserie %>% mutate(`SUESS MicroTec SE`=100*log(`SUESS MicroTec SE`/lag(`SUESS MicroTec SE`)))
staples_nonESG_timeserie <- staples_nonESG_timeserie %>% mutate(`PSI Software AG`=100*log(`PSI Software AG`/lag(`PSI Software AG`)))

#merge the 2dfs
staples_merged <- merge(staples_ESG_timeserie, staples_nonESG_timeserie, by="date")
staples_merged$date <- as.Date(staples_merged$date)
#separated lineplot:
p1 <- ggplot(na.omit(staples_merged), aes(x=date, group=1)) + 
  geom_line(aes(y = `SAP SE`), color = "#93c572") + ylim(-0.25,0.2) + 
  scale_x_date(date_labels = "%m-%Y")
p2 <- ggplot(na.omit(staples_merged), aes(x=date, group=1)) + 
  geom_line(aes(y = `All for One Group SE`), color="#FFA07A") + ylim(-0.25,0.2) + 
  scale_x_date(date_labels = "%m-%Y")
p3 <- ggplot(na.omit(staples_merged), aes(x=date, group=1)) + 
  geom_line(aes(y = `SUESS MicroTec SE`), color="#FF6347") + ylim(-0.25,0.2) + 
  scale_x_date(date_labels = "%m-%Y")
p4 <- ggplot(na.omit(staples_merged), aes(x=date, group=1)) + 
  geom_line(aes(y = `PSI Software AG`), color="#800000") + ylim(-0.25,0.2) + 
  scale_x_date(date_labels = "%m-%Y")
ggarrange(p1, p2, p3, p4, ncol = 2)
# Histogram with kernel density
h1 <- ggplot(na.omit(staples_merged), aes(x = `SAP SE`)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white") +
  geom_density(lwd = 1, colour = 3,
               fill = 3, alpha = 0.25)
h11 <- ggplot(na.omit(utilities_merged), aes(x = `Naturgy Energy Group SA`)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white") +
  geom_density(lwd = 1, colour = 3,
               fill = 3, alpha = 0.25)
h111 <- ggplot(na.omit(utilities_merged), aes(x = `Red Electrica Corporacion SA`)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white") +
  geom_density(lwd = 1, colour = 3,
               fill = 3, alpha = 0.25)
h2 <- ggplot(na.omit(staples_merged), aes(x = `All for One Group SE`)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 2, fill = "white") +
  geom_density(lwd = 1, colour = 2,
               fill = 2, alpha = 0.25)
h3 <- ggplot(na.omit(staples_merged), aes(x = `SUESS MicroTec SE`)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 2, fill = "white") +
  geom_density(lwd = 1, colour = 2,
               fill = 2, alpha = 0.25)
h4 <- ggplot(na.omit(staples_merged), aes(x = `PSI Software AG`)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 2, fill = "white") +
  geom_density(lwd = 1, colour = 2,
               fill = 2, alpha = 0.25)
ggarrange(h1, h2, h3, h4, ncol = 2)



#consumer:
consumer <- bind_rows(ESG[which(ESG$`ICB Industry name`== "Consumer Discretionary"),], nonESG[which(nonESG$`ICB Industry name`== "Consumer Discretionary"),])
#decide which ESG companies will make up the peer group:
consumer_ESG <- consumer[c(4,15,11,17),]
#decide which nonESG companies will make up the peer group --> BASED ON Industry, Leverage, and Country
consumer_nonESG <- consumer[c(42,31,24,41),]
#join them
consumer_peer <- bind_rows(consumer_ESG,consumer_nonESG)
consumer_peer
#keep only columns for benchmarking 
colnames(consumer_peer)
consumer_bench <- consumer_peer[,c(2,4,8,11,17,20,24)]
consumer_bench[,-c(1,2,7)] <- consumer_bench[,-c(1,2,7)]*100
names(consumer_bench)[names(consumer_bench) == 'ROE Total Equity % - 2019\n(FY-2)'] <- 'ROE %'
names(consumer_bench)[names(consumer_bench) == '52 Week Total Return\n(-2CY)'] <- 'Total Return %'
names(consumer_bench)[names(consumer_bench) == 'Operating Margin - % - Y2019\n(FY-2)'] <- 'Operating Margin %'
names(consumer_bench)[names(consumer_bench) == 'ROA Total Assets, Percent - 2019\n(1D, FY-2)'] <- 'ROA Total Assets %'
kable(consumer_bench, "latex")
#keep col for DYNAMIC benchmarking --> t-test of means
#ROA

#Op margin 

consumer_names <- consumer_bench$`Company Name`
consumer_ESG_timeserie <- data_frame(ESG_timeserie$Instrument, ESG_timeserie$PSON.L,  ESG_timeserie$WTB.L, ESG_timeserie$ELIOR.PA, ESG_timeserie$OREP.PA )
names(consumer_ESG_timeserie)[names(consumer_ESG_timeserie) == 'ESG_timeserie$Instrument'] <- 'date'
names(consumer_ESG_timeserie)[names(consumer_ESG_timeserie) == 'ESG_timeserie$PSON.L'] <- 'Pearson PLC'
names(consumer_ESG_timeserie)[names(consumer_ESG_timeserie) == 'ESG_timeserie$WTB.L'] <- 'Whitbread PLC'
names(consumer_ESG_timeserie)[names(consumer_ESG_timeserie) == 'ESG_timeserie$ELIOR.PA'] <- 'Elior Group SA'
names(consumer_ESG_timeserie)[names(consumer_ESG_timeserie) == 'ESG_timeserie$OREP.PA'] <- 'LOreal SA'
consumer_ESG_timeserie %>% select(date, everything()) %>%
  arrange(date = as.Date(date, "%d/%m/%Y"))
consumer_ESG_timeserie$`Pearson PLC` <- as.numeric(consumer_ESG_timeserie$`Pearson PLC`)
consumer_ESG_timeserie$`Whitbread PLC` <- as.numeric(consumer_ESG_timeserie$`Whitbread PLC`)
consumer_ESG_timeserie$`Elior Group SA` <- as.numeric(consumer_ESG_timeserie$`Elior Group SA`)
consumer_ESG_timeserie$`LOreal SA` <- as.numeric(consumer_ESG_timeserie$`LOreal SA`)
#create rtrns based on price
consumer_ESG_timeserie <- consumer_ESG_timeserie %>% mutate(`Pearson PLC`=100*log(`Pearson PLC`/lag(`Pearson PLC`)))
consumer_ESG_timeserie <- consumer_ESG_timeserie %>% mutate(`Whitbread PLC`=100*log(`Whitbread PLC`/lag(`Whitbread PLC`)))
consumer_ESG_timeserie <- consumer_ESG_timeserie %>% mutate(`Elior Group SA`=100*log(`Elior Group SA`/lag(`Elior Group SA`)))
consumer_ESG_timeserie <- consumer_ESG_timeserie %>% mutate(`LOreal SA`=100*log(`LOreal SA`/lag(`LOreal SA`)))

consumer_nonESG_timeserie <- data.frame(nonESG_timeserie$Instrument, nonESG_timeserie$TRNT.L, nonESG_timeserie$CVSG.L, nonESG_timeserie$IPAR.PA, nonESG_timeserie$TRIA.PA)
names(consumer_nonESG_timeserie)[names(consumer_nonESG_timeserie) == 'nonESG_timeserie.Instrument'] <- 'date'
names(consumer_nonESG_timeserie)[names(consumer_nonESG_timeserie) == 'nonESG_timeserie.TRNT.L'] <- 'Trainline PLC'
names(consumer_nonESG_timeserie)[names(consumer_nonESG_timeserie) == 'nonESG_timeserie.CVSG.L'] <- 'CVS Group PLC'
names(consumer_nonESG_timeserie)[names(consumer_nonESG_timeserie) == 'nonESG_timeserie.IPAR.PA'] <- 'Interparfums SA'
names(consumer_nonESG_timeserie)[names(consumer_nonESG_timeserie) == 'nonESG_timeserie.TRIA.PA'] <- 'Trigano SA'
consumer_nonESG_timeserie %>% select(date, everything()) %>%
  arrange(date = as.Date(date, "%d/%m/%Y"))
consumer_nonESG_timeserie$`Trainline PLC` <- as.numeric(consumer_nonESG_timeserie$`Trainline PLC`)
consumer_nonESG_timeserie$`CVS Group PLC` <- as.numeric(consumer_nonESG_timeserie$`CVS Group PLC`)
consumer_nonESG_timeserie$`Interparfums SA` <- as.numeric(consumer_nonESG_timeserie$`Interparfums SA`)
consumer_nonESG_timeserie$`Trigano SA` <- as.numeric(consumer_nonESG_timeserie$`Trigano SA`)
#create rtrns based on price
colnames(consumer_nonESG_timeserie)
consumer_nonESG_timeserie <- consumer_nonESG_timeserie %>% mutate(`Trainline PLC`=100*log(`Trainline PLC`/lag(`Trainline PLC`)))
consumer_nonESG_timeserie <- consumer_nonESG_timeserie %>% mutate(`CVS Group PLC`=100*log(`CVS Group PLC`/lag(`CVS Group PLC`)))
consumer_nonESG_timeserie <- consumer_nonESG_timeserie %>% mutate(`Interparfums SA`=100*log(`Interparfums SA`/lag(`Interparfums SA`)))
consumer_nonESG_timeserie <- consumer_nonESG_timeserie %>% mutate(`Trigano SA`=100*log(`Trigano SA`/lag(`Trigano SA`)))

#merge the 2dfs
consumer_merged <- merge(consumer_ESG_timeserie, consumer_nonESG_timeserie, by="date")
consumer_merged$date <- as.Date(consumer_merged$date)
#separated lineplot:
p1 <- ggplot(na.omit(consumer_merged), aes(x=date, group=1)) + 
  geom_line(aes(y = `Pearson PLC`), color = "#93c572") + ylim(-25,25)+
  scale_x_date(date_labels = "%m-%Y")
p11 <- ggplot(na.omit(consumer_merged), aes(x=date, group=1)) + 
  geom_line(aes(y = `Whitbread PLC`), color = "#6B8E23") + ylim(-25,25)+
  scale_x_date(date_labels = "%m-%Y")
p111 <- ggplot(na.omit(consumer_merged), aes(x=date, group=1)) + 
  geom_line(aes(y = `Elior Group SA`), color = "#00FA9A") + ylim(-25,25)+
  scale_x_date(date_labels = "%m-%Y")
p1111 <- ggplot(na.omit(consumer_merged), aes(x=date, group=1)) + 
  geom_line(aes(y = `LOreal SA`), color = "#3CB371") + ylim(-25,25)+
  scale_x_date(date_labels = "%m-%Y")
p2 <- ggplot(na.omit(consumer_merged), aes(x=date, group=1)) + 
  geom_line(aes(y = `Trainline PLC`), color="#FFA07A") + ylim(-25,25)+
  scale_x_date(date_labels = "%m-%Y")
p3 <- ggplot(na.omit(consumer_merged), aes(x=date, group=1)) + 
  geom_line(aes(y = `CVS Group PLC`), color="#FF6347") +ylim(-25,25)+
  scale_x_date(date_labels = "%m-%Y")
p4 <- ggplot(na.omit(consumer_merged), aes(x=date, group=1)) + 
  geom_line(aes(y = `Interparfums SA`), color="#800000") + ylim(-25,25)+
  scale_x_date(date_labels = "%m-%Y")
p5 <- ggplot(na.omit(consumer_merged), aes(x=date, group=1)) + 
  geom_line(aes(y = `Trigano SA`), color="#800000") +  ylim(-25,25)+
  scale_x_date(date_labels = "%m-%Y")
ggarrange(p1, p11, p111, p1111, p2, p3, p4, p5, ncol = 2)
# Histogram with kernel density
h1 <- ggplot(na.omit(consumer_merged), aes(x = `Pearson PLC`)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white") +
  geom_density(lwd = 1, colour = 3,
               fill = 3, alpha = 0.25)
h11 <- ggplot(na.omit(consumer_merged), aes(x = `Whitbread PLC`)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white") +
  geom_density(lwd = 1, colour = 3,
               fill = 3, alpha = 0.25)
h111 <- ggplot(na.omit(consumer_merged), aes(x = `Elior Group SA`)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white") +
  geom_density(lwd = 1, colour = 3,
               fill = 3, alpha = 0.25)
h1111 <- ggplot(na.omit(consumer_merged), aes(x = `LOreal SA`)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white") +
  geom_density(lwd = 1, colour = 3,
               fill = 3, alpha = 0.25)
h2 <- ggplot(na.omit(consumer_merged), aes(x = `Trainline PLC`)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 2, fill = "white") +
  geom_density(lwd = 1, colour = 2,
               fill = 2, alpha = 0.25)
h3 <- ggplot(na.omit(consumer_merged), aes(x = `CVS Group PLC`)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 2, fill = "white") +
  geom_density(lwd = 1, colour = 2,
               fill = 2, alpha = 0.25)
h4 <- ggplot(na.omit(consumer_merged), aes(x = `Interparfums SA`)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 2, fill = "white") +
  geom_density(lwd = 1, colour = 2,
               fill = 2, alpha = 0.25)
h5 <- ggplot(na.omit(consumer_merged), aes(x = `Trigano SA`)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 2, fill = "white") +
  geom_density(lwd = 1, colour = 2,
               fill = 2, alpha = 0.25)
ggarrange(h1, h11, h111, h1111, h2, h3, h4, h5, ncol = 2)
#table with std, mean rtrn, sharpe ratio, etc
STOXX600_timeserie <- read.csv("C:/Users/Hadrien Venance/Desktop/Thesis/Data/STOXX600_timeseries_new_dataset.csv")
consumer_mean <- c(mean(consumer_merged$`Pearson PLC`, na.rm = T),mean(consumer_merged$`Whitbread PLC`, na.rm = T),
                     mean(consumer_merged$`Elior Group SA`, na.rm = T),mean(consumer_merged$`LOreal SA`, na.rm = T),
                     mean(consumer_merged$`Trainline PLC`, na.rm = T), mean(consumer_merged$`CVS Group PLC`, na.rm = T),
                     mean(consumer_merged$`Interparfums SA`, na.rm = T), mean(consumer_merged$`Trigano SA`, na.rm = T))
consumer_std <- c(sd(consumer_merged$`Pearson PLC`, na.rm = T),sd(consumer_merged$`Whitbread PLC`, na.rm = T),
                    sd(consumer_merged$`Elior Group SA`, na.rm = T),sd(consumer_merged$`LOreal SA`, na.rm = T),
                    sd(consumer_merged$`Trainline PLC`, na.rm = T), sd(consumer_merged$`CVS Group PLC`, na.rm = T),
                    sd(consumer_merged$`Interparfums SA`, na.rm = T), sd(consumer_merged$`Trigano SA`, na.rm = T))
#excess ret by substracting ret from STOXX600
STOXX600_timeserie$`rf_STOXX600` <- 100*log(STOXX600_timeserie$`Close.Price`/lag(STOXX600_timeserie$`Close.Price`))
consumer_excess_ret <- consumer_merged[,2:9] - STOXX600_timeserie$rf_STOXX600
consumer_sharpe <- c(round(mean(consumer_excess_ret$`Pearson PLC`, na.rm = T)/sd(consumer_excess_ret$`Pearson PLC`, na.rm = T),3),round(mean(consumer_excess_ret$`Whitbread PLC`, na.rm = T)/sd(consumer_excess_ret$`Whitbread PLC`, na.rm = T),3),
                       round(mean(consumer_excess_ret$`Elior Group SA`, na.rm = T)/sd(consumer_excess_ret$`Elior Group SA`, na.rm = T),3),round(mean(consumer_excess_ret$`LOreal SA`, na.rm = T)/sd(consumer_excess_ret$`LOreal SA`, na.rm = T),3),
                       round(mean(consumer_excess_ret$`Trainline PLC`, na.rm = T)/sd(consumer_excess_ret$`Trainline PLC`, na.rm = T),3),
                       round(mean(consumer_excess_ret$`CVS Group PLC`, na.rm = T)/sd(consumer_excess_ret$`CVS Group PLC`, na.rm = T),3),
                       round(mean(consumer_excess_ret$`Interparfums SA`, na.rm = T)/sd(consumer_excess_ret$`Interparfums SA`, na.rm = T),3),
                     round(mean(consumer_excess_ret$`Trigano SA`, na.rm = T)/sd(consumer_excess_ret$`Trigano SA`, na.rm = T),3))
names(STOXX600_timeserie)[names(STOXX600_timeserie) == 'Date'] <- 'date'
STOXX600_timeserie$date <- as.Date(STOXX600_timeserie$date)
consumer_merged <- merge(consumer_merged, STOXX600_timeserie, by='date')
consumer_merged[,-1] <- sapply(consumer_merged[,-1], as.numeric)
consumer_merged$date <- as.Date(consumer_merged$date)
consumer_xts <- xts(consumer_merged[,-1], order.by=as.Date(consumer_merged[,1]))
consumer_xts <- consumer_xts[,-10] #remove Instrument column only w/ NAs
consumer_xts <-  consumer_xts[complete.cases(consumer_xts)]
consumer_infoRatio <- c(InformationRatio(consumer_xts[,"Pearson PLC",drop= FALSE]/100, consumer_xts[,"rf_STOXX600", drop =FALSE]/100),InformationRatio(consumer_xts[,"Whitbread PLC",drop= FALSE]/100, consumer_xts[,"rf_STOXX600", drop =FALSE]/100),
                          InformationRatio(consumer_xts[,"Elior Group SA",drop= FALSE]/100, consumer_xts[,"rf_STOXX600", drop =FALSE]/100),InformationRatio(consumer_xts[,"LOreal SA",drop= FALSE]/100, consumer_xts[,"rf_STOXX600", drop =FALSE]/100),
                          InformationRatio(consumer_xts[,"Trainline PLC",drop= FALSE]/100, consumer_xts[,"rf_STOXX600", drop =FALSE]/100), InformationRatio(consumer_xts[,"CVS Group PLC",drop= FALSE]/100, consumer_xts[,"rf_STOXX600", drop =FALSE]/100),
                          InformationRatio(consumer_xts[,"Interparfums SA",drop= FALSE]/100, consumer_xts[,"rf_STOXX600", drop =FALSE]/100),
                        InformationRatio(consumer_xts[,"Trigano SA",drop= FALSE]/100, consumer_xts[,"rf_STOXX600", drop =FALSE]/100))
consumer_perf <- data.frame(consumer_mean, consumer_std, consumer_sharpe, consumer_infoRatio)
kable(consumer_perf, "latex")


