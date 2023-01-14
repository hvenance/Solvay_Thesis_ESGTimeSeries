rm(list=ls())

library(xtable)
library(knitr)
library(tidyverse)
library(reshape2)
library(ggcorrplot)
library(fBasics)    
library(PerformanceAnalytics)
library(moments)
library(xts)

library(ggplot2)
library(readxl)
library(dplyr)
#library(caret)

#install.packages("moments")

rm(list=ls())

#ATTENTION IF BUG NEED TO CHANGE \n BY \r\n
#read data
STOXX600_timeserie <- read.csv("C:/Users/Hadrien Venance/Desktop/Thesis/Data/STOXX600_timeseries_new_dataset.csv")
ESG_timeserie <- read.csv("C:/Users/Hadrien Venance/Desktop/Thesis/Data/ESG_leaders_highPrice_timeseries.csv", na.strings="")
nonESG_timeserie <- read.csv("C:/Users/Hadrien Venance/Desktop/Thesis/Data/ESG_laggards_highPrice_timeseries.csv", na.strings="")

ESG <- read_excel("C:/Users/Hadrien Venance/Desktop/Thesis/Data/ESG_leaders_highPrice.xlsx")
nonESG<- read_excel("C:/Users/Hadrien Venance/Desktop/Thesis/Data/ESG_laggards_highPrice.xlsx")

names(nonESG)[names(nonESG) == 'Identifier (RIC)'] <- 'Identifier'
names(ESG)[names(ESG) == 'Identifier (RIC)'] <- 'Identifier'

colnames(ESG)
colnames(nonESG)
colnames(ESG) <- gsub("\r", "", colnames(ESG))
colnames(nonESG) <- gsub("\r", "", colnames(nonESG))


names(ESG)[names(ESG) == 'ROE Total Equity %  - 2019\n(FY-2)'] <- 'ROE Total Equity % - 2019\n(FY-2)'


##  Create Portfolios    ##
colnames(ESG_timeserie) <- ESG_timeserie[1,]
ESG_timeserie <- ESG_timeserie[-c(1:2),]
ptf_ESG_timeserie <- data.frame(ESG_timeserie$Instrument, ESG_timeserie$SAPG.DE, ESG_timeserie$ROG.S, ESG_timeserie$LONN.S,ESG_timeserie$MNDI.L, 
                                ESG_timeserie$ALFA.ST, ESG_timeserie$GASI.MI, ESG_timeserie$ALVG.DE, ESG_timeserie$ZURN.S,
                                ESG_timeserie$IBE.MC, ESG_timeserie$NTGY.MC, ESG_timeserie$REDE.MC,  ESG_timeserie$PSON.L,  
                                ESG_timeserie$WTB.L, ESG_timeserie$ELIOR.PA, ESG_timeserie$OREP.PA
                                   )
names(ptf_ESG_timeserie)[names(ptf_ESG_timeserie) == 'ESG_timeserie.Instrument'] <- 'date'
#transform all col in numeric (except date)
names(ptf_ESG_timeserie) <- sub("ESG_timeserie.", "", names(ptf_ESG_timeserie))
ptf_ESG_timeserie <- ptf_ESG_timeserie %>% select(date, everything()) %>% mutate_if(is.character, as.numeric)
ptf_ESG_timeserie$date <- ESG_timeserie$Instrument
ptf_ESG_timeserie$date <- sub("T00:00:00Z", "", ptf_ESG_timeserie$date)
ptf_ESG_timeserie <- ptf_ESG_timeserie %>% select(date, everything()) %>%
  arrange(date = as.Date(date, format="%d/%m/%Y"))
#only take data until 31st May 2020
ptf_ESG_timeserie <- ptf_ESG_timeserie[ptf_ESG_timeserie$date<="2020-05-31",]

ptf_ESG_timeserie <- ptf_ESG_timeserie %>% 
                                            mutate(`SAP SE`=100*log(`SAPG.DE`/lag(`SAPG.DE`)),`Roche Holding AG`=100*log(`ROG.S`/lag(`ROG.S`)),
                                                  `Lonza Group AG`=100*log(`LONN.S`/lag(`LONN.S`)),`Assicurazioni Generali SpA`=100*log(`GASI.MI`/lag(`GASI.MI`)),
                                                  `Mondi PLC`=100*log(`MNDI.L`/lag(`MNDI.L`)),`Alfa Laval AB`=100*log(`ALFA.ST`/lag(`ALFA.ST`)),
                                                  `Allianz SE`=100*log(`ALVG.DE`/lag(`ALVG.DE`)),`Zurich Insurance Group AG`=100*log(`ZURN.S`/lag(`ZURN.S`)),
                                                  `Iberdrola SA`=100*log(`IBE.MC`/lag(`IBE.MC`)),`Naturgy Energy Group SA`=100*log(`NTGY.MC`/lag(`NTGY.MC`)),
                                                  `Red Electrica Corporacion SA`=100*log(`REDE.MC`/lag(`REDE.MC`)),`Pearson PLC`=100*log(`PSON.L`/lag(`PSON.L`)),
                                                  `Whitbread PLC`=100*log(`WTB.L`/lag(`WTB.L`)),`Elior Group SA`=100*log(`ELIOR.PA`/lag(`ELIOR.PA`)),
                                                  `LOreal SA`=100*log(`OREP.PA`/lag(`OREP.PA`)),`SAP SE`=100*log(`SAPG.DE`/lag(`SAPG.DE`))
                                                    )

#keep only newcolumns
ptf_ESG_timeserie <- ptf_ESG_timeserie[,-c(2:16)]

#for event study I also need to download these returns
#ptf_ESG_names <- colnames(ptf_ESG_timeserie[,-1])
#returns_ESG_stata <- ptf_ESG_timeserie %>% pivot_longer(ptf_ESG_names,names_to = "Firms",values_to = "Returns")
#returns_ESG_stata <- returns_ESG_stata %>% arrange(date,Firms)
#write_xlsx(returns_ESG_stata, "C:/Users/Hadrien Venance/Downloads/returns_ESG_stata.xlsx")


colnames(nonESG_timeserie) <- nonESG_timeserie[1,]
nonESG_timeserie <- nonESG_timeserie[-c(1:2),]
ptf_nonESG_timeserie <- data.frame(nonESG_timeserie$Instrument, nonESG_timeserie$A1OS.DE, nonESG_timeserie$SMHNn.DE, nonESG_timeserie$PSAGn.DE,
                                   nonESG_timeserie$CLTN.S, nonESG_timeserie$COPN.S, nonESG_timeserie$JDG.L, nonESG_timeserie$MRLM.L, 
                                   nonESG_timeserie$JETPAK.ST, nonESG_timeserie$TIP.MI, nonESG_timeserie$FTKn.DE, nonESG_timeserie$SQN.S, nonESG_timeserie$PEHN.S,
                                   nonESG_timeserie$SLRS.MC, nonESG_timeserie$TRNT.L, nonESG_timeserie$CVSG.L, nonESG_timeserie$IPAR.PA, nonESG_timeserie$TRIA.PA)

names(ptf_nonESG_timeserie)[names(ptf_nonESG_timeserie) == 'nonESG_timeserie.Instrument'] <- 'date'
#transform all col in numeric (except date)
names(ptf_nonESG_timeserie) <- sub("nonESG_timeserie.", "", names(ptf_nonESG_timeserie))
ptf_nonESG_timeserie <- ptf_nonESG_timeserie %>% select(date, everything()) %>% mutate_if(is.character, as.numeric)
ptf_nonESG_timeserie$date <- nonESG_timeserie$Instrument
ptf_nonESG_timeserie$date <- sub("T00:00:00Z", "", ptf_nonESG_timeserie$date)
ptf_nonESG_timeserie <- ptf_nonESG_timeserie %>% select(date, everything()) %>%
  arrange(date = as.Date(date, format="%d/%m/%Y"))
#only take data until 31st May 2020
ptf_nonESG_timeserie <- ptf_nonESG_timeserie[ptf_nonESG_timeserie$date<"2020-05-31",]

ptf_nonESG_timeserie <- ptf_nonESG_timeserie %>% mutate(`All for One Group SE`=100*log(`A1OS.DE`/lag(`A1OS.DE`)),`SUESS MicroTec SE`=100*log(`SMHNn.DE`/lag(`SMHNn.DE`)),
                                                  `PSI Software AG`=100*log(`PSAGn.DE`/lag(`PSAGn.DE`)),`Coltene Holding AG`=100*log(`CLTN.S`/lag(`CLTN.S`)),
                                                  `Cosmo Pharmaceuticals NV`=100*log(`COPN.S`/lag(`COPN.S`)),`Judges Scientific PLC`=100*log(`JDG.L`/lag(`JDG.L`)),
                                                  `Marlowe PLC`=100*log(`MRLM.L`/lag(`MRLM.L`)),`Jetpak Top Holding AB (publ)`=100*log(`JETPAK.ST`/lag(`JETPAK.ST`)),
                                                  `Tamburi Investment Partners SpA`=100*log(`TIP.MI`/lag(`TIP.MI`)),`flatexDEGIRO AG`=100*log(`FTKn.DE`/lag(`FTKn.DE`)),
                                                  `Swissquote Group Holding SA`=100*log(`SQN.S`/lag(`SQN.S`)),`Private Equity Holding AG`=100*log(`PEHN.S`/lag(`PEHN.S`)),
                                                  `Solaria Energia y Medio Ambiente SA`=100*log(`SLRS.MC`/lag(`SLRS.MC`)),`Trainline PLC`=100*log(`TRNT.L`/lag(`TRNT.L`)),
                                                  `CVS Group PLC`=100*log(`CVSG.L`/lag(`CVSG.L`)),`Interparfums SA`=100*log(`IPAR.PA`/lag(`IPAR.PA`)),`Trigano SA`=100*log(`TRIA.PA`/lag(`TRIA.PA`))
)
#keep only new_columns
ptf_nonESG_timeserie <- ptf_nonESG_timeserie[,-c(2:18)]

#for event study I also need to download these returns
#and remove 2 companies which are COPNS & TRNTL
#ptf_nonESG_names <- colnames(ptf_nonESG_timeserie[,-c(1,6,15)])
#returns_nonESG_stata <- ptf_nonESG_timeserie %>% pivot_longer(ptf_nonESG_names,names_to = "Firms",values_to = "Returns")
#returns_nonESG_stata <- returns_nonESG_stata %>% arrange(date,Firms)
#write_xlsx(returns_nonESG_stata, "C:/Users/Hadrien Venance/Downloads/returns_nonESG_stata.xlsx")



#EW
ESG_N <- ncol(ptf_ESG_timeserie)-1
EW_ESG_weights <- rep(1/ESG_N,ESG_N)
#given all stocks have same importance
ptf_ESG_timeserie$EW_ret <- rowMeans(ptf_ESG_timeserie[,-1], na.rm=T)

nonESG_N <- ncol(ptf_nonESG_timeserie)-1
EW_nonESG_weights <- rep(1/nonESG_N,nonESG_N)
#given all stocks have same importance
ptf_nonESG_timeserie$EW_ret <- rowMeans(ptf_nonESG_timeserie[,-1], na.rm=T)


#CW
#vector of weights
ESG_cap_weights <- data.frame(ESG$`Company Name`, ESG$`Company Market Cap\n(2020-03-08, EUR)`)
ESG_cap_weights$ESG..Company.Name.[ESG_cap_weights$ESG..Company.Name. =="L'Oreal SA"] <- "LOreal SA"
ESG_firms <- colnames(ptf_ESG_timeserie[,-c(1,ncol(ptf_ESG_timeserie))])
ESG_cap_weights <- ESG_cap_weights[ESG_cap_weights$ESG..Company.Name. %in% ESG_firms,]
colnames(ESG_cap_weights)
ESG_cap_weights$CW_weights <- ESG_cap_weights$ESG..Company.Market.Cap.n.2020.03.08..EUR../sum(ESG_cap_weights$ESG..Company.Market.Cap.n.2020.03.08..EUR..)
#same order for each df
t_ESG <- as.data.frame(t(ESG_cap_weights))
colnames(t_ESG) <- t_ESG[1,]
t_ESG <- t_ESG[-1,]
t_ESG<-t_ESG[ ,names(ptf_ESG_timeserie[,-c(1,ncol(ptf_ESG_timeserie))])]
#create df with returns and weights
CW_ptf_ESG_timeserie <- t(t(ptf_ESG_timeserie[,-c(1,ncol(ptf_ESG_timeserie))])*as.numeric(t_ESG[2,]))
CW_ptf_ESG_timeserie <- data.frame(ptf_ESG_timeserie$date,CW_ptf_ESG_timeserie)
#new col in previous df time_serie
ptf_ESG_timeserie$CW_ret <- rowSums(CW_ptf_ESG_timeserie[,-1], na.rm=T)


#vector of weights
nonESG_cap_weights <- data.frame(nonESG$`Company Name`, nonESG$`Company Market Cap\n(2020-03-08, EUR)`)
#nonESG_cap_weights$ESG..Company.Name.[ESG_cap_weights$ESG..Company.Name. =="L'Oreal SA"] <- "LOreal SA"
nonESG_firms <- colnames(ptf_nonESG_timeserie[,-c(1,ncol(ptf_nonESG_timeserie))])
nonESG_cap_weights <- nonESG_cap_weights[nonESG_cap_weights$nonESG..Company.Name. %in% nonESG_firms,]
nonESG_cap_weights$CW_weights <- nonESG_cap_weights$nonESG..Company.Market.Cap.n.2020.03.08..EUR../sum(nonESG_cap_weights$nonESG..Company.Market.Cap.n.2020.03.08..EUR..)
#same order for each df 
t_nonESG <- as.data.frame(t(nonESG_cap_weights))
colnames(t_nonESG) <- t_nonESG[1,]
t_nonESG <- t_nonESG[-1,]
t_nonESG<-t_nonESG[ ,names(ptf_nonESG_timeserie[,-c(1,ncol(ptf_nonESG_timeserie))])]
#create df with returns and weights
CW_ptf_nonESG_timeserie <- t(t(ptf_nonESG_timeserie[,-c(1,ncol(ptf_nonESG_timeserie))])*as.numeric(t_nonESG[2,]))
CW_ptf_nonESG_timeserie <- data.frame(ptf_nonESG_timeserie$date,CW_ptf_nonESG_timeserie)
#new col in previous df time_serie
ptf_nonESG_timeserie$CW_ret <- rowSums(CW_ptf_nonESG_timeserie[,-1], na.rm=T)


### Trends visualizations ###
#EW
EW_ESG <- data.frame(ptf_ESG_timeserie$date,ptf_ESG_timeserie$EW_ret)
colnames(EW_ESG) <- c("date", "EW_ret")
EW_nonESG <- data.frame(ptf_nonESG_timeserie$date,ptf_nonESG_timeserie$EW_ret) 
colnames(EW_nonESG) <- c("date", "EW_ret")
time_series <- full_join(EW_ESG,EW_nonESG, by="date")
EW_STOXX <- STOXX600_timeserie[,-c(1,2,4)] #drop volume column
colnames(EW_STOXX)[1] <- "date"
EW_STOXX <- EW_STOXX[EW_STOXX$date<="2020-05-31",]
#arrange date
EW_STOXX$date <- sub("T00:00:00Z", "", EW_STOXX$date)
EW_STOXX <- EW_STOXX %>% select(date, everything()) %>%
  arrange(date = as.Date(date, format="%d/%m/%Y"))
time_series <- full_join(time_series,EW_STOXX, by="date")
colnames(time_series) <- c('date','ESG','nonESG','STOXX600')
time_series <- time_series %>% mutate(`STOXX600`=100*log(`STOXX600`/lag(`STOXX600`)))
#remove 1st date as nonESG would have NA value
time_series <- time_series[-1,]

plot_time_series <- time_series
plot_time_series$date <- as.Date(plot_time_series$date)
colnames(plot_time_series)[which(names(plot_time_series) == "ESG")] <- "ESG Leaders"
colnames(plot_time_series)[which(names(plot_time_series) == "nonESG")] <- "ESG Laggards"
#pivot for the plot
time <- plot_time_series %>% pivot_longer(c('ESG Leaders','ESG Laggards','STOXX600'),names_to = "Type",values_to = "Returns")
#plot of the 3 time series
ggplot(time, aes(x = date, y=Returns, color=Type)) +
  geom_line()+
  ggtitle("Returns (%) of the 3 equally-weighted portfolios") +
  theme(plot.title = element_text(hjust = 0.5))

# Correlation between the time series #
colnames(time_series)[which(names(time_series) == "ESG")] <- "ESG Leaders"
colnames(time_series)[which(names(time_series) == "nonESG")] <- "ESG Laggards"
var(time_series$`ESG Leaders`, na.rm = T)
var(time_series$`ESG Laggards`, na.rm = T)
correlation <- cor(time_series[,-1], use = "complete.obs")
correlation
#correlation heatmap
melted_series <- melt(correlation)
#colnames(melted_series)[,c(1,2)] <- c("Portfolio A", "Portfolio B")
ggplot(data = melted_series, aes(x=Var1, y=Var2, fill=value)) +
  ggtitle("Correlation heatmap between the 3 equally-weighted portfolios") +
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),)+
  geom_tile()

ggcorrplot(correlation)+
  ggtitle("Correlation heatmap between the 3 types of investments") +
  theme(plot.title = element_text(hjust = 0.5))

### summary stat ###
#n_r = length(ptf_ESG_timeserie[,1])
EW_mu_ESG = mean(EW_ESG$EW_ret,na.rm=T)
EW_SIGMA_ESG = sd(EW_ESG$EW_ret,na.rm=T)
EW_skew_ESG = skewness(EW_ESG$EW_ret,na.rm=T)
EW_kurt_ESG = kurtosis(EW_ESG$EW_ret,na.rm = T)+3
EW_mu_nonESG = mean(EW_nonESG$EW_ret,na.rm=T)
EW_SIGMA_nonESG = sd(EW_nonESG$EW_ret,na.rm=T)
EW_skew_nonESG = skewness(EW_nonESG$EW_ret,na.rm=T)
EW_kurt_nonESG = kurtosis(EW_nonESG$EW_ret,na.rm = T)+3
EW_mu_STOXX600 = mean(time_series$STOXX600,na.rm=T)
EW_SIGMA_STOXX600 = sd(time_series$STOXX600,na.rm=T)
EW_skew_STOXX600 = skewness(time_series$STOXX600,na.rm=T)
EW_kurt_STOXX600 = kurtosis(time_series$STOXX600,na.rm = T)+3

sum_stat <- data.frame(EW_mu_ESG, EW_SIGMA_ESG, EW_skew_ESG, EW_kurt_ESG, EW_mu_nonESG, EW_SIGMA_nonESG, EW_skew_nonESG, EW_kurt_nonESG,
                       EW_mu_STOXX600, EW_SIGMA_STOXX600, EW_skew_STOXX600, EW_kurt_STOXX600)
kable(sum_stat, "latex")

#get Mean ESG score
ESG_names <- c("SAPG.DE","ROG.S","LONN.S","MNDI.L","ALFA.ST","GASI.MI","ALVG.DE","ZURN.S","IBE.MC","NTGY.MC","REDE.MC","PSON.L","WTB.L","ELIOR.PA","OREP.PA")
ESG_characteristics <- ESG[ESG$Identifier %in% ESG_names,]
EW_Mean_ESG_score <- mean(ESG_characteristics$`ESG Combined Score\r\n(FY-2)`)

nonESG_names <- c("A1OS.DE","SMHNn.DE","COPN.S","TRNT.L","PSAGn.DE","CLTN.S","JDG.L","MRLM.L","JETPAK.ST","TIP.MI","FTKn.DE","SQN.S","PEHN.S",
                  "SLRS.MC","CVSG.L","IPAR.PA","TRIA.PA")
nonESG_characteristics <- nonESG[nonESG$Identifier %in% nonESG_names,]
EW_Mean_nonESG_score <- mean(nonESG_characteristics$`ESG Combined Score\n(FY-2)`)

### OTHER summary stat ###
ff_data = read.csv("C:/Users/Hadrien Venance/Desktop/Thesis/Code/Europe_3_Factors_Daily.csv", skip = 3)
colnames(ff_data)[1] = "date"
# convert dates in R date format
ff_data$date = as.Date(strptime(ff_data$date, format = "%Y%m%d"))
EW_ESG$date <- as.Date(EW_ESG$date)
EW_ESG = merge(as.data.frame(EW_ESG), as.data.frame(ff_data[,c(1,5)]), by = "date")
#EW_ESG$RF = EW_ESG$RF*100
EW_ESG$excess_ret <- EW_ESG$EW_ret - EW_ESG$RF
EW_sharpe_ESG <- round(mean(EW_ESG$excess_ret, na.rm = T)/sd(EW_ESG$excess_ret, na.rm = T),3)
time_series$date <- as.Date(time_series$date)
EW_ESG_merged <- merge(EW_ESG, time_series[,c(1,4)], by='date')
EW_ESG_merged[,-1] <- sapply(EW_ESG_merged[,-1], as.numeric)
EW_ESG_merged$date <- as.Date(EW_ESG_merged$date)
EW_ESG_xts <- xts(EW_ESG_merged[,-1], order.by=as.Date(EW_ESG_merged[,1]))
EW_ESG_xts <-  EW_ESG_xts[complete.cases(EW_ESG_xts)]
EW_infoRatio_ESG <- InformationRatio(EW_ESG_xts[,"EW_ret",drop= FALSE]/100, EW_ESG_xts[,"STOXX600", drop =FALSE]/100)
EW_Sortino_ESG <- SortinoRatio(EW_ESG_xts[,"EW_ret",drop= FALSE]/100, MAR = 0)
#nonESG
EW_nonESG$date <- as.Date(EW_nonESG$date)
EW_nonESG = merge(as.data.frame(EW_nonESG), as.data.frame(ff_data[,c(1,5)]), by = "date")
#EW_nonESG$RF = EW_nonESG$RF*100
EW_nonESG$excess_ret <- EW_nonESG$EW_ret - EW_nonESG$RF
EW_sharpe_nonESG <- round(mean(EW_nonESG$excess_ret, na.rm = T)/sd(EW_nonESG$excess_ret, na.rm = T),3)
time_series$date <- as.Date(time_series$date)
EW_nonESG_merged <- merge(EW_nonESG, time_series[,c(1,4)], by='date')
EW_nonESG_merged[,-1] <- sapply(EW_nonESG_merged[,-1], as.numeric)
EW_nonESG_merged$date <- as.Date(EW_nonESG_merged$date)
EW_nonESG_xts <- xts(EW_nonESG_merged[,-1], order.by=as.Date(EW_nonESG_merged[,1]))
EW_nonESG_xts <-  EW_nonESG_xts[complete.cases(EW_nonESG_xts)]
EW_infoRatio_nonESG <- InformationRatio(EW_nonESG_xts[,"EW_ret",drop= FALSE]/100, EW_nonESG_xts[,"STOXX600", drop =FALSE]/100)
EW_Sortino_nonESG <- SortinoRatio(EW_nonESG_xts[,"EW_ret",drop= FALSE]/100, MAR = 0)
#STOXX600
EW_STOXX600 = merge(as.data.frame(time_series), as.data.frame(ff_data[,c(1,5)]), by = "date")
EW_STOXX600$RF = EW_STOXX600$RF*100
EW_STOXX600$excess_STOXX600 <- EW_STOXX600$STOXX600 - EW_STOXX600$RF 
EW_sharpe_STOXX600 <- round(mean(EW_STOXX600$excess_STOXX600, na.rm = T)/sd(EW_STOXX600$excess_STOXX600, na.rm = T),3)
EW_STOXX600_xts <- xts(EW_STOXX600[,-c(1:3)], order.by=as.Date(EW_STOXX600[,1]))
EW_infoRatio_STOXX600 <- InformationRatio(EW_STOXX600_xts[,"STOXX600",drop= FALSE]/100, EW_STOXX600_xts[,"STOXX600", drop =FALSE]/100)
EW_Sortino_STOXX600 <- SortinoRatio(EW_nonESG_xts[,"STOXX600",drop= FALSE]/100, MAR = 0)

other_sum_stat <- data.frame(EW_sharpe_ESG, EW_infoRatio_ESG, EW_Sortino_ESG, EW_sharpe_nonESG, EW_infoRatio_nonESG, EW_Sortino_nonESG,
                             EW_sharpe_STOXX600, EW_infoRatio_STOXX600, EW_Sortino_STOXX600)
kable(other_sum_stat, "latex")


### OTHER visualizations - histograms + densities ###
ggplot(na.omit(time_series), aes(x = `ESG`)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white") +
  geom_density(lwd = 1, colour = 3,
               fill = 3, alpha = 0.25) +
  labs(x="ESG Leaders")

ggplot(na.omit(time_series), aes(x = `nonESG`)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 2, fill = "white") +
  geom_density(lwd = 1, colour = 2,
               fill = 2, alpha = 0.25) +
  labs(x="ESG Laggards")

ggplot(na.omit(time_series), aes(x = `STOXX600`)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 4, fill = "white") +
  geom_density(lwd = 1, colour = 4,
               fill = 4, alpha = 0.25) +
  labs(x="STOXX600")




#CW
CW_ESG <- data.frame(ptf_ESG_timeserie$date,ptf_ESG_timeserie$CW_ret)
colnames(CW_ESG) <- c("date", "CW_ret")
CW_nonESG <- data.frame(ptf_nonESG_timeserie$date,ptf_nonESG_timeserie$CW_ret) 
colnames(CW_nonESG) <- c("date", "CW_ret")
time_series <- full_join(CW_ESG,CW_nonESG, by="date")
CW_STOXX <- STOXX600_timeserie[,-c(1,2,4)] #drop volume column
colnames(CW_STOXX)[1] <- "date"
#arrange date
CW_STOXX$date <- sub("T00:00:00Z", "", CW_STOXX$date)
CW_STOXX <- CW_STOXX %>% select(date, everything()) %>%
  arrange(date = as.Date(date, format="%d/%m/%Y"))
time_series <- full_join(time_series,CW_STOXX, by="date")
colnames(time_series) <- c('date','ESG','nonESG','STOXX600')
time_series <- time_series %>% mutate(`STOXX600`=100*log(`STOXX600`/lag(`STOXX600`)))
#remove 1st date as nonESG would have NA value
time_series <- time_series[-1,]

plot_time_series <- time_series
plot_time_series$date <- as.Date(plot_time_series$date)
colnames(plot_time_series)[which(names(plot_time_series) == "ESG")] <- "ESG Leaders"
colnames(plot_time_series)[which(names(plot_time_series) == "nonESG")] <- "ESG Laggards"
#pivot for the plot
time <- plot_time_series %>% pivot_longer(c('ESG Leaders','ESG Laggards','STOXX600'),names_to = "Type",values_to = "Returns")
#plot of the 3 time series
ggplot(time, aes(x = date, y=Returns, color=Type)) +
  geom_line()+
  ggtitle("Returns (%) of the 3 capitalization-weighted portfolios") +
  theme(plot.title = element_text(hjust = 0.5))

# Correlation between the time series #
colnames(time_series)[which(names(time_series) == "ESG")] <- "ESG Leaders"
colnames(time_series)[which(names(time_series) == "nonESG")] <- "ESG Laggards"
var(time_series$`ESG Leaders`, na.rm = T)
var(time_series$`ESG Laggards`, na.rm = T)
correlation <- cor(time_series[,-1], use = "complete.obs")
correlation
#correlation heatmap
melted_series <- melt(correlation)
#colnames(melted_series)[,c(1,2)] <- c("Portfolio A", "Portfolio B")
ggplot(data = melted_series, aes(x=Var1, y=Var2, fill=value)) +
  ggtitle("Correlation heatmap between the 3 capitalization-weighted portfolios") +
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),)+
  geom_tile()

ggcorrplot(correlation)+
  ggtitle("Correlation heatmap between the 3 types of investments") +
  theme(plot.title = element_text(hjust = 0.5))

### summary stat ###
#n_r = length(ptf_ESG_timeserie[,1])
CW_mu_ESG = mean(CW_ESG$CW_ret,na.rm=T)
CW_SIGMA_ESG = sd(CW_ESG$CW_ret,na.rm=T)
CW_skew_ESG = skewness(CW_ESG$CW_ret,na.rm=T)
CW_kurt_ESG = kurtosis(CW_ESG$CW_ret,na.rm = T)+3
CW_mu_nonESG = mean(CW_nonESG$CW_ret,na.rm=T)
CW_SIGMA_nonESG = sd(CW_nonESG$CW_ret,na.rm=T)
CW_skew_nonESG = skewness(CW_nonESG$CW_ret,na.rm=T)
CW_kurt_nonESG = kurtosis(CW_nonESG$CW_ret,na.rm = T)+3
CW_mu_STOXX600 = mean(time_series$STOXX600,na.rm=T)
CW_SIGMA_STOXX600 = sd(time_series$STOXX600,na.rm=T)
CW_skew_STOXX600 = skewness(time_series$STOXX600)
CW_kurt_STOXX600 = kurtosis(time_series$STOXX600,na.rm = T)+3
sum_stat <- data.frame(CW_mu_ESG, CW_SIGMA_ESG, CW_skew_ESG, CW_kurt_ESG, CW_mu_nonESG, CW_SIGMA_nonESG, CW_skew_nonESG, CW_kurt_nonESG,
                       CW_mu_STOXX600, CW_SIGMA_STOXX600, CW_skew_STOXX600, CW_kurt_STOXX600)
kable(sum_stat, "latex")



#get Mean ESG score
CW_Mean_ESG_score <- sum(ESG_cap_weights$CW_weights*ESG_characteristics$`ESG Combined Score\r\n(FY-2)`)

CW_Mean_nonESG_score <- sum(nonESG_cap_weights$CW_weights*nonESG_characteristics$`ESG Combined Score\n(FY-2)`)

### OTHER summary stat ###
ff_data = read.csv("C:/Users/Hadrien Venance/Desktop/Thesis/Code/Europe_3_Factors_Daily.csv", skip = 3)
colnames(ff_data)[1] = "date"
# convert dates in R date format
ff_data$date = as.Date(strptime(ff_data$date, format = "%Y%m%d"))
CW_ESG$date <- as.Date(CW_ESG$date)
CW_ESG = merge(as.data.frame(CW_ESG), as.data.frame(ff_data[,c(1,5)]), by = "date")
#CW_ESG$RF = CW_ESG$RF*100
CW_ESG$excess_ret <- CW_ESG$CW_ret - CW_ESG$RF
CW_sharpe_ESG <- round(mean(CW_ESG$excess_ret, na.rm = T)/sd(CW_ESG$excess_ret, na.rm = T),3)
time_series$date <- as.Date(time_series$date)
CW_ESG_merged <- merge(CW_ESG, time_series[,c(1,4)], by='date')
CW_ESG_merged[,-1] <- sapply(CW_ESG_merged[,-1], as.numeric)
CW_ESG_merged$date <- as.Date(CW_ESG_merged$date)
CW_ESG_xts <- xts(CW_ESG_merged[,-1], order.by=as.Date(CW_ESG_merged[,1]))
CW_ESG_xts <-  CW_ESG_xts[complete.cases(CW_ESG_xts)]
CW_infoRatio_ESG <- InformationRatio(CW_ESG_xts[,"CW_ret",drop= FALSE]/100, CW_ESG_xts[,"STOXX600", drop =FALSE]/100)
CW_Sortino_ESG <- SortinoRatio(CW_ESG_xts[,"CW_ret",drop= FALSE]/100, MAR = 0)
#nonESG
CW_nonESG$date <- as.Date(CW_nonESG$date)
CW_nonESG = merge(as.data.frame(CW_nonESG), as.data.frame(ff_data[,c(1,5)]), by = "date")
#CW_nonESG$RF = CW_nonESG$RF*100
CW_nonESG$excess_ret <- CW_nonESG$CW_ret - CW_nonESG$RF
CW_sharpe_nonESG <- round(mean(CW_nonESG$excess_ret, na.rm = T)/sd(CW_nonESG$excess_ret, na.rm = T),3)
time_series$date <- as.Date(time_series$date)
CW_nonESG_merged <- merge(CW_nonESG, time_series[,c(1,4)], by='date')
CW_nonESG_merged[,-1] <- sapply(CW_nonESG_merged[,-1], as.numeric)
CW_nonESG_merged$date <- as.Date(CW_nonESG_merged$date)
CW_nonESG_xts <- xts(CW_nonESG_merged[,-1], order.by=as.Date(CW_nonESG_merged[,1]))
CW_nonESG_xts <-  CW_nonESG_xts[complete.cases(CW_nonESG_xts)]
CW_infoRatio_nonESG <- InformationRatio(CW_nonESG_xts[,"CW_ret",drop= FALSE]/100, CW_nonESG_xts[,"STOXX600", drop =FALSE]/100)
CW_Sortino_nonESG <- SortinoRatio(CW_nonESG_xts[,"CW_ret",drop= FALSE]/100, MAR = 0)
#STOXX600
CW_STOXX600 = merge(as.data.frame(time_series), as.data.frame(ff_data[,c(1,5)]), by = "date")
#CW_STOXX600$RF = CW_STOXX600$RF*100
CW_STOXX600$excess_STOXX600 <- CW_STOXX600$STOXX600 - CW_STOXX600$RF 
CW_sharpe_STOXX600 <- round(mean(CW_STOXX600$excess_STOXX600, na.rm = T)/sd(CW_STOXX600$excess_STOXX600, na.rm = T),3)
CW_STOXX600_xts <- xts(CW_STOXX600[,-c(1:3)], order.by=as.Date(CW_STOXX600[,1]))
CW_infoRatio_STOXX600 <- InformationRatio(CW_STOXX600_xts[,"STOXX600",drop= FALSE]/100, CW_STOXX600_xts[,"STOXX600", drop =FALSE]/100)
CW_Sortino_STOXX600 <- SortinoRatio(CW_nonESG_xts[,"STOXX600",drop= FALSE]/100, MAR = 0)

other_sum_stat <- data.frame(CW_sharpe_ESG, CW_infoRatio_ESG, CW_Sortino_ESG, CW_sharpe_nonESG, CW_infoRatio_nonESG, CW_Sortino_nonESG,
                             CW_sharpe_STOXX600, CW_infoRatio_STOXX600, CW_Sortino_STOXX600)
kable(other_sum_stat, "latex")


### OTHER visualizations - histograms + densities ###
ggplot(na.omit(time_series), aes(x = `ESG`)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white") +
  geom_density(lwd = 1, colour = 3,
               fill = 3, alpha = 0.25) +
  labs(x="ESG Leaders")

ggplot(na.omit(time_series), aes(x = `nonESG`)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 2, fill = "white") +
  geom_density(lwd = 1, colour = 2,
               fill = 2, alpha = 0.25) +
  labs(x="ESG Laggards")

ggplot(na.omit(time_series), aes(x = `STOXX600`)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 4, fill = "white") +
  geom_density(lwd = 1, colour = 4,
               fill = 4, alpha = 0.25) +
  labs(x="STOXX600")






#historical
#VaR
#put a negative sign in front of the losses data as we only care about quantiles for losses when computing VaR 
hist_EW_l_ESG =  data.frame(ptf_ESG_timeserie$date, -ptf_ESG_timeserie$EW_ret)
#hist_EW_l_ESG$date <- ESG_timeserie$Instrument
hist_EW_l_nonESG = data.frame(ptf_nonESG_timeserie$date, -ptf_nonESG_timeserie$EW_ret)
probs = c(0.95,0.975,0.99)
hist_EW_VaR_ESG   = quantile(hist_EW_l_ESG$X.ptf_ESG_timeserie.EW_ret,probs, na.rm=T)
hist_EW_VaR_nonESG = quantile(hist_EW_l_nonESG$X.ptf_nonESG_timeserie.EW_ret,probs,na.rm=T)

hist_CW_l_ESG = data.frame(ptf_ESG_timeserie$date, -ptf_ESG_timeserie$CW_ret)
hist_CW_l_nonESG = data.frame(ptf_nonESG_timeserie$date, -ptf_nonESG_timeserie$CW_ret)
probs = c(0.95,0.975,0.99)
hist_CW_VaR_ESG   = quantile(hist_CW_l_ESG$X.ptf_ESG_timeserie.CW_ret,probs)
hist_CW_VaR_nonESG = quantile(hist_CW_l_nonESG$X.ptf_nonESG_timeserie.CW_ret,probs)

#ES
#EW
ES(hist_EW_l_ESG$X.ptf_ESG_timeserie.EW_ret/100, method="historical", p=.95)*100
ES(hist_EW_l_ESG$X.ptf_ESG_timeserie.EW_ret/100, method="historical", p=.975)*100
ES(hist_EW_l_ESG$X.ptf_ESG_timeserie.EW_ret/100, method="historical", p=.99)*100

ES(hist_EW_l_nonESG$X.ptf_nonESG_timeserie.EW_ret/100, method="historical", p=.95)*100
ES(hist_EW_l_nonESG$X.ptf_nonESG_timeserie.EW_ret/100, method="historical", p=.975)*100
ES(hist_EW_l_nonESG$X.ptf_nonESG_timeserie.EW_ret/100, method="historical", p=.99)*100

#CW
ES(hist_CW_l_ESG$X.ptf_ESG_timeserie.CW_ret/100, method="historical", p=.95)*100
ES(hist_CW_l_ESG$X.ptf_ESG_timeserie.CW_ret/100, method="historical", p=.975)*100
ES(hist_CW_l_ESG$X.ptf_ESG_timeserie.CW_ret/100, method="historical", p=.99)*100

ES(hist_CW_l_nonESG$X.ptf_nonESG_timeserie.CW_ret/100, method="historical", p=.95)*100
ES(hist_CW_l_nonESG$X.ptf_nonESG_timeserie.CW_ret/100, method="historical", p=.975)*100
ES(hist_CW_l_nonESG$X.ptf_nonESG_timeserie.CW_ret/100, method="historical", p=.99)*100


#parametric
#EW
n_r = length(ptf_ESG_timeserie[,1])
para_EW_mu_ESG=cbind(apply(ptf_ESG_timeserie[1:n_r,2:(ncol(ptf_ESG_timeserie)-2)],2,mean,na.rm=T)) #empirical mean
para_EW_mu_ESG
para_EW_SIGMA_ESG=cov(ptf_ESG_timeserie[1:n_r,2:(ncol(ptf_ESG_timeserie)-2)],use="pairwise.complete.obs")      #empirical var-covar matrix
para_EW_SIGMA_ESG
para_EW_skew_ESG=apply(ptf_ESG_timeserie[1:n_r,2:(ncol(ptf_ESG_timeserie)-2)],2,skewness,na.rm=T)  #empirical skEWness
para_EW_skew_ESG
para_EW_kurt_ESG=apply(ptf_ESG_timeserie[1:n_r,2:(ncol(ptf_ESG_timeserie)-2)],2,kurtosis,na.rm=T)+3  #empirical excess kurtosis
para_EW_kurt_ESG
para_EW_mu_nonESG=cbind(apply(ptf_nonESG_timeserie[1:n_r,2:(ncol(ptf_nonESG_timeserie)-2)],2,mean,na.rm=T)) #empirical mean
para_EW_mu_nonESG
para_EW_SIGMA_nonESG=cov(ptf_nonESG_timeserie[1:n_r,2:(ncol(ptf_nonESG_timeserie)-2)],use="pairwise.complete.obs")      #empirical var-covar matrix
para_EW_SIGMA_nonESG
para_EW_skew_nonESG=apply(ptf_nonESG_timeserie[1:n_r,2:(ncol(ptf_nonESG_timeserie)-2)],2,skewness,na.rm=T)  #empirical skEWness
para_EW_skew_nonESG
para_EW_kurt_nonESG=apply(ptf_nonESG_timeserie[1:n_r,2:(ncol(ptf_nonESG_timeserie)-2)],2,kurtosis,na.rm=T)+3  #empirical excess kurtosis
para_EW_kurt_nonESG
z_95=qnorm(0.95,mean=0,sd=1) #Inverse normal cumulative distribution function giving the 95% quantile
z_975=qnorm(0.975,mean=0,sd=1) 
z_99=qnorm(0.99,mean=0,sd=1) 


GaussianVaR_EW_ESG_95= -EW_ESG_weights%*%para_EW_mu_ESG+(EW_ESG_weights%*%para_EW_SIGMA_ESG%*%cbind(EW_ESG_weights))^(1/2)*z_95
GaussianVaR_EW_ESG_975=-EW_ESG_weights%*%para_EW_mu_ESG+(EW_ESG_weights%*%para_EW_SIGMA_ESG%*%cbind(EW_ESG_weights))^(1/2)*z_975
GaussianVaR_EW_ESG_99=-EW_ESG_weights%*%para_EW_mu_ESG+(EW_ESG_weights%*%para_EW_SIGMA_ESG%*%cbind(EW_ESG_weights))^(1/2)*z_99
GaussianVaR_EW_nonESG_95=-EW_nonESG_weights%*%para_EW_mu_nonESG+(EW_nonESG_weights%*%para_EW_SIGMA_nonESG%*%cbind(EW_nonESG_weights))^(1/2)*z_95
GaussianVaR_EW_nonESG_975=-EW_nonESG_weights%*%para_EW_mu_nonESG+(EW_nonESG_weights%*%para_EW_SIGMA_nonESG%*%cbind(EW_nonESG_weights))^(1/2)*z_975
GaussianVaR_EW_nonESG_99=-EW_nonESG_weights%*%para_EW_mu_nonESG+(EW_nonESG_weights%*%para_EW_SIGMA_nonESG%*%cbind(EW_nonESG_weights))^(1/2)*z_99
sum_stat <- data.frame(GaussianVaR_EW_ESG_95,GaussianVaR_EW_ESG_975,GaussianVaR_EW_ESG_99,
                       GaussianVaR_EW_nonESG_95,GaussianVaR_EW_nonESG_975,GaussianVaR_EW_nonESG_99)
kable(sum_stat, "latex")

##ES 
GaussianES_EW_ESG_95=-EW_ESG_weights%*%para_EW_mu_ESG+(EW_ESG_weights%*%para_EW_SIGMA_ESG%*%cbind(EW_ESG_weights))^(1/2)*dnorm(z_95,0,1)/0.05
GaussianES_EW_ESG_975=-EW_ESG_weights%*%para_EW_mu_ESG+(EW_ESG_weights%*%para_EW_SIGMA_ESG%*%cbind(EW_ESG_weights))^(1/2)*dnorm(z_975,0,1)/0.025
GaussianES_EW_ESG_99=-EW_ESG_weights%*%para_EW_mu_ESG+(EW_ESG_weights%*%para_EW_SIGMA_ESG%*%cbind(EW_ESG_weights))^(1/2)*dnorm(z_99,0,1)/0.01
GaussianES_EW_nonESG_95=-EW_nonESG_weights%*%para_EW_mu_nonESG+(EW_nonESG_weights%*%para_EW_SIGMA_nonESG%*%cbind(EW_nonESG_weights))^(1/2)*dnorm(z_95,0,1)/0.05
GaussianES_EW_nonESG_975=-EW_nonESG_weights%*%para_EW_mu_nonESG+(EW_nonESG_weights%*%para_EW_SIGMA_nonESG%*%cbind(EW_nonESG_weights))^(1/2)*dnorm(z_975,0,1)/0.025
GaussianES_EW_nonESG_99=-EW_nonESG_weights%*%para_EW_mu_nonESG+(EW_nonESG_weights%*%para_EW_SIGMA_nonESG%*%cbind(EW_nonESG_weights))^(1/2)*dnorm(z_99,0,1)/0.01
sum_stat <- data.frame(GaussianES_EW_ESG_95,GaussianES_EW_ESG_975,GaussianES_EW_ESG_99,
                       GaussianES_EW_nonESG_95,GaussianES_EW_nonESG_975,GaussianES_EW_nonESG_99)
kable(sum_stat, "latex")


#CW
# changer les means avec les wieghts 
n_r = length(ptf_nonESG_timeserie[,1])
para_CW_mu_ESG=cbind(apply(ptf_ESG_timeserie[1:n_r,2:(ncol(ptf_ESG_timeserie)-2)],2,mean,na.rm=T)) #empirical mean
para_CW_mu_ESG
para_CW_SIGMA_ESG=cov(ptf_ESG_timeserie[1:n_r,2:(ncol(ptf_ESG_timeserie)-2)],use="pairwise.complete.obs")      #empirical var-covar matrix
para_CW_SIGMA_ESG
para_CW_skew_ESG=apply(ptf_ESG_timeserie[1:n_r,2:(ncol(ptf_ESG_timeserie)-2)],2,skewness,na.rm=T)  #empirical skCWness
para_CW_skew_ESG
para_CW_kurt_ESG=apply(ptf_ESG_timeserie[1:n_r,2:(ncol(ptf_ESG_timeserie)-2)],2,kurtosis,na.rm=T)+3  #empirical excess kurtosis
para_CW_kurt_ESG
para_CW_mu_nonESG=cbind(apply(ptf_nonESG_timeserie[1:n_r,2:(ncol(ptf_nonESG_timeserie)-2)],2,mean,na.rm=T)) #empirical mean
para_CW_mu_nonESG
para_CW_SIGMA_nonESG=cov(ptf_nonESG_timeserie[1:n_r,2:(ncol(ptf_nonESG_timeserie)-2)],use="pairwise.complete.obs")      #empirical var-covar matrix
para_CW_SIGMA_nonESG
para_CW_skew_nonESG=apply(ptf_nonESG_timeserie[1:n_r,2:(ncol(ptf_nonESG_timeserie)-2)],2,skewness,na.rm=T)  #empirical skCWness
para_CW_skew_nonESG
para_CW_kurt_nonESG=apply(ptf_nonESG_timeserie[1:n_r,2:(ncol(ptf_nonESG_timeserie)-2)],2,kurtosis,na.rm=T)+3  #empirical excess kurtosis
para_CW_kurt_nonESG
z_95=qnorm(0.95,mean=0,sd=1) #Inverse normal cumulative distribution function giving the 95% quantile
z_975=qnorm(0.975,mean=0,sd=1) 
z_99=qnorm(0.99,mean=0,sd=1) 

CW_ESG_weights <- as.numeric(t_ESG[2,])
CW_nonESG_weights <- as.numeric(t_nonESG[2,])
GaussianVaR_CW_ESG_95=-CW_ESG_weights%*%para_CW_mu_ESG+(CW_ESG_weights%*%para_CW_SIGMA_ESG%*%cbind(CW_ESG_weights))^(1/2)*z_95
GaussianVaR_CW_ESG_975=-CW_ESG_weights%*%para_CW_mu_ESG+(CW_ESG_weights%*%para_CW_SIGMA_ESG%*%cbind(CW_ESG_weights))^(1/2)*z_975
GaussianVaR_CW_ESG_99=-CW_ESG_weights%*%para_CW_mu_ESG+(CW_ESG_weights%*%para_CW_SIGMA_ESG%*%cbind(CW_ESG_weights))^(1/2)*z_99
GaussianVaR_CW_nonESG_95=-CW_nonESG_weights%*%para_CW_mu_nonESG+(CW_nonESG_weights%*%para_CW_SIGMA_nonESG%*%cbind(CW_nonESG_weights))^(1/2)*z_95
GaussianVaR_CW_nonESG_975=-CW_nonESG_weights%*%para_CW_mu_nonESG+(CW_nonESG_weights%*%para_CW_SIGMA_nonESG%*%cbind(CW_nonESG_weights))^(1/2)*z_975
GaussianVaR_CW_nonESG_99=-CW_nonESG_weights%*%para_CW_mu_nonESG+(CW_nonESG_weights%*%para_CW_SIGMA_nonESG%*%cbind(CW_nonESG_weights))^(1/2)*z_99
sum_stat <- data.frame(GaussianVaR_CW_ESG_95,GaussianVaR_CW_ESG_975,GaussianVaR_CW_ESG_99,
                       GaussianVaR_CW_nonESG_95,GaussianVaR_CW_nonESG_975,GaussianVaR_CW_nonESG_99)
kable(sum_stat, "latex")


##ES 
GaussianES_CW_ESG_95=-CW_ESG_weights%*%para_CW_mu_ESG+(CW_ESG_weights%*%para_CW_SIGMA_ESG%*%cbind(CW_ESG_weights))^(1/2)*dnorm(z_95,0,1)/0.05
GaussianES_CW_ESG_975=-CW_ESG_weights%*%para_CW_mu_ESG+(CW_ESG_weights%*%para_CW_SIGMA_ESG%*%cbind(CW_ESG_weights))^(1/2)*dnorm(z_975,0,1)/0.025
GaussianES_CW_ESG_99=-CW_ESG_weights%*%para_CW_mu_ESG+(CW_ESG_weights%*%para_CW_SIGMA_ESG%*%cbind(CW_ESG_weights))^(1/2)*dnorm(z_99,0,1)/0.01
GaussianES_CW_nonESG_95=-CW_nonESG_weights%*%para_CW_mu_nonESG+(CW_nonESG_weights%*%para_CW_SIGMA_nonESG%*%cbind(CW_nonESG_weights))^(1/2)*dnorm(z_95,0,1)/0.05
GaussianES_CW_nonESG_975=-CW_nonESG_weights%*%para_CW_mu_nonESG+(CW_nonESG_weights%*%para_CW_SIGMA_nonESG%*%cbind(CW_nonESG_weights))^(1/2)*dnorm(z_975,0,1)/0.025
GaussianES_CW_nonESG_99=-CW_nonESG_weights%*%para_CW_mu_nonESG+(CW_nonESG_weights%*%para_CW_SIGMA_nonESG%*%cbind(CW_nonESG_weights))^(1/2)*dnorm(z_99,0,1)/0.01
sum_stat <- data.frame(GaussianES_CW_ESG_95,GaussianES_CW_ESG_975,GaussianES_CW_ESG_99,
                       GaussianES_CW_nonESG_95,GaussianES_CW_nonESG_975,GaussianES_CW_nonESG_99)
kable(sum_stat, "latex")



#Kernel
#EW
EW_data=ptf_ESG_timeserie[1:n_r,2:(ncol(ptf_ESG_timeserie)-2)]
t=-EW_data*cbind(EW_ESG_weights) #This is the negative of the returns weighted by their portfolio weight (a)
#so that we talk about risk in positive terms
w=apply(t,1,sum,na.rm=T)
z=cbind(w) #because we need a vector to make the computations 
T=length(z)
h=sd(z)*T^(-1/5)
alpha=0.05
NonParamVaR=NULL
l=1000

for (i in 1:T){
  VaR=z[i]
  x=NULL
  Minobj=NULL
  for (j in 1:T) {
    x[j]=pnorm((z[j]-VaR)/h)}
  Minobj=(sum(x)/T-alpha)^2
  if (Minobj<l) {
    l=Minobj
    NonParamVaR=VaR}
}

print(NonParamVaR)

NonParamES_EW_ESG=mean(z*pnorm((z-NonParamVaR)/h))/alpha
NonParamES_EW_ESG

#for nonESG
EW_data=ptf_nonESG_timeserie[1:n_r,2:(ncol(ptf_nonESG_timeserie)-2)]
t=-EW_data*cbind(EW_nonESG_weights) #This is the negative of the returns weighted by their portfolio weight (a)
#so that we talk about risk in positive terms
w=apply(t,1,sum,na.rm=T)
z=cbind(w) #because we need a vector to make the computations 
T=length(z)
h=sd(z)*T^(-1/5)
alpha=0.05
NonParamVaR=NULL
l=1000

for (i in 1:T){
  VaR=z[i]
  x=NULL
  Minobj=NULL
  for (j in 1:T) {
    x[j]=pnorm((z[j]-VaR)/h)}
  Minobj=(sum(x)/T-alpha)^2
  if (Minobj<l) {
    l=Minobj
    NonParamVaR=VaR}
}

print(NonParamVaR)

NonParamES_EW_nonESG=mean(z*pnorm((z-NonParamVaR)/h))/alpha
NonParamES_EW_nonESG


#CW
CW_data=ptf_ESG_timeserie[1:n_r,2:(ncol(ptf_ESG_timeserie)-2)]
t=-CW_data*cbind(CW_ESG_weights) #This is the negative of the returns weighted by their portfolio weight (a)
#so that we talk about risk in positive terms
w=apply(t,1,sum,na.rm=T)
z=cbind(w) #because we need a vector to make the computations 
T=length(z)
h=sd(z)*T^(-1/5)
alpha= 0.025
NonParamVaR=NULL
l=1000

for (i in 1:T){
  VaR=z[i]
  x=NULL
  Minobj=NULL
  for (j in 1:T) {
    x[j]=pnorm((z[j]-VaR)/h)}
  Minobj=(sum(x)/T-alpha)^2
  if (Minobj<l) {
    l=Minobj
    NonParamVaR=VaR}
}

print(NonParamVaR)

NonParamES_CW_ESG=mean(z*pnorm((z-NonParamVaR)/h))/alpha
NonParamES_CW_ESG



#for nonESG
CW_data=ptf_nonESG_timeserie[1:n_r,2:(ncol(ptf_nonESG_timeserie)-2)]
t=-CW_data*cbind(CW_nonESG_weights) #This is the negative of the returns weighted by their portfolio weight (a)
#so that we talk about risk in positive terms
w=apply(t,1,sum,na.rm=T)
z=cbind(w) #because we need a vector to make the computations 
T=length(z)
h=sd(z)*T^(-1/5)
alpha=0.01
NonParamVaR=NULL
l=1000

for (i in 1:T){
  VaR=z[i]
  x=NULL
  Minobj=NULL
  for (j in 1:T) {
    x[j]=pnorm((z[j]-VaR)/h)}
  Minobj=(sum(x)/T-alpha)^2
  if (Minobj<l) {
    l=Minobj
    NonParamVaR=VaR}
}

print(NonParamVaR)


NonParamES_CW_nonESG=mean(z*pnorm((z-NonParamVaR)/h))/alpha
NonParamES_CW_nonESG




##    Cumulative wealth with 1€ invested    ##
# EW portfolio
#ESG
logrtrn_ESG <- ptf_ESG_timeserie$EW_ret/100
#clr_ESG <- cumsum(logrtrn_ESG[!is.na(logrtrn_ESG)])
clr_ESG <- cumsum(replace_na(logrtrn_ESG,0))
clr_ESG
#cumulative wealth 
ptf_ESG_timeserie$wealth_ESG <- exp(clr_ESG)
#nonESG
logrtrn_nonESG <- ptf_nonESG_timeserie$EW_ret/100
#clr_nonESG <- cumsum(logrtrn_nonESG[!is.na(logrtrn_nonESG)])
clr_nonESG <- cumsum(replace_na(logrtrn_nonESG,0))
#cumulative wealth 
ptf_nonESG_timeserie$wealth_nonESG <- exp(clr_nonESG)

#Plot of cumulative log returns and cumulative wealth for the equal-weighted portfolio
ptf_ESG_timeserie$date <- as.Date(ptf_ESG_timeserie$date)

ggplot(ptf_ESG_timeserie,aes(date,clr_ESG))  + 
  geom_line(color="#800000") +
  geom_line(color="#93c572") +
  scale_x_date(date_labels = "%m-%Y") 

serie_wealth <- data.frame(ptf_ESG_timeserie$date,ptf_ESG_timeserie$wealth_ESG,ptf_nonESG_timeserie$wealth_nonESG)
colnames(serie_wealth)[which(names(serie_wealth) == "ptf_ESG_timeserie.date")] <- "date"
colnames(serie_wealth)[which(names(serie_wealth) == "ptf_nonESG_timeserie.wealth_nonESG")] <- "Portfolio ESG Laggards"
colnames(serie_wealth)[which(names(serie_wealth) == "ptf_ESG_timeserie.wealth_ESG")] <- "Portfolio ESG Leaders"
plot_wealth <- serie_wealth %>% pivot_longer(c('Portfolio ESG Leaders','Portfolio ESG Laggards'),names_to = "Type",values_to = "Wealth")
ggplot(plot_wealth, aes(x = date, y=Wealth, color=Type)) +
  geom_line()+
  geom_hline(yintercept=1) +
  ggtitle("Wealth of the 2 equally-weighted portfolios - from Nov. 2019 until May 2020") +
  theme(plot.title = element_text(hjust = 0.5))


# CW portfolio
#ESG
logrtrn_ESG <- ptf_ESG_timeserie$CW_ret/100
clr_ESG <- cumsum(replace_na(logrtrn_ESG,0))
clr_ESG
#cumulative wealth 
ptf_ESG_timeserie$wealth_ESG <- exp(clr_ESG)
#nonESG
logrtrn_nonESG <- ptf_nonESG_timeserie$CW_ret/100
clr_nonESG <- cumsum(replace_na(logrtrn_nonESG,0))
#cumulative wealth 
ptf_nonESG_timeserie$wealth_nonESG <- exp(clr_nonESG)
ptf_ESG_timeserie$date <- as.Date(ptf_ESG_timeserie$date)
serie_wealth <- data.frame(ptf_ESG_timeserie$date,ptf_ESG_timeserie$wealth_ESG,ptf_nonESG_timeserie$wealth_nonESG)
colnames(serie_wealth)[which(names(serie_wealth) == "ptf_ESG_timeserie.date")] <- "date"
colnames(serie_wealth)[which(names(serie_wealth) == "ptf_nonESG_timeserie.wealth_nonESG")] <- "Portfolio ESG Laggards"
colnames(serie_wealth)[which(names(serie_wealth) == "ptf_ESG_timeserie.wealth_ESG")] <- "Portfolio ESG Leaders"
plot_wealth <- serie_wealth %>% pivot_longer(c('Portfolio ESG Leaders','Portfolio ESG Laggards'),names_to = "Type",values_to = "Wealth")
ggplot(plot_wealth, aes(x = date, y=Wealth, color=Type)) +
  geom_line()+
  geom_hline(yintercept=1) +
  ggtitle("Wealth of the 2 capitalization-weighted portfolios - from Nov. 2019 until May 2020") +
  theme(plot.title = element_text(hjust = 0.5))

