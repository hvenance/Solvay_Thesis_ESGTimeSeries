rm(list=ls())

#library(xtable)
library(knitr)
library(tidyverse)
library(ggplot2)
library(readxl)
library(dplyr)
#library(caret)
library(reshape2)
library(ggcorrplot)
#library(eventstudies)
library(xts)
library(zoo)
library(writexl)

#install.packages("eventstudies")
#install.packages("Rtools")

rm(list=ls())

#read data
STOXX600_timeserie <- read.csv("C:/Users/Hadrien Venance/Desktop/Thesis/Data/STOXX600_timeseries_new_dataset.csv")
ESG_timeserie <- read.csv("C:/Users/Hadrien Venance/Desktop/Thesis/Data/ESG_leaders_highPrice_timeseries.csv", na.strings="")
nonESG_timeserie <- read.csv("C:/Users/Hadrien Venance/Desktop/Thesis/Data/ESG_laggards_highPrice_timeseries.csv", na.strings="")

ESG <- read_excel("C:/Users/Hadrien Venance/Desktop/Thesis/Data/ESG_leaders_highPrice - Copy.xlsx")
nonESG<- read_excel("C:/Users/Hadrien Venance/Desktop/Thesis/Data/ESG_laggards_highPrice - Copy.xlsx")

names(nonESG)[names(nonESG) == 'Identifier (RIC)'] <- 'Identifier'
names(ESG)[names(ESG) == 'Identifier (RIC)'] <- 'Identifier'

colnames(ESG)
colnames(nonESG)

names(ESG)[names(ESG) == 'ROE Total Equity %  - 2019\n(FY-2)'] <- 'ROE Total Equity % - 2019\n(FY-2)'

#Retrieve names of companies for each group (Leaders/Laggards)
#Leaders
colnames(ESG_timeserie) <- ESG_timeserie[1,]
ESG_timeserie <- ESG_timeserie[-c(1:2),]
ptf_ESG_timeserie <- data.frame(ESG_timeserie$Instrument, ESG_timeserie$SAPG.DE, ESG_timeserie$ROG.S, ESG_timeserie$LONN.S,ESG_timeserie$MNDI.L, 
                                ESG_timeserie$ALFA.ST, ESG_timeserie$GASI.MI, ESG_timeserie$ALVG.DE, ESG_timeserie$ZURN.S,
                                ESG_timeserie$IBE.MC, ESG_timeserie$NTGY.MC, ESG_timeserie$REDE.MC,  ESG_timeserie$PSON.L,  
                                ESG_timeserie$WTB.L, ESG_timeserie$ELIOR.PA, ESG_timeserie$OREP.PA)
ESG_names <- c("SAPG.DE","ROG.S","LONN.S","MNDI.L","ALFA.ST","GASI.MI","ALVG.DE","ZURN.S","IBE.MC","NTGY.MC","REDE.MC","PSON.L","WTB.L","ELIOR.PA","OREP.PA")
ESG_characteristics <- ESG[ESG$Identifier %in% ESG_names,]
ESG_countries <- unique(ESG_characteristics$`Country of Exchange`)

#Laggards 
colnames(nonESG_timeserie) <- nonESG_timeserie[1,]
nonESG_timeserie <- nonESG_timeserie[-c(1:2),]
ptf_nonESG_timeserie <- data.frame(nonESG_timeserie$Instrument, nonESG_timeserie$A1OS.DE, nonESG_timeserie$SMHNn.DE, nonESG_timeserie$PSAGn.DE,
                                   nonESG_timeserie$CLTN.S, nonESG_timeserie$COPN.S, nonESG_timeserie$JDG.L, nonESG_timeserie$MRLM.L, 
                                   nonESG_timeserie$JETPAK.ST, nonESG_timeserie$TIP.MI, nonESG_timeserie$FTKn.DE, nonESG_timeserie$SQN.S, nonESG_timeserie$PEHN.S,
                                   nonESG_timeserie$SLRS.MC, nonESG_timeserie$TRNT.L, nonESG_timeserie$CVSG.L, nonESG_timeserie$IPAR.PA, nonESG_timeserie$TRIA.PA)
#nonESG_names <- c("A1OS.DE","SMHNn.DE","PSAGn.DE","CLTN.S","COPN.S","JDG.L","MRLM.L","JETPAK.ST","TIP.MI","FTKn.DE","SQN.S","PEHN.S",
#                  "SLRS.MC","TRNT.L","CVSG.L","IPAR.PA","TRIA.PA")
nonESG_names <- c("A1OS.DE","SMHNn.DE","PSAGn.DE","CLTN.S","JDG.L","MRLM.L","JETPAK.ST","TIP.MI","FTKn.DE","SQN.S","PEHN.S",
                  "SLRS.MC","CVSG.L","IPAR.PA","TRIA.PA")
nonESG_characteristics <- nonESG[nonESG$Identifier %in% nonESG_names,]
nonESG_countries <- unique(nonESG_characteristics$`Country of Exchange`)
#we have the same 7 countries for each group 

#need to create the df that will be used for analysis in Stata
#columns needed
#in excel template: miss VOLATILITY, Rev_growth --> OK
#for volatility, need to download data of 2019 and then take volatility --> in Excel manual add of a column
ESG_volatility <- read.csv("C:/Users/Hadrien Venance/Desktop/Thesis/Data/ESG_leaders_volatility.csv", na.strings="")
colnames(ESG_volatility) <- ESG_volatility[1,]
ESG_volatility <- ESG_volatility[-c(1:2),]
ESG_volatility["Total",] <- apply(ESG_volatility, 2, sd, na.rm=T)
colnames(ESG_characteristics)
ESG_stata1 <- data.frame(ESG_characteristics$Identifier, ESG_characteristics$`Country of Exchange`, ESG_characteristics$`ESG Combined Score\r\n(FY-2)`, ESG_characteristics$`52 Week Total Return\r\n(-2CY)`, 
                         ESG_characteristics$`Organization Founded Year`, ESG_characteristics$Volatility , 
                         ESG_characteristics$`Revenue - SmartEstimate Growth (This Yr/Last Yr)\r\n(-2CY)`, ESG_characteristics$`Total Debt Percentage of Total Equity\r\n(FY-2)`,
                         ESG_characteristics$`ROA Total Assets, Percent - 2019\r\n(1D, FY-2)`)
#for age of company: edit the column "founded year
ESG_stata1$Age <- rep(2022, nrow(ESG_stata1)) - as.numeric(ESG_stata1$ESG_characteristics..Organization.Founded.Year.)

#in loop PYTHON: market cap, PE, ROA, maybe ESG score but not sure 
#time-series characteristics
ESG_capit <- read.csv("C:/Users/Hadrien Venance/Desktop/Thesis/Data/ESG_leaders_highPrice_capitalization.csv", na.strings="")
colnames(ESG_capit) <- ESG_capit[1,]
ESG_capit <- ESG_capit[-c(1:2),]
ESG_capit_dates <- ESG_capit$Instrument
ESG_capit <- ESG_capit %>% select(everything()) %>% mutate_if(is.character, as.numeric)
ESG_capit <- ESG_capit[,ESG_names]
#add column dates
ESG_capit$date <- ESG_capit_dates
ESG_capit$date <- sub("T00:00:00Z", "", ESG_capit$date)
ESG_capit <- ESG_capit %>% select(date, everything()) %>%
  arrange(date = as.Date(date, format="%d/%m/%Y"))
#input mean values for all NAs in each of the preceding df
#ESG_capit2 <- ESG_capit                                              # Duplicate ESG_capit frame
for(i in 1:ncol(ESG_capit)) {                                   # Replace NA in all columns
  ESG_capit[ , i][is.na(ESG_capit[ , i])] <- mean(ESG_capit[ , i], na.rm = TRUE)
}
ESG_capit   
#pivot all columns into wide format: dates repeating, 1 column with names of firms
ESG_capit_names <- colnames(ESG_capit[,-1])
ESG_capit_pivot <- ESG_capit %>% pivot_longer(ESG_capit_names,names_to = "Firms",values_to = "Market_Cap")

ESG_PE <- read.csv("C:/Users/Hadrien Venance/Desktop/Thesis/Data/ESG_leaders_highPrice_PE.csv", na.strings="")
colnames(ESG_PE) <- ESG_PE[1,]
ESG_PE <- ESG_PE[-c(1:2),]
ESG_PE_dates <- ESG_PE$Instrument
ESG_PE <- ESG_PE %>% select(everything()) %>% mutate_if(is.character, as.numeric)
ESG_PE <- ESG_PE[,ESG_names]
#add column dates
ESG_PE$date <- ESG_PE_dates
ESG_PE$date <- sub("T00:00:00Z", "", ESG_PE$date)
ESG_PE <- ESG_PE %>% select(date, everything()) %>%
  arrange(date = as.Date(date, format="%d/%m/%Y"))
#input mean values for all NAs in each of the preceding df
for(i in 1:ncol(ESG_PE)) {                                   # Replace NA in all columns
  ESG_PE[ , i][is.na(ESG_PE[ , i])] <- mean(ESG_PE[ , i], na.rm = TRUE)
}
ESG_PE   
#pivot all columns into wide format: dates repeating, 1 column with names of firms
ESG_PE_names <- colnames(ESG_PE[,-1])
ESG_PE_pivot <- ESG_PE %>% pivot_longer(ESG_PE_names,names_to = "Firms",values_to = "Price-to-Earnings")
ESG_stata <- merge(ESG_capit_pivot,ESG_PE_pivot, by=c("date","Firms"))

#ROA is not a time serie but only "static" stats
#ESG_ROA <- read.csv("C:/Users/Hadrien Venance/Desktop/Thesis/Data/ESG_leaders_highPrice_ROA.csv", na.strings="")

#merge time-series of real returns (=EX-POST) ??

#create dummy for ESG Leader or not
ESG_stata$Leaders <- rep(1, nrow(ESG_stata))

#combine the 2 ESG stata dfs 
ESG_stata1 <- ESG_stata1[,-5] #remove the foundation year
colnames(ESG_stata1) <- c("Firms", "Country", "ESG_Combined_Score", "Return_2019", "Volatility_2019", "Rev_Growth_2019", "Debt_to_Equity_2019", "ROA_2019", "Age")
#colnames(ESG_stata1) <- c("Firms", "ESG_Combined_Score", "Return_2019", "Volatility_2019", "Rev_Growth_2019", "ROA", "Debt_to_Equity_2019", "Age")
ESG_stata <- left_join(ESG_stata,ESG_stata1, sort=T)

#write_xlsx(ESG_stata, "C:/Users/Hadrien Venance/Downloads/ESG_stata.xlsx")


## NON ESG ##
nonESG_volatility <- read.csv("C:/Users/Hadrien Venance/Desktop/Thesis/Data/ESG_laggards_volatility.csv", na.strings="")
colnames(nonESG_volatility) <- nonESG_volatility[1,]
nonESG_volatility <- nonESG_volatility[-c(1:2),]
nonESG_volatility["Total",] <- apply(nonESG_volatility, 2, sd, na.rm=T)
nonESG_stata1 <- data.frame(nonESG_characteristics$Identifier, nonESG_characteristics$`Country of Exchange`, nonESG_characteristics$`ESG Combined Score\r\n(FY-2)`, nonESG_characteristics$`52 Week Total Return\r\n(-2CY)`, 
                         nonESG_characteristics$`Organization Founded Year`, nonESG_characteristics$Volatility , 
                         nonESG_characteristics$`Revenue - SmartEstimate Growth (This Yr/Last Yr)\r\n(-2CY)` , nonESG_characteristics$`Total Debt Percentage of Total Equity\r\n(FY-2)`,
                         nonESG_characteristics$`ROA Total Assets, Percent - 2019\r\n(1D, FY-2)`)
#for age of company: edit the column "founded year
nonESG_stata1$Age <- rep(2022, nrow(nonESG_stata1)) - as.numeric(nonESG_stata1$nonESG_characteristics..Organization.Founded.Year.)

#in loop PYTHON: market cap, PE, ROA, maybe ESG score but not sure 
#time-series characteristics
nonESG_capit <- read.csv("C:/Users/Hadrien Venance/Desktop/Thesis/Data/ESG_laggards_highPrice_capitalization.csv", na.strings="")
colnames(nonESG_capit) <- nonESG_capit[1,]
nonESG_capit <- nonESG_capit[-c(1:2),]
nonESG_capit_dates <- nonESG_capit$Instrument
nonESG_capit <- nonESG_capit %>% select(everything()) %>% mutate_if(is.character, as.numeric)
nonESG_capit <- nonESG_capit[,nonESG_names]
#add column dates
nonESG_capit$date <- nonESG_capit_dates
nonESG_capit$date <- sub("T00:00:00Z", "", nonESG_capit$date)
nonESG_capit <- nonESG_capit %>% select(date, everything()) %>%
  arrange(date = as.Date(date, format="%d/%m/%Y"))
#input mean values for all NAs in each of the preceding df
#nonESG_capit2 <- nonESG_capit                                              # Duplicate nonESG_capit frame
for(i in 1:ncol(nonESG_capit)) {                                   # Replace NA in all columns
  nonESG_capit[ , i][is.na(nonESG_capit[ , i])] <- mean(nonESG_capit[ , i], na.rm = TRUE)
}
nonESG_capit   
#pivot all columns into wide format: dates repeating, 1 column with names of firms
nonESG_capit_names <- colnames(nonESG_capit[,-1])
nonESG_capit_pivot <- nonESG_capit %>% pivot_longer(nonESG_capit_names,names_to = "Firms",values_to = "Market_Cap")

nonESG_PE <- read.csv("C:/Users/Hadrien Venance/Desktop/Thesis/Data/ESG_laggards_highPrice_PE.csv", na.strings="")
colnames(nonESG_PE) <- nonESG_PE[1,]
nonESG_PE <- nonESG_PE[-c(1:2),]
nonESG_PE_dates <- nonESG_PE$Instrument
nonESG_PE <- nonESG_PE %>% select(everything()) %>% mutate_if(is.character, as.numeric)
colnames(nonESG_PE)
#miss COPN and TRNT
nonESG_PE <- nonESG_PE[,nonESG_names]
#add column dates
nonESG_PE$date <- nonESG_PE_dates
nonESG_PE$date <- sub("T00:00:00Z", "", nonESG_PE$date)
nonESG_PE <- nonESG_PE %>% select(date, everything()) %>%
  arrange(date = as.Date(date, format="%d/%m/%Y"))
#input mean values for all NAs in each of the preceding df
for(i in 1:ncol(nonESG_PE)) {                                   # Replace NA in all columns
  nonESG_PE[ , i][is.na(nonESG_PE[ , i])] <- mean(nonESG_PE[ , i], na.rm = TRUE)
}
nonESG_PE   
#pivot all columns into wide format: dates repeating, 1 column with names of firms
nonESG_PE_names <- colnames(nonESG_PE[,-1])
nonESG_PE_pivot <- nonESG_PE %>% pivot_longer(nonESG_PE_names,names_to = "Firms",values_to = "Price-to-Earnings")
nonESG_stata <- merge(nonESG_capit_pivot,nonESG_PE_pivot, by=c("date","Firms"))

#create dummy for ESG Leader or not
nonESG_stata$Leaders <- rep(1, nrow(nonESG_stata))

#combine the 2 ESG stata dfs 
nonESG_stata1 <- nonESG_stata1[,-5] #remove the foundation year
colnames(nonESG_stata1) <- c("Firms", "Country", "ESG_Combined_Score", "Return_2019", "Volatility_2019", "Rev_Growth_2019", "Debt_to_Equity_2019", "ROA", "Age")
nonESG_stata <- left_join(nonESG_stata,nonESG_stata1, sort=T)

#write_xlsx(nonESG_stata, "C:/Users/Hadrien Venance/Downloads/ESG_stata.xlsx")


#add Covid cases
#ADD ABOVE THE COUNTRY OF EXCHANGE AND
covid_cases <- read.csv("C:/Users/Hadrien Venance/Desktop/Thesis/Data/owid-covid-data.csv", na.strings="")
covid_cases <- covid_cases[,c(1,3,4,5,6)]
covid_cases <- covid_cases[covid_cases$location %in% ESG_countries,]

library(writexl)
#write_xlsx(covid_cases, "C:/Users/Hadrien Venance/Downloads/covid_cases.xlsx")
#write_xlsx(nonESG_stata, "C:/Users/Hadrien Venance/Downloads/nonESG_stata.xlsx")

ESG_stata <- read_excel("C:/Users/Hadrien Venance/Desktop/Thesis/Data/ESG_leaders_stata.xlsx")
nonESG_stata <- read_excel("C:/Users/Hadrien Venance/Desktop/Thesis/Data/ESG_laggards_stata.xlsx")
ESG_stata <- ESG_stata[,-1]
nonESG_stata <- nonESG_stata[,-1]
names(ESG_stata)[names(ESG_stata) == "total_cases"] <- "Total_Cases"
names(nonESG_stata)[names(nonESG_stata) == "Total cases"] <- "Total_Cases"
names(ESG_stata)[names(ESG_stata) == "new_cases"] <- "New_Cases"
names(nonESG_stata)[names(nonESG_stata) == "New cases"] <- "New_Cases"
names(ESG_stata)[names(ESG_stata) == "ROA"] <- "ROA_2019"
names(nonESG_stata)[names(nonESG_stata) == "ROA"] <- "ROA_2019"

## MAYBE ADD TIME SERIES OF ESG SCORE AND NOT LET IT AS STATIC CHARACTERISTIC
# NO as Refinitiv only creates new scores once a year


#combine 2 ESG and nonESG dfs
stata <- rbind(ESG_stata, nonESG_stata)
#write_xlsx(stata, "C:/Users/Hadrien Venance/Desktop/Thesis/Data/stata.xlsx")

#new edits to add the market returns per countries 
stata <- read_excel("C:/Users/Hadrien Venance/Desktop/Thesis/Data/stata.xlsx")
indices <- read.csv("C:/Users/Hadrien Venance/Desktop/Thesis/Data/stata_indexes_countries.csv")

colnames(indices) <- indices[1,]
indices <- indices[-c(1:2),]
indices_dates <- indices$Instrument
indices <- indices %>% select(everything()) %>% mutate_if(is.character, as.numeric)
colnames(indices)
n_row <- length(indices[,1])
#transform close prices in returns
"French_Index"<- data.frame(100*((indices[2:n_row,2]-indices[1:(n_row-1),2])/indices[1:(n_row-1),2]))
`French_Index` <- rbind(NA,`French_Index`)
colnames(`French_Index`) <- "French_Index"
`French_Index`$date <- indices_dates
`French_Index` <- `French_Index`%>%
  relocate(date, .before=`French_Index`)

`German_Index`<- data.frame(100*((indices[2:n_row,5]-indices[1:(n_row-1),5])/indices[1:(n_row-1),5]))
`German_Index` <- rbind(NA,`German_Index`)
colnames(`German_Index`) <- "German_Index"
`German_Index`$date <- indices_dates
`German_Index`<- `German_Index`%>%
  relocate(date, .before=`German_Index`)

`UK_Index`<- data.frame(100*((indices[2:n_row,4]-indices[1:(n_row-1),4])/indices[1:(n_row-1),4]))
`UK_Index` <- rbind(NA,`UK_Index`)
colnames(`UK_Index`) <- "UK_Index"
`UK_Index`$date <- indices_dates
`UK_Index`<- `UK_Index`%>%
  relocate(date, .before=`UK_Index`)

`Italian_Index`<- data.frame(100*((indices[2:n_row,3]-indices[1:(n_row-1),3])/indices[1:(n_row-1),3]))
`Italian_Index` <- rbind(NA,`Italian_Index`)
colnames(`Italian_Index`) <- "Italian_Index"
`Italian_Index`$date <- indices_dates
`Italian_Index`<- `Italian_Index`%>%
  relocate(date, .before=`Italian_Index`)

`Spanish_Index`<- data.frame(100*((indices[2:n_row,6]-indices[1:(n_row-1),6])/indices[1:(n_row-1),6]))
`Spanish_Index` <- rbind(NA,`Spanish_Index`)
colnames(`Spanish_Index`) <- "Spanish_Index"
`Spanish_Index`$date <- indices_dates
`Spanish_Index`<- `Spanish_Index`%>%
  relocate(date, .before=`Spanish_Index`)

`Swedish_Index`<- data.frame(100*((indices[2:n_row,7]-indices[1:(n_row-1),7])/indices[1:(n_row-1),7]))
`Swedish_Index` <- rbind(NA,`Swedish_Index`)
colnames(`Swedish_Index`) <- "Swedish_Index"
`Swedish_Index`$date <- indices_dates
`Swedish_Index`<- `Swedish_Index`%>%
  relocate(date, .before=`Swedish_Index`)

`Swiss_Index`<- data.frame(100*((indices[2:n_row,8]-indices[1:(n_row-1),8])/indices[1:(n_row-1),8]))
`Swiss_Index` <- rbind(NA,`Swiss_Index`)
colnames(`Swiss_Index`) <- "Swiss_Index"
`Swiss_Index`$date <- indices_dates
`Swiss_Index`<- `Swiss_Index`%>%
  relocate(date, .before=`Swiss_Index`)

indices1 <- merge(`French_Index`,Italian_Index, by= "date")
indices1 <- merge(indices1, UK_Index, by="date")
indices1 <- merge(indices1, German_Index, by="date")
indices1 <- merge(indices1, Spanish_Index, by="date")
indices1 <- merge(indices1, Swedish_Index, by="date")
indices1 <- merge(indices1, Swiss_Index, by="date")

#add column dates
indices1$date <- indices_dates
indices1$date <- sub("T00:00:00Z", "", indices1$date)
indices1 <- indices1 %>% select(date, everything()) %>%
  arrange(date = as.Date(date, format="%d/%m/%Y"))
#input mean values for all NAs in each of the preceding df
for(i in 1:ncol(indices1)) {                                   # Replace NA in all columns
  indices1[ , i][is.na(indices1[ , i])] <- mean(indices1[ , i], na.rm = TRUE)
}
indices1   

#now I have all returns of market per countries, I need to make them match with firms of specific countries in Excel
write_xlsx(indices1, "C:/Users/Hadrien Venance/Desktop/Thesis/Data/stata_mkt_returns_per_countries.xlsx")

#stata_1<- merge(indices1_pivot,stata, by=c("date","Firms"))




#add market returns
STOXX600_timeserie <- read.csv("C:/Users/Hadrien Venance/Desktop/Thesis/Data/STOXX600_timeseries_new_dataset.csv")
STOXX600_timeserie$Market_Return <- 100*log(STOXX600_timeserie$Close.Price/lag(STOXX600_timeserie$Close.Price))
#repeat df to have it for each firms
rows= c(1:nrow(STOXX600_timeserie))
times = 15
STOXX600_timeserie <- STOXX600_timeserie[rep(rows, times),]
#arrange it by date
STOXX600_timeserie <- STOXX600_timeserie %>% arrange(Date)
#write_xlsx(STOXX600_timeserie, "C:/Users/Hadrien Venance/Downloads/STOXX600.xlsx")

#add HML
ff_data = read.csv("C:/Users/Hadrien Venance/Desktop/Thesis/Code/Europe_3_Factors_Daily.csv", skip = 3)
colnames(ff_data)[1] = "Date"
# convert dates in R date format
ff_data$Date = as.Date(strptime(ff_data$Date, format = "%Y%m%d"))
head(ff_data)

HML <- data.frame(ff_data$Date, ff_data$HML)
HML = HML[HML$ff_data.Date >= "2019-10-01" & HML$ff_data.Date <= "2020-12-31",]
HML <- HML[rep(rows, times),]
#arrange it by date
HML <- HML %>% arrange(ff_data.Date)
#write_xlsx(HML, "C:/Users/Hadrien Venance/Downloads/HML.xlsx")

#add SMB 
SMB <- data.frame(ff_data$Date, ff_data$SMB)
SMB = SMB[SMB$ff_data.Date >= "2019-10-01" & SMB$ff_data.Date <= "2020-12-31",]
SMB <- SMB[rep(rows, times),]
SMB <- SMB %>% arrange(ff_data.Date)
#write_xlsx(SMB, "C:/Users/Hadrien Venance/Downloads/SMB.xlsx")


stata_excel <- read_excel("C:/Users/Hadrien Venance/Desktop/Thesis/Data/stata.xlsx")
colnames(stata_excel)

#predicted/fitted values --> FE Regressions


#compute abnormal values


#calculate mean values for each indicator according to ESG belonging or not





