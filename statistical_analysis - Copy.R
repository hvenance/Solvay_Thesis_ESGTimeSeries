rm(list=ls())
library(tidyverse)
library(ggplot2)
library(readxl)
library(dplyr)
library(caret)
library(reshape2)
library(ggcorrplot)


##time series of stock price##
nonESG_timeserie <- read.csv("C:/Users/Hadrien Venance/Desktop/Thesis/Data/nonSRI_price_timeseries.csv", na.strings = "")
ESG_timeserie <- read.csv("C:/Users/Hadrien Venance/Desktop/Thesis/Data/SRI_price_timeseries.csv",na.strings = "")
STOXX600_timeserie <- read.csv("C:/Users/Hadrien Venance/Desktop/Thesis/Data/STOXX600_timeseries.csv")

#rearrange the df
colnames(nonESG_timeserie) <- nonESG_timeserie[1,]
nonESG_timeserie <- nonESG_timeserie[-c(1:4),]

#to know which columns have a lot a NA
#remove if 5% or more of NA values
wx <- cbind(
  lapply(
    lapply(nonESG_timeserie, is.na)
    , sum)
)
to_remove <- which(wx > 22)
remove_names_nonESG <- colnames(nonESG_timeserie[,to_remove])
nonESG_timeserie <- nonESG_timeserie[,-to_remove] #remove instruments with a lot of missing values

#to know which rows have a lot a NA#
wx1 <- rowSums(is.na(nonESG_timeserie))
#remove if more than 10% of companies are missing values that day
date_to_remove1 <- nonESG_timeserie$Instrument[which(wx1 > 21)]


colnames(ESG_timeserie) <- ESG_timeserie[1,]
ESG_timeserie <- ESG_timeserie[-c(1:2),]


#remove if 5% or more of NA values
ww <- cbind(
  lapply(
    lapply(ESG_timeserie, is.na)
    , sum)
)
to_remove_w <- which(ww > 22)
remove_names_ESG <- colnames(ESG_timeserie[,to_remove_w])
ESG_timeserie <- ESG_timeserie[,-to_remove_w] #remove instruments with a lot of missing values

ww1 <- rowSums(is.na(ESG_timeserie))
#remove if more than 10% of companies are missing values that day
date_to_remove2 <- ESG_timeserie$Instrument[which(ww1 > 21)]

STOXX600_timeserie <- STOXX600_timeserie[,-c(1:2)]


#remove for the previous dates + Remove the date of 2nd January as it seems to be a huge outlier (value of 0)
date_to_remove <- c(date_to_remove1, date_to_remove2)
date_to_remove <- c(date_to_remove1, date_to_remove2, "2020-01-02T00:00:00Z")

#filter: based on instrument not in date_to_remove 

ESG_timeserie <- ESG_timeserie %>% filter(
  !Instrument %in% date_to_remove
)
nonESG_timeserie <- nonESG_timeserie %>% filter(
  !Instrument %in% date_to_remove
)
STOXX600_timeserie <- STOXX600_timeserie %>% filter(
  !Date %in% date_to_remove
)




#### equal-weight ptf (same importance for each stock) ####

#transform values/obs in numeric
nonESG_timeserie1 <- as.data.frame(lapply(nonESG_timeserie, as.numeric))
EW_nonESG <- as.data.frame(rowMeans(nonESG_timeserie1, na.rm = T))
EW_nonESG$date <- nonESG_timeserie$Instrument

ESG_timeserie1 <- as.data.frame(lapply(ESG_timeserie[,-1], as.numeric))
EW_ESG <- as.data.frame(rowMeans(ESG_timeserie1, na.rm = T))
EW_ESG$date <- ESG_timeserie$Instrument

EW_STOXX <- STOXX600_timeserie[,-2] #drop volume column
colnames(EW_STOXX)[1] <- "date"

#gather the 3 time series in one df
time_series <- full_join(EW_ESG,EW_nonESG, by="date")
time_series <- full_join(time_series,EW_STOXX, by="date")
time_series <- time_series %>% select(date, everything()) %>%
  arrange(date = as.Date(date, "%d/%m/%Y"))

colnames(time_series) <- c('date','ESG','nonESG','STOXX600')

#transform date format
time_series$date <- gsub('.{10}$', '', time_series$date)
time_series$date <- as.Date(time_series$date)

#remove 1st date as nonESG would have NA value
time_series <- time_series[-1,]

#test if I have NAs in my df 
apply(time_series,2, function(x) sum(is.na(x)))
#they are the last 9 rows so I need to remove them
time_series <- slice(time_series, 1:(n() -9))


#### DATA VISUALIZATION ####
#normalize the data for visualisation purpose
series.norm <- preProcess(time_series,method = 'range')
plot_time_series <- predict(series.norm,time_series)

#pivot for the plot
time <- plot_time_series %>% pivot_longer(c('ESG','nonESG','STOXX600'),names_to = "Type",values_to = "Price")

#plot of the 3 time series
ggplot(time, aes(x = date, y=Price, color=Type)) +
  geom_line()+
  ggtitle("Prices normalized of the 3 types of investments") +
  theme(plot.title = element_text(hjust = 0.5))


# Correlation between the time series #
var(time_series$ESG, na.rm = T)
var(time_series$nonESG, na.rm = T)

correlation <- cor(time_series[,-1], use = "complete.obs")
correlation

#correlation heatmap
melted_series <- melt(correlation)
ggplot(data = melted_series, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()

ggcorrplot(correlation)+
  ggtitle("Correlation heatmap between the 3 types of investments") +
  theme(plot.title = element_text(hjust = 0.5))




## industry ##
raw_ESG <- read_excel("C:/Users/Hadrien Venance/Desktop/Thesis/Data/data_SRI.xlsx")
raw_nonESG <- read_excel("C:/Users/Hadrien Venance/Desktop/Thesis/Data/data_non_SRI.xlsx")

industry_ESG <- raw_ESG %>% 
    filter(
    !`Identifier (RIC)` %in% remove_names_ESG) %>%
  count(`ICB Industry name`)

industry_nonESG <- 
  raw_nonESG %>%  
  filter(
    !`Identifier (RIC)` %in% remove_names_nonESG) %>%
    count(`ICB Industry name`)

#industries <- full_join(industry_ESG, industry_nonESG)
#industries

industries <- rename(industry_ESG, ESG_count = n )

#remove the last row of NA in nonESG
add <- industry_nonESG %>% filter(row_number() <= n()-1)
industries$nonESG_count = add$n

industries <- rbind(industries, c("Total", colSums(industries[,2:3])))
#add back the removed row
new_row <- c('NA',0,4)
industries <- rbind(industries,new_row)

View(industries)



#country of exchange#
country_ESG <- raw_ESG %>%     
  filter(
  !`Identifier (RIC)` %in% remove_names_ESG) %>%
  count(`Country of Exchange`)

country_nonESG <- raw_nonESG %>% 
  filter(
  !`Identifier (RIC)` %in% remove_names_nonESG) %>%
  count(`Country of Exchange`)

country <- merge(country_ESG, country_nonESG, by="Country of Exchange", all.x = T, all.y = T)
country <- rename(country, ESG = n.x , nonESG = n.y)

countries <- as.data.frame(lapply(country, as.numeric))
countries$`Country.of.Exchange` <- country$`Country of Exchange`

countries <- countries %>%
  rename(
    `Country of Exchange` = Country.of.Exchange
  )

countries <- rbind(countries, c("Total", colSums(countries[,2:3], na.rm = T)))

View(countries)






##  RETURN COMPUTATION ##
#Convert prices to returns (log returns in %)
returns = as.data.frame(sapply(time_series[,c(2:4)], function(x) diff(log(x)) * 100))  #note it will be one less
# create a different dataframe with returns
return_time_series = as.data.frame(cbind(Date = time_series$date[2:length(time_series$date)],
                                         returns), stringsAsFactors = FALSE, row.names = NULL)
# visual inspection
head(return_time_series)


######    SUMMARY STATISTICS    ######
library(psych)  #load the required package
args(describe)  #arguments for describe function

function (x, na.rm = TRUE, interp = FALSE, skew = TRUE, ranges = TRUE, 
          trim = 0.1, type = 3, check = TRUE, fast = NULL, quant = NULL, 
          IQR = FALSE, omit = FALSE, data = NULL) 
  NULL  

# use describe to calculate descriptive stats for data_cs1.1r
desc1 = describe(return_time_series[, 2:4])  #note we dont pass the date column
# check the output
head(desc1)


library(pastecs)
#install.packages('pander')
library(pander)
desc_stat1 = stat.desc(return_time_series[, 2:4], norm = TRUE)
pander(desc_stat1, caption = "Descriptive Statistics", split.table = Inf)




######    LINEAR REGRESSION    ######

### FOR ESG investments ###
#preliminary visualization
p1 = ggplot(return_time_series, aes(STOXX600, ESG))
p1 + geom_point(colour = "brown") + geom_smooth(method = "lm") + theme_minimal() +
  labs(title = "Scatter plot of STOXX600vsESG and Linear Fit")

p2 = ggplot(return_time_series, aes(Date))
p2 + geom_line(aes(y = ESG, color = "ESG"), size = 1, lty = 1) + geom_line(aes(y = STOXX600,
                                                                               color = "STOXX600"), size = 1, lty = 2) + scale_color_discrete("Asset") +
  theme_minimal() + labs("Line Chart of Returns")


#basic CAPM
lreg1 = lm(formula = ESG ~ STOXX600, data = return_time_series)
summary(lreg1)  #to generate main results

library(stargazer)
stargazer(lreg1, type = "text", title = "Regression Results")
#to copy paste in overleaf
stargazer(lreg1, type = "latex", title = "Regression Results")


## Multiple regression/ Fama-French 3 Factors ##
ff_data = read.csv("C:/Users/Hadrien Venance/Desktop/Thesis/Code/Europe_3_Factors_Daily.csv", skip = 3)
ff_data = na.omit(ff_data)  #remove missing values
head(ff_data)  #date is missing column names

colnames(ff_data)[1] = "Date"
# convert dates in R date format
ff_data$Date = as.Date(strptime(ff_data$Date, format = "%Y%m%d"))
head(ff_data)

# use merge (can use left_join from dplyr as well) to combine the stock returns and factor data
data_ffex = merge(return_time_series, ff_data, by = "Date")

## ATTENTION: in this work we will assume that the market return = STOXX600
# create another column with ESG_rf
data_ffex$ESG_rf = data_ffex$ESG - data_ffex$RF

ff_lreg = lm(ESG_rf ~ STOXX600 + SMB + HML, data = data_ffex)
stargazer(ff_lreg, summary = T, title = "Fama-French Regression OLS", type = "text")
stargazer(ff_lreg, summary = T, title = "Fama-French Regression OLS", type = "latex")

# As a digression, the marginal effect of SMB
library(ggeffects)
#install.packages("ggeffects")
mydf = ggpredict(ff_lreg, terms = c("SMB"))
(p_ff = plot(mydf) + geom_point(data = data_ffex, aes(x = SMB, y = ESG_rf),
                                color = "darkblue"))


## Fama French 4 factors ?? ##




## Panel regression ## 
tr <- t(ESG_timeserie)
tr$date <- 
p1 = ggplot(tr, aes(date, GDP, group = Country))
p1 + geom_path(aes(color = Country)) + theme_minimal() + theme(legend.position = "top")

#fixed and random effects
#install.packages('plm')
library(plm)







