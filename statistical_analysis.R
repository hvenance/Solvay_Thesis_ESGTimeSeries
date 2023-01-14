rm(list=ls())
library(tidyverse)
library(ggplot2)
library(readxl)
library(dplyr)
library(reshape2)
library(ggcorrplot)

#remove.packages("fPortfolio")
#install.packages("fPortfolio")
#library(fPortfolio)
#dev.off(dev.list()["RStudioGD"])

##time series of stock price##
#nonESG_timeserie <- read.csv("C:/Users/Hadrien Venance/Desktop/Thesis/Data/nonSRI_price_timeseries.csv", na.strings = "")
#ESG_timeserie <- read.csv("C:/Users/Hadrien Venance/Desktop/Thesis/Data/SRI_price_timeseries.csv",na.strings = "")
#STOXX600_timeserie <- read.csv("C:/Users/Hadrien Venance/Desktop/Thesis/Data/STOXX600_timeseries.csv")

######
STOXX600_timeserie <- read.csv("C:/Users/Hadrien Venance/Desktop/Thesis/Data/STOXX600_timeseries_new_dataset.csv")
ESG_timeserie <- read.csv("C:/Users/Hadrien Venance/Desktop/Thesis/Data/ESG_leaders_highPrice_timeseries.csv", na.strings="")
nonESG_timeserie <- read.csv("C:/Users/Hadrien Venance/Desktop/Thesis/Data/ESG_laggards_highPrice_timeseries.csv", na.strings="")
######


#rearrange the df
colnames(nonESG_timeserie) <- nonESG_timeserie[1,]
nonESG_timeserie <- nonESG_timeserie[-c(1:4),]

######
nonESG_timeserie <- nonESG_timeserie[-c(1:2),]
######

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

### next 2 lines not to run with the new datasets
remove_names_ESG <- colnames(ESG_timeserie[,to_remove_w])
ESG_timeserie <- ESG_timeserie[,-to_remove_w] #remove instruments with a lot of missing values

ww1 <- rowSums(is.na(ESG_timeserie))
#remove if more than 10% of companies are missing values that day
date_to_remove2 <- ESG_timeserie$Instrument[which(ww1 > 21)]

STOXX600_timeserie <- STOXX600_timeserie[,-c(1:2)]


#remove for the previous dates + Remove the date of 2nd January as it seems to be a huge outlier (value of 0)
#date_to_remove <- c(date_to_remove1, date_to_remove2)
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
raw_ESG <- read_excel("C:/Users/Hadrien Venance/Desktop/Thesis/Data/SRI_companies.xlsx")
raw_nonESG <- read_excel("C:/Users/Hadrien Venance/Desktop/Thesis/Data/non_SRI_companies.xlsx")

#remove companies with a lot of missing data
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

industries <- rename(industry_ESG, Leaders= n )

#remove the last row of NA in nonESG
add <- industry_nonESG %>% filter(row_number() <= n()-1)
industries$Laggards = add$n

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
country <- rename(country, Leaders = n.x , Laggards = n.y)

countries <- as.data.frame(lapply(country, as.numeric))
countries$`Country.of.Exchange` <- country$`Country of Exchange`

countries <- countries %>%
  rename(
    `Country of Exchange` = Country.of.Exchange
  )

countries <- rbind(countries, c("Total", colSums(countries[,2:3], na.rm = T)))

View(countries)



length(time_series$ESG)

##  RETURN COMPUTATION ##
#Convert prices to returns (log returns in %)
#returns = as.data.frame(sapply(time_series[,c(2:4)], function(x) diff(log(x)) * 100))  #note it will be one less
# Need to use simple returns for regressions 
returns = as.data.frame(sapply(time_series[,c(2:4)], function(x) diff(x)/x[-1]*100)) 
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

# visualization of returns
# convert to long
ret_long = pivot_longer(return_time_series, cols = -c(Date), values_to = "Return", names_to = "Stock")
# plot
port_p1 = ggplot(ret_long, aes(Date, Return, color = Stock)) + geom_path(stat = "identity") +
  facet_grid(Stock ~ .) + theme_minimal() + labs(x = "Date", y = "Returns")
port_p1  #covid crisis period is evident in the plot




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
## create another column with STOXX600_rf
data_ffex$STOXX600_rf = data_ffex$STOXX600 - data_ffex$RF

ff_lreg = lm(ESG_rf ~ STOXX600_rf + SMB + HML, data = data_ffex)
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
## FIXED effects ##
#install.packages('plm')
library(plm)
# using plm with 'within' estimator for fixed effects
# I controlled for industries & countries --> here I need to do it at the firm-level
# equation is: ESG_rf_return = STOXX600 + SMB + HML + FE_industry + FE_country
# based on the df "ESG-timeserie", I need to recompute returns for each company and then use and merge FF factors which are stored in data_ffex
fe_ESG_timeserie <- as.data.frame(lapply(ESG_timeserie[,-1], as.numeric))    #to remove the date column
#fe_ESG_timeserie$date <- ESG_timeserie$Instrument
fe_returns = as.data.frame(sapply(fe_ESG_timeserie[,c(2:ncol(fe_ESG_timeserie))], function(x) diff(x)/x[-1]*100)) 
# create a different dataframe with returns
fe_return_timeseries = as.data.frame(cbind(Date = ESG_timeserie$Instrument[2:length(ESG_timeserie$Instrument)],
                                         fe_returns), stringsAsFactors = FALSE, row.names = NULL)

# merge df of FF data and the one of ESG_returns of all stocks 
# use merge (can use left_join from dplyr as well) to combine the stock returns and factor data
#transform date format
fe_return_timeseries$Date <- gsub('.{10}$', '', fe_return_timeseries$Date)
fe_return_timeseries$Date <- as.Date(fe_return_timeseries$Date)
data_fe = merge(fe_return_timeseries, ff_data, by = "Date")

# to have RF returns, I will substract RF for all stocks, i.e. columns c(2:228)
data_fe[,c(2:228)] = data_fe[,c(2:228)] - data_fe$RF

############################ merge with characteristics of industry and countries in the CSV "SRI_companies.xlsx; merge based on Identifier (RIC)
raw_ESG
#filter companies that were removed because of lack of data coverage
raw_ESG <- raw_ESG %>% 
  filter(
    !`Identifier (RIC)` %in% remove_names_ESG)
##################" retrieve first country --> probably use a pivot longer with a column "RIC" = name of firm
######### --> or use 
fe_countries <- data_fe %>%
  pivot_longer(cols = , names_to = "date", values_to = "returns")


#then retrieve industry


#some visualizations 
#################### first need to group by country
p1 = ggplot(GDP_l, aes(Year, GDP, group = Country))
p1 + geom_path(aes(color = Country)) + theme_minimal() + theme(legend.position = "top")
p2 = ggplot(GDP_l, aes(Country, GDP))
p2 + geom_boxplot(aes(fill = Country)) + theme_minimal() + theme(legend.position = "top")




#on the "gross" return
fe1 = plm(inv ~ value + capital, data = pdata1, model = "within")
# the output can be summarised with summary
summary(fe1)

#on the risk-free return/ CAPM return
fe1 = plm(inv ~ value + capital, data = pdata1, model = "within")
# the output can be summarised with summary
summary(fe1)

#on the FF 3 factors return
fe1 = plm(inv ~ value + capital, data = pdata1, model = "within")
# the output can be summarised with summary
summary(fe1)


## Testing for OLS, fixed or random effect models 
# Simple OLS (without the intercept) using pooling
ols1 = plm(inv ~ value + capital - 1, data = pdata1, model = "pooling")
# summary of results
summary(ols1)

# Testing for the better model, null: OLS is a better
pFtest(fe1, ols1)

# plmtest using the Breuch-Pagan method, null: OLS is better
plmtest(ols1, type = c("bp"))

# phtest using the fitted models in fe1 and re1
# p-value<0.05 suggests that the fixed effect model is appropriate so in this case the random effect model should be used
phtest(fe1, re1)


## RANDOM effect model ##
re1 = plm(inv ~ value + capital, data = pdata1, model = "random")
# summary
summary(re1)


## VaR forecasts ## https://rforanalytics.com/11.4-var-forecasts-using-out-of-sample.html




######    PORTFOLIO MODELING    ######
## Portfolio with random weights ## 
np1 = 200  #number of portfolios
ret2 = return_time_series[, -1]  #excluding dates
mu1 = colMeans(ret2)  #mean returns
na1 = ncol(ret2)  #number of assets
varc1 = cov(ret2)

riskp1 = NULL  #vector to store risk
retp1 = NULL  #vector to store returns
# using loops here (not aiming for efficiency but demonstration)

for (i in 1:np1) {
  w = diff(c(0, sort(runif(na1 - 1)), 1))  # random weights
  r1 = t(w) %*% mu1  #matrix multiplication
  sd1 = t(w) %*% varc1 %*% w
  retp1 = rbind(retp1, r1)
  riskp1 = rbind(riskp1, sd1)
}

# create a data frame of risk and return
d_p1 = data.frame(Ret = retp1, Risk = riskp1)
# simple plot
plot(d_p1$Risk, d_p1$Ret, xlab = "Risk", ylab = "Return", main = "Frontier Portfolios",
     col = "blue")

p1 = ggplot(d_p1, aes(Risk, Ret, colour = Ret))
# scatter plot
p1 = p1 + geom_point()
# scatter plot with density and identified port risk return (highest
# lowest returns and min risk)
p1 + geom_point() + geom_hline(yintercept = c(max(d_p1$Ret), median(d_p1$Ret),
                                              min(d_p1$Ret)), colour = c("darkgreen", "darkgray", "darkred"), size = 1) +
  geom_vline(xintercept = d_p1[(d_p1$Risk == min(d_p1$Risk)), ][, 2]) +
  labs(colour = "Portfolio Return", x = "Portfolio Risk", y = "Portfolio Return",
       title = "Random Feasible Portfolios") + theme_bw()


## Efficient (Mean-variance) Portfolio (and long only)  ##
library(PortfolioAnalytics)
# initialise with asset names uses time series data
data_p2 = zoo(return_time_series[, -1], order.by = as.Date(return_time_series$Date))
# create specification
port = portfolio.spec(assets = c(colnames(data_p2)))
# add long only constraint
port = add.constraint(portfolio = port, type = "long_only")
# add full investment contraint
port = add.constraint(portfolio = port, type = "full_investment")

# objective: manimise risk
port_rnd = add.objective(portfolio = port, type = "risk", name = "StdDev")

# objective: maximise return
port_rnd = add.objective(portfolio = port_rnd, type = "return", name = "mean")

# 1. optimise random portfolios
rand_p = optimize.portfolio(R = data_p2, portfolio = port_rnd, optimize_method = "random",
                            trace = TRUE, search_size = 1000)
rand_p
# plot
chart.RiskReward(rand_p, risk.col = "StdDev", return.col = "mean", chart.assets = TRUE)  #also plots the equally weighted portfolio
# plot
plot(rand_p, risk.col = "StdDev", main = "Mean Variance Portfolio", chart.assets = TRUE)
# efficient frontier
minvar_ef = create.EfficientFrontier(R = data_p2, portfolio = port_rnd,
                                     type = "mean-StdDev")
chart.EfficientFrontier(minvar_ef, match.col = "StdDev", type = "l", tangent.line = FALSE,
                        chart.assets = TRUE)


## Evaluating Portfolios: Risk Adjusted Performance ##
library(fPortfolio)
# create specification
pspec3 = portfolioSpec()
setRiskFreeRate(pspec3) = mean(data_ffex$RF)
data_p2 = data_p2[,-1]
data_p2 = as.timeSeries(data_p2)
setNFrontierPoints(pspec3) = 50
# create efficient frontier
eff_front4 = portfolioFrontier(data_p2, spec = pspec3, constraints = "LongOnly")

# find the tangency port
tgport1 = tangencyPortfolio(data = data_p2, spec = pspec3, constraints = "LongOnly")

# create frontier plot
frontierPlot(eff_front4, pch = 1, auto = FALSE, xlim = c(0, 2.5), ylim = c(0,
                                                                           0.085))  #custom x and y limits
minvariancePoints(object = eff_front4, auto = FALSE, col = "red", pch = 20)  #add min variance port
tangencyPoints(object = eff_front4, col = "blue", pch = 20)  #add tangency port
tangencyLines(object = tgport1, col = "darkblue")  #add tangency portfolio line
grid()





######    REGRESSIONS WITH DUMMY VARIABLES    ######
# To test the exogenous impact of Covid 19 











