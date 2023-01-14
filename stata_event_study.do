clear
* Data cleaning and computing the event and estimation windows *
import excel "C:/Users/Hadrien Venance/Desktop/Thesis/Data/stata.xlsx", sheet("Sheet1") firstrow
*Group each firm with one id*
egen id = group(Firms)
describe id
sort id Country ESG_Combined_Score date
*Generate the estimation window*
by id: gen datenum = _n
* To only take companies with right event date in specified country *
keep if Country == "Spain"
by id: gen target = datenum if date=="2020-03-13"
egen td = min(target), by(id)
drop target
gen dif = datenum - td
* Let's start w/ a 5 days event window *
by id: gen event_window = 1 if dif >= 0  & dif <= 5
* Estimation window for regression analysis *
* Let's first use time window of -50 to -20 to get more data on covid cases and avoid BIG CRASH on 20/02 *
by id: gen estimation_window = 1 if dif <= -20 & dif >= -50
egen count_event_obs = count(event_window), by(id)
egen count_est_obs = count(estimation_window), by(id)
replace event_window = 0 if event_window==.
replace estimation_window = 0 if estimation_window==.
tab id if count_event_obs < 5
tab id if count_est_obs < 30
*Declare dataset to be Panel Data according to the id of each firm*
gen date_stata = daily(date, "YMD")
format date_stata %td
xtset id date_stata
* Specification process for the regression analysis *
*1st regression* /* OR MAYBE A BOUCLE FOR POUR TESTER LES REG MAIS BOF*
*Lagrange Multiplier test*
*CAPM instead of market model ??
*reg Firms_Return Market_Return_per_Country, hascons Rf
reg Firms_Return Market_Return_per_Country
estimates store POOLED
xtreg Firms_Return Market_Return_per_Country, fe
estimates store FE
* Breusch-Pagan Lagrange multiplier test for heteroskedasticity
reg Firms_Return Market_Return_per_Country
xtreg Firms_Return Market_Return_per_Country
xttest0
* Hausman test for endogeneity
hausman POOL FE
*we cannot reject the null hypothesis of the OLS model, therefore we use cluster to adjust standard errors - avoid correlated errors and biased standard errors*
reg Firms_Return Market_Return_per_Country, cluster(id)
*Event study regression*
*foreach var of varlist JDG.L, "MNDI.L
gen predicted_return = . 
*egen id = group(company_id)
levelsof Firms, local(lev)
foreach i of local lev {
        qui reg Firms_Return Market_Return_per_Country if estimation_window==1 & Firms == "`i'", cluster(id)
        predict p if Firms=="`i'"
        replace predicted_return=p if Firms=="`i'" & event_window==1
        drop p
        }
*Generate and compute abnormal values*
gen abnormal_return=Firms_Return-predicted_return if event_window==1 
by id: egen cumulative_abnormal_return=total(abnormal_return)
*Calculate the mean values for each indicator according to bank types*
set level 90
mean Firms_Return if event_window==1 , over(Leaders)
mean predicted_return, over(Leaders)
mean abnormal_return, over(Leaders)
* compare means of 2 groups 
 ttest abnormal_return, by(Leaders) level(90)
*Calculate the median values for each indicator according to bank types*
*signrank abnormal_return = 0 if Leaders ==1
*sum  abnormal_return if  Leaders ==1, detail
*signrank abnormal_return = 0 if Leaders ==0
*sum  abnormal_return if  Leaders ==0, detail
*Now I have to do it again but for the time window [-0,0] and [0,5]

*Graphics: daily mean evolution for actual returns & abnormal_returns*
*First, need to change time_window to [-5,5]
gen mean_Leaders=.
gen mean_Laggards=.
forvalues i=-5(1)5 {
sum Firms_Return if Leaders==1 & dif==`i'
return list
replace mean_Leaders=r(mean) if Leaders==1 & dif==`i'
}

forvalues i=-5(1)5 {
sum Firms_Return if Leaders==0 & dif==`i'
return list
replace mean_Laggards=r(mean) if Leaders==0 & dif==`i'
}
twoway (line mean_Leaders dif if Leaders == 1 & event_window==1, sort)(line mean_Laggards dif if Leaders == 0 & event_window==1 , sort) , xtitle(dif) title(Actual returns) legend(label(1 "ESG Leaders")) legend(label(2 "ESG Laggards"))
*Second plot for abnormal_returns*
gen mean_Leaders1=.
gen mean_Laggards1=.
forvalues i=-5(1)5 {
sum abnormal_return if Leaders==1 & dif==`i'
return list
replace mean_Leaders1=r(mean) if Leaders==1 & dif==`i'
}

forvalues i=-5(1)5 {
sum abnormal_return if Leaders==0 & dif==`i'
return list
replace mean_Laggards1=r(mean) if Leaders==0 & dif==`i'
}
twoway (line mean_Leaders1 dif if Leaders == 1 & event_window==1, sort)(line mean_Laggards1 dif if Leaders == 0 & event_window==1 , sort) , xtitle(dif) title(Abnormal returns) legend(label(1 "ESG Leaders")) legend(label(2 "ESG Laggards"))


*********						*********	


*2nd regression* 
clear
* Data cleaning and computing the event and estimation windows *
import excel "C:/Users/Hadrien Venance/Desktop/Thesis/Data/stata.xlsx", sheet("Sheet1") firstrow
*Group each firm with one id*
egen id = group(Firms)
describe id
sort id Country ESG_Combined_Score date
*Generate the estimation window*
by id: gen datenum = _n
* To only take companies with right event date in specified country *
keep if Country == "United Kingdom"
by id: gen target = datenum if date=="2020-03-23"
egen td = min(target), by(id)
drop target
gen dif = datenum - td
* Let's start w/ a 5 days event window *
by id: gen event_window = 1 if dif >= 0 & dif <= 5
* Estimation window for regression analysis *
* Let's first use time window of -50 to -20 to get more data on covid cases and avoid BIG CRASH on 20/02 *
by id: gen estimation_window = 1 if dif <= -20 & dif >= -50
egen count_event_obs = count(event_window), by(id)
egen count_est_obs = count(estimation_window), by(id)
replace event_window = 0 if event_window==.
replace estimation_window = 0 if estimation_window==.
tab id if count_event_obs < 5
tab id if count_est_obs < 30
*Declare dataset to be Panel Data according to the id of each firm*
gen date_stata = daily(date, "YMD")
format date_stata %td
xtset id date_stata
*Lagrange Multiplier test*
* New_Cases and Total_Cases gives crazy results* 
* Create dummay variable for "after_lockdown" and hence create the interaction term "after_lockdown*ESG_Combined_Score"
by id: gen after_lockdown = 1 if dif >=1 & dif <= 5
replace after_lockdown = 0 if after_lockdown==. 
reg Firms_Return ESG_Combined_Score c.ESG_Combined_Score#after_lockdown Debt_to_Equity_2019 Return_2019 Market_Return_per_Country HML SMB
estimates store POOLED
xtreg Firms_Return ESG_Combined_Score c.ESG_Combined_Score#after_lockdown Debt_to_Equity_2019 Return_2019 Market_Return_per_Country HML SMB, fe 
estimates store FE
* Breusch-Pagan Lagrange multiplier test for heteroskedasticity
reg Firms_Return ESG_Combined_Score c.ESG_Combined_Score#after_lockdown Debt_to_Equity_2019 Return_2019 Market_Return_per_Country HML SMB 
xtreg Firms_Return ESG_Combined_Score c.ESG_Combined_Score#after_lockdown Debt_to_Equity_2019 Return_2019 Market_Return_per_Country HML SMB 
xttest0
* ??? null hypothesis of the OLS model*
* Hausman test for endogeneity
hausman POOLED FE
* ???? we cannot reject the null hypothesis of the OLS model, therefore we use cluster to adjust standard errors - avoid correlated errors and biased standard errors*
*gen ln_mkt = log10(Market_Cap)
reg Firms_Return ESG_Combined_Score c.ESG_Combined_Score#after_lockdown Debt_to_Equity_2019 Return_2019 Market_Return_per_Country HML SMB
*Event study regression*
gen predicted_return = . 
*egen id = group(company_id)
levelsof Firms, local(lev)
foreach i of local lev {
        qui reg Firms_Return ESG_Combined_Score c.ESG_Combined_Score#after_lockdown Debt_to_Equity_2019 Return_2019 Market_Return_per_Country HML SMB if estimation_window==1 & Firms =="`i'"
        predict p if Firms=="`i'"
        replace predicted_return=p if Firms=="`i'" & event_window==1
        drop p
        }
*Generate and compute abnormal values*
gen abnormal_return=Firms_Return-predicted_return if event_window==1 
by id: egen cumulative_abnormal_return=total(abnormal_return)
*Calculate the mean values for each indicator according to bank types*
set level 90
mean Firms_Return if event_window==1 , over(Leaders)
mean predicted_return, over(Leaders)
mean abnormal_return, over(Leaders)
* compare means of 2 groups 
 ttest abnormal_return, by(Leaders) level(90)
*Now I have to do it again but for the time window [-0,0] and [0,5]

*Graphics: daily mean evolution for actual returns & abnormal_returns*
*First, need to change time_window to [-5,5]
gen mean_Leaders=.
gen mean_Laggards=.
forvalues i=-5(1)5 {
sum Firms_Return if Leaders==1 & dif==`i'
return list
replace mean_Leaders=r(mean) if Leaders==1 & dif==`i'
}

forvalues i=-5(1)5 {
sum Firms_Return if Leaders==0 & dif==`i'
return list
replace mean_Laggards=r(mean) if Leaders==0 & dif==`i'
}
twoway (line mean_Leaders dif if Leaders == 1 & event_window==1, sort)(line mean_Laggards dif if Leaders == 0 & event_window==1 , sort) , xtitle(dif) title(Actual returns) legend(label(1 "ESG Leaders")) legend(label(2 "ESG Laggards"))
*Second plot for abnormal_returns*
gen mean_Leaders1=.
gen mean_Laggards1=.
forvalues i=-5(1)5 {
sum abnormal_return if Leaders==1 & dif==`i'
return list
replace mean_Leaders1=r(mean) if Leaders==1 & dif==`i'
}

forvalues i=-5(1)5 {
sum abnormal_return if Leaders==0 & dif==`i'
return list
replace mean_Laggards1=r(mean) if Leaders==0 & dif==`i'
}
twoway (line mean_Leaders1 dif if Leaders == 1 & event_window==1, sort)(line mean_Laggards1 dif if Leaders == 0 & event_window==1 , sort) , xtitle(dif) title(Abnormal returns) legend(label(1 "ESG Leaders")) legend(label(2 "ESG Laggards"))



*********						*********	








*T-test for cumulative abnormal return*
sort id date
by id: egen ar_sd = sd(abnormal_return)
gen number_days_eventwindow = 5
gen test = (1/sqrt(number_days_eventwindow))*(cumulative_abnormal_return/ar_sd)
list company_id cumulative_abnormal_return
test if dif==0 


*2nd regression* --> more like a robustness check
*Lagrange Multiplier test*
reg Firms_Return Market_Return HML SMB
xtreg Firms_Return Market_Return HML SMB
xttest0


*3rd regression* 
*Lagrange Multiplier test*
reg Firms_Return Market_Return HML SMB ESG_Combined_Score
xtreg Firms_Return Market_Return HML SMB ESG_Combined_Score
xttest0



*Hausman test*
estimates store FE
xtreg Firms_Return Market_Return, re
estimates store RE
hausman FE RE
drop _est_FE _est_RE
*do not reject random effect model*
*5th regression*
*Lagrange Multiplier test*
reg Firms_Return ESG_Combined_Score New_Cases Return_2019 Volatility_2019 ROA_2019 Rev_Growth_2019 Market_Cap PricetoEarnings Age
xtreg Firms_Return ESG_Combined_Score New_Cases Return_2019 Volatility_2019 ROA_2019 Rev_Growth_2019 Market_Cap PricetoEarnings Age
xttest0
* cannot reject H0 of no OLS*


.....
*Hausman test*



*Graphics: daily mean evolution for actual returns & abnormal_returns*
*First, need to change time_window to [-5,5]
gen mean_Leaders=.
gen mean_Laggards=.
forvalues i=-5(1)0 {
sum Firms_Return if Leaders==1 & dif==`i'
return list
replace mean_Leaders=r(mean) if Leaders==1 & dif==`i'
}

forvalues i=-5(1)0 {
sum Firms_Return if Leaders==0 & dif==`i'
return list
replace mean_Laggards=r(mean) if Leaders==0 & dif==`i'
}
twoway (line mean_Leaders dif if Leaders == 1& event_window==1, sort)(line mean_Laggards dif if Leaders == 0 & event_window==1 , sort) , xtitle(dif) title(Actual returns) legend(off)

*replace each `var' with the dependent variables manually to extract each graph*
twoway (line mean_`var' Year if SB_dum == 1, sort)(line mean_CB_`var' /// 
Year if SB_dum == 0, sort) , xtitle(Year) title(`var') legend(off)
twoway (line Firms_Return dif if Leaders == 1 & event_window==1 , sort)(line Firms_Return dif if Leaders == 0 & event_window==1, sort) , xtitle(date) title(`Actual returns') legend(off)
twoway (line abnormal_return date if Leaders == 1 & event_window==1 , sort)(line abnormal_return date if Leaders == 0 & event_window==1, sort) , xtitle(date) title(`Actual returns') legend(off)



*Underlying regressions analysis (for preliminary/explanation part)*


* For loop for predicted values *
capture program drop one_regression
program define one_regression
    reg ret Firms_Return Market_Return if estimation_window==1 
    predict predicted_return if event_window
    exit
end
runby one_regression, by(Firms) status