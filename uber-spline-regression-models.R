##### Date: 08/22/2018
##### Analysis Test R Code
##### Team: Economics and Research

### load dependent library packages 
library(jsonlite)
library(tidyverse)
library(lubridate)
library(stargazer)
library(dummies)

### load dataset
date_client_login <- fromJSON("C:\\Users\\Naveed\\Desktop\\Employment\\Uber\\Research & Economics Candidate Challenge\\logins.json",flatten=TRUE)

### change to date format and extract date components
date_client_login <- lubridate::ymd_hms(date_client_login)
month_client_login <- lubridate::month(date_client_login)
week_client_login <- lubridate::week(date_client_login)
wday_client_login <- lubridate::wday(date_client_login,label=TRUE)
hour_client_login <- lubridate::hour(date_client_login)

### combine all date componets into a dataframe
df_client_login <- data.frame(date_client_login,month_client_login,wday_client_login,hour_client_login,week_client_login)
str(df_client_login)

remove(month_client_login)
remove(hour_client_login)
remove(wday_client_login)
remove(week_client_login)

### evaluate distributions of date components
prop.table(table(df_client_login$month_client_login))
prop.table(table(df_client_login$week_client_login))
prop.table(table(df_client_login$wday_client_login))
prop.table(table(df_client_login$hour_client_login))

### Q1) generate a graph showing an hourly breakdown of client login behavior
### the chart is a distribution of count of client logins by hour, week and weekday
df_client_login %>%
  dplyr::group_by(week_client_login,wday_client_login,hour_client_login) %>%
  dplyr::summarize(hs_week_wday = n()) %>%
  ggplot(aes(x=hour_client_login, y=hs_week_wday)) +
  geom_point() +
  geom_smooth(method = lm,formula = y~poly(x,2)) +
  ggtitle("Chart 1: Hourly Breakdown of Client Login Behavior") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Hour of Day (Military Time)") + ylab("Count of Client Logins")

### Q2) I used regression to determine best fit
## the graph in Q1, is grouped by by week and weekday 
df2_client_login <- df_client_login %>%
  dplyr::group_by(week_client_login,wday_client_login,hour_client_login) %>%
  dplyr::summarize(hs_week_wday = n())

# Model 1: Simple Linear Regression (slr)
model_1_slr <- lm(hs_week_wday ~ hour_client_login, data=df2_client_login)
summary(model_1_slr)

# Model 2: Polynomial Regression (pr)
# a polynomial to the 6th power had the best adj R2 (36%)
model_2_pr <- lm(hs_week_wday ~ 
                   hour_client_login
                  +I(hour_client_login^2)
                  ,data=df2_client_login)
summary(model_2_pr)

# Model 3: Piecewise/Spline Regression (spr)
# due to loss of degress of freedom and multicolinearity i choose to model
# a piecewise/spline regression on the data instead
df2_client_login$dummy1 <- ifelse(df2_client_login$hour_client_login <= 8,0,1)
df2_client_login$splineknot_8 <- df2_client_login$dummy1*(df2_client_login$hour_client_login-8)

df2_client_login$dummy2 <- ifelse(df2_client_login$hour_client_login <= 20,0,1)
df2_client_login$splineknot_20 <- df2_client_login$dummy2*(df2_client_login$hour_client_login-20)

model_3_spr <- lm(hs_week_wday ~ 
                   hour_client_login 
                  +splineknot_8
                  +splineknot_20
                  ,data=df2_client_login)

summary(model_3_spr)

# Model 4: Piecewise/Spline Regression w/weekend dummy 
# counts of client logins occurs more frequently during weekends. 
# there could be selection bias. therefore contol for weekend
df_client_login %>%
  ggplot(aes(x=wday_client_login)) +
  geom_bar() +
  ggtitle("Chart 2: Distribution of Client Logins by Day") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Day") + ylab("Total Count of Client Logins")

df2_client_login$weekend <- ifelse(df2_client_login$wday_client_login == 'Sat' 
                                   | df2_client_login$wday_client_login == 'Sun',1,0)

model_4_spr <- lm(hs_week_wday ~ 
                   hour_client_login 
                  +splineknot_8
                  +splineknot_20
                  +weekend
                  ,data=df2_client_login)
summary(model_4_spr)

# Model 5: Piecewise/Spline Regression w/weekend dummy and week FE (spr_fe)
# counts of client logins seemed to be increasing over time.
# sepcifically the spike that are occuring in the end of April
df_client_login %>%
  filter(week_client_login != 9 & week_client_login != 18) %>%
  ggplot(aes(x=week_client_login)) +
  geom_bar() +
  ggtitle("Chart 3: Distribution of Client Logins by Week") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Week") + ylab("Total Count of Client Logins")

df2_client_login <- as.data.frame(df2_client_login)
df2_client_login <- cbind(df2_client_login,dummy(df2_client_login$week_client_login, sep = "_"))

model_5_spr_fe <- lm(hs_week_wday ~ 
                   hour_client_login 
                  +splineknot_8
                  +splineknot_20
                  +weekend
                  +df2_client_login_10
                  +df2_client_login_11
                  +df2_client_login_12
                  +df2_client_login_13
                  +df2_client_login_14
                  +df2_client_login_15
                  +df2_client_login_16
                  +df2_client_login_17
                  +df2_client_login_18
                  ,data=df2_client_login)
summary(model_5_spr_fe)

# summary table of regression model results
stargazer(model_1_slr
          ,model_2_pr
          ,model_3_spr
          ,model_4_spr
          ,model_5_spr_fe
          ,type = "text"
          ,dep.var.labels = ""
          ,title = "Table 1: Regression Model Results"
          ,out = "model2.txt"
          ,digits = 2
          ,column.labels = c("Simple Linear"
                             ,"Polynomial"
                             ,"Piecewise"
                             ,"Piecewise w/Dummy"
                             ,"Piecewise FE")
          ,covariate.labels = c("Login Hour"
                                ,"Login Hour Squared"
                                ,"Spline knot 8AM"
                                ,"Spline knot 8PM"
                                ,"Weekend Dummy"
                                ,"Week 10 FE"
                                ,"Week 11 FE"
                                ,"Week 12 FE"
                                ,"Week 13 FE"
                                ,"Week 14 FE"
                                ,"Week 15 FE"
                                ,"Week 16 FE"
                                ,"Week 17 FE"
                                ,"Week 18 FE"))

### Q3) trends or deviations within the dataset
df_client_login %>%
  ggplot(aes(x=date_client_login)) +
  geom_freqpoly(binwidth=60000) +
  ggtitle("Chart 4: Time Series of Client Logins by Date") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Date") + ylab("Total Count of Client Logins")

### Q4) client logins by week and by hour
### since week 9 and and 18 are have only two and three days, respectively, i removed them.
df3_client_login <- df_client_login %>%
  dplyr::filter(week_client_login != 9 & week_client_login != 18) %>%
  dplyr::group_by(week_client_login,hour_client_login) %>%
  dplyr::summarize(count=n()) %>%
  dplyr::mutate(avg_week_hour = count/7)

df3_client_login$week_hour <- paste("wk",df3_client_login$week_client_login,"hr",df3_client_login$hour_client_login,sep = '_')
df3_client_login$avg_week_hour <- round(df3_client_login$avg_week_hour,digits = 0)

ggplot(df3_client_login, aes(week_hour,avg_week_hour)) +
  geom_col() + 
  scale_x_discrete(breaks=seq(by=8))
