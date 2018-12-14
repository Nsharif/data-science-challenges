##### Author: Naveed Sharif
##### Data Source: Instacart
##### Topic: Data Science Challenge
##### Date: 10/12/2018
##### Note, I removed spaces directly in the csv file prior to 
##### loading into the R enviornment

### Install and load dependent packages
library(tidyverse)
library(lubridate)
library(dummies)
library(gridExtra)
library(stargazer)
library(plotly)


### set working directory
setwd("C:/Users/Naveed/Desktop/Employment/Employment Applications/Instacart")

### functions
trim <- function (x) gsub("^\\s+|\\s+$", "", x) # funciton to remove white spaces

### load dataset into enviornment
df_main <- read.csv("instacart_raw_data.csv", stringsAsFactors = TRUE)

### examine dataset and fix date data type
glimpse(df_main)
summary(df_main)

### remove white spaces in region. region factor has a duplicate (i.e. sf)
df_main$region <- as.factor(trim(df_main$region))

### change to date format and extract date components
df_main$order_delivery_time <- ymd_hms(df_main$order_delivery_time) # date format
df_main$order_delivery_time_month <- month(df_main$order_delivery_time) # month
df_main$order_delivery_time_wday <- wday(df_main$order_delivery_time, label = FALSE) # the day of the week
df_main$order_delivery_time_hour <- hour(df_main$order_delivery_time) # hour
df_main$order_delivery_time_minute <- minute(df_main$order_delivery_time) # minute
df_main$order_delivery_time_mday <- mday(df_main$order_delivery_time) # day of the month

glimpse(df_main)

# there are duplicate order id's.
duplicate_order_id <- df_main[duplicated(df_main$order_id),]

# after inspecting a few duplicate order id's, the duplicate 
# is due to different order_delivery_time
filter(df_main, order_id == '233598578') # example of duplicate order_id

# calaculate the average difference in order time between duplicate order id's
df_main <- df_main %>%
  group_by(order_id) %>%
  arrange(order_delivery_time) %>%
  mutate(diff_order_delivery_time = as.integer((order_delivery_time) - (lag(order_delivery_time))))

### chart distribution of count of orders by minute, hour, day of week, and day of month
df_main %>%
  group_by(region,order_delivery_time_hour) %>%
  summarize(n_hs = n()) %>%
  ggplot(aes(x = order_delivery_time_hour, y = n_hs, group=region, color=region)) +
  geom_point() +
  geom_line() +
  ggtitle("Chart 1: Customer Orders by Hour and Region") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Hour of Day") + ylab("Count of Orders") 

grid.arrange(
  df_main %>%
    group_by(order_delivery_time_minute) %>%
    filter(region == 'chi') %>%
    summarize(n_min = n()) %>%
    ggplot(aes(x = order_delivery_time_minute, y = n_min)) +
    geom_line(color = '#FF6666') +
    scale_y_continuous(limits = c(60, 160)) +
    geom_smooth(color = 'grey') +
    ggtitle("Chicago") +
    theme(plot.title = element_text(hjust = 0.5)) +
    xlab("Minute in Hour") + ylab("Count of Orders"), 
  df_main %>%
    group_by(region,order_delivery_time_minute) %>%
    filter(region == 'sf') %>%
    summarize(n_min = n()) %>%
    ggplot(aes(x = order_delivery_time_minute, y = n_min)) +
    geom_line(color = '#619CFF') +
    scale_y_continuous(limits = c(60, 160)) +
    geom_smooth(color = 'darkgrey') +
    ggtitle("San Francisco") +
    theme(plot.title = element_text(hjust = 0.5)) +
    xlab("Minute in Hour") + ylab("Count of Orders"),
  nrow = 1,
  top = "Chart 2: Customer Orders by Minute in Hour and by Region")

df_main %>%
  group_by(region,order_delivery_time_minute) %>%
  filter(region == 'nyc') %>%
  summarize(n_min = n()) %>%
  ggplot(aes(x = order_delivery_time_minute, y = n_min)) +
  geom_line(color = '#00BA38') +
  geom_smooth(color = 'darkgrey') +
  ggtitle("New York City") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Minute in Hour") + ylab("Count of Orders")


grid.arrange(
  df_main %>%
    group_by(region,order_delivery_time_wday) %>%
    filter(region == 'chi') %>%
    ggplot(aes(x = order_delivery_time_wday)) +
    geom_bar(fill = "#FF6666") +
    scale_y_continuous(limits = c(0, 1300)) +
    scale_x_continuous(breaks = unique(df_main$order_delivery_time_wday)) +
    ggtitle("Chicago") +
    theme(plot.title = element_text(hjust = 0.5)) +
    xlab("Day of Week") + ylab("Count of Orders"),
  df_main %>%
    group_by(region,order_delivery_time_wday) %>%
    filter(region == 'nyc') %>%
    ggplot(aes(x = order_delivery_time_wday)) +
    geom_bar(fill = "#00BA38") +
    scale_y_continuous(limits = c(0, 1300)) +
    scale_x_continuous(breaks = unique(df_main$order_delivery_time_wday)) +
    ggtitle("New York City") +
    theme(plot.title = element_text(hjust = 0.5)) +
    xlab("Day of Week") + ylab("Count of Orders"),
  df_main %>%
    group_by(region,order_delivery_time_wday) %>%
    filter(region == 'sf') %>%
    ggplot(aes(x = order_delivery_time_wday)) +
    geom_bar(fill = "#619CFF") +
    scale_y_continuous(limits = c(0, 1300)) +
    scale_x_continuous(breaks = unique(df_main$order_delivery_time_wday)) +
    ggtitle("San Francisco") +
    theme(plot.title = element_text(hjust = 0.5)) +
    xlab("Day of Week") + ylab("Count of Orders"),
  nrow = 1,
  top = "Chart 3: Customer Orders by Day of Week and Region")

grid.arrange(
  df_main %>%
    group_by(region,order_delivery_time_mday) %>%
    filter(order_delivery_time_month == 5 & region == 'chi') %>%
    summarize(m_day = n()) %>%
    ggplot(aes(x = order_delivery_time_mday, y = m_day)) +
    geom_line(color = '#FF6666') +
    geom_smooth(method = "lm", color = "red", se = FALSE) +
    scale_y_continuous(limits = c(0, 350)) +
    ggtitle("Chicago") +
    theme(plot.title = element_text(hjust = 0.5)) +
    xlab("Day of Month") + ylab("Count of Orders"),
  df_main %>%
    group_by(region,order_delivery_time_mday) %>%
    filter(order_delivery_time_month == 5 & region == 'nyc') %>%
    summarize(m_day = n()) %>%
    ggplot(aes(x = order_delivery_time_mday, y = m_day)) +
    geom_line(color = '#00BA38') +
    geom_smooth(method = "lm", color = "green", se = FALSE) +
    scale_y_continuous(limits = c(0, 350)) +
    ggtitle("New York City") +
    theme(plot.title = element_text(hjust = 0.5)) +
    xlab("Day of Month") + ylab("Count of Orders"),
  df_main %>%
    group_by(region,order_delivery_time_mday) %>%
    filter(order_delivery_time_month == 5 & region == 'sf') %>%
    summarize(m_day = n()) %>%
    ggplot(aes(x = order_delivery_time_mday, y = m_day)) +
    geom_line(color = '#619CFF') +
    geom_smooth(method = "lm", se = FALSE) +
    scale_y_continuous(limits = c(0, 350)) +
    ggtitle("San Francisco") +
    theme(plot.title = element_text(hjust = 0.5)) +
    xlab("Day of Month") + ylab("Count of Orders"),
  nrow = 1,
  top = "Chart 4: Customer Orders by Day of Month and Region")

### analyzing repeating orders (by order id)
sum(!is.na(df_main$diff_order_delivery_time)) # 7.6% of orders are repeated (1,112/14,597)

grid.arrange(
  df_main %>%  
    group_by(order_delivery_time_wday) %>% 
    filter(region == 'chi' & diff_order_delivery_time <= quantile(diff_order_delivery_time, 0.90, na.rm = TRUE)) %>%
    summarize(mean_diff_time = mean(diff_order_delivery_time, na.rm = TRUE)) %>%
    ggplot(aes(x = order_delivery_time_wday, y = mean_diff_time)) +
    geom_line(color = '#FF6666') +
    geom_smooth(method = "lm", color = "red", se = FALSE) +
    scale_y_continuous(limits = c(2, 20)) +
    ggtitle("Chicago") +
    theme(plot.title = element_text(hjust = 0.5)) +
    xlab("Day of Week") + ylab("Average Difference in Time (minutes)"),
  df_main %>%  
    group_by(order_delivery_time_wday) %>% 
    filter(region == 'nyc' & diff_order_delivery_time <= quantile(diff_order_delivery_time, 0.90, na.rm = TRUE)) %>%
    summarize(mean_diff_time = mean(diff_order_delivery_time, na.rm = TRUE)) %>%
    ggplot(aes(x = order_delivery_time_wday, y = mean_diff_time)) +
    geom_line(color = '#00BA38') +
    geom_smooth(method = "lm", color = "green", se = FALSE) +
    scale_y_continuous(limits = c(2, 20)) +
    ggtitle("New York City") +
    theme(plot.title = element_text(hjust = 0.5)) +
    xlab("Day of Week") + ylab("Average Difference in Time (minutes)"),
  df_main %>%  
    group_by(order_delivery_time_wday) %>% 
    filter(region == 'sf' & diff_order_delivery_time <= quantile(diff_order_delivery_time, 0.90, na.rm = TRUE)) %>%
    summarize(mean_diff_time = mean(diff_order_delivery_time, na.rm = TRUE)) %>%
    ggplot(aes(x = order_delivery_time_wday, y = mean_diff_time)) +
    geom_line(color = '#619CFF') +
    geom_smooth(method = "lm", color = "blue", se = FALSE) +
    scale_y_continuous(limits = c(2, 20)) +
    ggtitle("San Francisco") +
    theme(plot.title = element_text(hjust = 0.5)) +
    xlab("Day of Week") + ylab("Average Difference in Time (minutes)"),
  nrow = 1,
  top = "Chart 5: Average Differnce in Time in Repeated Orders by Day of Week and Region")

grid.arrange(
  df_main %>%  
    group_by(order_delivery_time_hour) %>% 
    filter(region == 'chi' & diff_order_delivery_time <= quantile(diff_order_delivery_time, 0.90, na.rm = TRUE)) %>%
    summarize(mean_diff_time = mean(diff_order_delivery_time, na.rm = TRUE)) %>%
    ggplot(aes(x = order_delivery_time_hour, y = mean_diff_time)) +
    geom_line(color = '#FF6666') +
    scale_y_continuous(limits = c(0, 20)) +
    ggtitle("Chicago") +
    theme(plot.title = element_text(hjust = 0.5)) +
    xlab("Hour of Day") + ylab("Average Difference in Time (minutes)"),
  df_main %>%  
    group_by(order_delivery_time_hour) %>% 
    filter(region == 'nyc' & diff_order_delivery_time <= quantile(diff_order_delivery_time, 0.90, na.rm = TRUE)) %>%
    summarize(mean_diff_time = mean(diff_order_delivery_time, na.rm = TRUE)) %>%
    ggplot(aes(x = order_delivery_time_hour, y = mean_diff_time)) +
    geom_line(color = '#00BA38') +
    scale_y_continuous(limits = c(0, 20)) +
    ggtitle("New York City") +
    theme(plot.title = element_text(hjust = 0.5)) +
    xlab("Hour of Day") + ylab("Average Difference in Time (minutes)"),
  df_main %>%  
    group_by(order_delivery_time_hour) %>% 
    filter(region == 'sf' & diff_order_delivery_time <= quantile(diff_order_delivery_time, 0.90, na.rm = TRUE)) %>%
    summarize(mean_diff_time = mean(diff_order_delivery_time, na.rm = TRUE)) %>%
    ggplot(aes(x = order_delivery_time_hour, y = mean_diff_time)) +
    geom_line(color = '#619CFF') +
    scale_y_continuous(limits = c(0, 20)) +
    ggtitle("San Francisco") +
    theme(plot.title = element_text(hjust = 0.5)) +
    xlab("Hour of Day") + ylab("Average Difference in Time (minutes)"),
  nrow = 1,
  top = "Appendix: Average Differnce in Time in Repeated Orders by Hour of Day and Region")

### evaluating customer_order_rating and type_of_issue_reported
df_main %>% 
  group_by(region,customer_order_rating) %>%
  summarize(n_hs = n()) %>%
  mutate(percentage=round(n_hs/sum(n_hs),2)) %>%
  ggplot(aes(x = customer_order_rating, y = percentage, fill = region)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_x_continuous(breaks = unique(df_main$customer_order_rating)) +
  ggtitle("Distribution of Customer Order Ratings by Region") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Order Rating") + ylab("Percentage of Orders")

df_main %>% 
  group_by(region,customer_order_rating) %>%
  summarize(n_hs = n()) %>%
  filter(customer_order_rating == 0 | 
           customer_order_rating == 1 |
           customer_order_rating == 2) %>%
  mutate(percentage=round(n_hs/sum(n_hs),2)) %>%
  ggplot(aes(x = customer_order_rating, y = percentage, fill = region)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_x_continuous(breaks = unique(df_main$customer_order_rating)) +
  ggtitle("Distribution of Customer Order Ratings (0 to 2) by Region") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Order Rating") + ylab("Percentage of Orders")

# Distribution of Types of Issues Reported by Region
df_main %>% # New York City
  filter((customer_order_rating == 0 | 
           customer_order_rating == 1 |
           customer_order_rating == 2) &
           (region == 'nyc') & 
           type_of_issue_reported != '') %>%
  group_by(type_of_issue_reported) %>%
  summarize(count = n()) %>% 
  plot_ly(labels = ~type_of_issue_reported, values = ~count
        , type = 'pie'
        , textposition = 'inside'
        , textinfo = 'label+percent'
        , insidetextfont = list(color = '#FFFFFF')
        , marker = list(colors = colors
        , line = list(color = '#FFFFFF', width = 1))
        , showlegend = FALSE) %>%
  layout(title = 'New York City',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

df_main %>% # Chicago
  filter((customer_order_rating == 0 | 
            customer_order_rating == 1 |
            customer_order_rating == 2) &
           (region == 'chi') & 
           type_of_issue_reported != '') %>%
  group_by(type_of_issue_reported) %>%
  summarize(count = n()) %>% 
  plot_ly(labels = ~type_of_issue_reported, values = ~count
          , type = 'pie'
          , textposition = 'inside'
          , textinfo = 'label+percent'
          , insidetextfont = list(color = '#FFFFFF')
          , marker = list(colors = colors
                          , line = list(color = '#FFFFFF', width = 1))
          , showlegend = FALSE) %>%
  layout(title = 'Chicago',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

df_main %>% # San Francisco
  filter((customer_order_rating == 0 | 
            customer_order_rating == 1 |
            customer_order_rating == 2) &
           (region == 'sf') & 
           type_of_issue_reported != '') %>%
  group_by(type_of_issue_reported) %>%
  summarize(count = n()) %>% 
  plot_ly(labels = ~type_of_issue_reported, values = ~count
          , type = 'pie'
          , textposition = 'inside'
          , textinfo = 'label+percent'
          , insidetextfont = list(color = '#FFFFFF')
          , marker = list(colors = colors
                          , line = list(color = '#FFFFFF', width = 1))
          , showlegend = FALSE) %>%
  layout(title = 'San Francisco',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

### Regression models to estimate the changes in count of orders
### Models will be based on customer orders by hour of day
### I will be creating sepearte models for each region
### The models I will be specifying are Piecewise (Spline) Models
glimpse(df_main)

df_main %>% # I will be modeling this chart
  group_by(region,order_delivery_time_wday,order_delivery_time_hour) %>%
  summarize(n_hs = n()) %>%
  ggplot(aes(x = order_delivery_time_hour, y = n_hs, group=region, color=region)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Customer Orders by Day, Hour, and Region") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Hour of Day") + ylab("Count of Orders by Day") 

# create dummy variables for day of week
df_main_model <- df_main %>%
  group_by(region,order_delivery_time_wday,order_delivery_time_hour) %>%
  summarize(n_hs = n())

df_main_model <- as.data.frame(df_main_model)
df_main_model <- cbind(df_main_model,dummy(df_main_model$order_delivery_time_wday, sep = "_"))

# create the knots/splines 
df_main_model$dummy2 <- ifelse(df_main_model$order_delivery_time_hour <= 12,0,1)
df_main_model$splineknot_12 <- df_main_model$dummy2*(df_main_model$order_delivery_time_hour-12)

# create seperate datasets by region
df_main_model_san_francisco <- df_main_model %>%
  filter(region == 'sf')

df_main_model_new_york <- df_main_model %>%
  filter(region == 'nyc')

df_main_model_chicago <- df_main_model %>%
  filter(region == 'chi')

# regression piecewise (spline). compared against a ploynomial to the 4th order
# all i needed was one/two spline knots vs a polynomial to the 4th order
# its also easier to interpret than polynomial to the 4th power
# and I am loosing less degrees of freedom
model_sf_unrestricted <- lm(n_hs ~ 
                                order_delivery_time_hour
                              + splineknot_12
                              + df_main_model_1
                              + df_main_model_2
                              + df_main_model_3
                              + df_main_model_4
                              + df_main_model_5
                              + df_main_model_6
                              , data=df_main_model_san_francisco)
summary(model_sf_unrestricted)

model_sf_restricted <- lm(n_hs ~ 
                            order_delivery_time_hour
                          + splineknot_12
                          + df_main_model_1
                          + df_main_model_2
                          , data=df_main_model_san_francisco)
summary(model_sf_restricted)


model_nyc_unrestricted <- lm(n_hs ~ 
                               order_delivery_time_hour
                             + splineknot_12
                             + df_main_model_1
                             + df_main_model_2
                             + df_main_model_3
                             + df_main_model_4
                             + df_main_model_5
                             + df_main_model_6
                             , data=df_main_model_new_york)
summary(model_nyc_unrestricted)

model_nyc_restricted <- lm(n_hs ~ 
                              order_delivery_time_hour
                           + splineknot_12
                            , data=df_main_model_new_york)
summary(model_nyc_restricted)

model_chi_unrestricted <- lm(n_hs ~ 
                               order_delivery_time_hour
                             + splineknot_12
                             + df_main_model_1
                             + df_main_model_2
                             + df_main_model_3
                             + df_main_model_4
                             + df_main_model_5
                             + df_main_model_6
                             , data=df_main_model_chicago)
summary(model_chi_unrestricted)

model_chi_restricted <- lm(n_hs ~ 
                               order_delivery_time_hour
                             + splineknot_12
                             + df_main_model_1
                             + df_main_model_4
                             + df_main_model_5
                             , data=df_main_model_chicago)
summary(model_chi_restricted)

# summary table of regression model results
stargazer(model_sf_restricted
          ,model_nyc_restricted
          ,model_chi_restricted
          ,type = "text"
          ,dep.var.labels = ""
          ,title = "Table 1: Regression Model Results"
          ,out = "model1.txt"
          ,digits = 2
          ,column.labels = c("San Francisco"
                             ,"New York City"
                             ,"Chicago")
          ,covariate.labels = c("Order Deliver Time (hour)"
                                ,"Spline Knot 12"
                                ,"Day 1"
                                ,"Day 2"
                                ,"Day 4"
                                ,"Day 5"))

