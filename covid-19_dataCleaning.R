library(dplyr)
library(tidyverse)
library(stats)
library(ggplot2)
library(ggpubr)

# import and clean the data
Covid_data = read.csv("/Users/evanjiang/Desktop/spring20_research/Covid-19 (-4.4).txt",sep = ",")
#Covid_data = read.csv("Covid-19 (-4.4).txt",sep = ",")

keep_index_date = !is.na(Covid_data$date)
Covid_data = Covid_data[keep_index_date,]
keep_index_county = !is.na(Covid_data$county)
Covid_data = Covid_data[keep_index_county,]
keep_index_state = !is.na(Covid_data$state)
Covid_data = Covid_data[keep_index_state,]
keep_index_fips = !is.na(Covid_data$fips)
Covid_data = Covid_data[keep_index_fips,]
keep_index_cases = !is.na(Covid_data$cases)
Covid_data = Covid_data[keep_index_cases,]
keep_index_deaths = !is.na(Covid_data$deaths)
Covid_data = Covid_data[keep_index_deaths,]
head(Covid_data)

Covid_by_state = split(Covid_data, Covid_data$state)
daily_counts = lapply(Covid_by_state, function(x) tapply(x$cases, x$date, function(y) sum(y, na.rm = TRUE)))

daily_counts_no_na = daily_counts
for (i in names(daily_counts_no_na)){
  daily_counts_no_na[[i]][is.na(daily_counts_no_na[[i]])] = 0
}
daily_cumulative_counts = lapply(daily_counts_no_na, diff)

#daily_counts (Daily)
NY = daily_cumulative_counts$`New York`
NJ = daily_cumulative_counts$`New Jersey`
Michigan = daily_cumulative_counts$Michigan
California = daily_cumulative_counts$California
Louisiana = daily_cumulative_counts$Louisiana
Florida = daily_cumulative_counts$Florida
Massachusetts = daily_cumulative_counts$Massachusetts
Pennsylvania = daily_cumulative_counts$Pennsylvania
Illinois = daily_cumulative_counts$Illinois
Texas = daily_cumulative_counts$Texas

# New York Daily Cases
NY_Data = as.data.frame(NY)
NY_Data$date = row.names(NY_Data)
NY_Data$group = "New York"

# New Jersey Daily Cases
NJ_Data = as.data.frame(NJ)
NJ_Data$date = row.names(NJ_Data)
NJ_Data$group = "New Jersey"

# Michigan Daily Cases
Michigan_Data = as.data.frame(Michigan)
Michigan_Data$date = row.names(Michigan_Data)
Michigan_Data$group = "Michigan"

# California Daily Cases
California_Data = as.data.frame(California)
California_Data$date = row.names(California_Data)
California_Data$group = "California"

# Louisiana Daily Cases
Louisiana_Data = as.data.frame(Louisiana)
Louisiana_Data$date = row.names(Louisiana_Data)
Louisiana_Data$group = "Louisiana"

# Florida Daily Cases
Florida_Data = as.data.frame(Florida)
Florida_Data$date = row.names(Florida_Data)
Florida_Data$group = "Florida"

# Massachusetts Daily Cases
Massachusetts_Data = as.data.frame(Massachusetts)
Massachusetts_Data$date = row.names(Massachusetts_Data)
Massachusetts_Data$group = "Massachusetts"

# Pennsylvania Daily Cases
Pennsylvania_Data = as.data.frame(Pennsylvania)
Pennsylvania_Data$date = row.names(Pennsylvania_Data)
Pennsylvania_Data$group = "Pennsylvania"

# Illinois Daily Cases
Illinois_Data = as.data.frame(Illinois)
Illinois_Data$date = row.names(Illinois_Data)
Illinois_Data$group = "Illinois"

# Texas Daily Cases
Texas_Data = as.data.frame(Texas)
Texas_Data$date = row.names(Texas_Data)
Texas_Data$group = "Texas"


# cumulative cases by states
daily_counts_no_na = daily_counts

#daily_cumulative_counts
NY_C = daily_counts_no_na$`New York`
NJ_C = daily_counts_no_na$`New Jersey`
Michigan_C = daily_counts_no_na$Michigan
California_C = daily_counts_no_na$California
Louisiana_C = daily_counts_no_na$Louisiana
Florida_C = daily_counts_no_na$Florida
Massachusetts_C = daily_counts_no_na$Massachusetts
Pennsylvania_C = daily_counts_no_na$Pennsylvania
Illinois_C = daily_counts_no_na$Illinois
Texas_C = daily_counts_no_na$Texas

# New York Cumulative Cases
NY_C_Data = as.data.frame(NY_C)
NY_C_Data$date = row.names(NY_C_Data)
NY_C_Data$group = "New York"

# New Jersey Cumulative Cases
NJ_C_Data = as.data.frame(NJ_C)
NJ_C_Data$date = row.names(NJ_C_Data)
NJ_C_Data$group = "New Jersey"

# Michigan Cumulative Cases
Michigan_C_Data = as.data.frame(Michigan_C)
Michigan_C_Data$date = row.names(Michigan_C_Data)
Michigan_C_Data$group = "Michigan"

# California Cumulative Cases
California_C_Data = as.data.frame(California_C)
California_C_Data$date = row.names(California_C_Data)
California_C_Data$group = "California"

# Louisiana Cumulative Cases
Louisiana_C_Data = as.data.frame(Louisiana_C)
Louisiana_C_Data$date = row.names(Louisiana_C_Data)
Louisiana_C_Data$group = "Louisiana"

# Florida Cumulative Cases
Florida_C_Data = as.data.frame(Florida_C)
Florida_C_Data$date = row.names(Florida_C_Data)
Florida_C_Data$group = "Florida"

# Massachusetts Cumulative Cases
Massachusetts_C_Data = as.data.frame(Massachusetts_C)
Massachusetts_C_Data$date = row.names(Massachusetts_C_Data)
Massachusetts_C_Data$group = "Massachusetts"

# Pennsylvania Cumulative Cases
Pennsylvania_C_Data = as.data.frame(Pennsylvania_C)
Pennsylvania_C_Data$date = row.names(Pennsylvania_C_Data)
Pennsylvania_C_Data$group = "Pennsylvania"

# Illinois Cumulative Cases
Illinois_C_Data = as.data.frame(Illinois_C)
Illinois_C_Data$date = row.names(Illinois_C_Data)
Illinois_C_Data$group = "Illinois"

# Texas Cumulative Cases
Texas_C_Data = as.data.frame(Texas_C)
Texas_C_Data$date = row.names(Texas_C_Data)
Texas_C_Data$group = "Texas"


# Find the TOP 10 states with the most cases to date.
total_by_state = sapply(daily_cumulative_counts, sum)
total_by_state_sorted = sort(total_by_state, decreasing = TRUE)
top10_state = total_by_state_sorted[1:10]
top10_state = data.frame(state = names(top10_state), total_cases = top10_state)
row.names(top10_state) = NULL
names(top10_state) = c("state", "total new cases")

# Prepare for the daily counts and cumulative daily counts data for the TOP 10 states.
top10_state_names = as.character(top10_state$state)

# prepare for daily counts
top10_daily_cumulative_counts = daily_cumulative_counts[top10_state_names]
date = rep(names(top10_daily_cumulative_counts[[1]]), 10)
cumulative_cases = as.numeric(do.call("c", top10_daily_cumulative_counts))
state_names = rep(top10_state_names, each = length(names(top10_daily_cumulative_counts[[1]])))
top10_daily_counts_ts = data.frame(date, cases = cumulative_cases, state = state_names)
#top10_daily_counts_ts

# prepare for cumulative counts
top10_daily_counts = daily_counts[top10_state_names]
date = rep(names(top10_daily_counts[[1]]), 10)
cases = as.numeric(do.call("c", top10_daily_counts))
state_names = rep(top10_state_names, each = length(names(top10_daily_counts[[1]])))
top10_daily_cumulative_counts_ts = data.frame(date, cases, state = state_names)
#top10_daily_cumulative_counts_ts



## 没懂这后面的代码在干什么
total_by_state = sapply(daily_cumulative_counts, sum)
total_by_state_sorted = sort(total_by_state, decreasing = TRUE)
#top10_state = total_by_state_sorted[1:10]
total_by_state_sorted = data.frame(state = names(total_by_state_sorted), total_cases = 
                                     total_by_state_sorted)
row.names(total_by_state_sorted) = NULL
names(total_by_state_sorted) = c("state", "total new cases")
knitr::kable(total_by_state_sorted)
all_state_names = as.character(total_by_state_sorted$state)

all_cumulative_counts = daily_counts[all_state_names]
#date = rep(names(all_cumulative_counts[[1]]), 10)
date = rep(names(all_cumulative_counts[[1]]), 55)
cases = as.numeric(do.call("c", all_cumulative_counts))
state_names = rep(all_state_names, each = length(names(all_cumulative_counts[[1]])))
all_cumulative_counts_ts = data.frame(date, cases, state = state_names)
all_cumulative_counts_ts = all_cumulative_counts_ts %>% 
  filter(!is.na(all_cumulative_counts_ts$cases))
all_cumulative_counts_ts = all_cumulative_counts_ts %>% 
  mutate(day = c(1:nrow(all_cumulative_counts_ts)))

knots_all_cumulative_counts_ts = 
  quantile(all_cumulative_counts_ts$day, p = c(0.5, 0.75))

m_all_cumulative_counts_ts = lm(cases ~ splines::bs(day, knots = knots_all_cumulative_counts_ts), 
                                data = all_cumulative_counts_ts)
plot(all_cumulative_counts_ts$day, predict(m_all_cumulative_counts_ts))
ggplot(all_cumulative_counts_ts, aes(x = day, y = cases, colour = state, group = state)) + 
  geom_point() + 
  geom_line(lines(all_cumulative_counts_ts$day, 
                  predict(m_all_cumulative_counts_ts)), color = "red")

