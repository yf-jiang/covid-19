# plots of daily cases of top10 states
ny = ggplot(NY_Data, aes(x = date, y = NY, group = group))+
  geom_line() + geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "NY Daily Cases")

nj = ggplot(NJ_Data, aes(x = date, y = NJ, group = group))+
  geom_line() + geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "New Jersey Daily Cases")

m = ggplot(Michigan_Data, aes(x = date, y = Michigan, group = group))+
  geom_line() + geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Michigan Daily Cases")

c = ggplot(California_Data, aes(x = date, y = California, group = group))+
  geom_line() + geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "California Daily Cases")

l = ggplot(Louisiana_Data, aes(x = date, y = Louisiana, group = group))+
  geom_line() + geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Louisiana Daily Cases")

f = ggplot(Florida_Data, aes(x = date, y = Florida, group = group))+
  geom_line() + geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Florida Daily Cases")

ma = ggplot(Massachusetts_Data, aes(x = date, y = Massachusetts, group = group))+
  geom_line() + geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Massachusetts Daily Cases")

p = ggplot(Pennsylvania_Data, aes(x = date, y = Pennsylvania, group = group))+
  geom_line() + geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Pennsylvania Daily Cases")

i = ggplot(Illinois_Data, aes(x = date, y = Illinois, group = group))+
  geom_line() + geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Illinois Daily Cases")

t = ggplot(Texas_Data, aes(x = date, y = Texas, group = group))+
  geom_line() + geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Texas Daily Cases")

# Combine TOP 10 graphs into 1 graph
figure_daily =  ggarrange(ny,nj,m,c,l,f,ma,p,i,t,
                          ncol = 2, nrow = 5)
figure_daily

# plots of cumulative cases by state
ny_c = ggplot(NY_C_Data, aes(x = date, y = NY_C, group = group))+
  geom_line() + geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "NY Cumulative Cases")

nj_c = ggplot(NJ_C_Data, aes(x = date, y = NJ_C, group = group))+
  geom_line() + geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "New Jersey Cumulative Cases")

m_c = ggplot(Michigan_C_Data, aes(x = date, y = Michigan_C, group = group))+
  geom_line() + geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Michigan Cumulative Cases")

c_c = ggplot(California_C_Data, aes(x = date, y = California_C, group = group))+
  geom_line() + geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "California Cumulative Cases")

l_c = ggplot(Louisiana_C_Data, aes(x = date, y = Louisiana_C, group = group))+
  geom_line() + geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Louisiana Cumulative Cases")

f_c = ggplot(Florida_C_Data, aes(x = date, y = Florida_C, group = group))+
  geom_line() + geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Florida Cumulative Cases")

ma_c = ggplot(Massachusetts_C_Data, aes(x = date, y = Massachusetts_C, group = group))+
  geom_line() + geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Massachusetts Cumulative Cases")

p_c = ggplot(Pennsylvania_C_Data, aes(x = date, y = Pennsylvania_C, group = group))+
  geom_line() + geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Pennsylvania Cumulative Cases")

i_c = ggplot(Illinois_C_Data, aes(x = date, y = Illinois_C, group = group))+
  geom_line() + geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Illinois Cumulative Cases")

t_c = ggplot(Texas_C_Data, aes(x = date, y = Texas_C, group = group))+
  geom_line() + geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Texas Cumulative Cases")

# Combine TOP 10 graphs into 1 graph
figure_cumulative =  ggarrange(ny_c,nj_c,m_c,c_c,l_c,f_c,ma_c,p_c,i_c,t_c,
                               ncol = 2, nrow = 5)
figure_cumulative

# Plot the daily new cases time series for the top 10 states.
ggplot(top10_daily_counts_ts, aes(x = date, y = cases, colour = state, group = state)) + 
  geom_line() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  labs(title = "Daily New Cases")

# Plot the cumulative cases time series for the top 10 states.
ggplot(top10_daily_cumulative_counts_ts, aes(x = date, y = cases, colour = state, group = state)) + 
  geom_line() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  labs(title = "Cumulative Cases")