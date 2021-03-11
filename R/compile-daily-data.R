## Pull Together Daily Data From Downloaded Spreadsheets
## Version: 1.0
## Last Updated: March 11, 2021

library(tidyverse)
library(stringr)
library(janitor)

# list of files
spreadsheets <- list.files(path = 'data/daily/')

for(f in spreadsheets){
  
  this_country <- f %>% 
    str_remove_all(' 10-Year Bond Yield Historical Data') %>% 
    str_remove_all('.csv') %>% 
    str_remove_all(' \\(') %>% 
    str_remove_all('\\)') %>% 
    str_remove_all('[0-9]') %>% 
    unique
  
  daily <- paste0('data/daily/', f) %>% 
    read_csv() %>% 
    clean_names() %>% 
    mutate(date = as.Date(date, format = '%b %d, %Y')) %>% 
    mutate(country = this_country) %>% 
    select(country, date, price, open, high, low)
  
  if(f == spreadsheets[1]){
    data <- daily
  } else{
    data <- bind_rows(data, daily)
  }
}

# keep only the unique records, sort by country and date
data <- data %>% 
  unique %>% 
  arrange(country, date) %>% 
  # remove an extreme Austrian outlier
  filter(!(date == '2020-04-17' &
           country == 'Austria'))

write_csv(data, file = 'data/daily.csv')

# plot
ggplot(data) +
  geom_line(aes(x=date, y=price)) +
  facet_wrap(~country) +
  theme_bw() +
  labs(x='Date', y='Price')