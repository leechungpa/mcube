library(tidyverse)

getwd()

data = read_csv('./data/card_covid.csv')

seoul_data = data %>% filter(date!=0) %>% select(-c('X1',date)) %>% filter(city=='seoul')

seoul_data 
