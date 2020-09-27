getwd()

library(tidyverse)

data=read_csv('./data/localcurrency/general_currency_weekly.csv')


colnames(data)



data %>% filter(결제상품명 != '양평농협 창립50주년 기프트카드') %>%
  ggplot(aes(x=가맹점업종명, y=결제총액, col=가맹점업종명))+
  geom_line()+
  facet_wrap(~결제상품명, scales = "free", ncol=5)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())



data %>% filter(결제상품명 != '양평농협 창립50주년 기프트카드') %>%
  ggplot(aes(x=결제상품명, y=결제총액, fill=가맹점업종명))+
  geom_col()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#############



min(data$weekly_from_mon)
max(data$weekly_from_mon)


data %>% 
  # filter(결제상품명 == '부천페이') %>% 
  group_by(weekly_from_mon, 결제상품명 ) %>%
  summarize(total=sum(결제총액)) %>%
  ggplot(aes(x=weekly_from_mon , y=total, col = 결제상품명))+
  geom_line()
  



data %>% filter(결제상품명 != '양평농협 창립50주년 기프트카드') %>%
  ggplot(aes(x=가맹점업종명, y=결제총액, col=가맹점업종명))+
  geom_line()+
  facet_wrap(~결제상품명, scales = "free", ncol=5)