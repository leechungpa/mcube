getwd()

library(tidyverse)

data=read_csv('./data/general_currency_weekly.csv')


colnames(data)



data %>% filter(결제상품명 != '양평농협 창립50주년 기프트카드') %>%
  ggplot(aes(x=가맹점업종명, y=결제총액, col=가맹점업종명))+
  geom_line()+
  facet_wrap(~결제상품명, scales = "free", ncol=5)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())



data %>% filter(결제상품명 != '양평농협 창립50주년 기프트카드') %>%
  ggplot(aes(x=결제상품명, y=결제총액, fill=가맹점업종명))+
  geom_col()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

