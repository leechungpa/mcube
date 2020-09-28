getwd()

library(tidyverse)

#  1_유동인구.ipynb 이후 실행 필요
time_data=read_csv('./data/bigcontest/유동인구_정리(시간대별).csv') %>% select(-X1)
data=read_csv('./data/bigcontest/유동인구_정리.csv')%>% select(-X1)

colnames(time_data)
colnames(data)

time_data
data 

# 4개지역 전년이랑 차이
time_data %>% 
  mutate(WEEK = as.numeric(time_data$WEEK),YEAR = as.factor(time_data$YEAR)) %>%
  group_by(WEEK,GU_NM,YEAR) %>%
  summarize(CNT = sum(CNT))%>%
  ungroup() %>%
  filter(WEEK!=5 & WEEK!=22) %>%
  ggplot(aes(x=WEEK , y=CNT, color=YEAR))+
  geom_line()+
  facet_wrap(~GU_NM)


# 출퇴근 시간 4개지역 전년이랑 차이
time_data %>% 
  mutate(WEEK = as.numeric(time_data$WEEK), TIME = as.numeric(time_data$TIME), YEAR = as.factor(time_data$YEAR)) %>%
  filter((TIME >= 7 & TIME <= 10) | (TIME >= 17 & TIME <= 20)) %>%
  group_by(WEEK,GU_NM,YEAR) %>%
  summarize(CNT = sum(CNT))%>%
  ungroup() %>%
  filter(WEEK!=5 & WEEK!=22) %>%
  ggplot(aes(x=WEEK , y=CNT, color=YEAR))+
  geom_line()+
  facet_wrap(~GU_NM)



# 주간 시간 4개지역 전년이랑 차이
time_data %>% 
  mutate(WEEK = as.numeric(time_data$WEEK), TIME = as.numeric(time_data$TIME), YEAR = as.factor(time_data$YEAR)) %>%
  filter(TIME >= 11 & TIME <= 16) %>%
  group_by(WEEK,GU_NM,YEAR) %>%
  summarize(CNT = sum(CNT))%>%
  ungroup() %>%
  filter(WEEK!=5 & WEEK!=22) %>%
  ggplot(aes(x=WEEK , y=CNT, color=YEAR))+
  geom_line()+
  facet_wrap(~GU_NM)


# 야간 시간 4개지역 전년이랑 차이
time_data %>% 
  mutate(WEEK = as.numeric(time_data$WEEK), TIME = as.numeric(time_data$TIME), YEAR = as.factor(time_data$YEAR)) %>%
  filter(TIME <= 6 | TIME >= 21) %>%
  group_by(WEEK,GU_NM,YEAR) %>%
  summarize(CNT = sum(CNT))%>%
  ungroup() %>%
  filter(WEEK!=5 & WEEK!=22) %>%
  ggplot(aes(x=WEEK , y=CNT, color=YEAR))+
  geom_line()+
  facet_wrap(~GU_NM)








# 출퇴근 시간 등 4개지역 전년이랑 차이
time_data %>% 
  mutate(WEEK = as.numeric(time_data$WEEK), TIME = as.numeric(time_data$TIME), YEAR = as.factor(time_data$YEAR)) %>%
  mutate(time_lab = case_when(
    ((TIME >= 7 & TIME <= 10) | (TIME >= 17 & TIME <= 20)) ~ '출퇴근 시간',
     (TIME >= 11 & TIME <= 16) ~ '낮시간',
     TRUE ~ '밤시간')) %>%
  group_by(WEEK,GU_NM,YEAR, time_lab) %>%
  summarize(CNT = sum(CNT))%>%
  ungroup() %>%
  filter(WEEK!=5 & WEEK!=22)%>% 
  spread(YEAR, CNT,sep = "")%>%
  mutate(diff = YEAR2020-YEAR2019) %>%
  ggplot(aes(x=WEEK , y=diff, color=time_lab))+
  geom_line()+
  facet_wrap(~GU_NM) +
  ylim(NA, 1)

