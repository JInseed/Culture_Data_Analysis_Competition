rm(list=ls())
library(NbClust)
library(caret)
library(tidyverse)
library(cluster)
library(factoextra)
library(kableExtra)

write.csv(travel,file = '국민여행조사설문통합데이터_cl(여행자최종tour_cl).csv', row.names = F)

travel=read.csv('국민여행조사설문통합데이터(여행자최종).csv',header=TRUE)

travel$travel_day=as.factor(travel$travel_day)
travel$방문지역_시군구=as.factor(travel$방문지역_시군구)
travel$방문지역_시도=as.factor(travel$방문지역_시도)
travel$숙박시설=as.factor(travel$숙박시설)
travel$sleep_2=as.factor(travel$sleep_2)
travel$sleep_3=as.factor(travel$sleep_3)
travel$교통=as.factor(travel$교통)
travel$A1_2=as.factor(travel$A1_2)
travel$A1_3=as.factor(travel$A1_3)
travel$여행지선택이유_1=as.factor(travel$여행지선택이유_1)
travel$여행지선택이유_2=as.factor(travel$여행지선택이유_2)
travel$여행지선택이유_3=as.factor(travel$여행지선택이유_3)
travel$A7=as.factor(travel$A7)
travel$A11=as.factor(travel$A11)
travel$B9_1=as.factor(travel$B9_1)
travel$B9_2=as.factor(travel$B9_2)
travel$B9_3=as.factor(travel$B9_3)
travel$DQ6A=as.factor(travel$DQ6A)
travel$DQ6B=as.factor(travel$DQ6B)
travel$BSEX=as.factor(travel$BSEX)
travel$연령=as.factor(travel$연령)
travel$거주지=as.factor(travel$거주지)
travel$year=as.factor(travel$year)
travel$근교여행여부=as.factor(travel$근교여행여부)
travel$계절=as.factor(travel$계절)
travel$여행활동=as.factor(travel$여행활동)

tour_cl=read.csv('tour_cluster(번호).csv',header=TRUE)
view(tour_cl)

travel=travel %>% 
  left_join(tour_cl, by='방문지역_시군구')

travel %>% 
  filter(year==2019) %>% 
  select(거주지) %>% 
  table(useNA = 'ifany')


#1.년도별 근교여행여부
travel %>% 
  group_by(year,근교여행여부) %>% 
  summarise(count=n()) %>% 
  mutate(prop=count/sum(count)) %>% 
  ggplot()+
  geom_bar(aes(x=근교여행여부,
                     y = count,
                     fill=year),
           position='dodge',
           stat='identity')
  
travel %>% 
  group_by(계절,근교여행여부) %>% 
  summarise(count=n()) %>% 
  mutate(prop=count/sum(count)) %>% 
  ggplot(aes(x=계절,
             y=count, group=근교여행여부, colour=근교여행여부))+
  geom_line()+
  geom_point()




#2.년도별 여행활동 수
#2-1 2019
travel %>% 
  filter(year==2021) %>%
  group_by(여행활동) %>% 
  summarise(count=n()) %>% 
  mutate(prop=count/sum(count)) %>% 
  mutate(여행활동=fct_reorder(여행활동,count)) %>%
  arrange(-count) %>% 
  head(10) %>% 
  ggplot()+
  geom_bar(aes(여행활동,prop,fill=여행활동),col='white',stat='identity')+
  coord_flip()+
  theme(legend.position = 'none')+
  ylim(0,0.35)+
  labs(title='2021년도 여행활동 횟수비율 상위 10개', y='비율')


#3. 년도계절별 여행 횟수
travel %>% 
  mutate(계절=fct_relevel(계절,c('봄','여름','가을','겨울') )) %>% 
  ggplot()+
  geom_bar(aes(계절,fill=year),
           position='dodge')


travel %>% 
  mutate(계절=fct_relevel(계절,c('봄','여름','가을','겨울') )) %>%
  group_by(계절,year) %>% 
  summarise(count=n()) %>% 
  mutate(prop=count/sum(count)) %>% 
  ggplot(aes(x=계절,
             y=count, group=year,col=year))+
  geom_line()+
  geom_point()



#4.년도별 방문지 순서
travel %>% 
  filter(year==2021) %>%
  group_by(방문지역_시군구) %>% 
  summarise(count=n()) %>%
  mutate(prop=count/sum(count)) %>% 
  mutate(방문지역_시군구=fct_reorder(방문지역_시군구,count)) %>% 
  arrange(-count) %>% 
  head(20) %>% 
  ggplot()+
  geom_bar(aes(방문지역_시군구,prop,fill=방문지역_시군구),stat='identity')+
  coord_flip()+
  theme(legend.position = 'none')+
  ylim(0,0.08)+
  labs(title='2021년도 여행방문지역횟수비율', y='비율')

unique(travel$여행활동)
#5.여행활동 별 지역 방문 횟수 순위
travel %>% 
  filter(year==2021) %>% 
  filter(여행활동=='역사 유적지 방문') %>% 
  group_by(방문지역_시군구) %>% 
  summarise(count=n()) %>% 
  mutate(방문지역_시군구=fct_reorder(방문지역_시군구,count)) %>% 
  arrange(-count) %>% 
  head(20) %>% 
  ggplot()+
  geom_bar(aes(방문지역_시군구,count,fill=방문지역_시군구),stat='identity')+
  coord_flip()+
  theme(legend.position = 'none')

#6.년도별 여행지 선택 이유 순위별 순위
#6-1순위
travel %>% 
  filter(year==2021) %>% 
  group_by(여행지선택이유_3) %>% 
  filter(!is.na(여행지선택이유_3)) %>% 
  summarise(count=n()) %>%
  ggplot()+
  geom_bar(aes(fct_reorder(여행지선택이유_3,count),count,fill=여행지선택이유_3),
           stat='identity')+
  coord_flip()+
  theme(legend.position = 'none')+
  labs(title='여행지 선택 이유 1순위', y='횟수', x='여행지 선택 이유')

travel %>% 
  filter(year==2021) %>% 
  group_by(여행지선택이유_2) %>% 
  filter(!is.na(여행지선택이유_2)) %>% 
  summarise(count=n()) %>%
  ggplot()+
  geom_bar(aes(fct_reorder(여행지선택이유_2,count),count,fill=여행지선택이유_2),
           stat='identity')+
  coord_flip()+
  theme(legend.position = 'none')+
  labs(title='여행지 선택 이유 2순위', y='횟수', x='여행지 선택 이유')

table(travel$여행지선택이유_1)
#6-1.여행지 선택 이유별 여행 활동
travel %>% 
  filter(여행지선택이유_1=='저렴한 여행경비') %>% 
  group_by(여행활동) %>% 
  filter(!is.na(여행활동)) %>% 
  summarise(count=n()) %>%
  ggplot()+
  geom_bar(aes(fct_reorder(여행활동,count),count,fill=여행활동),
           stat='identity')+
  coord_flip()+
  theme(legend.position = 'none')+
  labs(title='여행지 지명도', y='횟수', x='여행활동')

#6-2.여행지 선택 이유별 방문 지역 횟수
travel %>% 
  filter(여행지선택이유_1=='이동 거리') %>% 
  group_by(방문지역_시군구) %>% 
  filter(!is.na(방문지역_시군구)) %>% 
  summarise(count=n()) %>%
  arrange(-count) %>% 
  head(20) %>% 
  ggplot()+
  geom_bar(aes(fct_reorder(방문지역_시군구,count),count,fill=방문지역_시군구),
           stat='identity')+
  coord_flip()+
  theme(legend.position = 'none')+
  labs(title='여행지 지명도', y='횟수', x='여행활동')



  
#7.년도별, 거주지별 비여행이유 순위별 순위
n_travel=read.csv('국민여행조사설문통합데이터(비여행자최종).csv',header=TRUE)
table(n_travel$거주지, useNA='ifany')
#7-1년도별
n_travel %>% 
  filter(year==2020) %>% 
  group_by(비여행이유_1) %>% 
  filter(!is.na(비여행이유_1)) %>% 
  summarise(count=n()) %>%
  ggplot()+
  geom_bar(aes(fct_reorder(비여행이유_1,count),count,fill=비여행이유_1),
           stat='identity')+
  coord_flip()+
  theme(legend.position = 'none')+
  labs(title='2019 비여행 이유 1순위', y='횟수', x='비여행 이유')


#7-2 거주지별
n_travel %>% 
  filter(거주지=='강원') %>% 
  group_by(비여행이유_1) %>% 
  filter(!is.na(비여행이유_1)) %>% 
  summarise(count=n()) %>%
  ggplot()+
  geom_bar(aes(fct_reorder(비여행이유_1,count),count,fill=비여행이유_1),
           stat='identity')+
  coord_flip()+
  theme(legend.position = 'none')+
  labs(title='서울 비여행 이유 1순위', y='횟수', x='비여행 이유')


#8.년도별 여행일수
travel %>% 
  filter(year==2019) %>% 
  group_by(travel_day) %>% 
  filter(!is.na(travel_day)) %>% 
  summarise(count=n()) %>%
  head(5) %>% 
  ggplot()+
  geom_bar(aes(fct_relevel(travel_day,c('0','1','2','3','4')),count,fill=travel_day),
           stat='identity')+
  theme(legend.position = 'none')+
  labs(title='2019 여행일수', y='횟수', x='여행일수')+
  ylim(0,30000)


#9. 년도별 여행동반자수
travel %>% 
  filter(year==2021) %>% 
  group_by(A7) %>% 
  filter(!is.na(A7)) %>% 
  summarise(count=n()) %>%
  mutate(prop=count/sum(count)) %>% 
  ggplot()+
  geom_bar(aes(fct_relevel(A7,c('1','2','3','4','5','6','7','8','9','10')),prop,fill=A7),
           stat='identity')+
  theme(legend.position = 'none')+
  labs(title='2019 여행동반자수', y='횟수', x='여행동반자수')+
  ylim(0,0.4)

table(travel$year)







