rm(list=ls())
library(NbClust)
library(caret)
library(tidyverse)
library(cluster)
library(factoextra)
library(kableExtra)
library(gridExtra)

travel=read.csv('국민여행조사설문통합데이터_cl(여행자최종tour_cl).csv',header=TRUE)
n_travel=read.csv('국민여행조사설문통합데이터(비여행자최종).csv', header=TRUE)

test=read.csv('국민여행조사설문통합데이터_cl(여행자최종tour_cl).csv',header=TRUE)



table(travel$tour_cluster, useNA = 'ifany')
view(arch)

#1.군집별 방문지 유망주 선정
p1=travel %>% 
  filter(tour_cluster=='자연_상') %>%
  group_by(방문지역_시군구) %>% 
  summarise(count=n()) %>%
  mutate(방문지역_시군구=fct_reorder(방문지역_시군구,count)) %>% 
  arrange(-count) %>% 
  ggplot()+
  geom_bar(aes(방문지역_시군구,count,fill=방문지역_시군구),stat='identity')+
  labs(title='군집별 방문지 순위', y='방문횟수')+
  ylim(0,5500)+
  theme(legend.position = 'none')+
  coord_flip()

  
p2=travel %>% 
  filter(tour_cluster=='자연_중') %>%
  group_by(방문지역_시군구) %>% 
  summarise(count=n()) %>%
  mutate(방문지역_시군구=fct_reorder(방문지역_시군구,count)) %>% 
  arrange(-count) %>% 
  ggplot()+
  geom_bar(aes(방문지역_시군구,count,fill=방문지역_시군구),stat='identity')+
  labs(title='군집별 방문지 순위', y='방문횟수')+
  ylim(0,5500)+
  theme(legend.position = 'none')+
  coord_flip()

grid.arrange(p1,p2,nrow=2)

#레포츠
lh=travel %>% 
  filter(tour_cluster=='레포츠_상') %>%
  group_by(방문지역_시군구) %>% 
  summarise(count=n()) %>%
  arrange(-count) %>% 
  mutate(cluster='레포츠_상')

lm=travel %>% 
  filter(tour_cluster=='레포츠_중') %>%
  group_by(방문지역_시군구) %>% 
  summarise(count=n()) %>% 
  arrange(-count) %>% 
  mutate(cluster='레포츠_중')


lesuir=rbind(lh,lm)
lesuir %>% 
  group_by(cluster) %>%
  mutate(cluster=as.factor(cluster)) %>% 
  mutate(cluster=fct_relevel(cluster,'레포츠_중','레포츠_상')) %>% 
  mutate(방문지역_시군구=fct_reorder(방문지역_시군구,count)) %>%
  head(20) %>% 
  ggplot()+
  geom_bar(aes(방문지역_시군구,count,fill=cluster),stat='identity')+
  labs(title='레포츠 cluster 방문지횟수', y='방문횟수')+
  coord_flip()

#자연
nh=travel %>% 
  filter(tour_cluster=='자연_상') %>%
  group_by(방문지역_시군구) %>% 
  summarise(count=n()) %>%
  arrange(-count) %>% 
  mutate(cluster='자연_상')

nm=travel %>% 
  filter(tour_cluster=='자연_중') %>%
  group_by(방문지역_시군구) %>% 
  summarise(count=n()) %>% 
  arrange(-count) %>% 
  mutate(cluster='자연_중')


nature=rbind(nh,nm)
nature %>% 
  group_by(cluster) %>% 
  mutate(cluster=as.factor(cluster)) %>% 
  mutate(cluster=fct_relevel(cluster,'자연_중','자연_상')) %>%
  mutate(방문지역_시군구=fct_reorder(방문지역_시군구,count)) %>%
  head(23) %>% 
  ggplot()+
  geom_bar(aes(방문지역_시군구,count,fill=cluster),stat='identity')+
  labs(title='자연 cluster 방문지횟수', y='방문횟수')+
  coord_flip()

#역사
hh=travel %>% 
  filter(tour_cluster=='역사_상') %>%
  group_by(방문지역_시군구) %>% 
  summarise(count=n()) %>%
  arrange(-count) %>% 
  mutate(cluster='역사_상')

hm=travel %>% 
  filter(tour_cluster=='역사_중') %>%
  group_by(방문지역_시군구) %>% 
  summarise(count=n()) %>% 
  arrange(-count) %>% 
  mutate(cluster='역사_중')


history=rbind(hh,hm)
history %>% 
  group_by(cluster) %>%
  mutate(cluster=as.factor(cluster)) %>% 
  mutate(cluster=fct_relevel(cluster,'역사_중','역사_상')) %>% 
  mutate(방문지역_시군구=fct_reorder(방문지역_시군구,count)) %>% 
  head(12) %>% 
  ggplot()+
  geom_bar(aes(방문지역_시군구,count,fill=cluster),stat='identity')+
  labs(title='역사 cluster 방문지횟수', y='방문횟수')+
  coord_flip()

#문화쇼핑휴양
ch=travel %>% 
  filter(tour_cluster=='문화쇼핑휴양_상') %>%
  group_by(방문지역_시군구) %>% 
  summarise(count=n()) %>%
  arrange(-count) %>% 
  mutate(cluster='문화쇼핑휴양_상')

cm=travel %>% 
  filter(tour_cluster=='문화쇼핑휴양_중') %>%
  group_by(방문지역_시군구) %>% 
  summarise(count=n()) %>% 
  arrange(-count) %>% 
  mutate(cluster='문화쇼핑휴양_중')


culture=rbind(ch,cm)
culture %>% 
  group_by(cluster) %>%
  mutate(cluster=as.factor(cluster)) %>% 
  mutate(cluster=fct_relevel(cluster,'문화쇼핑휴양_중','문화쇼핑휴양_상')) %>% 
  mutate(방문지역_시군구=fct_reorder(방문지역_시군구,count)) %>% 
  head(30) %>% 
  ggplot()+
  geom_bar(aes(방문지역_시군구,count,fill=cluster),stat='identity')+
  labs(title='문화쇼핑휴양 cluster 방문지횟수', y='방문횟수')+
  coord_flip()



#2.군집별 전반적 만족도 일단 논외
travel %>% 
  group_by(tour_cluster, year) %>% 
  summarise(만족도=mean(A10,na.rm=TRUE))

travel %>% 
  filter(year!=2019) %>% 
  select(A10A_1) %>% 
  table(useNA = 'ifany')


table(travel$A10A_4, useNA = 'ifany')


#3. 군집별 여행 활동
#3-레포츠
travel %>% 
  filter(tour_cluster=='레포츠_상') %>% 
  group_by(여행활동) %>% 
  summarise(count=n()) %>% 
  mutate(prop=count/sum(count)) %>% 
  mutate(여행활동=fct_reorder(여행활동,count)) %>%
  head(10) %>% 
  ggplot()+
  geom_bar(aes(여행활동,prop,fill=여행활동),col='white',stat='identity')+
  labs(title='군집별 여행 활동', y='방문횟수')+
  theme(legend.position = 'none')+
  coord_flip()

#3-자연
travel %>% 
  filter(tour_cluster=='자연_상') %>% 
  group_by(여행활동) %>% 
  summarise(count=n()) %>% 
  mutate(prop=count/sum(count)) %>% 
  mutate(여행활동=fct_reorder(여행활동,-count)) %>%
  head(10) %>% 
  ggplot()+
  geom_bar(aes(여행활동,prop,fill=여행활동),col='white',stat='identity')+
  labs(title='군집별 여행 활동', y='방문횟수')+
  theme(legend.position = 'none')

#3-역사
travel %>% 
  filter(tour_cluster=='역사_상') %>% 
  group_by(여행활동) %>% 
  summarise(count=n()) %>% 
  mutate(prop=count/sum(count)) %>% 
  mutate(여행활동=fct_reorder(여행활동,-count)) %>%
  head(10) %>% 
  ggplot()+
  geom_bar(aes(여행활동,prop,fill=여행활동),col='white',stat='identity')+
  labs(title='군집별 여행 활동', y='방문횟수')+
  theme(legend.position = 'none')

#3-문화쇼핑휴양
travel %>% 
  filter(tour_cluster=='문화쇼핑휴양_상') %>% 
  group_by(여행활동) %>% 
  summarise(count=n()) %>% 
  mutate(prop=count/sum(count)) %>% 
  mutate(여행활동=fct_reorder(여행활동,-count)) %>%
  head(10) %>% 
  ggplot()+
  geom_bar(aes(여행활동,prop,fill=여행활동),col='white',stat='identity')+
  labs(title='군집별 여행 활동', y='방문횟수')+
  theme(legend.position = 'none')


#4. 군집별 숙박 시설
#4-레포츠
travel %>% 
  filter(테마=='역사') %>%
  filter(!is.na(숙박시설)) %>% 
  group_by(숙박시설) %>% 
  summarise(count=n()) %>% 
  mutate(prop=count/sum(count)) %>% 
  mutate(sleep_1=fct_reorder(숙박시설,count)) %>%
  ggplot()+
  geom_bar(aes(sleep_1,prop,fill=sleep_1),col='white',stat='identity')+
  labs(title='군집별 숙박시설',x='숙박시설', y='숙박시설이용횟수')+
  theme(legend.position = 'none')+
  coord_flip()





#5. 군집별 주요 교통수단
travel %>% 
  filter(tour_cluster=='문화쇼핑휴양_상') %>% 
  group_by(교통) %>% 
  summarise(count=n()) %>% 
  mutate(prop=count/sum(count)) %>% 
  arrange(-count) %>% 
  head(10) %>% 
  ggplot()+
  geom_bar(aes(fct_reorder(교통,count),prop,fill=교통),col='white',stat='identity')+
  labs(title='레포츠_상 교통수단',x='교통수단', y='교통수단횟수비율')+
  theme(legend.position = 'none')+
  coord_flip()

#6. 군집별 여행지 여행이유 순위별 순위
travel %>% 
  filter(tour_cluster=='자연_하') %>%
  group_by(여행지선택이유_1) %>% 
  summarise(count=n()) %>% 
  mutate(prop=count/sum(count)) %>% 
  arrange(-prop) %>% 
  head(5) %>% 
  ggplot()+
  geom_bar(aes(fct_reorder(여행지선택이유_1,count),prop,fill=여행지선택이유_1),col='white',stat='identity')+
  labs(title='군집별 여행이유 1순위',x='여행이유', y='여행이유 횟수')+
  theme(legend.position = 'none')

#군집별 여행지 선택 이유별 방문 횟수
travel %>% 
  filter(여행지선택이유_1=='체험 프로그램 유무') %>%
  filter(!is.na(tour_cluster)) %>% 
  group_by(tour_cluster) %>% 
  summarise(count=n()) %>% 
  mutate(tour_cluster=fct_reorder(tour_cluster,count)) %>% 
  arrange(-count) %>% 
  ggplot()+
  geom_bar(aes(tour_cluster,count,fill=tour_cluster),stat='identity')+
  coord_flip()+
  theme(legend.position = 'none')

travel %>% 
  filter(tour_cluster=='자연_하') %>% 
  select(방문지역_시군구) %>% 
  unique()

#7. 군집별 여행동반자수
travel %>% 
  filter(tour_cluster=='문화쇼핑휴양_하') %>% 
  group_by(A7) %>% 
  filter(!is.na(A7)) %>% 
  summarise(count=n()) %>%
  ggplot()+
  geom_bar(aes(fct_relevel(A7,c('1','2','3','4','5','6','7','8','9','10')),count,fill=A7),
           stat='identity')+
  theme(legend.position = 'none')+
  labs(title='2019 여행동반자수', y='횟수', x='여행동반자수')

#8. 군집별 재방문의도 평균
travel %>% 
  group_by(tour_cluster) %>% 
  summarise(재방문의도=mean(A11,na.rm=TRUE))

#9. 군집별 연령 분포
travel %>% 
  filter(tour_cluster=='자연_하') %>% 
  group_by(연령) %>% 
  summarise(count=n()) %>%
  mutate(prop=count/sum(count)) %>% 
  ggplot()+
  geom_bar(aes(fct_relevel(연령,c('15~19세','20대','30대','40대','50대','60대','70대이상')),prop,fill=연령),col='white',stat='identity')+
  labs(title='군집별 연령분포',x='', y='연령 횟수')+
  theme(legend.position = 'none')

#10. 군집 계절 별 여행횟수
travel %>% 
  filter(tour_cluster=='역사_중') %>% 
  mutate(계절=fct_relevel(계절,c('봄','여름','가을','겨울') )) %>% 
  ggplot()+
  geom_bar(aes(계절,fill=계절),
           position='dodge')

#11. 군집 별 여행 총 경비 평균
travel %>% 
  group_by(tour_cluster) %>% 
  summarise(여행총경비=mean(travel_cost,na.rm=TRUE))

#12. 군집 별 여행 일수
travel %>% 
  filter(tour_cluster=='문화쇼핑휴양_상') %>% 
  group_by(travel_day) %>% 
  filter(!is.na(travel_day)) %>% 
  summarise(count=n()) %>%
  head(5) %>% 
  ggplot()+
  geom_bar(aes(fct_relevel(travel_day,c('0','1','2','3','4')),count,fill=travel_day),
           stat='identity')+
  theme(legend.position = 'none')+
  labs(title='군집별 여행일수', y='횟수', x='여행일수')

#13. 군집 별 유형 별 만족도
travel %>% 
  filter(A10A_10!=9) %>% 
  group_by(tour_cluster) %>% 
  summarise(자연경관=mean(A10A_10,na.rm=TRUE))



