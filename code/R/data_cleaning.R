rm(list=ls())
library(tidyverse)

#데이터 불러오기
all_2019=read.csv('C:/Users/82102/Desktop/문화 관광 project/국민여행조사/데이터/2019년 국민여행조사_국내여행.csv', header=TRUE)

all_2020=read.csv('C:/Users/82102/Desktop/문화 관광 project/국민여행조사/데이터/2020년 국민여행조사_국내여행.csv', header=TRUE)

all_2021=read.csv('C:/Users/82102/Desktop/문화 관광 project/국민여행조사/데이터/2021년 국민여행조사_국내여행.csv', header=TRUE)

#1.변수 통합
#2019년 데이터에는 숙박 장소에 대한 변수 없으므로 na 처리하여 변수 생성 + 만족도 변수

#1-1.
all_2019=all_2019 %>% 
  mutate(sleep_1=rep(NA,dim(all_2019)[1]),
         sleep_2=rep(NA,dim(all_2019)[1]),
         sleep_3=rep(NA,dim(all_2019)[1]),
         A10A_1=rep(NA,dim(all_2019)[1]),
         A10A_2=rep(NA,dim(all_2019)[1]),
         A10A_3=rep(NA,dim(all_2019)[1]),
         A10A_4=rep(NA,dim(all_2019)[1]),
         A10A_5=rep(NA,dim(all_2019)[1]),
         A10A_6=rep(NA,dim(all_2019)[1]),
         A10A_7=rep(NA,dim(all_2019)[1]),
         A10A_8=rep(NA,dim(all_2019)[1]),
         A10A_9=rep(NA,dim(all_2019)[1]),
         A10A_10=rep(NA,dim(all_2019)[1]),
         A10A_11=rep(NA,dim(all_2019)[1]),
         A10A_12=rep(NA,dim(all_2019)[1]))

cname=read.csv('C:/Users/82102/Desktop/문화 관광 project/국민여행조사 사용할 변수명.csv')

# 년도 변수 생성
all_2019 <- all_2019 %>%
  mutate(year = rep(2019, dim(all_2019)[1]))

all_2020 <- all_2020 %>%
  mutate(year = rep(2020, dim(all_2020)[1]))

all_2021 <- all_2021 %>%
  mutate(year = rep(2021, dim(all_2021)[1]))

df_2019 <- all_2019 %>%
  select(cname[, 1], year)
colnames(df_2019) <- c(cname[, 4], "year")

df_2020 <- all_2020 %>%
  select(cname[, 2], year)
colnames(df_2020) <- c(cname[, 4], "year")

df_2021 <- all_2021 %>%
  select(cname[, 3], year)
colnames(df_2021) <- c(cname[, 4], "year")

df <- rbind(df_2019, df_2020, df_2021)

table(df_2019$BARA, useNA = 'ifany')

#2.방문 지역 column 생성 및 지역 코드 처리
df=df %>% 
  pivot_longer(
    cols = spot_1:spot_3, 
    names_to = "spot", 
    values_to = "방문지역코드") 

write.csv(travel, file = "국민여행조사설문통합데이터.csv", row.names = F)


df=read.csv('C:/Users/82102/Desktop/문화 관광 project/국민여행조사설문통합데이터(방문지역).csv',header=TRUE)
df=df %>% 
  mutate(지역명=ifelse(지역명=='세종 세종시','세종.',지역명))

write.csv(df2, file = "국민여행조사설문통합데이터(최종).csv", row.names = F)


df=read.csv('국민여행조사설문통합데이터(최종).csv',header=TRUE)

df %>% 
  filter(year==2021) %>% 
  select(BARA) %>% 
  table(useNA = 'ifany')


df=df %>% 
  mutate(거주지=BARA) %>% 
  select(-BARA)

df=df %>% 
  mutate(거주지코드=거주지) %>% 
  select(-거주지)


df=df %>% 
  mutate(방문지역_시군구=지역명,
                 방문지역_시도=str_split(지역명,' ',simplify=TRUE)[,1]) %>% 
  select(-지역명,-방문지역코드,-시군구명,-시도명)

view(tail(df,1000))
table(df$방문지역_시군구,useNA = 'ifany')

area=read.csv('C:/Users/82102/Desktop/문화 관광 project/설문거주지코드정리.csv',header=TRUE)

area2=read.csv('C:/Users/82102/Desktop/문화 관광 project/설문거주지코드정리(2019).csv',header=TRUE)

df_2120=df %>% 
  filter(year %in% c(2020,2021))

df_19=df %>% 
  filter(year==2019)


df_2120=df_2120 %>% 
  left_join(area, by='거주지코드')

df_19=df_19 %>% 
  left_join(area2, by='거주지코드')

df=rbind(df_2120,df_19)

df=df %>% 
  select(-거주지코드)



df=df %>% 
  mutate(방문지역_시군구=ifelse(방문지역_시군구=='',NA,방문지역_시군구)) %>%
  mutate(방문지역_시도=ifelse(방문지역_시도=='',NA,방문지역_시도))

df=df %>% 
  mutate(근교여행여부=ifelse(거주지==방문지역_시도,1,
                          ifelse(is.na(방문지역_시도),NA,0)))


df=df %>% 
  mutate(계절=ifelse(when_travel %in% c(3,4,5), '봄',
                   ifelse(when_travel %in% c(6,7,8), '여름',
                          ifelse(when_travel %in% c(9,10,11),'가을',
                                 ifelse(when_travel %in% c(12,1,2), '겨울', NA)))))

write.csv(df, file = "국민여행조사설문통합데이터(최종2).csv", row.names = F)
df=read.csv('국민여행조사설문통합데이터(최종2).csv',header=TRUE)

n_travel=df %>% 
  filter(여행경험여부==2)

n_travel=n_travel %>% 
  mutate(여행방문순서=ifelse(spot=='spot_1',1,
                       ifelse(spot=='spot_2',2,3))) %>% 
  select(-spot)

travel=df %>% 
  filter(여행경험여부==1)

table(n_travel$계절,useNA = 'ifany')



travel=travel %>% 
  mutate(여행방문순서=ifelse(spot=='spot_1',1,
                       ifelse(spot=='spot_2',2,3))) %>% 
  select(-spot)
view(head(n_travel,1000))

colnames(travel)

tt=travel %>% 
  select(A5_1:A5_21)

tt[apply(is.na(tt),1,all),]


travel=travel %>% 
  pivot_longer(
  cols = A5_1:A5_21, 
  names_to = "여행활동변수", 
  values_to = "여행활동")

travel=travel %>% 
  filter(!is.na(방문지역_시도))

travel=travel %>% 
  select(-여행활동변수) %>% 
  filter(!is.na(여행활동))

colnames(n_travel)
colnames(travel)

write.csv(n_travel, file = "국민여행조사설문통합데이터(비여행자최종).csv", row.names = F)
write.csv(travel, file = "국민여행조사설문통합데이터(여행자최종).csv", row.names = F)


#코드 변수 변환
rm(list=ls())
travel=read.csv('국민여행조사설문통합데이터_cl(여행자최종tour_cl).csv',header=TRUE)
traffic=read.csv('C:/Users/82102/Desktop/문화 관광 project/코드변수매칭/교통.csv',header=TRUE)
age=read.csv('C:/Users/82102/Desktop/문화 관광 project/코드변수매칭/연령.csv',header=TRUE)
sel=read.csv('C:/Users/82102/Desktop/문화 관광 project/코드변수매칭/여행지선택이유.csv',header=TRUE)
sleep=read.csv('C:/Users/82102/Desktop/문화 관광 project/코드변수매칭/숙박시설.csv',header=TRUE)
non=read.csv('C:/Users/82102/Desktop/문화 관광 project/코드변수매칭/비여행이유.csv',header=TRUE)
act=read.csv('C:/Users/82102/Desktop/문화 관광 project/코드변수매칭/여행활동.csv',header=TRUE)


n_travel=read.csv('국민여행조사설문통합데이터(비여행자최종).csv',header=TRUE)

n_travel=n_travel %>% 
  left_join(non, by='B9_3') %>% 
  select(-B9_3)


travel=travel %>% 
  left_join(traffic, by='A1_1') %>% 
  select(-A1_1)

travel=travel %>% 
  left_join(age, by='BAGE') %>% 
  select(-BAGE)

travel=travel %>% 
  left_join(sel, by='A4_1') %>% 
  select(-A4_1)

travel=travel %>% 
  left_join(sel, by='A4_2') %>% 
  select(-A4_2)

travel=travel %>% 
  left_join(sel, by='A4_3') %>% 
  select(-A4_3)


travel=travel %>% 
  mutate(여행지선택이유_1=여행지선택이유) %>% 
  select(-여행지선택이유)


travel=travel %>% 
  left_join(sleep, by='sleep_1') %>% 
  select(-sleep_1)

travel=travel %>% 
  mutate(travel_act=여행활동) %>% 
  select(-여행활동)

travel=travel %>% 
  left_join(act, by='travel_act') %>% 
  select(-travel_act)

travel=travel %>% 
  mutate(A7=ifelse(A7 %in% c('1','2','3','4','5','6','7','8','9','10'),A7,'단체'))

travel=travel %>% 
  mutate(테마=ifelse(tour_cluster %in% c('레포츠_상','레포츠_중','레포츠_하'),'레포츠',
                   ifelse(tour_cluster %in% c('자연_상','자연_중','자연_하'),'자연', 
                          ifelse(tour_cluster %in% c('문화쇼핑휴양_상','문화쇼핑휴양_중','문화쇼핑휴양_하'),'문화쇼핑휴양',
                                 ifelse(tour_cluster %in% c('역사_상','역사_중','역사_하'), '역사',NA)))))











