rm(list=ls())
library(tidyverse)
library(NbClust)
library(caret)
library(cluster)
library(factoextra)
library(kableExtra)

card=read.csv('C:/Users/82102/Desktop/문화 관광 project/군집화/카드데이터전처리.csv',header=TRUE)
card[is.na(card)]=0
colnames(card)
card=card[,-c(2,4,6,8,10,12,14,16,18,20,22)]
card2=card[,c(1,2,4,6,8,10,12,14,16,18,20,22)]

card=card %>% 
  mutate(지역명=str_split(지역명,' ',simplify=TRUE)[,1]) %>% 
  group_by(지역명) %>% 
  summarise(관광쇼핑=sum(관광쇼핑.결제.수),
              교통=sum(교통.결제.수),
              레저스포츠=sum(레저스포츠.결제.수),
              목욕=sum(목욕.결제.수),
              문화=sum(문화.결제.수),
              사진촬영=sum(사진촬영.결제.수),
              숙박=sum(숙박.결제.수),
              외식=sum(외식.결제.수),
              운동경기관람=sum(운동경기관람.결제.수),
              종교활동=sum(종교활동.결제.수),
              체험=sum(체험.결제.수)) %>% 
  as.data.frame()


card2=card2 %>% 
  mutate(지역명=str_split(지역명,' ',simplify=TRUE)[,1]) %>% 
  group_by(지역명) %>% 
  summarise(관광쇼핑=sum(관광쇼핑.매출),
                교통=sum(교통.매출),
                레저스포츠=sum(레저스포츠.매출),
                목욕=sum(목욕.매출),
                문화=sum(문화.매출),
                사진촬영=sum(사진촬영.매출),
                숙박=sum(숙박.매출),
                외식=sum(외식.매출),
                운동경기관람=sum(운동경기관람.매출),
                종교활동=sum(종교활동.매출),
                체험=sum(체험.매출)) %>% 
  as.data.frame()


rownames(card)=card[[1]]
rownames(card2)=card2[[1]]

card=card %>% 
  select(-지역명)

card2=card2 %>% 
  select(-지역명)


card=round(card)
card2=round(card2)

view(card)

card.z=scale(card)
card.z=as.data.frame(card.z)

card.s=sqrt(card+1)
card.s=as.data.frame(card.s)

card.l=log(card+1)
card.l=as.data.frame(card.l)

card.lz=scale(card.l)
card.lz=as.data.frame(card.lz)

# NbClust 함수로 최적의 군집 수 찾기

nc <- NbClust(card.s, min.nc = 2, max.nc = 10, method = "kmeans")

par(mfrow=c(1,1))
barplot(table(nc$Best.n[1,]),
        xlab="Number of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen")

#Elbow Method로 찾기
## find the optimal number of clusters using Total within-cluster sum of squares
fviz_nbclust(card, kmeans, method = "wss", k.max = 10) + 
  theme_minimal() + 
  ggtitle("Elbow Method")


#실루엣 이용
fviz_nbclust(card.z, kmeans, method = "silhouette", k.max = 10) + 
  theme_minimal() + 
  ggtitle("Silhouette Method")


card_km = kmeans(card.z, centers = 2)
sil = silhouette(card_km$cluster, dist(card.z))
fviz_silhouette(sil)


fviz_cluster(card_km, data = card.z, geom = "text")+
  theme(legend.position = "none")

sm <-card_km$centers %>%
  t()  %>%  
  kable("pandoc", digits = 3)
sm

card_km$cluster



#card2
card2=round(card2)
view(card2)

card2.z=scale(card2)
card2.z=as.data.frame(card2.z)

card2.s=sqrt(card2+1)
card2.s=as.data.frame(card2.s)

card2.l=log(card2+1)
card2.l=as.data.frame(card2.l)

card2.lz=scale(card2.l)
card2.lz=as.data.frame(card2.lz)

# NbClust 함수로 최적의 군집 수 찾기
view(card2.z)
nc <- NbClust(card2.z, min.nc = 2, max.nc = 10, method = "kmeans")

par(mfrow=c(1,1))
barplot(table(nc$Best.n[1,]),
        xlab="Number of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen")

#Elbow Method로 찾기
## find the optimal number of clusters using Total within-cluster sum of squares
fviz_nbclust(card2.z, kmeans, method = "wss", k.max = 10) + 
  theme_minimal() + 
  ggtitle("Elbow Method")


#실루엣 이용
fviz_nbclust(card2.z, kmeans, method = "silhouette", k.max = 10) + 
  theme_minimal() + 
  ggtitle("Silhouette Method")


card2_km = kmeans(card2.z, centers = 4)
sil = silhouette(card2_km$cluster, dist(card2.z))
fviz_silhouette(sil)


fviz_cluster(card2_km, data = card2.z, geom = "text")+
  theme(legend.position = "none")

sm <-card2_km$centers %>%
  t()  %>%  
  kable("pandoc", digits = 3)
sm

card2_km$cluster
