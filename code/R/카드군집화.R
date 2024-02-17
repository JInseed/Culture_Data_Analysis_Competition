rm(list=ls())
library(factoextra)
library(psych)
card=read.csv('C:/Users/82102/Desktop/문화 관광 project/군집화/카드데이터전처리.csv',header=TRUE)

rownames(card)=card[[1]]

card=card %>% 
  select(-지역명)

view(card.z)
view(card)
card[is.na(card)]=0
card=round(card)

card=card[,-c(1,3,5,7,9,11,13,15,17,19,21)]
card=card[,c(1,3,5,7,9,11,13,15,17,19,21)]


card.z=scale(card)
card.z=as.data.frame(card.z)

card.s=sqrt(card+1)
card.s=as.data.frame(card.s)

card.l=log(card+1)
card.l=as.data.frame(card.l)

card.lz=scale(card.l)
card.lz=as.data.frame(card.lz)



head(card)

card.pca=princomp(card.l)
summary(card.pca)
card.pca$loadings

card.fa=principal(card.l, nfactors=11, rotate='varimax')
card.fa



card_pc=princomp(card)

card_pc$loadings

summary(card_pc)





# NbClust 함수로 최적의 군집 수 찾기

nc <- NbClust(card.m, min.nc = 2, max.nc = 20, method = "kmeans")

par(mfrow=c(1,1))
barplot(table(nc$Best.n[1,]),
        xlab="Number of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen")

#Elbow Method로 찾기
## find the optimal number of clusters using Total within-cluster sum of squares
fviz_nbclust(card.m, kmeans, method = "wss", k.max = 20) + 
  theme_minimal() + 
  ggtitle("Elbow Method")


fviz_nbclust(card.l, kmeans, method = "gap_stat", k.max = 20) + 
  theme_minimal() + 
  ggtitle("Elbow Method")


#실루엣 이용
fviz_nbclust(card.z, kmeans, method = "silhouette", k.max = 20) + 
  theme_minimal() + 
  ggtitle("Silhouette Method")

set.seed(1004)
km_2 = kmeans(card, centers = 2)
km_3 = kmeans(card, centers = 3)
km_4 = kmeans(card, centers = 4)
km_5 = kmeans(card, centers = 5)
km_6 = kmeans(card, centers = 6)
km_7 = kmeans(card, centers = 7)

# Visualize silhouhette information
sil_2 = silhouette(km_2$cluster, dist(card))
sil_3 = silhouette(km_3$cluster, dist(card))
sil_4 = silhouette(km_4$cluster, dist(card))
sil_5 = silhouette(km_5$cluster, dist(card))
sil_6 = silhouette(km_6$cluster, dist(card))
sil_7 = silhouette(km_7$cluster, dist(card))

fviz_silhouette(sil_2)
fviz_silhouette(sil_3)
fviz_silhouette(sil_4)
fviz_silhouette(sil_5)
fviz_silhouette(sil_6)
fviz_silhouette(sil_7)


km_10 = kmeans(card.l, centers = 8)
sil_10 = silhouette(km_10$cluster, dist(card.l))
fviz_silhouette(sil_10)



#군집화
card_km=kmeans(card.z, centers = 5, nstart=25)

table(card_km$cluster)

par(mfrow=c(1,1))

#clusplot####
clusplot(card, clus =card_km$cluster , #case별 기준점
         color = T, #색
         shade = T, #빗금
         labels = 2, #군집의 개별 케이스 행이름은 2번으로, 1 이름 없음.
         lines = 0, #군집간 거리 삭제
         main="kmeans cluster Plot"
)


fviz_cluster(card_km, data = card.z, geom = "text")+theme(legend.position = "none")
#+ggtitle()



#속성파악
#property raw data
aggregate(card,by=list(cluster=card_km$cluster),mean)

#이미 계산된 표준화
aggregate(card_z,by=list(cluster=card.z_km$cluster),mean)

card_km$centers # standardized


#속성파악을 위한 정리 
sm <-card_km$centers %>%
  t()  %>%  
  kable("pandoc", digits = 3)
sm
# standardized
state.vari <-rownames(sm)
colnames(sm)<- c("C1","C2",'C3')
sm<-sm %>%as.data.frame() 

smd<-sm %>% 
  mutate(C1s=ifelse(C1>0.3,"*","")) %>% 
  mutate(C2s=ifelse(C2>0.3,"*","")) %>% 
  mutate(C3s=ifelse(C3>0.3,"*",""))

smd<-cbind(state.vari, smd)
#최종 자료 분석 정리 
smd[,c(1,4,2,5,3,6)] %>% kable("pandoc",digits = 3,caption = "state.x77의 군집분석후 군의 속성")