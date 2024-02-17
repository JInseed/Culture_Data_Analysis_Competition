rm(list=ls())
install.packages("NbClust")
install.packages("caret")
install.packages("factoextra")
install.packages('kableExtra')

library(NbClust)
library(caret)
library(tidyverse)
library(cluster)
library(factoextra)
library(kableExtra)


#원데이터 및 비율
arch=read.csv('C:/Users/82102/Desktop/문화 관광 project/군집화/관광지역피봇.csv',header=TRUE)

view(arch)

rownames(arch)=arch[[1]]

arch=arch %>% 
  select(-지역명)

arch.z=scale(arch)
arch.z=as.data.frame(arch.z)

arch.l=log(arch+1)
arch.l=as.data.frame(arch.l)

arch.lz=scale(arch.l)
arch.lz=as.data.frame(arch.lz)


# NbClust 함수로 최적의 군집 수 찾기

nc <- NbClust(arch, min.nc = 2, max.nc = 20, method = "kmeans")

par(mfrow=c(1,1))
barplot(table(nc$Best.n[1,]),
        xlab="Number of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen")

#Elbow Method로 찾기
## find the optimal number of clusters using Total within-cluster sum of squares
fviz_nbclust(arch.z, kmeans, method = "wss", k.max = 20) + 
  theme_minimal() + 
  ggtitle("Elbow Method")


#실루엣 이용
fviz_nbclust(arch.z, kmeans, method = "silhouette", k.max = 20) + 
  theme_minimal() + 
  ggtitle("Silhouette Method")

set.seed(1004)
km_2 = kmeans(arch, centers = 2)
km_3 = kmeans(arch, centers = 3)
km_4 = kmeans(arch, centers = 4)
km_5 = kmeans(arch, centers = 5)
km_6 = kmeans(arch, centers = 6)
km_7 = kmeans(arch, centers = 7)

# Visualize silhouhette information
sil_2 = silhouette(km_2$cluster, dist(arch))
sil_3 = silhouette(km_3$cluster, dist(arch))
sil_4 = silhouette(km_4$cluster, dist(arch))
sil_5 = silhouette(km_5$cluster, dist(arch))
sil_6 = silhouette(km_6$cluster, dist(arch))
sil_7 = silhouette(km_7$cluster, dist(arch))

fviz_silhouette(sil_2)
fviz_silhouette(sil_3)
fviz_silhouette(sil_4)
fviz_silhouette(sil_5)
fviz_silhouette(sil_6)
fviz_silhouette(sil_7)


km_10 = kmeans(arch.z, centers = 4)
sil_10 = silhouette(km_10$cluster, dist(arch.z))
fviz_silhouette(sil_10)



#군집화
arch_km=kmeans(arch.z, centers = 4, nstart=25)

table(arch_km$cluster)

par(mfrow=c(1,1))

#clusplot####
clusplot(arch.m, clus =km$cluster , #case별 기준점
         color = T, #색
         shade = T, #빗금
         labels = 2, #군집의 개별 케이스 행이름은 2번으로, 1 이름 없음.
         lines = 0, #군집간 거리 삭제
         main="kmeans cluster Plot"
)


fviz_cluster(arch_km, data = arch.z, geom = "text")+theme(legend.position = "none")
#+ggtitle()



#속성파악
#property raw data
aggregate(arch,by=list(cluster=arch_km$cluster),mean)

#이미 계산된 표준화
aggregate(arch_z,by=list(cluster=arch.z_km$cluster),mean)

arch_km$centers # standardized


#속성파악을 위한 정리 
sm <-arch_km$centers %>%
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

ggplot(data=arch.z)+
  geom_point(aes(역사관광지, 자연관광지),col=arch_km$cluster)

