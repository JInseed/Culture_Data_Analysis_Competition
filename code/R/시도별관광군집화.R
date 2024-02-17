library(tidyverse)
library(NbClust)
library(caret)
library(cluster)
library(factoextra)
library(kableExtra)
#시도별 관광타입

df=read.csv('C:/Users/82102/Desktop/문화 관광 project/군집화/관광지역피봇.csv',header=TRUE)

arch=df %>% 
  mutate(지역명=str_split(지역명,' ',simplify=TRUE)[,1]) %>% 
  group_by(지역명) %>% 
  summarise(건축.조형물=sum(건축.조형물),
              레포츠=sum(레포츠),
              문화체험=sum(문화체험),
              산업관광지=sum(산업관광지),
              쇼핑=sum(쇼핑),
              숙박시설=sum(숙박시설),
              역사관광지=sum(역사관광지),
              음식점=sum(음식점),
              자연관광지=sum(자연관광지),
              체험관광지=sum(체험관광지),
              휴양관광지=sum(휴양관광지)) %>% 
  as.data.frame()


rownames(arch)=arch[[1]]

arch=arch %>% 
  select(-지역명)

arch.z=scale(arch)
arch.z=as.data.frame(arch.z)

arch.l=log(arch+1)
arch.l=as.data.frame(arch.l)

arch.lz=scale(arch.l)
arch.lz=as.data.frame(arch.lz)

arch.s=sqrt(arch)
arch.s=as.data.frame(arch.s)

# NbClust 함수로 최적의 군집 수 찾기

nc <- NbClust(arch, min.nc = 2, max.nc = 6, method = "kmeans")
view(arch)
par(mfrow=c(1,1))
barplot(table(nc$Best.n[1,]),
        xlab="Number of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen")

#Elbow Method로 찾기
## find the optimal number of clusters using Total within-cluster sum of squares
fviz_nbclust(arch, kmeans, method = "wss", k.max = 10) + 
  theme_minimal() + 
  ggtitle("Elbow Method")


#실루엣 이용
fviz_nbclust(arch, kmeans, method = "silhouette", k.max = 10) +
  theme_minimal() + 
  ggtitle("Silhouette Method")

arch_km = kmeans(arch, centers = 4)
sil = silhouette(arch_km$cluster, dist(arch))
fviz_silhouette(sil)

fviz_cluster(arch_km, data = arch, geom = "text")+
  theme(legend.position = "none")

sm <-arch_km$centers %>%
  t()  %>%  
  kable("pandoc", digits = 3)
sm

arch_km$cluster