
#Performance comparison using SSE (Sum of Square Error)
clusters.data<-data.frame(Longitude=train.depth$longitude,Latitude=train.depth$latitude,Depth=train.depth$depth,Magnitude=train.magnitude$magnitude,SOM.depth=ide.group.depth,som.magnitude=ide.group.magnitude,km.depth=depth.cluster$cluster,km.magnitude=magnitude.cluster$cluster,Hi.depth=sub_grp_depth,hi.mag=sub_grp_magnitude)
cluster.data<-as.data.frame(clusters.data)



#Calcualting SSE for each cluster
sse<- function(data)
{
  for (i in 1:4)
  {
    if (i==1)
    {
      #extract columns ( magnitude and depth) in cluster 1
      som.mag.1<-select(filter(data,som.magnitude==i),Magnitude)
      som.dep.1<-select(filter(data,SOM.depth==i),Depth)
      
      km.mag.1<-select(filter(data,km.magnitude==i),Magnitude)
      km.dep.1<-select(filter(data,km.depth==i),Depth)
      
      hi.mag.1<-select(filter(data,hi.mag==i),Magnitude)
      hi.dep.1<-select(filter(data,Hi.depth==i),Depth)
      
      #calculate sse for each columns in each cluster 1 
      sse.sommag1<-sum(((som.mag.1-mean(som.mag.1$Magnitude)))^2)
      sse.somdep1<-sum(((som.dep.1-mean(som.dep.1$Depth)))^2)
      
      sse.kmmag1<-sum(((km.mag.1-mean(km.mag.1$Magnitude)))^2)
      sse.kmdep1<-sum(((km.dep.1-mean(km.dep.1$Depth)))^2)
      
      sse.himag1<-sum(((hi.mag.1-mean(hi.mag.1$Magnitude)))^2)
      sse.hidep1<-sum(((hi.dep.1-mean(hi.dep.1$Depth)))^2)
      
    }
    if(i==2)
    {
      #extract depth and magnitude in cluster 2
      som.mag.2=select(filter(data,som.magnitude==i),Magnitude)
      som.dep.2=select(filter(data,SOM.depth==i),Depth)
      km.mag.2<-select(filter(data,km.magnitude==i),Magnitude)
      km.dep.2<-select(filter(data,km.depth==i),Depth)
      hi.mag.2<-select(filter(data,hi.mag==i),Magnitude)
      hi.dep.2<-select(filter(data,Hi.depth==i),Depth)
      
      #calculate sse for each columns in cluster 2 
      sse.sommag2<-sum(((som.mag.2-mean(som.mag.2$Magnitude)))^2)
      sse.somdep2<-sum(((som.dep.2-mean(som.dep.2$Depth)))^2)
      
      sse.kmmag2<-sum(((km.mag.2-mean(km.mag.2$Magnitude)))^2)
      sse.kmdep2<-sum(((km.dep.2-mean(km.dep.2$Depth)))^2)
      
      sse.himag2<-sum(((hi.mag.2-mean(hi.mag.2$Magnitude)))^2)
      sse.hidep2<-sum(((hi.dep.2-mean(hi.dep.2$Depth)))^2)
      
    }
    if (i==3)
    {
      #extract columns mangitude and depth in cluster 3
      som.mag.3=select(filter(data,som.magnitude==i),Magnitude)
      som.dep.3=select(filter(data,SOM.depth==i),Depth)
      
      km.mag.3<-select(filter(data,km.magnitude==i),Magnitude)
      km.dep.3<-select(filter(data,km.depth==i),Depth)
      
      hi.mag.3<-select(filter(data,hi.mag==i),Magnitude)
      hi.dep.3<-select(filter(data,Hi.depth==i),Depth)
      
      #calculate sse for each columns in each cluster 3 
      sse.sommag3<-sum(((som.mag.3-mean(som.mag.3$Magnitude)))^2)
      sse.somdep3<-sum(((som.dep.3-mean(som.dep.3$Depth)))^2)
      
      sse.kmmag3<-sum(((km.mag.3-mean(km.mag.3$Magnitude)))^2)
      sse.kmdep3<-sum(((km.dep.3-mean(km.dep.3$Depth)))^2)
      
      sse.himag3<-sum(((hi.mag.3-mean(hi.mag.3$Magnitude)))^2)
      sse.hidep3<-sum(((hi.dep.3-mean(hi.dep.3$Depth)))^2)
      
    }
    if(i==4)
    {
      #extract columns magnitude and depth in cluster 4
      som.mag.4=select(filter(data,som.magnitude==i),Magnitude)
      som.dep.4=select(filter(data,SOM.depth==i),Depth)
      
      km.mag.4<-select(filter(data,km.magnitude==i),Magnitude)
      km.dep.4<-select(filter(data,km.depth==i),Depth)
      
      hi.mag.4<-select(filter(data,hi.mag==i),Magnitude)
      hi.dep.4<-select(filter(data,Hi.depth==i),Depth)
      
      #calculate sse for each columns in each cluster 4
      sse.sommag4<-sum(((som.mag.4-mean(som.mag.4$Magnitude)))^2)
      sse.somdep4<-sum(((som.dep.4-mean(som.dep.4$Depth)))^2)
      
      sse.kmmag4<-sum(((km.mag.4-mean(km.mag.4$Magnitude)))^2)
      sse.kmdep4<-sum(((km.dep.4-mean(km.dep.4$Depth)))^2)
      
      sse.himag4<-sum(((hi.mag.4-mean(hi.mag.4$Magnitude)))^2)
      sse.hidep4<-sum(((hi.dep.4-mean(hi.dep.4$Depth)))^2)
      
    }
  }
  sse.som.mag<-(sse.sommag1+sse.sommag2+sse.sommag3+sse.sommag4)/4
  sse.km.mag<-(sse.kmmag1+sse.kmmag2+sse.kmmag3+sse.kmmag4)/4
  sse.hi.mag<-(sse.himag1+sse.himag2+sse.himag3+sse.himag4)/4
  
  sse.som.dep<-(sse.somdep1+sse.somdep2+sse.somdep3+sse.somdep4)/4
  sse.km.dep<-(sse.kmdep1+sse.kmdep2+sse.kmdep3+sse.kmdep4)/4 
  sse.hi.dep<-(sse.hidep1+sse.hidep2+sse.hidep3+sse.hidep4)/4
  
  sse.data<-data.frame(Magnitude=c(sse.som.mag,sse.km.mag,sse.hi.mag),Depth=c(sse.som.dep,sse.km.dep,sse.hi.dep))
  knitr::kable(sse.data)
}

sse(clusters.data)
