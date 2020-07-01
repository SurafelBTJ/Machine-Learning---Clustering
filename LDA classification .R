
#Create a dataframe of magnitude and depth for each cluster
class.km.magnitude<-data.frame(Latitude=as.double(),Longitude=as.double(),Magnitude=as.double(),Class=as.numeric())
class.km.depth<-data.frame(Latitude=as.double(),Longitude=as.double(),Depth=as.double(),Class=as.numeric())


#extract columns ( magnitude and depth) in cluster 1 and label the columns as a class 1
km.mag1<-select(filter(clusters.data,Latitude,Longitude,Magnitude,km.magnitude==1),Latitude,Longitude,Magnitude,km.magnitude)
km.dep1<-dplyr::select(filter(clusters.data,Latitude,Longitude,Depth,km.depth==1),Latitude,Longitude,Depth,km.depth)


km.mag2<-select(filter(clusters.data,Latitude,Longitude,Magnitude,km.magnitude==2),Latitude,Longitude,Magnitude,km.magnitude)
km.dep2<-select(filter(clusters.data,Latitude,Longitude,Depth,km.depth==2),Latitude,Longitude,Depth,km.depth)


km.mag3<-select(filter(clusters.data,Latitude,Longitude,Magnitude,km.magnitude==3),Latitude,Longitude,Magnitude,km.magnitude)
km.dep3<-select(filter(clusters.data,Latitude,Longitude,Depth,km.depth==3),Latitude,Longitude,Depth,km.depth)


km.mag4<-select(filter(clusters.data,Latitude,Longitude,Magnitude,km.magnitude==4),Latitude,Longitude,Magnitude,km.magnitude)
km.dep4<-select(filter(clusters.data,Latitude,Longitude,Depth,km.depth==4),Latitude,Longitude,Depth,km.depth)



#combine the dataframe of clusters obtained above
class.km.magnitude<-rbind(class.km.magnitude,km.mag1,km.mag2,km.mag3,km.mag4)
class.km.depth<-rbind(class.km.depth,km.dep1,km.dep2,km.dep3,km.dep4)

#Rename column names
colnames(class.km.depth)<-c("Latitude","Longitude","Magnitude","Class")
colnames(class.km.magnitude)<-c("Latitude","Longitude","Depth","Class")


km.depth<-class.km.depth[sample(nrow(class.km.depth), nrow(class.km.depth)),]
km.magnitude<-class.km.magnitude[sample(nrow(class.km.magnitude), nrow(class.km.magnitude)),]
km.depth$Class<-as.factor(km.depth$Class)
km.magnitude$Class<-as.factor(km.magnitude$Class)
print(km.depth)
print(km.magnitude)

#Apply linear descriminant analysis to classify the clusters 
r = runif(3290)
All = 1:3290
test = All[rank(r) > 2000]
training=All[rank(r)<=2000]
train.depth = km.depth[training,]
test.depth = km.depth[test,]

train.magnitude = km.magnitude[training,]
test.magnitude = km.magnitude[test,]

fit.lda.dep<-lda(Class~.,data=train.depth)
pred.lda.dep<-predict(fit.lda,newdata = km.depth[test,-4])
conf.dep<-table(pred.lda.dep[[1]],km.depth[test,4])

fit.lda.mag<-lda(Class~.,data=train.magnitude)
pred.mag.dep<-predict(fit.lda.mag,newdata = km.magnitude[test,-4])
conf.mag<-table(Predicted=pred.mag.dep[[1]],Actual=km.magnitude[test,4])
knitr::kable(conf.mag)

#predict the classes
pred<-prediction(predict(fit.lda.mag,km.magnitude[test,4]))

