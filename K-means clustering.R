
#Determining the potential number of potential clusters using elbow method
wss.depth <- (nrow(mydata.depth)-1)*sum(apply(mydata_depth,2,var)) 
wss.magnitude <- (nrow(mydata_magnitude)-1)*sum(apply(mydata_magnitude,2,var)) 

for (i in 2:20) {
  wss.depth[i] <- sum(kmeans(mydata_depth, centers=i)$withinss)
  wss.magnitude[i] <- sum(kmeans(mydata_magnitude, centers=i)$withinss)
  
}

par(mfrow=c(1,2))
grid(plot(wss.depth,type="b"),4,lwd = 3,equilogs = T)
grid(plot(wss.magnitude,type="b"),4,lwd = 3,equilogs = T)

#Clustering earthquake depth and magnitude using K-means clustering
depth.cluster<-kmeans(train.depth,4,nstart = 20)
magnitude.cluster<-kmeans(train.magnitude,4,nstart = 20)




#visualize the result in 3-D plot
scatter3D(train.magnitude$longitude, train.magnitude$latitude, train.magnitude$magnitude, pch = 18, bty = "g", colkmy = FALSE, 
          main ="Magnitude cluster by K-means",col=c("steelblue1","sienna1","yellowgreen","blue"), col.panel ="steelblue", expand =0.4, 
          col.grid = "darkblue",colvar = magnitude.cluster$cluster,xlab="Longitude",ylab="Latitude",zlab="Magnitude",color=colours,ticktype="detailed")
legend("right",legend=levels(as.factor(magnitude.cluster$cluster)),col=c("steelblue1","sienna1","yellowgreen","blue"),pch=18)

scatter3D(train.depth$longitude, train.depth$latitude, train.depth$depth, pch = 18, bty = "g", colkmy = FALSE, 
          main ="Depth cluster by k-means",col=c("steelblue1","sienna1","yellowgreen","blue"), col.panel ="steelblue", expand =0.4, 
          col.grid = "darkblue",colvar = depth.cluster$cluster,xlab="Longitude",ylab="Latitude",zlab="Magnitude",color=colours,ticktype="detailed")
legend("right",legend=levels(as.factor(depth.cluster$cluster)),col=c("steelblue1","sienna1","yellowgreen","blue"),pch=18)
plotrgl(lighting=T)

