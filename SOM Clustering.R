

#Import earthquake dataset 
earthquakm<-read.csv(choose.files(),header = T)
head(earthquakm)


#Extract magnitude and depth of the data set 
set.seed(100)
train.depth<-earthquakm[,c(5,6,8)]
train.magnitude<-earthquakm[,c(5,6,7)]
cor<-earthquakm[,c(5,6,7,8)]

#Correltion analysis
par(mfrow=c(1,2))
corrplot(cor(cor), type = "upper", method = "number", tl.cex = 0.9,main="\n    Correlation coefficient among the four variables")


#scaling the magnitude and depth of the eqrthquake 
scaled.depth<-scale(train.depth,center=T,scale=T)
scaled.magnitude<-scale(train.magnitude,center = T,scale=T)

#inisialize the number of nodes of SOM 
som_grid_depth<-somgrid(xdim = 15,ydim=15,topo = "hexagonal")
som_grid_magnitude<-somgrid(xdim = 16,ydim=14,topo = "hexagonal")


set.seed(100)
som_model_depth<-som(scaled.depth, grid=som_grid_depth, rlen=6000, 
                     alpha=c(0.05,0.01))
som_model_magnitude<-som(scaled.magnitude, grid=som_grid_depth, rlen=6000, 
                         alpha=c(0.05,0.01))



#training progress
plot(som_model_depth,type="changes")
plot(som_model_magnitude,type = "changes")

#Node counts
degrade.bleu <- function(n){
  return(rgb(0,0,1,alpha=seq(0,1,2/n)))
}

par(mfrow=c(1,2))
plot(som_model_depth,type = "count",palette.name = degrade.bleu)
plot(som_model_magnitude,type = "count",palette.name = degrade.bleu)

#
nb.depth<-table(som_model_depth$unit.classif)
nb.magnitude<-table(som_model_magnitude$unit.classif)

#Neighbour Distance
par(mfrow=c(1,2))
plot(som_model_depth,type = "dist.neighbours")
plot(som_model_magnitude,type="dist.neighbours")


#Code books chart/ Vector weight 
par(mfrow=c(1,2))
plot(som_model_depth,type = "codes",codeRendering = "segments")
plot(som_model_magnitude,type = "codes",codeRendering = "segments")




#Areas of high values (red) and low values(blue) for each variable
coolBlueHotRed <- function(n, alpha = 1) {
  rainbow(n, end=4/6, alpha=alpha)[n:1]
}
mydata_depth<-as.data.frame(som_model_depth$codes)
mydata_magnitude<-as.data.frame(som_model_magnitude$codes)

colnames(mydata_depth)<-c("Longitude","Latitude","Depth")
colnames(mydata_magnitude)<-c("Longitude","Latitude","Magnitude")

#plotting the heatmap for each variable
par(mfrow=c(1,3))
for (j in 1:3)
{
  plot(som_model_magnitude,type="property",property=mydata_magnitude[,j],palette.name=coolBlueHotRed,main=colnames(mydata_magnitude)[j],cex=0.5)
}

for (j in 1:3)
{
  plot(som_model_depth,type="property",property=mydata_depth[,j],palette.name=coolBlueHotRed,main=colnames(mydata_depth)[j],cex=0.5)
}




#Finding the number of potential clusters using elbow method
mydata.depth <- som_model_depth$codes[[1]] 
mydata_magnitude <- som_model_magnitude$codes[[1]] 

wss.depth <- (nrow(mydata.depth)-1)*sum(apply(mydata_depth,2,var)) 
wss.magnitude <- (nrow(mydata_magnitude)-1)*sum(apply(mydata_magnitude,2,var)) 

for (i in 2:20) {
  wss.depth[i] <- sum(kmeans(mydata_depth, centers=i)$withinss)
  wss.magnitude[i] <- sum(kmeans(mydata_magnitude, centers=i)$withinss)
  
}

par(mfrow=c(1,2))
grid(plot(wss.depth,type="b"),4,lwd = 3,equilogs = T)
grid(plot(wss.magnitude,type="b"),4,lwd = 3,equilogs = T)

##Use hierarchical clustering to cluster the codebook vectors

# plot these results:
#Clustering of nodes
dc.depth<-dist(som_model_depth$codes[[1]])
dc.magnitude<-dist(som_model_magnitude$codes[[1]])
cah.depth<-hclust(dc.depth,method="ward.D2")
cah.magnitude<-hclust(dc.magnitude,method="ward.D2")

#Ploting the clusters
par(mfrow=c(1,2))
groups.depth<-cutree(cah.depth,k=4)
plot(som_model_depth,type="mapping",bgcol=c("steelblue1","sienna1","yellowgreen","blue")[groups.depth],main="Depth")
add.cluster.boundaries(som_model_depth,clustering=groups.depth)

groups.magnitude<-cutree(cah.magnitude,k=4)
plot(som_model_magnitude,type="mapping",bgcol=c("steelblue1","sienna1","yellowgreen","blue")[groups.magnitude],main="Magnitude")
add.cluster.boundaries(som_model_magnitude,clustering=groups.magnitude)

ide.group.depth<-groups.depth[som_model_depth$unit.classif]
ide.group.magnitude<-groups.depth[som_model_magnitude$unit.classif]



#Visualize the clusters of magnitude and depth in 3-dimensional plot
scatter3D(train.magnitude$longitude, train.magnitude$latitude, train.magnitude$magnitude, pch = 18, bty = "g", colkmy = FALSE, 
          main = "Magnitude cluster by SOM", col.panel ="steelblue", expand =0.4, 
          col.grid = "darkblue",colvar = ide.group.magnitude,xlab="Longitude",ylab="Latitude",zlab="Magnitude",col=c("steelblue1","sienna1","yellowgreen","blue"),ticktype="detailed")
legend("right",legend=levels(as.factor(ide.group.magnitude)),col=c("steelblue1","sienna1","yellowgreen","blue"),pch=18)
plotrgl(lighting=T)

scatter3D(train.depth$longitude, train.depth$latitude, train.depth$depth, pch = 18, bty = "g", colkmy = FALSE, 
          main ="Depth cluster by SOM", col.panel ="steelblue", expand =0.4, 
          col.grid = "darkblue",colvar = ide.group.depth,xlab="Longitude",ylab="Latitude",zlab="Magnitude",col=c("steelblue1","sienna1","yellowgreen","blue"),ticktype="detailed")
legend("right",legend=levels(as.factor(ide.group.depth)),col=c("steelblue1","sienna1","yellowgreen","blue"),pch=18)
plotrgl(lighting=T)