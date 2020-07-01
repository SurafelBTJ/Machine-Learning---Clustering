#Aglomerative hirarchical clustering
hc.depth<-hclust(dist(train.depth),method = "ward.D2")
hc.magnitude<-hclust(dist(train.magnitude),method = "ward.D2")

#Cut the depth of the tree (dendogram plot)
sub_grp_depth<-cutree(hc.depth,k=4)
sub_grp_magnitude<-cutree(hc.magnitude,k=4)

#Visualize the dendogram
plot(hc.depth,cex=0.6,xlab="Features",sub="",main="Depth cluster by hierarchical clustering")
rect.hclust(hc.depth,k=4,border = 2:5)

plot(hc.magnitude,cex=0.6,xlab="Features",main="Magnitude cluster by hierarchical clustering",sub="")
rect.hclust(hc.magnitude,k=4,border = 2:5)

fviz_dend(hc.magnitude, k = 4, k_colors = "jco",  
          as.ggplot = TRUE, show_labels = FALSE)




#Visualize the clusters in 3-D plot
scatter3D(train.magnitude$longitude, train.magnitude$latitude,sub_grp_magnitude, pch = 18, bty = "g", colkmy = FALSE, 
          main ="Magnitude Cluster by Hyerarchical Clustering ",col=c("steelblue1","sienna1","yellowgreen","blue"), col.panel ="steelblue", expand =0.4, 
          col.grid = "darkblue",colvar = sub_grp_magnitude,xlab="Longitude",ylab="Latitude",zlab="Magnitude",color=colours,ticktype="detailed")
legend("right",legend=levels(as.factor(sub_grp_magnitude)),col=c("steelblue1","sienna1","yellowgreen","red"),pch=18)
plotrgl(lighting=T)

scatter3D(train.depth$longitude, train.depth$latitude, sub_grp_depth, pch = 18, bty = "g", colkmy = FALSE, 
          main ="Depth Cluster by Hierarchical Clustering ",col=c("steelblue1","sienna1","yellowgreen","blue"), col.panel ="steelblue", expand =0.4, 
          col.grid = "darkblue",colvar = sub_grp_depth,xlab="Longitude",ylab="Latitude",zlab="Magnitude",color=colours,ticktype="detailed")
legend("right",legend=levels(as.factor(sub_grp_depth)),col=c("steelblue1","sienna1","yellowgreen","red"),pch=18)
plotrgl(lighting=T)
