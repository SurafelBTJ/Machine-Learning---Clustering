

#Performance comparison metrics (silhouette)
sil.depth.hc<-silhouette(sub_grp_depth,dist(train.depth)^2)
sil.magnitude.hc<-silhouette(sub_grp_magnitude,dist(train.magnitude$magnitude)^2)

par(mfrow=c(1,2))
fviz_silhouette(sil.depth.hc,palette = "jco",
                ggtheme = theme_minimal())
fviz_silhouette(sil.magnitude.hc,palette = "jco",
                ggtheme = theme_minimal())



#visualize the silhoutte score of each cluster
sil.depth.som<-silhouette(ide.group.depth,dist(train.depth)^2)
sil.magnitude.som<-silhouette(ide.group.magnitude,dist(train.magnitude$magnitude)^2)

par(mfrow=c(1,2))
fviz_silhouette(sil.depth.som,palette = "jco",
                ggtheme = theme_minimal())
fviz_silhouette(sil.magnitude.som,palette = "jco",
                ggtheme = theme_minimal())




sil.depth.km<-silhouette(depth.cluster$cluster,dist(train.depth)^2)
sil.magnitude.km<-silhouette(magnitude.cluster$cluster,dist(train.magnitude$magnitude)^2)
par(mfrow=c(1,2))

fviz_silhouette(sil.depth.km,palette = "jco",
                ggtheme = theme_minimal())
fviz_silhouette(sil.magnitude.km,palette = "jco",
                ggtheme = theme_minimal())

