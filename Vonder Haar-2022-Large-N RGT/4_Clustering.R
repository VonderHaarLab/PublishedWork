
##Get subset with no interventions and at "stable" time point (10+ ses, post-injury & 15+ session pre-inj):
#Counts by session/subject	

temp<-Choice[,c("SbjID", "Session_norm", "Pre.Injury", "Injury", "Study_ID", "ChoiceOption", "ChoiceCount", "TotChoice")]
temp<-spread(temp, ChoiceOption, ChoiceCount)
colnames(temp)[7:10]<-c("P1","P2","P3","P4")
ChoiceClust<-temp
temp<-AllVars[,c("SbjID", "Session_norm", "Pre.Injury", "Injury", "Score")]
ChoiceClust<-merge(ChoiceClust, temp, by=c("SbjID", "Session_norm", "Pre.Injury", "Injury"))
	ChoiceClustLong<-gather(ChoiceClust, "Choice", "Count", P1:P4, factor_key = T)
	ChoiceClustLong$Pct<-ChoiceClustLong$Count/ChoiceClustLong$TotChoice


#ShamChoiceClust<-subset(ChoiceClust, Injury=="Sham")
#TBIChoiceClust<-subset(ChoiceClust, Injury=="TBI")


library(factoextra)
library(latticeExtra)


##Calculate what % choice would be based on rate of reinforcement.
	#A reasonably exploratory rat would collapse on these values
	#An exploitative rat would center in on highest value
	RateDF<-data.frame(
		Choice=c("P1","P2","P3","P4"),
		ExpectedVal=c(9.82/31.34, 13.71/31.34, 4.5/31.34, 3.31/31.34))
	ChoiceClustLong<-merge(ChoiceClustLong, RateDF, by="Choice", all=T)
	ChoiceClustLong$SS<-(ChoiceClustLong$Pct-ChoiceClustLong$ExpectedVal)^2
		#Grab fit for sorting
	temp<-aggregate(SS~SbjID+Injury+Pre.Injury+Study_ID, data=ChoiceClustLong, FUN=sum)
	colnames(temp)[colnames(temp)=="SS"]<-"SStot"
	ChoiceClustLong<-merge(ChoiceClustLong, temp, by=c("SbjID","Injury","Pre.Injury","Study_ID"), all=T)


#Plot of all subjects overall choice vs rates
	ChoiceClustAvg<-aggregate(cbind(Pct, ExpectedVal, SStot)~Choice+SbjID+Injury+Pre.Injury+Study_ID, data=ChoiceClustLong, FUN=mean)
	ChoiceClustAvg<-subset(ChoiceClustAvg, !((Study_ID=="CCI-A_SS" | Study_ID=="HFD-A") & Pre.Injury=="Pre"))
	ChoiceClustAvg$SbjID<-factor(ChoiceClustAvg$SbjID, levels=unique(ChoiceClustAvg$SbjID[rev(order(ChoiceClustAvg$SStot))]))
	barchart(Pct~Choice|SbjID, data=ChoiceClustAvg, main="Individual Choice Profiles", ylim=c(0,1), strip=F)+
		as.layer(xyplot(ExpectedVal~Choice|SbjID, data=ChoiceClustAvg, type="a", lwd=3))




#Sham
#Clustering on aggregate data. 
	#Explore by manipulating cluster number in syntax.
temp<-subset(spread(ChoiceClustAvg[,-c(7:8)], Choice, Pct), Injury=="Sham")
temp<-temp[,-c(1:4)]
clust_number = 4
	#adjust number for different clustering
Clusters<-kmeans(temp, clust_number, iter.max=200, nstart=30)
fviz_cluster(Clusters, data=temp)
temp<-cbind(subset(spread(ChoiceClustAvg[,-c(7:8)], Choice, Pct), Injury=="Sham"), cluster=Clusters$cluster)
	temp %>% count(cluster)/length(temp$SbjID)
temp<-gather(temp, "Choice", "Pct", P1:P4, factor_key = T)
#histogram(~Pct|as.factor(cluster)*Choice, data=temp)
xyplot(Pct~Choice|as.factor(cluster), group=SbjID, data=temp, type=c("p","a"), lwd=2, cex=1.1)+
	as.layer(xyplot(Pct~Choice|as.factor(cluster), data=temp, type=c("p","a"), lwd=7, cex=1.4))
#get data for future use
	ChoicePhenotypesSham<-temp


#Objective tests for cluster numbers
temp<-subset(spread(ChoiceClustAvg[,-c(7:8)], Choice, Pct), Injury=="Sham")
temp<-temp[,-c(1:4)]
fviz_nbclust(temp, kmeans, method = "wss", iter.max=200, nstart=30)
fviz_nbclust(temp, kmeans, method = "gap_stat", iter.max=200, nstart=30)
	#6-7+ clusters result in a cluster with <5% of data




#TBI
#Clustering on aggregate data. 
	#Explore by manipulating cluster number in syntax.
temp<-subset(spread(ChoiceClustAvg[,-c(7:8)], Choice, Pct), Injury=="TBI")
temp<-temp[,-c(1:4)]
clust_number = 7
	#adjust number for different clustering
Clusters<-kmeans(temp, clust_number, iter.max=200, nstart=30)
fviz_cluster(Clusters, data=temp)
temp<-cbind(subset(spread(ChoiceClustAvg[,-c(7:8)], Choice, Pct), Injury=="TBI"), cluster=Clusters$cluster)
	temp %>% count(cluster)/length(temp$SbjID)
temp<-gather(temp, "Choice", "Pct", P1:P4, factor_key = T)
#histogram(~Pct|as.factor(cluster)*Choice, data=temp)
xyplot(Pct~Choice|as.factor(cluster), group=SbjID, data=temp, type=c("p","a"), lwd=2, cex=1.1)+
	as.layer(xyplot(Pct~Choice|as.factor(cluster), data=temp, type=c("p","a"), lwd=7, cex=1.4))
#get data for future use
	ChoicePhenotypesTBI<-temp


#Objective tests for cluster numbers
temp<-subset(spread(ChoiceClustAvg[,-c(7:8)], Choice, Pct), Injury=="TBI")
temp<-temp[,-c(1:4)]
fviz_nbclust(temp, kmeans, method = "wss", iter.max=200, nstart=30)
fviz_nbclust(temp, kmeans, method = "gap_stat", iter.max=200, nstart=30)
	#8+ clusters result in a cluster with <5% of data
fviz_nbclust(temp, kmeans, method = "gap_stat", iter.max=200, nstart=30, k.max=7)



#All
#Clustering on aggregate data. 
	#Explore by manipulating cluster number in syntax.
temp<-subset(spread(ChoiceClustAvg[,-c(7:8)], Choice, Pct), )
temp<-temp[,-c(1:4)]
clust_number = 5
	#adjust number for different clustering
Clusters<-kmeans(temp, clust_number, iter.max=200, nstart=30)
fviz_cluster(Clusters, data=temp)
temp<-cbind(subset(spread(ChoiceClustAvg[,-c(7:8)], Choice, Pct), ), cluster=Clusters$cluster)
	temp %>% count(cluster)/length(temp$SbjID)
	temp %>% count(cluster, Injury)
	#3/51 = 5% for TBI
	#3/59 = 5% for Sham
temp<-gather(temp, "Choice", "Pct", P1:P4, factor_key = T)
#histogram(~Pct|as.factor(cluster)*Choice, data=temp)
xyplot(Pct~Choice|as.factor(cluster), group=SbjID, data=temp, type=c("p","a"), lwd=2, cex=1.1)+
	as.layer(xyplot(Pct~Choice|as.factor(cluster), data=temp, type=c("p","a"), lwd=7, cex=1.4))
xyplot(Pct~Choice|as.factor(cluster), group=Injury, data=temp, type=c("p","a"), lwd=4, cex=1.1)
#get data for future use
	ChoicePhenotypes<-temp

#Objective tests for cluster numbers
temp<-subset(spread(ChoiceClustAvg[,-c(7:8)], Choice, Pct))
temp<-temp[,-c(1:4)]
fviz_nbclust(temp, kmeans, method = "wss", iter.max=200, nstart=30)
fviz_nbclust(temp, kmeans, method = "gap_stat", iter.max=200, nstart=30)
	#5+ clusters result in a cluster with <5% of data
	#6+ clusters result in cluster with <5% of data for a given group (sham)





##CHECK cluster numbers as they can shift each run
ChoicePhenotypes$cluster[ChoicePhenotypes$cluster==1]<-"Optimal(P2 Pref)"
ChoicePhenotypes$cluster[ChoicePhenotypes$cluster==4]<-"Exploratory"
ChoicePhenotypes$cluster[ChoicePhenotypes$cluster==2]<-"Indeterminate (P1 Pref)"
ChoicePhenotypes$cluster[ChoicePhenotypes$cluster==3]<-"Risky (P3 Pref)"
ChoicePhenotypes$cluster[ChoicePhenotypes$cluster==5]<-"Risky (P4 Pref)"
ChoicePhenotypes$cluster<-factor(ChoicePhenotypes$cluster, levels=unique(ChoicePhenotypes$cluster[rev(order(ChoicePhenotypes$cluster))]))
xyplot(Pct~Choice|as.factor(cluster), group=Injury, data=ChoicePhenotypes, type=c("p","a"), lwd=4, cex=1.1)

library(plotrix)
summary(aov(Pct~Choice*Injury, data=subset(ChoicePhenotypes, cluster=="Optimal(P2 Pref)")))
summary(aov(Pct~Choice*Injury, data=subset(ChoicePhenotypes, cluster=="Exploratory")))
summary(aov(Pct~Choice*Injury, data=subset(ChoicePhenotypes, cluster=="Risky (P3 Pref)")))
summary(aov(Pct~Choice*Injury, data=subset(ChoicePhenotypes, cluster=="Risky (P4 Pref)")))
temp<-aggregate(Pct~Choice+Injury+cluster, data=ChoicePhenotypes, FUN=sd)
aggregate(Pct*100~Injury+cluster, data=temp, FUN=sum)
temp<-merge(
	aggregate(Pct*100~Injury+cluster+Choice, data=ChoicePhenotypes, FUN=mean),
	aggregate(Pct*100~Injury+cluster+Choice, data=ChoicePhenotypes, FUN=std.error),
	by=c("Injury", "cluster", "Choice"))
colnames(temp)[colnames(temp)=="Pct * 100.x"]<-"Mean"
colnames(temp)[colnames(temp)=="Pct * 100.y"]<-"Error"
write.table(temp, "Phenotypes.csv", sep=",", col.names=NA)
temp<-spread(Choice, Pct, data=ChoicePhenotypes)
write.table(temp, "Phenotypes.csv", sep=",", col.names=NA, append=T)
temp<-spread(Choice, Pct, data=ChoicePhenotypes)
fisher.test(temp$Injury, temp$cluster)
temp<-temp %>% count(cluster, Injury)
write.table(temp, "Phenotypes.csv", sep=",", col.names=NA, append=T)


write.table(aggregate(Pct~SbjID+Injury+cluster, data=ChoicePhenotypes, FUN=sum), "PhenoSubjects.csv", sep=",", col.names=NA)




#For cluster graphing:
temp<-subset(spread(ChoiceClustAvg[,-c(7:8)], Choice, Pct), )
temp<-temp[,-c(1:4)]
clust_number = 5
	#adjust number for different clustering
Clusters<-kmeans(temp, clust_number, iter.max=200, nstart=30)

#Check these because they change each time:
	#use plots to figure which is which
		#P3 & P4 oppose each other for the two risky clusters
		fviz_cluster(Clusters, data=temp, choose.vars=c("P3","P4"))
			Clusters$cluster[Clusters$cluster==5]<-"Risky (P4)"
			Clusters$cluster[Clusters$cluster==3]<-"Risky (P3)"
		#Optimal clusters high on P2, exploratory the spread out one, and indiscriminant the tiny one
		fviz_cluster(Clusters, data=temp, choose.vars=c("P1","P2"))
			Clusters$cluster[Clusters$cluster==2]<-"Indeterminate"
			Clusters$cluster[Clusters$cluster==1]<-"Optimal"
			Clusters$cluster[Clusters$cluster==4]<-"Exploratory"

P1<-c(-0.8,5.8)
P2<-c(-1.9,1.4)
P3<-c(-0.9,3.3)
P4<-c(-0.8,3.8)

style<-theme(
    axis.text=element_text(face="bold", size=32, color="black"),
    axis.line=element_line(size=1.5),
	  axis.ticks=element_line(size=1.5), 
    axis.ticks.length=unit(12, "pt"),
  	legend.title = element_blank(),
  	legend.text = element_text(size = 20, face="bold"),
  	legend.key=element_blank(),
	)
fviz_cluster(Clusters, data=temp, 
	geom="point", pointsize=5, main=F, xlab=F, ylab=F, show.clust.cent=F, stroke=1.8,
	choose.vars=c("P1","P2"),  
	xlim=P1,
	ylim=P2,
	)+style+
  scale_colour_manual(values = c("#0987D7", "#DDB91C", "#0DBD51", "#F46764", "#6a1506"))+
  scale_fill_manual(values = c("#0987D7", "#DDB91C", "#0DBD51", "#F46764", "#6a1506"))+
  scale_shape_manual(values=c(23, 22, 21, 24, 25) )
	ggsave("stats/P1-P2.tiff", dpi="print", compression = "lzw")
fviz_cluster(Clusters, data=temp, 
	geom="point", pointsize=5, main=F, xlab=F, ylab=F, show.clust.cent=F, stroke=1.8,
	choose.vars=c("P1","P3"),  
	xlim=P1,
	ylim=P3
	)+style+
  scale_colour_manual(values = c("#0987D7", "#DDB91C", "#0DBD51", "#F46764", "#6a1506"))+
  scale_fill_manual(values = c("#0987D7", "#DDB91C", "#0DBD51", "#F46764", "#6a1506"))+
  scale_shape_manual(values=c(23, 22, 21, 24, 25) )
	ggsave("stats/P1-P3.tiff", dpi="print", compression = "lzw")
fviz_cluster(Clusters, data=temp, 
	geom="point", pointsize=5, main=F, xlab=F, ylab=F, show.clust.cent=F, stroke=1.8,
	choose.vars=c("P1","P4"),  
	xlim=P1,
	ylim=P4
	)+style+
  scale_colour_manual(values = c("#0987D7", "#DDB91C", "#0DBD51", "#F46764", "#6a1506"))+
  scale_fill_manual(values = c("#0987D7", "#DDB91C", "#0DBD51", "#F46764", "#6a1506"))+
  scale_shape_manual(values=c(23, 22, 21, 24, 25) )
	ggsave("stats/P1-P4.tiff", dpi="print", compression = "lzw")
fviz_cluster(Clusters, data=temp, 
	geom="point", pointsize=5, main=F, xlab=F, ylab=F, show.clust.cent=F, stroke=1.8,
	choose.vars=c("P2","P3"),  
	xlim=P2,
	ylim=P3
	)+style+
  scale_colour_manual(values = c("#0987D7", "#DDB91C", "#0DBD51", "#F46764", "#6a1506"))+
  scale_fill_manual(values = c("#0987D7", "#DDB91C", "#0DBD51", "#F46764", "#6a1506"))+
  scale_shape_manual(values=c(23, 22, 21, 24, 25) )
	ggsave("stats/P2-P3.tiff", dpi="print", compression = "lzw")
fviz_cluster(Clusters, data=temp, 
	geom="point", pointsize=5, main=F, xlab=F, ylab=F, show.clust.cent=F, stroke=1.8,
	choose.vars=c("P2","P4"),  
	xlim=P2,
	ylim=P4
	)+style+
  scale_colour_manual(values = c("#0987D7", "#DDB91C", "#0DBD51", "#F46764", "#6a1506"))+
  scale_fill_manual(values = c("#0987D7", "#DDB91C", "#0DBD51", "#F46764", "#6a1506"))+
  scale_shape_manual(values=c(23, 22, 21, 24, 25) )
	ggsave("stats/P2-P4.tiff", dpi="print", compression = "lzw")
fviz_cluster(Clusters, data=temp, 
	geom="point", pointsize=5, main=F, xlab=F, ylab=F, show.clust.cent=F, stroke=1.8,
	choose.vars=c("P3","P4"),  
	xlim=P3,
	ylim=P4
	)+style+
  scale_colour_manual(values = c("#0987D7", "#DDB91C", "#0DBD51", "#F46764", "#6a1506"))+
  scale_fill_manual(values = c("#0987D7", "#DDB91C", "#0DBD51", "#F46764", "#6a1506"))+
  scale_shape_manual(values=c(23, 22, 21, 24, 25) )
	ggsave("stats/P3-P4.tiff", dpi="print", compression = "lzw")

	
	
#Compare Unilateral/bilateral:
	temp<-spread(Choice, Pct, data=subset(ChoicePhenotypes, Injury=="TBI"))
	temp$Type<-ifelse(temp$Study_ID=="CCI-E", "Uni", "Bilateral")
	fisher.test(temp$Type, temp$cluster)
	temp<-temp %>% count(cluster, Type)
	write.table(temp, "Phenotypes.csv", sep=",", col.names=NA, append=T)	
	
	
	
	
	
	
	
	
#Save cluster info for matching new subjects:
#isolate centroid of each cluster
  clust1<-as.vector(Clusters$centers[1,])
  clust2<-as.vector(Clusters$centers[2,])
  clust3<-as.vector(Clusters$centers[3,])
  clust4<-as.vector(Clusters$centers[4,])
  clust5<-as.vector(Clusters$centers[5,])

#Bring back new/old data:
  ChoiceClustPrePost<-aggregate(cbind(TotChoice, P1, P2, P3, P4)~SbjID+Pre.Injury+Injury+Study_ID, data=ChoiceClust, FUN=mean)
  PrePost<-aggregate(Session_norm~SbjID+ Injury, data=subset(Choice, Study_ID=="CCI-A_SS" | Study_ID=="HFD-A"), FUN=sum)
  PrePost<-subset(PrePost, Injury=="TBI")
  PrePost<-droplevels(PrePost$SbjID)
  ChoiceClustPrePost<-subset(ChoiceClustPrePost, SbjID %in% levels(PrePost))
  ChoiceClustPrePost<-gather(ChoiceClustPrePost, "Choice", "Count", P1:P4, factor_key = T)
  ChoiceClustPrePost$Pct<-ChoiceClustPrePost$Count/ChoiceClustPrePost$TotChoice
  ChoiceClustPrePost<-spread(ChoiceClustPrePost[,-c(5,7)], Choice, Pct)

#calculate euclidean distance from all 5 clusters for each subject
  #check against names above 
  Optimal<-sqrt(rowSums(t(t(ChoiceClustPrePost[,5:8]) - clust1)^ 2))
  Indeterminate<-sqrt(rowSums(t(t(ChoiceClustPrePost[,5:8]) - clust2)^ 2))
  Risky.P3<-sqrt(rowSums(t(t(ChoiceClustPrePost[,5:8]) - clust3)^ 2))
  Exploratory<-sqrt(rowSums(t(t(ChoiceClustPrePost[,5:8]) - clust4)^ 2))
  Risky.P4<-sqrt(rowSums(t(t(ChoiceClustPrePost[,5:8]) - clust5)^ 2))
  distances<-as.data.frame(cbind(Optimal,Exploratory,Indeterminate,Risky.P3,Risky.P4))
#identify smallest euclidean difference to determine cluster
  distances$cluster<-names(distances)[apply(distances, MARGIN = 1, FUN = which.min)]
  ChoiceClustPrePost<-cbind(ChoiceClustPrePost, distances)
  write.table(ChoiceClustPrePost, "Phenotypes.csv", sep=",", col.names=NA, append=T)

  
#Compare PrePost:
  fisher.test(ChoiceClustPrePost$Injury, ChoiceClustPrePost$cluster)

  
  
