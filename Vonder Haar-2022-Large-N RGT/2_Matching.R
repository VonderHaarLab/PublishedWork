


##Setup Data frame. Working with "Choice" from prior step
		#Choice<-rbind(ShamChoice, TBIChoice)	#if needed to reset frame


#Calculate variables for matching law
	#log(reinf/not reinf) and log(chosen/not chosen)			
		Choice$ChoiceRatio<-Choice$ChoiceCount/(Choice$TotChoice-Choice$ChoiceCount)
	#Obtained reinforcement rate:
	#Pellets/(pun duration + 5s ITI*choices)
		Choice$ReinfRate<-Choice$Pellets/(Choice$Pun_Dur+Choice$ChoiceCount*5)*60
		length(Choice$ReinfRate[is.nan(Choice$ReinfRate)])
		length(Choice$ReinfRate[Choice$ReinfRate==0])
	#Programmed reinforcement rate to put in NAN spots and outliers (single-choice, 0's):
	#P(Pellet #)  / (P(Pun Time) + 5 s ITI)
		ChoiceOption<-c(1,2,3,4)
		ProgReinfRate<-c(9.82, 13.71, 4.5, 3.31)
		ProgReinfRatio<-c(9.82/7.17, 13.71/5.88, 4.5/8.95, 3.31/9.34)
		temp<-data.frame(ChoiceOption, ProgReinfRate, ProgReinfRatio)
		Choice<-merge(Choice, temp, by="ChoiceOption", all=T)
		Choice$ReinfRate[is.nan(Choice$ReinfRate)]<-Choice$ProgReinfRate[is.nan(Choice$ReinfRate)]
		Choice$ReinfRate[Choice$ReinfRate==0]<-Choice$ProgReinfRate[Choice$ReinfRate==0]
		Choice$ReinfRate[Choice$ChoiceCount<5]<-Choice$ProgReinfRate[Choice$ChoiceCount<5]
			#Filter any with very low observations (4 or less)
#		Choice$ReinfRate[Choice$ReinfRate>Choice$ProgReinfRate*1.5]<-Choice$ProgReinfRate[Choice$ReinfRate>Choice$ProgReinfRate*1.5]
#		Choice$ReinfRate[Choice$ReinfRate<Choice$ProgReinfRate*(1/1.5)]<-Choice$ProgReinfRate[Choice$ReinfRate<Choice$ProgReinfRate*(1/1.5)]
			#filter ones that are more than 1.5x out from programmed due to low observations
			#went with the 4 cutoff instead		
		temp<-aggregate(ReinfRate~SbjID+Session_norm+Pre.Injury, data=Choice, FUN=sum)
		colnames(temp)[colnames(temp)=="ReinfRate"] <- "TotReinfRate"
		Choice<-merge(Choice, temp, by=c("SbjID", "Session_norm", "Pre.Injury"))		
		Choice$ReinfRatio<-Choice$ReinfRate/(Choice$TotReinfRate-Choice$ReinfRate)

#Number of infinites (exclusive choice of an option)
#68
length(Choice$ChoiceRatio[Choice$ChoiceRatio==Inf])
temp=subset(Choice, ChoiceRatio==Inf)
summary(temp)
		#27 minimum trials (TotChoice). Adding 1 to others should not have huge effect
		#View(temp)
		
#Number of zeroes (no choice of an option)
#1796
length(Choice$ChoiceRatio[Choice$ChoiceRatio==0])
temp=subset(Choice, ChoiceRatio==0)
summary(temp)
  #View(temp)
	#Relatively few at low end of trials (TotChoice). Adding 1 to all should not have huge effect.
temp$tally<-1
temp<-aggregate(tally~SbjID+Session_norm+Pre.Injury, data=temp, FUN=sum)
Choice<-merge(Choice, temp, by=c("SbjID", "Session_norm", "Pre.Injury"), all=T)
Choice$tally[is.na(Choice$tally)]<-0
Choice$TotChoice<-Choice$TotChoice+Choice$tally
Choice<-subset(Choice, select=-tally)
Choice$ChoiceCount[Choice$ChoiceCount==0]<-1
Choice$ChoiceRatio<-Choice$ChoiceCount/(Choice$TotChoice-Choice$ChoiceCount)









###Matching Calculations

Summary<-data.frame(Subject=double(), Injury=factor(), Acquisition=factor(), Bias=double(), Sensitivity=double(), R2=double())
levels(Summary$Injury)<-c("Sham","TBI")
levels(Summary$Acquisition)<-c("Acquisition","Trained")
GroupSummary<-data.frame(Injury=factor(), Acquisition=factor(), Bias=double(), Sensitivity=double(), R2=double())
levels(GroupSummary$Injury)<-c("Sham","TBI")
levels(GroupSummary$Acquisition)<-c("Acquisition","Trained")

Choice$SbjID<-as.factor(Choice$SbjID)

#Normal sham acquisition:
#CCI-E: post Aq
#CCI-D: post Aq
#CCI-A-Aq: post Aq
#CCI-A-SS: pre Aq
#HFD-A: pre Aq
temp<-subset(Choice, Study_ID=="CCI-E" & Injury=="Sham" & Session_norm>26)
temp<-rbind(temp, subset(Choice, Study_ID=="CCI-D" & Injury=="Sham" & Session_norm>19))
temp<-rbind(temp, subset(Choice, Study_ID=="CCI-A_AQ" & Injury=="Sham" & Session_norm>18))
temp<-rbind(temp, subset(Choice, Study_ID=="CCI-A_SS" & Injury=="Sham" & Pre.Injury=="Pre" & Session_norm>13))
temp<-rbind(temp, subset(Choice, Study_ID=="HFD-A" & Injury=="Sham" & Pre.Injury=="Pre" & Session_norm>14))
temp$SbjID<-droplevels(temp$SbjID)

for (Sbj in levels(temp$SbjID) ) 
{ 
	a<-try(lm(log(ChoiceRatio)~log(ReinfRatio), 
	data=subset(temp, SbjID==Sbj)))
    try(Summary[nrow(Summary)+1,]<-c(Sbj, "Sham", "Acquisition", coef(a), summary(a)$r.squared))
}

temp<-aggregate(cbind(ChoiceCount, TotChoice, ReinfRate, TotReinfRate)~SbjID+ChoiceOption, data=temp, FUN=mean)
a<-lm(log(ChoiceCount/(TotChoice-ChoiceCount))~log(ReinfRate/(TotReinfRate-ReinfRate)), data=temp)
GroupSummary[nrow(GroupSummary)+1,]<-c("Sham", "Acquisition", coef(a), summary(a)$r.squared)
xyplot(log(ChoiceCount/(TotChoice-ChoiceCount))~log(ReinfRate/(TotReinfRate-ReinfRate)), data=temp, type=c("p","r"), lwd=4, cex=1.2)



#TBI effect on acquisition:
#CCI-E: post Aq
#CCI-D: post Aq
#CCI-A-Aq: post Aq
temp<-subset(Choice, Study_ID=="CCI-E" & Injury=="TBI" & Session_norm>26)
temp<-rbind(temp, subset(Choice, Study_ID=="CCI-D" & Injury=="TBI" & Session_norm>19))
temp<-rbind(temp, subset(Choice, Study_ID=="CCI-A_AQ" & Injury=="TBI" & Session_norm>18))
temp$SbjID<-droplevels(temp$SbjID)

for (Sbj in levels(temp$SbjID) ) 
{ 
	a<-try(lm(log(ChoiceRatio)~log(ReinfRatio), 
	data=subset(temp, SbjID==Sbj)))
    try(Summary[nrow(Summary)+1,]<-c(Sbj, "TBI", "Acquisition", coef(a), summary(a)$r.squared))
}

temp<-aggregate(cbind(ChoiceCount, TotChoice, ReinfRate, TotReinfRate)~SbjID+ChoiceOption, data=temp, FUN=mean)
a<-lm(log(ChoiceCount/(TotChoice-ChoiceCount))~log(ReinfRate/(TotReinfRate-ReinfRate)), data=temp)
GroupSummary[nrow(GroupSummary)+1,]<-c("TBI", "Acquisition", coef(a), summary(a)$r.squared)
xyplot(log(ChoiceCount/(TotChoice-ChoiceCount))~log(ReinfRate/(TotReinfRate-ReinfRate)), data=temp, type=c("p","r"), lwd=4, cex=1.2)



#TBI effect on pretrained:
#CCI-A-SS: pre Aq
#HFD-A: pre Aq
temp<-subset(Choice, Study_ID=="CCI-A_SS" & Injury=="TBI" & Pre.Injury=="Post" & Session_norm>17)
temp<-rbind(temp, subset(Choice, Study_ID=="HFD-A" & Injury=="TBI" & Pre.Injury=="Post" & Session_norm>33))
temp$SbjID<-droplevels(temp$SbjID)

for (Sbj in levels(temp$SbjID) ) 
{ 
	a<-try(lm(log(ChoiceRatio)~log(ReinfRatio), 
	data=subset(temp, SbjID==Sbj)))
    try(Summary[nrow(Summary)+1,]<-c(Sbj, "TBI", "Trained", coef(a), summary(a)$r.squared))
}

temp<-aggregate(cbind(ChoiceCount, TotChoice, ReinfRate, TotReinfRate)~SbjID+ChoiceOption, data=temp, FUN=mean)
a<-lm(log(ChoiceCount/(TotChoice-ChoiceCount))~log(ReinfRate/(TotReinfRate-ReinfRate)), data=temp)
GroupSummary[nrow(GroupSummary)+1,]<-c("TBI", "Trained", coef(a), summary(a)$r.squared)
xyplot(log(ChoiceCount/(TotChoice-ChoiceCount))~log(ReinfRate/(TotReinfRate-ReinfRate)), data=temp, type=c("p","r"), lwd=4, cex=1.2)



temp<-subset(Choice, Study_ID=="CCI-E" & Injury=="TBI" & Session_norm>26)
temp<-rbind(temp, subset(Choice, Study_ID=="CCI-D" & Injury=="TBI" & Session_norm>19))
temp<-rbind(temp, subset(Choice, Study_ID=="CCI-A_AQ" & Injury=="TBI" & Session_norm>18))
temp<-rbind(temp, subset(Choice, Study_ID=="CCI-A_SS" & Injury=="TBI" & Pre.Injury=="Post" & Session_norm>17))
temp<-rbind(temp, subset(Choice, Study_ID=="HFD-A" & Injury=="TBI" & Pre.Injury=="Post" & Session_norm>33))
temp$SbjID<-droplevels(temp$SbjID)
temp<-aggregate(cbind(ChoiceCount, TotChoice, ReinfRate, TotReinfRate)~SbjID+ChoiceOption, data=temp, FUN=mean)
a<-lm(log(ChoiceCount/(TotChoice-ChoiceCount))~log(ReinfRate/(TotReinfRate-ReinfRate)), data=temp)
GroupSummary[nrow(GroupSummary)+1,]<-c("TBI", NA, coef(a), summary(a)$r.squared)
xyplot(log(ChoiceCount/(TotChoice-ChoiceCount))~log(ReinfRate/(TotReinfRate-ReinfRate)), data=temp, type=c("p","r"), lwd=4, cex=1.2)



temp<-data.frame(do.call(rbind, strsplit(as.vector(Summary$Subject), split = "-")))
temp$Study_ID<-paste(temp$X1, temp$X2, sep="-")
Study_ID<-temp[,-c(1:3)]
Summary<-cbind(Summary, Study_ID)
names<-c(4:6)
Summary[,names]<-lapply(Summary[,names], as.numeric)
names<-c(1,7)
Summary[,names]<-lapply(Summary[,names], as.factor)

write.table(Summary, "MatchingSummaryData.csv", sep=",", col.names = NA)


write.table(GroupSummary, "MatchingSummaryGroupData.csv", sep=",", col.names = NA)

temp<-subset(Choice, Study_ID=="CCI-E" & Session_norm>26)
temp<-rbind(temp, subset(Choice, Study_ID=="CCI-D" & Session_norm>19))
temp<-rbind(temp, subset(Choice, Study_ID=="CCI-A_AQ" & Session_norm>18))
temp<-rbind(temp, subset(Choice, Study_ID=="CCI-A_SS"  & Pre.Injury=="Post" & Session_norm>17))
temp<-rbind(temp, subset(Choice, Study_ID=="HFD-A" & Pre.Injury=="Post" & Session_norm>33))
temp$SbjID<-droplevels(temp$SbjID)
temp<-aggregate(cbind(Injury, log(ChoiceCount/(TotChoice-ChoiceCount)),log(ReinfRate/(TotReinfRate-ReinfRate)))~SbjID+ChoiceOption, data=temp, FUN=mean)
temp$Injury<-as.factor(temp$Injury)
levels(temp$Injury)<-c("Sham","TBI")
colnames(temp)[colnames(temp)=="V2"] <- "logChoiceRatio"
colnames(temp)[colnames(temp)=="V3"] <- "logReinfRatio"
write.table(temp, "MatchingSummaryGroupData.csv", sep=",", col.names = NA, append=T)





###Plotting
library(lattice)

#Normal sham:
ShamGraph<-subset(Choice, Study_ID=="CCI-E" & Injury=="Sham" & Session_norm>26)
ShamGraph<-rbind(ShamGraph, subset(Choice, Study_ID=="CCI-D" & Injury=="Sham" & Session_norm>19))
ShamGraph<-rbind(ShamGraph, subset(Choice, Study_ID=="CCI-A_AQ" & Injury=="Sham" & Session_norm>18))
ShamGraph<-rbind(ShamGraph, subset(Choice, Study_ID=="CCI-A_SS" & Injury=="Sham" & Pre.Injury=="Pre" & Session_norm>13))
ShamGraph<-rbind(ShamGraph, subset(Choice, Study_ID=="HFD-A" & Injury=="Sham" & Pre.Injury=="Pre" & Session_norm>14))
ShamGraph$SbjID<-droplevels(ShamGraph$SbjID)
names(ShamGraph)[names(ShamGraph) == "SbjID"] <- "Subject"
ShamGraph<-merge(ShamGraph, Summary[,-c(3,7)], by=c("Subject", "Injury"))
ShamGraph$Acquisition<-"Acquisition"
ShamGraph$Subject<-factor(ShamGraph$Subject, levels=unique(ShamGraph$Subject[rev(order(ShamGraph$Sensitivity))]))

graph_key <- list(x =1, y = .05, corner=c(1,1),
                  text = list(label=c("Sham", "TBI"), cex=1.5, font=2),
                  lines = list(col = c("black", 'red'), lwd = 4, lty = 1),
                  points = list(col = c("black", 'red'), pch=1, cex=1.5, lwd=2)
)
xyplot(log(ChoiceRatio)~log(ReinfRatio)|Subject, data=subset(ShamGraph, ), 
       col="black", cex=1.1, lwd=4,
        type=c("p","r"),
        ylab = list(label="Relative Choice Rate (log-ratio)", cex=1.7, fontface="bold"), 
        xlab = list(label="Relative Reinforcement Rate (log-ratio)", cex=1.7, fontface="bold"),
        main = list(label="Sham Sensitivity", cex=2, fontface="bold"),
        strip = FALSE,
        par.settings=list(axis.line = list(lwd = 3)),
        xlim=c(-4,1),
		    ylim=c(-5.8, 5),
        layout=c(6,14),
        as.table=T,
        key=graph_key,
        scales=list(
            cex=c(1.4,1.4), tck=c(1.5,0), fontface="bold", lwd=3, 
            y=list(labels=c("-4","0","4"), at=c(-4,0,4), alternating=c(0,1)),
            x=list(labels=c("-3","-2","-1","0"), at=c(-3,-2,-1,0), alternating=c(1,1)))
			)

#TBI effect:
TBIGraph<-subset(Choice, Study_ID=="CCI-E" & Injury=="TBI" & Session_norm>26)
TBIGraph<-rbind(TBIGraph, subset(Choice, Study_ID=="CCI-D" & Injury=="TBI" & Session_norm>19))
TBIGraph<-rbind(TBIGraph, subset(Choice, Study_ID=="CCI-A_AQ" & Injury=="TBI" & Session_norm>18))
TBIGraph<-rbind(TBIGraph, subset(Choice, Study_ID=="CCI-A_SS" & Injury=="TBI" & Pre.Injury=="Post" & Session_norm>17))
TBIGraph<-rbind(TBIGraph, subset(Choice, Study_ID=="HFD-A" & Injury=="TBI" & Pre.Injury=="Post" & Session_norm>33))
TBIGraph$SbjID<-droplevels(TBIGraph$SbjID)
names(TBIGraph)[names(TBIGraph) == "SbjID"] <- "Subject"
TBIGraph<-merge(TBIGraph, Summary[,-c(3,7)], by=c("Subject", "Injury"))
TBIGraph$Acquisition[TBIGraph$Study_ID=="CCI-A_AQ" | TBIGraph$Study_ID=="CCI-E" | TBIGraph$Study_ID=="CCI-D"]<-"Aquisition"
TBIGraph$Acquisition[TBIGraph$Study_ID=="CCI-A_SS" | TBIGraph$Study_ID=="HFD-A"]<-"Trained"
TBIGraph$Subject<-factor(TBIGraph$Subject, levels=unique(TBIGraph$Subject[rev(order(TBIGraph$Sensitivity))]))
    xyplot(log(ChoiceRatio)~log(ReinfRatio)|Subject, data=TBIGraph,
          col="red", cex=1.1, lwd=4,
          type=c("p","r"),
          ylab = list(label="Relative Choice Rate (log-ratio)", cex=1.7, fontface="bold"), 
          xlab = list(label="Relative Reinforcement Rate (log-ratio)", cex=1.7, fontface="bold"),
          main = list(label="TBI Sensitivity", cex=2, fontface="bold"),
          strip = FALSE,
        	par.settings=list(axis.line = list(lwd = 3)),
        	xlim=c(-4,1),
        	ylim=c(-5.8, 5),
        	layout=c(4,14),
          as.table=T,
        	scales=list(
          	  cex=c(1.4,1.4), tck=c(1.5,0), fontface="bold", lwd=3, draw=T,
          	  y=list(labels=c("-4","0","4"), at=c(-4,0,4), alternating=c(0,1)),
          	  x=list(labels=c("-3","-2","-1","0"), at=seq(-3,0,1), alternating=c(1,1)))
    )

		

#Pre vs. post
PrePost<-aggregate(Session_norm~Subject+ Injury, data=subset(rbind(ShamGraph,TBIGraph), Study_ID=="CCI-A_SS" | Study_ID=="HFD-A"), FUN=sum)
PrePost<-subset(PrePost, Injury=="TBI")
PrePost<-droplevels(PrePost$Subject)
PrePostGraph<-subset(rbind(ShamGraph,TBIGraph), Subject %in% levels(PrePost))

graph_key <- list(x =1.02, y = .93, corner=c(1,1),
                  text = list(label=c("Pre", "TBI"), cex=1.5, font=2),
                  lines = list(col = c("black", 'red'), lwd = 4, lty = 1),
                  points = list(col = c("black", 'red'), pch=1, cex=1.5, lwd=2)
)
PrePostGraph$Subject<-factor(PrePostGraph$Subject,levels=unique(PrePostGraph$Subject[order(PrePostGraph$Injury, PrePostGraph$Sensitivity)]))
xyplot(log(ChoiceRatio)~log(ReinfRatio)|Subject, data=PrePostGraph,
	group=Injury,
	type=c("p","r"),
    ylab = list(label="Relative Choice Rate (log-ratio)", cex=1.3, fontface="bold"), 
    xlab = list(label="Relative Reinforcement Rate (log-ratio)", cex=1.3, fontface="bold"),
    main = list(label="Pre- vs. Post-TBI Sensitivity", cex=2, fontface="bold"),
    strip = FALSE,
    par.settings=simpleTheme(col=c("black", "red"), cex=1.1, lwd=4),
    xlim=c(-4,1),
	ylim=c(-5.8, 5),
    scales=list(
		cex=c(1.2,1.2), tck=c(1.5,0), fontface="bold", lwd=2, alternating=c(0,1),
        y=list(labels=c("-4","0","4"), at=c(-4,0,4)),
        x=list(labels=c("-3","-2","-1","0"), at=c(-3,-2,-1,0))),
		key=graph_key
		)





#Uni vs. Bi
TBIGraph$Type<-ifelse(TBIGraph$Study_ID=="CCI-E", "Unilateral", "Bilateral")

graph_key <- list(x =1.01, y = .98, corner=c(1,1),
                  text = list(label=c("Bilateral", "Unilateral"), cex=1.5, font=2),
                  lines = list(col = c("red", 'orange'), lwd = 4, lty = 1),
                  points = list(col = c("red", 'orange'), pch=1, cex=1.5, lwd=2)
)
TBIGraph$Subject<-factor(TBIGraph$Subject,levels=unique(TBIGraph$Subject[order(TBIGraph$Type, TBIGraph$Sensitivity)]))
xyplot(log(ChoiceRatio)~log(ReinfRatio)|Subject, data=TBIGraph,
       group=Type,
       type=c("p","r"),
       ylab = list(label="Relative Choice Rate (log-ratio)", cex=1.3, fontface="bold"), 
       xlab = list(label="Relative Reinforcement Rate (log-ratio)", cex=1.3, fontface="bold"),
       main = list(label="Uni- vs. Bi-lateral Sensitivity", cex=2, fontface="bold"),
       strip = FALSE,
       par.settings=simpleTheme(col=c("red", "orange"), cex=1.1, lwd=4),
       xlim=c(-4,1),
       ylim=c(-5.8, 5),
       layout=c(6,9),
       scales=list(
         cex=c(1.2,1.2), tck=c(1.5,0), fontface="bold", lwd=2, alternating=c(0,1),
         y=list(labels=c("-4","0","4"), at=c(-4,0,4)),
         x=list(labels=c("-3","-2","-1","0"), at=c(-3,-2,-1,0))),
       key=graph_key
)



		
		
#Plots - bar graph of: sensitivity, bias, R2
  #SigmaPlot to graph
#Sham matching plot
#TBI matching plot


#Must remove pre-injury shams
	temp<-subset(Summary, !(Subject %in% levels(PrePost) & Injury=="Sham"))
t.test(Sensitivity~Injury, data=temp, var.equal=F)
t.test(Bias~Injury, data=temp, var.equal=F)
t.test(R2~Injury, data=temp, var.equal=F)

PrePostSummary<-subset(Summary, Subject %in% levels(PrePost))
PrePostSummary<-reshape(PrePostSummary[,-c(3,7)], direction="wide", idvar="Subject", timevar="Injury")
t.test(Pair(Sensitivity.Sham, Sensitivity.TBI)~1, data=PrePostSummary)
t.test(Pair(Bias.Sham, Bias.TBI)~1, data=PrePostSummary)
t.test(Pair(R2.Sham, R2.TBI)~1, data=PrePostSummary)

InjTypeSummary<-subset(Summary, Injury=="TBI")
InjTypeSummary$Type<-ifelse(InjTypeSummary$Study_ID=="CCI-E", "Uni", "Bilateral")
t.test(Sensitivity~Type, data=InjTypeSummary, var.equal=T)
t.test(Bias~Type, data=InjTypeSummary, var.equal=T)
t.test(R2~Type, data=InjTypeSummary)


