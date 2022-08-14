##Setup Data frame. Working with "Choice" from first step
		#Choice<-rbind(ShamChoice, TBIChoice)	#if needed to reset frame
		
#Overall data to report
	#Total switches by group
	#Overall win-stay by group
	#Overall lose-stay by group


OutcomeOverall<-aggregate(cbind(WinStay,LoseStay,TotWins,TotLosses, Stays, Switches)~SbjID+Session_norm+Injury+Pre.Injury, data=subset(Choice, Session_norm<30), sum)
OutcomeOverall<-subset(OutcomeOverall,  Session_norm>11)
OutcomeOverall<-droplevels(OutcomeOverall)


library(latticeExtra)
densityplot(~Stays/(Stays+Switches), group=Injury, data=subset(OutcomeOverall, (Stays+Switches)>4 & Pre.Injury=="Post"), 
            lwd=6, pch=32, 
            xlim=c(-0.05,1.05), 
            ylim=c(-0.05, 2.05),
            ylab = list(label="Density", cex=1.7, fontface="bold"), 
            xlab = list(label="p(Stay)", cex=1.7, fontface="bold"),
            par.settings = list(axis.line = list(lwd = 3), 
              superpose.line = list(lwd=4,
              col = c("black", "red"))
              ),
            scales=list(
              cex=c(1.4,1.4), tck=c(1.5,0), fontface="bold", lwd=3),
            auto.key=list(
              text=c("Sham","TBI"),
              lines=T, cex=1.5, fontface="bold",
              corner=c(0.975,0.02)
            )
            )

graph_key <- list(x = .975, y = .14, corner=c(1,1),
                  text = list(label=c("Win", "Loss", 'Sham', 'TBI'), cex=1.5, font=2),
                  lines = list(col = c("grey", "grey", "black", 'red'), lwd = 4, lty = c(1,3,1,1))
                  )
densityplot(~WinStay/TotWins, group=Injury, data=subset(OutcomeOverall, (Stays+Switches)>4 & Pre.Injury=="Post"),
            lwd=6, pch=32, 
            xlim=c(-0.05,1.05), 
            ylim=c(-0.05, 2.05),
            ylab = list(label="Density", cex=1.7, fontface="bold"), 
            xlab = list(label="p(Stay)", cex=1.7, fontface="bold"),
            par.settings = list(axis.line = list(lwd = 3), 
                            superpose.line = list(lwd=4,
                            col = c("black","red"))),
            scales=list(
              cex=c(1.4,1.4), tck=c(1.5,0), fontface="bold", lwd=3),
            key=graph_key
)+
as.layer(densityplot(~LoseStay/TotLosses, group=Injury, data=subset(OutcomeOverall, (Stays+Switches)>4),
          lwd=7, pch=32,
          col=c("black", "red"), lty=3
          ))







#Individual subjects overall Switches
OutcomeOverallAvg<-aggregate(cbind(Stays, Switches, WinStay, TotWins, LoseStay, TotLosses)~SbjID+Injury, data=OutcomeOverall, FUN=mean)
OutcomeOverallAvg$Tot<-OutcomeOverallAvg$Stays/(OutcomeOverallAvg$Switches+OutcomeOverallAvg$Stays)
OutcomeOverallAvg$Win<-OutcomeOverallAvg$WinStay/OutcomeOverallAvg$TotWins
OutcomeOverallAvg$Loss<-OutcomeOverallAvg$LoseStay/OutcomeOverallAvg$TotLosses
OutcomeOverallAvg$SbjID<-factor(OutcomeOverallAvg$SbjID, levels=unique(OutcomeOverallAvg$SbjID[rev(order(OutcomeOverallAvg$Tot))]))
OutcomeOverallAvg<-gather(OutcomeOverallAvg[,-c(3:8)], "Type", "pStay", Tot:Loss, factor_key=T)

library(RColorBrewer)
graph_key <- list(x =1, y = .05, corner=c(1,1),
                  text = list(label=c("Total", "Win", "Loss"), cex=1.5, font=2),
                  rectangles = list(col=rev(brewer.pal(3, "Greys")), lwd = 2 )
)
barchart(pStay~Type|SbjID, data=subset(OutcomeOverallAvg, Injury=="Sham"),
       ylab = list(label="p(Stay)", cex=1.7, fontface="bold"), 
       main = list(label="Sham Tendency to Stay", cex=2, fontface="bold"),
       strip = FALSE, lwd=2,
       par.settings=list(axis.line = list(lwd = 3),
         plot.polygon=(list(col=rev(brewer.pal(3, "Greys")))), lwd=2, cex=1.1),
       scales=list(cex=c(1.4,1.4), tck=c(1.5,0), fontface="bold", lwd=2, 
                   x=list(rot=30, labels="", at="", alternating=c(1,0)),
                   y=list(alternating=c(1,0))),
      as.table=T,
      layout=c(6,14),
      key=graph_key
)


a<-subset(OutcomeOverallAvg, Injury=="TBI")
a$SbjID<-droplevels(factor(a$SbjID, levels=unique(a$SbjID[rev(order(a$pStay[a$Type=="Tot"]))])))
graph_key <- list(x =1, y = .05, corner=c(1,1),
                  text = list(label=c("Total", "Win", "Loss"), cex=1.5, font=2),
                  rectangles = list(col=rev(brewer.pal(3, "Reds")), lwd = 2 )
)
barchart(pStay~Type|SbjID, data=subset(a, ),
         ylab = list(label="p(Stay)", cex=1.7, fontface="bold"), 
         main = list(label="TBI Tendency to Stay", cex=2, fontface="bold"),
         strip = FALSE, lwd=2,
         par.settings=list(axis.line = list(lwd = 3),
                           plot.polygon=(list(col=rev(brewer.pal(3, "Reds")))), lwd=2, cex=1.1),
         scales=list(cex=c(1.4,1.4), tck=c(1.5,0), fontface="bold", lwd=2, 
                     x=list(rot=30, labels="", at="", alternating=c(1,0)),
                     y=list(alternating=c(1,0))),
         as.table=T,
         layout=c(4,14),
         key=graph_key
)









#Filter down less than 4? 8? wins or losses? Will cause some outliers if done by percent
	#11668 lines, down to:
	length(Choice$TotWins[Choice$TotWins>4])
	length(Choice$TotWins[Choice$TotWins>8])
	length(Choice$TotLosses[Choice$TotLosses>4])
	length(Choice$TotLosses[Choice$TotLosses>8])
#Or aggregate overall
#Maybe even just by subject at stability - limit total sessions to min tested sessions
	#28 sessions is min for CCI-A_SS post-inj
	
OutcomeChoices<-aggregate(cbind(WinStay,LoseStay,TotWins,TotLosses, Stays, Switches)~SbjID+Injury+Pre.Injury+ChoiceOption, data=subset(Choice, Pre.Injury=="Post" & Session_norm>11 & Session_norm<30), sum)
	#436 lines down to:
	length(OutcomeChoices$TotWins[OutcomeChoices$TotWins>4])
	length(OutcomeChoices$TotWins[OutcomeChoices$TotWins>8])
	length(OutcomeChoices$TotLosses[OutcomeChoices$TotLosses>4])
	length(OutcomeChoices$TotLosses[OutcomeChoices$TotLosses>8])

densityplot(~Stays/(Stays+Switches)|ChoiceOption, group=Injury, data=subset(OutcomeChoices, (Stays+Switches)>4), 
            lwd=6, pch=32, as.table=T,
            xlim=c(-0.05,1.05), 
            ylim=c(-0.2,7.5), 
            ylab = list(label="Density", cex=1.7, fontface="bold"), 
            xlab = list(label="p(Stay)", cex=1.7, fontface="bold"),
            par.settings = list(axis.line = list(lwd = 3), 
                                superpose.line = list(lwd=4, col = c("black", "red")),
                                strip.border=list(lwd=3),
                                layout.heights=list(strip=1.55)),
            strip = strip.custom(bg="grey",  par.strip.text=list(cex=1.4, fontface="bold"), 
                                 factor.levels=c('P1 - Suboptimal','P2 - Optimal','P3 - Risky','P4 - Risky')),
            scales=list(
              cex=c(1.4,1.4), tck=c(1.5,0), fontface="bold", lwd=3, alternating=F),
            auto.key=list(
              text=c("Sham","TBI"),
              lines=T, cex=1.5, fontface="bold",
              corner=c(0.98,0.41)
            )
)


graph_key <- list(x = .985, y = .45, corner=c(1,1),
                  text = list(label=c("Win", "Loss", 'Sham', 'TBI'), cex=1.5, font=2),
                  lines = list(col = c("grey", "grey", "black", 'red'), lwd = 4, lty = c(1,3,1,1))
)
densityplot(~WinStay/TotWins|ChoiceOption, group=Injury, data=subset(OutcomeChoices, (Stays+Switches)>4),
            lwd=6, pch=32, as.table=T,
            xlim=c(-0.05,1.05), 
            ylim=c(-0.2,7.5), 
            ylab = list(label="Density", cex=1.7, fontface="bold"), 
            xlab = list(label="p(Stay)", cex=1.7, fontface="bold"),
            par.settings = list(axis.line = list(lwd = 3), 
                                superpose.line = list(lwd=4,col = c("black","red")),
                                strip.border=list(lwd=3),
                                layout.heights=list(strip=1.55)),
            strip = strip.custom(bg="grey", par.strip.text=list(cex=1.4, fontface="bold"), 
                                 factor.levels=c('P1 - Suboptimal','P2 - Optimal','P3 - Risky','P4 - Risky')),
            scales=list(
              cex=c(1.4,1.4), tck=c(1.5,0), fontface="bold", lwd=3, alternating=F),
            key=graph_key
)+
  
    as.layer(densityplot(~LoseStay/TotLosses|ChoiceOption, group=Injury, data=subset(OutcomeChoices, (Stays+Switches)>4),
                       lwd=7, pch=32,
                       col=c("black", "red"), lty=3
  ))











library(lmerTest)
#Overall tendency
a<-lmer(asin(sqrt(Stays/(Switches+Stays)))~Session_norm*Injury+(1|SbjID), data=subset(OutcomeOverall, (Stays+Switches)>4 & Pre.Injury=="Post"))
anova(a)

#Breakdown with win/loss variable
OutcomeOverallLong<-subset(OutcomeOverall, (Stays+Switches)>4)
OutcomeOverallLong$pStayWin<-OutcomeOverallLong$WinStay/OutcomeOverallLong$TotWins
OutcomeOverallLong$pStayLoss<-OutcomeOverallLong$LoseStay/OutcomeOverallLong$TotLosses
OutcomeOverallLong<-gather(OutcomeOverallLong[,-c(5:10)], "Win", "prob", pStayWin:pStayLoss, factor_key=T)
a<-lmer(asin(sqrt(prob))~Session_norm*Injury*Win+(1|SbjID), data=subset(OutcomeOverallLong, Pre.Injury=="Post"))
anova(a)

#Pre vs. post injury
PrePost<-aggregate(Session_norm~SbjID+ Injury, data=subset(Choice, Study_ID=="CCI-A_SS" | Study_ID=="HFD-A"), FUN=sum)
PrePost<-subset(PrePost, Injury=="TBI")
PrePost<-droplevels(PrePost$SbjID)
OutcomePrePost<-subset(OutcomeOverall, SbjID %in% levels(PrePost))
OutcomePrePostLong<-subset(OutcomeOverallLong, SbjID %in% levels(PrePost))
a<-lmer(asin(sqrt(Stays/(Switches+Stays)))~Session_norm*Injury+(1|SbjID), data=subset(OutcomePrePost, (Stays+Switches)>4))
anova(a)
a<-lmer(asin(sqrt(prob))~Session_norm*Injury*Win+(1|SbjID), data=subset(OutcomePrePostLong,))
anova(a)

#Uni vs bilateral
OutcomeInjType<-subset(OutcomeOverall, Injury=="TBI")
OutcomeInjType$Type<-ifelse(grepl("CCI-E", OutcomeInjType$SbjID, fixed=T), "Unilateral", "Bilateral")
OutcomeInjTypeLong<-subset(OutcomeOverallLong, Injury=="TBI")
OutcomeInjTypeLong$Type<-ifelse(grepl("CCI-E", OutcomeInjTypeLong$SbjID, fixed=T), "Unilateral", "Bilateral")
a<-lmer(asin(sqrt(Stays/(Switches+Stays)))~Session_norm*Type+(1|SbjID), data=subset(OutcomeInjType, (Stays+Switches)>4))
anova(a)
a<-lmer(asin(sqrt(prob))~Session_norm*Type*Win+(1|SbjID), data=subset(OutcomeInjTypeLong,))
anova(a)





temp<-subset(OutcomeChoices, Stays+Switches>4 & Pre.Injury=="Post")
	
a<-aov(asin(sqrt(Stays/(Stays+Switches)))~Injury*ChoiceOption, data=temp)
summary(a)
P1<-aov(asin(sqrt(Stays/(Stays+Switches)))~Injury, data=subset(temp,ChoiceOption==1))
summary(P1)
P2<-aov(asin(sqrt(Stays/(Stays+Switches)))~Injury, data=subset(temp,ChoiceOption==2))
summary(P2)
P3<-aov(asin(sqrt(Stays/(Stays+Switches)))~Injury, data=subset(temp,ChoiceOption==3))
summary(P3)
P4<-aov(asin(sqrt(Stays/(Stays+Switches)))~Injury, data=subset(temp,ChoiceOption==4))
summary(P4)
temp$pStay<-temp$Stays/(temp$Stays+temp$Switches)
temp$pStayWin<-temp$WinStay/temp$TotWins
temp$pStayLoss<-temp$LoseStay/temp$TotLosses
write.table(temp, "pStay_by_subject.csv", sep=",", col.names = NA)


temp$pStayWin<-temp$WinStay/temp$TotWins
temp$pStayLoss<-temp$LoseStay/temp$TotLosses
temp<-gather(temp[,-c(5:10)], "Win", "prob", pStayWin:pStayLoss, factor_key=T)
a<-aov(asin(sqrt(prob))~Injury*ChoiceOption*Win, data=temp)
summary(a)
P1<-aov(asin(sqrt(prob))~Injury*Win, data=subset(temp,ChoiceOption==1))
summary(P1)
P2<-aov(asin(sqrt(prob))~Injury*Win, data=subset(temp,ChoiceOption==2))
summary(P2)
P3<-aov(asin(sqrt(prob))~Injury*Win, data=subset(temp,ChoiceOption==3))
summary(P3)
P4<-aov(asin(sqrt(prob))~Injury*Win, data=subset(temp,ChoiceOption==4))
summary(P4)








#Individual subjects overall Switches
OutcomePrePostAvg<-aggregate(cbind(Stays, Switches, WinStay, TotWins, LoseStay, TotLosses)~SbjID+Injury, data=OutcomePrePost, FUN=mean)
OutcomePrePostAvg$Tot<-OutcomePrePostAvg$Stays/(OutcomePrePostAvg$Switches+OutcomePrePostAvg$Stays)
OutcomePrePostAvg$Win<-OutcomePrePostAvg$WinStay/OutcomePrePostAvg$TotWins
OutcomePrePostAvg$Loss<-OutcomePrePostAvg$LoseStay/OutcomePrePostAvg$TotLosses
OutcomePrePostAvg$SbjID<-factor(OutcomePrePostAvg$SbjID, levels=unique(OutcomePrePostAvg$SbjID[rev(order(OutcomePrePostAvg$Tot))]))
OutcomePrePostAvg<-gather(OutcomePrePostAvg[,-c(3:8)], "Type", "pStay", Tot:Loss, factor_key=T)

library(RColorBrewer)
graph_key <- list(x =.9, y = 0.15, corner=c(1,1),
                  text = list(label=c("Pre", "TBI"), cex=1.5, font=2),
                  rectangles = list(col=c("#636363","#DE2D26"), lwd = 2 )
)
barchart(pStay~Type|SbjID, data=subset(OutcomePrePostAvg, ), group=Injury,
         ylab = list(label="p(Stay)", cex=1.7, fontface="bold"), 
         main = list(label="Pre- vs. Post-TBI Tendency to Stay", cex=2, fontface="bold"),
         strip = FALSE, lwd=2,
         par.settings=list(
                            axis.line = list(lwd = 3),
                            superpose.polygon=list(
                              col=c("#636363","#DE2D26"), 
                              lwd=2, cex=1.1)),
         scales=list(cex=c(1.3,1.4), tck=c(1.5,0), fontface="bold", lwd=2, 
                     x=list(alternating=c(1,1)),
                     y=list(alternating=c(1,0))),
         as.table=T,
         layout=c(7,3),
         key=graph_key
)



library(latticeExtra)
densityplot(~Stays/(Stays+Switches), group=Injury, data=subset(OutcomePrePost, (Stays+Switches)>4), 
            lwd=6, pch=32, 
            xlim=c(-0.05,1.05), 
            ylim=c(-0.05, 2.05),
            ylab = list(label="Density", cex=1.7, fontface="bold"), 
            xlab = list(label="p(Stay)", cex=1.7, fontface="bold"),
            par.settings = list(axis.line = list(lwd = 3), 
                                superpose.line = list(lwd=4,
                                                      col = c("black", "red"))
            ),
            scales=list(
              cex=c(1.4,1.4), tck=c(1.5,0), fontface="bold", lwd=3),
            auto.key=list(
              text=c("Pre","TBI"),
              lines=T, cex=1.5, fontface="bold",
              corner=c(0.975,0.02)
            )
)

graph_key <- list(x = .975, y = .14, corner=c(1,1),
                  text = list(label=c("Win", "Loss", 'Pre', 'TBI'), cex=1.5, font=2),
                  lines = list(col = c("grey", "grey", "black", 'red'), lwd = 4, lty = c(1,3,1,1))
)
densityplot(~WinStay/TotWins, group=Injury, data=subset(OutcomePrePost, (Stays+Switches)>4 ),
            lwd=6, pch=32, 
            xlim=c(-0.05,1.05), 
            ylim=c(-0.05, 2.05),
            ylab = list(label="Density", cex=1.7, fontface="bold"), 
            xlab = list(label="p(Stay)", cex=1.7, fontface="bold"),
            par.settings = list(axis.line = list(lwd = 3), 
                                superpose.line = list(lwd=4,
                                                      col = c("black","red"))),
            scales=list(
              cex=c(1.4,1.4), tck=c(1.5,0), fontface="bold", lwd=3),
            key=graph_key
)+
  as.layer(densityplot(~LoseStay/TotLosses, group=Injury, data=subset(OutcomeOverall, (Stays+Switches)>4),
                       lwd=7, pch=32,
                       col=c("black", "red"), lty=3
  ))





library(latticeExtra)
densityplot(~Stays/(Stays+Switches), group=Type, data=subset(OutcomeInjType, (Stays+Switches)>4), 
            lwd=6, pch=32, 
            xlim=c(-0.05,1.05), 
            ylim=c(-0.05, 2.25),
            ylab = list(label="Density", cex=1.7, fontface="bold"), 
            xlab = list(label="p(Stay)", cex=1.7, fontface="bold"),
            par.settings = list(axis.line = list(lwd = 3), 
                                superpose.line = list(lwd=4,
                                                      col = c("red", "#ff8000"))
            ),
            scales=list(
              cex=c(1.4,1.4), tck=c(1.5,0), fontface="bold", lwd=3),
            auto.key=list(
              text=c("Bilateral","Unilateral"),
              lines=T, cex=1.5, fontface="bold",
              corner=c(0.975,0.02)
            )
)

graph_key <- list(x = .93, y = .14, corner=c(1,1),
                  text = list(label=c("Win", "Loss", 'Bilateral', 'Unilateral'), cex=1.5, font=2),
                  lines = list(col = c("grey", "grey", "red", '#ff8000'), lwd = 4, lty = c(1,3,1,1))
)
densityplot(~WinStay/TotWins, group=Type, data=subset(OutcomeInjType, (Stays+Switches)>4 ),
            lwd=6, pch=32, 
            xlim=c(-0.05,1.05), 
            ylim=c(-0.05, 2.25),
            ylab = list(label="Density", cex=1.7, fontface="bold"), 
            xlab = list(label="p(Stay)", cex=1.7, fontface="bold"),
            par.settings = list(axis.line = list(lwd = 3), 
                                superpose.line = list(lwd=4,
                                                      col = c("red","#ff8000"))),
            scales=list(
              cex=c(1.4,1.4), tck=c(1.5,0), fontface="bold", lwd=3),
            key=graph_key
)+
  as.layer(densityplot(~LoseStay/TotLosses, group=Type, data=subset(OutcomeInjType, (Stays+Switches)>4),
                       lwd=7, pch=32,
                       col=c("red", "#ff8000"), lty=3
  ))





OutcomeInjTypeAvg<-aggregate(cbind(Stays, Switches, WinStay, TotWins, LoseStay, TotLosses)~SbjID+Type, data=subset(OutcomeInjType, Injury=="TBI"), FUN=mean)
OutcomeInjTypeAvg$Tot<-OutcomeInjTypeAvg$Stays/(OutcomeInjTypeAvg$Switches+OutcomeInjTypeAvg$Stays)
OutcomeInjTypeAvg$Win<-OutcomeInjTypeAvg$WinStay/OutcomeInjTypeAvg$TotWins
OutcomeInjTypeAvg$Loss<-OutcomeInjTypeAvg$LoseStay/OutcomeInjTypeAvg$TotLosses
OutcomeInjTypeAvg$SbjID<-factor(OutcomeInjTypeAvg$SbjID, levels=unique(OutcomeInjTypeAvg$SbjID[rev(order(OutcomeInjTypeAvg$Type, OutcomeInjTypeAvg$Tot))]))
OutcomeInjTypeAvg<-gather(OutcomeInjTypeAvg[,-c(3:8)], "WinLoss", "pStay", Tot:Loss, factor_key=T)

graph_key <- list(x =.97, y = 0.05, corner=c(1,1),
                  text = list(label=c("Unilateral", "Bilateral"), cex=1.5, font=2),
                  rectangles = list(col=c("red","orange"), lwd = 2 )
)
barchart(pStay~WinLoss|SbjID, data=subset(OutcomeInjTypeAvg, ), group=Type,
         ylab = list(label="p(Stay)", cex=1.7, fontface="bold"), 
         main = list(label="Uni- vs. Bi-lateral Tendency to Stay", cex=2, fontface="bold"),
         strip = FALSE, lwd=2,
         par.settings=list(
           axis.line = list(lwd = 3),
           superpose.polygon=list(
             col=c("red","orange"), 
             lwd=2, cex=1.1)),
         scales=list(cex=c(1.3,1.4), tck=c(1.5,0), fontface="bold", lwd=2, 
                     x=list(alternating=c(1,1), rot=30),
                     y=list(alternating=c(1,0))),
         as.table=T,
         layout=c(6,9),
         key=graph_key
)
