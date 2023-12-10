data<-read.csv("ASTSum.csv", na.strings="")
data$t.Trial<-log(data$Trials)
data$t.Om<-log(data$Omissions+0.5)
data$t.Cor<-log(data$Correct)
data$t.Err<-log(data$Errors)
data$Subject<-as.factor(data$Subject)


Trials<-aov(t.Trial~Injury*Phase, data=data)
Er<-aov(t.Err~Injury*Phase, data=data)
Om<-aov(t.Om~Injury*Phase, data=data)
Cor<-aov(t.Cor~Injury*Phase, data=data)



write.table("Trials", "ASTStats.csv", sep=",", col.names=NA)
write.table(anova(Trials), "ASTStats.csv", sep=",", col.names=NA, append=T)

write.table("Errors", "ASTStats.csv", sep=",", col.names=NA, append=T)
write.table(anova(Er), "ASTStats.csv", sep=",", col.names=NA, append=T)

write.table("Omissions", "ASTStats.csv", sep=",", col.names=NA, append=T)
write.table(anova(Om), "ASTStats.csv", sep=",", col.names=NA, append=T)

write.table("Corrects", "ASTStats.csv", sep=",", col.names=NA, append=T)
write.table(anova(Cor), "ASTStats.csv", sep=",", col.names=NA, append=T)