#Taking an approach to the data to see how many subjects have a central block and how many have a peripheral block etc. 
#Specifically ones that are different between linalool and TMA
#start with df.merge from SplitNostril Setup1.R
library(ggplot2)
library(plyr)
library(reshape2)

#function to delete a certain amount of nas
delete.na <- function(df, n=0) {
  log <- apply(df, 2, is.na)
  logindex <- apply(log, 1, function(x) sum(x) <= n)
  df[logindex, ]
}
df.merge <- delete.na(df.merge)

#try new way of normalizingdata - noramlizes data between 0 and 1 - use this
range01 <- function(x){(x-min(x))/(max(x)-min(x))}
normalizedData <- ddply(df.merge, .variables = c("Subject", "Block"), transform, TMANorm = range01(TMA), antagNorm = range01(Antag))
#Get a database that just has the both in different noses and the TMA
#centralEffect <- df.merge[which((df.merge$NostrilType == "Alone" & df.merge$Odor == "TMA" )| df.merge$NostrilType == "Different"),]
#get average across trials
#this is for non-normalized data
#write.table(df.merge, file = "CleanedSetup1Data", sep ="\t")
#write.table(normalizedData, file = "NormalizedCleanedSetup1Data", sep = "\t")

centralEffectAverage <- ddply(.data = df.merge, .variables = c("Subject", "NostrilType", "Odor", "Block"), .fun= summarize, avgTMA = mean(TMA))

# #if we want it normalized - normalizedData comes from the Setup1Analysis
# centralEffectAverage <- ddply(.data = normalizedData, .variables = c("Subject", "NostrilType", "Odor", "Block"), .fun= summarize, avgTMA = mean(TMANorm))

#if we want it normalized - normalizedData comes from the Setup1Analysis
centralEffectAverage <- ddply(.data = normalizedData, .variables = c("Subject", "NostrilType", "Odor", "Block"), .fun= summarize, avgTMA = mean(TMANorm))

#Average across replicates
#For each subject calculate: TMA Alone-different
blockingDifferences <- ddply(centralEffectAverage, .(Subject,Block),function(x) c(CentralBlock=(subset(x,Odor=="TMA")$avgTMA-subset(x,NostrilType=="Different")$avgTMA)*1, PeripheralBlock=(subset(x,NostrilType=="Different")$avgTMA-subset(x,NostrilType=="Same")$avgTMA)*1,TotalBlock=(subset(x,Odor=="TMA")$avgTMA-subset(x,NostrilType=="Same")$avgTMA*1)))
#blockingDifferences <- blockingDifferences[3:5]+1

blockingDifferences.melt <- melt(blockingDifferences,id=c("Subject","Block"))

ggplot(blockingDifferences.melt,aes(y=value,x=variable,color=Block))+geom_boxplot()

#this graph just makes it so that instead of blocking going up on the y axis, the more blocking, the lower it is, like the rest of our graphs
blockingDifferences.inhibition <- ddply(centralEffectAverage, .(Subject,Block),function(x) c(CentralBlock=subset(x,NostrilType=="Different")$avgTMA-subset(x,Odor=="TMA")$avgTMA+1, PeripheralBlock=subset(x,NostrilType=="Same")$avgTMA-subset(x,NostrilType=="Different")$avgTMA+1,TotalBlock=subset(x,NostrilType=="Same")$avgTMA-subset(x,Odor=="TMA")$avgTMA+1))
blockingDifferences.inhibition.melt <- melt(blockingDifferences.inhibition, id = c("Subject", "Block"))

#blockingDifferences.inhibition.melt$Block <- factor(blockingDifferences.inhibition.melt$Block, levels = c("Linalool", "Nonenol"), labels = c("Linalool", "Trans-2-Nonen-1-ol"))
#pdf("SplitNostrilforPatent_BW.pdf")
Legend_title = "Test Odor"
ggplot(subset(blockingDifferences.inhibition.melt, variable %in% c("CentralBlock", "PeripheralBlock")), aes(y= value, x=variable, fill = Block)) +
  geom_boxplot() +
  xlab("Type of Block") +
  ylab("Normalized Fishy Intensity") +
  geom_hline(y = 1) +
  geom_hline(y = 0) +
  theme_bw() +
  scale_fill_grey(start = .3, end = 0.9, Legend_title) +
  theme(legend.text = element_text(size = 20)) +
  theme(legend.title = element_text(size = 22, face = "bold")) +
  theme(legend.justification=c(0,1), legend.position=c(0,.3)) +
  theme(axis.title.x = element_text(face="bold", size=22),axis.text.x  = element_text(vjust=0.5, size=18, colour = "black")) +
  theme(axis.title.y = element_text(face="bold", size=22),axis.text.y  = element_text(vjust=0.5, size=10, colour = "black")) +
  theme(panel.grid.minor=element_blank(), panel.grid.major.y = element_line(colour = "grey"), panel.grid.major.x = element_line(colour = "grey"))+

   
#dev.off()


###Attempting to create a graph that makes more sense - here I want to use unnormalized data that i will normalize by making into a percent of TMA by itself. 
centralEffectAverage <- ddply(.data = df.merge, .variables = c("Subject", "NostrilType", "Odor", "Block"), .fun= summarize, avgTMA = mean(TMA))
#blockingDifferences.percent <- ddply(centralEffectAverage, .(Subject,Block),function(x) c(CentralBlock=(subset(x,NostrilType=="Different")$avgTMA-subset(x,Odor=="TMA")$avgTMA)/subset(x, Odor == "TMA")$avgTMA*100, PeripheralBlock=(subset(x,NostrilType=="Same")$avgTMA-subset(x,NostrilType=="Different")$avgTMA)/subset(x, Odor == "TMA")$avgTMA*100,TotalBlock=(subset(x,NostrilType=="Same")$avgTMA-subset(x,Odor=="TMA")$avgTMA)/subset(x, Odor == "TMA")$avgTMA*100))
#blockingDifferences.percent.melt <- melt(blockingDifferences.percent,id=c("Subject","Block"))
blockingDifferences.percent <- ddply(centralEffectAverage, .(Subject,Block),function(x) c(CentralBlock=subset(x,NostrilType=="Different")$avgTMA/subset(x, Odor == "TMA")$avgTMA*100, PeripheralBlock=(subset(x,NostrilType=="Same")$avgTMA-subset(x,NostrilType=="Different")$avgTMA+1)/subset(x, Odor == "TMA")$avgTMA*100,TotalBlock=subset(x,NostrilType=="Same")$avgTMA/subset(x, Odor == "TMA")$avgTMA*100))
blockingDifferences.percent.melt <- melt(blockingDifferences.percent,id=c("Subject","Block"))
ggplot(blockingDifferences.percent.melt, aes(y= value, x=variable, color = Block)) +
  geom_boxplot() +
  xlab("Type of Block") +
  ylab("Change in Fishy Intensity")


# centralEffect.cast <- dcast(centralEffectAverage, Subject~Block+NostrilType+Odor,  value.var = "avgTMA")
# centralEffect.cast$centralNonenol <- centralEffect.cast$Nonenol_Alone_TMA - centralEffect.cast$Nonenol_Different_Both 
# centralEffect.cast$centralLinalool <- centralEffect.cast$Linalool_Alone_TMA - centralEffect.cast$Linalool_Different_Both
# centralEffect.cast$perifNonenol <- centralEffect.cast$Nonenol_Different_Both - centralEffect.cast$Nonenol_Same_Both
# centralEffect.cast$perifLinalool <- centralEffect.cast$Linalool_Different_Both - centralEffect.cast$Linalool_Same_Both
# centralEffect.cast$totalBlockNonenol <- centralEffect.cast$Nonenol_Alone_TMA - centralEffect.cast$Nonenol_Same_Both
# centralEffect.cast$totalBlockLinalool <- centralEffect.cast$Linalool_Alone_TMA - centralEffect.cast$Linalool_Same_Both
# centralEffect.melt <- melt(centralEffect.cast, c("Subject"))
# 
# ggplot(subset(centralEffect.melt, variable %in% c("centralNonenol", "centralLinalool", "perifNonenol", "perifLinalool", "totalBlockNonenol", "totalBlockLinalool")), aes(x = variable, y = value, fill = variable))+
#   geom_bar(stat = "identity", position = "dodge") +
#   facet_grid(.~Subject) +
#   ggtitle("Percent difference in thingies")
# 
# 
# ggplot(subset(centralEffect.melt, variable %in% c("centralNonenol", "centralLinalool", "perifNonenol", "perifLinalool", "totalBlockNonenol", "totalBlockLinalool")), aes(x = variable, y = value, fill = variable))+
#   geom_boxplot()+
#   ggtitle("Difference between fishy and everything")
# 
# #make the variables easier to graph
# totalBlocking <- centralEffect.melt
# for(i in 1:length(totalBlocking$Subject)){
#   if (totalBlocking$variable[i] %in% c("centralNonenol", "perifNonenol", "totalBlockNonenol") ){
#     totalBlocking$whichBlocker[i] <- "Nonenol"
#   }
#   else if (totalBlocking$variable[i] %in% c("centralLinalool", "perifLinalool", "totalBlockLinalool")){
#     totalBlocking$whichBlocker[i] <- "Linalool"
#   }
#   else
#     totalBlocking$whichBlocker[i] <- "none"
#   if (totalBlocking$variable[i] %in% c("centralNonenol", "centralLinalool")){
#     totalBlocking$type[i] <- "central"
#   }
#   else if (totalBlocking$variable[i] %in% c("perifNonenol", "perifLinalool")){
#     totalBlocking$type[i] <- "peripheral"
#   }
#   else if (totalBlocking$variable[i] %in% c("totalBlockNonenol", "totalBlockLinalool")){
#     totalBlocking$type[i] <- "total"
#   }
#   else
#     totalBlocking$type[i] <- "none"
#   
# }

p_meds <- ddply(totalBlocking, .(type, whichBlocker), summarise, med = mean(value)) #med is actually calculating mean here
p_meds <- p_meds[which(p_meds$type != "none"),]

#pdf("/Volumes/mainland/Projects/TMA blocker/Split Nostril Study/Setup1/SummaryPerifvCentralwo237NormalizedIndiv.pdf")
ggplot(subset(totalBlocking, whichBlocker %in% c("Nonenol", "Linalool")), aes(x = type , y = value, colour = whichBlocker)) +
  geom_boxplot() +
  #facet_grid(Subject~.)+
  xlab("Type of Blocking") +
  ylab("Subject Difference in Rating of Fishy Intensity") +
  ggtitle("Split Nostril Setup1")+
  geom_text(data = p_meds, aes(x = type, y = med, label = med), size = 3, vjust = -1.5) #checking that the means of central and peripheral add up to the mean of total they do!
#dev.off()

#want to run t-test on these to see if each individual person is significantly blocking centrally
#Also want to look at graph of percent difference - this is probably a better way to normalize things. 

#run an anova on this...
stats <- subset(totalBlocking, whichBlocker %in% c("Nonenol", "Linalool"))
fit <- aov(value ~variable, stats)
summary(fit)

fit <- aov(value ~type*whichBlocker, stats)
summary(fit)

ttestCentral <- t.test(value~whichBlocker, data = subset(stats, type == "central"))
ttestCentral #no significant difference in central

ttestPeripheral <- t.test(value~whichBlocker, data = subset(stats, type == "peripheral"))
ttestPeripheral #no significant difference in peripheral

ttestTotal <- t.test(value~whichBlocker, data = subset(stats, type == "total"))
ttestTotal #no significant difference in peripheral

#this is not significant


#######################
ggplot(blockingDifferences.inhibition.melt, aes(y= value, x=variable, colour = Block)) +
  #geom_boxplot() +
  geom_point()+
  xlab("Type of Block") +
  ylab("Normalized Fishy Intensity") +
  facet_grid(Subject~.)
################################
ggplot(centralEffectAverage, aes(y= avgTMA, x=Block, colour = Odor)) +
  #geom_boxplot() +
  geom_point()+
  xlab("Type of Block") +
  ylab("Normalized Fishy Intensity") +
  facet_grid(Subject~NostrilType)
  
#Given this individual results, there looks like there's no effect... this is really stupid. 
