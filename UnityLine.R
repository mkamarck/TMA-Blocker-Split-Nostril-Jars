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
#range01 <- function(x){(x-min(x))/(max(x)-min(x))}
#normalizedData <- ddply(df.merge, .variables = c("Subject", "Block"), transform, TMANorm = range01(TMA), antagNorm = range01(Antag))

#don't normalize it for now... maybe come back to this

#for each subject, calculate the difference between central and total and the difference between total and peripheral. Plot those against each other
#first average within subject the different ratings
centralEffectAverage <- ddply(.data = df.merge, .variables = c("Subject", "NostrilType", "Odor", "Block"), .fun= summarize, avgTMA = mean(TMA))
#then look at the differences
blockingDifferences <- ddply(centralEffectAverage, .(Subject,Block),function(x) c(CentralBlock=(subset(x,Odor=="TMA")$avgTMA-subset(x,NostrilType=="Different")$avgTMA)*1, PeripheralBlock=(subset(x,NostrilType=="Different")$avgTMA-subset(x,NostrilType=="Same")$avgTMA)*1,TotalBlock=(subset(x,Odor=="TMA")$avgTMA-subset(x,NostrilType=="Same")$avgTMA*1)))

#plot for each person, the central on the x and the peripheral on the y, anything above the line is those that have more peripheral block than central block. 
pdf("/Volumes/mainland/Projects/TMA blocker/Split Nostril Study/Setup1/unityline_splitNostril.pdf")
ggplot(data = subset(blockingDifferences, Block == "Nonenol"), aes(x = CentralBlock, y = PeripheralBlock, colour = factor(Subject))) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1,col="blue")  
dev.off()


#pdf("/Volumes/mainland/Projects/TMA blocker/Split Nostril Study/Setup1/unityline_splitNostril.pdf")
ggplot(data = subset(blockingDifferences, Block == "Linalool"), aes(x = CentralBlock, y = PeripheralBlock, colour = factor(Subject))) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1,col="blue")  
#dev.off()

pdf("/Volumes/mainland/Projects/TMA blocker/Split Nostril Study/Setup1/unityline_splitNostril.pdf", width = 11.5, height = 8)
ggplot(blockingDifferences, aes(x = CentralBlock, y = PeripheralBlock, colour = factor(Subject))) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1,col="blue")  +
  facet_grid(.~Block)
dev.off()


