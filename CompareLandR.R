#is it true that you only breathe out of one nostril at a time? 
#Compare values of left and right nostrils with TMA for each person - if they are different this may be a problem...

#start with valveCode

df.merge  <- merge(dfSubset, ValveCode, c("Running.Block.", "Trial"))
df.merge <- subset(df.merge, select = c("Subject", "TMA", "Antag", "Block", "Odor", "NostrilType", "TMALeftRight"))
df.melt <- melt(df.merge, c("Subject", "Block", "Odor", "NostrilType", "TMALeftRight"))


#Compare left and right nostrils
ggplot(data = subset(df.melt, variable == "TMA" & Block == "Nonenol"), aes(x = Odor, y = value, colour = NostrilType)) +
  geom_boxplot() +
  facet_grid(Subject~TMALeftRight)
#this doesn't look like its making a dramatic difference. 

#Normalize data
normalizedData <- ddply(df.merge, .variables = c("Subject", "Block"), transform, TMANorm = scale(TMA), antagNorm = scale(Antag))

#plot normalized data individually
#pdf("alldata.pdf", height = 8, width = 11)

ggplot(data = normalizedData, aes(x = Odor, y = TMANorm, colour = TMALeftRight))+
  geom_boxplot() +
  facet_grid(Subject~NostrilType)


#we need to know if the individual variability of TMA rated alone is more comparing the right and left nostrils
#than between two measurements in the same nostril
#this is what an anova can test, right?  - this question I'm very interested in becuase it will matter for whether i bother to set up the split nostril study or not

#or maybe we still run the split nostril, but we can't interpret it the same way.  For instance, if you are really inhaling a lot more through
#one nostril than the other, and you are subtracting the individual difference of the between nostril test from the TMA alone, you 
#should subtract the difference from the average of the TMA alone rating in the right nostril from both in different nostrils but where the TMA is in the right nostril
#that way it is a comparable level of TMA...
#would this truely solve the issue? maybe talk to Joel about whether its worth analyzing this way...


#doing the central and peripheral processing with the sides separated
centralEffectAverageLR <- ddply(.data = df.merge, .variables = c("Subject", "NostrilType", "Odor", "Block", "TMALeftRight"), .fun= summarize, avgTMA = mean(TMA))
centralEffectAverageLR <- ddply(.data = normalizedData, .variables = c("Subject", "NostrilType", "Odor", "Block", "TMALeftRight"), .fun= summarize, avgTMA = mean(TMANorm))
centralEffectAverageLR <- centralEffectAverageLR[which(centralEffectAverageLR$TMALeftRight != "N"),]
centralEffect.castLR <- dcast(centralEffectAverageLR, Subject~Block+NostrilType+Odor+TMALeftRight,  value.var = "avgTMA")
#Left
centralEffect.castLR$centralNonenol.L <- centralEffect.castLR$Nonenol_Alone_TMA_L - centralEffect.castLR$Nonenol_Different_Both_L 
centralEffect.castLR$centralLinalool.L <- centralEffect.castLR$Linalool_Alone_TMA_L - centralEffect.castLR$Linalool_Different_Both_L
centralEffect.castLR$perifNonenol.L <- centralEffect.castLR$Nonenol_Different_Both_L - centralEffect.castLR$Nonenol_Same_Both_L
centralEffect.castLR$perifLinalool.L <- centralEffect.castLR$Linalool_Different_Both_L - centralEffect.castLR$Linalool_Same_Both_L
centralEffect.castLR$totalBlockNonenol.L <- centralEffect.castLR$Nonenol_Alone_TMA_L - centralEffect.castLR$Nonenol_Same_Both_L
centralEffect.castLR$totalBlockLinalool.L <- centralEffect.castLR$Linalool_Alone_TMA_L - centralEffect.castLR$Linalool_Same_Both_L
#right
centralEffect.castLR$centralNonenol.R <- centralEffect.castLR$Nonenol_Alone_TMA_R - centralEffect.castLR$Nonenol_Different_Both_R 
centralEffect.castLR$centralLinalool.R <- centralEffect.castLR$Linalool_Alone_TMA_R - centralEffect.castLR$Linalool_Different_Both_R
centralEffect.castLR$perifNonenol.R <- centralEffect.castLR$Nonenol_Different_Both_R - centralEffect.castLR$Nonenol_Same_Both_R
centralEffect.castLR$perifLinalool.R <- centralEffect.castLR$Linalool_Different_Both_R - centralEffect.castLR$Linalool_Same_Both_R
centralEffect.castLR$totalBlockNonenol.R <- centralEffect.castLR$Nonenol_Alone_TMA_R - centralEffect.castLR$Nonenol_Same_Both_R
centralEffect.castLR$totalBlockLinalool.R <- centralEffect.castLR$Linalool_Alone_TMA_R - centralEffect.castLR$Linalool_Same_Both_R

centralEffect.meltLR <- melt(centralEffect.castLR, c("Subject"))

#make the variables easier to graph
totalBlockingLR <- centralEffect.meltLR[which(centralEffect.meltLR$variable %in% c("centralNonenol.L", "centralLinalool.L", "perifNonenol.L", "perifLinalool.L", "totalBlockNonenol.L", "totalBlockLinalool.L", "centralNonenol.R", "centralLinalool.R", "perifNonenol.R", "perifLinalool.R", "totalBlockNonenol.R", "totalBlockLinalool.R")),]
for(i in 1:length(totalBlockingLR$Subject)){
  if (totalBlockingLR$variable[i] %in% c("centralNonenol.L", "perifNonenol.L", "totalBlockNonenol.L", "centralNonenol.R", "perifNonenol.R", "totalBlockNonenol.R") ){
    totalBlockingLR$whichBlocker[i] <- "Nonenol"
  }
  else if (totalBlockingLR$variable[i] %in% c("centralLinalool.L", "perifLinalool.L", "totalBlockLinalool.L", "centralLinalool.R", "perifLinalool.R", "totalBlockLinalool.R")){
    totalBlockingLR$whichBlocker[i] <- "Linalool"
  }
  else
    totalBlockingLR$whichBlocker[i] <- "none"
  if (totalBlockingLR$variable[i] %in% c("centralNonenol.L", "centralLinalool.L", "centralNonenol.R", "centralLinalool.R")){
    totalBlockingLR$type[i] <- "central"
  }
  else if (totalBlockingLR$variable[i] %in% c("perifNonenol.L", "perifLinalool.L", "perifNonenol.R", "perifLinalool.R")){
    totalBlockingLR$type[i] <- "peripheral"
  }
  else if (totalBlockingLR$variable[i] %in% c("totalBlockNonenol.L", "totalBlockLinalool.L", "totalBlockNonenol.R", "totalBlockLinalool.R")){
    totalBlockingLR$type[i] <- "total"
  }
  else
    totalBlockingLR$type[i] <- "none"
  if(totalBlockingLR$variable[i] %in% c("centralNonenol.L", "perifNonenol.L", "totalBlockNonenol.L", "centralLinalool.L", "perifLinalool.L", "totalBlockLinalool.L")){
    totalBlockingLR$LR[i] <- "L"
  }
  else if(totalBlockingLR$variable[i] %in% c("centralNonenol.R", "perifNonenol.R", "totalBlockNonenol.R", "centralLinalool.R", "perifLinalool.R", "totalBlockLinalool.R")){
    totalBlockingLR$LR[i] <- "R"
  }
  else
    totalBlockingLR$LR[i] <- "none"
  
}

ggplot(subset(totalBlockingLR, whichBlocker %in% c("Nonenol", "Linalool")), aes(x = type , y = value, colour = whichBlocker)) +
  geom_boxplot() +
  facet_grid(Subject~LR)+
  xlab("Type of Blocking") +
  ylab("Subject Difference in Rating of Fishy Intensity") +
  ggtitle("Split Nostril Setup1")#+
  #geom_text(data = p_meds, aes(x = type, y = med, label = med), size = 3, vjust = -1.5) #checking that the means of central and peripheral add up to the mean of total they do!
#dev.off()

ggplot(subset(totalBlockingLR, whichBlocker %in% c("Nonenol", "Linalool")), aes(x = type , y = value, colour = whichBlocker)) +
  geom_boxplot() +
  facet_grid(.~LR)+
  xlab("Type of Blocking") +
  ylab("Subject Difference in Rating of Fishy Intensity") +
  ggtitle("Split Nostril Setup1")

#average each subject for blocking across both nostrils
totalBlockingLR.avg <- ddply(totalBlockingLR, .variables = c("whichBlocker", "type", "Subject"), .fun = summarize, avg = mean(value))

ggplot(totalBlockingLR.avg, aes(x = type , y = avg, colour = whichBlocker)) +
  geom_boxplot() +
  xlab("Type of Blocking") +
  ylab("Subject Difference in Rating of Fishy Intensity") +
  ggtitle("Split Nostril Setup1")
#This graph with the normalized and unnormalized data look pretty similar to them analyzed the other way. It maybe slightly emphasizes the peripheral blocking of nonenol. 
