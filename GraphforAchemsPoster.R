# trying to make a graph for my poster
#start with this
#centralEffectAverage <- ddply(.data = df.merge, .variables = c("Subject", "NostrilType", "Odor", "Block"), .fun= summarize, avgTMA = mean(TMA))
range01 <- function(x){(x-min(x))/(max(x)-min(x))}
normalizedData <- ddply(df.merge, .variables = c("Subject"), transform, TMANorm = range01(TMA), antagNorm = range01(Antag))

ggplot(normalizedData, aes(x = NostrilType, y = TMANorm, fill = Block)) +
  geom_boxplot()



#normalize data
avgnorm <- ddply(.data = normalizedData, .variables = c("Subject", "NostrilType", "Odor", "Block"), .fun= summarize, avgTMA = mean(TMANorm), stdev = sd(TMANorm), sterr = stdev/sqrt(4))
pureavg <- ddply(.data = avgnorm, .variables = c("NostrilType", "Odor", "Block"), .fun = summarize, avgTMA2 = mean(avgTMA), stdev = sd(avgTMA), sterr = sd(avgTMA)/2)
#devSVG("splitnostrilgraph2.svg")
ggplot(pureavg, aes(x = NostrilType, y = avgTMA2, fill = Block)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(.~Block) + 
  geom_errorbar(data = pureavg, position = "dodge", stat = "identity", aes(ymin = avgTMA2 - sterr, ymax = avgTMA2 + sterr))
dev.off()

#jar intensity balance graph
avgnormAntag <- ddply(.data = normalizedData, .variables = c("Subject", "NostrilType", "Odor", "Block"), .fun= summarize, avgAntag = mean(antagNorm), stdev = sd(antagNorm))
pureavgAntag <- ddply(.data = avgnormAntag, .variables = c("NostrilType", "Odor", "Block"), .fun = summarize, avgAntag2 = mean(avgAntag), stdev = sd(avgAntag), sterr = sd(avgAntag)/2)
antagsub <- subset(pureavgAntag, NostrilType == "Alone" & Odor != "TMA") 
names(antagsub) <- c("NostrilType", "Odor", "Block",  "Intensity", "stdev", "sterr")
tmasub <- subset(pureavg, NostrilType == "Alone" & Odor == "TMA")
names(tmasub) <-  c("NostrilType", "Odor", "Block",  "Intensity", "stdev", "sterr")
together <- rbind(tmasub, antagsub)
together$Odor <- factor(together$Odor, levels = c("TMA", "Non", "Lin"))

#devSVG("/Volumes/mainland/Projects/TMA blocker/Poster/splitnostrilgraphIntensity.svg")
ggplot(together, aes(x = Odor, y = Intensity, fill = Odor)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(data = together, position = "dodge", stat = "identity", aes(ymin = Intensity - sterr, ymax = Intensity + sterr))
#dev.off()