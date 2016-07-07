################################################################

#Import Libraries
library(reshape2)
library(ggplot2)
library(plyr)

#delete function
delete.na <- function(df, n=0) {
  log <- apply(df, 2, is.na)
  logindex <- apply(log, 1, function(x) sum(x) <= n)
  df[logindex, ]
}

#clear previous stuff
rm(df)
rm(df.merge)
rm(df.norm)
rm(df.norm_melt)
rm(df.norm_scaled)
rm(dfSubset)
rm(dfSubset2)
rm(statsdf)
rm(df.melt)
rm(melt.normalizedData)
rm(normalizedData)
#setwd("Data/Raw Data")
setwd("/Volumes/mainland/Projects/TMA blocker/Split Nostril Study/Setup1/")



#read all files from directory
subj <- dir(path = "Data/", pattern="\\.txt$", full.names=TRUE)# creating a list of all file names
names(subj)  <- basename(subj)
df  <- ldply(subj, read.delim, stringsAsFactors=FALSE)

#subsetdata to get rid of test mode and include only relevant columnss
dfSubset  <- subset(df, Procedure.SubTrial. == "RatingProc", select = c ("Subject", "Session", "Running.Block.",  "Trial", "Procedure.Trial.", "BlockRating.SubTrial.", "BlockRating", "TMARating.SubTrial.", "TMARating", "Straw1and2", "Straw3and4", "Jar1", "Jar2", "Jar3", "Jar4"))
#Combine the weird thing that's happening with the two different columns for rating...




for(i in 1:length(dfSubset$TMARating)){
  if(is.na(dfSubset$TMARating[i])){
    dfSubset$TMARating[i] <- dfSubset$TMARating.SubTrial.[i]
    dfSubset$BlockRating[i] <- dfSubset$BlockRating.SubTrial.[i]
    }
  }
dfSubset <- subset(dfSubset, select = -c(TMARating.SubTrial.,BlockRating.SubTrial.))


#Get rid of Joel and Wendy's data because Joel is missing some datapoints, and Wendy can't smell TMA well
#also they were both run before odors were refreshed
#dfSubset <- dfSubset[which(dfSubset$Subject != 6 & dfSubset$Subject !=7 & dfSubset$Subject != 10),] #These people couldn't tell nonenol from TMA very well
dfSubset <- dfSubset[which(dfSubset$Subject != 2 & dfSubset$Subject != 3 & dfSubset$Subject !=7),]
#need to go back and connect this to quiz data


#cONVErT SCALES
dfSubset$TMA  <- 401-as.integer(dfSubset$TMARating)
dfSubset$Antag  <- 401 - as.integer(dfSubset$BlockRating)

test <- dfSubset[which(dfSubset$TMA < 0 | dfSubset$Antag <0),] #check this to make sure none of the values are negative
#I changed the size of the allowable rectangle for clicks in the program, so the scale is not perfectly from 400 to 50 in pixel distance
#This should not be a problem for the ratings because they are normalized in the next steps
dfSubset <- delete.na(dfSubset)

ValveCode <- read.csv("Valve_code.csv", header=TRUE, sep=",", stringsAsFactors=TRUE)

ValveCode$Running.Block. <- NULL
for(i in 1:length(ValveCode$Trial)){
  if(ValveCode$Block[i] == "Nonenol"){
    ValveCode$Running.Block.[i] <- "Test1"
  }
  if(ValveCode$Block[i] == "Linalool"){
    ValveCode$Running.Block.[i] <- "Test2"
  }
}

ValveCode2 <- subset(ValveCode, select = -c(TMALeftRight))

df.merge  <- merge(dfSubset, ValveCode2, c("Running.Block.", "Trial"))
df.merge <- subset(df.merge, select = c("Subject", "TMA", "Antag", "Block", "Odor", "NostrilType"))
df.melt <- melt(df.merge, c("Subject", "Block", "Odor", "NostrilType"))


#Raw data graphed poorly by individuals
#pdf("pilot.pdf", height = 8, width = 11)
ggplot(data = subset(df.melt, variable == "Antag"), aes(x = Odor, y = value, colour = NostrilType)) +
    geom_boxplot() +
    facet_grid(Block~Subject)
#dev.off()



#Normalize data
normalizedData <- ddply(df.merge, .variables = c("Subject", "Block"), transform, TMANorm = scale(TMA), antagNorm = scale(Antag))

#melt normalized Data
melt.normalizedData <- melt(normalizedData, c("Subject", "Block", "Odor", "NostrilType"))

#plot normalized data individually
#pdf("alldata.pdf", height = 8, width = 11)
ggplot(data = normalizedData, aes(x = Odor, y = TMANorm, colour = NostrilType)) +
  geom_boxplot() +
  facet_grid(Block~Subject)
#dev.off()

#################################
#Try to graph central versus peripheral blocking
################################
#Start with unnormalized data
# df.merge$Odor_Nostril <- paste(df.merge$Odor, df.merge$NostrilType, sep = " ")
# 
# averageByTrialType <- ddply(df.merge, .(Subject, Block, Odor_Nostril), .fun = summarize, avg = mean(TMA))
# averageByTrialTypeCast <- dcast(averageByTrialType, Subject~Odor_Nostril, value.var = "avg")


