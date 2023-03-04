library(stopwords)
library(naivebayes)
library(class)
library(e1071)

DTM <- read.csv('https://raw.githubusercontent.com/Minhokg/text-mining-r/main/ReuterDTM.csv',
         header = T, row.names = 1)
df <- DTM[,-1]
lbls <- data.frame(DTM[,1])
grps <- sort(unique(lbls[,1]))
indexes <- lbls[,1]

sum(colnames(df) %in% c(stopwords()))

