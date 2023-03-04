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

# Identifying whether there is a stopword in text data
sum(colnames(df) %in% c(stopwords()))

# Remove all one words like 'a','c'
# because they are not words, just alphabets
oneword <- which(unlist(lapply(colnames(df), nchar)) == 1)
df <- df[,-oneword]

# Feature selection by chi square statistic



# We need confusion matrix to get chi square statistic

counts <- function(df, grp) {
  tp <- colSums(grp*df>0) # true positive
  fn <- colSums(grp*(df == 0)) # false negative
  fp <- colSums((1-grp)*df > 0) # false positive 
  tn <- colSums((1-grp)*(df == 0)) # true negative
  obspos <- tp + fp # the number of the label
  obsneg <- tn + fn # the number of not label
  cnts <- cbind(tp, fn, fp, fn, 
                tp+fn, fp + tn, obspos, obsneg)
  colnames(cnts) <- c('TP','FN','FP','TN',
                      'Pos','Neg','pos','neg')
  cnts
}

# And define the user-definition chi square function like below

chi2s <- function(df){
  tp <- df[,1]; fn <- df[,2]
  fp <- df[,3]; tn <- df[,4]
  n1 <- tp + fn; n0 <- fp + tn
  obs1 <- tp + fp; obs0 <- fn + tn
  N <- n1 + n0
  p11 <- tp/N; p10 <- fn/N
  p01 <- fp/N; p00 <- tn/N
  cnts <- cbind(tp,fn,fp,tn,N,p11,p10,p01,p00)
  chinum <- (n1+n0) * (tp*tn - fp*fn)^2
  chiden <- (tp+fp) * (fn+tn) * n1 * n0
  chi <- ifelse(chiden == 0, 0, chinum/chiden)
  res <- cbind(cnts, chi)
  colnames(res) <- c('TP','FN','FP','TN','N','p11','p10','p01','p00',
                     'Chisq')
  res
}

# Make files the top 10, 20,... 100 words, based on chi square statistic, used in classification model
# And, among them, take a look at the top 10 words of each topic
for (nselw in (1:10)*10){
  slctrms <- matrix(0,ncol = length(grps),nrow = nselw)
  colnames(slctrms) <- grps
  for (ii in seq_along(grps)) {
    grp <- grindexes == grps[ii]
    trnws <- counts(df,grp)
    res <- chi2s(trnws)
    slctrms[,ii] <- names(sort(res[,'Chisq'],decreasing = T))[1:nselw]
  }
  if (nselw==10){
    print(slctrms)
  }
  write.csv(slctrms,paste(nselw," features.csv",sep =""))
}

# 10 files are made.

# Now Let's build a classification model
# First is a Naive Bayes 
# Define a Naive Bayes function as below

NBayes <- function(df,tests){
  dtmat <- as.matrix(df)
  traindat <- dtmat[-tests,]
  testdat <- dtmat[tests,]
  trainy <- lbls[-tests,1]
  testy <- lbls[tests,1]
  trainnb <- multinomial_naive_bayes(x=traindat,y=trainy)
  prednb <- predict(trainnb, testdat)
  tblnb <- table(data.frame(lbl=testy,pred=prednb))
}

# Let's set text indexes.
# The portion of test is 0.2
nobs <- dim(df)[1]
kcv <- 5; tests <- sample(nobs,nobs/kcv)
# For evaluating the performance, make empty vectors.
acc_nb <- c(); confmat_nb <- c()

# 

