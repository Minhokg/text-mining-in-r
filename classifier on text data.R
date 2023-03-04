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
# We will see Naive Bayes, KNN, and support vector machine.
# First is Naive Bayes 
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

# Now use files we made.
# I will use compare models trained on 10, 20, ..., 100 words
# and last all words (1102)

for (nselw in c((1:10)*10,dim(df)[2])){
  if (nselw<=100){
    slctrms <- read.csv(paste(nselw," features.csv",sep =""),row.names = 1)
  } else{
    slctrms <- names(df)
  }
  slctrms <- unique(unlist(slctrms))
  sDTM <- df[,slctrms]
  tblnb <- NBayes(sDTM, tests)
  confmat_nb <- cbind(confmat_nb,as.vector(tblnb))
  acc_nb <- c(acc_nb,sum(diag(tblnb))/sum(tblnb))
}

# Now check accuracy and confusion matrix
rownames(confmat_nb) <- do.call(paste, c(expand.grid(grps,grps)[,2:1],sep=","))
colnames(confmat_nb) <- c((1:10)*10,dim(df)[2])
names(acc_nb) <- c((1:10)*10,dim(df)[2])
list(accuaracy = acc_nb, confusionmatrix = confmat_nb)

loc_nb <- names(which.max(acc_nb))
best_nb <- as.vector(acc_nb)[which.max(acc_nb)]
paste('The best accuracy is ',best_nb,' when using the number of ',loc_nb,' words')

# Next model is a KNN
# Unlike naive model, one more parameter is added, the number of k
KNN <- function(df,tests,kk){
  dtmat <- as.matrix(df)
  traindat <- dtmat[-tests,]
  testdat <- dtmat[tests,]
  trainy <- lbls[-tests,1]
  testy <- lbls[tests,1]
  predknn <- knn(train=traindat,test = testdat,cl=trainy,k=kk)
  tblnb <- table(data.frame(lbl=testy, pred=predknn))
}

# Also, accuracy is contained in a matrix, not a vector 
# because there is a 'k' parameter
# although below function is long, it is just a contraction of what we did in
KNNclass <- function(df,tests){
  acmat <- c()
  confmat_knn_list <- vector(mode = 'list',length=11)
  word_number <- c((1:10)*10,dim(df)[2])
  for (nselw in word_number){
    acc_knn <- c(); confmat_knn <- c()
    if (nselw <=100){
      slctrms <- read.csv(paste(nselw," features.csv",sep =""), row.names = 1)
      
    } else {slctrms <- names(df)}
    slctrms <- unique(unlist(slctrms))
    sDTM <- df[,slctrms]
    for (kk in seq(1,9,2)){
      tblknn <- KNN(sDTM, tests,kk)
      confmat_knn <- cbind(confmat_knn,as.vector(tblknn))
      acc_knn <- c(acc_knn,sum(diag(tblknn))/sum(tblknn))
    }
    rownames(confmat_knn) <- do.call(paste, c(expand.grid(grps,grps)[,2:1],sep=","))
    colnames(confmat_knn) <- seq(1,9,2)
    confmat_knn_list[[which(word_number==nselw)]] <- confmat_knn
    acmat <- cbind(acmat,acc_knn)
    
  }
  rownames(acmat) <- seq(1,9,2)
  colnames(acmat) <- word_number
  names(confmat_knn_list) <- word_number
  list(acmat=acmat, confmat_knn_list = confmat_knn_list)
}

result_knn <- KNNclass(df, tests)

best_knn <- max(result[['acmat']])
paste('When K is ', 2*(which.max(result[['acmat']]) %% 5) -1,'and the number of words is ',
      10*(which.max(result[['acmat']]) %/% 5 + 1), 'the accuracy is best at ',best_knn)

# Last, let's see Support Vector Machine Model.

SVM <- function(df,tests,mthd,cst){
  dtmat <- as.matrix(df)
  traindat <- dtmat[-tests,]
  testdat <- dtmat[tests,]
  trainy <- lbls[-tests,1]
  testy <- lbls[tests,1]
  trainsvm <- svm(x=traindat,y=as.factor(trainy),
                  kernel=mthd,cost=cst)
  predsvm <- predict(trainsvm, testdat)
  tblsvm <- table(data.frame(lbl=testy, pred=predsvm))
}

# This time, we will not make confusion matrix.
# Because it is a four-dimensional object. so we can't handle that.
# We can just generate accuracy list 

SVMclass <- function(df, tests,mthd,cst){
  acc <- c()
  for (nselw in seq(10,90,20)) {
    if (nselw<=100){
      slctrms <- read.csv(paste('final',nselw," features.csv",sep =""), row.names = 1)
    } else {
      slctrms <- names(df)
    }
    slctrms <- unique(unlist(slctrms))
    sDTM <- df[,slctrms]
    tblsvm <- SVM(sDTM, tests, mthd,cst)
    acc <- c(acc,sum(diag(tblsvm))/sum(tblsvm))
  }
  acc
}


acclist <- vector(mode = 'list',length=2)
method <- c('linear','radial')
cost <- c(0.1,1,10)
names(acclist) <- method
