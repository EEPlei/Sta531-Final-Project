library(readr)
library(tm)
library(HMM)
library(stringr)
library(markovchain) 

weightcomp<-function(weightsold,weightsnew,old,new){
  if (old > new){
    
  }
}

splittext<- function(text){
  dataframe<-text[1:(floor(length(text)/2))]
  dataframe.test<-text[(floor(length(text)/2)+1):length(text)]
  unique<-unique(dataframe)
  unique<-unique[unique != ""]
  dataframe<-dataframe[dataframe != ""]
  unique.test<-unique(dataframe.test)
  unique.test<-unique.test[unique.test != ""]
  dataframe.test<-dataframe.test[dataframe.test != ""]
  return(list(dataframe,dataframe.test))
}

test.text<-splittext(text)


mixture.train<-function(MvS,HMM,MM,text.train,text.test,weights){
  weightmat<-matrix(0,nrow=8,ncol=8)
  # MvS: list of matrices as produced for Mark v Shaney
  # HMM: hmm file as produced with *HMM*
  # weights: c(w1,w2) where w1+w2 = 1 are imput weights 
  # words: length of output matrix
  dataframe<-text.train
  dataframe.test<-text.test
  unique<-unique(dataframe)
  unique<-unique[unique != ""]
  unique.test<-unique(dataframe.test)
  unique.test<-unique.test[unique.test != ""]
  num<-seq(1,length(unique),by=1)
  w1<-weights[1]
  w2<-weights[2]
  w3<-weights[3]
  BW<-HMM
  Tmat<-MM
  xmat<-MvS
  zseq<-seq(0,0,length=length(dataframe))
  zseq[1]<-sample(seq(1,length(BW$hmm$startProbs),by=1),1,prob=BW$hmm$startProbs)
  xseq<-seq(0,0,length=length(dataframe))
  probseq<-seq(0,0,length=length(dataframe))
  xseq[1]<-round(runif(1,1,length(num)))
  xseq[2]<-sample(seq(1,length(num),by=1),size=1,prob = rowSums(xmat[[xseq[1]]]))
  for (i in 2:length(dataframe)){
    zseq[i]<-sample(seq(1,length(BW$hmm$startProbs),by=1),1,replace=FALSE,prob= BW$hmm$transProbs[,zseq[i-1]])
  }
  for (i in 3:length(dataframe)){
    BWprobs<-(BW$hmm$emissionProbs[zseq[i-1],])
    MMprobs<-(Tmat[xseq[i-1],])/sum(Tmat[xseq[i-1],])
    MvSprobs<-(xmat[[xseq[i-2]]][xseq[i-1],])
    xseq[i]<-sample(seq(1,length(unique),by=1),1,replace=FALSE,prob= w1*BWprobs+w2*MMprobs+w3*MvSprobs)
    probseq[i]<-sum(BWprobs)
  }
  # note that by default a string with the same length as the input text is generated
  truerate<-scale(c(sort(table(dataframe.test),decreasing=TRUE)[1:20]))
  rate<-scale(c(sort(table(xseq),decreasing=TRUE)[1:20]))
  return(sum(abs(rate-truerate)))
}

test<-mixture.train(xmat,BW,MM,test.text[[1]],test.text[[2]],weights=c(0.2,0,0.9))


combine<-function(text,maxit,states){
  test.text<-splittext(text)
  xmat<-genMvS(test.text[[1]])
  BW<-genHMM(test.text[[1]],maxit,states)
  MM<-genMM(test.text[[1]])
  xmat.2<-genMvS(test.text[[2]])
  BW.2<-genHMM(test.text[[2]],maxit,states)
  MM.2<-genMM(test.text[[2]])
  return(list(list(test.text[[1]],xmat,BW,MM),list(test.text[[2]],xmat.2,BW.2,MM.2)))
  # This is a list of two lists. The first part is the "training" half of the text, and the second part for the "test" half
  # note that we split the original text down the center
  # both part contain a list of all components needed for mixture. 
}

ptc<-proc.time()
test.3<-combine(dataframe,10,10)
# list of two lists # 
# first list is a list of four things # 
# first is first half of text # 
# second is MvS list of transitions for first half # 
# BW for first half #
# MC transition for first half #
# symmetric second list #
proc.time()-ptc

text.jm<-test.3[[1]][[1]]
xmat.jm<-test.3[[1]][[2]]
BW.jm<-test.3[[1]][[3]]    
MM.jm<-test.3[[1]][[4]]

metropolis <- function(weights, MvS, HMM, MM, text.train, text.test, iter){
  weight.vec <- matrix(nrow = 3, ncol = iter)
  weight.vec[,1] <- weights
  
  for(i in 1:iter){
    dist.old <- mixture.train(MvS, HMM, MM, text.train, text.test, weight.vec[,i-1])
    new.weights <- rdirichlet(1, weight.vec[,i-1])
    new.weights <- new.weights/sum(new.weights)
    dist.new <- mixture.train(MvS, HMM, MM, text.train, text.test, new.weights)
    
    if(dist.new < dist.old){
      weight.vec[i] <- new.weights
    }
    else(weights.vec[i] <- weights)
  }
  return(weights.vec)
  
}
