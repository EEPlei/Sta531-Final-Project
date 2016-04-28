library(readr)
library(tm)
library(HMM)
library(stringr)
library(markovchain) 

Bern <- read_file("D://OneDrive//Stat 531//Final//test 1.txt")

cname<-file.path("D://OneDrive//Stat 531","Final")

docs <- Corpus(DirSource(cname))   

summary(docs)

inspect(docs[1])
docs <- tm_map(docs[4], removePunctuation)   
docs <- tm_map(docs[4], removeNumbers)   
docs<-tm_map(docs,content_transformer(tolower))

dataframe<-data.frame(text=unlist(sapply(docs, `[`, "content")), 
                      stringsAsFactors=F)

dataframe<-as.character(dataframe)

dataframe<-strsplit(as.character(dataframe),split=" ",fixed=TRUE)

dataframe<-unlist(dataframe)

unique<-unique(dataframe)


num<-seq(1,length(unique),by=1)

x<-seq(0,0,length=length(dataframe))
for (i in 1:length(dataframe)){
  x[i]<-num[which(unique==dataframe[i])]
}

gen<-function(m,k){
  pi<-runif(m,0,100)
  startProbs<-pi/sum(pi)
  trans<- matrix(runif(m^2,0,100),nrow=m,ncol=m)
  transProbs<-trans[1:m,]/rowSums(trans)
  phi <- matrix(runif(m*k,0,100),nrow=m,ncol=k)
  emissionProbs<-phi[1:m,]/rowSums(phi)
  vals<-list(startProbs,transProbs,emissionProbs)
}

vals<-gen(10,1712)


mtest<-10
hmm <- initHMM(seq(1,mtest,by=1), seq(1,1712,by=1),startProbs=vals[[1]]
               , transProbs=vals[[2]],emissionProbs =vals[[3]])
BW<-baumWelch(hmm,observation=x,maxIterations = 10)

BW$hmm$emissionProbs


zseq<-seq(0,0,length=500)
zseq[1]<-sample(seq(1,mtest,by=1),1,prob=BW$hmm$startProbs)
xseq<-seq(0,0,length=500)

for (i in 2:500){
  zseq[i]<-sample(seq(1,mtest,by=1),1,replace=FALSE,prob= BW$hmm$transProbs[,zseq[i-1]])
  xseq[i-1]<-sample(seq(1,1712,by=1),1,replace=FALSE,prob= BW$hmm$emissionProbs[zseq[i-1],])
}

unique[xseq]

Outstring10<-as.vector(unique[xseq])
Outstring10<-paste(Outstring10,sep=" ",collapse = " ")

mcFit <- markovchainFit(data=x,method='mle',byrow=TRUE)
Tmat.MM<-mcFit$estimat@transitionMatrix

xseq.2<-seq(0,0,length=1000)
xseq.2[1]<-3
for (i in 2:1000){
  xseq.2[i]<-sample(seq(1,length(unique),by=1),1,replace=FALSE,prob= Tmat.MM[xseq.2[i-1],])
}


zseq<-seq(0,0,length=1000)
zseq[1]<-sample(seq(1,mtest,by=1),1,prob=BW$hmm$startProbs)
xseq<-seq(0,0,length=1000)
xseq[1]<-sample(seq(1,1712,by=1),1)


for (i in 2:500){
  zseq[i]<-sample(seq(1,mtest,by=1),1,replace=FALSE,prob= BW$hmm$transProbs[,zseq[i-1]])
  xseq[i]<-sample(seq(1,1712,by=1),1,replace=FALSE,prob= (BW$hmm$emissionProbs[zseq[i-1],]+Tmat.MM[xseq[i-1]]))
}


Outstring10<-as.vector(unique[xseq.2])
Outstring10<-paste(Outstring10,sep=" ",collapse = " ")
