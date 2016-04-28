cleanText = function(text){
  punct <- '[]\\?!\"\'#$%&(){}+*/:;,._`|~\\[<=>@\\^-]'
  punct2 <- sub( ",.", "", punct )
  newText = gsub(punct2, "", text)
  newText2<-gsub('.',' .',newText,fixed=TRUE)
  newText2<-gsub(',',' ,',newText2,fixed=TRUE)
  newText2<-paste(c('. '),c(newText2))
  return(newText2)
}

cname<-file.path("D://OneDrive//Stat 531//Final","Adele")
docs <- Corpus(DirSource(cname))   

test<-c()
for (i in 1:10){
  test<-paste(test,docs[[i]]$content,collapse = '[i]')
}
test<-docs[[1]]$content

cleanText(test)

paste(c('. '),c( 'ABC') )

x<-cleanText(test)


dataframe<-strsplit(as.character(x),split=" ",fixed=TRUE)

dataframe<-unlist(dataframe)

unique<-unique(dataframe)

# monte carlo
mcFit <- markovchainFit(data=dataframe,method='mle',byrow=TRUE)
Tmat.MM<-mcFit$estimat@transitionMatrix


xseq.2<-seq(0,0,length=1000)
xseq.2[1]<-3
for (i in 2:1000){
  xseq.2[i]<-sample(seq(1,length(unique),by=1),1,replace=FALSE,prob= Tmat.MM[xseq.2[i-1],])
}

Outstring10<-as.vector(unique[xseq.2])
Outstring10<-paste(Outstring10,sep=" ",collapse = " ")

# MvS
num<-seq(1,length(unique),by=1)

x<-seq(0,0,length=length(dataframe))
for (i in 1:length(dataframe)){
  x[i]<-num[which(unique==dataframe[i])]
}

x3<-matrix(0,nrow=length(x)-2,ncol=3)
for (i in 1:(length(x)-2)){
  x3[i,]<-x[i:(i+2)]
}

seqtemp<-seq(0,0,length=500)
seqtemp[1]<-round(runif(1,1,length(num)))
x3temp<-x3[x3[,1]==seqtemp[1],]
if(length(x3temp)==3){
  seqtemp[2]<-sample(x=x3temp[2],size=1)
}
if (length(x3temp >3)) {
  seqtemp[2]<-sample(x=x3temp[,2],size=1)
}

for (i in 2:(length(seqtemp)-2)){
  x3temp<-x3[x3[,1]==seqtemp[(i-1)],]
  if (length(x3temp)==3){
    x3temp<-t(as.matrix(x3temp))
  }
  if (length(x3temp)>3){
    x3temp<-x3temp[x3temp[,2]==seqtemp[i],]
    if (length(x3temp)==3){
      x3temp<-t(as.matrix(x3temp))
    }
  }
  if (length(x3temp)==0){
    x3temp<-x3[x3[,1]==seqtemp[(i-1)],]
    x3temp[1,3]<-1
  }
  
  seqtemp[i+1]<-sample(x=x3temp[,3],size=1)
}

Outstring10<-as.vector(unique[seqtemp])
Outstring10<-paste(Outstring10,sep=" ",collapse = " ")


# HMM

gen<-function(m,k){
  pi<-runif(m,0,100)
  startProbs<-pi/sum(pi)
  trans<- matrix(runif(m^2,0,100),nrow=m,ncol=m)
  transProbs<-trans[1:m,]/rowSums(trans)
  phi <- matrix(runif(m*k,0,100),nrow=m,ncol=k)
  emissionProbs<-phi[1:m,]/rowSums(phi)
  vals<-list(startProbs,transProbs,emissionProbs)
}

vals<-gen(10,length(unique(x)))


mtest<-20
hmm <- initHMM(seq(1,mtest,by=1), seq(1,length(unique(x)),by=1),startProbs=vals[[1]]
               , transProbs=vals[[2]],emissionProbs =vals[[3]])
BW<-baumWelch(hmm,observation=x,maxIterations = 10)

BW$hmm$emissionProbs


zseq<-seq(0,0,length=500)
zseq[1]<-sample(seq(1,mtest,by=1),1,prob=BW$hmm$startProbs)
xseq<-seq(0,0,length=500)

for (i in 2:500){
  zseq[i]<-sample(seq(1,mtest,by=1),1,replace=FALSE,prob= BW$hmm$transProbs[,zseq[i-1]])
  xseq[i-1]<-sample(seq(1,length(unique(x)),by=1),1,replace=FALSE,prob= BW$hmm$emissionProbs[zseq[i-1],])
}

unique[xseq]

Outstring10<-as.vector(unique[xseq])
Outstring10<-paste(Outstring10,sep=" ",collapse = " ")
