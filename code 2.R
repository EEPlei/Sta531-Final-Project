x3<-matrix(0,nrow=length(x)-2,ncol=3)
for (i in 1:(length(x)-2)){
  x3[i,]<-x[i:(i+2)]
}

x3[998,]
x[1:3]

length(x)


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
  }

  seqtemp[i+1]<-sample(x=x3temp[,3],size=1)
  }

dataframe

Outstring10<-as.vector(unique[seqtemp])
Outstring10<-paste(Outstring10,sep=" ",collapse = " ")

seqtemp
i<-9

a<-c(1,2,3)

length(a)
is.logical(nrow(x3temp))
