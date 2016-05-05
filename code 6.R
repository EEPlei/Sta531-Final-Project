

xmat<-test.3[[1]][[2]]
BW<-test.3[[1]][[3]]
MM<-test.3[[1]][[4]]
# data using to generate # 
text.test<-test.3[[1]][[1]]
# holdout #  
text.train <- test.3[[2]][[1]]
# generated text # 
gen.seq <- mixture(xmat, BW, MM, text.test, weights = c(0.2, 0.4, 0.4))
gen.text <- as.vector(unique[gen.seq])
gen.text <- paste(gen.text,sep=" ", collapse = " ")

wordcompare<-function(seq1,seq2,topwords){
  truerate<-scale(c(sort(table(seq1),decreasing=TRUE)[1:topwords]))
  rate<-scale(c(sort(table(seq2),decreasing=TRUE)[1:topwords]))
  return(sum(abs(rate-truerate)))
}



wordcompare(gen.seq,text.test,topwords=20)
# gen.seq comes from mixture
# second argument any vector of words or numbers 
# third argument how many most frequent words we want to consider

numtotext<-function(seq1,text){
  dataframe<-text
  unique<-unique(dataframe)
  unique<-unique[unique != ""]
  gen.text <- as.vector(unique[seq1])
  return(gen.text)
}
# takes argument vector of numbers and text chunk (non-holdout)
# returns concatenated text chunk


numtotext(gen.seq,gen.text)
