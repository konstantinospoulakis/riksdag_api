#install.packages("tm")
#install.packages("SnowballC")
#install.packages("topicmodels")
#install.packages("lda")
library(SnowballC)
library(tm)
data<-new[[1]]
data<- VectorSource(data);
data<-Corpus(data);
data <- tm_map(data, removePunctuation) # convert all text to lower case
data <- tm_map(data, removeNumbers)
data<-tm_map(data,stripWhitespace)
data <- tm_map(data, removeWords, c("="))
data <- tm_map(data, stemDocument, language = "swedish") ## Stemming the words 

data.dtm <- DocumentTermMatrix(data, control = list( minWordLength =4)) 
        
mdata<-as.matrix(data.dtm)
v <- sort(rowSums(mdata),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
data.dtm.sp <- removeSparseTerms(data.dtm, sparse=0.5) 
data.dtm.sp.df<-as.data.frame(inspect(data.dtm.sp ))
nrow(data.dtm.sp.df)
require(slam)
data.dtm.sp.t <- t(data.dtm.sp)
summary(col_sums(data.dtm.sp.t))
term_tfidf <- tapply(data.dtm.sp.t$v/row_sums(data.dtm.sp.t)[data.dtm.sp.t$i], data.dtm.sp.t$j,mean) * 
             log2(nDocs(data.dtm.sp.t)/col_sums(data.dtm.sp.t>0)) 
summary(term_tfidf)
data.dtm.sp.t.tdif <- data.dtm.sp.t[,term_tfidf>=1.0]
data.dtm.sp.t.tdif <- data.dtm.sp.t[row_sums(data.dtm.sp.t) > 0, ] 

require(topicmodels)
myModel=builtModel<-LDA(data.dtm, 10);
head(topics(myModel))


corpusLDA <- lexicalize(data )
require(lda)
which(corpusLDA$vocab=="=")

ptm<-proc.time()
ldaModel=lda.collapsed.gibbs.sampler(corpusLDA$documents,K=10,vocab=corpusLDA$vocab,burnin=3000,num.iterations=2000,alpha=1,eta=0.1)
top.words <- top.topic.words(ldaModel$topics, 50, by.score=T)
proc.time()-ptm


words<-as.data.frame(top.words)
write.table(words,"resultslda2010.txt",sep=",",quote=F)



