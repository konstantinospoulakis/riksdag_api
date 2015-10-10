library(XML)
library(jsonlite)
library(RCurl)
library(xml2)
library(tau)
library(BBmisc)


#Step 1
#we are retrieving the file names of the debades in order to download them later
url<-getURL("http://data.riksdagen.se/dokumentlista/?sok=&doktyp=prot&rm=&from=2008-01-01&tom=2015-10-01&ts=&bet=&tempbet=&nr=&org=&iid=&webbtv=&talare=&exakt=&planering=&sort=rel&sortorder=desc&rapport=&utformat=xml&a=s&s=1")
#The url being examined has parameterixed to receive protocols between (2010-01-01,2015-07-01)
sidor<-xmlTreeParse(url,useInternalNodes=T)
sidor<-xmlRoot(sidor)
ifelse(xmlSize(xmlAttrs(sidor))==14,sidor<-c(xmlAttrs(sidor)[[7]]),sidor<-c(xmlAttrs(sidor)[[6]]))#the structure of the xml for more than 1457 results is different so we have to parameterize this in order to return the right number of pages 
lista<-NULL
ids<-NULL
for (i in 1:sidor){
  doc<-getURL(paste("http://data.riksdagen.se/dokumentlista/?sok=&doktyp=prot&rm=&from=2008-01-01&tom=2015-10-01&ts=&bet=&tempbet=&nr=&org=&iid=&webbtv=&talare=&exakt=&planering=&sort=rel&sortorder=desc&rapport=&utformat=xml&a=s&",paste("p=",i,sep=""),sep=""))                
  data_doc<-xmlTreeParse(doc,useInternalNodes=T)
  data_root<-xmlRoot(data_doc)
  ids<-c(getNodeSet(data_root,"//dokument/dok_id")) #with this process we extract the document ids
  id<-NULL
  for (j in 1: xmlSize(ids)) {
    id[j]<-xmlValue(ids[[j]])
  }
  lista<-c(lista,id)
}

lista #we can check which file names have been found through the process 
text<-NULL

#Step 2
# now that we have the names we are ready to download the text within the xlm files of every answer of every debade

for (i in 1:length(lista)) {                              
  k=1  #counter for the while loop, it will count how many responses are in every debade in order to let the while loop extract the speeches from there
  exist<- getURL(paste("http://data.riksdagen.se/anforande/",paste(lista[i]),ifelse(k<10,paste("-",0,k,sep=""),paste("-",k,sep="")),sep="")) #
  #exist helps to check if an xml exists because many url's dont exist
  while (exist!=paste("") ){  #if it exists then we continue 
    dokumentolikaanforanden<-getURL(paste("http://data.riksdagen.se/anforande/",paste(lista[i]),ifelse(k<10,paste("-",0,k,sep=""),paste("-",k,sep="")),sep=""))   
    #calling k responce in i debade
    if(is.error(try(xmlTreeParse(dokumentolikaanforanden,useInternalNodes=T) , T))==F)  { #iff the xml is well structured then the loop proceeds in the parcing and extracting of the text
      dokumentolikaanforanden<-xmlTreeParse(dokumentolikaanforanden,useInternalNodes=T);  
      dokumentolikaanforanden<-xmlRoot(dokumentolikaanforanden);
      dokumentolikaanforanden<-getNodeSet(dokumentolikaanforanden,"//anforande/anforandetext");
      text<-c(text,fixEncoding(xmlValue(dokumentolikaanforanden[[1]]), latin1 = FALSE)[[1]]) ;   #here we are saving the encoded text, symbols like c(?,?,?) are getting recognized through this command      
    }
    k<-k+1 #drives the while to the next document
    exist<- getURL(paste("http://data.riksdagen.se/anforande/H30910",paste(lista[i]),ifelse(k<10,paste("-",0,k,sep=""),paste("-",k,sep="")),sep=""))  
  }
}

