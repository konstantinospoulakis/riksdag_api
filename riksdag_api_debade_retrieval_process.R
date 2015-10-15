library(XML)
library(jsonlite)
library(RCurl)
library(xml2)
library(tau) 
library(BBmisc)


data<-function(from_date,to_date){

#Step 1
#we are retrieving the file names of the debades in order to download them later
url<-getURL(paste("http://data.riksdagen.se/dokumentlista/?sok=&doktyp=prot&rm=&from=",paste(from_date),"&tom=",paste(to_date),
            paste("&ts=&bet=&tempbet=&nr=&org=&iid=&webbtv=&talare=&exakt=&planering=&sort=rel&sortorder=desc&rapport=&utformat=xml&a=s&s=1"),sep=""))
#The url has been parameterixed to receive protocols between (from_date,to_date)
sidor<-xmlTreeParse(url,useInternalNodes=T)
sidor<-xmlRoot(sidor)
ifelse(xmlSize(xmlAttrs(sidor))==14,sidor<-c(xmlAttrs(sidor)[[7]]),sidor<-c(xmlAttrs(sidor)[[6]]))#the structure of the xml for more than 1457 results is different so we have to parameterize this in order to return the right number of pages 
lista<-NULL
ids<-NULL
for (i in 1:sidor){
  doc<-getURL(paste("http://data.riksdagen.se/dokumentlista/?sok=&doktyp=prot&rm=&from=",paste(from_date),"&tom=",paste(to_date),
            paste("&ts=&bet=&tempbet=&nr=&org=&iid=&webbtv=&talare=&exakt=&planering=&sort=rel&sortorder=desc&rapport=&utformat=xml&a=s&"),paste("p=",i,sep=""),sep=""))                
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
report<-data.frame(lista,rep(0,length(lista)))
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
      text<-c(text,fixEncoding(xmlValue(dokumentolikaanforanden[[1]]), latin1 = FALSE)[[1]]) ;   #here we are saving the encoded text, letters of swedish alphabet are getting recognized through this command      
    }
    k<-k+1 #drives the while to the next document
    exist<- getURL(paste("http://data.riksdagen.se/anforande/",paste(lista[i]),ifelse(k<10,paste("-",0,k,sep=""),paste("-",k,sep="")),sep=""))  
  }
  report[i,2]<-ifelse(k==1,0,k)
if(i%%10==0) print(paste(i,"debades so far"))
}
results<-list(text,report)
return(results)
}


from_date="2010-01-01"  #insert the starting date of the debades you want
to_date="2010-12-31"    #insert the ending date of the debades you want

ptm<-proc.time
new<-data(from_date,to_date)
proc.time() - ptm

report<-as.data.frame(new[2])

        
write.table(report,"number of responces in debades.txt",sep=" , ",quote=F)






