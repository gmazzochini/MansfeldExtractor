#species = character vector with species names
#file.name = path where to write the file and the name of the file

#RUN
mansfeld_extractor<-function(species,file.name=""){
  if("rvest" %in% rownames(installed.packages()) == FALSE) {install.packages("rvest")}
  require(rvest)
  database<-data.frame(Species = species, Uses=NA, References=NA)
  for(i in 1:length(species)){
    urlR<-paste("http://mansfeld.ipk-gatersleben.de/apex/f?p=185:145:::NO::P3_BOTNAME:",gsub(" ","%20",species[i]),sep="")
    webpage <- read_html(urlR) 
    noEntry <- webpage %>% html_nodes("small")
    if(any(grepl("No entries found",noEntry))){
      database[i,"Uses"]<-"No information."
      database[i,"References"]<-"No information."
      
      cat(paste(round(i/length(species)*100,1),"% - ",species[i]," - No info... :(\n",sep=""))
      next
    }
    results <- webpage %>% html_nodes("a")
    
    url2<-as.character(results[grepl(gsub(" ","%20",species[i]),results)])
    if(length(url2)>1){
      use2 <- vector("character",length(url2))
      references2 <- vector("character",length(url2))
      for(j in 1:length(url2)){
        urlR <- url2[j]
        endNum <- as.numeric(gregexpr(">",urlR)[[1]][1]-2)
        urlR3 <- paste("http://mansfeld.ipk-gatersleben.de/apex/",substring(urlR,34,endNum),sep="")
        webpage2 <- read_html(urlR3) 
        results2 <- webpage2 %>% html_nodes("div")
        use<-as.character(results2[which(grepl("Phylogeny",results2))+1][2])
        if(length(use)==0) {
          endNum2<-gregexpr(",",urlR)[[1]]
          endNum2<-endNum2[length(endNum2)]-1
          use2[j]<-paste(gsub("%20", " ", substring(urlR,126,endNum2)), ": No info.", sep="")
        }
        else{
          out2<-gsub("\n"," ",substring(use,45,nchar(use)-7))
          endNum2<-gregexpr(",",urlR3)[[1]]
          endNum2<-endNum2[length(endNum2)]-1
          use2[j]<-paste(gsub("%20", " ", substring(urlR3,132,endNum2)), ": ",out2, sep="")
          
          references <- webpage2 %>% html_nodes("li")
          references <- gsub("\n","", as.character(references[!grepl("text-decoration:none",references)]))
          if(length(references)==0) {
            references2[j] <- "No information."
          }
          
          else references2[j] <- paste(substring(references,5,sapply(references,nchar)-5) , collapse= "; ")
        }
      }
      out <- paste(use2, collapse="; ")
      database[i,"Uses"]<-out
      database[i,"References"]<-paste(references2, collapse="; ")
      
    }
    else{
      endNum<-as.numeric(gregexpr(">",url2)[[1]][1]-2)
      url3<-paste("http://mansfeld.ipk-gatersleben.de/apex/",substring(url2,34,endNum),sep="")
      webpage2 <- read_html(url3) 
      results2 <- webpage2 %>% html_nodes("div")
      use<-as.character(results2[which(grepl("Phylogeny",results2))+1][2])
      if(length(use)==0){
        database[i,2:3]<-"No information."
        cat(paste(round(i/length(species)*100,1),"% - ",species[i]," - No info... :(\n",sep=""))
        next
      }
      else{
        out<-gsub("\n"," ",substring(use,45,nchar(use)-7))
        database[i,"Uses"]<-out
        
        references <- webpage2 %>% html_nodes("li")
        references <- gsub("\n","", as.character(references[!grepl("text-decoration:none",references)]))
        ifelse(length(references)==0,
               database[i,"References"] <- "No information.",
               database[i,"References"] <- paste(substring(references,5,sapply(references,nchar)-5) , collapse= "; "))
      }
    }
    cat(paste(round(i/length(species)*100,1),"% - ",species[i]," - OK! :D\n",sep=""))
  }
  if(file.name!=""){
    write.table(database, file = file.name, row.names = F)
  }
  return(database)
}
#END RUN

#### Example ####

spList<-c("Euterpe edulis", "Mangifera indica")

data_uses<-mansfeld_extractor(species = spList,
                              file.name = "C:/Users/gmazz/Desktop/test2.txt")
