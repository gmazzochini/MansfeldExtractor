
#######################################################################################################
# This function  works like the 0.1 version but checks for species synonims using the "flora" package #
#######################################################################################################

#species = character vector with species names
#file.name = path where to write the file and the name of the file

require(rvest)
require(flora)

mansfeld_extractor<-function(species,file.name=""){
  database<-data.frame(Species = species, Uses=NA, References=NA)
  for(i in 1:length(species)){
    synomyms<-na.omit(c(as.character(species[i]),as.character(get.synonyms(species[i]))))
    noEntrylist<-vector("list",length(synomyms))
    webpagelist<-vector("list",length(synomyms))
    for(k in 1:length(synomyms)){
      urlR<-paste("http://mansfeld.ipk-gatersleben.de/apex/f?p=185:145:::NO::P3_BOTNAME:",gsub(" ","%20",synomyms[k]),sep="")
      webpagelist[[k]] <- read_html(urlR) 
      noEntrylist[[k]] <- webpagelist[[k]] %>% html_nodes("small")
    }
    synomyms_out<-which(grepl("No entries found",unlist(lapply(noEntrylist,as.character))))/10
    
    if(length(synomyms_out)==length(synomyms)){
      database[i,"Uses"]<-"No information."
      database[i,"References"]<-"No information."
      
      cat(paste(round(i/length(species)*100,1),"% - ",species[i]," - No info... :(\n",sep=""))
      next
    }
    
    synomyms<-synomyms[!1:length(synomyms) %in% synomyms_out]
    webpagelist[synomyms_out]<-NULL
    if(length(synomyms)>1){
      resultslist<-vector("list", length(synomyms))
      url2<-as.character()
      for(h in 1:length(synomyms)){
        resultslist[[h]] <- webpagelist[[h]] %>% html_nodes("a")
        
        url2<-c(url2,as.character(resultslist[[h]][grepl(gsub(" ","%20",synomyms[h]),resultslist[[h]])]))
        
      }
    }
    else {
      results <- webpagelist[[1]] %>% html_nodes("a")
      url2<-as.character(results[grepl(gsub(" ","%20",synomyms),results)])
    }
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
          use2[j]<-paste(gsub("%20", " ", substring(urlR,124,endNum2)), ": No info.", sep="")
        }
        else{
          out2<-gsub("\n"," ",substring(use,45,nchar(use)-7))
          endNum2<-gregexpr(",",urlR3)[[1]]
          endNum2<-endNum2[length(endNum2)]-1
          use2[j]<-paste(gsub("%20", " ", substring(urlR3,131,endNum2)), ": ",out2, sep="")
          
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


##### Example #####

spList<-c("Euterpe edulis", "Mangifera indica", "Anacardium occidentale")

data_uses<-mansfeld_extractor(species = spList,
                              file.name = "C:/Users/gmazz/Desktop/test.txt")


