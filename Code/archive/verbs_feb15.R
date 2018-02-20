if(Sys.info()['user'] == "cindycheng"){
  source('/Users/cindycheng/Dropbox/Documents/SNIS/Code/setup.R')
}

if(Sys.info()['user'] == "tranganhdo"){
  source('/Users/tranganhdo/Dropbox/Code/setup.R')
}

# get list of directories inside the currect working directory
setwd(pathData)
directories <- paste(paste(pathData, c(list.files()[1:38]), sep= '/') , '/txt/', sep = '')

# set up the data frame that will become the .csv
a <- data.frame(dirname=c(), docs=c(), word.count=c(), k=c(),l=c(), p=c(), w=c(), d=c(), e.CH=c(), a=c(),c=c(), b=c(), a=c(),c=c(), b=c(), b=c(), a=c(),c=c(), b=c(), b=c(), a=c(),c=c(), b=c(), b=c(), a=c(),c=c(), b=c(), b=c(), a=c(),c=c(), b=c(), b=c(), a=c(),c=c(), b=c(), b=c(), a=c(),c=c(), b=c(), b=c(), a=c(),c=c(), b=c(), b=c(), a=c(), b=c())

# iterate through each directory, looking for texts


   for (i in 1: length(directories)){
    cname <- directories[i]
    docs <- Corpus(DirSource(cname))
    
    # Remove punctuation
    docs <- tm_map(docs, removePunctuation)   
    
    for(j in seq(docs))   
    {   
      docs[[j]] <- gsub("/", " ", docs[[j]])   
      docs[[j]] <- gsub("@", " ", docs[[j]])   
      docs[[j]] <- gsub("\\|", " ", docs[[j]])   
    }   
    
    docs <- tm_map(docs, tolower) 
    
    #Removing “stopwords” (common words) that usually have no analytic value.
    length(stopwords("english"))   
    stopwords("english")   
    
    #Remove common word endings
    
    docs <- tm_map(docs, stemDocument)   
    
    #Strip whitespace
    docs <- tm_map(docs, stripWhitespace)   
    docs <- tm_map(docs, stripWhitespace)   
    
    #Tell R to treat preprocessed documents as text documents.
    docs <- tm_map(docs, PlainTextDocument)
    
    c <- c("Viet Nam", "Vietnam", "China", "the delegation of Vietnam", "the delegation of Viet Nam", "the Vietnamese delegation", "the Chinese delegation", "the Peoples Republic of China", "the People's Republic of China")
    
    d <- c("proposed","pointed","noted","disagreed","introduced","informed","had reservations","stressed","stated","expressed","supported","indicated","emphasized","drew attention to","felt","considered","reserved","opposed","suggested","questioned","posed","requested","presented","underlined")
    
    g <- c("was the opinion of", "proposal by", "proposal of", "proposal from", "comments of", "working group led by")
    
    f <- apply(format(expand.grid(c,d)), 1, paste, collapse=" ")
    l <- apply(format(expand.grid(g,c)), 1, paste, collapse=" ")
    matches <- c(f,l)
    
    corp <- Corpus(VectorSource(docs))
    
    for (k in 1:length(docs)){
      b <- apply_as_df(docs[k], termco, match.list=matches)
      full.row <- b$raw
      full.row$dirname <- directories[i]
      full.row$docs <- k
      print(full.row)
      a <- rbind(a, full.row)
    }
   }

#done working on directories


write.csv(a, "/Users/tranganhdo/Dropbox/Data/Codex/verbs_Feb16.csv") 
verbs <- read.csv("/Users/tranganhdo/Dropbox/Data/Codex/verbs_Feb16.csv")
########################################################
# Open verbs.xlsx
library(xlsx)
mydata <- read.xlsx("/Users/tranganhdo/Dropbox/Data/Codex/verbs.xlsx", 1)

# get the index of column names that have "Viet" and "China"
indx.VN <- grepl('Viet', colnames(mydata))
indx.C <- grepl('Chin', colnames(mydata))

# and use that to see how many times C/VN are mentioned in each meeting in total
mydata$sum.VN <- rowSums(mydata[indx.VN])
mydata$sum.C <- rowSums(mydata[indx.C])

write.csv(mydata$sum.VN, "/Users/tranganhdo/Downloads/sumVN.csv") 
write.csv(mydata$sum.C, "/Users/tranganhdo/Downloads/sumC.csv") 

sum(mydata$sum.VN) #23
sum(mydata$sum.C) #254


########################################################
# Example to extract sentences that contain multiple words: http://stackoverflow.com/questions/31535154/how-to-extract-sentences-containing-specific-person-names-using-r

para <- c("Opposed as a reformer at Tübingen, he accepted a call to the University of Wittenberg by Martin Luther, recommended by his great-uncle Johann Reuchlin. Melanchthon became professor of the Greek language in Wittenberg at the age of 21. He studied the Scripture, especially of Paul, and Evangelical doctrine. He was present at the disputation of Leipzig (1519) as a spectator, but participated by his comments. Johann Eck having attacked his views, Melanchthon replied based on the authority of Scripture in his Defensio contra Johannem Eckium")

toMatch <- c("Martin Luther", "Paul", "Melanchthon","(?=.*Melanchthon)(?=.*Scripture)")
sentences<-unlist(strsplit(para,split="\\."))
sentences[grep(paste(toMatch, collapse="|"),sentences)]
foo<-function(Match){c(Match,sentences[grep(Match,sentences,perl = T)])}
lapply(toMatch,foo)


