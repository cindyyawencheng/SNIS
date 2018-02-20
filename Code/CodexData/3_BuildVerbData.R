if(Sys.info()['user'] == "cindycheng"){
  source('/Users/cindycheng/Dropbox/Documents/SNIS/Code/setup.R')
}
options(mc.cores=1) # this option is necssary for the verbgramTokenizer to run

# newest version of tm (07-01) doesn't work with ngrams for whatever reason, so reinstalling old version
install.packages("https://cran.r-project.org/src/contrib/Archive/tm/tm_0.6-1.tar.gz", repos = NULL, type = 'source')
library(tm)
sessionInfo()

# get list of directories inside the currect working directory

directories <- paste(paste(paste(pathData, 'Codex', sep =''), c(list.files()[1:38]), sep= '/') , '/txt', sep = '')
FOLDERS = list.files()[1:37]
directories

# make vector of matches
c <- c("Viet Nam", "Vietnam", "China", "the Peoples Republic of China", "the People s Republic of China") # note I took out "the People's Republic of China because we remove punctuation in the text documents"
d <- c("proposed","pointed","noted","disagreed","introduced","informed","had reservations","stressed","stated","expressed","supported","indicated","emphasized","drew attention to","felt","considered","reserved","opposed","suggested","questioned","posed","requested","presented","underlined")
g <- c("was the opinion of", "proposal by", "proposal of", "proposal from", "comments of", "working group led by")
f <- apply(format(expand.grid(c,d)), 1, paste, collapse=" ")
l <- apply(format(expand.grid(g,c)), 1, paste, collapse=" ")
matches <- c(f,l, c("the delegation of Vietnam", "the delegation of Viet Nam", "the Vietnamese delegation", "the Chinese delegation", "the delegation of China", "the delegation of the Peoples Republic of China", "the delegation of the People s Republic of China"))

matches
 
# create empty list to save results
verbs = list()

# iterate through each directory, looking for texts
ptm <- proc.time() # start time of code
for (i in 1:length(directories)){ 
 
    print(FOLDERS[i])
 
  setwd(paste0(pathData, 'Codex'))
    files = list.files(directories[i])[grep('.txt',list.files(directories[i]))]
    docs <- VCorpus(DirSource(directories[i], encoding = "UTF-8"))
 
 # Remove punctuation
    docs <- tm_map(docs,  toSpace, '[[:punct:]]', '')
    docs <- tm_map(docs, toSpace, '\\/', ' ')
    docs <- tm_map(docs, content_transformer(tolower)) 
    
    #Remove common word endings
    # docs <- tm_map(docs, stemDocument)   
    
    #Strip whitespace
    docs <- tm_map(docs, stripWhitespace)   
 
    #Tell R to treat preprocessed documents as text documents.
     # docs <- tm_map(docs, PlainTextDocument)
    
    verbs[[i]] <-  data.frame(as.matrix(DocumentTermMatrix(docs,
                            control = list(tokenize=VerbgramTokenizer,  dictionary = tolower(matches) , wordLengths= c(1, Inf)))), row.names = NULL)
 
    verbs[[i]]$doc = files
    verbs[[i]]$Folder =  FOLDERS[i]

    #check
    # print(colnames(verbs[[i]]))
}
proc.time() - ptm # end time of code
 
verbsAll = do.call(rbind, verbs)
 
# check
names(verbsAll)
dim(verbsAll)
sum(rowSums(verbsAll[,1:152]))
summary(verbsAll)

# save file
setwd(paste0(pathData, '/Codex'))
write.csv(verbsAll, file = "verbs.csv", row.names = F) 




