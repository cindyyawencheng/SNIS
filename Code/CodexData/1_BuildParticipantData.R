
options(mc.cores=1) # necessary for getting the biggrams to work : http://stackoverflow.com/questions/17703553/bigrams-instead-of-single-words-in-termdocument-matrix-using-r-and-rweka

source('/Users/cindycheng/Dropbox/Documents/SNIS/Code/setup.R')
Sys.setenv(LANG = "EN")
##########################################

##########################################
# Clean Country Names

setwd(pathData)
members = read.csv(paste0(pathData, '/Codex/memberStates.csv'), stringsAsFactors = F)
cntries = gsub("\\(|\\)|Plurinational State of|Islamic Republic of|People's Democratic Republic|Federated States of|Bolivarian Republic of|The former Yugoslav Republic of", 
			'', members$Member)
cntries = str_trim(cntries)
cntries = cntries[-c(44, 70, 71)] # remove "C<U+0099>te d'Ivoire" and Guina
cntries[which(cntries == "Democratic People's Republic of Korea")] = "Democratic Peoples Republic of Korea"
cntries[which(cntries == "United States of America")] = "United States"
cntries[which(cntries == "United Republic of Tanzania")] = "Tanzania"
cntries= c(cntries, 'Vietnam', "Washington", "Ivoire",'ivory', 'Bangkok', 'GUINEA T', 'Guinea Bissau', 'Korea', 'Congo', 'usa', 'Cape Verde', 'Czechoslovakia', 'yugoslavia', 'russia', 'USSR') # Guinea T is to scrape for instance in which Guinea is followed by 'telephone number'. This is because Guinea by itself can match to either Guinea, Paupa New Guina or Equatoral Guinea or Guinea Bissau

# Clean Observer Names
setwd(paste0(pathData, '/Codex'))
observers =  read.csv('observers.csv', stringsAsFactors = F, encoding = 'Windows-1252' )
acronyms = gsub('-|\\/', '', observers$acronym)
acronyms = c(acronyms, 'FOODDRINK EUROPE')

obsvrs = gsub("\\\216|\\\227|\\\207|\\s*\\([^\\)]+\\)|\\-|\\\217|\\\234|\\\222|UNCTAD/WTO| e. V.|l'U.E.  CEP| G.C.C.|\\/|\\'|\\&", '',  observers$fullName)
obsvrs = c(obsvrs, 'FOODDRINK EUROPE') 


startCountryWords = c("LIST OF PARTICIPANTS DELEGATES AND OBSERVERS",
              'LISTE DE PARTICIPANTS',
              'LISTE DES PARTICIPANTS',
              'LIST OF PARTICIPANTS1',
              'Liste de participants',
              'LISTA DE PARTICIPANTES',
              'LISTE DE S PARTICIPANTS',
              'Liste des participants',
              'Appendix I LIST OF PARTICIPANTS CHAIRPERSON',
              'LISTE DES PARTICIPAN1S')

startCountryWords2 = c('APPENDIX I LIST OF PARTICIPANTS',
               'APPENDIX 1 LIST OF PARTICIPANTS',
                'LIST OF PARICIPATING DELEGATES',
                'LIST OF PARTICIPATING DELEGATES ADVISERS AND OBSERVERS',
                'List of Participants:')


startCountryWords3 = c('APPENDIX I: LIST OF PARTICIPANTS',
              'ATTENDANCE AT THE ',
              'ATTENDANCE AT TBE ',
              'The Executive Committee was presided over by the Chairman',
              'Committee was attended by the following members')

startCountryWords4 = c('LIST OF PARTICIPANTS',
                        'presided over by the Chairman')

startCountryWords5= c('List of Participants',
                      'List of Delegates')
 
startCountryWords6 = c('List of participants',
                      'Experts from the following countries participated in the meeting',
                      'At the meeting Government experts and advisers from',
                      'Delegates from the. following countries participated in the meeting',
                      'The meeting was attended by Government experts and advisers from|day. Representatives from 13 countries - ',
                      'Experts from the following ECE and',
                      'At the Meeting Government experts and advisers from the following',
                      'and attended by delegates and observers from the following Members',
                      'Committee was attended by the following members',
                      'The Committee wae attended by the following members',
                      'Government experts and observers from the following countries attended the meeting')

endCountryWords = c('ORGANISATIONS ORGANIZACIONES',
                    'INTERNATIONAL ORGANIZATIONS',
                    'OBSERVER ORGANIZATIONS',
                    'INTERNATIONAL GOVERNMENTAL ORGANIZATIONS',
                    'INTERNATIONAL NON-GOVERNMENTAL ORGANIZATIONS',
                    'UNITED NATIONS AND OTHER RELATED ORGANIZATIONS',
                    'PARTICIPANTS AND OFFICERS OF THE COMMISSION',
                    'OBSERVER COUNTRY',
                    'OBSERVERS Mr VC')
endCountryWords2 = c('SUMMARY OF POINTS FOR ACTION BY GOVERNMENTS',
                    'Appendix II',
                    'APPENDIX II',
                    'Obervers',
                    'Observers',
                    'OBSERVERS',
                    'OBSERVER COUNTRIES')


startIOWords= c('OBSERVERS',
                'Obervers',
                'ORGANISATIONS ORGANIZACIONES',
                'OBSERVER ORGANISATIONS',
                'OBSERVER ORGANIZATIONS',
                'INTERNATIONAL GOVERNMENTAL ORGANIZATIONS',
                'INTERNATIONAL NON-GOVERNMENTAL ORGANIZATIONS',
                'UNITED NATIONS AND OTHER RELATED ORGANIZATIONS',
                'OBSERVER COUNTRIES') 
		
startIOWords2 = c('INTERNATIONAL ORGANIZATIONS')

endIOWords = c('Appendix II',
                  'APPENDIX II', 
               'Apologies for absence were received from')
		
endIOWords2 = c('Appendix A',
                'ANNEX II')


##########################################

##########################################
Sys.setlocale('LC_ALL','C') 

#Fal70_24e.txt reportID   1970                 

setwd(paste0(pathData, '/Codex'))
FOLDERS = list.files()[1:38]
  
# set empty vectors to save participant information and information about problematic files
problemFiles = list()
PARTCIPANTSLIST = list()
OBSRVRSLIST = list()

 
for (folder in 1:length(FOLDERS)){

	print(paste('Now starting in folder', FOLDERS[folder], sep = ' '))

	# set working directory to where txt files are
	setwd(paste0(pathData, '/Codex/', FOLDERS[folder], '/txt'))
	files = list.files()
  files = files[grep('txt', files)]
  
 
	countryTxtVector = c()
	IOTxtVector = c()
 
# Subset each txt file to only have text for list of participants and put it in a vector for each folder year
	for ( f in 1:length(files)) {
	 
 
		print(paste(c("Now subsetting file", f, ':', files[f]), collapse = ' ' ))

    doc = read.table(files[f], sep = '\t', stringsAsFactors = F, quote = '', row.names = NULL)
    doc[,1] = gsub('[[:punct:]]+','',doc[,1])
    doc = paste(unlist(doc, use.names = FALSE), collapse = ' ')
 
    
		# catch files that were not properly converted
		# problemFiles[[as.character(folder)]][f] = tryCatch(read.table(files[f], sep = '\t', stringsAsFactors = F, quote = ''))
		# names(problemFiles[[as.character(folder)]] [f]) = files[f]

		### extract participant list for countries 

		# stagger the way in which you select for the beginning of the participant list in order of accuracy. 
 		# i.e. 'List of participants' is the last of these 'if' statements because it is not uncommon for a document to make reference to the 'list of participants' in a sentence --- as such you will grab the start of the list too early
	
    print('    Selecting country participants...')
    charStartPos = c()
     
    if(length(unlist(stri_locate_all(pattern = paste(startCountryWords, collapse = '|'), doc ,regex = TRUE))!=0)){
      charStartPos = c(t(stri_locate_all(pattern = paste(startCountryWords, collapse = '|'), doc, regex = TRUE)[[1]]))}
   
     if(length(charStartPos)==0){
      charStartPos = c(t(stri_locate_all(pattern = paste(startCountryWords2, collapse = '|'), doc, regex = TRUE)[[1]]))}
    
       if(length(charStartPos)==0){
      charStartPos = c(t(stri_locate_all(pattern = paste(startCountryWords3, collapse = '|'), doc, regex = TRUE)[[1]]))}
    
     if(length(charStartPos)==0){
      charStartPos = c(t(stri_locate_all(pattern = paste(startCountryWords4, collapse = '|'), doc, regex = TRUE)[[1]]))}
    
     if(length(charStartPos)==0){
      charStartPos = c(t(stri_locate_all(pattern = paste(startCountryWords5, collapse = '|'), doc, regex = TRUE)[[1]]))}
    
     
    if(length(charStartPos)==0){
      charStartPos = c(t(stri_locate_all(pattern = paste(startCountryWords6, collapse = '|'), doc, regex = TRUE)[[1]]))}
    
    if(length(charStartPos)==0){
       charStartPos = c(1, 20)}
 
    startCountryWord = str_sub(doc, charStartPos[c(which.max(charStartPos)-1)], max(charStartPos))
  startCountryTxt = paste(unlist(str_split(doc,startCountryWord)[[1]][-1]), collapse = ' ')
 
  charEndPos = c()
  endCountryWord = c()
  
      if(length(unlist(stri_locate_all(pattern = paste(endCountryWords, collapse = '|'), startCountryTxt ,regex = TRUE))!=0)){
      charEndPos =  c(t(stri_locate_all(pattern = paste(endCountryWords, collapse = '|'), startCountryTxt, regex = TRUE)[[1]]))}
 
   if(length(charEndPos)==0){
      charEndPos =  c(t(stri_locate_all(pattern = paste(endCountryWords2, collapse = '|'), startCountryTxt, regex = TRUE)[[1]]))}
    
      if(length(charEndPos)==0){
        endCountryWord = str_sub(startCountryTxt, start = -50) }
 
      if(length(charEndPos)!=0){
      endCountryWord = str_sub(startCountryTxt, charEndPos[c(which.max(charEndPos)-1)], max(charEndPos)  )}
 
  countryTxt = str_split(startCountryTxt,endCountryWord)[[1]][1]
  rawIOTxt = str_split(startCountryTxt,endCountryWord)[[1]][2]
 
 print('    Selecting IO participants...')
    ioStartPos = c()
     
    if(length(unlist(stri_locate_all(pattern = paste(startIOWords, collapse = '|'), rawIOTxt ,regex = TRUE))!=0)){
      ioStartPos = c(t(stri_locate_all(pattern = paste(startIOWords, collapse = '|'), rawIOTxt ,regex = TRUE)[[1]])) }
    
    if(length(ioStartPos)==0){
            ioStartPos = c(t(stri_locate_all(pattern = paste(startIOWords2, collapse = '|'), rawIOTxt ,regex = TRUE)[[1]]))}
    
    
     startIOWord = substr(rawIOTxt, ioStartPos[1], ioStartPos[2])
     IOStartTxt = str_split(rawIOTxt,startIOWord)[[1]][2]

    ioEndPos = c()
   
		if(length(unlist(stri_locate_all(pattern = paste(endIOWords, collapse = '|'), IOStartTxt ,regex = TRUE))!=0)){
      ioEndPos = c(t(stri_locate_all(pattern = paste(endIOWords, collapse = '|'), IOStartTxt ,regex = TRUE)[[1]])) }
    
    if(length(ioEndPos)==0){
            ioEndPos = c(t(stri_locate_all(pattern = paste(endIOWords2, collapse = '|'), IOStartTxt ,regex = TRUE)[[1]]))}
 
    
     endIOWord = substr(IOStartTxt, ioEndPos[1], ioEndPos[2])
     IOTxt = str_split(IOStartTxt,startIOWord)[[1]][2]

     
       countryTxt  = sapply(countryTxt ,function(row) iconv(row, "latin1", "ASCII", sub=""))
        IOTxt  = sapply(IOTxt ,function(row) iconv(row, "latin1", "ASCII", sub=""))
  
  # adjust for whether the doc is an agenda or a report
     
  if (is.na(IOTxt)  & is.na(countryTxt) ==FALSE){ # if this is a report, but there is no section for IOs
	    	IOTxt =  countryTxt
	    	IOTxtVector = c(IOTxtVector, paste(IOTxt,
			collapse = ' '))}
	else if (is.na(countryTxt) ) { # if this is an agenda, not a report
			countryTxtVector = c( countryTxtVector, '')
			IOTxtVector = c( IOTxtVector, '')}
	 
	countryTxtVector = c(countryTxtVector, paste(countryTxt, collapse = ' '))

  IOTxtVector = c(IOTxtVector, paste(IOTxt, collapse = ' '))

  
  
	}
	
 
 print('now compiling texts into corpus')
	countryCorpus = VCorpus(VectorSource(countryTxtVector))
 	countryCorpus <- tm_map(countryCorpus, content_transformer(tolower))
	countryCorpus <- tm_map(countryCorpus, toSpace, '\\/', ' ')  # removes punctuations; difference with function removePunctuation is that it allows the subspaced to be '' instead of ' '
	countryCorpus <- tm_map(countryCorpus, toSpace, '[[:punct:]]', '')  # removes punctuations; difference with function removePunctuation is that it allows the subspaced to be '' instead of ' '

	IOCorpus = VCorpus(VectorSource(IOTxtVector))
	IOCorpus <- tm_map(IOCorpus, content_transformer(tolower))  # removes punctuations; difference with function removePunctuation is that it allows the subspaced to be '' instead of ' '
	IOCorpus <- tm_map(IOCorpus, toSpace, '\\/', ' ')  # removes punctuations; difference with function removePunctuation is that it allows the subspaced to be '' instead of ' '
	IOCorpus <- tm_map(IOCorpus, toSpace, '[[:punct:]]', '')  # removes punctuations; difference with function removePunctuation is that it allows the subspaced to be '' instead of ' '


 #which(files %in% c( '2Fal72_3e.txt', 'Fal72_12e.txt' ))
#CNTRIES[, which(files %in% c( '2Fal83_3e.txt', '2Fal83_4e.txt' ))]
## Extract list of partcipants from each txt file and put it in a vector for each folder year
	CNTRIES = as.matrix(TermDocumentMatrix(countryCorpus,
	                           control = list(tokenize=BigramTokenizer, dictionary = tolower(cntries)	 , wordLengths= c(0, Inf))))

	                         
	ACRONYMS =  as.matrix(TermDocumentMatrix(IOCorpus,
	                           control = list(tokenize=BigramTokenizer, dictionary =  tolower(acronyms[-which(duplicated(acronyms))]), wordLengths= c(1, Inf)))) # observers that have one-word names

	OBSRVRS =  as.matrix(TermDocumentMatrix(IOCorpus,
	                           control = list(tokenize=BigramTokenizer, dictionary =  tolower(obsvrs), wordLengths= c(1, Inf)))) # observers that have one-word names
 

	## put countries into data.frame
	CNTRY = data.frame(CNTRIES )

	# put observers into data.frame
	obsOrder = c()
	for ( i in 1:length(acronyms[-which(duplicated(acronyms))])){
	obsOrder[i] = which(   row.names(OBSRVRS) %in% tolower(obsvrs[-which(duplicated(acronyms))])[i] )
	}  # annoyingly, tm alphabetizes the dictionary so you cannot simply add the matrices for OBSRVRS and ACRONYMS but must reorder OBSRVRS first

	OBSRVRS2 = OBSRVRS[obsOrder,]
	OBSRVRS3 = OBSRVRS2 + ACRONYMS

	OBSRVRS4 = data.frame(rbind(OBSRVRS3, OBSRVRS[which(   row.names(OBSRVRS) %in% tolower(obsvrs[which(duplicated(acronyms))])),]))


	print ('make .csv')
	PARTICIPANTS = CNTRY
	PARTICIPANTS2 =  data.frame(t(PARTICIPANTS))
	PARTICIPANTS2$doc = files
	PARTICIPANTS2$Folder = FOLDERS[folder]

	PARTCIPANTSLIST[[folder]] = PARTICIPANTS2

	OBSRVRSLIST[[folder]] = data.frame(t(OBSRVRS4))
	setwd(paste0(pathData, '/Codex/', FOLDERS[folder]))
	write.csv(PARTICIPANTS2, file = paste(c('participants', FOLDERS[folder],  '.csv'), collapse = ''), row.names = F)

}

length(PARTCIPANTSLIST) 
# check 
for ( i in 1:37){
a  = rowSums(PARTCIPANTSLIST[[i]][,1:423] )
names(a) = PARTCIPANTSLIST[[i]][ , 424]
print(FOLDERS[i])
print(a)
}


## save all participants
setwd(paste0(pathData, '/Codex'))
PARTICIPANTSALL = do.call(rbind, PARTCIPANTSLIST)
write.csv(PARTICIPANTSALL , file = 'participantsAll.csv', row.names = F)

 dim(PARTICIPANTSALL)
 
setwd(paste0(pathData, '/Codex'))
OBSRVRSALL = do.call(rbind, OBSRVRSLIST)
write.csv(OBSRVRSALL, file = 'observersAll.csv')



