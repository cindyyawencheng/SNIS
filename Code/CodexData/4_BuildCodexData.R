if(Sys.info()['user'] == "cindycheng"){
source('/Users/cindycheng/Dropbox/Documents/SNIS/Code/setup.R')
}

if (Sys.info()['user'] == 'tranganhdo'){
 source('/Users/tranganhdo/Dropbox/Code/setup.R')
}

 ####################################

####################################
# meta data

meta = read.csv(paste0(pathData, 'Codex/metadata.csv' ), stringsAsFactors = F)
 

meta2 = reshape(meta, varying = c('reportID', 'agendaID'), 
                        v.names = 'doc', timevar = 'doctype', 
                        times = c('reportID', 'agendaID'), direction = 'long')


meta3 = meta2[-which(is.na(meta2$doc)|meta2$doc == ''), -length(meta2)]
meta3$doc = gsub('.pdf', '.txt', meta3$doc ) 

# need to grab more of the url in order to be able to differentiate between them later
meta3[which(meta3$doc == "Fal71_18e.txt" & meta3$Folder == 1971), 'doc'] = '05%252Fal71_18e.txt'
meta3[which(meta3$doc == "Fal71_18e.txt" & meta3$Folder == 1970), 'doc'] = '04%252Fal71_18e.txt'


## check difference between meta data and participant/verb data (though in practice, just participant data)
# note these agendas 
meta3[which(meta3$doc %in% setdiff(meta3$doc, participation$doc)),c('Meeting', 'doc', 'Folder', 'CodexError',   'doctype')]

 
participation[which(participation$doc %in% setdiff( participation$doc, meta3$doc)),c('doc', 'Folder')]
# these discrepancies are accounted for here
# duplicate  1972 Fcx73_16e.txt : REPORT
# duplicate in 1972:  1974 714-08%252Fal72_22e.txt REport of the eigth session  of the codex committee on food labelling
# duplicate 16%252Fal89_16e
# appendix IV_Corr_e
# agenda 06%252Fbt06_01e.txt
# duplicate in 2009:  2010 Fal32_42e.txt REPORT OF THE SECOND SESSION OF THE CODEX AD HOC INTERGOVERNMENTAL TASK FORCE ON ANTIMICROBIAL RESISTANCE
# agenda: amr03_01e.txt
# duplicate 0%252FREP13_AFe
# agenda  Fne08_10e.txt


# 1989 l89_35e_Part-II.txt : REport of the First Session of the Codex Committee on Tropical Fresh Fruits and Vegetables



# grab only rows that were not duplicated and are reports, not agendas
meta4 = meta3[which(meta3$CodexError == 0 & meta3$doctype == "reportID"), 
-which(names(meta3) %in% c('EN', 'FR', 'ES', 'AR', 'ZH', 'RU', 'hrefEnAgenda', 'hfrefEnReport'))]
dim(meta4)

####################################

####################################
# participation
participation<-read.csv(paste0(pathData, "Codex/participantsAll.csv"), header=T, stringsAsFactors = F)
participation[which(participation$doc == "2Fal69_3e.txt" &participation$Folder == 1969), -which(names(participation) %in% c('Folder'))] <- 
    participation[which(participation$doc == "2Fal69_3e.txt" &participation$Folder == 1968), -which(names(participation) %in% c('Folder'))] # "2Fal69_3e.txt" from 1968 is more nicely typed than that in 1969 so will use the scraped version from 1968
participation[which(participation$doc == "2Fal69_9e.txt" &participation$Folder == 1969),  -which(names(participation) %in% c('Folder'))] = 
    participation[which(participation$doc == "2Fal69_9e.txt" &participation$Folder == 1968),  -which(names(participation) %in% c('Folder'))] # "2Fal69_9e.txt" from 1968 is more nicely typed than that in 1969 so will use the scraped version from 1968
participation[which(participation$doc == "2Fal95_4e.txt" &participation$Folder == 1995),  -which(names(participation) %in% c('Folder'))] = 
    participation[which(participation$doc == "2Fal95_4e.txt" &participation$Folder == 1991),  -which(names(participation) %in% c('Folder'))] # "Fal71_18e.txt" from 1991 is more nicely typed than that in 1995 so will use the scraped version from 1995

# need to grab more of the url in order to be able to differentiate between them later
participation[which(participation$doc == "Fal71_18e.txt" & participation$Folder == 1971), 'doc'] = '05%252Fal71_18e.txt'
participation[which(participation$doc == "Fal71_18e.txt" & participation$Folder == 1970), 'doc'] = '04%252Fal71_18e.txt'

# create a column that sums the two columns 'viet.nam' and 'vietnam'
participation$viet <- participation$viet.nam + participation$vietnam
 
#test to see if it works
participation[,c('viet.nam','vietnam', 'viet')]



#### sum total number of attendants
# total number of countries
participation$attend_allCntries <- rowSums(sign(participation[, which(names(participation) == 'albania') : which(names(participation) == 'zimbabwe')]))
# participation2$attend_allIOs <- rowSums(sign(participation2[, which(names(participation2) == 'X49th.parallel.biotechnology.consortium') : which(names(participation2) == 'international.society.of.citriculture')]))
# participation2$attend_all <- rowSums(sign(participation2[, which(names(participation2) == 'albania') : which(names(participation2) == 'international.society.of.citriculture')]))



# create column 'attend' indicating whether C/VN attended
participation$attend_V <- ifelse(participation$viet != 0, 1,0)
participation$attend_C <- ifelse(participation$china != 0, 1,0)




 

participation$CodexError = meta3$CodexError[match(paste0(participation$doc, participation$Folder), paste0(meta3$doc, meta3$Folder))]
 
# check
dim(participation)
participation[which(duplicated(participation$doc)|duplicated(participation$doc, fromLast = T)), c('doc', 'Folder', 'CodexError')]

 
####################################

####################################
# verbs
verbs <- read.csv(paste0(pathData, "/Codex/verbs.csv"), header=T, stringsAsFactors = F)
 
# Show the repeat entries
# verbs2<- verbs[-c( 
#                 # codex posts duplicate meeting entries over different Folders
#                 which(verbs$doc == "-01%252Fal65_9e.txt" & verbs$Folder == 1966), 
#                 which(verbs$doc == "-02%252Fal65_2e.txt" & verbs$Folder == 1966), 
#                 which(verbs$doc == "-03%252Fal65_9e.txt" &verbs$Folder == 1965), 
#                 which(verbs$doc == "01%252Fal65_15e.txt" &verbs$Folder == 1966), 

#                 # codex uploads wrong document but uses same url
#                 which(verbs$doc == "2Fal69_3e.txt" & verbs$Folder == 1968), 
#                 which(verbs$doc == "2Fal69_9e.txt" & verbs$Folder == 1968),
#                 which(verbs$doc == "2Fal95_4e.txt" & verbs$Folder == 1991),
#                 which(verbs$doc == "Fal04_33e.txt" & verbs$Folder == 2005),
#                 which(verbs$doc == "al93_24Ae.txt" & verbs$Folder == 1991)), ]
               
# need to grab more of the url in order to be able to differentiate between them later
verbs[which(verbs$doc == "Fal71_18e.txt" & verbs$Folder == 1971), 'doc'] = '05%252Fal71_18e.txt'
verbs[which(verbs$doc == "Fal71_18e.txt" & verbs$Folder == 1970), 'doc'] = '04%252Fal71_18e.txt'


# see how many times C/VN are mentioned in each meeting in total
indx.C <- grepl('chin', colnames(verbs))
indx.V <- grepl('viet', colnames(verbs))
verbs$comments_V <- rowSums(verbs[indx.V])
verbs$comments_C <- rowSums(verbs[indx.C])
 

# total times each countries speaks in all meetings
sum(verbs$comments_V) #23 ; 27 under new coding
sum(verbs$comments_C) #253 ; 449 under new coding



 

verbs$CodexError = meta3$CodexError[match(paste0(verbs$doc, verbs$Folder), paste0(meta3$doc, meta3$Folder))]
 

#check to see if there are any duplicates left
dim(verbs)
verbs[which(duplicated(verbs$doc)|duplicated(verbs$doc, fromLast = T)), c('doc', 'Folder', 'CodexError')]



####################################

####################################

# merge data
codexData <- merge(meta4, verbs, by=c( "Folder", 'doc'), all.x = T) 
dim(codexData);codexData[which(duplicated(codexData[, c('doc', 'Folder')])), c('doc', 'Folder')]
codexData <- merge(codexData, participation, by=c("Folder", 'doc'), all.x = T) 
dim(codexData); codexData[which(duplicated(codexData[, c('doc', 'Folder')])), c('doc', 'Folder')]



# documents for which there is a meeting report but no appendix or list of participants
codexData$noParticipantInfo = c(0)
codexData$noParticipantInfo[which(codexData$doc %in% c('-01%252Fal65_9e.txt',
                                                       '01%252Fal65_22e.txt',
                                                       '02%252Fal65_11e.txt',
                                                       '02%252Fal65_13e.txt',
                                                       '02%252Fal65_21e.txt',
                                                       '03%252Fal66_21e.txt',
                                                       '2Fal66_4-3Rev1e.txt',
                                                       'Fal68_13e.txt',
                                                       'Fal68_21e.txt',
                                                       'Fal69_23e.txt', # note it says it has a list of participants, but then is just a blank page
                                                      '9_18_AnnexII-Ve.txt', # double check to see if they have more than an annex available now CCFFP13 Codex Committee on Fish and Fishery Products
                                                      'Fal95_12e.txt',
                                                      'Fal9913ae.txt', # note it says it has a list of participants, but then is just a blank page
                                                      '01%252Fal65_23e.txt',
                                                      '01%252Fal66_4Ae.txt',
                                                      'Fal70_20e.txt'
                                                      ))] = 1

 
## double check scraping of  participants
codexData[which(codexData$attend_allCntries < 5 & codexData$noParticipantInfo == 0) , c('doc', 'doctype', 'Folder', 'attend_allCntries', 'Meeting')]



### Before 1987, no list of participants given for Executive Committee -- listed in the front
# Fal70_20e.pdf - 1970 - list of participants mentioned as being in the appendix but appendix not in pdf
# Fal69_23e - 1970 - list of participants mentioned but not included
# 2Fal66_4-3Rev1e - 1966 - no list of participants
# 03%252Fal66_21e - 1966 - list of participants (omitted)
# 03%252Fal66_12e - 1966 - full list of participants omitted but some mentioned
# 01%252Fal66_4Ae - 1966 - full list of participants omitted
# 01%252Fal65_23e - 1966 - full list of participants not given
# 02%252Fal65_20e - 1965 - full list of participants omitted
# 02%252Fal65_13e. - 1965 - full list of participants not given
# 02%252Fal65_11e - 1965    f ull list of participants omitted
# 01%252Fal65_22e - 1865 - full list of partcipants omitted
# 01%252Fal65_15e - 1965 - full list of particiants omitted
# -01%252Fal65_9e - 1965 - full list of particiants omitted - 17 countries, 6 IOs
#-01%252Fal65_4e - 1965 - list of participants not given - 16 countries ; unclear how many IOS
# 01%252Fal64_24e - 1965 - list of participants not given - unclear how many countries/IOs
# 01%252Fal64_19e - 1965 list of participants not given - unclear hhow many participants/countries/IOs

setwd(paste0(pathData, '/Codex'))
write.csv(codexData,"codexData.csv", row.names = F)
  
 ### make raw dataset 

# codexDataRaw <- merge(meta3[meta3$doctype == "reportID",], verbs2, by=c( "Folder", 'doc'), all.x = T) 
# dim(codexDataRaw);codexDataRaw[which(duplicated(codexDataRaw[, c('doc', 'Folder')])), c('doc', 'Folder')]
# codexDataRaw <- merge(codexDataRaw, participation, by=c("Folder", 'doc'), all.x = T) 
# dim(codexDataRaw); codexDataRaw[which(duplicated(codexDataRaw[, c('doc', 'Folder')])), c('doc', 'Folder')]
# codexDataRaw$year = as.numeric(substring(codexDataRaw$To, 7, 10))

# setwd(paste0(pathData, '/Codex'))
# write.csv(codexDataRaw,"codexDataRaw.csv", row.names = F)
  

 




