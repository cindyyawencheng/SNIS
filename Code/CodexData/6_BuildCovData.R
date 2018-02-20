# combine all covariates

if(Sys.info()['user'] == "cindycheng"){
source('/Users/cindycheng/Dropbox/Documents/SNIS/Code/setup.R')
}


##################################


##################################
setwd(paste0(pathData, '/WDI_csv'))

covWB <- read.csv('WDI_Data.csv', stringsAsFactors = F)
covWB1 = covWB[which(covWB$Indicator.Name %in% c("Population, total", "GDP per capita (constant 2005 US$)",  "GDP at market prices (constant 2005 US$)" )), -which(names(covWB) %in% c("Country.Code", "Indicator.Code"))]
covWB2<-melt(covWB1, id.vars=c("Country.Name", "Indicator.Name"))
covWB3<-reshape(covWB2, idvar=c("Country.Name", "variable"), timevar="Indicator.Name", direction ="wide")
 names(covWB3) <-c("country", "year",  "gdpconstant", "gdpconstantpc",  "pop" ) 
covWB3$year<-gsub("X", "", covWB3$year)


##################################


##################################
setwd(paste0(pathData, "/Polity IV"))
polity2<-read.csv("p4v2014.csv", stringsAsFactors = F) # Polity 2 data
polity2=polity2[,3:ncol(polity2)]

# fix NAs
polity2$xconst[which(polity2$xconst==-88|polity2$xconst==-77|polity2$xconst==-66)]<-NA


polity2<-polity2[, which(names(polity2) %in% c("country", "year", 'cname',  "polity2", "xconst" ))] 


##################################


##################################
setwd(paste0(pathData, "/Codex"))
codexData <- read.csv('codexData.csv', stringsAsFactors = F)

vars = c('Folder', 'doc', 'Meeting', 'Place', 'From', 'To', 
		'reportEN', 'reportFR', 'reportES', 'reportAR', 'reportZH',  'reportRU',
		'comments_V', 'comments_C', 
		'attend_V', 'attend_C',   'attend_allCntries', 'noParticipantInfo')

codexData = codexData[, vars]
codexData$year =  paste0(str_sub(codexData$Folder, 1, 2), str_sub(codexData$To, -2))
codexData$year[which(codexData$year== 'Cu15')] = '2015' 
codexData$year[which(codexData$year== '2099')] = '1999' 
codexData$year = as.numeric(codexData$year) 
  



codexDataC = codexData[, -grep('_V', names(codexData))]
codexDataC$country = 'China'
names(codexDataC)[grep('_C', names(codexDataC))] = c('comments', 'attendance')

codexDataV = codexData[, -grep('_C', names(codexData))]
codexDataV$country = 'Vietnam'
names(codexDataV)[grep('_V', names(codexDataV))] = c('comments', 'attendance')

codexData2 = rbind(codexDataC, codexDataV)

## standardize meetings
codexData2$meetingGroupsStd = gsub("\\d", "", codexData$Meeting)


###################################


###################################
 

load(file = paste0(pathData, '/Codex/chinaVietnamParticipantsClean.rda'))






##################################


##################################

covData = merge(codexData2, covWB3, by = c('country', 'year'), all.x = T)
dim(covData) ; 
covData = merge(covData, polity2, by = c('country', 'year'), all.x = T)
dim(covData) 
covData = merge(covData, chinaVietnam_Agg[, -which(names(chinaVietnam_Agg) %in% c('attendance', 'comments', 'X'))], by = c('Folder', 'doc', "country"), all.x = T)
dim(covData)
  
# fix false positives discovered from chinaVietnamParticipantsClean


covData[which(covData$country == "China" & covData$doc %in% c('Fal81_30e.txt',
															'2Fal87_3e.txt',
															'Fal95_12e.txt',
															'Fal01_23e.txt',
															'13%252Fal03_31e.txt',
															'05%252FAL03_11e.txt',
															'08%252FAL03_16e.txt',
															'%252FREP13_PFVe.txt')),c('attendance')] = 0
covData[which(covData$country == "Vietnam" & covData$doc %in% c('Fal01_34e.txt',
																'13%252Fal03_31e.txt',
																'%252FREP14_FFVe.txt')),c('attendance')] = 0

													 
dim(covData)
write.csv( covData, paste0(pathData, '/Codex/covData.csv'), row.names = F)


















