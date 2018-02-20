
rm(list = ls())
if(Sys.info()['user'] == "cindycheng"){
source('/Users/cindycheng/Dropbox/Documents/SNIS/Code/setup.R')
}

if(Sys.info()['user'] == "tranganhdo"){source('Users/tranganhdo/Dropbox/snis/Code /Users/tranganhdo/Dropbox/snis/Code')
}
library(tidyr)

# ----------------------------------------------------------------------------
# load data

covData = read.csv(paste0(pathData, '/covData.csv'), stringsAsFactors = F)


# clean/subset data
covData[covData$country == 'China' & covData$year <= 1978,'country']  = 'Taiwan'
covData[covData$country == 'Vietnam' & covData$year <= 1975,'country']  = 'South Vietnam'
covData = covData[-which(covData$Folder == 'CurrentForthcoming'),]


# Make csvs to and handcode number and type of participants for China and Vietnam
# write.csv(covData[covData$country == 'China' & covData$attendance>0, c('Folder', 'doc', 'attendance', 'comments')], file = paste0(pathData, '/Codex/ChinaParticipants.csv'), row.names = FALSE)
# write.csv(covData[covData$country == 'Vietnam' & covData$attendance>0, c('Folder', 'doc', 'attendance', 'comments')], file = paste0(pathData, '/Codex/VietnamParticipants.csv'), row.names = FALSE)

# ----------------------------------------------------------------------------
# reshape and clean data
 
# China
chinaPart = read.csv( file = paste0(pathData, '/Codex/ChinaParticipants.csv'), stringsAsFactors = F, skip = 1) 
chinaPart = chinaPart[-which(chinaPart$X == 'False Positive'),]
 

# rename variables so that can reshape them properly later
names(chinaPart)[9:73] = unlist(lapply(str_split(names(chinaPart)[9:73], "_"), function(x) { paste0(x[2], '_', x[1])}))
names(chinaPart)[9:73] = paste0('n', names(chinaPart)[9:73])
names(chinaPart)[74:97] = unlist(lapply(str_split(names(chinaPart)[74:97], "_Priv_"), function(x) { paste0(x[2], 'Priv_', x[1])}))
names(chinaPart)[74:97] = paste0('n', names(chinaPart)[74:97])
 

 
chinaPart_1_Long= chinaPart[, 1:73] %>% 
	gather(v, value, n1_Name:n13_GovDummy) %>%
	separate(v, c('label', 'col')) %>%
	arrange(doc) %>%
	spread(col, value)
 


chinaPart_1_Long = do.call(rbind, lapply(split(chinaPart_1_Long, chinaPart_1_Long$doc), function(x){
	if (all(is.na(x$GovDummy)|x$GovDummy=='')==T){
		x = x[x$label=='n1',]
	}else if ( length(which(is.na(x$GovDummy)))>0) {
	x = x[-which(is.na(x$GovDummy)),]
}
	else if (length(which(is.na(x$GovDummy)))==0){
		x = x
	}
} 
))
 



chinaPart_1_Long$label = as.numeric(gsub('n', '', chinaPart_1_Long$label))
chinaPart_1_Long = chinaPart_1_Long[order(chinaPart_1_Long$Folder, chinaPart_1_Long$doc, chinaPart_1_Long$label),]
chinaPart_1_Long[intersect(grep("Professor", chinaPart_1_Long$Job), which(chinaPart_1_Long$GovDummy == 0)), c('GovDummy')] = 1

head(chinaPart_1_Long)
# aggregate participation in government delegation
chinaPart_1_Long$GovDummy[which(chinaPart_1_Long$GovDummy == 2)] = NA
chinaPart_1_Long$GovDummy= as.numeric(chinaPart_1_Long$GovDummy)


chinaPart_1_Agg1 = chinaPart_1_Long[which(is.na(chinaPart_1_Long$total_participants_gov)),]


chinaPart_1_Agg1 = do.call(rbind, lapply(split(chinaPart_1_Agg1, chinaPart_1_Agg1$doc) , function(x){
	x$total_participants_gov = dim(x)[1]
	x$private_participants_gov = length(which(x$GovDummy==0))
	x$total_participants_priv =0
	return(x[1, 1:8])
}))


chinaPart_1_Agg2 = chinaPart_1_Long[-which(is.na(chinaPart_1_Long$total_participants_gov)),]
chinaPart_1_Agg2= chinaPart_1_Agg2[-which(duplicated(chinaPart_1_Agg2$doc)),1:8]

china_Agg = rbind(chinaPart_1_Agg1, chinaPart_1_Agg2)
china_Agg$country = "China"


#--------------------------------

# Statistics

# number of academics
chinaPart_1_Long[grep('Prof|prof|Research|research', chinaPart_1_Long$Job), c('Job', 'Institution')] # 28; 'Chief of Scinetici Research Division and Engineer not counted'

# total number of participants
sum(china_Agg$total_participants_gov)

# total number of private participants
sum(china_Agg$private_participants_gov, na.rm = T)
sum(china_Agg$total_participants_priv, na.rm = T)


# 28/2162
# c(105+28)/2162

# 28+28+105

# 2162-161
# 2001/2162


# # select and reshape participation in private delegation
# chinaPart_2 = chinaPart[c(which(chinaPart$doc == 'Fal95_15e.txt'), c(which(chinaPart$doc == 'al97_23Ae.txt')+1): dim(chinaPart)[1]),]
 
# chinaPart_2_Long= chinaPart_2[, c(1:8, 74:97)] %>% 
# 	gather(v, value, n1Priv_Name:n6Priv_City) %>%
# 	separate(v, c('label', 'col')) %>%
# 	arrange(doc) %>%
# 	spread(col, value)


# chinaPart_2_Long = chinaPart_2_Long[-which(chinaPart_2_Long$total_participants_priv == 0|is.na(chinaPart_2_Long$total_participants_priv)),]
# chinaPart_2_Long = chinaPart_2_Long[-which(chinaPart_2_Long$Name == ''),]

# names(chinaPart_2_Long)[10:13] = paste0(names(chinaPart_2_Long)[10:13], '_Priv')
 

# Vietnam

vietPart = read.csv( file = paste0(pathData, '/Codex/VietnamParticipants.csv'), stringsAsFactors = F) 
vietPart = vietPart[-which(vietPart$X %in% c("false positive", "possibly a mistake, address is in Mexico")),]
vietPart = vietPart[-which(is.na(vietPart$Folder)),]
vietPart$doc[which(vietPart$doc == ' .txt')]= 'Fal97_33e.txt' # looks like this doc name got accidentally manually deleted
 
names(vietPart) = gsub('_', '_n', names(vietPart))
names(vietPart)[6:105] = unlist(lapply(str_split(names(vietPart)[6:105], "_"), function(x) { paste0(x[2], '_', x[1])}))


vietPart_Long = vietPart %>% 
	gather(v, value, n1_Name:n20_GovDummy) %>%
	separate(v, c('label', 'col')) %>%
	arrange(doc) %>%
	spread(col, value)

vietPart_Long = vietPart_Long[-which(vietPart_Long$Name == ''),] 
vietPart_Long$label = as.numeric(gsub('n', '', vietPart_Long$label))
vietPart_Long = vietPart_Long[order(vietPart_Long$Folder, vietPart_Long$doc, vietPart_Long$label),]



viet_Agg = do.call(rbind, lapply(split(vietPart_Long, vietPart_Long$doc) , function(x){
	x$total_participants_gov = dim(x)[1]
	x$private_participants_gov = length(which(x$GovDummy==0))
	return(x[1, c(1:5, 12, 13)])
}))

viet_Agg$total_participants_priv = 0
viet_Agg$country = "Vietnam"

# ----------------------------------------
# merge China and Vietnam
chinaVietnam_Agg = rbind(china_Agg, viet_Agg)
 
save(chinaVietnam_Agg, file = paste0(pathData, '/Codex/chinaVietnamParticipantsClean.rda'))


