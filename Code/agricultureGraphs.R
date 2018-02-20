if(Sys.info()['user'] == "cindycheng"){
source('/Users/cindycheng/Dropbox/Documents/SNIS/Code/setup.R')
}



#################################################


###############################################
## get region data 
region = read.csv(paste0(pathData, 'FAO/FaostatAreaGroupList.csv'), stringsAsFactors = F )
region2 = region[which(region$Group.name %in% c("Africa" ,"Americas" , "Asia" , "Europe" , "Oceania"  )), ]
region2[which(region2$Group.name %in% c( 'Europe', "Oceania") |region2$Area.name %in% c('United States of America', 'Canada' )), 'Group.name'] = 'Developed Countries'
region2[which(region2$Group.name %in% c("Americas" )), 'Group.name'] = 'Latin America and the Caribbean'
# region2[which(region2$Group.name %in% c("Asia" )), 'Group.name'] = 'Asia (excluding China and Vietnam)'
 
 

region2$cname = panel$cname[match(cname(region2$Area.name), panel$cname)]
region2$cname[which(region2$Area.name == 'Cabo Verde')] = "CAPE VERDE"
region2$cname[which(region2$Area.name == 'Congo')] = "CONGO, REPUBLIC OF"
region2$cname[which(region2$Area.name == 'Czechoslovakia')] = 'CZECH REPUBLIC'
region2$cname[which(region2$Area.name == 'Democratic Republic of the Congo')] = "CONGO, THE DEMOCRATIC REPUBLIC OF"
region2$cname[which(region2$Area.name == 'Libya')] = "LIBYAN ARAB JAMAHIRIYA"
region2$cname[which(region2$Area.name %in% c("Serbia and Montenegro", 'Yugoslav SFR'))] = "SERBIA"
region2$cname[which(region2$Area.name ==  "Viet Nam" )] = "VIETNAM"


region2 = region2[-which(is.na(region2$cname)),]
names(region2)[2] = 'region'
unique(region2$region) 
#################################################


###############################################
## get agricultural production data 
crops = read.csv(paste0(pathData, 'FAO/faoCropsProduction.csv'), stringsAsFactors = F)
crops = crops[-which(crops$ItemName == ''),]

 

cropsAll =	data.frame(crops[, which(names(crops) %in% c('ItemName', 'Year', 'AreaName', 'Value' ))] %>% 
		group_by( AreaName, Year)  %>% 
		summarise(sum(Value, na.rm = T)))


cropsAll$cname = panel$cname[match(cname(cropsAll$AreaName), panel$cname)]
cropsAll$cname[which(cropsAll$AreaName == 'Cabo Verde')] = "CAPE VERDE"
cropsAll$cname[which(cropsAll$AreaName == 'Congo')] = "CONGO, REPUBLIC OF"
cropsAll$cname[which(cropsAll$AreaName == 'Czechoslovakia')] = 'CZECH REPUBLIC'
cropsAll$cname[which(cropsAll$AreaName == 'Democratic Republic of the Congo')] = "CONGO, THE DEMOCRATIC REPUBLIC OF"
cropsAll$cname[which(cropsAll$AreaName == 'Libya')] = "LIBYAN ARAB JAMAHIRIYA"
cropsAll$cname[which(cropsAll$AreaName %in% c("Serbia and Montenegro", 'Yugoslav SFR'))] = "SERBIA"
cropsAll$cname[which(cropsAll$AreaName ==  "Viet Nam" )] = "VIETNAM"

# get rid of all entries that are not countries
cropsAll = cropsAll[-which(is.na(cropsAll$cname)|cropsAll$AreaName %in% c('China, mainland')),]
names(cropsAll)[c(2, 3)] = c('year', 'cropsProduction')


# dupcheck
cropsAll[duplicated(cropsAll[, c('cname', 'year')]),]

#################################################


###############################################
## get livestock data 


livestock = read.csv(paste0(pathData, 'FAO/faoLivestockProduction.csv'), stringsAsFactors = F)
livestock = livestock[-which(livestock$ItemName == ''), which(names(livestock) %in% c('Year', 'AreaName', 'Value' ))]
livestock$cname = panel$cname[match(cname(livestock$AreaName), panel$cname)]
livestock$cname[which(livestock$AreaName == 'Cabo Verde')] = "CAPE VERDE"
livestock$cname[which(livestock$AreaName == 'Congo')] = "CONGO, REPUBLIC OF"
livestock$cname[which(livestock$AreaName == 'Czechoslovakia')] = 'CZECH REPUBLIC'
livestock$cname[which(livestock$AreaName == 'Democratic Republic of the Congo')] = "CONGO, THE DEMOCRATIC REPUBLIC OF"
livestock$cname[which(livestock$AreaName == 'Libya')] = "LIBYAN ARAB JAMAHIRIYA"
livestock$cname[which(livestock$AreaName %in% c("Serbia and Montenegro", 'Yugoslav SFR'))] = "SERBIA"
livestock$cname[which(livestock$AreaName ==  "Viet Nam" )] = "VIETNAM"


# get rid of all entries that are not countries
livestock = livestock[-which(is.na(livestock$cname)|livestock$AreaName %in% c('Australia & New Zealand', 'Southern Africa', 'China, mainland', 'Micronesia')),]

names(livestock)[c(2, 3)] = c('year', 'livestockProduction')



# dupcheck
livestock[duplicated(livestock[, c('cname', 'year')], fromLast = T),]


#################################################


###############################################
## get agricultural trade data 

trade = read.csv(paste0(pathData, 'FAO/faoTrade.csv'), stringsAsFactors = F)
imports = trade[which(trade$ElementName == 'Import Value'),]
imports2 = reshape(imports[, which(names(imports) %in% c('Year', 'AreaName', 'ItemName', 'Value' ))],
	 timevar = c( 'ItemName') ,  idvar = c('AreaName', 'Year'), direction = 'wide')

names(imports2)[2:4] = c('year', 'importsAg', 'importMeat')
 
exports = trade[which(trade$ElementName == 'Export Value'),]
exports2 = reshape(exports[, which(names(exports) %in% c('Year', 'AreaName', 'ItemName', 'Value' ))],
	 timevar = c( 'ItemName') ,  idvar = c('AreaName', 'Year'), direction = 'wide')
names(exports2)[2:4] = c('year', 'exportsAg', 'exportsMeat')
 
trade2 = merge(imports2, exports2, by = c('AreaName', 'year'), all = T) 
 
 
trade2$cname = panel$cname[match(cname(trade2$AreaName), panel$cname)]
trade2$cname[which(trade2$AreaName == 'Cabo Verde')] = "CAPE VERDE"
trade2$cname[which(trade2$AreaName == 'Congo')] = "CONGO, REPUBLIC OF"
trade2$cname[which(trade2$AreaName == 'Czechoslovakia')] = 'CZECH REPUBLIC'
trade2$cname[which(trade2$AreaName == 'Democratic Republic of the Congo')] = "CONGO, THE DEMOCRATIC REPUBLIC OF"
trade2$cname[which(trade2$AreaName == 'Libya')] = "LIBYAN ARAB JAMAHIRIYA"
trade2$cname[which(trade2$AreaName %in% c("Serbia and Montenegro", 'Yugoslav SFR'))] = "SERBIA"
trade2$cname[which(trade2$AreaName ==  "Viet Nam" )] = "VIETNAM"

# check
unique(trade2$AreaName[which(is.na(trade2$cname))])

# remove all non-countries
trade2 = trade2[-which(is.na(trade2$cname)|trade2$AreaName %in% c('Australia & New Zealand', 'Southern Africa', 'China, mainland', 'Micronesia')),]

 # dupcheck
trade2[duplicated(trade2[, c('cname', 'year')], fromLast = T), c('cname', 'AreaName')]

#################################################


###############################################

# merge 
agData = merge(cropsAll[which(cropsAll$year <2014), -1], livestock[, -1], by = c('cname', 'year'), all = T)
dim(agData); agData[duplicated(agData[, c('cname', 'year')]),]
agData  = merge(agData, trade2[, -1], by = c('cname', 'year'), all = T)
dim(agData); agData[duplicated(agData[, c('cname', 'year')]),]

agData$agProduction = rowSums(cbind(agData$cropsProduction, agData$livestockProduction), na.rm = F)
agData$agTrade= rowSums(cbind(agData$importsAg, agData$exportsAg), na.rm = F)

 
### merge with region data
agData2 = merge(agData, region2[, which(names(region2) %in% c('region', 'cname'))], by = 'cname')

# check
which(is.na(agData2$region)) 

# isolate China and Vietnam
agData2[which(agData2$cname %in% c('CHINA')), 'region'] = 'China'
agData2[which(agData2$cname %in% c('VIETNAM')), 'region'] = 'Vietnam'

agData3 =  agData2[,-1] %>% 
		group_by( region,year) %>% 
		summarise_each(funs(sum(., na.rm = TRUE)))
 

agData3$region <- factor(agData3$region, levels = c('China', 'Vietnam', 'Asia', 'Africa', "Latin America and the Caribbean", "Developed Countries"  ))		

agData3 = agData3[order(agData3$region),]


#################################################


###############################################
### Make Stacked Area Graph
# http://stackoverflow.com/questions/5030389/getting-a-stacked-area-plot-in-r


setwd(pathGraphics)

pdf('agProductionShare.pdf')
ggplot(agData3, aes(x = year, y = agProduction, group = region, fill = region)) +
	 geom_area(position = 'fill') + 
	 ylab('Share of crops and livestock produced')+
  	scale_x_continuous(breaks = round(seq(1965, 2010, by = 5),1)) + 
  	theme(legend.position="bottom")+   	
  	scale_fill_manual(values=c(gg_color_hue(6)[1], gg_color_hue(6)[5], gg_color_hue(6)[2], gg_color_hue(6)[3:4], gg_color_hue(6)[6]))+
  	guides(fill=guide_legend(nrow=2))+
  	theme(legend.text=element_text(size=14))
dev.off()


pdf('agTradeShare.pdf')
ggplot(agData3, aes(x = year, y = agTrade, group = region, fill = region)) +
	 geom_area(position = 'fill') + 
	 ylab('Share of crops and livestock traded')+
  	scale_x_continuous(breaks = round(seq(1965, 2010, by = 5),1))+ 
  	theme(legend.position="bottom")	+   
  	  	scale_fill_manual(values=c(gg_color_hue(6)[1], gg_color_hue(6)[5], gg_color_hue(6)[2], gg_color_hue(6)[3:4], gg_color_hue(6)[6]))+
  	guides(fill=guide_legend(nrow=2))+
  	theme(legend.text=element_text(size=14))
dev.off()











