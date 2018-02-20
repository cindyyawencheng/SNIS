if(Sys.info()['user'] == "cindycheng"){
source('/Users/cindycheng/Dropbox/Documents/SNIS/Code/setup.R')
}


##################################


##################################


gaviPart = read.csv(paste0(pathData, '/GAVI/gavi_boardmeeting_participation.csv'), stringsAsFactors = F)

 

gaviPart2 = gaviPart
gaviPart2$Start.Date
gaviPart2[gaviPart2$Country == 'China' & gaviPart2$Location == 'Phnom Penh, Cambodia', 'Representative.Dummy'] = 3
gaviPart2[gaviPart2$Country == 'Vietnam' & gaviPart2$Start.Date == "7-Jul-11", 'Representative.Dummy'] = 2


gaviPart2 = gaviPart2[-which(gaviPart2$Country == 'China' & gaviPart2$Location == 'Phnom Penh, Cambodia' & gaviPart2$First.Name %in% c('Meng', 'Yang')), ]


gaviPart2 = gaviPart2[-which(gaviPart2$Country == 'Vietnam' & gaviPart2$Start.Date == "7-Jul-11" & gaviPart2$Last.Name %in% c('Nguyen')), ]


gaviPart2$Start.Date = base:::as.Date(gaviPart2$Start.Date, "%d-%b-%y" )
 

gaviPart2 = gaviPart2[-which(is.na(gaviPart2$Start.Date)),]
gaviPart2$RegretsRev = ifelse(gaviPart2$Representative.Dummy == 0 & gaviPart2$Regrets == 0, 'Not Invited' ,
  ifelse(gaviPart2$Representative.Dummy == 0 & gaviPart2$Regrets == 1, 'Sends Regrets', 'Attended'))


gaviPart2$Country[gaviPart2$Country == 'Independent Individual'] = c('Independent Individual (China)')


pdf(paste0(pathGraphics, '/GAVIParticp.pdf'))
print(ggplot(gaviPart2, aes(x = Start.Date) )+
geom_point(aes(y = Representative.Dummy, colour = Country, shape = RegretsRev, size = 4, alpha = .05) )+ 
geom_line(aes(y = Representative.Dummy ,colour = Country), size = 1)+
xlab('Board Meeting Date')+
ylab('Number of Participants')+ 
scale_x_date(breaks = gaviPart2$Start.Date, labels = format(gaviPart2$Start.Date, "%d %b %Y" ) )+ 
theme(axis.text.x = element_text(angle=90),  axis.title=element_text(size=14))+
    theme(legend.position="bottom")+  labs(colour='Country')+
     labs(shape='Status')+
    scale_alpha(guide = 'none')+
    scale_size(guide = 'none'))
dev.off()
 


 


 

##################################


##################################
setwd(paste0(pathData, '/GAVI'))


gavi = read.csv('All-Countries-Commitments-and-Disbursements.csv', 
				stringsAsFactors = F,
				skip = 4)


gavi2 = gavi[c(seq(grep('China', gavi$Country)[1], grep('China', gavi$Country)[2]-1), 
			seq(grep('Vietnam', gavi$Country)[1], grep('Vietnam', gavi$Country)[2]-1)),]

gavi2$Country[1:3]= 'China'
gavi2$Country[4:length(gavi2$Country)] = 'Vietnam'

gavi3 = reshape(gavi2[,-which(names(gavi2) %in% c('High.Level.Category', 'Grand.Total', 'X', 'X.1', 'Region')) ], varying = c(paste('X', 2001:2020, sep = '')), v.names = 'committment', 
				timevar = 'year', times = c(2001:2020), direction = 'long' )


gavi3$committment = as.numeric(gsub(',', '',  gavi3$committment))

gavi3[c(which(gavi3$Country == 'Vietnam' & gavi3$Sub.category == 'HSS' & gavi3$year == 2011), 
		which(gavi3$Country == 'Vietnam' & gavi3$Sub.category == 'INS' & gavi3$year == 2005),
		which(gavi3$Country == 'Vietnam' & gavi3$Sub.category == 'Vaccine Introduction Grant' & gavi3$year %in% c(2003:2006, 2009:2012, 2014) )	),'committment'] = 0 

gavi3$categoryFullName = gavi3$Sub.category

unique(gavi3$Sub.category)
gavi3[which(gavi3$Sub.category == "HepB mono" ), 'categoryFullName'] = "Hepatitis B Birth Dose"  
gavi3[which(gavi3$Sub.category == 'HSS'), 'categoryFullName'] = 'Health System Strengthening Support'
gavi3[which(gavi3$Sub.category == 'INS'), 'categoryFullName'] = 'Injection Safety Support'
gavi3[which(gavi3$Sub.category == 'ISS'), 'categoryFullName'] = 'Immunization Services Support'
gavi3[which(gavi3$Sub.category == 'IPV'), 'categoryFullName'] = 'Inactivated Polio Vaccine Support'
gavi3[which(gavi3$Sub.category == "MR - Operational costs" ), 'categoryFullName'] = "Measles-Rubella - Operational costs" 

 
gavi3$categoryFullName = factor(gavi3$categoryFullName,levels = c('Hepatitis B Birth Dose', 'Injection Safety Support', 'Vaccine Introduction Grant', 'Health System Strengthening Support', 'Inactivated Polio Vaccine Support', 'Immunization Services Support',"Measles-Rubella - Operational costs" , 'Measles', 'Measles-Rubella', 'Penta'))
levels(gavi3$categoryFullName) = c('\nHepatitis B\n Birth Dose\n', '\nInjection Safety Support\n', '\nVaccine Introduction \nGrant\n', '\nHealth System \nStrengthening Support\n', '\nInactivated Polio \nVaccine Support\n', '\nImmunization \nServices Support\n',"\nMeasles-Rubella - \nOperational costs\n" , '\nMeasles\n', '\nMeasles-Rubella\n', '\nPentavalent Vaccine\n')
 
 

gavi3 = gavi3[order(gavi3$Country, gavi3$categoryFullName),]
gavi3 = gavi3[-which(gavi3$year>2017),]
 

setwd(pathGraphics)
pdf('gaviGrants.pdf') 
ggplot(gavi3, aes(x=year, y=committment, colour=Country, shape = categoryFullName,
  group=interaction(Country, categoryFullName))) +
  scale_shape_manual(values=1:nlevels(gavi3$categoryFullName),  name="Grant")+ 
  geom_point() + geom_line(name = 'Country')+ 
  theme(legend.position="bottom")	+   
  guides(shape=guide_legend(ncol=3,byrow=TRUE, order = 2), colour = guide_legend(order = 1)) +
  ylab('Committments (USD)')
dev.off()


## Presentation 
setwd(pathGraphics)
pdf('gaviGrantsBeamer.pdf') 
ggplot(gavi3, aes(x=year, y=committment, colour=Country, shape = categoryFullName,
  group=interaction(Country, categoryFullName))) +
  scale_shape_manual(values=1:nlevels(gavi3$categoryFullName),  name="Grant")+ 
  geom_point() + geom_line(name = 'Country')+ 
  guides(shape=guide_legend(ncol=2,byrow=TRUE, order = 2), colour = guide_legend(order = 1)) +
  ylab('Committments (USD)')+
     theme(legend.text=element_text(size=10))
  dev.off()

