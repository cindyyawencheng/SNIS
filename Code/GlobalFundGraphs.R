if(Sys.info()['user'] == "cindycheng"){
source('/Users/cindycheng/Dropbox/Documents/SNIS/Code/setup.R')
}


##################################


##################################
# participation
 
setwd(paste0(pathData, '/GlobalFund'))
 

partList = read.csv('BM_Participants_All.csv', skip = 5, stringsAsFactors = F)
partList2 = partList[grep('China|Viet',partList$Location.of.Organization),]
partList2$Location.of.Organization[which(partList2$Location.of.Organization == 'Viet Nam')] = 'Vietnam'
partList2$count = 1

partList3 = data.frame(partList2 %>% group_by(Meeting, Location.of.Organization) %>% summarise(count = sum(count)))
 

partList4 = rbind(partList3, data.frame(Meeting = c(1, 1, 3, 3, 23, 23, 25, 25, 29, 29), 
                                        Location.of.Organization = rep(c('China', 'Vietnam'), 5), 
                                             count = rep(c(NA), 10)))

partList4 = partList4[-which(partList4$Meeting == 3 & partList4$count == 1),] 

                                   

vietMissMeeting = setdiff(1:35, partList4[which(partList4$Location.of.Organization == 'Vietnam'),'Meeting' ])
partList5 = rbind(partList4, data.frame(Meeting = vietMissMeeting, Location.of.Organization = 'Vietnam', count = rep(0, length(vietMissMeeting))))


 
pdf(paste0(pathGraphics, '/globalFundParticp.pdf'))
print(ggplot(partList5, aes(x = Meeting, y = count))+
geom_line(aes(colour = Location.of.Organization))+geom_point(aes(colour = Location.of.Organization))+
xlab('Board Meeting')+
ylab('Number of Participants')+
# ggtitle('Participation of China and Vietnam in Global Fund Board Meetings')+
 scale_x_continuous(breaks = 1:35)+ 
 theme(axis.text.x = element_text(angle=0))+
    theme(legend.position="bottom")+  labs(colour='Country'))
dev.off()



partList2[partList2$Location.of.Organization == 'Vietnam', c('Role', 'Meeting')]
partList[partList2$Meeting == '36',]



head(partList)

table(partList$Role)


##################################


##################################
setwd(paste0(pathData, '/WHO'))


# e_prev_100k = Estimated prevalence of TB (all forms) per 100 000 population
# e_inc_100k = Estimated incidence (all forms) per 100 000 population
tb = read.csv('TB_burden_countries_2016-03-05.csv', stringsAsFactors = F)
tb[which(tb$country == 'Viet Nam'), 'country'] = 'Vietnam'

tb2 = tb[which(tb$country %in% c('China', "Vietnam")),]

setwd(pathGraphics)
pdf('tbPrevalenceNum.pdf')
ggplot(tb2, aes(x = year, y =e_prev_num,  group = country)) +
    geom_line(aes(colour = country), lwd = 1) +
    geom_ribbon(aes(ymin=e_prev_num_lo,ymax=e_prev_num_hi),alpha=0.3) +
    ylab('Estimated Prevalence Number') +
    theme(legend.position="bottom")
dev.off()

pdf('tbPrevalence100k.pdf')
ggplot(tb2, aes(x = year, y =e_prev_100k,  group = country)) +
    geom_line(aes(colour = country), lwd = 1)  +
    geom_ribbon(aes(ymin=e_prev_100k_lo,ymax=e_prev_100k_hi),alpha=0.3) +
    ylab('Estimated Prevalence per 100k people') +
    theme(legend.position="bottom")
dev.off()

pdf('tbIncNum.pdf')
ggplot(tb2, aes(x = year, y =e_inc_num,  group = country)) +
    geom_line(aes(colour = country), lwd = 1) +
    geom_ribbon(aes(ymin=e_inc_num_lo,ymax=e_inc_num_hi),alpha=0.3) +
    ylab('Estimated Incidence Number') +
    theme(legend.position="bottom")
 dev.off()

pdf('tbInc100k.pdf')
ggplot(tb2, aes(x = year, y =e_inc_100k,  group = country)) +
    geom_line(aes(colour = country), lwd = 1) +
    geom_ribbon(aes(ymin=e_inc_100k_lo,ymax=e_inc_100k_hi),alpha=0.3) +
    ylab('Estimated Incidence per 100k people') +
    theme(legend.position="bottom")
  dev.off()

# setwd(paste0(pathData, '/WDI_csv'))

# covWB <- read.csv('WDI_Data.csv', stringsAsFactors = F)
# unique(covWB$Indicator.Name[ grep("tuberculosis", covWB$Indicator.Name)])
# tb = covWB[which(covWB$Indicator.Name %in% c("Incidence of tuberculosis (per 100,000 people)" )), -which(names(covWB) %in% c("Country.Code", "Indicator.Code"))] 
# tb2<-melt(tb, id.vars=c("Country.Name", "Indicator.Name"))
# tb3<-reshape(tb2, idvar=c("Country.Name", "variable"), timevar="Indicator.Name", direction ="wide")
# names(tb3) <-c("country", "year",  "tb" ) 
# tb3$year<-gsub("X", "", tb3$year)
# tb4 = tb3[which(tb3$country %in% c('China', 'Vietnam')),] 
# setwd(pathGraphics)
# pdf('tbIncidence.pdf')
# ggplot(tb4[which(tb4$year>1989),], aes(x = year, y =tb,  group = country)) +
#     geom_line(aes(colour = country), lwd = 1) +
#     ylab('Incidence of tuberculosis (per 100,000)')
# dev.off()
#  



##################################


##################################
# malaria


malariaCasesChina = data.frame(country = 'China',
						year = c( 2000, 2005, 2010, 2013),
						malCase = c(29000, 23000, 5900, 4800),
						malCase_hi = c(36000, 25000, 6300, 5200),
						malCase_low = c(22000, 21000, 5200, 4300))

malariaCasesViet = data.frame(country = 'Vietnam',
						year = c( 2000, 2005, 2010, 2013),
						malCase = c(200000, 39000, 25000, 23000),
						malCase_hi = c(250000, 47000, 29000, 27000),
						malCase_low = c(160000, 32000, 22000, 2000))


malariaCases = rbind(malariaCasesChina, malariaCasesViet)

setwd(pathGraphics)
pdf('malariaCases.pdf')
ggplot(malariaCases, aes(x = year, y =malCase,  group = country)) +
    geom_line(aes(colour = country), lwd = 1) +
    geom_ribbon(aes(ymin=malCase_low,ymax=malCase_hi),alpha=0.3) +
    ylab('Estimated Number of Malaria Cases') +
    theme(legend.position="bottom")
dev.off()

