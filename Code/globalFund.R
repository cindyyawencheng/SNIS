setwd('~/Downloads/globalfund')


partList = read.csv('BM_Participants_All.csv', skip = 5, stringsAsFactors = F)


partList2 = partList[grep('China|Viet',partList$Location.of.Organization),]
partList2$Location.of.Organization[which(partList2$Location.of.Organization == 'Viet Nam')] = 'Vietnam'
partList2$count = 1

partList3 = data.frame(partList2 %>% group_by(Meeting, Location.of.Organization) %>% summarise(count = sum(count)))
 

partList4 = rbind(partList3, data.frame(Meeting = c(1, 1, 3, 3, 23, 23, 25, 25, 29, 29), 
										Location.of.Organization = rep(c('China', 'Vietnam'), 5), 
partList4 = partList4[-which(partList4$Meeting == 3 & partList4$count == 1),] 

										count = rep(c(NA), 10)))


vietMissMeeting = setdiff(1:35, partList4[which(partList4$Location.of.Organization == 'Vietnam'),'Meeting' ])

partList5 = rbind(partList4, data.frame(Meeting = vietMissMeeting, Location.of.Organization = 'Vietnam', count = rep(0, length(vietMissMeeting))))



pdf()
ggplot(partList5, aes(x = Meeting, y = count))+
geom_line(aes(colour = Location.of.Organization))+geom_point(aes(colour = Location.of.Organization))+
xlab('Board Meeting')+
ylab('Number of Participants')+
ggtitle('Participation of China and Vietnam in Global Fund Board Meetings')+
 scale_x_continuous(breaks = 1:35)+ 
 theme(axis.text.x = element_text(angle=0))+
    theme(legend.position="bottom")+  labs(colour='Country')



