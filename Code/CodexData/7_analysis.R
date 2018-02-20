if(Sys.info()['user'] == "cindycheng"){
source('/Users/cindycheng/Dropbox/Documents/SNIS/Code/setup.R')
}
 
if(Sys.info()['user'] == "tranganhdo"){source('Users/tranganhdo/Dropbox/snis/Code /Users/tranganhdo/Dropbox/snis/Code')
}

Sys.setenv(LANG = "EN")

#----------------------------------------------------

toWide = function(d, c, v){
    wideDF = reshape(d[which(d$country == c), 
                    c('MeetingNum', 'country', 'meetingAbbrev', v)],
                    timevar =  "meetingAbbrev",
                      idvar = c('country', 'MeetingNum'),
                      direction = 'wide' )
      wideDF = wideDF[order(wideDF$MeetingNum),]}


firstDiffPanel = function(data, country, variable){
  wideDF = toWide(d = data, c = country, v = variable)
 

  firstDiff = diff(as.matrix(wideDF[, -c(1,2)]))
  firstDiffWide = cbind(wideDF[, c(1,2)], rbind(firstDiff, rep(NA, dim(firstDiff)[2])))
  firstDiffLong = reshape(firstDiffWide,
                  varying = names(firstDiffWide)[-c(1,2)],
                  v.names = paste0(variable, '_fd'),
                  timevar = 'meetingAbbrev',
                  times = names(firstDiffWide)[-c(1,2)],
                  direction = "long")

  firstDiffLong$meetingAbbrev = gsub(paste0(variable, '.'), "",firstDiffLong$meetingAbbrev )
  firstDiffLong = firstDiffLong[which(paste0(firstDiffLong$meetingAbbrev, firstDiffLong$MeetingNum, firstDiffLong$country) %in% paste0(covData$meetingAbbrev, covData$MeetingNum, covData$country)),]
  
  firstDiffLong$year = covData$year[match(paste0(firstDiffLong$meetingAbbrev, firstDiffLong$MeetingNum), paste0(covData$meetingAbbrev, covData$MeetingNum))]

  firstDiffLong$dum2004 = covData$dum2004[match(paste0(firstDiffLong$meetingAbbrev, firstDiffLong$MeetingNum), paste0(covData$meetingAbbrev, covData$MeetingNum))]
  return(firstDiffLong)

}
 
detrendPanel = function(data, country, variable){

  wideDF = toWide(d = data, c = country, v = variable)
 

  detrendLinear = detrend(as.matrix(wideDF[, -c(1,2)]))
  detrendLinearWide = cbind(wideDF[, c(1,2)],  detrendLinear  )
  detrendLinearLong = reshape(detrendLinearWide,
                  varying = names(detrendLinearWide)[-c(1,2)],
                  v.names = paste0(variable, '_linearDetrend'),
                  timevar = 'meetingAbbrev',
                  times = names(detrendLinearWide)[-c(1,2)],
                  direction = "long")

  detrendLinearLong$meetingAbbrev = gsub("total_participants_gov.", "",detrendLinearLong$meetingAbbrev )
  detrendLinearLong = detrendLinearLong[which(paste0(detrendLinearLong$meetingAbbrev, detrendLinearLong$MeetingNum, detrendLinearLong$country) %in% paste0(covData$meetingAbbrev, covData$MeetingNum, covData$country)),]
  
  detrendLinearLong$year = covData$year[match(paste0(detrendLinearLong$meetingAbbrev, detrendLinearLong$MeetingNum), paste0(covData$meetingAbbrev, covData$MeetingNum))]

   detrendLinearLong$dum2004 = covData$dum2004[match(paste0(detrendLinearLong$meetingAbbrev, detrendLinearLong$MeetingNum), paste0(covData$meetingAbbrev, covData$MeetingNum))]
  return(detrendLinearLong)

}
 
stationarityTestsPanel = function(data, country, variable, 
                diffLogic = FALSE, test = 'adf'){
      wideDF = toWide(d = data, c = country, v = variable)
 
    
      if (diffLogic == TRUE){
        wideDFDiff = apply(wideDF[,-c(1,2)], 2, diff)
        results = do.call(rbind, apply(wideDFDiff, 2, function(x){
          x = as.numeric(x[-which(is.na(x))])
          if (length(x)>7){
            if(length(unique(x))>1){
           if(test == 'adf'){
            tryCatch(adf.test(x, k = 0))}
          else if(test == 'box'){
            Box.test(x, type = "Ljung-Box")}
          else if(test == 'kpss'){
            kpss.test(x, null = 'Trend')}
           }}
                }))
      }


  if (diffLogic == FALSE){
     results = do.call(rbind, apply(wideDF[,-c(1,2)], 2, function(x){
          x = as.numeric(x[-which(is.na(x))])
          if (length(x)>6){
            if(length(unique(x))>1){
            
           if(test == 'adf'){
           adf.test(x)}
          else if(test == 'box'){
            Box.test(x, type = "Ljung-Box")}
          else if(test == 'kpss'){
            kpss.test (x, null = 'Trend')}

         }}
                }))
    }

    results = data.frame(results)
    if(test == 'kpss'){
        results$assess = ifelse(results$p.value <.05, 'trend non stationary', 'trend stationary')}
    if(test == 'adf'){
        results$assess = ifelse(results$p.value <.05, 'stationary', 'unit-root')}

    return(results)
}


 
aggTest = function(dataset, country, var){
  x = dataset[which(dataset$country == country), var]
  if(any(is.na(x))){
    x = x[-which(is.na(x))]
  }

  adfUR1 = adf.test(x)
  adfUR1$assess  = ifelse(adfUR1$p.value <.05, 'stationary', 'unit-root')

 
  adfUR2 = adf.test(diff(x))
  adfUR2$assess  = ifelse(adfUR2$p.value <.05, 'stationary', 'unit-root')


  Adftests = rbind (adfUR1, adfUR2)
  

  kpssU1 = kpss.test(x, null = 'Trend')
  kpssU1$assess = ifelse(kpssU1$p.value <.05, 'trend non stationary', 'trend stationary')
    kpssU2 = kpss.test(x, null = 'Trend')
  kpssU2$assess = ifelse(kpssU2$p.value <.05, 'trend non stationary', 'trend stationary')
  KpssTests = rbind(kpssU1, kpssU2)
  
return(list(Adf = Adftests, Kpss = KpssTests))
}

disaggTest = function(dataset, cntry, var){
  adfUR1 = data.frame(stationarityTestsPanel(data = dataset, country = cntry, variable = var, test = 'adf'))
  adfUR2 = data.frame(stationarityTestsPanel(data = dataset, country = cntry, variable = var, test = 'adf', diffLogic = TRUE))
  
  kpssUR1 = data.frame(stationarityTestsPanel(data = dataset, country = cntry, variable = var, test = 'kpss'))
  kpssUR2 = data.frame(stationarityTestsPanel(data = dataset, country = cntry, variable = var, test = 'kpss', diffLogic = TRUE))
 
  return(list(adfUR1 = adfUR1, adfUR2 = adfUR2, kpssUR1 = kpssUR1, kpssUR2 = kpssUR2))

}


# ---------------------------------------

covData = read.csv(paste0(pathData, '/Codex/covData.csv'), stringsAsFactors = F)

# clean country names 
covData[covData$country == 'China' & covData$year <= 1983,'country']  = 'Taiwan'
covData[covData$country == 'Vietnam' & covData$year <= 1988,'country']  = 'South Vietnam'




# turn all NAs of participants into 0
covData$total_participants_gov[which(is.na(covData$total_participants_gov))] = 0
covData$total_participants_priv[which(is.na(covData$total_participants_priv))] = 0
covData$private_participants_gov[which(is.na(covData$private_participants_gov))] = 0


# create meeting abbreviation
covData$meetingAbbrev =  sub(" .*", "", covData$meetingGroupsStd)

#  create dummy for working groups that had meetings *both* before and after 2004
covData$dum2004 =  ifelse(covData$year>=2004, "After 2004", "Before 2004")
disqualifiedWGs2004 = names(unlist(lapply(split(covData$year, covData$meetingAbbrev), function(x){
      check = data.frame(allAfter = all(x>=2004), allBefore = all(x<=2004))
     which(!apply(check, 1, function(y){all(y ==FALSE)}))})))
covData$dum2004[which(covData$meetingAbbrev %in% disqualifiedWGs2004)]= NA
 
#  create dummy for working groups that had meetings *both* before and after 2001
covData$dum2001 =  ifelse(covData$year>=2001, "After 2001", "Before 2001")
disqualifiedWGs2001 = names(unlist(lapply(split(covData$year, covData$meetingAbbrev), function(x){
      check = data.frame(allAfter = all(x>=2001), allBefore = all(x<=2001))
     which(!apply(check, 1, function(y){all(y ==FALSE)}))})))
covData$dum2001[which(covData$meetingAbbrev %in% disqualifiedWGs2001)]= NA
 

#  create dummy for working groups that had meetings *both* before and after 1995
covData$dum1995 =  ifelse(covData$year>=1995, "After 1995", "Before 1995")
disqualifiedWGs1995 = names(unlist(lapply(split(covData$year, covData$meetingAbbrev), function(x){
      check = data.frame(allAfter = all(x>=1995), allBefore = all(x<=1995))
     which(!apply(check, 1, function(y){all(y ==FALSE)}))})))
covData$dum1995[which(covData$meetingAbbrev %in% disqualifiedWGs1995)]= NA


#  create dummy for working groups that had meetings *both* before and after 2007
covData$dum2007 =  ifelse(covData$year>=2007, "After 2007", "Before 2007")
disqualifiedWGs2007 = names(unlist(lapply(split(covData$year, covData$meetingAbbrev), function(x){
      check = data.frame(allAfter = all(x>=2007), allBefore = all(x<=2007))
     which(!apply(check, 1, function(y){all(y ==FALSE)}))})))
covData$dum2007[which(covData$meetingAbbrev %in% disqualifiedWGs2007)]= NA


# create variable of average comments
covData$avgCommentsPerParticipant = covData$comments/covData$total_participants_gov

# Note --- some documents have the delegation of China making a comment/proposal ..
# even though China is not included in the participant list (either as a country or observer)
# in such cases, I code the average number of comments as being NA
covData$avgCommentsPerParticipant[which(covData$country == 'China' & 
                                        covData$doc %in% c('al85_24Ae.txt',
                                                          'Fal87_13e.txt',
                                                          'Fal95_35e.txt',
                                                          '49%252Fal03_03e.txt') )] = NA
covData$avgCommentsPerParticipant[which(covData$country == 'Vietnam' & 
                                        covData$doc %in% c('16%252Fal29_31e.txt',
                                                            '01%252Fal30_41e.txt',
                                                            '6%252FREP12_CFe.txt') )] = NA

# fix algorithim mistake
covData[which(covData$country == 'China' & covData$doc == '16%252Fal29_31e.txt'), c('comments', 'total_participants_gov', 'attendance', 'avgCommentsPerParticipant')] = c(0, 3, 1, 0)
covData[which(covData$country == 'China' & covData$doc == 'Fal33_18e.txt'), c('comments', 'total_participants_gov', 'attendance', 'avgCommentsPerParticipant')] = c(0, 6, 1, 0)


# fix 0/0 error

covData[is.nan(covData$avgCommentsPerParticipant), c('avgCommentsPerParticipant')] = 0

#  check --- note when you have time, can fix this for Taiwan too
covData[is.infinite(covData$avgCommentsPerParticipant), c('doc', 'country', 'Folder','comments', 'total_participants_gov', 'attendance')]



covData$MeetingNum = as.numeric(gsub("[^0-9]", "", covData$Meeting))

# ---------------------------------------
vars = c('reportEN', 'reportFR', 'reportES', 'reportAR', 'reportRU',
    'comments',  'attendance' ,'attend_allCntries', 'noParticipantInfo',
    'total_participants_gov')

# aggregate data by country-year (instead of by country-meeting)
covDataAgg = data.frame(covData[which(covData$noParticipantInfo == 0 ), c('year', 'country', vars)] %>% 
        group_by(year, country) %>% summarise_each(funs(sum(., na.rm = T))))

covDataAgg$attendanceShare = covDataAgg$attendance/covDataAgg$reportEN
covDataAgg$commentsAvg = covDataAgg$comments/covDataAgg$attendance
covDataAgg$group = ifelse(covDataAgg$year<2004, 'Before 2004', 'After 2004')

covDataAgg = covDataAgg[order(covDataAgg$country),]
covDataAgg$total_participants_gov_fd = unlist(lapply(tapply(covDataAgg$total_participants_gov, covDataAgg$country, diff), function(x){
  x = c(x, NA)}))

covDataAgg$attendance_fd = unlist(lapply(tapply(covDataAgg$attendance, covDataAgg$country, diff), function(x){
  x = c(x, NA)}))
# It is possible for a time series to be non-stationary, have no unit-root yet be trend-stationary
# http://www.statosphere.com.au/check-time-series-stationary-r/



# Using aggregate data
chinaDataAgg = covDataAgg[which(covDataAgg$country == "China"),]
summary(lm(total_participants_gov~dplyr:::lag(total_participants_gov)+year, data =chinaDataAgg , na.action = na.omit))

vietDataAgg = covDataAgg[which(covDataAgg$country == "Vietnam"),]
summary(lm(total_participants_gov~dplyr:::lag(total_participants_gov)+year, data =vietDataAgg , na.action = na.omit))


# ---------------------------------------
# participation members

# fairly strong, consistent evidence for linear trend non-stationarity for both China and Vietnam
# little evidence for difference non-stationarity for Vietnam, some for China
library(tseries)
# aggregate data
aggTest(covDataAgg, "China", "total_participants_gov")

# tests  : no effect
threshold = 2004
t.test(covDataAgg[which(covDataAgg$country == 'China' & covDataAgg$year <threshold ), 'total_participants_gov_fd'], covDataAgg[which(covDataAgg$country == 'China' & covDataAgg$year >=threshold ), 'total_participants_gov_fd'])

t.test(covDataAgg[which(covDataAgg$country == 'Vietnam' & covDataAgg$year <threshold ), 'total_participants_gov_fd'], covDataAgg[which(covDataAgg$country == 'Vietnam' & covDataAgg$year >=threshold ), 'total_participants_gov_fd'])

# Using disaggregated data
summary(lm(total_participants_gov~dplyr:::lag(total_participants_gov)+year+meetingAbbrev, data =covData[which(covData$country == 'China'),] , na.action = na.omit))
summary(lm(total_participants_gov~dplyr:::lag(total_participants_gov)+year+meetingAbbrev, data =covData[which(covData$country == 'Vietnam'),] , na.action = na.omit))



# non stationary --- unit root 1  detected
disaggTest(covData, 'China', 'total_participants_gov')
disaggTest(covData, 'Vietnam', 'total_participants_gov')

participants_china_fd = firstDiffPanel(covData, "China", 'total_participants_gov')
participants_vietnam_fd = firstDiffPanel(covData, "Vietnam", 'total_participants_gov')

t.test(participants_china_fd$total_participants_gov[which(participants_china_fd$dum2004 == 'Before 2004')],
      participants_china_fd$total_participants_gov[which(participants_china_fd$dum2004 == 'After 2004')])
t.test(participants_vietnam_fd$total_participants_gov[which(participants_vietnam_fd$dum2004 == 'Before 2004')], participants_china_fd$total_participants_gov[which(participants_vietnam_fd$dum2004 == 'After 2004')])


summary(lm(total_participants_gov~dplyr:::lag(total_participants_gov)+year+meetingAbbrev+dum2004, data =covData[which(covData$country == 'China'),] , na.action = na.omit))
summary(lm(total_participants_gov~dplyr:::lag(total_participants_gov)+year+meetingAbbrev+dum2004, data =covData[which(covData$country == 'Vietnam'),] , na.action = na.omit))

summary(lm(total_participants_gov_fd~year+meetingAbbrev+dum2004, data =participants_china_fd , na.action = na.omit))
summary(lm(total_participants_gov_fd~year+dum2004+meetingAbbrev, data =participants_vietnam_fd , na.action = na.omit))



summary(lm(total_participants_gov_fd~year+group, data =chinaDataAgg , na.action = na.omit))
summary(lm(total_participants_gov_fd~year+group, data =vietDataAgg , na.action = na.omit))

# -----------------
# participation meetings

# aggregate data
# evidence of unit root (1)

aggTest(covDataAgg, "China", "attendance")
aggTest(covDataAgg, "Vietnam", "attendance")

# adf disagg china
# evidence of unit root (1)
fooDF = toWide(covData, 'China', 'attendance')
 adfList = list()
for ( i in c(3:4, 6:7, 9:12, 14:18, 20:23, 25:35, 37:42)){
  x = fooDF[,i]
  x = x[-which(is.na(x))]
  if (length(unique(x))>1){
  print(i)
  print(x)
  adfList [[i]] =  adf.test(x)
  }}

adf = data.frame(do.call(rbind, adfList))
adf$assess = ifelse(adf$p.value <.05, 'stationary', 'unit-root')

stationarityTestsPanel(covData, 'China', 'attendance', test = 'adf', diffLogic = TRUE)
stationarityTestsPanel(covData, 'China', 'attendance', test = 'kpss')
stationarityTestsPanel(covData, 'China', 'attendance', test = 'kpss', diffLogic = TRUE)
# tests
# agg
t.test(covDataAgg[which(covDataAgg$country == 'China' & covDataAgg$year <threshold ), 'attendance_fd'], covDataAgg[which(covDataAgg$country == 'China' & covDataAgg$year >=threshold ), 'attendance_fd'])

t.test(covDataAgg[which(covDataAgg$country == 'Vietnam' & covDataAgg$year <threshold ), 'attendance_fd'], covDataAgg[which(covDataAgg$country == 'Vietnam' & covDataAgg$year >=threshold ), 'attendance_fd'])

# disagg
attendance_china_fd = firstDiffPanel(covData, "China", 'attendance')
attendance_vietnam_fd = firstDiffPanel(covData, "Vietnam", 'attendance')
dim(attendance_vietnam_fd)

t.test(attendance_china_fd$attendance[which(attendance_china_fd$dum2004 == 'Before 2004')], attendance_china_fd$attendance[which(attendance_china_fd$dum2004 == 'After 2004')])

t.test(attendance_vietnam_fd$attendance[which(attendance_vietnam_fd$dum2004 == 'Before 2004')], attendance_vietnam_fd$attendance[which(attendance_vietnam_fd$dum2004 == 'After 2004')])

# -----------------------
# avg comments
aggTest(covDataAgg, "China", "commentsAvg")
aggTest(covDataAgg, "Vietnam", "commentsAvg")
 
disaggTest(covData, 'China', 'avgCommentsPerParticipant')
disaggTest(covData, 'Vietnam', 'avgCommentsPerParticipant')

# tests
commentsAvg_china_fd = firstDiffPanel(covData, "China", 'avgCommentsPerParticipant')
commentsAvg_vietnam_fd = firstDiffPanel(covData, "Vietnam",'avgCommentsPerParticipant')
 
t.test(commentsAvg_china_fd$avgCommentsPerParticipant_fd[which(commentsAvg_china_fd$dum2004 == 'Before 2004')], commentsAvg_china_fd$avgCommentsPerParticipant_fd[which(commentsAvg_china_fd$dum2004 == 'After 2004')])

t.test(commentsAvg_vietnam_fd$avgCommentsPerParticipant_fd[which(commentsAvg_vietnam_fd$dum2004 == 'Before 2004')], commentsAvg_vietnam_fd$avgCommentsPerParticipant_fd[which(commentsAvg_vietnam_fd$dum2004 == 'After 2004')])

# -----------------------
# raw comments
names(covDataAgg)
aggTest(covDataAgg, "China", "comments")
aggTest(covDataAgg, "Vietnam", "comments")
 
# unit root 1
disaggTest(covData, 'China', 'comments')
disaggTest(covData, 'Vietnam', 'comments')


comments_china_fd = firstDiffPanel(covData, "China", 'comments')
comments_vietnam_fd = firstDiffPanel(covData, "Vietnam",'comments')
 
t.test(comments_china_fd$comments_fd[which(comments_china_fd$dum2004 == 'Before 2004')], comments_china_fd$comments_fd[which(comments_china_fd$dum2004 == 'After 2004')])

t.test(comments_vietnam_fd$comments_fd[which(comments_vietnam_fd$dum2004 == 'Before 2004')], comments_vietnam_fd$comments_fd[which(comments_vietnam_fd$dum2004 == 'After 2004')])

########################################

########################################
# graphs of raw attendance and comments


## make bar graphs
# http://www.cookbook-r.com/Graphs/Bar_and_line_graphs_(ggplot2)/
#http://www.sthda.com/english/wiki/ggplot2-colors-how-to-change-colors-automatically-and-manually


## Paper graphs
setwd(pathGraphics)
pdf('attendanceShareBoxplot2004.pdf')
print(ggplot(covDataAgg[-which(covDataAgg$country  %in% c('Taiwan', 'South Vietnam')),], 
  aes(x = country, y =attendanceShare,  fill = dum2004)) +
  geom_boxplot()+
  ylab('Percent of meetings attended')  + 
  scale_fill_manual(values=c("#999999", "#E69F00")))
dev.off()


pdf('avgNumCommentsPerMeetingBoxPlot2004.pdf')
print(ggplot(covDataAgg[-which(covDataAgg$country  %in% c('Taiwan', 'South Vietnam')),], aes(x = country, y =commentsAvg,  fill = group)) +
  geom_boxplot()+
  ylab('Average number of comments per meeting')+ 
  scale_fill_manual(values=c("#999999", "#E69F00")))
dev.off()

pdf(paste0(pathGraphics,  '/totNumMeeting.pdf'))
covData$`Participant Info` = ifelse(covData$noParticipantInfo == 1, 'Not Available', "Available")
covData$`Participant Info` = factor(covData$`Participant Info`, levels = c('Not Available', "Available"))
print(ggplot(covData, aes(x = year, y =reportEN, fill = `Participant Info`)) +
    geom_bar(stat="identity") + ylab('Number of Codex Meetings'))
dev.off()
 
## Presentaiton graphs
setwd(pathGraphics)
pdf('attendanceShareBarPlot2004.pdf')
print(ggplot(covDataAgg2004 [-which(covDataAgg2004 $country  %in% c('Taiwan', 'South Vietnam')),], aes(x = country, y =attendanceShare,  fill = year)) +
    geom_bar(stat="identity", position=position_dodge())+
    ylab('Percent of meetings attended')  + 
    xlab ('')+
    scale_fill_manual(values=c("#999999", "#E69F00" ))+
     theme(legend.text=element_text(size=20),legend.key.size = unit(1.5, "cm"),  
     	axis.text.x = element_text(size = 20),
     	axis.title.y=element_text(size=14, vjust = 1.5)))
  
dev.off()


pdf('avgNumCommentsPerMeetingBarPlot2004.pdf')
ggplot(covDataAgg2004 [-which(covDataAgg2004 $country  %in% c('Taiwan', 'South Vietnam')),], aes(x = country, y =commentsAvg,  fill = year)) +
    geom_bar(stat="identity", position=position_dodge())+
    ylab('Average number of comments per meeting')+ 
    xlab('')+
    scale_fill_manual(values=c("#999999", "#E69F00"))+
     theme(legend.text=element_text(size=20),legend.key.size = unit(1.5, "cm"),  
     	axis.text.x = element_text(size = 20),
     	axis.title.y=element_text(size=14, vjust = 1.5))
dev.off()


# number of total participants per meeting
ggplot(covDataAgg[which(covDataAgg$country %in% c('China')),], 
        aes(x= meetingAbbrev, y = attend_allCntries, fill = dum2004))+
  geom_boxplot( )+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_x_continuous(breaks =seq(min(covDataAgg$year), max(covDataAgg$year)))



 # plot number of meetings attended by each country
pdf(paste0(pathGraphics, '/numberMeetingsByCountry.pdf'))
ggplot(covDataAgg[which(covDataAgg$country %in% c('China', 'Vietnam')),], 
        aes(x= year, y = attendance, color = country))+
  geom_line( )+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_x_continuous(breaks =seq(min(covDataAgg$year), max(covDataAgg$year)))+
  xlab("Year") + 
  ylab("Number of Meetings")
dev.off()

# plot average number of participants per meeting by year by country
covDataAgg$avgNumParticipantsPerMeeting = covDataAgg$total_participants_gov/covDataAgg$attendance
covDataAgg$avgNumParticipantsPerMeeting[which(is.nan(covDataAgg$avgNumParticipantsPerMeeting))] = 0

pdf(paste0(pathGraphics, '/numAvgParticipantsPerMeetingByCountry.pdf'))
ggplot(covDataAgg[which(covDataAgg$country %in% c('China', 'Vietnam')),], 
        aes(x= year, y = avgNumParticipantsPerMeeting, color = country))+
  geom_line( )+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_x_continuous(breaks =seq(min(covDataAgg$year), max(covDataAgg$year)))+
  xlab("Year") + 
  ylab("Average Number of Participants per Meeting")
dev.off()

 

covDataAgg$commentsAvg[which(is.na(covDataAgg$commentsAvg))] = 0
pdf(paste0(pathGraphics, '/numAvgCommentsPerMeetingByCountry.pdf'))
ggplot(covDataAgg[which(covDataAgg$country %in% c('China', 'Vietnam')),], 
        aes(x= year, y = commentsAvg, color = country))+
  geom_line( )+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_x_continuous(breaks =seq(min(covDataAgg$year), max(covDataAgg$year)))+
  xlab("Year") + 
  ylab("Average Number of Comments per Meeting")
dev.off()

pdf(paste0(pathGraphics, '/totalCommentsByCountry.pdf'))
ggplot(covDataAgg[which(covDataAgg$country %in% c('China', 'Vietnam')),], 
        aes(x= year, y = comments, color = country))+
  geom_line( )+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_x_continuous(breaks =seq(min(covDataAgg$year), max(covDataAgg$year)))+
  xlab("Year") + 
  ylab("Total Number of Comments per Year")
dev.off()





#  # plot number of participants by year by country
# ggplot(covDataAgg[which(covDataAgg$country %in% c('China', 'Vietnam')),], 
#         aes(x= year, y = total_participants_gov, color = country))+
#   geom_line( )+ 
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))+
#   scale_x_continuous(breaks =seq(min(covDataAgg$year), max(covDataAgg$year)))+
#   xlab("Year") + 
#   ylab("Total Number of Participants")

########################################

########################################
# graphs of participation by meeting group

# China vs Vietnam

# China over time
chinaData = covData[which(covData$country  %in% c('China')),]
setDT(chinaData)[,freq := .N, by = c("meetingAbbrev")]
chinaData = chinaData[order(freq, decreasing = T),]
chinaData$count = 1

chinaData$meetingAbbrev = factor(chinaData$meetingAbbrev, levels = unique(chinaData$meetingAbbrev))

# select only meetings where china has attended at least once
chinaData = chinaData[-which(chinaData$meetingAbbrev %in% names(which(tapply(chinaData$total_participants_gov, chinaData$meetingAbbrev, mean) == 0))),]


# remove meetings that did not take place both before and after 2004
chinaData2004 = chinaData[-which(is.na(chinaData$dum2004)),]
chinaData2004$dum2004 = factor(chinaData2004$dum2004, levels = c('Before 2004', 'After 2004'))

# remove meetings that did not take place both before and after 2004
chinaData2001 = chinaData[-which(is.na(chinaData$dum2001)),]
chinaData2001$dum2001 = factor(chinaData2001$dum2001, levels = c('Before 2001', 'After 2001'))

# remove meetings that did not take place both before and after 2004
chinaData1995 = chinaData[-which(is.na(chinaData$dum1995)),]
chinaData1995$dum1995 = factor(chinaData1995$dum1995, levels = c('Before 1995', 'After 1995'))

 ggplot(chinaData , 
    aes(x= meetingAbbrev, y = total_participants_gov))+
  geom_boxplot( )+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab('Number of government delegates')+
  xlab('Codex Committee Meeting')


 ggplot(chinaData , 
    aes(x = year, color= meetingAbbrev, y = total_participants_gov))+
  geom_line( )+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab('Number of government delegates')+
  xlab('Codex Committee Meeting')

pdf(paste0(pathGraphics, '/china2004participants.pdf')) 
ggplot(chinaData2004 , 
    aes(x= meetingAbbrev, y = total_participants_gov, fill= dum2004))+
  geom_boxplot( )+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab('Number of government delegates')+
  xlab('Codex Committee Meeting')
dev.off()

pdf(paste0(pathGraphics, '/china2001participants.pdf')) 
ggplot(chinaData2001 , 
    aes(x= meetingAbbrev, y = total_participants_gov, fill= dum2001))+
  geom_boxplot( )+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
dev.off()

pdf(paste0(pathGraphics, '/china1995participants.pdf')) 
ggplot(chinaData1995 , 
    aes(x= meetingAbbrev, y = total_participants_gov, fill= dum1995))+
  geom_boxplot( )+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
dev.off()

ggplot(chinaData , 
    aes(x= meetingAbbrev, y = count))+
  geom_bar(stat = 'identity' )+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  



#----------------
 
vietData = covData[which(covData$country  %in% c('Vietnam')),]
setDT(vietData)[,freq := .N, by = c("meetingAbbrev")]
vietData = vietData[order(freq, decreasing = T),]
vietData$count = 1

vietData$meetingAbbrev = factor(vietData$meetingAbbrev, levels = unique(vietData$meetingAbbrev))


# select only meetings where viet has attended at least once
vietData = vietData[-which(vietData$meetingAbbrev %in% names(which(tapply(vietData$total_participants_gov, vietData$meetingAbbrev, mean) == 0))),]

 
# remove meetings that did not take place both before and after 2004
vietData2004 = vietData[-which(is.na(vietData$dum2004)),]

# remove meetings that did not take place both before and after 2007
vietData2007 = vietData[-which(is.na(vietData$dum2007)),]

# remove meetings that did not take place both before and after 1995
vietData1995 = vietData[-which(is.na(vietData$dum1995)),]

 
ggplot(vietData2004 , 
    aes(x= meetingAbbrev, y = total_participants_gov, fill= dum2004))+
  geom_boxplot( )+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(vietData2007 , 
    aes(x= meetingAbbrev, y = total_participants_gov, fill= dum2007))+
  geom_boxplot( )+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(vietData1995 , 
    aes(x= meetingAbbrev, y = total_participants_gov, fill= dum1995))+
  geom_boxplot( )+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


ggplot(vietData , 
    aes(x= meetingAbbrev, y = count))+
  geom_bar(stat = 'identity' )+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  



#------------
# subset to working groups that both countries have participated in at least once

threshold= table(chinaData$meetingGroupsStd)[table(chinaData$meetingGroupsStd) > 15 ]


ggplot(chinaData[chinaData$meetingGroupsStd %in% c(names(threshold)),], 
    aes(x= meetingAbbrev, y = total_participants_gov, fill= dum2004))+
  geom_boxplot( )+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(chinaData[chinaData$meetingGroupsStd %in% c(names(threshold)),], 
    aes(x= meetingAbbrev, y = total_participants_gov, fill= dum2001))+
  geom_boxplot( )+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


ggplot(chinaData[chinaData$meetingGroupsStd %in% c(names(threshold)),], 
    aes(x= meetingAbbrev, y = total_participants_gov, fill= dum1995))+
  geom_boxplot( )+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


ggplot(chinaData[chinaData$meetingGroupsStd %in% c(names(threshold)),], 
    aes(x= meetingAbbrev, y = count))+
  geom_bar(stat = 'identity' )+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  


dim(chinaData[which(chinaData$meetingAbbrev == "CCNFSDU"),])

dim(chinaData[which(chinaData$meetingAbbrev == "CCEXEC"),])





ggplot(covData[which(covData$country  %in% c('China') &
        covData$meetingGroupsStd %in% c(names(threshold))),], 
    aes(x= meetingGroupsStd, y = total_participants_gov, fill= dum2001))+
  geom_boxplot( )+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


ggplot(covData[which(covData$country  %in% c('China') &
        covData$meetingGroupsStd %in% c(names(threshold))),], 
    aes(x= meetingGroupsStd, y = total_participants_gov, fill= dum1995))+
  geom_boxplot( )

ggplot(covData[which(covData$country  %in% c('China') &
        covData$meetingGroupsStd %in% c(names(threshold))),], 
    aes(x= meetingAbbrev, y = total_participants_priv, fill= dum2004))+
  geom_boxplot( )

  + 
  theme(legend.position = "none")


ggplot(covData[which(covData$country  %in% c('China') & covData$meetingGroupsStd == "CCEXEC Executive Committee of the Codex Alimentarius Commission"),], 
    aes(x= year, y = total_participants_gov))+
  geom_line( )+ 
  theme(legend.position = "none")
 



test = covData[which(covData$country  %in% c('China') & covData$meetingGroupsStd == "CCEXEC Executive Committee of the Codex Alimentarius Commission"),]


head(test[, c('total_participants_gov')])
dim(test)






### Other graphs 
setwd(pathGraphics)
pdf('attendanceShare.pdf')
print(ggplot(covDataAgg[-which(covDataAgg$country  %in% c('Taiwan', 'South Vietnam')),], aes(x = year, y =attendanceShare,  fill = country)) +
    geom_bar(stat="identity", position=position_dodge())+
    ylab('Percent of meetings attended'))
dev.off()

pdf('avgNumCommentsPerMeeting.pdf')
print(ggplot(covDataAgg[-which(covDataAgg$country  %in% c('Taiwan', 'South Vietnam')),], aes(x = year, y =commentsAvg,  fill = country)) +
    geom_bar(stat="identity", position=position_dodge())+
    ylab('Average number of comments per meeting'))
dev.off()



install.packages('schoRsch')
install.packages('xtable')
library(schoRsch)
library(xtable)

test.C <- t.test(covData[which(covData$country == 'China' & covData$year < 2004), 'attendance'], covData[which(covData$country == 'China' & covData$year >= 2004), 'attendance'])
test.VN <- t.test(covData[which(covData$country == 'Vietnam' & covData$year < 2004), 'attendance'], covData[which(covData$country == 'Vietnam' & covData$year >= 2004), 'attendance'])

test.C.comment <- t.test(covData[which(covData$country == 'China' & covData$year < 2004& covData$attendance >0), 'comments'], covData[which(covData$country == 'China' & covData$year >= 2004& covData$attendance >0), 'comments'])

test.VN.comment <- t.test(covData[which(covData$country == 'Vietnam' & covData$year < 2004& covData$attendance >0), 'comments'], covData[which(covData$country == 'Vietnam' & covData$year >= 2004& covData$attendance >0), 'comments'])
xtable(
  t_out(toutput=test.C, n.equal = TRUE, welch.df.exact = TRUE, welch.n = NA,
        d.corr = TRUE, print = TRUE)
)

##################Teressract PDFs#########################
Tes.2003<- c("02%252FAL03_40e.pdf","03%252FAL03_38e","04%252FAF03_01e","05%252FAL03_11e","08%252FAL03_16e","09%252FMH03_01e","13%252Fal03_31e") #Folder 2003




########################################

########################################
# regressions



covData$thresh95 = 0
covData$thresh95[covData$year > 1995] = 1

dv = c('attendance', 'comments')
iv = list(c('threshold95'), c('threshold95'))

DATASETS = list(covData, covData[which(covData$attendance == 1),])
names_d = c('full', 'attended')

pooled = list()
fixed_dum= list()
random = list()


for (i in 1:length(dv)){ 
  for(dataset in 1:length(DATASETS)){
    pooled[[paste(dv[i], names_d[dataset], sep = '_')]] = lm(as.formula(paste(dv[i], 'thresh95', sep = '~')), data = DATASETS[[dataset]])
    fixed_dum[[paste(dv[i], names_d[dataset], sep = '_')]] = lm(as.formula(paste(dv[i], paste(c('thresh95', 'as.factor(country)' ), collapse = '+' ), sep = '~')), data = DATASETS[[dataset]])
    random[[paste(dv[i], names_d[dataset], sep = '_')]] = lmer(as.formula(paste(dv[i], paste(c('thresh95', '(1|country)' ), collapse = '+' ), sep = '~')), data = DATASETS[[dataset]])
    
  }
  
}


lapply(pooled, summary)
lapply(fixed_dum, summary)
lapply(random, summary)



########################################

########################################
# t tests for raw participation

###### China ########
# note china joined wto in 2001


# attendance meetings raw

# creation of wto matters raw
t.test(covData[which(covData$country == 'China' & covData$dum1995 =="Before 1995"  ), 'attendance'], covData[which(covData$country == 'China' & covData$dum1995 =="After 1995" ), 'attendance'])

# china joining wto matters raw
t.test(covData[which(covData$country == 'China' & covData$dum2001 =="Before 2001" ), 'attendance'], covData[which(covData$country == 'China' & covData$dum2001 =="After 2001" ), 'attendance'])

# codex fund matters raw
t.test(covData[which(covData$country == 'China' & covData$dum2004 =="Before 2004"), 'attendance'], covData[which(covData$country == 'China' & covData$dum2004 =="After 2004"), 'attendance'])


# creation of wto doesn't matter, conditional on before 2001
t.test(covData[which(covData$country == 'China' & covData$dum1995 =="Before 1995" & covData$year<2001), 'attendance'], covData[which(covData$country == 'China' & covData$dum1995 =="After 1995"& covData$year<2001), 'attendance'])

# creation of wto matters, conditional on before 2004
t.test(covData[which(covData$country == 'China' & covData$dum1995 =="Before 1995" & covData$year<2004), 'attendance'], covData[which(covData$country == 'China' & covData$dum1995 =="After 1995"& covData$year<2004), 'attendance'])

# china joining wto matters, conditional on before codex
t.test(covData[which(covData$country == 'China' & covData$dum2001 =="Before 2001"& covData$year<2004), 'attendance'], covData[which(covData$country == 'China' & covData$dum2001 =="After 2001"& covData$year<2004), 'attendance'])

# china joining wto matters, conditional on before codex and after wto creation
t.test(covData[which(covData$country == 'China' & covData$dum2001 =="Before 2001"& covData$year<2004& covData$year>1995), 'attendance'], covData[which(covData$country == 'China' & covData$dum2001 =="After 2001"& covData$year<2004& covData$year>1995), 'attendance'])


# codex fund doesn't matter, conditoinal on china joining
t.test(covData[which(covData$country == 'China' & covData$dum2004 =="Before 2004"& covData$year>2001), 'attendance'], covData[which(covData$country == 'China' & covData$dum2004 =="After 2004" & covData$year>2001), 'attendance'])

 

#-------
# attendance detailed raw

# creation of wto matters raw 
t.test(covData[which(covData$country == 'China' & covData$dum1995 =="Before 1995"  ), 'total_participants_gov'], covData[which(covData$country == 'China' & covData$dum1995 =="After 1995" ), 'total_participants_gov'])

# joining wto matters raw
t.test(covData[which(covData$country == 'China' & covData$dum2001 =="Before 2001"  ), 'total_participants_gov'], covData[which(covData$country == 'China' & covData$dum2001 =="After 2001"  ), 'total_participants_gov'])


# codex fund matters raw
t.test(covData[which(covData$country == 'China' & covData$dum2004 =="Before 2004" ), 'total_participants_gov'], covData[which(covData$country == 'China' & covData$dum2004 =="After 2004" ), 'total_participants_gov'])


# creation of wto doesn't matter, conditional on before 2001
t.test(covData[which(covData$country == 'China' & covData$dum1995 =="Before 1995" & covData$year<2001), 'total_participants_gov'], covData[which(covData$country == 'China' & covData$dum1995 =="After 1995" & covData$year<2001), 'total_participants_gov'])

# creation of wto matters, conditional on before 2004
t.test(covData[which(covData$country == 'China' & covData$dum1995 =="Before 1995" & covData$year<2004), 'total_participants_gov'], covData[which(covData$country == 'China' & covData$dum1995 =="After 1995" & covData$year<2004), 'total_participants_gov'])

# *** joining wto matters, conditional on before codex
t.test(covData[which(covData$country == 'China' & covData$dum2001 =="Before 2001" & covData$year<2004), 'total_participants_gov'], covData[which(covData$country == 'China' & covData$dum2001 =="After 2001" & covData$year<2004), 'total_participants_gov'])

# ***  joining wto matters, conditional on after creation of wto and before codex
t.test(covData[which(covData$country == 'China' & covData$dum2001 =="Before 2001" & covData$year<2004 & covData$year>1995), 'total_participants_gov'], covData[which(covData$country == 'China' & covData$dum2001 =="After 2001" & covData$year<2004& covData$year>1995), 'total_participants_gov'])

# *** creation of codex fund matters, conditional on after creation of wto
t.test(covData[which(covData$country == 'China' & covData$dum2004 =="Before 2004"& covData$year>2001), 'total_participants_gov'], covData[which(covData$country == 'China' & covData$dum2004 =="After 2004"& covData$year>2001), 'total_participants_gov'])




# comments raw
t.test(covData[which(covData$country == 'China' & covData$dum1995 =="Before 1995" & covData$year<2001), 'comments'], covData[which(covData$country == 'China' & covData$dum1995 =="After 1995"& covData$year<2001),  'comments'])
t.test(covData[which(covData$country == 'China' & covData$dum2001 =="Before 2001" &covData$year<2004),  'comments'], covData[which(covData$country == 'China' & covData$dum2001 =="After 2001"& covData$year<2004),  'comments'])
t.test(covData[which(covData$country == 'China' & covData$dum2004 =="Before 2004"& covData$year>2001),  'comments'], covData[which(covData$country == 'China' & covData$dum2004 =="After 2004"& covData$year>2001),  'comments'])

# comments given attendance


t.test(covData[which(covData$country == 'China' & covData$dum1995 =="Before 1995"& covData$attendance >0 ), 'comments'], covData[which(covData$country == 'China' & covData$dum1995 =="After 1995"& covData$attendance >0 ), 'comments'])


t.test(covData[which(covData$country == 'China' & covData$dum2001 =="Before 2001"& covData$attendance >0 ), 'comments'], covData[which(covData$country == 'China' & covData$dum2001 =="After 2001"& covData$attendance >0 ), 'comments'])
t.test(covData[which(covData$country == 'China' & covData$dum2004 =="Before 2004"& covData$attendance >0), 'comments'], covData[which(covData$country == 'China' & covData$dum2004 =="After 2004"& covData$attendance >0), 'comments'])

# creation of wto doesn't matter
t.test(covData[which(covData$country == 'China' & covData$dum1995 =="Before 1995"& covData$attendance >0& covData$year<2001), 'comments'], covData[which(covData$country == 'China' & covData$dum1995 =="After 1995"& covData$attendance >0& covData$year<2001), 'comments'])

# joining wto doesn't matter
t.test(covData[which(covData$country == 'China' & covData$dum2001 =="Before 2001"& covData$attendance >0& covData$year<2004& covData$year>1995), 'comments'], covData[which(covData$country == 'China' & covData$dum2001 =="After 2001"& covData$attendance >0& covData$year<2004 & covData$year>1995), 'comments'])

# codex fund doesn't matter
t.test(covData[which(covData$country == 'China' & covData$dum2004 =="Before 2004"& covData$attendance >0& covData$year>2001), 'comments'], covData[which(covData$country == 'China' & covData$dum2004 =="After 2004"& covData$attendance >0& covData$year>2001), 'comments'])


# average number of comments given attendance
t.test(covData[which(covData$country == 'China' & covData$dum1995 =="Before 1995"& covData$attendance >0), 'avgCommentsPerParticipant'], covData[which(covData$country == 'China' & covData$dum1995 =="After 1995"& covData$attendance >0), 'avgCommentsPerParticipant'])
t.test(covData[which(covData$country == 'China' & covData$dum2001 =="Before 2001"& covData$attendance >0), 'avgCommentsPerParticipant'], covData[which(covData$country == 'China' & covData$dum2001 =="After 2001"& covData$attendance >0), 'avgCommentsPerParticipant'])
t.test(covData[which(covData$country == 'China' & covData$dum2004 =="Before 2004"& covData$attendance >0), 'avgCommentsPerParticipant'], covData[which(covData$country == 'China' & covData$dum2004 =="After 2004"& covData$attendance >0), 'avgCommentsPerParticipant'])


# creation of wto matters, fewer
t.test(covData[which(covData$country == 'China' & covData$dum1995 =="Before 1995"& covData$attendance >0& covData$year<2001), 'avgCommentsPerParticipant'], covData[which(covData$country == 'China' & covData$dum1995 =="After 1995"& covData$attendance >0& covData$year<2001), 'avgCommentsPerParticipant'])

# joining wto doesn't matter
t.test(covData[which(covData$country == 'China' & covData$dum2001 =="Before 2001"& covData$attendance >0 &  covData$year>1995 & covData$year<2004), 'avgCommentsPerParticipant'], covData[which(covData$country == 'China' & covData$dum2001 =="After 2001"& covData$attendance >0& covData$year>1995 & covData$year<2004), 'avgCommentsPerParticipant'])

# codex fund doesn't matter
t.test(covData[which(covData$country == 'China' & covData$dum2004 =="Before 2004"& covData$attendance >0& covData$year>2001), 'avgCommentsPerParticipant'], covData[which(covData$country == 'China' & covData$dum2004 =="After 2004"& covData$attendance >0& covData$year>2001), 'avgCommentsPerParticipant'])


###### Vietnam ########
# note vietnam joined wto in 2007

# attendance, number of meetings

# # creation of wto matters
# t.test(covData[which(covData$country == 'Vietnam' & covData$dum1995 =="Before 1995" ), 'attendance'], covData[which(covData$country == 'Vietnam' & covData$dum1995 =="After 1995" ), 'attendance'])

# # joining wto matters
# t.test(covData[which(covData$country == 'Vietnam' & covData$dum2007 =="Before 2007" ), 'attendance'], covData[which(covData$country == 'Vietnam' & covData$dum2007 =="After 2007" ), 'attendance'])

# # codex fund matters
# t.test(covData[which(covData$country == 'Vietnam' & covData$dum2004 =="Before 2004" ), 'attendance'], covData[which(covData$country == 'Vietnam' & covData$dum2004 =="After 2004" ), 'attendance'])





# ** creation of wto matters, conditional
t.test(covData[which(covData$country == 'Vietnam' & covData$dum1995 =="Before 1995"& covData$year<2004), 'attendance'], covData[which(covData$country == 'Vietnam' & covData$dum1995 =="After 1995"& covData$year<2004), 'attendance'])

# ** joining wto doesn't matter, conditional
t.test(covData[which(covData$country == 'Vietnam' & covData$dum2007 =="Before 2007" & covData$year>2004), 'attendance'], covData[which(covData$country == 'Vietnam' & covData$dum2007 =="After 2007"& covData$year>2004), 'attendance'])


# ** codex doesnt' matter
t.test(covData[which(covData$country == 'Vietnam' & covData$dum2004 =="Before 2004"& covData$year<2007 & covData$year>1995), 'attendance'], covData[which(covData$country == 'Vietnam' & covData$dum2004 =="After 2004"& covData$year<2007& covData$year>1995), 'attendance'])
