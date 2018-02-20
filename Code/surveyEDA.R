rm(list = ls())
source('/Users/cindycheng/Dropbox/Documents/Papers/foodSafetySurvey/RCode/setup.R')

# load unimputed telephone data
load(file = paste0(pathData, '/telephoneSurveyRecoded.rda')) 
data$lincome = log(data$newinc + 1)

# load unimputed internet data
load(file = paste0(pathData, '/onlineSurvey/qualtricsFinal.rda'))
 
 


# ------------
qualtricsFinal$International = 6 - qualtricsFinal$International
 
internetResponseInt = table(qualtricsFinal$International)
names(internetResponseInt) = c('Large negative impact',
                                'Small negative impact', 
                                'Same as before',
                                'Small positive impact', 
                                'Large positive impact')
teleResponseInt = table(data$q20_international)
names(teleResponseInt) = c('Large negative impact',
                             'Small negative impact', 
                             'Same as before',
                             'Small positive impact', 
                             'Large positive impact')


table(data$q20_international)
table(data$city, data$q20_international)
table(data$femaleDum, data$q20_international)
table(data$q01_interest, data$q20_international)
table(data$q02_worry, data$q20_international)
table(data$q04_trust_cen, data$q20_international)
table(data$q05_trust_prov, data$q20_international)
table(data$q06_trust_indust, data$q20_international)
table(data$q07_trust_ngo, data$q20_international)


table(data$q08_ability_cen, data$q20_international)
table(data$q09_ability_prov, data$q20_international)
table(data$q10_ability_indust, data$q20_international)
table(data$q11_ability_ngo, data$q20_international)
table(data$q23_certifam, data$q20_international)
table(data$q22_porkunsafe, data$q20_international)

table(data$q20_international)
model1 = lm(q20_international~ q01_interest+q02_worry+q04_trust_cen+q05_trust_prov, data = data)
summary(model1)
