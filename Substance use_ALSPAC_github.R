############################################################################################################################
############################################### ALSPAC ~ SUBSTANCE USE #####################################################
############################################################################################################################

############ DATA CLEANING AND PREPARATION ###############################
rm(list = ls())

install.packages("knitr")
install.packages("imputeTS")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("xlsx")
install.packages("lavaan")
install.packages("semTools")

#load packages 
library("foreign")
library("dplyr")
library("knitr")
library("ggplot2")
library("xlsx")
library("lavaan")
library("corrplot")

#set working directory
setwd("N:/Data/Alspac")

#wd MC 
setwd("~/Desktop/Data/Alspac")


#load data
datALSP<- read.spss("Pingault_29Jun17.sav", use.value.labels=FALSE, to.data.frame=TRUE) ## read in dataframe 
save(datALSP, file = "datALSP")

### SCALES: 
### Fagestrom: Assessed at 17.5(clinic), 20+, 21+, 22+ yrs
### Audit: 16, 17.5 (clinic), 18, 20+, 22+
### Cast: 16, 17.5 (clinic), 18, 20+, 22+
### Other drugs: 16, 17.5(clinic), 18, 20+, 22+

#################################################### Age 20+ ##########################################################

################## Fagestrom ################
###Frequency of use: ordinal variable with 4 levels 
###(1=never smoked; 2= less than once a month; 3=less than once per week; 4=frequent ~ once a week or more)

##CCU3000 = Has smoked a whole cigarette (includes roll-ups)
# 1= yes; 2=no
table(datALSP$CCU3000, exclude = FALSE)
#1     2  <NA> 
#2521  1724 11200

##CCU3010 = Smoked cigarettes in the past 30 days
# 1= yes; 2= no; NA= missing or never smoked
table(datALSP$CCU3010, exclude = FALSE)
#  1     2  <NA> 
#1289  1229 12927 

##CCU3014 = YP smokes every week
# 1= yes; 2= no
table(datALSP$CCU3014, exclude = FALSE)
#1     2  <NA> 
#814  1685 12946 

#CCU3012: Smokes every day
table(datALSP$CCU3012, exclude = FALSE)


## function to create variable of cig smoking frequency 

cig_fre<- function(cig_ever, cig_month, cig_week, cig_day) {
  cig_var <- cig_ever
  cig_var[cig_ever == "2"] = 0 #"0:Never"
  cig_var[cig_ever == "1" & cig_month == "2"] = 1 # less than once a month
  cig_var[cig_month == "1" & cig_week == "2"] = 2 #"less than once a week"
  cig_var[cig_week == "1"] = 3 #"once a week or more"
  cig_var[cig_day == "1"] = 4 # daily 
  return(cig_var)
}


datALSP$cig20 <-cig_fre(datALSP$CCU3000, datALSP$CCU3010,datALSP$CCU3014, datALSP$CCU3012)
table(datALSP$cig20, exclude = FALSE)
#    0     1     2     3     4  <NA> 
#  1724  1230   468   225   598 11200 

##Fagestrom items ~ dichotomise all for factor analysis?

#CCU3013: Number of cigarettes smoke on average per day
#original values range between 0 and 30; NA = 14842
table(datALSP$CCU3013, exclude = FALSE) 
#new variable with 4 levels 
datALSP$FT1_20<-datALSP$CCU3013
datALSP$FT1_20<- as.numeric(as.character(datALSP$FT1_20))
datALSP$FT1_20[datALSP$FT1_20 <= 10] <- 0
datALSP$FT1_20[datALSP$FT1_20 > 10 & datALSP$FT1_20 <= 20] <- 1
datALSP$FT1_20[datALSP$FT1_20 > 20 & datALSP$FT1_20 <= 30] <- 2
datALSP$FT1_20[datALSP$FT1_20 > 30]<- 3
table(datALSP$FT1_20)
mean(datALSP$FT1_20, na.rm = TRUE)#0.2454


#CCU3020: Time from waking up to smoking first cigarette
#original coding: 1=within 5 mins; 2=6-30 mins; 3=31-60 mins; 4=more than 1 h; NA = 14644 
table(datALSP$CCU3020, exclude = FALSE)
#new variable with 4 levels
datALSP$FT2_20<- datALSP$CCU3020
datALSP$FT2_20[datALSP$CCU3020 == "1"]<- 3
datALSP$FT2_20[datALSP$CCU3020 == "2"]<- 2
datALSP$FT2_20[datALSP$CCU3020 == "3"]<- 1
datALSP$FT2_20[datALSP$CCU3020 == "4"]<- 0
table(datALSP$FT2_20)
mean(datALSP$FT2_20, na.rm = TRUE) # 0.882


#CCU3025: Finds it difficult to refrain from smoking in places where it is forbidden (e.g. in church, buses, trains, the library, cinemas)
#original coding: 1= yes; 2 = no; NA = 14638 
table(datALSP$CCU3025, exclude = FALSE)
#new variable with 2 levels
datALSP$FT3_20<- datALSP$CCU3025
datALSP$FT3_20[datALSP$CCU3025 == "2"]<- 0
datALSP$FT3_20[datALSP$CCU3025 == "1"]<- 1
table(datALSP$FT3_20)
mean(datALSP$FT3_20, na.rm = TRUE)#0.1053

#CCU3030: Cigarette would hate most to give up
#original coding: 1= first one/morning; 2 = all others; NA = 14664 
table(datALSP$CCU3030, exclude = FALSE)
#new variable with 2 levels
datALSP$FT4_20<- datALSP$CCU3030
datALSP$FT4_20[datALSP$CCU3030 == "1"]<- 1
datALSP$FT4_20[datALSP$CCU3030 == "2"]<- 0
table(datALSP$FT4_20)
mean(datALSP$FT4_20, na.rm = TRUE)#0.2906

#CCU3040: Smokes more frequently during the first hours after waking than during the rest of the day
#original coding: 1=yes; 2=no; NA = 14640 
table(datALSP$CCU3040, exclude = FALSE)
#new variable with 2 levels
datALSP$FT5_20<- datALSP$CCU3040
datALSP$FT5_20 [datALSP$CCU3040 == "1"]<- 1
datALSP$FT5_20 [datALSP$CCU3040 == "2"]<- 0
table(datALSP$FT5_20)
mean(datALSP$FT5_20, na.rm = TRUE)#0.127

#CCU3041: Smokes if they are so ill that they are in bed most of the day
#original coding: 1=yes; 2=no; NA = 14640
table(datALSP$CCU3041, exclude = FALSE)
#new variable with 2 levels
datALSP$FT6_20<- datALSP$CCU3041
datALSP$FT6_20[datALSP$CCU3041 == "1"]<- 1
datALSP$FT6_20[datALSP$CCU3041 == "2"]<- 0
table(datALSP$FT6_20)
mean(datALSP$FT6_20, na.rm = TRUE) # 0.2509

#compute my overall score 
datALSP$FT_mytotal20<- datALSP$FT1_20 + datALSP$FT2_20 + datALSP$FT3_20 + datALSP$FT4_20 + datALSP$FT5_20 + datALSP$FT6_20
table(datALSP$FT_mytotal20) # more NA than derived variable in tjhe dataset

#CCU3045: total score 
#ranges from 0 to 9; NA = 14670 
table(datALSP$CCU3045,datALSP$cig20, exclude = FALSE ) # people who smoke less than once per week were are coded as NA
table(datALSP$CCU3045)
# assign value of 0 to those who smoke less than one a week
datALSP$FT_total20<- datALSP$CCU3045 + 1# total score includes 0, thus add +1 to all scores so that scores for people who completed the questionnaire range bet 1 and 9, and then assign 0 to those who have never smoked or rarely smoked 
table(datALSP$FT_total20)# ok, now scores range bet 1-10

#assign 0 score to those who smoke less than weekly and did therefore not complete FT questionnaire
datALSP$FT_total20[datALSP$cig20 %in% c(0,1,2)]<- 0
table(datALSP$FT_total20, datALSP$cig20, exclude = FALSE) # looks good:)
hist(datALSP$FT_total20) # extremely skewed, a lot of 0s - zero-inflated poisson distribution would fit well


######################### AUDIT ############################

#CCU3100: Over the past year, frequency had a drink containing alcohol					
#original coding: 1=Never; 2=monthly or less; 3=2-4 times a month; 4=2-3 times a week; 5=4 or more times a week, NA = 11352 

table(datALSP$CCU3100, exclude = TRUE)

#function to recode audit items
audit<- function (audit) {
  alc_var <- audit
  alc_var[audit == "1"] = 0 
  alc_var[audit == "2"] = 1
  alc_var[audit == "3"] = 2
  alc_var[audit == "4"] = 3
  alc_var[audit == "5"] = 4
  return(alc_var)
}

datALSP$audit1_20<-audit(datALSP$CCU3100)
table(datALSP$audit1_20)
hist(datALSP$audit1_20) # OK
mean(datALSP$audit1_20, na.rm = TRUE) # 2.176643

#CCU3101: Over the past year, number of units had on a typical day when drinking					
#original coding: 1= 1 or 2; 2= 3 or 4; 3= 5 or 5; 4= 7 to 9; 5= 10 or more; NA = 11510 
table(datALSP$CCU3101, exclude = TRUE)
datALSP$alc_var<-0
datALSP$audit2_20<-audit(datALSP$alc_var, datALSP$CCU3101)
table(datALSP$audit2_20)
hist(datALSP$audit2_20) # not normal
mean(datALSP$audit2_20, na.rm = TRUE) #  1.923761

#CCU3102: Over the past year, frequency had six or more units on one occasion					
#original coding: 1=never; 2=less than monthly; 3=monthly; 4=weekly; 5=daily or almost daily; NA = 11477 
table(datALSP$CCU3102, exclude = TRUE)
datALSP$alc_var<-0
datALSP$audit3_20<-audit(datALSP$alc_var, datALSP$CCU3102)
table(datALSP$audit3_20)
hist(datALSP$audit3_20) # not normal
mean(datALSP$audit3_20, na.rm = TRUE)# 1.864

#CCU3103: During the past year, frequency found that they were not able to stop drinking once they had started					
#original coding: 1=never; 2=less than monthly; 3=monthly;4=at least weekly; NA = 11500 
table(datALSP$CCU3103, exclude = TRUE)
datALSP$alc_var<-0
datALSP$audit4_20<-audit(datALSP$alc_var, datALSP$CCU3103)
table(datALSP$audit4_20)
hist(datALSP$audit4_20) # not normal
mean(datALSP$audit4_20, na.rm = TRUE) # 0.312

#CCU3104: During the past year, frequency failed to do what was normally expected of them because of drinking					
#original coding: 1=never; 2=less than monthly; 3=monthly; 4=weekly; 5=daily or almost daily; NA = 11481
table(datALSP$CCU3104, exclude = TRUE)
datALSP$alc_var<-0
datALSP$audit5_20<-audit(datALSP$alc_var, datALSP$CCU3104)
table(datALSP$audit5_20)
hist(datALSP$audit5_20) # not normal
mean(datALSP$audit5_20, na.rm = TRUE) #0.4606

#CCU3105: During the past year, frequency needed a first drink in the morning to get themselves going after a heavy drinking session					
#original coding: 1=never; 2=less than monthly; 3=monthly;4=at least weekly; NA =  11481 
table(datALSP$CCU3105, exclude = TRUE)
datALSP$alc_var<-0
datALSP$audit6_20<-audit(datALSP$alc_var, datALSP$CCU3105)
table(datALSP$audit6_20)
hist(datALSP$audit6_20) # not normal 
mean(datALSP$audit6_20, na.rm = TRUE) # 0.0469

#CCU3106: During past year, frequency had a feeling of guilt or remorse after drinking					
#original coding: 1=never; 2=less than monthly; 3=monthly; 4=weekly; 5=daily or almost daily
table(datALSP$CCU3106, exclude = TRUE)
datALSP$alc_var<-0
datALSP$audit7_20<-audit(datALSP$alc_var, datALSP$CCU3106)
table(datALSP$audit7_20)
hist(datALSP$audit7_20) # not normal
mean(datALSP$audit7_20, na.rm = TRUE) # 0.4504

#CCU3107: During past year, frequency been unable to remember what happened the night before because they had been drinking					
#original coding: 1=never; 2=less than monthly; 3=monthly; 4=weekly; 5=daily or almost daily
table(datALSP$CCU3107, exclude = TRUE)
datALSP$alc_var<-0
datALSP$audit8_20<-audit(datALSP$alc_var, datALSP$CCU3107)
table(datALSP$audit8_20)
hist(datALSP$audit8_20) # not normal
mean(datALSP$audit8_20, na.rm = TRUE) # mean:0.771

#CCU3108: Respondent or someone else injured as a result of respondent's drinking					
#original coding: 1=no; 2=yes, but not in the past year; 3=yes, during the past year; NA = 11479 
table(datALSP$CCU3108, exclude = TRUE)

#function to recode audit items 2 
audit2<- function (audit2) {
  alc_var2 <- audit2
  alc_var2[audit2 == "1"] = 0 
  alc_var2[audit2 == "2"] = 2
  alc_var2[audit2 == "3"] = 4
  return(alc_var2)
}

datALSP$audit9_20<-audit2(datALSP$alc_var, datALSP$CCU3108)
table(datALSP$audit9_20)
hist(datALSP$audit9_20) # not normal 
mean(datALSP$audit9_20, na.rm = TRUE) # 0.785

#CCU3109: Relative, friend, doctor or other health worker been concerned about respondent's drinking or suggested respondent cut down					
#original coding: 1=no; 2=yes, but not in the past year; 3=yes, during the past year; NA = 11482
table(datALSP$CCU3109, exclude = TRUE)

datALSP$audit10_20<-audit2(datALSP$alc_var, datALSP$CCU3109)
table(datALSP$audit10_20)
hist(datALSP$audit10_20) # not normal
mean(datALSP$audit10_20, na.rm = TRUE) # 0.2316

### Note: all items should be scored on a 0-4 scale, but items CCU3103 and CCU3105 were scored 0-3 in Alspac. 
### dichotomise all items (0 vs. all other levels) for CFA? 

#my tot score 
datALSP$audit_mytot20<- datALSP$audit1_20 +datALSP$audit2_20 + datALSP$audit3_20 + datALSP$audit4_20 + datALSP$audit5_20 + datALSP$audit6_20 + datALSP$audit7_20 + datALSP$audit8_20 + datALSP$audit9_20 + datALSP$audit10_20 
table(datALSP$audit_mytot20)
#CCU3095: DV total Audit score 
table(datALSP$CCU3095)
datALSP$audit_tot20<- datALSP$CCU3095
hist(datALSP$audit_tot20)# distribution is skewed, but not too bad. 

#CCU3050: Have had a whole drink	
table(datALSP$CCU3050, exclude = FALSE) # my overall score has not 0, while the DV score in the dataset does ~ this is becuase ps who reported they never had alcohol were assigned score of 0. 

################## Cast ##########################

## Frequency cannabis use 

#CCU3300: Tried cannabis
#original coding: 1=yes; 2= No; NA= 11229 
table(datALSP$CCU3300, exclude = FALSE)

#CCU3302: In the last 12 months, frequency used cannabis			
#original coding: 1= once or twice; 2= less than monthly; 3= monthly; 4= weekly; 5=daily or almost; 6= not in last 12 months; NA = 13288 
table(datALSP$CCU3302, exclude = FALSE)

# new variable for frequency of cannabis use 
cannabis<- function (can_ever, can_12month) {
  can <- can_ever
  can[can_ever == "2"] = 0 # never tried cannabis  
  can[can_12month == "6"] = 1 # not in past 12 months 
  can[can_12month == "1"] = 2 # once or twice in past 12 months 
  can[can_12month == "2"] = 3 # less than monthly
  can[can_12month == "3"] = 4 # monthly
  can[can_12month == "4"] = 5 # weekly
  can[can_12month == "5"] = 6 # daily or almost 
  return(can)
}

datALSP$can_fre20<- cannabis(datALSP$CCU3300, datALSP$CCU3302)
#check coding is right 
table(datALSP$can_fre20, datALSP$CCU3302)
table(datALSP$can_fre20, datALSP$CCU3300)

## cast items 
#CCU3320: In the past 12 months, used cannabis before midday			
#original coding: 1=never; 2=rarely; 3=from time to time; 4=fairly often; 5=often; NA = 13310 
table(datALSP$CCU3320, exclude = FALSE)

#function to recode items on 0-4 scale 
cast<- function (item) {
  cast<- item
  cast [item == "1"] <- 0
  cast [item == "2"] <- 1
  cast [item == "3"] <- 2
  cast [item == "4"] <- 3
  cast [item == "5"] <- 4
  return(cast)
}

datALSP$cast1_20<- cast(datALSP$CCU3320)
table(datALSP$cast1_20, datALSP$CCU3320)
mean(datALSP$cast1_20, na.rm = TRUE) # 0.4117


#CCU3321: In the past 12 months, used cannabis when they were alone			
#original coding: 1=never; 2=rarely; 3=from time to time; 4=fairly often; 5=often; NA = 13313 
table(datALSP$CCU3321, exclude = FALSE)

datALSP$cast2_20<- cast(datALSP$CCU3321)
table(datALSP$cast2_20, datALSP$CCU3321)
mean(datALSP$cast2_20, na.rm = TRUE) #  0.3878

#CCU3322: In the past 12 months, had memory problems when they've used cannabis			
#original coding:1=never; 2=rarely; 3=from time to time; 4=fairly often; 5=often; NA = 13317
table(datALSP$CCU3322, exclude = FALSE)

datALSP$cast3_20<- cast(datALSP$CCU3322)
table(datALSP$cast3_20, datALSP$CCU3322)
mean(datALSP$cast3_20, na.rm = TRUE) # 0.3519

#CCU3323: In the past 12 months, friends or members of respondent's family have ever told them they ought to reduce their cannabis use			
#original coding:1=never; 2=rarely; 3=from time to time; 4=fairly often; 5=often; NA = 13316
table(datALSP$CCU3323, exclude = FALSE)

datALSP$cast4_20<- cast(datALSP$CCU3323)
table(datALSP$cast4_20, datALSP$CCU3323)
mean(datALSP$cast4_20, na.rm = TRUE) # 0.186

#CCU3324: In the past 12 months, tried to reduce or stop their cannabis use without success			
#original coding:1=never; 2=rarely; 3=from time to time; 4=fairly often; 5=often; NA =  13317
table(datALSP$CCU3324, exclude = FALSE)

datALSP$cast5_20<- cast(datALSP$CCU3324)
table(datALSP$cast5_20, datALSP$CCU3324)
mean(datALSP$cast5_20, na.rm = TRUE)# 0.1414


#CCU3325: In the past 12 months, had problems because of their use of cannabis (argument, fight, accident, bad result at school, other problems)			
#original coding:1=never; 2=rarely; 3=from time to time; 4=fairly often; 5=often; NA = 13313
table(datALSP$CCU3325, exclude = FALSE) 

datALSP$cast6_20<- cast(datALSP$CCU3325)
table(datALSP$cast6_20, datALSP$CCU3325)
mean(datALSP$cast6_20, na.rm = TRUE)# 0.1355

#tot score 
datALSP$cast_tot20<- datALSP$cast1_20 + datALSP$cast2_20 + datALSP$cast3_20 + datALSP$cast4_20 + datALSP$cast5_20 + datALSP$cast6_20
table(datALSP$cast_tot20)

table(datALSP$cast_tot20, datALSP$can_fre20, exclude = FALSE) # people who never tried cannabis coded as NA

# add +1 to distinguish between those who have never tried cannabis and those who have 
datALSP$cast_tot20<- datALSP$cast_tot20 + 1 
table(datALSP$cast_tot20, exclude = TRUE)

#assign score of 0 to those who have never tried cannabis and 1 rarerly 

datALSP$cast_tot20[datALSP$can_fre20 <2]<- 0
datALSP$cast_tot20[datALSP$can_fre20 == 1]<- 1
table(datALSP$cast_tot20, datALSP$can_fre20, exclude = F)
hist(datALSP$cast_tot20) 


save(datALSP, file = "datALSP.R")

##Note: tot score DV was available in dataset but only ranged between 0-6 rather than 0-24
########################## Other drugs ####################################

## cocaine 
#CCU3400: In their life YP has ever used cocaine (coke, crack etc.); 1=yes; 2=no; NA = 11205			
#CCU3401: In the last year YP has tried  cocaine (coke, crack etc.); 1=yes; 2=no		
#CCU3402: In the last three months YP has tried  cocaine (coke, crack etc.); 1=yes; 2=no				
table(datALSP$CCU3400, exclude = FALSE)
table(datALSP$CCU3401, exclude = FALSE)
table(datALSP$CCU3402, exclude = FALSE)

drug<- function (drug_ever, drug_year, drug_month){
  drug_var<- drug_ever
  drug_var[drug_ever == "2"]<- 0 # never used
  drug_var[drug_ever == "1" & drug_year == "2"]<- 1 # not used in last 12 months
  drug_var[drug_year == "1" & drug_month == "2"]<- 2 # not used in last 3 months
  drug_var[drug_month == "1"]<- 3 # used in last three months 
  return(drug_var)
}

datALSP$coke_20<- drug(datALSP$CCU3400, datALSP$CCU3401, datALSP$CCU3402)
table(datALSP$coke_20, exclude = TRUE)

## amphetamine-type stimulants
#CCU3410: In their life YP has ever used amphetamine-type stimulants (speed, diet pills, ecstasy, etc.); 1=yes; 2=no;NA = 11237  				
#CCU3411: In the last year YP has tried amphetamine-type stimulants 				
#CCU3412: In the last three months YP has tried  amphetamine-type stimulants 			
table(datALSP$CCU3410, exclude = FALSE)
table(datALSP$CCU3411, exclude = FALSE)
table(datALSP$CCU3412, exclude = FALSE)

datALSP$amph_20<- drug(datALSP$CCU3410, datALSP$CCU3411, datALSP$CCU3412)
table(datALSP$amph_20, exclude = TRUE)

## inhalants 
#CCU3420: In their life YP has ever used inhalants (nitrous, glue, petrol, paint thinner,etc.); NA = 11231 			
#CCU3421: In the last year YP has tried  inhalants 				
#CCU3422: In the last three months YP has tried  inhalants 
table(datALSP$CCU3420, exclude = FALSE)
table(datALSP$CCU3421, exclude = FALSE)
table(datALSP$CCU3422, exclude = FALSE)

datALSP$inha_20<- drug(datALSP$CCU3420, datALSP$CCU3421, datALSP$CCU3422)
table(datALSP$inha_20, exclude = TRUE)

## sedatives 
#CCU3430: In their life YP has ever used sedatives or sleeping pills (Valium, Rohypnol, etc.), NA = 11241 			
#CCU3431: In the last year YP has tried  sedatives or sleeping pills 			
#CCU3432: In the last three months YP has tried sedatives or sleeping pills 				
table(datALSP$CCU3430, exclude = FALSE)
table(datALSP$CCU3431, exclude = FALSE)
table(datALSP$CCU3432, exclude = FALSE)

datALSP$seda_20<- drug(datALSP$CCU3430, datALSP$CCU3431, datALSP$CCU3432)
table(datALSP$seda_20, exclude = TRUE)

## hallucinogens
#CCU3440: In their life YP has ever used hallucinogens (LSD, acid, mushrooms, PCP, Ketamine, Special K, etc.); NA = 11244 				
#CCU3441: In the last year YP has tried  hallucinogens 			
#CCU3442: In the last three months YP has tried  hallucinogens 			
table(datALSP$CCU3440, exclude = FALSE)
table(datALSP$CCU3441, exclude = FALSE)
table(datALSP$CCU3442, exclude = FALSE)

datALSP$hall_20<- drug(datALSP$CCU3440, datALSP$CCU3441, datALSP$CCU3442)
table(datALSP$hall_20, exclude = TRUE)

## opioids
#CCU3450: In their life YP has ever used opioids (heroin, morphine, methadone, codeine, etc.)				
#CCU3451: In the last year YP has tried  opioids 				
#CCU3452: In the last three months YP has tried  opioids 		
table(datALSP$CCU3450, exclude = FALSE)
table(datALSP$CCU3451, exclude = FALSE)
table(datALSP$CCU3452, exclude = FALSE)

datALSP$opio_20<- drug(datALSP$CCU3450, datALSP$CCU3451, datALSP$CCU3452)
table(datALSP$opio_20, exclude = TRUE) # very low prop of users 

## injected illicit drugs
#CCU3460: In their life YP has ever used injected illicit drugs; nA =11252 			
#CCU3461: In the last year YP has tried  injected illicit drugs				
#CCU3462: In the last three months YP has tried  injected illicit drugs				
table(datALSP$CCU3460, exclude = FALSE)
table(datALSP$CCU3461, exclude = FALSE)
table(datALSP$CCU3462, exclude = FALSE)

datALSP$injd_20<- drug(datALSP$CCU3460, datALSP$CCU3461, datALSP$CCU3462)
table(datALSP$injd_20, exclude = TRUE)# very low prop 

##sedatives 
#CCU3431	D42ei: In the last year YP has tried  sedatives or sleeping pills (Valium, Rohypnol, etc.)	

## overall scores 
#CCU3470: Count of types illicit substances YP has ever used				
#CCU3471: Count of types illicit substances YP has used in the last year				
#CCU3472: Count of types illicit substances YP has used in the last 3 months				
table(datALSP$CCU3470, exclude = FALSE)# range bet 0-7
table(datALSP$CCU3471, exclude = FALSE)# range bet 0-6
table(datALSP$CCU3472, exclude = FALSE)# range bet 0-6

# for analysis use: CCU3471: Count of types illicit substances YP has used in the last year	
datALSP$drug_tot20<- datALSP$CCU3471
table(datALSP$drug_tot20, exclude =FALSE)
hist(datALSP$drug_tot20) # not too bad 

save(datALSP, file = "datALSP.R")
load("datALSP.R")


################################################# sort out substance use scores for all available measurement points #########################
load("datALSP.R")
### ### Fagestrom: Assessed at 17.5(clinic), 20+, 21+, 22+ yrs ###

### age 21 ###
#overall score NA, need to compute it ##

## frequency of smoking  ##
#YPA6000: ever smoked
#YPA6010: smoked last month 
#YPA6030: smokes every week
#YPA6020: smokes every day 

datALSP$cig21<- cig_fre(datALSP$YPA6000, datALSP$YPA6010, datALSP$YPA6030, datALSP$YPA6020)
table(datALSP$cig21)
sum(is.na(datALSP$cig21))

## recode items ##

#YPA6021:number of cigarettes smoked per day, on average
table(datALSP$YPA6021)

#new variable with 4 levels 
datALSP$FT1_21<-datALSP$YPA6021
datALSP$FT1_21<- as.numeric(as.character(datALSP$FT1_21))
datALSP$FT1_21[datALSP$FT1_21 <= 10] <- 0
datALSP$FT1_21[datALSP$FT1_21 > 10 & datALSP$FT1_21 <= 20] <- 1
datALSP$FT1_21[datALSP$FT1_21 > 20 & datALSP$FT1_21 <= 30] <- 2
datALSP$FT1_21[datALSP$FT1_21 > 30]<- 3
table(datALSP$FT1_21)
mean(datALSP$FT1_21, na.rm = TRUE)

#YPA6040: Amount of time after YP wakes up before smoke their first cigarette	
table(datALSP$YPA6040, exclude = FALSE)

#new variable with 4 levels
datALSP$FT2_21<- datALSP$YPA6040
datALSP$FT2_21[datALSP$YPA6040 == "1"]<- 3
datALSP$FT2_21[datALSP$YPA6040 == "2"]<- 2
datALSP$FT2_21[datALSP$YPA6040 == "3"]<- 1
datALSP$FT2_21[datALSP$YPA6040 == "4"]<- 0
table(datALSP$FT2_21)
mean(datALSP$FT2_21, na.rm = TRUE) 

#YPA6050: YP finds it difficult to refrain from smoking in places where it is forbidden (eg in church, buses, trains, the library, cinemas)	
table(datALSP$YPA6050, exclude = FALSE)

#new variable with 2 levels
datALSP$FT3_21<- datALSP$YPA6050
datALSP$FT3_21[datALSP$YPA6050 == "2"]<- 0
datALSP$FT3_21[datALSP$YPA6050 == "1"]<- 1
table(datALSP$FT3_21)
mean(datALSP$FT3_21, na.rm = TRUE)


#YPA6060: The cigarette that YP would most hate to give up
table(datALSP$YPA6060, exclude = FALSE)
#new variable with 2 levels
datALSP$FT4_21<- datALSP$YPA6060
datALSP$FT4_21[datALSP$YPA6060 == "1"]<- 1
datALSP$FT4_21[datALSP$YPA6060 == "2"]<- 0
table(datALSP$FT4_21)
mean(datALSP$FT4_21, na.rm = TRUE)


#YPA6070: YP smokes more frequently during the first hours after waking than during the rest of the day	
table(datALSP$YPA6070, exclude = FALSE)
#new variable with 2 levels
datALSP$FT5_21<- datALSP$YPA6070
datALSP$FT5_21 [datALSP$YPA6070 == "1"]<- 1
datALSP$FT5_21 [datALSP$YPA6070 == "2"]<- 0
table(datALSP$FT5_21)
mean(datALSP$FT5_21, na.rm = TRUE)#


#YPA6080: YP smokes if they are so ill that they are in bed most of the day	
table(datALSP$YPA6080, exclude = FALSE)
#new variable with 2 levels
datALSP$FT6_21<- datALSP$YPA6080
datALSP$FT6_21[datALSP$YPA6080 == "1"]<- 1
datALSP$FT6_21[datALSP$YPA6080 == "2"]<- 0
table(datALSP$FT6_21)
mean(datALSP$FT6_21, na.rm = TRUE) 

#compute my overall score 
datALSP$FT_total21<- datALSP$FT1_21 + datALSP$FT2_21 + datALSP$FT3_21 + datALSP$FT4_21 + datALSP$FT5_21 + datALSP$FT6_21
table(datALSP$FT_total21, exclude = FALSE) # score range between 0-9
table(datALSP$FT_total21, datALSP$cig21, exclude = FALSE)# ps who don't smoke every day are coded as NA

#add +1 to overall score 
datALSP$FT_total21<- datALSP$FT_total21 +1 
table(datALSP$FT_total21)

# assign score of 0 to those who don't smoke every day 
datALSP$FT_total21[datALSP$cig21 %in% c(0,1,2, 3)]<- 0
table(datALSP$FT_total21, datALSP$cig21, exclude = FALSE) # looks good:)
hist(datALSP$FT_total21) # extremely skewed

### age 22 ###
# overall score available 

### frequency od smoking ###
#YPB4000: ever smoked 
#YPB4010: smoked in past 30 days 
#YPB4020: smokes every week
#YPB4030: smokes every day 

datALSP$cig22<- cig_fre(datALSP$YPB4000, datALSP$YPB4010, datALSP$YPB4020, datALSP$YPB4030)
table(datALSP$cig22)
sum(is.na(datALSP$cig22))


#YPB4121: overall Fagerstrom score	
table(datALSP$YPB4121) # score range between 0 and 9
table(datALSP$YPB4121, datALSP$cig22, exclude = FALSE) # ps who don't smoke daily coded as NA

#create new overall score and add +1 
datALSP$FT_total22<- datALSP$YPB4121 + 1
table(datALSP$FT_total22)

# assign score of 0 to those who don't smoke every day 
datALSP$FT_total22[datALSP$cig22 %in% c(0,1,2, 3)]<- 0
table(datALSP$FT_total22, datALSP$cig22, exclude = FALSE) # looks good:)
hist(datALSP$FT_total22) 

### age 17.5 ###
# overall score available 

## frequency of smoking ##
#FJSM050: ever smoked 
#FJSM250: smoked in past 30 days 
#FJSM450: smokes every week
#FJSM350: smokes evry day 


datALSP$cig17.5<- cig_fre(datALSP$FJSM050, datALSP$FJSM250, datALSP$FJSM450, datALSP$FJSM350)
table(datALSP$cig17.5)
sum(is.na(datALSP$cig17.5))

#FJSM1000: FTND total score 
table(datALSP$FJSM1000, exclude =FALSE) # scores range bet 0-10
table(datALSP$FJSM1000,datALSP$cig17.5, exclude =FALSE) # ps who don't smoke daily coded as NA

#create new overall score and add +1 
datALSP$FT_total17.5<- datALSP$FJSM1000 + 1
table(datALSP$FT_total17.5)

# assign score of 0 to those who don't smoke every day 
datALSP$FT_total17.5[datALSP$cig17.5 %in% c(0,1,2, 3)]<- 0
table(datALSP$FT_total17.5, datALSP$cig17.5, exclude = FALSE) # looks good:)
hist(datALSP$FT_total17.5) 

save(datALSP, file = "datALSP.R")

### ### Audit: 16, 17.5 (clinic), 18, 20+, 22+ ###

### age 16 ###
#no overall score, need to compute it 

#ccs3540: Frequency YP has a drink containing alcohol	
datALSP$audit1_16<-audit(datALSP$ccs3540)
table(datALSP$audit1_16, exclude = FALSE)

#ccs3541: Number of units YP consumes on a day when they are consuming alcohol				
datALSP$audit2_16<-audit(datALSP$ccs3541)
table(datALSP$audit2_16, exclude = FALSE)

#ccs3542: Frequency YP has 6 or more units of alcohol on one occasion
datALSP$audit3_16<- audit(datALSP$ccs3542)
table(datALSP$audit3_16, exclude = FALSE)

#ccs3543: Frequency during the last year YP has found they were unable to stop drinking once they'd started				
datALSP$audit4_16<- audit(datALSP$ccs3543)
table(datALSP$audit4_16, exclude = FALSE)

#ccs3544: Frequency during the last year YP has failed to do what was normally expected of them because of drinking				
datALSP$audit5_16<- audit(datALSP$ccs3544)
table(datALSP$audit5_16, exclude = FALSE)

#ccs3545: Frequency during the last year YP has needed a drink in the morning to get them going after a heavy drinking session				
datALSP$audit6_16<- audit(datALSP$ccs3545)
table(datALSP$audit6_16, exclude = FALSE)

#ccs3546: Frequency during the last year YP has felt guilty/remorseful after drinking				
datALSP$audit7_16<- audit(datALSP$ccs3546)
table(datALSP$audit7_16, exclude = FALSE)

#ccs3547: Frequency during the last year YP has been unable to remember the night before because of drinking				
datALSP$audit8_16<- audit(datALSP$ccs3547)
table(datALSP$audit8_16, exclude = FALSE)

#ccs3548: Frequency during the last year someone has been injured as a result of YP drinking				
datALSP$audit9_16<- audit2(datALSP$ccs3548)
table(datALSP$audit9_16, exclude = FALSE)

#ccs3549: A relative/friend/doctor/health worker has been concerned about YP's drinking/suggested they cut down				
datALSP$audit10_16<- audit2(datALSP$ccs3549)
table(datALSP$audit10_16, exclude = FALSE)

# compute tot score 
datALSP$audit_tot16<- datALSP$audit1_16 +datALSP$audit2_16 + datALSP$audit3_16 + datALSP$audit4_16 + datALSP$audit5_16 + datALSP$audit6_16 + datALSP$audit7_16 + datALSP$audit8_16 + datALSP$audit9_16 + datALSP$audit10_16 
table(datALSP$audit_tot16)  # scores range between 0 and 36; in this case ps with 0 are those who have never had a drink in past 12 months 

#ccs3500: ever had alcohol 
table(datALSP$ccs3500)
table(datALSP$audit_tot16, datALSP$ccs3500, exclude = TRUE) ## all people who reported never had alc are coded as NA in tot score 

#assign score of 0 to those who have never had alc
datALSP$audit_tot16[datALSP$ccs3500 == "2"]<- 0
table(datALSP$audit_tot16, datALSP$ccs3500, exclude = TRUE)
hist(datALSP$audit_tot16)


### age 17.5 ###
# overall score is available 


#FJAL4000: audit tot score 
table(datALSP$FJAL4000) # range between 1 and 36
datALSP$audit_tot17.5<- datALSP$FJAL4000
table(datALSP$audit_tot17.5)

#FJAL050: YP has ever had a whole drink 
table(datALSP$FJAL050)
str(datALSP$FJAL050)
table(datALSP$audit_tot17.5,datALSP$FJAL050,exclude = FALSE) ## all people who reported never had alc are coded as NA in tot score 

#assign score of 0 to those who have never had alc
datALSP$audit_tot17.5[datALSP$FJAL050 == "2"]<- 0
table(datALSP$audit_tot17.5, datALSP$FJAL050, exclude = TRUE)
hist(datALSP$audit_tot17.5)

### age 18 ###
#overall score not available 
# new function to recode audit items (becuase some have an additional level)
audit<- function (audit) {
  alc_var <- audit
  alc_var[audit == "1"] = 0 
  alc_var[audit == "2"] = 1
  alc_var[audit == "3"] = 2
  alc_var[audit == "4"] = 3
  alc_var[audit %in% c("5", "6")] = 4
  return(alc_var)
}

#cct5030: Frequency respondent has drinks containing alcohol	
datALSP$audit1_18<-audit(datALSP$cct5030)
hist(datALSP$audit1_18, exclude = FALSE)

#cct5031: Number of units respondent has on typical day when drinking	
datALSP$audit2_18<-audit(datALSP$cct5031)
table(datALSP$audit2_18, exclude = FALSE)

#cct5032: Frequency respondent had six or more units on one occasion in past year	
table(datALSP$cct5032)
datALSP$audit3_18<-audit(datALSP$cct5032)
table(datALSP$audit3_18, exclude = FALSE)

#cct5033: Frequency found self unable to stop drinking once started in past year	
datALSP$audit4_18<-audit(datALSP$cct5033)
table(datALSP$audit4_18, exclude = FALSE)

#cct5034: Frequency respondent failed to do what was normally expected of respondent (e.g. school/college, work, sport, go out) because of respondent's drinking in past year	
datALSP$audit5_18<-audit(datALSP$cct5034)
table(datALSP$audit5_18, exclude = FALSE)

#cct5035: Frequency respondent needed first drink in morning to get going after heavy drinking session in past year	
datALSP$audit6_18<-audit(datALSP$cct5035)
table(datALSP$audit6_18, exclude = FALSE)

#cct5036: Frequency respondent had feeling of guilt or remorse after drinking in past year	
datALSP$audit7_18<-audit(datALSP$cct5036)
table(datALSP$audit7_18, exclude = FALSE)

#cct5037: Frequency respondent been unable to remember what happened night before because of drinking in past year	
datALSP$audit8_18<-audit(datALSP$cct5037)
table(datALSP$audit8_18, exclude = FALSE)

#cct5038: Respondent or someone else been injured as a result of respondent's drinking	
datALSP$audit9_18<-audit2(datALSP$cct5038)
table(datALSP$audit9_18, exclude = FALSE)

#cct5039: Relative/friend/doctor/other health worker been concerned about respondent's drinking or suggested that respondent cut down	
datALSP$audit10_18<-audit2(datALSP$cct5039)
table(datALSP$audit10_18, exclude = FALSE)

#compute overall score 
datALSP$audit_tot18<- datALSP$audit1_18 +datALSP$audit2_18 + datALSP$audit3_18 + datALSP$audit4_18 + datALSP$audit5_18 + datALSP$audit6_18 + datALSP$audit7_18 + datALSP$audit8_18 + datALSP$audit9_18 + datALSP$audit10_18 
table(datALSP$audit_tot18) #range between 1 and 37

#cct5020: Respondent has ever had a whole alcoholic drink				
table(datALSP$cct5020)
table(datALSP$audit_tot18,datALSP$cct5020, exclude = FALSE) ## all people who reported never had alc are coded as NA in tot score 

#assign score of 0 to those who have never had alc
datALSP$audit_tot18[datALSP$cct5020 == "2"]<- 0
table(datALSP$audit_tot18, datALSP$cct5020, exclude = TRUE)
hist(datALSP$audit_tot18)

### age 22 ###
#tot score is available 

#YPB4388a: Total audit score	
table(datALSP$YPB4388a) # range between 0 and 32
datALSP$audit_tot22<- datALSP$YPB4388a

#YPB4130: Have had a whole drink 
table(datALSP$YPB4388a,datALSP$YPB4130, exclude = FALSE ) # score is fine ~ those who have never had alc have a score of 0. 

save(datALSP, file = "datALSP.R")

### ### Cast: 16, 17.5 (clinic), 18, 20+, 22+ ###

### age 16 ###
#no overall score available 

## frequency cannabis use #

#ccs4060: YP has ever tried cannabis	
table(datALSP$ccs4060)

#ccs4065: Frequency YP smokes cannabis		
table(datALSP$ccs4065)

datALSP$can_fre16<- cannabis(datALSP$ccs4060, datALSP$ccs4065)
table(datALSP$can_fre16, exclude = FALSE)

## cast items 

#ccs4140: Since their 15th birthday YP has used cannabis before midday	
datALSP$cast1_16<- cast(datALSP$ccs4140)
table(datALSP$cast1_16, exclude = FALSE)

#ccs4141: Since their 15th birthday YP has used cannabis when they were alone	
datALSP$cast2_16<- cast(datALSP$ccs4141)
table(datALSP$cast2_16, exclude = FALSE)

#ccs4142: Since their 15th birthday YP has had memory problems when they used cannabis	
datALSP$cast3_16<- cast(datALSP$ccs4142)
table(datALSP$cast3_16, exclude = FALSE)

#ccs4143: Since their 15th birthday YP's friend/family member has told them to reduce their cannabis use	
datALSP$cast4_16<- cast(datALSP$ccs4143)
table(datALSP$cast4_16, exclude = FALSE)

#ccs4144: Since their 15th birthday YP has tried to reduce their cannabis use without succeeding	
datALSP$cast5_16<- cast(datALSP$ccs4144)
table(datALSP$cast5_16, exclude = FALSE)

#ccs4145: Since their 15th birthday YP has had problems because of their cannabis use	
datALSP$cast6_16<- cast(datALSP$ccs4145)
table(datALSP$cast6_16, exclude = FALSE)

## tot score 
datALSP$cast_tot16<- datALSP$cast1_16 + datALSP$cast2_16 + datALSP$cast3_16 + datALSP$cast4_16 + datALSP$cast5_16 + datALSP$cast6_16
table(datALSP$cast_tot16, exclude = FALSE)# range 0-18
table(datALSP$cast_tot16,datALSP$can_fre16, exclude = FALSE) #ps who have never tried cannabis are coded as NA

#add +1 to tot score 
datALSP$cast_tot16 <- datALSP$cast_tot16 + 1

#assign 0 to ps who have never tried cannabis 
datALSP$cast_tot16[datALSP$can_fre16 == 0]<- 0
table(datALSP$cast_tot16)
hist(datALSP$cast_tot16) 

### age 17.5 ###

#FJDR050: YP has ever tried cannabis
table(datALSP$FJDR050)

#FJDR250: Frequency YP uses cannabis 														
table(datALSP$FJDR250) # coded as 1= monthly or less; 2=2-4 times month; 3=2-3 times week; 4=4+ times

datALSP$can_fre17.5<- datALSP$FJDR250
datALSP$can_fre17.5[datALSP$FJDR050 == "2"]<- 0
table(datALSP$can_fre17.5, exclude = FALSE) 

### cast items 

#FJDR1000: YP has ever used cannabis before midday, in the past 12 months [F17]
table(datALSP$FJDR1000)
datALSP$cast1_17.5<- cast(datALSP$FJDR1000)
table(datALSP$cast1_17.5, exclude = FALSE)

#FJDR1050: YP has ever used cannabis when they were alone, in the past 12 months [F17]
datALSP$cast2_17.5<- cast(datALSP$FJDR1050)
table(datALSP$cast2_17.5, exclude = FALSE)

#FJDR1100: YP has ever had memory problems when they used cannabis, in the past 12 months [F17]
datALSP$cast3_17.5<- cast(datALSP$FJDR1100)
table(datALSP$cast3_17.5, exclude = FALSE)

#FJDR1150: YP has friends or family members tell them they ought to reduce their cannabis use, in the past 12 months [F17]
datALSP$cast4_17.5<- cast(datALSP$FJDR1150)
table(datALSP$cast4_17.5, exclude = FALSE)

#FJDR1200: YP has ever tried to reduce or stop their cannabis use without succeeding, in the past 12 months [F17]
datALSP$cast5_17.5<- cast(datALSP$FJDR1200)
table(datALSP$cast5_17.5, exclude = FALSE)

#FJDR1250: YP has ever had problems because of their use of cannabis in the past 12 months [F17]
datALSP$cast6_17.5<- cast(datALSP$FJDR1250)
table(datALSP$cast6_17.5, exclude = FALSE)

## tot score 
datALSP<-alsp3
datALSP$cast_tot17.5<- datALSP$cast1_17.5 + datALSP$cast2_17.5 + datALSP$cast3_17.5 + datALSP$cast4_17.5 + datALSP$cast5_17.5 + datALSP$cast6_17.5
table(datALSP$cast_tot17.5)#range 0-24
table(datALSP$cast_tot17.5,datALSP$can_fre17.5, exclude =FALSE ) # never tried all coded as NA

#add +1 
datALSP$cast_tot17.5<- datALSP$cast_tot17.5 + 1

# assign 0 to those who have never tried and 1 to rarerly  

datALSP$cast_tot17.5[datALSP$can_fre17.5 <2 ]<- 0
datALSP$cast_tot17.5[datALSP$can_fre17.5 == 1]<- 1
table(datALSP$cast_tot17.5, datALSP$can_fre17.5, exclude = F)# ok 

save(datALSP, file = "datALSP.R")

### age 18 ###

#cct5050: Respondent tried cannabis	
table(datALSP$cct5050)
#cct5055	E18: Frequency used cannabis in past 12 months	
table(datALSP$cct5055) # different coding: 1=once or twice; 2=less than monthly; 3=monthly; 4=weekly; 5=daily or almost daily

# new variable for frequency of use
datALSP$can_fre18<- datALSP$cct5055
datALSP$can_fre18[datALSP$cct5050 == "2"]<- 0
table(datALSP$can_fre18, exclude=FALSE) 

## cast items ##

#cct5070: Respondent used cannabis before midday
table(datALSP$cct5070) #item already coded 0-4
datALSP$cast1_18<- datALSP$cct5070
table(datALSP$cast1_18, exclude = FALSE)

#cct5071: Respondent used cannabis when alone
table(datALSP$cct5071) #item already coded 0-4
datALSP$cast2_18<- datALSP$cct5071
table(datALSP$cast2_18, exclude = FALSE)

#cct5072: Respondent had memory problems when using cannabis
table(datALSP$cct5072) #item already coded 0-4
datALSP$cast3_18<- datALSP$cct5072
table(datALSP$cast3_18, exclude = FALSE)

#cct5073: Respondent's friends or family members told respondent that respondent ought to reduce cannabis use
table(datALSP$cct5073) #item already coded 0-4
datALSP$cast4_18<- datALSP$cct5073
table(datALSP$cast4_18, exclude = FALSE)

#cct5074: Respondent tried to reduce or stop cannabis use without succeeding
table(datALSP$cct5074) #item already coded 0-4
datALSP$cast5_18<- datALSP$cct5074
table(datALSP$cast5_18, exclude = FALSE)

#cct5075: Respondent had problems (e.g. argument, fight, accident, bad result at school, other problems) because of use of cannabis
table(datALSP$cct5075) #item already coded 0-4
datALSP$cast6_18<- datALSP$cct5075
table(datALSP$cast6_18, exclude = FALSE)


##overall score 
datALSP$cast_tot18<- datALSP$cast1_18 + datALSP$cast2_18 + datALSP$cast3_18 + datALSP$cast4_18 +datALSP$cast5_18 +datALSP$cast6_18
table(datALSP$cast_tot18, exclude = FALSE) #range from 0 to 24
table(datALSP$cast_tot18, datALSP$can_fre18, exclude = FALSE) # all ps who have never used cannabis are coded as NA

# add +1 to score 
datALSP$cast_tot18<-datALSP$cast_tot18 + 1
table(datALSP$cast_tot18)

#assign 0 to ps who have never tried cannabis 
datALSP$cast_tot18[datALSP$can_fre18 == 0]<- 0
table(datALSP$cast_tot18)# ok 

### age 22 ###
#overall DV score is available but does not look good 
datALSP<- alsp3
#YPB4390:ever tried cannabis
table(datALSP$YPB4390)
table(alsp3$YPB4390, exclude = F)

#YPB4400:  In the last 12 months, frequency used cannabis	
table(datALSP$YPB4400)# same coding as age 20

# new variable for frequency of use 
datALSP$can_fre22<- cannabis(datALSP$YPB4390, datALSP$YPB4400)
table(datALSP$can_fre22, exclude = F)

save(datALSP, file = "datALSP.R")
load("datALSP.R")

## cast items ##
#YPB4430: In the past 12 months, used cannabis before midday
table(datALSP$YPB4430, exclude = FALSE)#items coded 1-5
datALSP$cast1_22<- cast(datALSP$YPB4430)
table(datALSP$cast1_22)

#YPB4431: In the past 12 months, used cannabis when they were alone
table(datALSP$YPB4431, exclude = FALSE)#items coded 1-5
datALSP$cast2_22<- cast(datALSP$YPB4431)
table(datALSP$cast2_22)

#YPB4432: In the past 12 months, had memory problems when they had used cannabis
table(datALSP$YPB4432, exclude = FALSE)#items coded 1-5
datALSP$cast3_22<- cast(datALSP$YPB4432)
table(datALSP$cast3_22)


#YPB4433: In the past 12 months, friends or members of respondents family have ever said to reduce cannabis use
table(datALSP$YPB4433, exclude = FALSE)#items coded 1-5
datALSP$cast4_22<- cast(datALSP$YPB4433)
table(datALSP$cast4_22)

#YPB4434: In the past 12 months, tried to reduce or stop their cannabis use without succeeding
table(datALSP$YPB4434, exclude = FALSE)#items coded 1-5
datALSP$cast5_22<- cast(datALSP$YPB4434)
table(datALSP$cast5_22)

#YPB4435: In the past 12 months, had problems because of their use of cannabis (argument, fight, accident, other problems)
table(datALSP$YPB4435, exclude = FALSE)#items coded 1-5
datALSP$cast6_22<- cast(datALSP$YPB4435)
table(datALSP$cast6_22)

#tot score 
datALSP$cast_tot22<- datALSP$cast1_22 + datALSP$cast2_22 + datALSP$cast3_22 + datALSP$cast4_22 +datALSP$cast5_22 +datALSP$cast6_22
table(datALSP$cast_tot22, exclude = FALSE) #range from 0 to 24
table(datALSP$cast_tot22, datALSP$can_fre22, exclude = FALSE) # all ps who have never used cannabis are coded as NA

# add +1 to score 
datALSP$cast_tot22<-datALSP$cast_tot22 + 1
table(datALSP$cast_tot22)

#assign 0 to ps who have never tried cannabis and 1 to rarely 
datALSP$cast_tot22[datALSP$can_fre22 < 2]<- 0
datALSP$cast_tot22[datALSP$can_fre22 == 1]<- 1
table(datALSP$cast_tot22,datALSP$can_fre22, exclude = F)# ok 
hist(datALSP$cast_tot22)

### ### Other drugs: 16, 17.5(clinic), 18, 20+, 22+ ###

### age 16 ### drugs assessed in different way - leave out for now 

### age 17.5 ###
#note: injected illicit drugs NA for 12 months, but additional variable for whether tried any other substance 
#note: DV score NA 

#FJDR5050	DR2010: YP has tried cocaine in the last year [F17]	
str(datALSP$FJDR5050)
table(datALSP$FJDR5050)
table(datALSP$FJDR5050, datALSP$FJDR5000, exclude = FALSE) #item coded as 1=yes; 2=no - however, ps who have never tried it are coded as NA

#new function to recode items: 0=never tried or not in last 12 motnhs;1=yes in past 12 months 
drug<- function (drug_ever, drug_year){
  drug<-drug_ever
  drug [drug_ever == "2"]<- 0
  drug [drug_ever == "1"]<- 0
  drug [drug_year == "2"]<- 0
  drug [drug_year == "1"]<- 1
  return(drug)
}
datALSP$coke_17.5<- drug(datALSP$FJDR5000,datALSP$FJDR5050)
table(datALSP$coke_17.5)#ok 

#FJDR5200	DR2013: YP has tried amphetamines in the last year [F17]
table(datALSP$FJDR5200)
table(datALSP$FJDR5200, datALSP$FJDR5150, exclude = FALSE) 
datALSP$amph_17.5<- drug(datALSP$FJDR5150, datALSP$FJDR5200)
table(datALSP$amph_17.5)

#FJDR5350	DR2016: YP has tried inhalants in the last year [F17]
table(datALSP$FJDR5350)
table(datALSP$FJDR5300, datALSP$FJDR5350, exclude = FALSE)
datALSP$inha_17.5<- drug(datALSP$FJDR5300, datALSP$FJDR5350)
table(datALSP$inha_17.5)

#FJDR5500	DR2019: YP has tried Sedatives or Sleeping Pills in the last year [F17]
table(datALSP$FJDR5500)
table(datALSP$FJDR5450, datALSP$FJDR5500, exclude = FALSE)
datALSP$seda_17.5<- drug(datALSP$FJDR5450, datALSP$FJDR5500)
table(datALSP$seda_17.5)

#FJDR5650	DR2022: YP has tried Hallucinogens in the last year [F17]
table(datALSP$FJDR5650)
table(datALSP$FJDR5600, datALSP$FJDR5650, exclude = FALSE)
datALSP$hall_17.5<- drug(datALSP$FJDR5600, datALSP$FJDR5650)
table(datALSP$hall_17.5)

#FJDR5800	DR2025: YP has tried Opioids in the last year [F17]
table(datALSP$FJDR5800)
table(datALSP$FJDR5750, datALSP$FJDR5800, exclude = TRUE)
datALSP$opio_17.5<- drug(datALSP$FJDR5750, datALSP$FJDR5800)
table(datALSP$opio_17.5)

#FJDR5950	DR2028: YP has tried other substance in the last year [F17]
table(datALSP$FJDR5950)
table(datALSP$FJDR5900, datALSP$FJDR5950, exclude = FALSE)
datALSP$subs_17.5<- drug(datALSP$FJDR5900, datALSP$FJDR5950)
table(datALSP$subs_17.5)

##tot score 
datALSP$drug_tot17.5<- datALSP$coke_17.5 + datALSP$amph_17.5 + datALSP$inha_17.5 + datALSP$seda_17.5 + datALSP$hall_17.5 + datALSP$opio_17.5 + datALSP$subs_17.5

table(datALSP$drug_tot17.5)
hist(datALSP$drug_tot17.5)

### age 22 ###
#Notes: DV score available 

#YPB4478 DV:  Count of types illicit substances YP has used in the last year
table(datALSP$YPB4478)
hist(datALSP$YPB4478)

datALSP$drug_tot22<- datALSP$YPB4478
table(datALSP$drug_tot17.5)
table(datALSP$drug_tot20)
save(datALSP, file = "datALSP.R")
load("datALSP.R")

### standardise variables 
#FT
datALSP$FT_total17.5_s<- scale(datALSP$FT_total17.5, center = TRUE, scale = TRUE)
hist(datALSP$FT_total17.5_s)
hist(datALSP$FT_total17.5)

datALSP$FT_total20_s<- scale(datALSP$FT_total20, center = TRUE, scale = TRUE)
hist(datALSP$FT_total20_s)
hist(datALSP$FT_total20)

datALSP$FT_total22_s<- scale(datALSP$FT_total22, center = TRUE, scale = TRUE)
hist(datALSP$FT_total22_s)
hist(datALSP$FT_total22)

#audit
datALSP$audit_tot17.5_s<- scale(datALSP$audit_tot17.5, center = TRUE, scale = TRUE)
hist(datALSP$audit_tot17.5_s)
hist(datALSP$audit_tot17.5)
table(datALSP$audit_tot17.5, exclude = FALSE)

datALSP$audit_tot20_s<- scale(datALSP$audit_tot20, center = TRUE, scale = TRUE)
hist(datALSP$audit_tot20_s)
hist(datALSP$audit_tot20)
table(datALSP$audit_tot20, exclude = FALSE)

datALSP$audit_tot22_s<- scale(datALSP$audit_tot22, center = TRUE, scale = TRUE)
hist(datALSP$audit_tot22_s)
hist(datALSP$audit_tot22)
table(datALSP$audit_tot22, exclude = FALSE)

#cast
datALSP$cast_tot17.5_s<- scale(datALSP$cast_tot17.5, center = TRUE, scale = TRUE)
hist(datALSP$cast_tot17.5_s)
hist(datALSP$cast_tot17.5)

datALSP$cast_tot20_s<- scale(datALSP$cast_tot20, center = TRUE, scale = TRUE)
hist(datALSP$cast_tot20_s)
hist(datALSP$cast_tot20)

datALSP$cast_tot22_s<- scale(datALSP$cast_tot22, center = TRUE, scale = TRUE)
hist(datALSP$cast_tot22_s)
hist(datALSP$cast_tot22)

#other drugs 
datALSP$drug_tot17.5_s<- scale(datALSP$drug_tot17.5, center = TRUE, scale = TRUE)
hist(datALSP$drug_tot17.5_s)
hist(datALSP$drug_tot17.5)
table(datALSP$drug_tot17.5, exclude = FALSE)

datALSP$drug_tot20_s<- scale(datALSP$drug_tot20, center = TRUE, scale = TRUE)
hist(datALSP$drug_tot20_s)
hist(datALSP$drug_tot20)
table(datALSP$drug_tot20, exclude = FALSE)

datALSP$drug_tot22_s<- scale(datALSP$drug_tot22, center = TRUE, scale = TRUE)
hist(datALSP$drug_tot22_s)
hist(datALSP$drug_tot22)
table(datALSP$drug_tot22, exclude = FALSE)

load("datALSP.R")
save(datALSP, file = "datALSP.R")

#### subset sample with at least one observation available 
alsp<-subset(datALSP, !is.na(FT_total17.5) | !is.na(FT_total20) | !is.na(FT_total22) | !is.na(audit_tot17.5)| !is.na(audit_tot20) | !is.na(audit_tot22)| !is.na(cast_tot17.5)| !is.na(cast_tot20)| !is.na(cast_tot22)| !is.na(drug_tot17.5)| !is.na(drug_tot20) | !is.na(drug_tot22))

save(alsp, file = "alsp")

###integrate SNPs data ####

datALSP$IID = paste0(datALSP$cidB2411,datALSP$qlet) ## create matching ID (matches IDs in .bed/.bim/.fam file) 

#rs1558902
rs1558902 = read.table("rs1558902_file_snps.raw", header=TRUE) ## file with allele frequencies per subject 
datALSP2= merge(datALSP, rs1558902, by = "IID", all.x = TRUE) 
head(datALSP2$rs1558902_A)

alsp2<-subset(datALSP2, !is.na(FT_total17.5) | !is.na(FT_total20) | !is.na(FT_total22) | !is.na(audit_tot17.5)| !is.na(audit_tot20) | !is.na(audit_tot22)| !is.na(cast_tot17.5)| !is.na(cast_tot20)| !is.na(cast_tot22)| !is.na(drug_tot17.5)| !is.na(drug_tot20) | !is.na(drug_tot22))

#rs671_file_snps
rs671 = read.table("rs671_file_snps.raw", header=TRUE) ## file with allele frequencies per subject 
alsp2= merge(alsp2, rs671, by = "IID", all.x = TRUE) 
head(alsp2$rs671_A)

#rs1229984_file_snps
rs1229984 = read.table("rs1229984_file_snps.raw", header=TRUE) ## file with allele frequencies per subject 
alsp2= merge(alsp2, rs1229984, by = "IID", all.x = TRUE) 
head(alsp2$rs1229984_T)

#rs16969968_file_snps
rs16969968 = read.table("rs16969968_file_snps.raw", header=TRUE) ## file with allele frequencies per subject 
alsp2= merge(alsp2, rs16969968, by = "IID", all.x = TRUE) 
head(alsp2$rs16969968)

save(alsp2, file = "alsp2")

load("alsp2")
head(alsp2)

#sex 1 = male; 2 = female 
table(alsp2$kz021)
alsp2$sex<- alsp2$kz021

load("alsp2")

#### descriptive stats ####
#ft
summary(alsp2$FT_total17.5)
sd(alsp2$FT_total17.5, na.rm = TRUE)
2203/6399
0.3442725*100

summary(alsp2$FT_total20)
sd(alsp2$FT_total20, na.rm = TRUE)

summary(alsp2$FT_total22)
sd(alsp2$FT_total22, na.rm = TRUE)

#audit 
summary(alsp2$audit_tot17.5)
sd(alsp2$audit_tot17.5, na.rm = TRUE)

summary(alsp2$audit_tot20)
sd(alsp2$audit_tot20, na.rm = TRUE)

summary(alsp2$audit_tot22)
sd(alsp2$audit_tot22, na.rm = TRUE)

#cast 
summary(alsp2$cast_tot17.5)
sd(alsp2$cast_tot17.5, na.rm = TRUE)

summary(alsp2$cast_tot20)
sd(alsp2$cast_tot20, na.rm = TRUE)

summary(alsp2$cast_tot22)
sd(alsp2$cast_tot22, na.rm = TRUE)

#other drugs 
summary(alsp2$drug_tot17.5)
sd(alsp2$drug_tot17.5, na.rm = TRUE)

summary(alsp2$drug_tot20)
sd(alsp2$drug_tot20, na.rm = TRUE)

summary(alsp2$drug_tot22)
sd(alsp2$drug_tot22, na.rm = TRUE)

load("alsp2")

#bmi at ahe 17.5

hist(alsp2$FJMR022a)
summary(alsp2$FJMR022a)

a<- lm(FJMR022a ~ rs1558902_A, data = alsp2) ## higher bmi associated with higher SNP values 
summary(a)

# someone told ED - yes/no
table(alsp2$cct4153, exclude = FALSE)
alsp2$ED<- 0
alsp2$ED [alsp2$cct4153 == 1|alsp2$cct4152 == 1 |alsp2$cct4151 == 1 |alsp2$cct4150 == 1]<- 1
table(alsp2$ED)

a<- glm(ED ~ rs1558902_A, family = "binomial", data = alsp2)
summary(a)

# genetic variants as factors 
alsp2$SNP2<- alsp2$rs1229984_T
alsp2$SNP2<- as.character(alsp2$SNP2)

alsp2$SNP3<- alsp2$rs16969968_A
alsp2$SNP3<- as.character(alsp2$SNP3)

alsp2$SNP4<- alsp2$rs1558902_A
alsp2$SNP4<- as.character(alsp2$SNP4)

load("alsp2")

# audit item 1 at age 22 
table(alsp2$YPB4150)
alsp2$audit1_22<-audit(alsp2$YPB4150)
table(alsp2$audit1_22)
# audit item 1 age 17.5
table(alsp2$FJAL1000)
alsp2$audit1_17.5<- audit(alsp2$FJAL1000)
table(alsp2$audit1_17.5)

#assign 0 to those who have never tried alcohol 
#audit17.5
alsp2$audit1_17.5[alsp2$FJAL050 == "2"]<- 0
table(alsp2$audit1_17.5)

#audit20
alsp2$audit1_20[alsp2$CCU3050 == "2"]<- 0
table(alsp2$audit1_20)

#audit22
alsp2$audit1_22[alsp2$YPB4130 == "2"]<- 0
table(alsp2$audit1_22)

save(alsp2, file = "alsp2")
load("alsp2")

# more descriptives of the data 
round(prop.table(table(alsp2$cig17.5, exclude = TRUE)),4)
round(prop.table(table(alsp2$cig20, exclude = TRUE)),4)
round(prop.table(table(alsp2$cig22, exclude = TRUE)),4)

round(prop.table(table(alsp2$audit1_17.5, exclude = TRUE)),4)
round(prop.table(table(alsp2$audit1_20, exclude = TRUE)),4)
round(prop.table(table(alsp2$audit1_22, exclude = TRUE)),4)

round(prop.table(table(alsp2$can_fre17.5, exclude = TRUE)),4)
round(prop.table(table(alsp2$can_fre20, exclude = TRUE)),4)
round(prop.table(table(alsp2$can_fre22, exclude = TRUE)),4)


save(alsp2, file = "alsp2")
load("alsp2")

############################ Hardy-Weinberg equation ####################################################
#mathematical equation that can be used to calculate the genetic variation of a population at equilibrium.

#### BMI SNP: rs1558902_A
#(A;A)            Higher BMI than normal
#(A;T)            Higher BMI than normal
#(T;T)            Normal 
# Minor Allele =>       A 


# dbSNP frequencies
# RefSNP Alleles:       A/T
# 1000 Genomes: A=0.2280/1142
# TOPMED:       A=0.2711/7895


hwe = function(q) {
  q=q
  p=1-q
  exp=c(p^2, 2*p*q, q^2)
  return(exp)
}

round(hwe(0.2280), 3) 
round(hwe(0.2711), 3)
round(prop.table(table(alsp2$rs1558902_A)), 3)
table(alsp2$rs1558902_A, exclude = FALSE)

### Smoking SNP:  rs16969968_A
#minor Allele => A

#dbSNP frequencies
#RefSNP Alleles:      A/G
#A=0.1496/749 (1000 Genomes)
#A=0.2517/3267 (GO-ESP)
#A=0.2257/28343 (TOPMED)

round(hwe(0.1496), 3) 
round(hwe(0.2517), 3)
round(prop.table(table(alsp2$rs16969968_A)), 3)
table(alsp2$rs16969968_A, exclude = FALSE)

###### alcohol SNP : rs1229984_T
#minor allele => T

#dbSNP frequencies
#RefSNP Alleles:	A/C/G
#T=0.1585/794 (1000 Genomes)
#T=0.0365/475 (GO-ESP)
#T=0.0760/9545 (TOPMED)

round(hwe(0.1585), 3) 
round(hwe(0.0760), 3)
round(prop.table(table(alsp2$rs1229984_T)), 3)
table(alsp2$rs1229984_T, exclude = FALSE)

##### alcohol SNP: rs671_A
#minor allele => A

#dbSNP frequencies
#RefSNP Alleles:A/G
#A=0.0357/179 (1000 Genomes)
#A=0.0156/1960 (TOPMED)

round(hwe(0.0357), 3) 
round(hwe(0.0156), 3)
round(prop.table(table(alsp2$rs671_A)), 3)
table(alsp2$rs671_A, exclude = FALSE)

load("alsp2")

### recode frequecny of use data for cannabis to have a variable with 6 categories 
table(alsp2$can_fre17.5)
table(alsp2$can_fre20)
table(alsp2$can_fre22)

#3 and 4 = monthly or less
alsp2$can_fre17.5[alsp2$can_fre17.5 %in% c(3,4)]<- 3
alsp2$can_fre17.5[alsp2$can_fre17.5 == 5]<- 4
alsp2$can_fre17.5[alsp2$can_fre17.5 == 6]<- 5
table(alsp2$can_fre17.5)

alsp2$can_fre20[alsp2$can_fre20 %in% c(3,4)]<- 3
alsp2$can_fre20[alsp2$can_fre20 == 5]<- 4
alsp2$can_fre20[alsp2$can_fre20 == 6]<- 5
table(alsp2$can_fre20)

alsp2$can_fre22[alsp2$can_fre22 %in% c(3,4)]<- 3
alsp2$can_fre22[alsp2$can_fre22 == 5]<- 4
alsp2$can_fre22[alsp2$can_fre22 == 6]<- 5
table(alsp2$can_fre22)

round(prop.table(table(alsp2$can_fre17.5)),4)
sum(is.na(alsp2$can_fre17.5))

round(prop.table(table(alsp2$can_fre20)),4)
sum(is.na(alsp2$can_fre20))

round(prop.table(table(alsp2$can_fre22)),4)
sum(is.na(alsp2$can_fre22))

##ask!! JB if you should combine last two groups 

### combine drug items across 3 time points ###

#function 
mymean<- function (a,b, c){
  a1<-a
  b1<-b
  c1<-c
  a1[is.na(a1)]<- 0
  b1[is.na(b1)]<- 0
  c1[is.na(c1)]<- 0
  d<- (a1 + b1 +  c1)/3
  d[is.na(a) & is.na(b) & is.na(c)]<-NA
  return(d)
}

mymean2<- function (a,b, c){
  a1<-a
  b1<-b
  c1<-c
  a1<- na.mean(a1)
  b1<- na.mean(b1)
  c1<-na.mean(c1)
  d<- (a1 + b1 +  c1)/3
  d[is.na(a) & is.na(b) & is.na(c)]<- NA
  return(d)
}
alsp2$cig17.5<- as.numeric(alsp2$cig17.5)
alsp2$cig20<- as.numeric(alsp2$cig20)
alsp2$cig22<- as.numeric(alsp2$cig22)
table(alsp2$cig17.5)
table(alsp2$cig20)
table(alsp2$cig22)

alsp2$cig_mean<-mymean(alsp2$cig17.5,alsp2$cig20,alsp2$cig22 )
summary(alsp2$cig_mean)
hist(alsp2$cig_mean)

alsp2$cig_mean2<-mymean2(alsp2$cig17.5,alsp2$cig20,alsp2$cig22 )
summary(alsp2$cig_mean2)
hist(alsp2$cig_mean2)
sd(alsp2$cig_mean2, na.rm = TRUE)

alsp2$cig_mean<- NULL
alsp2$cig_mean2<- NULL

#alcohol fre 
str(alsp2$audit1_17.5)
str(alsp2$audit1_20)
str(alsp2$audit1_22)

alsp2$alc_mean<- mymean(alsp2$audit1_17.5, alsp2$audit1_20, alsp2$audit1_22)
summary(alsp2$alc_mean)
hist(alsp2$alc_mean)

alsp2$alc_mean2<- mymean2(alsp2$audit1_17.5, alsp2$audit1_20, alsp2$audit1_22)
summary(alsp2$alc_mean2)
hist(alsp2$alc_mean2)
sd(alsp2$alc_mean2, na.rm = TRUE)

alsp2$alc_mean<- NULL
alsp2$alc_mean2<- NULL

#cannabis fre 
str(alsp2$can_fre17.5)
str(alsp2$can_fre20)
str(alsp2$can_fre22)

alsp2$can_mean<- mymean(alsp2$can_fre17.5, alsp2$can_fre20, alsp2$can_fre22)
summary(alsp2$can_mean)
hist(alsp2$can_mean)

alsp2$can_mean2<- mymean2(alsp2$can_fre17.5, alsp2$can_fre20, alsp2$can_fre22)
summary(alsp2$can_mean2)
hist(alsp2$can_mean2)
sd(alsp2$can_mean2, na.rm = TRUE)

alsp2$can_mean<- NULL
alsp2$can_mean2<- NULL
#drug fre 
str(alsp2$drug_tot17.5)
str(alsp2$drug_tot20)
str(alsp2$drug_tot22)

alsp2$drug_mean<-mymean(alsp2$drug_tot17.5, alsp2$drug_tot20, alsp2$drug_tot22)
summary(alsp2$drug_mean)
hist(alsp2$drug_mean)

alsp2$drug_mean2<-mymean2(alsp2$drug_tot17.5, alsp2$drug_tot20, alsp2$drug_tot22)
summary(alsp2$drug_mean2)
hist(alsp2$drug_mean2)
sd(alsp2$drug_mean2, na.rm = TRUE)

alsp2$drug_mean<- NULL
alsp2$drug_mean2<- NULL

#FT_mean 
str(alsp2$FT_total17.5)
str(alsp2$FT_total20)
str(alsp2$FT_total22)

alsp2$FT_mean<- mymean (alsp2$FT_total17.5, alsp2$FT_total20, alsp2$FT_total22)
summary(alsp2$FT_mean)
hist(alsp2$FT_mean)

alsp2$FT_mean2<- mymean2 (alsp2$FT_total17.5, alsp2$FT_total20, alsp2$FT_total22)
summary(alsp2$FT_mean2)
hist(alsp2$FT_mean2)
sd(alsp2$FT_mean2, na.rm = TRUE)

alsp2$FT_mean2<- NULL
alsp2$FT_mean<- NULL

# audit mean 
str(alsp2$audit_tot17.5)
str(alsp2$audit_tot20)
str(alsp2$audit_tot22)

alsp2$audit_mean<- mymean (alsp2$audit_tot17.5, alsp2$audit_tot20, alsp2$audit_tot22)
summary(alsp2$audit_mean)
hist(alsp2$audit_mean)

alsp2$audit_mean2<- mymean2 (alsp2$audit_tot17.5, alsp2$audit_tot20, alsp2$audit_tot22)
summary(alsp2$audit_mean2)
hist(alsp2$audit_mean2)
sd(alsp2$audit_mean2, na.rm = TRUE)

alsp2$audit_mean2<- NULL
alsp2$audit_mean<- NULL

#cast mean 
str(alsp2$cast_tot17.5)
str(alsp2$cast_tot20)
str(alsp2$cast_tot22)

alsp2$cast_mean<- mymean(alsp2$cast_tot17.5, alsp2$cast_tot20, alsp2$cast_tot22)
summary(alsp2$cast_mean)
hist(alsp2$cast_mean)

alsp2$cast_mean2<- mymean2(alsp2$cast_tot17.5, alsp2$cast_tot20, alsp2$cast_tot22)
summary(alsp2$cast_mean2)
hist(alsp2$cast_mean2)
sd(alsp2$cast_mean2, na.rm = TRUE)

alsp2$cast_mean<-NULL
alsp2$cast_mean2<- NULL
### load 
load("alsp2")
## new means 
#example 
data=data.frame(vec1,vec2,vec3)
mean(data,na.rm=T)
data$mean=apply(data,1,mean,na.rm=T)

# compute means and merge with original dataset 
smoking<- select (alsp2, cig17.5, cig20, cig22)
smoking$mean_sm<- apply(smoking, 1, mean, na.rm = T)
alsp2$mean_sm<- smoking$mean_sm

alcohol<- select (alsp2, audit1_17.5, audit1_20, audit1_22)
alcohol$mean_alc<- apply(alcohol, 1, mean, na.rm = T)
alsp2$mean_alc=alcohol$mean_alc

cannabis<- select(alsp2, can_fre17.5, can_fre20, can_fre22)
cannabis$mean_can<- apply(cannabis, 1, mean, na.rm = T)
alsp2$mean_can=cannabis$mean_can

drug<- select(alsp2, drug_tot17.5, drug_tot20, drug_tot22)
drug$mean_drug<- apply(drug, 1, mean, na.rm = T)
alsp2$mean_drug=drug$mean_drug

FT<- select(alsp2, FT_total17.5, FT_total20, FT_total22)
FT$mean_FT<- apply(FT, 1, mean, na.rm = T)
alsp2$mean_FT<- FT$mean_FT

audit<- select(alsp2, audit_tot17.5, audit_tot20, audit_tot22)
audit$mean_audit<- apply(audit, 1, mean, na.rm = T)
alsp2$mean_audit<- audit$mean_audit

cast<- select(alsp2, cast_tot17.5, cast_tot20, cast_tot22)
cast$mean_cast<- apply(cast, 1, mean, na.rm = T)
alsp2$mean_cast<- cast$mean_cast

summary(alsp2$mean_sm)
summary(alsp2$mean_alc)
summary(alsp2$mean_can)
summary(alsp2$mean_drug)
summary(alsp2$mean_FT)
summary(alsp2$mean_audit)
summary(alsp2$mean_cast)

sd(alsp2$mean_sm, na.rm = T)
sd(alsp2$mean_alc, na.rm = T)
sd(alsp2$mean_can, na.rm = T)
sd(alsp2$mean_drug, na.rm = T)
sd(alsp2$mean_FT, na.rm = T)
sd(alsp2$mean_audit, na.rm = T)
sd(alsp2$mean_cast, na.rm = T)

save(alsp2, file = "alsp2")
load("alsp2")

##### new session: 16/04 ####

## BMI analysis ##

#BMI at age 17.5
hist(alsp2$FJMR022a)
summary(alsp2$FJMR022a)
table(alsp2$FJMR022a)
sd(alsp2$FJMR022a, na.rm = TRUE)

alsp2$BMI17.5<- alsp2$FJMR022a # new BMI variable 

alsp2$BMI_cut50<- alsp2$BMI17.5 # cut bmi due to 3 extreme values higher than 50 
alsp2$BMI_cut50[alsp2$BMI_cut50 > 50]<- NA
summary(alsp2$BMI_cut50)

#mean of BMI in each SNP category for BMI
a<-alsp2 %>%
  group_by(SNP4) %>%
  summarise(m = mean(BMI17.5, na.rm = TRUE))

#mean of BMI in each SNP category for smoking
b<-alsp2 %>%
  group_by(SNP3) %>%
  summarise(m = mean(BMI17.5, na.rm = TRUE))

#mean of BMI in each SNP category for alcohol 
c<-alsp2 %>%
  group_by(SNP2) %>%
  summarise(m = mean(BMI17.5, na.rm = TRUE))

d<- bind_rows(a,b,c)
d<- format.data.frame(d, digits = 4)
write.csv(d, file = "bmimeans.csv")

#regression 
a<- lm(BMI17.5 ~ SNP4, data = alsp2) ## higher bmi associated with higher genetic risk  
summary(a)
a<-expl(a)

a1<- lm(BMI_cut50 ~ SNP4, data = alsp2) ## no difference   
summary(a1)

b1<- lm(BMI_cut50 ~ SNP3 , data = alsp2) ## no difference 
summary(b1)


b<- lm(BMI17.5 ~ SNP3 , data = alsp2) 
summary(b)
b<-expl(b)

c<- lm(BMI17.5 ~ SNP2 , data = alsp2) 
summary(c)
c<- expl(c)

d<-bind_rows(a,b,c)
d<-format(d, digits = 2)

write.csv(d, file = "bmireg.csv")

## number of cigarettes smoked every day on average 

#these scores only include those who smoke at least weekly. 
#age 17.5
table(alsp2$FJSM400, exclude = FALSE)
hist(alsp2$FJSM400)
#age 20
table(alsp2$CCU3013, exclude = FALSE)
hist(alsp2$CCU3013)
#age 22
table(alsp2$YPB4031, exclude = FALSE)
hist(alsp2$YPB4031)

mean1<- select (alsp2,FJSM400, CCU3013, YPB4031)
mean1$mean_cign1<- apply(mean1, 1, mean, na.rm = T)
mean1<- format(mean1, digits = 0)
alsp2$mean_cign1<- mean1$mean_cign1
table(alsp2$mean_cign1)
alsp2$mean_cign1<- as.numeric (alsp2$mean_cign1)
hist(alsp2$mean_cign1)

b<- table(alsp2$mean_cign1, exclude = F)
b<- data.frame(b)
## assign 0 to those who smoke less than weekly and did not therefore complete FT items 
#17.5
alsp2$ncig17.5<- alsp2$FJSM400
alsp2$ncig17.5[alsp2$cig17.5 %in% c(0,1,2, 3)]<- 0
table(alsp2$ncig17.5)

#20
alsp2$ncig20<- alsp2$CCU3013
alsp2$ncig20[alsp2$cig20 %in% c(0,1,2, 3)]<- 0
table(alsp2$ncig20)

#22
alsp2$ncig22<- alsp2$YPB4031
alsp2$ncig22[alsp2$cig22 %in% c(0,1,2, 3)]<- 0
table(alsp2$ncig22)

#mean 
mean<- select(alsp2, ncig17.5, ncig20, ncig22)
mean$mean_cign<- apply(mean, 1, mean, na.rm = T)

mean<- format(mean, digits = 0)
table(mean$mean_cign)

alsp2$mean_cign<- mean$mean_cign


alsp2$mean_cign<- as.numeric(alsp2$mean_cign)
a<- table(alsp2$mean_cign,exclude = F)
a<- data.frame(a)

##assign 0 to those who smoke less than weekly but have tried to smoke
#17.5
alsp2$ncig17.5a<- alsp2$FJSM400
alsp2$ncig17.5a[alsp2$cig17.5 %in% c(1,2, 3)]<- 0
table(alsp2$ncig17.5a)

#20
alsp2$ncig20a<- alsp2$CCU3013
alsp2$ncig20a[alsp2$cig20 %in% c(1,2, 3)]<- 0
table(alsp2$ncig20a)

#22
alsp2$ncig22a<- alsp2$YPB4031
alsp2$ncig22a[alsp2$cig22 %in% c(1,2, 3)]<- 0
table(alsp2$ncig22a)

#mean
mean2<- select (alsp2, ncig17.5a, ncig20a, ncig22a)
mean2$mean_cign2<- apply(mean2, 1, mean, na.rm = T)

mean2<- format(mean2, digits = 0)
table(mean2$mean_cign2)

alsp2$mean_cign2<- mean2$mean_cign2

table(alsp2$mean_cign2)
alsp2$mean_cign2<- as.numeric(alsp2$mean_cign2)
hist(alsp2$mean_cign2)

c<- table(alsp2$mean_cign2, exclude = F)
c<- data.frame(c)

save(alsp2, file = "alsp2")

d<- bind_cols(a,b, c)
write.csv(d, file = "c.csv")

### create new dataset without ps who have never tried to smoke 
#do analyses looking at associations of smoking fre, FT, and number of cigarettes with smoking and BMI SNPs both linear and categorical 

smoker<- select(alsp2, cig17.5, cig20, cig22, mean_sm, mean_FT,rs1229984_T, rs16969968_A, rs1558902_A, SNP2, SNP3, SNP4, mean_cign2, mean_cign)
smoker<- filter(smoker, cig17.5> 0 | cig20 > 0 | cig22 > 0) # new sample has 4055

#smoking snp
a<-lm(rs16969968_A ~ mean_sm, data = smoker)
summary(a)
a<- expl(a)

b<-lm(rs16969968_A ~ mean_FT, data = smoker)
summary(b)
b<- expl(b)

c<-lm(rs16969968_A ~ mean_cign, data = smoker)
summary(c)
c<- expl(c)

#bmi snp 
d<-lm(rs1558902_A ~ mean_sm, data = smoker)
summary(d)
d<- expl(d)

e<-lm(rs1558902_A ~ mean_FT, data = smoker)
summary(e)
e<- expl(e)

f<-lm(rs1558902_A ~ mean_cign, data = smoker)
summary(f)
f<- expl(f)

g<- bind_cols(a, b,c,d,e,f)
g<- format(g, digits = 2)
write.csv(g, file = "g.csv")

## means 
#smoking SNP
a<-smoker %>%
  group_by(SNP3) %>%
  summarise(m = mean(mean_sm, na.rm = TRUE))

b<-smoker %>%
  group_by(SNP3) %>%
  summarise(m = mean(mean_FT, na.rm = TRUE))

c<-smoker %>%
  group_by(SNP3) %>%
  summarise(m = mean(mean_cign, na.rm = TRUE))

#bmi SNP 
d<-smoker %>%
  group_by(SNP4) %>%
  summarise(m = mean(mean_sm, na.rm = TRUE))

e<-smoker %>%
  group_by(SNP4) %>%
  summarise(m = mean(mean_FT, na.rm = TRUE))

f<-smoker %>%
  group_by(SNP4) %>%
  summarise(m = mean(mean_cign, na.rm = TRUE))

g<- bind_cols(a,b,c,d,e,f)
g<- format.data.frame(g, digits = 3)
write.csv(g, file = "g.csv")

## round mean scores in full sample to do hurdle models 
round<- select(alsp2, cig_mean, alc_mean, can_mean, drug_mean, cast_mean, FT_mean, audit_mean)
round<- format(round, digits = 0)

alsp2$round_cig_mean<- round$cig_mean
table(alsp2$round_cig_mean)
alsp2$round_cig_mean<-as.numeric(alsp2$round_cig_mean)

alsp2$round_alc_mean<- round$alc_mean
alsp2$round_alc_mean<- as.numeric(alsp2$round_alc_mean)

alsp2$round_can_mean<- round$can_mean
alsp2$round_can_mean<- as.numeric(alsp2$round_can_mean)

alsp2$round_drug_mean<- round$drug_mean
alsp2$round_drug_mean<- as.numeric(alsp2$round_drug_mean)

alsp2$round_cast_mean<- round$cast_mean
alsp2$round_cast_mean<- as.numeric(alsp2$round_cast_mean)

alsp2$round_FT_mean<- round$FT_mean
alsp2$round_FT_mean<- as.numeric(alsp2$round_FT_mean)

alsp2$round_audit_mean<- round$audit_mean
alsp2$round_audit_mean<- as.numeric(alsp2$round_audit_mean)

#regressions hurdle with continuous SNPs 

#cig 
obj<- hurdle(round_cig_mean ~ rs1229984_T, dist="negbin", data = alsp2)
summary(obj)
obj1<- exph(obj)

obj<- hurdle(round_cig_mean ~ rs16969968_A, dist="negbin", data = alsp2)
summary(obj)
obj2<- exph(obj)

obj<- hurdle(round_cig_mean ~ rs1558902_A, dist="negbin", data = alsp2)
summary(obj)
obj3<- exph(obj)

#alc
obj<- hurdle(round_alc_mean ~ rs1229984_T, dist="negbin", data = alsp2)
summary(obj)
obj4<- exph(obj)

obj<- hurdle(round_alc_mean ~ rs16969968_A, dist="negbin", data = alsp2)
summary(obj)
obj5<- exph(obj)

obj<- hurdle(round_alc_mean ~ rs1558902_A, dist="negbin", data = alsp2)
summary(obj)
obj6<- exph(obj)

#can
obj<- hurdle(round_can_mean ~ rs1229984_T, dist="negbin", data = alsp2)
summary(obj)
obj7<- exph(obj)

obj<- hurdle(round_can_mean ~ rs16969968_A, dist="negbin", data = alsp2)
summary(obj)
obj8<- exph(obj)

obj<- hurdle(round_can_mean ~ rs1558902_A, dist="negbin", data = alsp2)
summary(obj)
obj9<- exph(obj)

#drug
obj<- hurdle(round_drug_mean ~ rs1229984_T, dist="negbin", data = alsp2)
summary(obj)
obj10<- exph(obj)

obj<- hurdle(round_drug_mean ~ rs16969968_A, dist="negbin", data = alsp2)
summary(obj)
obj11<- exph(obj)

obj<- hurdle(round_drug_mean ~ rs1558902_A, dist="negbin", data = alsp2)
summary(obj)
obj12<- exph(obj)

#FT
obj<- hurdle(round_FT_mean ~ rs1229984_T, dist="negbin", data = alsp2)
summary(obj)
obj13<- exph(obj)

obj<- hurdle(round_FT_mean ~ rs16969968_A, dist="negbin", data = alsp2)
summary(obj)
obj14<- exph(obj)

obj<- hurdle(round_FT_mean ~ rs1558902_A, dist="negbin", data = alsp2)
summary(obj)
obj15<- exph(obj)

#audit
obj<- hurdle(round_audit_mean ~ rs1229984_T, dist="negbin", data = alsp2)
summary(obj)
obj16<- exph(obj)

obj<- hurdle(round_audit_mean ~ rs16969968_A, dist="negbin", data = alsp2)
summary(obj)
obj17<- exph(obj)

obj<- hurdle(round_audit_mean ~ rs1558902_A, dist="negbin", data = alsp2)
summary(obj)
obj18<- exph(obj)

#cast
obj<- hurdle(round_cast_mean ~ rs1229984_T, dist="negbin", data = alsp2)
summary(obj)
obj19<- exph(obj)

obj<- hurdle(round_cast_mean ~ rs16969968_A, dist="negbin", data = alsp2)
summary(obj)
obj20<- exph(obj)

obj<- hurdle(round_cast_mean ~ rs1558902_A, dist="negbin", data = alsp2)
summary(obj)
obj21<- exph(obj)

exp1<-bind_rows(obj1, obj2, obj3) 
exp2<- bind_rows(obj4, obj5, obj6)
exp3<- bind_rows(obj7, obj8, obj9)
exp4<- bind_rows(obj10, obj11, obj12)
exp5<- bind_rows(obj13, obj14, obj15)
exp6<- bind_rows(obj16, obj17, obj18)
exp7<- bind_rows(obj19, obj20, obj21)

exp<- bind_cols(exp1, exp2,exp3, exp4, exp5, exp6, exp7)
write.csv(exp, file = "exp.csv")


### hurdle with categorical SNPs 

#cig 
obj<- hurdle(round_cig_mean ~ SNP2, dist="negbin", data = alsp2)
summary(obj)
obj1<- exph(obj)

obj<- hurdle(round_cig_mean ~ SNP3, dist="negbin", data = alsp2)
summary(obj)
obj2<- exph(obj)

obj<- hurdle(round_cig_mean ~ SNP4, dist="negbin", data = alsp2)
summary(obj)
obj3<- exph(obj)

#alc
obj<- hurdle(round_alc_mean ~ SNP2, dist="negbin", data = alsp2)
summary(obj)
obj4<- exph(obj)

obj<- hurdle(round_alc_mean ~ SNP3, dist="negbin", data = alsp2)
summary(obj)
obj5<- exph(obj)

obj<- hurdle(round_alc_mean ~ SNP4, dist="negbin", data = alsp2)
summary(obj)
obj6<- exph(obj)

#can
obj<- hurdle(round_can_mean ~ SNP2, dist="negbin", data = alsp2)
summary(obj)
obj7<- exph(obj)

obj<- hurdle(round_can_mean ~ SNP3, dist="negbin", data = alsp2)
summary(obj)
obj8<- exph(obj)

obj<- hurdle(round_can_mean ~ SNP4, dist="negbin", data = alsp2)
summary(obj)
obj9<- exph(obj)

#drug
obj<- hurdle(round_drug_mean ~ SNP2, dist="negbin", data = alsp2)
summary(obj)
obj10<- exph(obj)

obj<- hurdle(round_drug_mean ~ SNP3, dist="negbin", data = alsp2)
summary(obj)
obj11<- exph(obj)

obj<- hurdle(round_drug_mean ~ SNP4, dist="negbin", data = alsp2)
summary(obj)
obj12<- exph(obj)

#FT
obj<- hurdle(round_FT_mean ~ SNP2, dist="negbin", data = alsp2)
summary(obj)
obj13<- exph(obj)

obj<- hurdle(round_FT_mean ~ SNP3, dist="negbin", data = alsp2)
summary(obj)
obj14<- exph(obj)

obj<- hurdle(round_FT_mean ~ SNP4, dist="negbin", data = alsp2)
summary(obj)
obj15<- exph(obj)

#audit
obj<- hurdle(round_audit_mean ~SNP2, dist="negbin", data = alsp2)
summary(obj)
obj16<- exph(obj)

obj<- hurdle(round_audit_mean ~ SNP3, dist="negbin", data = alsp2)
summary(obj)
obj17<- exph(obj)

obj<- hurdle(round_audit_mean ~ SNP4, dist="negbin", data = alsp2)
summary(obj)
obj18<- exph(obj)

#cast
obj<- hurdle(round_cast_mean ~ SNP2, dist="negbin", data = alsp2)
summary(obj)
obj19<- exph(obj)

obj<- hurdle(round_cast_mean ~ SNP3, dist="negbin", data = alsp2)
summary(obj)
obj20<- exph(obj)

obj<- hurdle(round_cast_mean ~ SNP4, dist="negbin", data = alsp2)
summary(obj)
obj21<- exph(obj)


exp1<-bind_rows(obj1, obj2, obj3) 
exp2<- bind_rows(obj4, obj5, obj6)
exp3<- bind_rows(obj7, obj8, obj9)
exp4<- bind_rows(obj10, obj11, obj12)
exp5<- bind_rows(obj13, obj14, obj15)
exp6<- bind_rows(obj16, obj17, obj18)
exp7<- bind_rows(obj19, obj20, obj21)

exp<- bind_cols(exp1, exp2,exp3, exp4, exp5, exp6, exp7)
write.csv(exp, file = "exph.csv")

save(alsp2, file = "alsp2")
load("alsp2")

### DAWBA DSM-IV clinical diagnosis 
#kr827a: Any anxiety disorder
#kr832a: Any depressive disorder
#kr803a: Any ADHD disorder
#kr813a: Any oppositional-conduct disorder

table(alsp2$kr827a, exclude = F)
table(alsp2$kr832a, exclude = F)
table(alsp2$kr803a, exclude = F)
table(alsp2$kr813a, exclude = F)

alsp2$anxiety<- alsp2$kr827a
alsp2$depression<- alsp2$kr832a
alsp2$ADHD<- alsp2$kr803a
alsp2$conduct<- alsp2$kr813a

alsp2$anxiety<- as.numeric(alsp2$anxiety)
table(alsp2$anxiety)
alsp2$depression<- alsp2$kr832a
table(alsp2$depression)
alsp2$depression<- as.numeric(alsp2$depression)
alsp2$ADHD<- as.numeric(alsp2$ADHD)
alsp2$conduct<- as.numeric(alsp2$conduct)


table(alsp2$anypsy)
alsp2$anypsy[alsp2$conduct == 1 ]<- 2
alsp2$anypsy[alsp2$ADHD == 1 ]<- 2
alsp2$anypsy[alsp2$depression == 1]<- 2
alsp2$anypsy[alsp2$anxiety == 1]<- 2
alsp2$anypsy[is.na (alsp2$conduct)]<- NA
round(prop.table(table(alsp2$anypsy)),2)

save(alsp2, file = "alsp2")
load("alsp2")

alsp2$anxiety [alsp2$kr827a == 1]<-2
alsp2$anxiety [alsp2$kr827a == 2]<-1
round(prop.table(table(alsp2$anxiety, exclude= F)),2)

alsp2$depression[alsp2$kr832a == 1]<- 2
alsp2$depression[alsp2$kr832a == 2]<- 1
round(prop.table(table(alsp2$depression, exclude= F)),3)

alsp2$ADHD[alsp2$kr803a == 1]<- 2
alsp2$ADHD[alsp2$kr803a == 2]<- 1
round(prop.table(table(alsp2$ADHD, exclude= F)),2)

alsp2$conduct[alsp2$kr813a == 1]<- 2
alsp2$conduct[alsp2$kr813a == 2]<- 1
round(prop.table(table(alsp2$conduct, exclude= F)),2)

save(alsp2, file = "alsp2")


### new SNPs #### :)  (19/04)
load("alsp2")

#rs4471463 cannabis 

rs4471463 = read.table("rs4471463_file_snps.raw", header=TRUE) ## file with allele frequencies per subject 
a<- select(rs4471463, rs4471463_C, IID)
alsp2= merge(alsp2, a, by = "IID", all.x = TRUE) 

head(alsp2$rs4471463_C)
table(alsp2$rs4471463_C, exclude = F)

alsp2$SNP5<- alsp2$rs4471463_C
alsp2$SNP5<- as.character(alsp2$SNP5)

# filter
table(alsp2$audit1_22)

#subsample without people who have never tried alcohol 
alc<- alsp2

alc$FT_total17.5_s<- NULL
alc$FT_total20_s<- NULL
alc$FT_total22_s<- NULL
alc$audit_tot17.5_s<- NULL
alc$audit_tot20_s<- NULL
alc$audit_tot22_s<- NULL
alc$cast_tot17.5_s<- NULL
alc$cast_tot20_s<- NULL
alc$cast_tot22_s<- NULL
alc$drug_tot17.5_s<- NULL
alc$drug_tot20_s<- NULL
alc$drug_tot22_s<- NULL

alc<- filter(alc,alsp2$audit1_22 > 0)


##remove decimals from mean_audit and mean_alc
table(alsp2$mean_alc)
table(alsp2$mean_alc_round)

b<- select(alsp2, mean_alc, mean_audit)
b<- format(b, digits = 0)

alsp2$mean_alc_round<- b$mean_alc
alsp2$mean_alc_round<- as.numeric(alsp2$mean_alc_round)
alsp2$mean_audit_round<- b$mean_audit
alsp2$mean_audit_round<-as.numeric(alsp2$mean_audit_round)

#also from all other mean scores 
b<- select(alsp2, mean_can, mean_cast, mean_FT, mean_sm, mean_drug)
b<- format(b, digits = 0)
alsp2$mean_can_round <- b$mean_can
alsp2$mean_cast_round<- b$mean_cast
alsp2$mean_FT_round<- b$mean_FT
alsp2$mean_sm_round<- b$mean_sm
alsp2$mean_drug_round<- b$mean_drug

alsp2$mean_can_round <- as.numeric(alsp2$mean_can_round)
alsp2$mean_cast_round<- as.numeric(alsp2$mean_cast_round)
alsp2$mean_FT_round<- as.numeric(alsp2$mean_FT_round)
alsp2$mean_sm_round<- as.numeric(alsp2$mean_sm_round)
alsp2$mean_drug_round<- as.numeric(alsp2$mean_drug_round)

## merge with ADHD PRS
ADHD<- read.table("ADHD_PRS_plink_format_18April.best", header = T)
ADHD<- select(ADHD, IID, PRS)
alsp2<- merge(alsp2, ADHD, by = "IID", all.x = T)
hist(alsp2$PRS)

alsp2$adhdPRS100<- alsp2$PRS
alsp2$adhdPRS100<- alsp2$adhdPRS100*100
hist(alsp2$adhdPRS100)


## BMI PRS 

bmi<- read.table("BMI_age7_plink_18April.best", header = T)
bmi$bmiPRS<- bmi$PRS
bmi<- select(bmi, bmiPRS, IID)
alsp2<- merge(alsp2, bmi, bu = "IID", all.x = T)
hist(alsp2$bmiPRS)

alsp2$bmiPRS100<- alsp2$bmiPRS*100
hist(alsp2$bmiPRS100)

save(alsp2, file = "alsp2")

### new session: 20/04 ###
load("alsp2")

# new variable for lifetime cannabis use 
table(alsp2$can_fre17.5, exclude = F)
table(alsp2$can_fre20, exclude = F)
table(alsp2$can_fre22, exclude = F)

alsp2$lifecan_17.5<- alsp2$can_fre17.5
alsp2$lifecan_17.5[alsp2$can_fre17.5 > 0]<- 1
table(alsp2$lifecan_17.5)

alsp2$lifecan_20<- alsp2$can_fre20
alsp2$lifecan_20[alsp2$can_fre20 > 0]<- 1
table(alsp2$lifecan_20)

alsp2$lifecan_22<- alsp2$can_fre22
alsp2$lifecan_22[alsp2$can_fre22 > 0]<- 1
table(alsp2$lifecan_22)

### bind factor scores to alsp2
alsp2<- bind_cols(alsp2, scores)

save(alsp2, file = "alsp2")

#### new session: 23/04 ####
load("alsp2")
save (alsp2, file = "alsp2")

#merge with PC scores 
load("alsp2")
pca<- read.table("PCA.eigenvec", header = T)

names(pca)
#pc names: "X.0.00101721" "X.0.0143344"   "X.0.0105101"   "X0.0204806" "X.0.00715047"  "X.0.0124797" "X.0.000671629" "X.0.00267093"  
#"X0.00530748" "X0.000155351" "X0.00731922"  "X.0.00234404" "X.0.000769747" "X9.82213e.05"  "X0.00679761" "X.0.0160743" "X0.0105473" "X.0.00528689

pca$X5A.1<- NULL
colnames(pca)<- c("IID", "pc1", "pc2", "pc3", "pc4", "pc5", "pc6", "pc7", "pc8", "pc9", "pc10", "pc11", "pc12", "pc13", "pc14", "pc15", "pc16", "pc17", "pc18", "pc19", "pc20")
alsp2<- merge(alsp2, pca, by = "IID", all.x = T)

save(alsp2, file = "alsp2")
#### new PRS scores ###
adhdPRS<- read.table("ADHD_fastscore_plink_24April.all.score", header = T)
save(adhdPRS, file = "adhdPRS")

bmiPRS<- read.table("BMI_fastscore_plink_24April.all..score", header = T)
save(bmiPRS, file = "bmiPRS")

MDD<- read.table("MDD_fastscore_plink_24April.all.score", header =T)
save(MDD, file = "mddPRS")

schiz<- read.table("Schizophrenia_fastscore_plink_24April.all.score", header = T)
save(schiz, file = "schizPRS")

### 25/04 ###
load("alsp2")
load("adhdPRS")
names(adhdPRS)
### merge with p<0.3 and 0.5 ##
#adhd 
adhd<- select(adhdPRS, IID,  X0.300000, X0.500000)
colnames(adhd)<- c("IID", "adhdPRS3", "adhdPRS5")
alsp2<- merge(alsp2, adhd, by = "IID", all.x = T)

hist(alsp2$adhdPRS3)
hist(alsp2$adhdPRS5)

# MDD
load("mddPRS")
mddPRS<- select (MDD, IID, X0.300000, X0.500000)
colnames(mddPRS)<- c ("IID", "mddPRS3", "mddPRS5")
alsp2<- merge(alsp2, mddPRS, by = "IID", all.x = T)
hist(alsp2$mddPRS5)

#schiz
load ("schizPRS")
schizPRS<- select (schiz, IID, X0.300000, X0.500000)
colnames(schizPRS)<- c ("IID", "schizPRS3", "schizPRS5")
alsp2<- merge(alsp2, schizPRS, by = "IID", all.x = T)
hist(alsp2$schizPRS5)

#bmi
load ("bmiPRS")
bmiPRS<- select (bmiPRS, IID, X0.300000, X0.500000)
colnames(bmiPRS)<- c ("IID", "bmiPRS3", "bmiPRS5")
alsp2<- merge(alsp2, bmiPRS, by = "IID", all.x = T)
hist(alsp2$bmiPRS5)

save(alsp2, file = "alsp2")

### 26/04 ###
load("alsp2")
alsp2$bmiPRS3<- NULL
alsp2$bmiPRS5<- NULL
## new bmi score
bmiPRS<- read.table("BMI_fastscore_plink_25April.all.score", header = T)
save(bmiPRS, file = "bmiPRS")
bmiPRS<- select (bmiPRS, IID, X0.300000, X0.500000)
colnames(bmiPRS)<- c ("IID", "bmiPRS3", "bmiPRS5")
alsp2<- merge(alsp2, bmiPRS, by = "IID", all.x = T)
hist(alsp2$bmiPRS5)
save(alsp2, file = "alsp2")

## 
load
round(prop.table(table(alsp2$drug_tot22)),4)
round(prop.table(table(alsp2$cig22)),4)
round(prop.table(table(alsp2$audit1_22)),4)
round(prop.table(table(alsp2$can_fre22)),4)

save(alsp2, file = "alsp2")

load("alsp2")

## new categorical variables for SU categories of validated questionnaires 
## age 22

#Ft
alsp2<- alsp3
table(alsp2$FT_total17.5)
alsp2$FTclass22<- alsp2$FT_total22
alsp2$FTclass22[alsp2$FT_total22 == 0]<- "non-smokers"
alsp2$FTclass22[alsp2$FT_total22 > 0 & alsp2$FT_total22 < 4]<- "low"
alsp2$FTclass22[alsp2$FT_total22 >= 4 & alsp2$FT_total22 < 7]<- "moderate"
alsp2$FTclass22[alsp2$FT_total22 >= 7]<- "high"
table(alsp2$FTclass22)
table(alsp3$FTclass22)

alsp3<- alsp2
rm(alsp2)
#audit 
table(alsp2$audit_tot22)
alsp2$auditclass22<- alsp2$audit_tot22
alsp2$auditclass22[alsp2$audit_tot22 > 0 & alsp2$audit_tot22 < 8] <- "low risk"
alsp2$auditclass22[alsp2$audit_tot22 >= 8 & alsp2$audit_tot22 < 16] <- "hazardous"
alsp2$auditclass22[alsp2$audit_tot22 >= 16] <- "harmful"

round(prop.table(table(alsp2$auditclass22)),3)

#cast 
table(alsp2$cast_tot22)
alsp2$castclass22<- alsp2$cast_tot22
alsp2$castclass22[alsp2$cast_tot22 > 0 & alsp2$cast_tot22 < 3]<- "low"
alsp2$castclass22[alsp2$cast_tot22 >= 3 & alsp2$cast_tot22 < 8 ]<- "moderate"
alsp2$castclass22[alsp2$cast_tot22 >= 8]<- "high"
round(prop.table(table(alsp2$castclass22)),3)

save(alsp2, file = "alsp2")

load("alsp2")

### nice histogram
ggplot(alsp2, aes(mddPRS5, fill = cut(mddPRS5, 100))) +
  geom_histogram(show.legend = FALSE) +
  scale_fill_discrete(h = c(240, 10))

ggplot(alsp2, aes(adhdPRS5, fill = cut(adhdPRS5, 100))) +
  geom_histogram(show.legend = FALSE) +
  scale_fill_discrete(h = c(240, 10))

ggplot(alsp2, aes(schizPRS5, fill = cut(schizPRS5, 100))) +
  geom_histogram(show.legend = FALSE) +
  scale_fill_discrete(h = c(240, 10))

load("alsp2")

summary(alsp2$adhdPRS5)
summary(alsp2$mddPRS5)
summary(alsp2$schizPRS5)
summary(alsp2$bmiPRS5)

#### 07/06 ###
#drop cases which do not have DNA data = 1532

alsp3<- subset(alsp2, !is.na (adhdPRS5))

#cultural background ~ 1= white, 2=mixed, 5=other 
table(alsp3$kw9500)
summary(alsp3$kw9500)

#sex 1=male; 2=female 
prop.table(table(alsp3$sex)) # ~57.1% female 

save(alsp3, file = "alsp3")

#### 02/07: new PRSs ####
load("alsp3")
prs <- read.xlsx("PRS_all_score_28June.xlsx", 1)
pca<- read.table("ALSPAC_DATA_PCA_independent_pairwise.eigenvec", header = T)

#rename pca 
?rename
names(pca)
pca<- rename(pca, IID = X5A)  
pca<- rename(pca, pca1 = X0.00196554 )  
pca<- rename(pca, pca2 = X0.0147207 )
pca<- rename(pca, pca3 = X0.00174206)
pca<- rename(pca, pca4 = X0.0141202)
pca<- rename(pca, pca5 = X.0.0206211 )
pca<- rename(pca, pca6 = X0.0163249)
pca<- rename(pca, pca7 = X.0.00637418)
pca<- rename(pca, pca8 = X0.00618003)
pca<- rename(pca, pca9 = X.0.000989774)
pca<- rename(pca, pca10 = X.0.00449615 )
pca<- rename(pca, pca11 = X.0.00949923)
pca<- rename(pca, pca12 = X.0.00224795 )
pca<- rename(pca, pca13 = X.0.00886189)
pca<- rename(pca, pca14 = X.0.00498645)
pca<- rename(pca, pca15 = X.0.00699098)
pca<- rename(pca, pca16 = X.0.00408523)
pca<- rename(pca, pca17 = X0.0129188)
pca<- rename(pca, pca18 = X0.00261826)
pca<- rename(pca, pca19 = X0.00146863)
pca<- rename(pca, pca20 = X0.00308618)

pca$X5A.1<- NULL

alsp<- merge(alsp3, prs, by = "IID")
alspp<- merge(alsp, pca, by = "IID")

alsp3<- alspp
rm(alsp)
rm(alspp)

save(alsp3, file ="alsp3")

###21 July### add new PRSs scores ### 41 new PRSs 
load("alsp3")

pca1<- read.csv("PRS_individual_11July_Ellie.csv", header = T, sep = "," )

#attach suffix to all columns 
colnames(pca1)<- paste0(colnames(pca1), "2")
names(pca1)

pca1$IID<- pca1$IID2
pca1$IID2<- NULL

#merge 
alsp3<- merge(alsp3, pca1, by = "IID")

#scales all prss
alsp3$AlcoholFrequency2<- scale(alsp3$AlcoholFrequency2, center = TRUE, scale = TRUE)
alsp3$Anorexia2<- scale(alsp3$Anorexia2, center = TRUE, scale = TRUE)
alsp3$BipolarDisorder2<- scale(alsp3$BipolarDisorder2, center = TRUE, scale = TRUE)
alsp3$BMI2<- scale(alsp3$BMI2, center = TRUE, scale = TRUE)
alsp3$CigaretteSeverity2<- scale(alsp3$CigaretteSeverity2, center = TRUE, scale = TRUE)
alsp3$Conscientiousness2<- scale(alsp3$Conscientiousness2, center = TRUE, scale = TRUE)
alsp3$DepressionSymptoms2<- scale(alsp3$DepressionSymptoms2, center = TRUE, scale = TRUE)
alsp3$Extraversion2<- scale(alsp3$Extraversion2, center = TRUE, scale = TRUE)
alsp3$InflammatoryBowelDisease2<- scale(alsp3$InflammatoryBowelDisease2, center = TRUE, scale = TRUE)
alsp3$Internalizing2<- scale(alsp3$Internalizing2, center = TRUE, scale = TRUE)
alsp3$Neuroticism2<- scale(alsp3$Neuroticism2, center = TRUE, scale = TRUE)
alsp3$Osteoporosis2<- scale(alsp3$Osteoporosis2, center = TRUE, scale = TRUE)
alsp3$Schizophrenia2<- scale(alsp3$Schizophrenia2, center = TRUE, scale = TRUE)
alsp3$ADHD2<- scale(alsp3$ADHD2, center = TRUE, scale = TRUE)
alsp3$AlcoholicLiverDisease2<- scale(alsp3$AlcoholicLiverDisease2, center = TRUE, scale = TRUE)
alsp3$Anxiety2<- scale(alsp3$Anxiety2, center = TRUE, scale = TRUE)
alsp3$BirthWeight2<- scale(alsp3$BirthWeight2, center = TRUE, scale = TRUE)
alsp3$CannabisUseDisorder2<- scale(alsp3$CannabisUseDisorder2, center = TRUE, scale = TRUE)
alsp3$CognitiveFunctionReactionTime2<- scale(alsp3$CognitiveFunctionReactionTime2, center = TRUE, scale = TRUE)
alsp3$CrossDisorder2<- scale(alsp3$CrossDisorder2, center = TRUE, scale = TRUE)
alsp3$EducationalAttainment2<- scale(alsp3$EducationalAttainment2, center = TRUE, scale = TRUE)
alsp3$HealthSelfRated2<- scale(alsp3$HealthSelfRated2, center = TRUE, scale = TRUE)
alsp3$Insomnia2<- scale(alsp3$Insomnia2, center = TRUE, scale = TRUE)
alsp3$Irritability2<- scale(alsp3$Irritability2, center = TRUE, scale = TRUE)
alsp3$OCD2<- scale(alsp3$OCD2, center = TRUE, scale = TRUE)
alsp3$ParkinsonDisease2<- scale(alsp3$ParkinsonDisease2, center = TRUE, scale = TRUE)
alsp3$SubjectiveWellbeing2<- scale(alsp3$SubjectiveWellbeing2, center = TRUE, scale = TRUE)
alsp3$Agreeableness2<- scale(alsp3$Agreeableness2, center = TRUE, scale = TRUE)
alsp3$AlzheimerFamilyHx2<- scale(alsp3$AlzheimerFamilyHx2, center = TRUE, scale = TRUE)
alsp3$Autism2<- scale(alsp3$Autism2, center = TRUE, scale = TRUE)
alsp3$BMIextreme2<- scale(alsp3$BMIextreme2, center = TRUE, scale = TRUE)
alsp3$CigaretteAgeOnset2<- scale(alsp3$CigaretteAgeOnset2, center = TRUE, scale = TRUE)
alsp3$CognitivePerformance2<- scale(alsp3$CognitivePerformance2, center = TRUE, scale = TRUE)
alsp3$DepressionDiagnosis2<- scale(alsp3$DepressionDiagnosis2, center = TRUE, scale = TRUE)
alsp3$ExtraversionIRT2<- scale(alsp3$ExtraversionIRT2, center = TRUE, scale = TRUE)
alsp3$Height2<- scale(alsp3$Height2, center = TRUE, scale = TRUE)
alsp3$Intelligence2<- scale(alsp3$Intelligence2, center = TRUE, scale = TRUE)
alsp3$Loneliness2<- scale(alsp3$Loneliness2, center = TRUE, scale = TRUE)
alsp3$Openness2<- scale(alsp3$Openness2, center = TRUE, scale = TRUE)
alsp3$RiskTaking2<- scale(alsp3$RiskTaking2, center = TRUE, scale = TRUE)
alsp3$Worry2<- scale(alsp3$Worry2, center = TRUE, scale = TRUE)

hist(alsp3$RiskTaking)

save(alsp3, file = "alsp3")

load("alsp3")
save(alsp3, file = "alsp3")

### new PRSs 28 August 

load("alsp3")

pca1<- read.csv("PRS_individual_28August.csv", header = T, sep = "," )

#attach suffix to all columns 
colnames(pca1)<- paste0(colnames(pca1), "3")
names(pca1)

pca1$IID<- pca1$IID3
pca1$IID3<- NULL
pca1$X3<- NULL

#merge 
alsp3<- merge(alsp3, pca1, by = "IID")

#scales all prs
alsp3$AlcoholFrequency3<- scale(alsp3$AlcoholFrequency3, center = TRUE, scale = TRUE)
alsp3$Anorexia3<- scale(alsp3$Anorexia3, center = TRUE, scale = TRUE)
alsp3$BipolarDisorder3<- scale(alsp3$BipolarDisorder3, center = TRUE, scale = TRUE)
alsp3$BMI3<- scale(alsp3$BMI3, center = TRUE, scale = TRUE)
alsp3$CigaretteSeverity3<- scale(alsp3$CigaretteSeverity3, center = TRUE, scale = TRUE)
alsp3$Conscientiousness3<- scale(alsp3$Conscientiousness3, center = TRUE, scale = TRUE)
alsp3$DepressionSymptoms3<- scale(alsp3$DepressionSymptoms3, center = TRUE, scale = TRUE)
alsp3$Extraversion3<- scale(alsp3$Extraversion3, center = TRUE, scale = TRUE)
alsp3$Internalizing3<- scale(alsp3$Internalizing3, center = TRUE, scale = TRUE)
alsp3$Neuroticism3<- scale(alsp3$Neuroticism3, center = TRUE, scale = TRUE)
alsp3$Schizophrenia3<- scale(alsp3$Schizophrenia3, center = TRUE, scale = TRUE)
alsp3$ADHD3<- scale(alsp3$ADHD3, center = TRUE, scale = TRUE)
alsp3$Anxiety3<- scale(alsp3$Anxiety3, center = TRUE, scale = TRUE)
alsp3$BirthWeight3<- scale(alsp3$BirthWeight3, center = TRUE, scale = TRUE)
alsp3$CannabisUseDisorder3<- scale(alsp3$CannabisUseDisorder3, center = TRUE, scale = TRUE)
alsp3$CrossDisorder3<- scale(alsp3$CrossDisorder3, center = TRUE, scale = TRUE)
alsp3$EducationalAttainment3<- scale(alsp3$EducationalAttainment3, center = TRUE, scale = TRUE)
alsp3$Insomnia3<- scale(alsp3$Insomnia3, center = TRUE, scale = TRUE)
alsp3$Irritability3<- scale(alsp3$Irritability3, center = TRUE, scale = TRUE)
alsp3$OCD3<- scale(alsp3$OCD3, center = TRUE, scale = TRUE)
alsp3$Agreeableness3<- scale(alsp3$Agreeableness3, center = TRUE, scale = TRUE)
alsp3$Autism3<- scale(alsp3$Autism3, center = TRUE, scale = TRUE)
alsp3$CigaretteAgeOnset3<- scale(alsp3$CigaretteAgeOnset3, center = TRUE, scale = TRUE)
alsp3$DepressionDiagnosis3<- scale(alsp3$DepressionDiagnosis3, center = TRUE, scale = TRUE)
alsp3$ExtraversionIRT3<- scale(alsp3$ExtraversionIRT3, center = TRUE, scale = TRUE)
alsp3$Height3<- scale(alsp3$Height3, center = TRUE, scale = TRUE)
alsp3$Intelligence3<- scale(alsp3$Intelligence3, center = TRUE, scale = TRUE)
alsp3$Loneliness3<- scale(alsp3$Loneliness3, center = TRUE, scale = TRUE)
alsp3$Openness3<- scale(alsp3$Openness3, center = TRUE, scale = TRUE)
alsp3$RiskTaking3<- scale(alsp3$RiskTaking3, center = TRUE, scale = TRUE)
alsp3$Worry3<- scale(alsp3$Worry3, center = TRUE, scale = TRUE)
alsp3$AlcoholAddictionEver3<- scale(alsp3$AlcoholAddictionEver3, center = TRUE, scale = TRUE)

#scale pca scores 
alsp3$pca1<- scale(alsp3$pca1, center = TRUE, scale = TRUE)
alsp3$pca2<- scale(alsp3$pca2, center = TRUE, scale = TRUE)
alsp3$pca3<- scale(alsp3$pca3, center = TRUE, scale = TRUE)
alsp3$pca4<- scale(alsp3$pca4, center = TRUE, scale = TRUE)
alsp3$pca5<- scale(alsp3$pca5, center = TRUE, scale = TRUE)
alsp3$pca6<- scale(alsp3$pca6, center = TRUE, scale = TRUE)
alsp3$pca7<- scale(alsp3$pca7, center = TRUE, scale = TRUE)
alsp3$pca8<- scale(alsp3$pca8, center = TRUE, scale = TRUE)
alsp3$pca9<- scale(alsp3$pca9, center = TRUE, scale = TRUE)
alsp3$pca10<- scale(alsp3$pca10, center = TRUE, scale = TRUE)
alsp3$pca11<- scale(alsp3$pca11, center = TRUE, scale = TRUE)
alsp3$pca12<- scale(alsp3$pca12, center = TRUE, scale = TRUE)
alsp3$pca13<- scale(alsp3$pca13, center = TRUE, scale = TRUE)
alsp3$pca14<- scale(alsp3$pca14, center = TRUE, scale = TRUE)
alsp3$pca15<- scale(alsp3$pca15, center = TRUE, scale = TRUE)
alsp3$pca16<- scale(alsp3$pca16, center = TRUE, scale = TRUE)
alsp3$pca17<- scale(alsp3$pca17, center = TRUE, scale = TRUE)
alsp3$pca18<- scale(alsp3$pca18, center = TRUE, scale = TRUE)
alsp3$pca19<- scale(alsp3$pca19, center = TRUE, scale = TRUE)
alsp3$pca20<- scale(alsp3$pca20, center = TRUE, scale = TRUE)

save(alsp3, file = "alsp3")

### descriptive stats of substance use ##

#new categoristion following recommended cut-offs (10 Sep) 

load("alsp3")
#### Ft ###

#age22
alsp2<- alsp3

table(alsp2$FT_total22)
alsp2$FTclass22<- alsp2$FT_total22
alsp2$FTclass22[alsp2$FT_total22 == 0]<- "non-smokers"
alsp2$FTclass22[alsp2$FT_total22 > 0 & alsp2$FT_total22 <= 2]<- "low"
alsp2$FTclass22[alsp2$FT_total22 >= 3 & alsp2$FT_total22 < 6]<- "moderate"
alsp2$FTclass22[alsp2$FT_total22 >= 6]<- "high"
round(prop.table(table(alsp2$FTclass22)),3)


#age20
table(alsp2$FT_total20)
alsp2$FTclass20<- alsp2$FT_total20
alsp2$FTclass20[alsp2$FT_total20 == 0]<- "non-smokers"
alsp2$FTclass20[alsp2$FT_total20 > 0 & alsp2$FT_total20 <= 2]<- "low"
alsp2$FTclass20[alsp2$FT_total20 >= 3 & alsp2$FT_total20 < 6]<- "moderate"
alsp2$FTclass20[alsp2$FT_total20 >= 6]<- "high"
round(prop.table(table(alsp2$FTclass20)),3)

#age17.5
table(alsp2$FT_total17.5)
alsp2$FTclass17.5<- alsp2$FT_total17.5
alsp2$FTclass17.5[alsp2$FT_total17.5 == 0]<- "non-smokers"
alsp2$FTclass17.5[alsp2$FT_total17.5 > 0 & alsp2$FT_total17.5 <= 2]<- "low"
alsp2$FTclass17.5[alsp2$FT_total17.5 >= 3 & alsp2$FT_total17.5 < 6]<- "moderate"
alsp2$FTclass17.5[alsp2$FT_total17.5 >= 6]<- "high"
round(prop.table(table(alsp2$FTclass17.5)),3)


### audit ###

#age22
table(alsp2$audit_tot22)
alsp2$auditclass22<- alsp2$audit_tot22
alsp2$auditclass22[alsp2$audit_tot22 == 0]<- "non-drinkers"
alsp2$auditclass22[alsp2$audit_tot22 > 0 & alsp2$audit_tot22 < 8] <- "low risk"
alsp2$auditclass22[alsp2$audit_tot22 >= 8 & alsp2$audit_tot22 < 16] <- "hazardous"
alsp2$auditclass22[alsp2$audit_tot22 >= 16] <- "harmful"
round(prop.table(table(alsp2$auditclass22)),3)

#age20
table(alsp2$audit_tot20)
alsp2$auditclass20<- alsp2$audit_tot20
alsp2$auditclass20[alsp2$audit_tot20 == 0]<- "non-drinkers"
alsp2$auditclass20[alsp2$audit_tot20 > 0 & alsp2$audit_tot20 < 8] <- "low risk"
alsp2$auditclass20[alsp2$audit_tot20 >= 8 & alsp2$audit_tot20 < 16] <- "hazardous"
alsp2$auditclass20[alsp2$audit_tot20 >= 16] <- "harmful"
round(prop.table(table(alsp2$auditclass20)),3)

#age17.5
table(alsp2$audit_tot17.5)
alsp2$auditclass17.5<- alsp2$audit_tot17.5
alsp2$auditclass17.5[alsp2$audit_tot17.5 == 0]<- "non-drinkers"
alsp2$auditclass17.5[alsp2$audit_tot17.5 > 0 & alsp2$audit_tot17.5 < 8] <- "low risk"
alsp2$auditclass17.5[alsp2$audit_tot17.5 >= 8 & alsp2$audit_tot17.5 < 16] <- "hazardous"
alsp2$auditclass17.5[alsp2$audit_tot17.5 >= 16] <- "harmful"
round(prop.table(table(alsp2$auditclass17.5)),3)


### cast ###
#age22
load("alsp4")
alsp2=alsp4
table(alsp2$cast_tot22)
alsp2$castclass22<- alsp2$cast_tot22
alsp2$castclass22[alsp2$cast_tot22 == 0]<- "no cannabis"
alsp2$castclass22[alsp2$cast_tot22 > 0 & alsp2$cast_tot22 < 4]<- "low"
alsp2$castclass22[alsp2$cast_tot22 >= 4 & alsp2$cast_tot22 < 7 ]<- "moderate"
alsp2$castclass22[alsp2$cast_tot22 >= 7]<- "high"
round(prop.table(table(alsp2$castclass22)),3)

##age20
table(alsp2$cast_tot20)
alsp2$castclass20<- alsp2$cast_tot20
alsp2$castclass20[alsp2$cast_tot20 == 0]<- "no cannabis"
alsp2$castclass20[alsp2$cast_tot20 > 0 & alsp2$cast_tot20 < 4]<- "low"
alsp2$castclass20[alsp2$cast_tot20 >= 4 & alsp2$cast_tot20 < 7 ]<- "moderate"
alsp2$castclass20[alsp2$cast_tot20 >= 7]<- "high"
round(prop.table(table(alsp2$castclass20)),3)

##age17.5
table(alsp2$cast_tot17.5)
alsp2$castclass17.5<- alsp2$cast_tot17.5
alsp2$castclass17.5[alsp2$cast_tot17.5 == 0]<- "no cannabis"
alsp2$castclass17.5[alsp2$cast_tot17.5 > 0 & alsp2$cast_tot17.5 < 4]<- "low"
alsp2$castclass17.5[alsp2$cast_tot17.5 >= 4 & alsp2$cast_tot17.5 < 7 ]<- "moderate"
alsp2$castclass17.5[alsp2$cast_tot17.5 >= 7]<- "high"
round(prop.table(table(alsp2$castclass17.5)),3)
 
### other illict drugs:just group them into 0, 1-2,3-4,5 + 

#age22
table(alsp2$drug_tot22)
alsp2$drugclass22<- alsp2$drug_tot22
alsp2$drugclass22[alsp2$drug_tot22 > 0 & alsp2$drug_tot22 <= 2]<- "1-2"
alsp2$drugclass22[alsp2$drug_tot22 >= 3 & alsp2$drug_tot22 < 5 ]<- "3-4"
alsp2$drugclass22[alsp2$drug_tot22 >= 5]<- "5+"
round(prop.table(table(alsp2$drugclass22)),3)

#age20
table(alsp2$drug_tot20)
alsp2$drugclass20<- alsp2$drug_tot20
alsp2$drugclass20[alsp2$drug_tot20 > 0 & alsp2$drug_tot20 <= 2]<- "1-2"
alsp2$drugclass20[alsp2$drug_tot20 >= 3 & alsp2$drug_tot20 < 5 ]<- "3-4"
alsp2$drugclass20[alsp2$drug_tot20 >= 5]<- "5+"
round(prop.table(table(alsp2$drugclass20)),3)

#17.5
table(alsp2$drug_tot17.5)
alsp2$drugclass17.5<- alsp2$drug_tot17.5
alsp2$drugclass17.5[alsp2$drug_tot17.5 > 0 & alsp2$drug_tot17.5 <= 2]<- "1-2"
alsp2$drugclass17.5[alsp2$drug_tot17.5 >= 3 & alsp2$drug_tot17.5 < 5 ]<- "3-4"
alsp2$drugclass17.5[alsp2$drug_tot17.5 >= 5]<- "5+"
round(prop.table(table(alsp2$drugclass17.5)),3)

alsp3<- datALSP
alsp2<- alsp3
rm(alsp2)
rm(datALSP)
save(alsp3, file = "alsp3")
load("alsp3")
#


#### new PGSs 24 Sep ####
load("alsp3")

pca1<- read.csv("PRS_Ellie_24Sep.csv", header = T, sep = "," )

#attach suffix to all columns 
colnames(pca1)<- paste0(colnames(pca1), "3")
names(pca1)

pca1$IID<- pca1$IID3
pca1$IID3<- NULL
pca1$X3<- NULL

### remove all old PRSs ###

alsp3$AlcoholFrequency3<- NULL
alsp3$Anorexia3<- NULL
alsp3$BipolarDisorder3<- NULL
alsp3$BMI3<- NULL
alsp3$CigaretteSeverity3<- NULL
alsp3$Conscientiousness3<- NULL
alsp3$DepressionSymptoms3<- NULL
alsp3$Extraversion3<- NULL
alsp3$Internalizing3<- NULL
alsp3$Neuroticism3<- NULL
alsp3$Schizophrenia3<- NULL
alsp3$ADHD3<- NULL
alsp3$Anxiety3<- NULL
alsp3$BirthWeight3<- NULL
alsp3$CannabisUseDisorder3<- NULL
alsp3$CrossDisorder3<- NULL
alsp3$EducationalAttainment3<- NULL
alsp3$Insomnia3<- NULL
alsp3$Irritability3<- NULL
alsp3$OCD3<- NULL
alsp3$Agreeableness3<- NULL
alsp3$Autism3<- NULL
alsp3$CigaretteAgeOnset3<- NULL
alsp3$DepressionDiagnosis3<- NULL
alsp3$ExtraversionIRT3<- NULL
alsp3$Height3<- NULL
alsp3$Intelligence3<- NULL
alsp3$Loneliness3<- NULL
alsp3$Openness3<- NULL
alsp3$RiskTaking3<- NULL
alsp3$Worry3<- NULL
alsp3$AlcoholAddictionEver3<- NULL

#merge two datasets
alsp3<- merge(alsp3, pca1, by = "IID", all.x = T)

#scales all prs
alsp3$AlcoholFrequency3<- scale(alsp3$AlcoholFrequency3, center = TRUE, scale = TRUE)
alsp3$Anorexia3<- scale(alsp3$Anorexia3, center = TRUE, scale = TRUE)
alsp3$BipolarDisorder3<- scale(alsp3$BipolarDisorder3, center = TRUE, scale = TRUE)
alsp3$BMI3<- scale(alsp3$BMI3, center = TRUE, scale = TRUE)
alsp3$CigaretteSeverity3<- scale(alsp3$CigaretteSeverity3, center = TRUE, scale = TRUE)
alsp3$Conscientiousness3<- scale(alsp3$Conscientiousness3, center = TRUE, scale = TRUE)
alsp3$DepressionSymptoms3<- scale(alsp3$DepressionSymptoms3, center = TRUE, scale = TRUE)
alsp3$Extraversion3<- scale(alsp3$Extraversion3, center = TRUE, scale = TRUE)
alsp3$Internalizing3<- scale(alsp3$Internalizing3, center = TRUE, scale = TRUE)
alsp3$Neuroticism3<- scale(alsp3$Neuroticism3, center = TRUE, scale = TRUE)
alsp3$Schizophrenia3<- scale(alsp3$Schizophrenia3, center = TRUE, scale = TRUE)
alsp3$ADHD3<- scale(alsp3$ADHD3, center = TRUE, scale = TRUE)
alsp3$Anxiety3<- scale(alsp3$Anxiety3, center = TRUE, scale = TRUE)
alsp3$BirthWeight3<- scale(alsp3$BirthWeight3, center = TRUE, scale = TRUE)
alsp3$CrossDisorder3<- scale(alsp3$CrossDisorder3, center = TRUE, scale = TRUE)
alsp3$EducationalAttainment3<- scale(alsp3$EducationalAttainment3, center = TRUE, scale = TRUE)
alsp3$Insomnia3<- scale(alsp3$Insomnia3, center = TRUE, scale = TRUE)
alsp3$Irritability3<- scale(alsp3$Irritability3, center = TRUE, scale = TRUE)
alsp3$OCD3<- scale(alsp3$OCD3, center = TRUE, scale = TRUE)
alsp3$Agreeableness3<- scale(alsp3$Agreeableness3, center = TRUE, scale = TRUE)
alsp3$Autism3<- scale(alsp3$Autism3, center = TRUE, scale = TRUE)
alsp3$CigaretteAgeOnset3<- scale(alsp3$CigaretteAgeOnset3, center = TRUE, scale = TRUE)
alsp3$DepressionDiagnosis3<- scale(alsp3$DepressionDiagnosis3, center = TRUE, scale = TRUE)
alsp3$ExtraversionIRT3<- scale(alsp3$ExtraversionIRT3, center = TRUE, scale = TRUE)
alsp3$Height3<- scale(alsp3$Height3, center = TRUE, scale = TRUE)
alsp3$Intelligence3<- scale(alsp3$Intelligence3, center = TRUE, scale = TRUE)
alsp3$Loneliness3<- scale(alsp3$Loneliness3, center = TRUE, scale = TRUE)
alsp3$Openness3<- scale(alsp3$Openness3, center = TRUE, scale = TRUE)
alsp3$RiskTaking3<- scale(alsp3$RiskTaking3, center = TRUE, scale = TRUE)
alsp3$Worry3<- scale(alsp3$Worry3, center = TRUE, scale = TRUE)
alsp3$AlcoholAddictionEver3<- scale(alsp3$AlcoholAddictionEver3, center = TRUE, scale = TRUE)
alsp3$CannabisUseEver3<- scale(alsp3$CannabisUseEver3,center = TRUE, scale = TRUE )
alsp3$CannabisUseFrequency3<- scale(alsp3$CannabisUseFrequency3, center = TRUE, scale = TRUE)

save(alsp3, file = "alsp3")

load("alsp3")

## read file with results ###
prs<-read.csv("PRS.csv")

#fdr corrected p-values 
prs$p.value_fdr<- p.adjust(prs$p.value, method = "fdr", n = 80)

#round corrected p-values
numVars <- sapply(prs, is.numeric) 
prs[numVars] <- lapply(prs[numVars], round, digits = 3) 

 
write.csv(prs, "prs_adjusted.csv")
  
?p.adjust

##adjusted p-values for file with bipolar 
prs<- read.csv("prs_adjusted.csv")

prs$p.value_fdr<- p.adjust(prs$p.value, method = "fdr")

### check nicotine 
table(alsp3$FJSM1001)

### so all people who did not smoke daily (22, 17.5) or weekly (20) did not complete FT scale, so assign different value to non-smoker 
table(alsp3$FTclass17.5, alsp3$cig17.5)
alsp3$FTclass17.5[alsp3$cig17.5 %in% c(1,2,3)]<- "no dependence"
table(alsp3$FTclass17.5, alsp3$cig17.5)

table(alsp3$FTclass20, alsp3$cig20)
alsp3$FTclass20[alsp3$cig20 %in% c(1,2)]<- "no dependence"
table(alsp3$FTclass20, alsp3$cig20)

table(alsp3$FTclass22, alsp3$cig22)
alsp3$FTclass22[alsp3$cig22 %in% c(1,2,3)]<- "no dependence"
table(alsp3$FTclass22, alsp3$cig22)

round(prop.table(table(alsp3$FTclass17.5)),3)
round(prop.table(table(alsp3$FTclass20)),3)
round(prop.table(table(alsp3$FTclass22)),3)

save(alsp3, file ="alsp3")

##check cannabis (0-1 are no cannabis )
table(alsp3$can_fre17.5, alsp3$castclass17.5)
table(alsp3$can_fre20, alsp3$castclass20)
table(alsp3$can_fre22, alsp3$castclass22)

#chekc audit
table(alsp3$auditclass20, alsp3$CCU3320)

#### correlation of mean drug scores ###
#create average scores 

#example from Tabea, mean score for those who have at least one observation 
# TOTAL SCORE (relational) ACROSS ALL AGES
BFIS_total_relational = data.frame(datALSP$BFIS_age13_mean_relational,datALSP$BFIS_age10_mean_relational,datALSP$BFIS_age8_mean_relational)
datALSP$BFIS_total_mean_relational=NA #create n empty variable
datALSP$BFIS_total_mean_relational = rowMeans(BFIS_total_relational, na.rm=TRUE) #rowmeans over all the items
BFIS_total_missing_relational = BFIS_total_relational; BFIS_total_missing_relational[is.na(BFIS_total_relational) ==FALSE] = 0; BFIS_total_missing[is.na(BFIS_total) ==TRUE] = 1
datALSP$BFIS_total_mean_relational[rowSums(BFIS_total_missing_relational) >=2] <- NA #code NA when the same of missing items is superior or equal to 6=> usually about 2/3 or 75% of the items need to be there, so that will vary depending on number of items in your scale
summary(datALSP$BFIS_total_mean_relational)

#my avergae scores 
#FT_total17.5, audit_tot17.5, cast_tot17.5_s, drug_tot17.5_s
load("alsp3")
#nicotine 
nic=select(alsp3, FT_total17.5_s, FT_total20_s, FT_total22_s)
alsp3$FT_meantot=rowMeans(nic, na.rm = T)
summary(alsp3$FT_meantot)
hist(alsp3$FT_meantot)

#alcohol 
alc=select(alsp3, audit_tot17.5_s, audit_tot20_s, audit_tot22_s)
alsp3$audit_meantot=rowMeans(alc, na.rm = T)
summary(alsp3$audit_meantot)
hist(alsp3$audit_meantot)

#cannabis 
can= select(alsp3, cast_tot17.5_s, cast_tot20_s, cast_tot22_s)
alsp3$cast_meantot=rowMeans(can, na.rm = T)
summary(alsp3$cast_meantot)
hist(alsp3$cast_meantot)

#other drugs
drug= select(alsp3, drug_tot17.5_s, drug_tot20_s, drug_tot22_s )
alsp3$drug_meantot=rowMeans(drug, na.rm =T)
summary(alsp3$drug_meantot)
hist(alsp3$drug_meantot)

save(alsp3, file ="alsp3")


#####
0.576+
0.396+
0.741+
0.555+
0.954+
0.789+
0.869+
0.870+
0.645+
0.645+
0.876+
0.677

8.593/12

load("alsp3")
hist(alsp3$Schizophrenia3)


#### new PGS February 2019 ####

load("alsp3")

pca1<- read.csv("PGS_Ellie_30Jan_reduced.csv", header = T, sep = "," )

#attach suffix to all columns 
colnames(pca1)<- paste0(colnames(pca1), "3")
names(pca1)

pca1$IID<- pca1$IID3
pca1$IID3<- NULL
pca1$X3<- NULL

### remove all old PRSs ###

alsp3$AlcoholFrequency3<- NULL
alsp3$Anorexia3<- NULL
alsp3$BipolarDisorder3<- NULL
alsp3$BMI3<- NULL
alsp3$CigaretteSeverity3<- NULL
alsp3$Conscientiousness3<- NULL
alsp3$DepressionSymptoms3<- NULL
alsp3$Extraversion3<- NULL
alsp3$Internalizing3<- NULL
alsp3$Neuroticism3<- NULL
alsp3$Schizophrenia3<- NULL
alsp3$ADHD3<- NULL
alsp3$Anxiety3<- NULL
alsp3$BirthWeight3<- NULL
alsp3$CannabisUseDisorder3<- NULL
alsp3$CrossDisorder3<- NULL
alsp3$EducationalAttainment3<- NULL
alsp3$Insomnia3<- NULL
alsp3$Irritability3<- NULL
alsp3$OCD3<- NULL
alsp3$Agreeableness3<- NULL
alsp3$Autism3<- NULL
alsp3$CigaretteAgeOnset3<- NULL
alsp3$DepressionDiagnosis3<- NULL
alsp3$ExtraversionIRT3<- NULL
alsp3$Height3<- NULL
alsp3$Intelligence3<- NULL
alsp3$Loneliness3<- NULL
alsp3$Openness3<- NULL
alsp3$RiskTaking3<- NULL
alsp3$Worry3<- NULL
alsp3$AlcoholAddictionEver3<- NULL
alsp3$CannabisUseFrequency3<- NULL

#merge two datasets
alsp3<- merge(alsp3, pca1, by = "IID", all.x = T)
head(alsp3[,16090:16111])

#scales all prs

alsp3$Anorexia3<- scale(alsp3$Anorexia3, center = TRUE, scale = TRUE)
alsp3$BMI3<- scale(alsp3$BMI3, center = TRUE, scale = TRUE)
alsp3$Conscientiousness3<- scale(alsp3$Conscientiousness3, center = TRUE, scale = TRUE)
alsp3$ExtraversionIRT3<- scale(alsp3$ExtraversionIRT3, center = TRUE, scale = TRUE)
alsp3$Neuroticism3<- scale(alsp3$Neuroticism3, center = TRUE, scale = TRUE)
alsp3$Schizophrenia3<- scale(alsp3$Schizophrenia3, center = TRUE, scale = TRUE)
alsp3$ADHD3<- scale(alsp3$ADHD3, center = TRUE, scale = TRUE)
alsp3$Anxiety3<- scale(alsp3$Anxiety3, center = TRUE, scale = TRUE)
alsp3$BirthWeight3<- scale(alsp3$BirthWeight3, center = TRUE, scale = TRUE)
alsp3$EducationalAttainment3<- scale(alsp3$EducationalAttainment3, center = TRUE, scale = TRUE)
alsp3$Irritability3<- scale(alsp3$Irritability3, center = TRUE, scale = TRUE)
alsp3$Autism3<- scale(alsp3$Autism3, center = TRUE, scale = TRUE)
alsp3$DepressionDiagnosis3<- scale(alsp3$DepressionDiagnosis3, center = TRUE, scale = TRUE)
alsp3$Height3<- scale(alsp3$Height3, center = TRUE, scale = TRUE)
alsp3$Openness3<- scale(alsp3$Openness3, center = TRUE, scale = TRUE)
alsp3$RiskTaking3<- scale(alsp3$RiskTaking3, center = TRUE, scale = TRUE)
alsp3$Worry3<- scale(alsp3$Worry3, center = TRUE, scale = TRUE)
alsp3$CannabisUseFrequency3<- scale(alsp3$CannabisUseFrequency3, center = TRUE, scale = TRUE)
alsp3$AlcoholAUDIT_total3<- scale(alsp3$AlcoholAUDIT_total3, center = TRUE, scale = TRUE)
alsp3$CigarettesPerDay3<- scale(alsp3$CigarettesPerDay3, center = TRUE, scale = TRUE)
alsp3$CigaretteAgeOnset3<- scale(alsp3$CigaretteAgeOnset3, center = TRUE, scale = TRUE)




a=lm(Schizophrenia3 ~ CannabisUseFrequency3, data =alsp3)
summary(a)
alsp3$audit_meantot


alsp4=alsp3
save(alsp4, file ="alsp4")

## read file with results ###
prs<-read.csv("results_hope.csv")


#fdr corrected p-values 
prs$p.value_fdr<- p.adjust(prs$p.value, method = "fdr", n = 80)

#round corrected p-values
numVars <- sapply(prs, is.numeric) 
prs[numVars] <- lapply(prs[numVars], round, digits = 3) 


write.csv(prs, "prs_adjusted.csv")

### recategorise cannabis abuse: rannge is 0-25, but 0 and 1 = non smokers 
### cast ###
#age22
load("alsp4")
alsp2=alsp4

table(alsp2$cast_tot22)
alsp2$castclass22<- alsp2$cast_tot22
alsp2$castclass22[alsp2$cast_tot22 <= 1]<- "no cannabis"
alsp2$castclass22[alsp2$cast_tot22 > 1 & alsp2$cast_tot22 < 10]<- "low"
alsp2$castclass22[alsp2$cast_tot22 >= 10 & alsp2$cast_tot22 < 18 ]<- "moderate"
alsp2$castclass22[alsp2$cast_tot22 >= 18]<- "high"
round(prop.table(table(alsp2$castclass22, exclude =F)),3)*100

##age20
table(alsp2$cast_tot20)
alsp2$castclass20<- alsp2$cast_tot20
alsp2$castclass20[alsp2$cast_tot20 <=1]<- "no cannabis"
alsp2$castclass20[alsp2$cast_tot20 > 1 & alsp2$cast_tot20 < 10]<- "low"
alsp2$castclass20[alsp2$cast_tot20 >= 10 & alsp2$cast_tot20 < 18 ]<- "moderate"
alsp2$castclass20[alsp2$cast_tot20 >= 18]<- "high"
round(prop.table(table(alsp2$castclass20, exclude =F)),3)*100

##age17.5
table(alsp2$cast_tot17.5)
alsp2$castclass17.5<- alsp2$cast_tot17.5
alsp2$castclass17.5[alsp2$cast_tot17.5 <=1]<- "no cannabis"
alsp2$castclass17.5[alsp2$cast_tot17.5 > 1 & alsp2$cast_tot17.5 < 10]<- "low"
alsp2$castclass17.5[alsp2$cast_tot17.5 >= 10 & alsp2$cast_tot17.5 < 18 ]<- "moderate"
alsp2$castclass17.5[alsp2$cast_tot17.5 >= 18]<- "high"
round(prop.table(table(alsp2$castclass17.5)),3)*100

save(alsp4, file ="alsp4") # saved both to N:Alspac and onedrive, rotation2

##### hurray, new polygenic scores (20/02/19) #####
setwd("~/Data/Alspac")
load("alsp4")

## new scores : AlcoholDependence3	
#               AlcoholPerWeek3
#               CannabisUseDisorder3
#               CannabisUseFrequency3	
#               CigaretteAgeOnsetRegular3	
#               CigarettesPerDay3

#delete pGS already present in dataset
alsp4$CannabisUseFrequency3= NULL
alsp4$CigarettesPerDay3 =NULL

#load csv with new SA PGSs
sa=read.csv("Substance abuse PGSs.csv")

#merge files 
alsp4=merge(alsp4, sa, by = "IID", all.x = T)
head(alsp4[,16110:ncol(alsp4)])

#scale scores 
alsp4$CigarettesPerDay3<- scale(alsp4$CigarettesPerDay3, center = TRUE, scale = TRUE)
alsp4$CigaretteAgeOnsetRegular3<- scale(alsp4$CigaretteAgeOnsetRegular3, center = TRUE, scale = TRUE)

alsp4$AlcoholDependence3<- scale(alsp4$AlcoholDependence3, center = TRUE, scale = TRUE)
alsp4$AlcoholPerWeek3<- scale(alsp4$AlcoholPerWeek3, center = TRUE, scale = TRUE)

alsp4$CannabisUseDisorder3<- scale(alsp4$CannabisUseDisorder3, center = TRUE, scale = TRUE)
alsp4$CannabisUseFrequency3	<- scale(alsp4$CannabisUseFrequency3	, center = TRUE, scale = TRUE)

save(alsp4, file ="alsp4")
load("alsp4")

### adjust new results - 09 sep 2019 
##risk factors 
#single 
prs<- read.csv("results_singlerisk_09sep.csv")
#fdr corrected p-values 
prs$p.value_fdr<- p.adjust(prs$p.value, method = "fdr", n = 90)

#round corrected p-values
numVars <- sapply(prs, is.numeric) 
prs[numVars] <- lapply(prs[numVars], round, digits = 3) 

write.csv(prs, "results_singlerisk_09sep_adj.csv")

#multi-PGS models 
multi=read.csv("Multi-PGS risk factors_09sep.csv")
multi$p.value_fdr<- p.adjust(multi$p.value, method = "fdr")

#round corrected p-values
numVars <- sapply(multi, is.numeric) 
multi[numVars] <- lapply(multi[numVars], round, digits = 3) 

write.csv(multi, file="Multi-PGS risk factors_09sep_adj.csv")

##substance abuse 
#single 
multi=read.csv("single-PGS substance abuse_09sep.csv")
multi$p.value_fdr<- p.adjust(multi$p.value, method = "fdr")

#round corrected p-values
numVars <- sapply(multi, is.numeric) 
multi[numVars] <- lapply(multi[numVars], round, digits = 3) 

write.csv(multi, file="single-PGS substance abuse_09sep_adj.csv")

#multi
multi=read.csv("multi-PGS substance_09sep.csv")
multi$p.value_fdr<- p.adjust(multi$p.value, method = "fdr")

#round corrected p-values
numVars <- sapply(multi, is.numeric) 
multi[numVars] <- lapply(multi[numVars], round, digits = 3) 

write.csv(multi, file="multi-PGS substance_09sep_adj.csv")




###note that standardizedSolution gives you all parameters

setwd("C:/Users/rmjleio/OneDrive/Rotation 2")
load("alsp4")


#descriptive stats mean and sd of substance use measures 
#FT
round(mean(alsp4$FT_total17.5, na.rm=T),1)
round(sd(alsp4$FT_total17.5, na.rm=T),1)

round(mean(alsp4$FT_total20, na.rm=T),1)
round(sd(alsp4$FT_total20, na.rm=T),1)

round(mean(alsp4$FT_total22, na.rm=T),1)
round(sd(alsp4$FT_total22, na.rm=T),1)

#alcohol 
round(mean(alsp4$audit_tot17.5, na.rm=T),1)
round(sd(alsp4$audit_tot17.5, na.rm=T),1)

round(mean(alsp4$audit_tot20, na.rm=T),1)
round(sd(alsp4$audit_tot20, na.rm=T),1)

round(mean(alsp4$audit_tot22, na.rm=T),1)
round(sd(alsp4$audit_tot22, na.rm=T),1)

#cannabis 
round(mean(alsp4$cast_tot17.5, na.rm=T),1)
round(sd(alsp4$cast_tot17.5, na.rm=T),1)

round(mean(alsp4$cast_tot20, na.rm=T),1)
round(sd(alsp4$cast_tot20, na.rm=T),1)

round(mean(alsp4$cast_tot22, na.rm=T),1)
round(sd(alsp4$cast_tot22, na.rm=T),1)


#other 
round(mean(alsp4$drug_tot17.5, na.rm=T),1)
round(sd(alsp4$drug_tot17.5, na.rm=T),1)

round(mean(alsp4$drug_tot20, na.rm=T),1)
round(sd(alsp4$drug_tot20, na.rm=T),1)

round(mean(alsp4$drug_tot22, na.rm=T),1)
round(sd(alsp4$drug_tot22, na.rm=T),1)


