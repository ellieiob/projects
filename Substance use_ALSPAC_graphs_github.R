#################################################################################################################################
###################################################### TSO Plots ###############################################################
##############################################################################################################################
setwd("C:/Users/rmjleio/OneDrive/Rotation 2")

load("alsp4")
install.packages("ggplot2")
library("ggplot2")
alsp3=alsp4

### install bbc library ### 
#https://bbc.github.io/rcookbook/#how_to_create_bbc_style_graphics
install.packages("devtools")
library(devtools)
devtools::install_github('bbc/bbplot')
library("bbc/bbplot")

setwd("C:/Users/rmjleio/OneDrive/Rotation 2/Figure3 09sep")

##read files(NB: column name of files must be equal ~ 
##Phenotype_names, Coefficient, Standard.Error,p.value, Beta, lower_CI, upper_CI
SU_single<- read.csv("Risk_SA_single_09sep.csv")
SU_multi<- read.csv("Risk_SA_multi_09sep.csv")

#create new column id in both 
SU_multi$Group="Multi" 
SU_single$Group="Simple" 


#bind the two dfs
allModelFrame <- data.frame(rbind(SU_single, SU_multi)) 

## create new variables
allModelFrame$Phenotype_names_renamed= allModelFrame$Phenotype_names
SU_single$Phenotype_names_renamed=SU_single$Phenotype_names


## order variables corresponding to the size of beta // convert to factor or numerical
allModelFrame$Coefficient_num=as.numeric(as.character(allModelFrame$Coefficient))
allModelFrame$Group_fac=as.factor(allModelFrame$Group)
allModelFrame$Phenotype_names_renamed_ordered= factor(allModelFrame$Phenotype_names_renamed, levels = SU_single$Phenotype_names_renamed[order(allModelFrame$Coefficient_num)])
allModelFrame$upper_CI_num=as.numeric(allModelFrame$upper_CI)
allModelFrame$lower_CI_num=as.numeric(allModelFrame$lower_CI)

## rename variables that are significant based on corrected pvalue ### or do it in CSV file before reading it in R 
#allModelFrame$Phenotype_names_renamed_ordered= revalue(allModelFrame$Phenotype_names_renamed_ordered, 
                                                       #c("Depression Diagnosis"="Depression Diagnosis (*)", 
                                                        #"Extraversion"="Extraversion (*)",
                                                        # "Risk Taking"="Risk Taking (*)"))

# Plot results
allModelFrame$Group_name[allModelFrame$Group=="Multi"]="Multi-PGS regression"
allModelFrame$Group_name[allModelFrame$Group=="Simple"]="Single-PGS regression"

new_plot <- ggplot(allModelFrame, aes(shape= Group_name,col=Group_name)) +
  scale_color_manual(values = c("black", "purple")) 

new_plot <- new_plot + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2)
new_plot <- new_plot + geom_linerange(aes(x = Phenotype_names_renamed_ordered, ymin = lower_CI_num,
                                          ymax = upper_CI_num),
                                      lwd = 1, position = position_dodge(width = 1/2))
new_plot <- new_plot + geom_pointrange(aes(x = Phenotype_names_renamed_ordered, y = Coefficient_num, ymin = lower_CI_num,
                                           ymax = upper_CI_num),
                                       lwd = 1/2, position = position_dodge(width = 1/2))
new_plot <- new_plot  +   coord_flip() +
  theme(legend.position="top") +
  labs(title = "", x = "", y = "", color = c(""))  +   scale_shape_discrete(name  ="")

new_plot <- new_plot + theme(axis.text.x = element_text(size=12),
                             axis.text.y = element_text(size=12))
new_plot= new_plot + scale_y_reverse() +
  scale_y_continuous(limits = c(-0.21, 0.37))
new_plot1<- new_plot + theme_minimal() + theme(legend.position="bottom") #+ ggtitle("Common Substance Abuse") 

print(new_plot1)

### Nicotine ###
##read files(NB: column name of files must be equal ~ 
##Phenotype_names, Coefficient, Standard.Error,p.value, Beta, lower_CI, upper_CI
Nicotine_single<- read.csv("Risk_Nicotine_single_09sep.csv")
Nicotine_multi<- read.csv("Risk_Nicotine_multi_09sep.csv")

#create new column id in both 
Nicotine_multi$Group="Multi" 
Nicotine_single$Group="Simple" 

#bind the two dfs
allModelFrame <- data.frame(rbind(Nicotine_single, Nicotine_multi)) 

## create new variables
allModelFrame$Phenotype_names_renamed= allModelFrame$Phenotype_names
Nicotine_single$Phenotype_names_renamed=Nicotine_single$Phenotype_names


## order variables corresponding to the size of beta // convert to factor or numerical
allModelFrame$Coefficient_num=as.numeric(as.character(allModelFrame$Coefficient))
allModelFrame$Group_fac=as.factor(allModelFrame$Group)
allModelFrame$Phenotype_names_renamed_ordered= factor(allModelFrame$Phenotype_names_renamed, levels = Nicotine_single$Phenotype_names_renamed[order(allModelFrame$Coefficient_num)])
allModelFrame$upper_CI_num=as.numeric(allModelFrame$upper_CI)
allModelFrame$lower_CI_num=as.numeric(allModelFrame$lower_CI)

## OPTIONAL: rename variables that are significant based on corrected pvalue
#allModelFrame$Phenotype_names_renamed_ordered= revalue(allModelFrame$Phenotype_names_renamed_ordered, 
                                                       #c("Educational Attainment"="Education (*)",
                                                         #"Depression Diagnosis"="Depression Diagnosis (*)", 
                                                         #"ADHD"="ADHD (*)",
                                                         #"BMI"="BMI (*)"))

# Plot results
allModelFrame$Group_name[allModelFrame$Group=="Multi"]="Multi-PGS regression"
allModelFrame$Group_name[allModelFrame$Group=="Simple"]="Single-PGS regression"

new_plot <- ggplot(allModelFrame, aes(shape= Group_name,col=Group_name)) +
  scale_color_manual(values = c("black", "darkorange")) 

new_plot <- new_plot + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2)
new_plot <- new_plot + geom_linerange(aes(x = Phenotype_names_renamed_ordered, ymin = lower_CI_num,
                                          ymax = upper_CI_num),
                                      lwd = 1, position = position_dodge(width = 1/2))
new_plot <- new_plot + geom_pointrange(aes(x = Phenotype_names_renamed_ordered, y = Coefficient_num, ymin = lower_CI_num,
                                           ymax = upper_CI_num),
                                       lwd = 1/2, position = position_dodge(width = 1/2))

new_plot <- new_plot  +   coord_flip() +
  theme(legend.position="top") +
  labs(title = "", x = "", y = "", color = c(""))  +   scale_shape_discrete(name  ="")

new_plot <- new_plot + theme(axis.text.x = element_text(size=12),
                             axis.text.y = element_text(size=12))
new_plot= new_plot + scale_y_reverse() +
  scale_y_continuous(limits = c(-0.21, 0.37))

new_plot2<- new_plot + theme_minimal() + theme(legend.position="bottom") #+ggtitle("Cigarette Abuse") 

print(new_plot2)

#### Alcohol ####
##read files(NB: column name of files must be equal ~ 
##Phenotype_names, Coefficient, Standard.Error,p.value, Beta, lower_CI, upper_CI
Alcohol_single<- read.csv("Risk_Alcohol_single_09sep.csv")
Alcohol_multi<- read.csv("Risk_Alcohol_multi_09sep.csv")
table(Alcohol_single$Phenotype_names)
table(Alcohol_multi$Phenotype_names)

#create new column id in both 
Alcohol_multi$Group="Multi" 
Alcohol_single$Group="Simple" 


#bind the two dfs
allModelFrame <- data.frame(rbind(Alcohol_single, Alcohol_multi)) 

## create new variables
allModelFrame$Phenotype_names_renamed= allModelFrame$Phenotype_names
Alcohol_single$Phenotype_names_renamed=Alcohol_single$Phenotype_names


## order variables corresponding to the size of beta // convert to factor or numerical
allModelFrame$Coefficient_num=as.numeric(as.character(allModelFrame$Coefficient))
allModelFrame$Group_fac=as.factor(allModelFrame$Group)
allModelFrame$Phenotype_names_renamed_ordered= factor(allModelFrame$Phenotype_names_renamed, levels = Alcohol_single$Phenotype_names_renamed[order(allModelFrame$Coefficient_num)])
allModelFrame$upper_CI_num=as.numeric(allModelFrame$upper_CI)
allModelFrame$lower_CI_num=as.numeric(allModelFrame$lower_CI)

# Plot results
allModelFrame$Group_name[allModelFrame$Group=="Multi"]="Multi-PGS regression"
allModelFrame$Group_name[allModelFrame$Group=="Simple"]="Single-PGS regression"

new_plot <- ggplot(allModelFrame, aes(shape= Group_name,col=Group_name)) +
  scale_color_manual(values = c("black", "blue")) 

new_plot <- new_plot + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2)
new_plot <- new_plot + geom_linerange(aes(x = Phenotype_names_renamed_ordered, ymin = lower_CI_num,
                                          ymax = upper_CI_num),
                                      lwd = 1, position = position_dodge(width = 1/2))
new_plot <- new_plot + geom_pointrange(aes(x = Phenotype_names_renamed_ordered, y = Coefficient_num, ymin = lower_CI_num,
                                           ymax = upper_CI_num),
                                       lwd = 1/2, position = position_dodge(width = 1/2))

new_plot <- new_plot  +   coord_flip() +
  theme(legend.position="top") +
  labs(title = "", x = "", y = "", color = c(""))  +   scale_shape_discrete(name  ="")

new_plot <- new_plot + theme(axis.text.x = element_text(size=12),
                             axis.text.y = element_text(size=12))
new_plot= new_plot + scale_y_reverse() +
  scale_y_continuous(limits = c(-0.21, 0.37))

new_plot3<- new_plot  + theme_minimal() + theme(legend.position="bottom") #+ ggtitle("Alcohol Abuse") 

print(new_plot3)

#### Cannabis ###
##read files(NB: column name of files must be equal ~ 
##Phenotype_names, Coefficient, Standard.Error,p.value, Beta, lower_CI, upper_CI
Cannabis_single<- read.csv("Risk_Cannabis_single_09sep.csv")
table(Cannabis_single$Phenotype_names)

#create new column id 
Cannabis_single$Group="Simple" 

##rename dataframe 
allModelFrame<- Cannabis_single

## create new variables
allModelFrame$Phenotype_names_renamed= allModelFrame$Phenotype_names
Cannabis_single$Phenotype_names_renamed=Cannabis_single$Phenotype_names


## order variables corresponding to the size of beta // convert to factor or numerical
allModelFrame$Coefficient_num=as.numeric(as.character(allModelFrame$Coefficient))
allModelFrame$Group_fac=as.factor(allModelFrame$Group)
allModelFrame$Phenotype_names_renamed_ordered= factor(allModelFrame$Phenotype_names_renamed, levels = Cannabis_single$Phenotype_names_renamed[order(allModelFrame$Coefficient_num)])
allModelFrame$upper_CI_num=as.numeric(allModelFrame$upper_CI)
allModelFrame$lower_CI_num=as.numeric(allModelFrame$lower_CI)

# Plot results
allModelFrame$Group_name[allModelFrame$Group=="Simple"]="Single-PGS regression"

new_plot <- ggplot(allModelFrame, aes(shape= Group_name,col=Group_name)) +
  scale_color_manual(values = c("darkgreen", "black")) 
  

new_plot <- new_plot + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2)
new_plot <- new_plot + geom_linerange(aes(x = Phenotype_names_renamed_ordered, ymin = lower_CI_num,
                                          ymax = upper_CI_num),
                                      lwd = 1, position = position_dodge(width = 1/2))
new_plot <- new_plot + geom_pointrange(aes(x = Phenotype_names_renamed_ordered, y = Coefficient_num, ymin = lower_CI_num,
                                           ymax = upper_CI_num),
                                       lwd = 1/2, position = position_dodge(width = 1/2), shape = 17)

new_plot <- new_plot  +   coord_flip() +
  theme(legend.position="top") +
  labs(title = "", x = "", y = "", color = c(""))  +   scale_shape_discrete(name  ="")

new_plot <- new_plot + theme(axis.text.x = element_text(size=12),
                             axis.text.y = element_text(size=12))
new_plot= new_plot + scale_y_reverse() +
  scale_y_continuous(limits = c(-0.21, 0.37))

new_plot4<- new_plot + theme_minimal() + theme(legend.position="bottom") #+ggtitle("Cannabis Abuse") 

print(new_plot4)

### Other drugs 
##read files(NB: column name of files must be equal ~ 
##Phenotype_names, Coefficient, Standard.Error,p.value, Beta, lower_CI, upper_CI
Drug_single<- read.csv("Risk_Other_single_09sep.csv")
Drug_multi<- read.csv("Risk_Other_multi_09sep.csv")
table(Drug_single$Phenotype_names)
table(Drug_multi$Phenotype_names)

#create new column id in both 
Drug_multi$Group="Multi" 
Drug_single$Group="Simple" 


#bind the two dfs
allModelFrame <- data.frame(rbind(Drug_single, Drug_multi)) 

## create new variables
allModelFrame$Phenotype_names_renamed= allModelFrame$Phenotype_names
Drug_single$Phenotype_names_renamed=Drug_single$Phenotype_names


## order variables corresponding to the size of beta // convert to factor or numerical
allModelFrame$Coefficient_num=as.numeric(as.character(allModelFrame$Coefficient))
allModelFrame$Group_fac=as.factor(allModelFrame$Group)
allModelFrame$Phenotype_names_renamed_ordered= factor(allModelFrame$Phenotype_names_renamed, levels = Drug_single$Phenotype_names_renamed[order(allModelFrame$Coefficient_num)])
allModelFrame$upper_CI_num=as.numeric(allModelFrame$upper_CI)
allModelFrame$lower_CI_num=as.numeric(allModelFrame$lower_CI)

# Plot results
allModelFrame$Group_name[allModelFrame$Group=="Multi"]="Multi-PGS regression"
allModelFrame$Group_name[allModelFrame$Group=="Simple"]="Single-PGS regression"

new_plot <- ggplot(allModelFrame, aes(shape= Group_name,col=Group_name)) +
  scale_color_manual(values = c("black", "red")) 
  

new_plot <- new_plot + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2)
new_plot <- new_plot + geom_linerange(aes(x = Phenotype_names_renamed_ordered, ymin = lower_CI_num,
                                          ymax = upper_CI_num),
                                      lwd = 1, position = position_dodge(width = 1/2))
new_plot <- new_plot + geom_pointrange(aes(x = Phenotype_names_renamed_ordered, y = Coefficient_num, ymin = lower_CI_num,
                                           ymax = upper_CI_num),
                                       lwd = 1/2, position = position_dodge(width = 1/2))

new_plot <- new_plot  +   coord_flip() +
  theme(legend.position="top") +
  labs(title = "", x = "", y = "", color = c(""))  +   scale_shape_discrete(name  ="")

new_plot <- new_plot + theme(axis.text.x = element_text(size=12),
                             axis.text.y = element_text(size=12))
new_plot= new_plot + scale_y_reverse() +
  scale_y_continuous(limits = c(-0.21, 0.37))

new_plot5<- new_plot + theme_minimal() + theme(legend.position="bottom") #+ ggtitle("Other Substance Abuse") 

print(new_plot5)


#2) substance abuse ##
## common SA ##

SU_single<- read.csv("SU_SA_single_09sep.csv")
SU_multi<- read.csv("SU_SA_multi_09sep.csv")

#create new column id in both 
SU_multi$Group="Multi" 
SU_single$Group="Simple" 


#bind the two dfs
allModelFrame <- data.frame(rbind(SU_single, SU_multi)) 

## create new variables
allModelFrame$Phenotype_names_renamed= allModelFrame$Phenotype_names
SU_single$Phenotype_names_renamed=SU_single$Phenotype_names


## order variables corresponding to the size of beta // convert to factor or numerical
allModelFrame$Coefficient_num=as.numeric(as.character(allModelFrame$Coefficient))
allModelFrame$Group_fac=as.factor(allModelFrame$Group)
allModelFrame$Phenotype_names_renamed_ordered= factor(allModelFrame$Phenotype_names_renamed, levels = SU_single$Phenotype_names_renamed[order(allModelFrame$Coefficient_num)])
allModelFrame$upper_CI_num=as.numeric(allModelFrame$upper_CI)
allModelFrame$lower_CI_num=as.numeric(allModelFrame$lower_CI)

## rename variables that are significant based on corrected pvalue ### or do it in CSV file before reading it in R 
#allModelFrame$Phenotype_names_renamed_ordered= revalue(allModelFrame$Phenotype_names_renamed_ordered, 
#c("Depression Diagnosis"="Depression Diagnosis (*)", 
#"Extraversion"="Extraversion (*)",
# "Risk Taking"="Risk Taking (*)"))

# Plot results
allModelFrame$Group_name[allModelFrame$Group=="Multi"]="Multi-PGS regression"
allModelFrame$Group_name[allModelFrame$Group=="Simple"]="Single-PGS regression"

new_plot <- ggplot(allModelFrame, aes(shape= Group_name,col=Group_name)) +
  scale_color_manual(values = c("black", "purple")) 

new_plot <- new_plot + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2)
new_plot <- new_plot + geom_linerange(aes(x = Phenotype_names_renamed_ordered, ymin = lower_CI_num,
                                          ymax = upper_CI_num),
                                      lwd = 1, position = position_dodge(width = 1/2))
new_plot <- new_plot + geom_pointrange(aes(x = Phenotype_names_renamed_ordered, y = Coefficient_num, ymin = lower_CI_num,
                                           ymax = upper_CI_num),
                                       lwd = 1/2, position = position_dodge(width = 1/2))
new_plot <- new_plot  +   coord_flip() +
  theme(legend.position="top") +
  labs(title = "", x = "", y = "", color = c(""))  +   scale_shape_discrete(name  ="")

new_plot <- new_plot + theme(axis.text.x = element_text(size=12),
                             axis.text.y = element_text(size=12))
new_plot= new_plot + scale_y_reverse() +
  scale_y_continuous(limits = c(-0.21, 0.37))
new_plot6<- new_plot+ theme_minimal() + theme(legend.position="bottom") + ggtitle("Common substance use problems") + theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold", margin=margin(0,0,30,0))) +theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())


print(new_plot6)

##nicotine

Nicotine_single<- read.csv("SU_Nicotine_single_09sep.csv")
Nicotine_multi<- read.csv("SU_Nicotine_multi_09sep.csv")

#create new column id in both 
Nicotine_multi$Group="Multi" 
Nicotine_single$Group="Simple" 

#bind the two dfs
allModelFrame <- data.frame(rbind(Nicotine_single, Nicotine_multi)) 

## create new variables
allModelFrame$Phenotype_names_renamed= allModelFrame$Phenotype_names
Nicotine_single$Phenotype_names_renamed=Nicotine_single$Phenotype_names


## order variables corresponding to the size of beta // convert to factor or numerical
allModelFrame$Coefficient_num=as.numeric(as.character(allModelFrame$Coefficient))
allModelFrame$Group_fac=as.factor(allModelFrame$Group)
allModelFrame$Phenotype_names_renamed_ordered= factor(allModelFrame$Phenotype_names_renamed, levels = Nicotine_single$Phenotype_names_renamed[order(allModelFrame$Coefficient_num)])
allModelFrame$upper_CI_num=as.numeric(allModelFrame$upper_CI)
allModelFrame$lower_CI_num=as.numeric(allModelFrame$lower_CI)

## OPTIONAL: rename variables that are significant based on corrected pvalue
#allModelFrame$Phenotype_names_renamed_ordered= revalue(allModelFrame$Phenotype_names_renamed_ordered, 
#c("Educational Attainment"="Education (*)",
#"Depression Diagnosis"="Depression Diagnosis (*)", 
#"ADHD"="ADHD (*)",
#"BMI"="BMI (*)"))

# Plot results
allModelFrame$Group_name[allModelFrame$Group=="Multi"]="Multi-PGS regression"
allModelFrame$Group_name[allModelFrame$Group=="Simple"]="Single-PGS regression"

new_plot <- ggplot(allModelFrame, aes(shape= Group_name,col=Group_name)) +
  scale_color_manual(values = c("black", "darkorange")) 

new_plot <- new_plot + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2)
new_plot <- new_plot + geom_linerange(aes(x = Phenotype_names_renamed_ordered, ymin = lower_CI_num,
                                          ymax = upper_CI_num),
                                      lwd = 1, position = position_dodge(width = 1/2))
new_plot <- new_plot + geom_pointrange(aes(x = Phenotype_names_renamed_ordered, y = Coefficient_num, ymin = lower_CI_num,
                                           ymax = upper_CI_num),
                                       lwd = 1/2, position = position_dodge(width = 1/2))

new_plot <- new_plot  +   coord_flip() +
  theme(legend.position="top") +
  labs(title = "", x = "", y = "", color = c(""))  +   scale_shape_discrete(name  ="")

new_plot <- new_plot + theme(axis.text.x = element_text(size=12),
                             axis.text.y = element_text(size=12))
new_plot= new_plot + scale_y_reverse() +
  scale_y_continuous(limits = c(-0.21, 0.37))

new_plot7<- new_plot + theme_minimal() + theme(legend.position="bottom") +ggtitle("Cigarette use problems") + theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold", margin=margin(0,0,30,0)))  +theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())


print(new_plot7)

#### Alcohol ####
##read files(NB: column name of files must be equal ~ 
##Phenotype_names, Coefficient, Standard.Error,p.value, Beta, lower_CI, upper_CI
Alcohol_single<- read.csv("SU_Alcohol_single_09sep.csv")
Alcohol_multi<- read.csv("SU_Alcohol_multi_09sep.csv")
table(Alcohol_single$Phenotype_names)
table(Alcohol_multi$Phenotype_names)

#create new column id in both 
Alcohol_multi$Group="Multi" 
Alcohol_single$Group="Simple" 


#bind the two dfs
allModelFrame <- data.frame(rbind(Alcohol_single, Alcohol_multi)) 

## create new variables
allModelFrame$Phenotype_names_renamed= allModelFrame$Phenotype_names
Alcohol_single$Phenotype_names_renamed=Alcohol_single$Phenotype_names


## order variables corresponding to the size of beta // convert to factor or numerical
allModelFrame$Coefficient_num=as.numeric(as.character(allModelFrame$Coefficient))
allModelFrame$Group_fac=as.factor(allModelFrame$Group)
allModelFrame$Phenotype_names_renamed_ordered= factor(allModelFrame$Phenotype_names_renamed, levels = Alcohol_single$Phenotype_names_renamed[order(allModelFrame$Coefficient_num)])
allModelFrame$upper_CI_num=as.numeric(allModelFrame$upper_CI)
allModelFrame$lower_CI_num=as.numeric(allModelFrame$lower_CI)

# Plot results
allModelFrame$Group_name[allModelFrame$Group=="Multi"]="Multi-PGS regression"
allModelFrame$Group_name[allModelFrame$Group=="Simple"]="Single-PGS regression"

new_plot <- ggplot(allModelFrame, aes(shape= Group_name,col=Group_name)) +
  scale_color_manual(values = c("black", "blue")) 

new_plot <- new_plot + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2)
new_plot <- new_plot + geom_linerange(aes(x = Phenotype_names_renamed_ordered, ymin = lower_CI_num,
                                          ymax = upper_CI_num),
                                      lwd = 1, position = position_dodge(width = 1/2))
new_plot <- new_plot + geom_pointrange(aes(x = Phenotype_names_renamed_ordered, y = Coefficient_num, ymin = lower_CI_num,
                                           ymax = upper_CI_num),
                                       lwd = 1/2, position = position_dodge(width = 1/2))

new_plot <- new_plot  +   coord_flip() +
  theme(legend.position="top") +
  labs(title = "", x = "", y = "", color = c(""))  +   scale_shape_discrete(name  ="")

new_plot <- new_plot + theme(axis.text.x = element_text(size=12),
                             axis.text.y = element_text(size=12))
new_plot= new_plot + scale_y_reverse() +
  scale_y_continuous(limits = c(-0.21, 0.38))

new_plot8<- new_plot  + theme_minimal() + theme(legend.position="bottom") + ggtitle("Alcohol use problems") + theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold", margin=margin(0,0,30,0)))  +theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())


print(new_plot8)

##cannabis 

Cannabis_single<- read.csv("SU_Cannabis_single_09sep.csv")
Cannabis_multi<- read.csv("SU_Cannabis_multi_09sep.csv")


#create new column id in both 
Cannabis_multi$Group="Multi" 
Cannabis_single$Group="Simple" 


#bind the two dfs
allModelFrame <- data.frame(rbind(Cannabis_single,Cannabis_multi)) 

allModelFrame$Phenotype_names_renamed= allModelFrame$Phenotype_names
Cannabis_single$Phenotype_names_renamed=Cannabis_single$Phenotype_names


## create new variables
allModelFrame$Coefficient_num=as.numeric(as.character(allModelFrame$Coefficient))
allModelFrame$Group_fac=as.factor(allModelFrame$Group)
allModelFrame$Phenotype_names_renamed_ordered= factor(allModelFrame$Phenotype_names_renamed, levels = Cannabis_single$Phenotype_names_renamed[order(allModelFrame$Coefficient_num)])
allModelFrame$upper_CI_num=as.numeric(allModelFrame$upper_CI)
allModelFrame$lower_CI_num=as.numeric(allModelFrame$lower_CI)

## order variables corresponding to the size of beta // convert to factor or numerical

allModelFrame$Phenotype_names_renamed_ordered= factor(allModelFrame$Phenotype_names_renamed, levels = Cannabis_single$Phenotype_names_renamed[order(allModelFrame$Coefficient)])
allModelFrame$upper_CI_num=as.numeric(allModelFrame$upper_CI)
allModelFrame$lower_CI_num=as.numeric(allModelFrame$lower_CI)

# Plot results
allModelFrame$Group_name[allModelFrame$Group=="Multi"]="Multi-PGS regression"
allModelFrame$Group_name[allModelFrame$Group=="Simple"]="Single-PGS regression"

new_plot <- ggplot(allModelFrame, aes(shape= Group_name,col=Group_name)) +
  scale_color_manual(values = c("black", "darkgreen")) 

new_plot <- new_plot + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2)
new_plot <- new_plot + geom_linerange(aes(x = Phenotype_names_renamed_ordered, ymin = lower_CI_num,
                                          ymax = upper_CI_num),
                                      lwd = 1, position = position_dodge(width = 1/2))
new_plot <- new_plot + geom_pointrange(aes(x = Phenotype_names_renamed_ordered, y = Coefficient_num, ymin = lower_CI_num,
                                           ymax = upper_CI_num),
                                       lwd = 1/2, position = position_dodge(width = 1/2))

new_plot <- new_plot  +   coord_flip() +
  theme(legend.position="top") +
  labs(title = "", x = "", y = "", color = c(""))  +   scale_shape_discrete(name  ="")

new_plot <- new_plot + theme(axis.text.x = element_text(size=12),
                             axis.text.y = element_text(size=12))
new_plot= new_plot + scale_y_reverse() +
  scale_y_continuous(limits = c(-0.21, 0.37))

new_plot9<- new_plot  + theme_minimal() + theme(legend.position="bottom") + ggtitle("Cannabis use problems") + theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold", margin=margin(0,0,30,0)))  +theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())


print(new_plot9)

### Other drugs 
##read files(NB: column name of files must be equal ~ 
##Phenotype_names, Coefficient, Standard.Error,p.value, Beta, lower_CI, upper_CI
Drug_single<- read.csv("SU_Other_single_09sep.csv")
Drug_multi<- read.csv("SU_Other_multi_09sep.csv")
table(Drug_single$Phenotype_names)
table(Drug_multi$Phenotype_names)

#create new column id in both 
Drug_multi$Group="Multi" 
Drug_single$Group="Simple" 


#bind the two dfs
allModelFrame <- data.frame(rbind(Drug_single, Drug_multi)) 

## create new variables
allModelFrame$Phenotype_names_renamed= allModelFrame$Phenotype_names
Drug_single$Phenotype_names_renamed=Drug_single$Phenotype_names


## order variables corresponding to the size of beta // convert to factor or numerical
allModelFrame$Coefficient_num=as.numeric(as.character(allModelFrame$Coefficient))
allModelFrame$Group_fac=as.factor(allModelFrame$Group)
allModelFrame$Phenotype_names_renamed_ordered= factor(allModelFrame$Phenotype_names_renamed, levels = Drug_single$Phenotype_names_renamed[order(allModelFrame$Coefficient_num)])
allModelFrame$upper_CI_num=as.numeric(allModelFrame$upper_CI)
allModelFrame$lower_CI_num=as.numeric(allModelFrame$lower_CI)

# Plot results
allModelFrame$Group_name[allModelFrame$Group=="Multi"]="Multi-PGS regression"
allModelFrame$Group_name[allModelFrame$Group=="Simple"]="Single-PGS regression"

new_plot <- ggplot(allModelFrame, aes(shape= Group_name,col=Group_name)) +
  scale_color_manual(values = c("black", "red")) 


new_plot <- new_plot + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2)
new_plot <- new_plot + geom_linerange(aes(x = Phenotype_names_renamed_ordered, ymin = lower_CI_num,
                                          ymax = upper_CI_num),
                                      lwd = 1, position = position_dodge(width = 1/2))
new_plot <- new_plot + geom_pointrange(aes(x = Phenotype_names_renamed_ordered, y = Coefficient_num, ymin = lower_CI_num,
                                           ymax = upper_CI_num),
                                       lwd = 1/2, position = position_dodge(width = 1/2))

new_plot <- new_plot  +   coord_flip() +
  theme(legend.position="top") +
  labs(title = "", x = "", y = "", color = c(""))  +   scale_shape_discrete(name  ="")

new_plot <- new_plot + theme(axis.text.x = element_text(size=12),
                             axis.text.y = element_text(size=12))
new_plot= new_plot + scale_y_reverse() +
  scale_y_continuous(limits = c(-0.21, 0.37))

new_plot10<- new_plot + theme_minimal() + theme(legend.position="bottom") + ggtitle("Other substance use problems") + theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold", margin=margin(0,0,30,0)))  +theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())


print(new_plot10)


#### combined plots ###
#package 1
install.packages("gridExtra")
library(gridExtra)
grid.arrange(new_plot2, new_plot3, new_plot4, new_plot5, ncol= 2)
?grid.arrange

grid.arrange(new_plot6, new_plot1, heights = c(0.9, 2), nrow=2)

#package 2
install.packages("ggpubr")
library(ggpubr)

common=ggarrange(new_plot6, new_plot1, align = "v",
          heights = c(1.5, 2),
          labels = c("A", "B"),
          nrow = 2)
print(common)

cigarette=ggarrange(new_plot7, new_plot2, align = "v",
                 heights = c(1.5, 2),
                 labels = c("A", "B"),
                 nrow = 2)
print(cigarette)

alcohol=ggarrange(new_plot8, new_plot3, align = "v",
                    heights = c(1.5, 2),
                    labels = c("A", "B"),
                    nrow = 2)
print(alcohol)

cannabis=ggarrange(new_plot9, new_plot4, align = "v",
                  heights = c(1.5, 2),
                  labels = c("A", "B"),
                  nrow = 2)
print(cannabis)

other=ggarrange(new_plot10, new_plot5, align = "v",
                   heights = c(1.5, 2),
                   labels = c("A", "B"),
                   nrow = 2)
print(other)

grid.arrange(common, cigarette, alcohol, cannabis, other, ncol= 2)

install.packages("cowplot")                      
library("cowplot")
ggdraw() +
  draw_plot(common, x = 0.25, y = .70, width = .5, height = .30) +
  draw_plot(cigarette, x = .0, y = .35, width = .5, height = .30) +
  draw_plot(alcohol, x = 0.5, y = .35, width = .5, height = 0.30) +
  draw_plot(cannabis, x = 0.0, y = 0.0, width = .5, height = 0.30) +
  draw_plot(other, x = 0.5, y = 0.0, width = .5, height = 0.30)

##nice ggplot
#http://zevross.com/blog/2014/08/04/beautiful-plotting-in-r-a-ggplot2-cheatsheet-3/#change-spacing-in-multi-line-text-lineheight