# Example of the Lavaan syntax used for the single-PGS and multi-PGSs TSO models of the common liability factor (i.e. ‘eta’).  

setwd("~/OneDrive/Rotation 2")

library("lavaan")

#Single-PGS TSO model 

model<- ' #state factors;

eta1 =~ NA*FT_total17.5_s + (lambda1)*FT_total17.5_s + (lambda2)*audit_tot17.5_s + (lambda3)*cast_tot17.5_s + (lambda4)*drug_tot17.5_s 
eta2 =~ NA*FT_total20_s + (lambda1)*FT_total20_s + (lambda2)*audit_tot20_s + (lambda3)*cast_tot20_s + (lambda4)*drug_tot20_s
eta3 =~ NA*FT_total22_s + (lambda1)*FT_total22_s + (lambda2)*audit_tot22_s + (lambda3)*cast_tot22_s + (lambda4)*drug_tot22_s

#occasion factors
etao1 =~ 1*eta1
etao2 =~ 1*eta2
etao3 =~ 1*eta3

#trait factor
eta =~ 1*eta1 + 1*eta2 + 1*eta3 
eta1 ~~ 0*eta1
eta2 ~~ 0*eta2 
eta3 ~~ 0*eta3

#covariances
eta ~~ 0*etao1

#method factors with effects coding;
etam1 =~ NA*FT_total17.5_s + (lambda5)*FT_total17.5_s + (lambda6)*FT_total20_s + (lambda7)*FT_total22_s 
etam2 =~ NA*audit_tot17.5_s + (lambda8)*audit_tot17.5_s  + (lambda9)*audit_tot20_s + (lambda10)*audit_tot22_s 
etam3 =~ NA*cast_tot17.5_s + (lambda11)*cast_tot17.5_s  + (lambda12)*cast_tot20_s + (lambda13)*cast_tot22_s 
etam4 =~ NA*drug_tot17.5_s + (lambda14)*drug_tot17.5_s  + (lambda15)*drug_tot20_s + (lambda16)*drug_tot22_ 

eta ~~ 0*etam1 + 0*etam2 + 0*etam3 + 0*etam4 +eta
etam1 ~~ 0*etao1 + 0*etao2 + 0*etao3 + 0*etam2 + 0*etam3 + 0*etam4
etam2 ~~ 0*etao1 + 0*etao2 + 0*etao3 + 0*etam3 + 0*etam4
etam3 ~~ 0*etao1 + 0*etao2 + 0*etao3 + 0*etam4
etam4 ~~ 0*etao1 + 0*etao2 + 0*etao3

#complex constraints for effects coding identification;
#Model constraint:
lambda1 == 4 - lambda2 - lambda3 - lambda4
lambda5 == 3 - lambda6 - lambda7 
lambda8 == 3 - lambda9 - lambda10 
lambda11 == 3 - lambda12 - lambda13 
lambda14 == 3 - lambda15 - lambda16

#single-PGS regression 
eta ~  PGS + sex + pca1 + pca2 + pca3 + pca4 + pca5 + pca6 + pca7 + pca8 + pca9 + pca10
'
fitmodel<- sem(model, data=alsp3, information = "expected", missing = "ML", estimator = "MLR")
summary(fitmodel, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE, ci = TRUE)


#Multi-PGSs TSO model 

model<- ' #state factors;

eta1 =~ NA*FT_total17.5_s + (lambda1)*FT_total17.5_s + (lambda2)*audit_tot17.5_s + (lambda3)*cast_tot17.5_s + (lambda4)*drug_tot17.5_s 
eta2 =~ NA*FT_total20_s + (lambda1)*FT_total20_s + (lambda2)*audit_tot20_s + (lambda3)*cast_tot20_s + (lambda4)*drug_tot20_s
eta3 =~ NA*FT_total22_s + (lambda1)*FT_total22_s + (lambda2)*audit_tot22_s + (lambda3)*cast_tot22_s + (lambda4)*drug_tot22_s

#occasion factors
etao1 =~ 1*eta1
etao2 =~ 1*eta2
etao3 =~ 1*eta3

#trait factor
eta =~ 1*eta1 + 1*eta2 + 1*eta3 
eta1 ~~ 0*eta1
eta2 ~~ 0*eta2 
eta3 ~~ 0*eta3

#covariances
eta ~~ 0*etao1

#method factors with effects coding;
etam1 =~ NA*FT_total17.5_s + (lambda5)*FT_total17.5_s + (lambda6)*FT_total20_s + (lambda7)*FT_total22_s 
etam2 =~ NA*audit_tot17.5_s + (lambda8)*audit_tot17.5_s  + (lambda9)*audit_tot20_s + (lambda10)*audit_tot22_s 
etam3 =~ NA*cast_tot17.5_s + (lambda11)*cast_tot17.5_s  + (lambda12)*cast_tot20_s + (lambda13)*cast_tot22_s 
etam4 =~ NA*drug_tot17.5_s + (lambda14)*drug_tot17.5_s  + (lambda15)*drug_tot20_s + (lambda16)*drug_tot22_s 

eta ~~ 0*etam1 + 0*etam2 + 0*etam3 + 0*etam4 +eta
etam1 ~~ 0*etao1 + 0*etao2 + 0*etao3 + 0*etam2 + 0*etam3 + 0*etam4
etam2 ~~ 0*etao1 + 0*etao2 + 0*etao3 + 0*etam3 + 0*etam4
etam3 ~~ 0*etao1 + 0*etao2 + 0*etao3 + 0*etam4
etam4 ~~ 0*etao1 + 0*etao2 + 0*etao3

#complex constraints for effects coding identification;
#Model constraint:
lambda1 == 4 - lambda2 - lambda3 - lambda4
lambda5 == 3 - lambda6 - lambda7 
lambda8 == 3 - lambda9 - lambda10 
lambda11 == 3 - lambda12 - lambda13 
lambda14 == 3 - lambda15 - lambda16

#multi-PGSs regression 
eta ~ PGS1 + PGS2 + PGS3 + PGS4 + PGS5 + sex + pca1 + pca2 + pca3 + pca4 + pca5 + pca6 + pca7 + pca8 + pca9 + pca10
'
fitmodel<- sem(model, data=alsp3, information = "expected", missing = "ML", estimator = "MLR")
summary(fitmodel, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE, ci = TRUE)

