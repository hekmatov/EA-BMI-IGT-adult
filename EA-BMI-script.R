# Hekmat Alrouh: SEM model of BMI/EA intergenerational transmission with multiple (6) offspring per family (3male+3female)

rm(list=ls())
library(foreign)
library(lavaan)

#twindata is the dataframe used for the main analyses of the study
twindata <- read.spss("EA-BMI.sav",to.data.frame = TRUE,use.value.labels = FALSE)


#model1r is the full model with separate coefficients by offpsring gender (b3m/b3f, b4m/b4f, etc..)
#0_1,0_2,0_10 = extensions for the three male offpsring
#1_1,1_2,1_10 = extensions for the three female offspring
#0_31 = fathers, 1_41 = mothers

model1r<-'
BMI_0_1~b3m*EA_0_31+b4m*EA_1_41+b5m*BMI_0_31+b6m*BMI_1_41
BMI_0_2~b3m*EA_0_31+b4m*EA_1_41+b5m*BMI_0_31+b6m*BMI_1_41
BMI_0_10~b3m*EA_0_31+b4m*EA_1_41+b5m*BMI_0_31+b6m*BMI_1_41
BMI_1_1~b3f*EA_0_31+b4f*EA_1_41+b5f*BMI_0_31+b6f*BMI_1_41
BMI_1_2~b3f*EA_0_31+b4f*EA_1_41+b5f*BMI_0_31+b6f*BMI_1_41
BMI_1_10~b3f*EA_0_31+b4f*EA_1_41+b5f*BMI_0_31+b6f*BMI_1_41
EA_0_1~e3m*EA_0_31+e4m*EA_1_41+e5m*BMI_0_31+e6m*BMI_1_41
EA_0_2~e3m*EA_0_31+e4m*EA_1_41+e5m*BMI_0_31+e6m*BMI_1_41
EA_0_10~e3m*EA_0_31+e4m*EA_1_41+e5m*BMI_0_31+e6m*BMI_1_41
EA_1_1~e3f*EA_0_31+e4f*EA_1_41+e5f*BMI_0_31+e6f*BMI_1_41
EA_1_2~e3f*EA_0_31+e4f*EA_1_41+e5f*BMI_0_31+e6f*BMI_1_41
EA_1_10~e3f*EA_0_31+e4f*EA_1_41+e5f*BMI_0_31+e6f*BMI_1_41
EA_0_1~~c1m*BMI_0_1
EA_0_2~~c1m*BMI_0_2
EA_0_10~~c1m*BMI_0_10
EA_1_1~~c1f*BMI_1_1
EA_1_2~~c1f*BMI_1_2
EA_1_10~~c1f*BMI_1_10
EA_0_31~~c1mp*BMI_0_31
EA_1_41~~c1fp*BMI_1_41
EA_0_31~~c2*EA_1_41
BMI_0_31~~c3*BMI_1_41
EA_0_31~~c4*BMI_1_41
BMI_0_31~~c5*EA_1_41
BMI_0_1~~v1*BMI_0_1
BMI_0_2~~v1*BMI_0_2
BMI_0_10~~v1*BMI_0_10
BMI_1_1~~v2*BMI_1_1
BMI_1_2~~v2*BMI_1_2
BMI_1_10~~v2*BMI_1_10
EA_0_1~~v3*EA_0_1
EA_0_2~~v3*EA_0_2
EA_0_10~~v3*EA_0_10
EA_1_1~~v4*EA_1_1
EA_1_2~~v4*EA_1_2
EA_1_10~~v4*EA_1_10
'

fit1r<-sem(model1r,data=twindata, estimator="ML", missing="fiml.x")
summary(fit1r,fit.measures=TRUE,standardized=TRUE)
sink("output-genst-3m3f-m1r.txt")
print(summary(fit1r,fit.measures=TRUE,standardized=TRUE))
sink()

#model1s is used to generate observed covariances/correlations while constraining same gender offspring to have the same parameters
model1s<-'
BMI_0_1~~b3m*EA_0_31
BMI_0_1~~b4m*EA_1_41
BMI_0_1~~b5m*BMI_0_31
BMI_0_1~~b6m*BMI_1_41
BMI_0_2~~b3m*EA_0_31
BMI_0_2~~b4m*EA_1_41
BMI_0_2~~b5m*BMI_0_31
BMI_0_2~~b6m*BMI_1_41
BMI_0_10~~b3m*EA_0_31
BMI_0_10~~b4m*EA_1_41
BMI_0_10~~b5m*BMI_0_31
BMI_0_10~~b6m*BMI_1_41
BMI_1_1~~b3f*EA_0_31
BMI_1_1~~b4f*EA_1_41
BMI_1_1~~b5f*BMI_0_31
BMI_1_1~~b6f*BMI_1_41
BMI_1_2~~b3f*EA_0_31
BMI_1_2~~b4f*EA_1_41
BMI_1_2~~b5f*BMI_0_31
BMI_1_2~~b6f*BMI_1_41
BMI_1_10~~b3f*EA_0_31
BMI_1_10~~b4f*EA_1_41
BMI_1_10~~b5f*BMI_0_31
BMI_1_10~~b6f*BMI_1_41
EA_0_1~~e3m*EA_0_31
EA_0_1~~e4m*EA_1_41
EA_0_1~~e5m*BMI_0_31
EA_0_1~~e6m*BMI_1_41
EA_0_2~~e3m*EA_0_31
EA_0_2~~e4m*EA_1_41
EA_0_2~~e5m*BMI_0_31
EA_0_2~~e6m*BMI_1_41
EA_0_10~~e3m*EA_0_31
EA_0_10~~e4m*EA_1_41
EA_0_10~~e5m*BMI_0_31
EA_0_10~~e6m*BMI_1_41
EA_1_1~~e3f*EA_0_31
EA_1_1~~e4f*EA_1_41
EA_1_1~~e5f*BMI_0_31
EA_1_1~~e6f*BMI_1_41
EA_1_2~~e3f*EA_0_31
EA_1_2~~e4f*EA_1_41
EA_1_2~~e5f*BMI_0_31
EA_1_2~~e6f*BMI_1_41
EA_1_10~~e3f*EA_0_31
EA_1_10~~e4f*EA_1_41
EA_1_10~~e5f*BMI_0_31
EA_1_10~~e6f*BMI_1_41
EA_0_1~~c1m*BMI_0_1
EA_0_2~~c1m*BMI_0_2
EA_0_10~~c1m*BMI_0_10
EA_1_1~~c1f*BMI_1_1
EA_1_2~~c1f*BMI_1_2
EA_1_10~~c1f*BMI_1_10
EA_0_31~~c1mp*BMI_0_31
EA_1_41~~c1fp*BMI_1_41
EA_0_31~~c2*EA_1_41
BMI_0_31~~c3*BMI_1_41
EA_0_31~~c4*BMI_1_41
BMI_0_31~~c5*EA_1_41
BMI_0_1~~v1*BMI_0_1
BMI_0_2~~v1*BMI_0_2
BMI_0_10~~v1*BMI_0_10
BMI_1_1~~v2*BMI_1_1
BMI_1_2~~v2*BMI_1_2
BMI_1_10~~v2*BMI_1_10
EA_0_1~~v3*EA_0_1
EA_0_2~~v3*EA_0_2
EA_0_10~~v3*EA_0_10
EA_1_1~~v4*EA_1_1
EA_1_2~~v4*EA_1_2
EA_1_10~~v4*EA_1_10
'

fit1s<-sem(model1s,data=twindata, estimator="ML", missing="fiml.x")


#model1eallr is used for omnibus test of all equality constraints across genders of parent and offspring
model1eallr<-'
BMI_0_1~b3*EA_0_31+b4*EA_1_41+b5*BMI_0_31+b6*BMI_1_41
BMI_0_2~b3*EA_0_31+b4*EA_1_41+b5*BMI_0_31+b6*BMI_1_41
BMI_0_10~b3*EA_0_31+b4*EA_1_41+b5*BMI_0_31+b6*BMI_1_41
BMI_1_1~b3*EA_0_31+b4*EA_1_41+b5*BMI_0_31+b6*BMI_1_41
BMI_1_2~b3*EA_0_31+b4*EA_1_41+b5*BMI_0_31+b6*BMI_1_41
BMI_1_10~b3*EA_0_31+b4*EA_1_41+b5*BMI_0_31+b6*BMI_1_41
EA_0_1~e3*EA_0_31+e4*EA_1_41+e5*BMI_0_31+e6*BMI_1_41
EA_0_2~e3*EA_0_31+e4*EA_1_41+e5*BMI_0_31+e6*BMI_1_41
EA_0_10~e3*EA_0_31+e4*EA_1_41+e5*BMI_0_31+e6*BMI_1_41
EA_1_1~e3*EA_0_31+e4*EA_1_41+e5*BMI_0_31+e6*BMI_1_41
EA_1_2~e3*EA_0_31+e4*EA_1_41+e5*BMI_0_31+e6*BMI_1_41
EA_1_10~e3*EA_0_31+e4*EA_1_41+e5*BMI_0_31+e6*BMI_1_41
EA_0_1~~c1m*BMI_0_1
EA_0_2~~c1m*BMI_0_2
EA_0_10~~c1m*BMI_0_10
EA_1_1~~c1f*BMI_1_1
EA_1_2~~c1f*BMI_1_2
EA_1_10~~c1f*BMI_1_10
EA_0_31~~c1mp*BMI_0_31
EA_1_41~~c1fp*BMI_1_41
EA_0_31~~c2*EA_1_41
BMI_0_31~~c3*BMI_1_41
EA_0_31~~c4*BMI_1_41
BMI_0_31~~c5*EA_1_41
BMI_0_1~~v1*BMI_0_1
BMI_0_2~~v1*BMI_0_2
BMI_0_10~~v1*BMI_0_10
BMI_1_1~~v2*BMI_1_1
BMI_1_2~~v2*BMI_1_2
BMI_1_10~~v2*BMI_1_10
EA_0_1~~v3*EA_0_1
EA_0_2~~v3*EA_0_2
EA_0_10~~v3*EA_0_10
EA_1_1~~v4*EA_1_1
EA_1_2~~v4*EA_1_2
EA_1_10~~v4*EA_1_10
'

fit1eallr<-sem(model1eallr,data=twindata, estimator="ML", missing="fiml.x")

sink("output-genst-3m3f-m1eallr.txt")
print(summary(fit1eallr,fit.measures=TRUE,standardized=TRUE))
sink()

anova(fit1r,fit1eallr)

#model1ee3r: testing constraint e3m==e3f
model1ee3r<-'
BMI_0_1~b3m*EA_0_31+b4m*EA_1_41+b5m*BMI_0_31+b6m*BMI_1_41
BMI_0_2~b3m*EA_0_31+b4m*EA_1_41+b5m*BMI_0_31+b6m*BMI_1_41
BMI_0_10~b3m*EA_0_31+b4m*EA_1_41+b5m*BMI_0_31+b6m*BMI_1_41
BMI_1_1~b3f*EA_0_31+b4f*EA_1_41+b5f*BMI_0_31+b6f*BMI_1_41
BMI_1_2~b3f*EA_0_31+b4f*EA_1_41+b5f*BMI_0_31+b6f*BMI_1_41
BMI_1_10~b3f*EA_0_31+b4f*EA_1_41+b5f*BMI_0_31+b6f*BMI_1_41
EA_0_1~e3*EA_0_31+e4m*EA_1_41+e5m*BMI_0_31+e6m*BMI_1_41
EA_0_2~e3*EA_0_31+e4m*EA_1_41+e5m*BMI_0_31+e6m*BMI_1_41
EA_0_10~e3*EA_0_31+e4m*EA_1_41+e5m*BMI_0_31+e6m*BMI_1_41
EA_1_1~e3*EA_0_31+e4f*EA_1_41+e5f*BMI_0_31+e6f*BMI_1_41
EA_1_2~e3*EA_0_31+e4f*EA_1_41+e5f*BMI_0_31+e6f*BMI_1_41
EA_1_10~e3*EA_0_31+e4f*EA_1_41+e5f*BMI_0_31+e6f*BMI_1_41
EA_0_1~~c1m*BMI_0_1
EA_0_2~~c1m*BMI_0_2
EA_0_10~~c1m*BMI_0_10
EA_1_1~~c1f*BMI_1_1
EA_1_2~~c1f*BMI_1_2
EA_1_10~~c1f*BMI_1_10
EA_0_31~~c1mp*BMI_0_31
EA_1_41~~c1fp*BMI_1_41
EA_0_31~~c2*EA_1_41
BMI_0_31~~c3*BMI_1_41
EA_0_31~~c4*BMI_1_41
BMI_0_31~~c5*EA_1_41
BMI_0_1~~v1*BMI_0_1
BMI_0_2~~v1*BMI_0_2
BMI_0_10~~v1*BMI_0_10
BMI_1_1~~v2*BMI_1_1
BMI_1_2~~v2*BMI_1_2
BMI_1_10~~v2*BMI_1_10
EA_0_1~~v3*EA_0_1
EA_0_2~~v3*EA_0_2
EA_0_10~~v3*EA_0_10
EA_1_1~~v4*EA_1_1
EA_1_2~~v4*EA_1_2
EA_1_10~~v4*EA_1_10
'
fit1ee3r<-sem(model1ee3r,data=twindata, estimator="ML", missing="fiml.x")

summary(fit1ee3r,fit.measures=TRUE,standardized=TRUE)


#model1ee4r: testing constraint e4m==e4f
model1ee4r<-'
BMI_0_1~b3m*EA_0_31+b4m*EA_1_41+b5m*BMI_0_31+b6m*BMI_1_41
BMI_0_2~b3m*EA_0_31+b4m*EA_1_41+b5m*BMI_0_31+b6m*BMI_1_41
BMI_0_10~b3m*EA_0_31+b4m*EA_1_41+b5m*BMI_0_31+b6m*BMI_1_41
BMI_1_1~b3f*EA_0_31+b4f*EA_1_41+b5f*BMI_0_31+b6f*BMI_1_41
BMI_1_2~b3f*EA_0_31+b4f*EA_1_41+b5f*BMI_0_31+b6f*BMI_1_41
BMI_1_10~b3f*EA_0_31+b4f*EA_1_41+b5f*BMI_0_31+b6f*BMI_1_41
EA_0_1~e3m*EA_0_31+e4*EA_1_41+e5m*BMI_0_31+e6m*BMI_1_41
EA_0_2~e3m*EA_0_31+e4*EA_1_41+e5m*BMI_0_31+e6m*BMI_1_41
EA_0_10~e3m*EA_0_31+e4*EA_1_41+e5m*BMI_0_31+e6m*BMI_1_41
EA_1_1~e3f*EA_0_31+e4*EA_1_41+e5f*BMI_0_31+e6f*BMI_1_41
EA_1_2~e3f*EA_0_31+e4*EA_1_41+e5f*BMI_0_31+e6f*BMI_1_41
EA_1_10~e3f*EA_0_31+e4*EA_1_41+e5f*BMI_0_31+e6f*BMI_1_41
EA_0_1~~c1m*BMI_0_1
EA_0_2~~c1m*BMI_0_2
EA_0_10~~c1m*BMI_0_10
EA_1_1~~c1f*BMI_1_1
EA_1_2~~c1f*BMI_1_2
EA_1_10~~c1f*BMI_1_10
EA_0_31~~c1mp*BMI_0_31
EA_1_41~~c1fp*BMI_1_41
EA_0_31~~c2*EA_1_41
BMI_0_31~~c3*BMI_1_41
EA_0_31~~c4*BMI_1_41
BMI_0_31~~c5*EA_1_41
BMI_0_1~~v1*BMI_0_1
BMI_0_2~~v1*BMI_0_2
BMI_0_10~~v1*BMI_0_10
BMI_1_1~~v2*BMI_1_1
BMI_1_2~~v2*BMI_1_2
BMI_1_10~~v2*BMI_1_10
EA_0_1~~v3*EA_0_1
EA_0_2~~v3*EA_0_2
EA_0_10~~v3*EA_0_10
EA_1_1~~v4*EA_1_1
EA_1_2~~v4*EA_1_2
EA_1_10~~v4*EA_1_10
'

fit1ee4r<-sem(model1ee4r,data=twindata, estimator="ML", missing="fiml.x")

summary(fit1ee4r,fit.measures=TRUE,standardized=TRUE)


model1ee5r<-'
BMI_0_1~b3m*EA_0_31+b4m*EA_1_41+b5m*BMI_0_31+b6m*BMI_1_41
BMI_0_2~b3m*EA_0_31+b4m*EA_1_41+b5m*BMI_0_31+b6m*BMI_1_41
BMI_0_10~b3m*EA_0_31+b4m*EA_1_41+b5m*BMI_0_31+b6m*BMI_1_41
BMI_1_1~b3f*EA_0_31+b4f*EA_1_41+b5f*BMI_0_31+b6f*BMI_1_41
BMI_1_2~b3f*EA_0_31+b4f*EA_1_41+b5f*BMI_0_31+b6f*BMI_1_41
BMI_1_10~b3f*EA_0_31+b4f*EA_1_41+b5f*BMI_0_31+b6f*BMI_1_41
EA_0_1~e3m*EA_0_31+e4m*EA_1_41+e5*BMI_0_31+e6m*BMI_1_41
EA_0_2~e3m*EA_0_31+e4m*EA_1_41+e5*BMI_0_31+e6m*BMI_1_41
EA_0_10~e3m*EA_0_31+e4m*EA_1_41+e5*BMI_0_31+e6m*BMI_1_41
EA_1_1~e3f*EA_0_31+e4f*EA_1_41+e5*BMI_0_31+e6f*BMI_1_41
EA_1_2~e3f*EA_0_31+e4f*EA_1_41+e5*BMI_0_31+e6f*BMI_1_41
EA_1_10~e3f*EA_0_31+e4f*EA_1_41+e5*BMI_0_31+e6f*BMI_1_41
EA_0_1~~c1m*BMI_0_1
EA_0_2~~c1m*BMI_0_2
EA_0_10~~c1m*BMI_0_10
EA_1_1~~c1f*BMI_1_1
EA_1_2~~c1f*BMI_1_2
EA_1_10~~c1f*BMI_1_10
EA_0_31~~c1mp*BMI_0_31
EA_1_41~~c1fp*BMI_1_41
EA_0_31~~c2*EA_1_41
BMI_0_31~~c3*BMI_1_41
EA_0_31~~c4*BMI_1_41
BMI_0_31~~c5*EA_1_41
BMI_0_1~~v1*BMI_0_1
BMI_0_2~~v1*BMI_0_2
BMI_0_10~~v1*BMI_0_10
BMI_1_1~~v2*BMI_1_1
BMI_1_2~~v2*BMI_1_2
BMI_1_10~~v2*BMI_1_10
EA_0_1~~v3*EA_0_1
EA_0_2~~v3*EA_0_2
EA_0_10~~v3*EA_0_10
EA_1_1~~v4*EA_1_1
EA_1_2~~v4*EA_1_2
EA_1_10~~v4*EA_1_10
'
fit1ee5r<-sem(model1ee5r,data=twindata, estimator="ML", missing="fiml.x")

summary(fit1ee5r,fit.measures=TRUE,standardized=TRUE)


model1ee6r<-'
BMI_0_1~b3m*EA_0_31+b4m*EA_1_41+b5m*BMI_0_31+b6m*BMI_1_41
BMI_0_2~b3m*EA_0_31+b4m*EA_1_41+b5m*BMI_0_31+b6m*BMI_1_41
BMI_0_10~b3m*EA_0_31+b4m*EA_1_41+b5m*BMI_0_31+b6m*BMI_1_41
BMI_1_1~b3f*EA_0_31+b4f*EA_1_41+b5f*BMI_0_31+b6f*BMI_1_41
BMI_1_2~b3f*EA_0_31+b4f*EA_1_41+b5f*BMI_0_31+b6f*BMI_1_41
BMI_1_10~b3f*EA_0_31+b4f*EA_1_41+b5f*BMI_0_31+b6f*BMI_1_41
EA_0_1~e3m*EA_0_31+e4m*EA_1_41+e5m*BMI_0_31+e6*BMI_1_41
EA_0_2~e3m*EA_0_31+e4m*EA_1_41+e5m*BMI_0_31+e6*BMI_1_41
EA_0_10~e3m*EA_0_31+e4m*EA_1_41+e5m*BMI_0_31+e6*BMI_1_41
EA_1_1~e3f*EA_0_31+e4f*EA_1_41+e5f*BMI_0_31+e6*BMI_1_41
EA_1_2~e3f*EA_0_31+e4f*EA_1_41+e5f*BMI_0_31+e6*BMI_1_41
EA_1_10~e3f*EA_0_31+e4f*EA_1_41+e5f*BMI_0_31+e6*BMI_1_41
EA_0_1~~c1m*BMI_0_1
EA_0_2~~c1m*BMI_0_2
EA_0_10~~c1m*BMI_0_10
EA_1_1~~c1f*BMI_1_1
EA_1_2~~c1f*BMI_1_2
EA_1_10~~c1f*BMI_1_10
EA_0_31~~c1mp*BMI_0_31
EA_1_41~~c1fp*BMI_1_41
EA_0_31~~c2*EA_1_41
BMI_0_31~~c3*BMI_1_41
EA_0_31~~c4*BMI_1_41
BMI_0_31~~c5*EA_1_41
BMI_0_1~~v1*BMI_0_1
BMI_0_2~~v1*BMI_0_2
BMI_0_10~~v1*BMI_0_10
BMI_1_1~~v2*BMI_1_1
BMI_1_2~~v2*BMI_1_2
BMI_1_10~~v2*BMI_1_10
EA_0_1~~v3*EA_0_1
EA_0_2~~v3*EA_0_2
EA_0_10~~v3*EA_0_10
EA_1_1~~v4*EA_1_1
EA_1_2~~v4*EA_1_2
EA_1_10~~v4*EA_1_10
'

fit1ee6r<-sem(model1ee6r,data=twindata, estimator="ML", missing="fiml.x")

summary(fit1ee6r,fit.measures=TRUE,standardized=TRUE)

model1eb3r<-'
BMI_0_1~b3*EA_0_31+b4m*EA_1_41+b5m*BMI_0_31+b6m*BMI_1_41
BMI_0_2~b3*EA_0_31+b4m*EA_1_41+b5m*BMI_0_31+b6m*BMI_1_41
BMI_0_10~b3*EA_0_31+b4m*EA_1_41+b5m*BMI_0_31+b6m*BMI_1_41
BMI_1_1~b3*EA_0_31+b4f*EA_1_41+b5f*BMI_0_31+b6f*BMI_1_41
BMI_1_2~b3*EA_0_31+b4f*EA_1_41+b5f*BMI_0_31+b6f*BMI_1_41
BMI_1_10~b3*EA_0_31+b4f*EA_1_41+b5f*BMI_0_31+b6f*BMI_1_41
EA_0_1~e3m*EA_0_31+e4m*EA_1_41+e5m*BMI_0_31+e6m*BMI_1_41
EA_0_2~e3m*EA_0_31+e4m*EA_1_41+e5m*BMI_0_31+e6m*BMI_1_41
EA_0_10~e3m*EA_0_31+e4m*EA_1_41+e5m*BMI_0_31+e6m*BMI_1_41
EA_1_1~e3f*EA_0_31+e4f*EA_1_41+e5f*BMI_0_31+e6f*BMI_1_41
EA_1_2~e3f*EA_0_31+e4f*EA_1_41+e5f*BMI_0_31+e6f*BMI_1_41
EA_1_10~e3f*EA_0_31+e4f*EA_1_41+e5f*BMI_0_31+e6f*BMI_1_41
EA_0_1~~c1m*BMI_0_1
EA_0_2~~c1m*BMI_0_2
EA_0_10~~c1m*BMI_0_10
EA_1_1~~c1f*BMI_1_1
EA_1_2~~c1f*BMI_1_2
EA_1_10~~c1f*BMI_1_10
EA_0_31~~c1mp*BMI_0_31
EA_1_41~~c1fp*BMI_1_41
EA_0_31~~c2*EA_1_41
BMI_0_31~~c3*BMI_1_41
EA_0_31~~c4*BMI_1_41
BMI_0_31~~c5*EA_1_41
BMI_0_1~~v1*BMI_0_1
BMI_0_2~~v1*BMI_0_2
BMI_0_10~~v1*BMI_0_10
BMI_1_1~~v2*BMI_1_1
BMI_1_2~~v2*BMI_1_2
BMI_1_10~~v2*BMI_1_10
EA_0_1~~v3*EA_0_1
EA_0_2~~v3*EA_0_2
EA_0_10~~v3*EA_0_10
EA_1_1~~v4*EA_1_1
EA_1_2~~v4*EA_1_2
EA_1_10~~v4*EA_1_10
'

fit1eb3r<-sem(model1eb3r,data=twindata, estimator="ML", missing="fiml.x")

summary(fit1eb3r,fit.measures=TRUE,standardized=TRUE)

model1eb4r<-'
BMI_0_1~b3m*EA_0_31+b4*EA_1_41+b5m*BMI_0_31+b6m*BMI_1_41
BMI_0_2~b3m*EA_0_31+b4*EA_1_41+b5m*BMI_0_31+b6m*BMI_1_41
BMI_0_10~b3m*EA_0_31+b4*EA_1_41+b5m*BMI_0_31+b6m*BMI_1_41
BMI_1_1~b3f*EA_0_31+b4*EA_1_41+b5f*BMI_0_31+b6f*BMI_1_41
BMI_1_2~b3f*EA_0_31+b4*EA_1_41+b5f*BMI_0_31+b6f*BMI_1_41
BMI_1_10~b3f*EA_0_31+b4*EA_1_41+b5f*BMI_0_31+b6f*BMI_1_41
EA_0_1~e3m*EA_0_31+e4m*EA_1_41+e5m*BMI_0_31+e6m*BMI_1_41
EA_0_2~e3m*EA_0_31+e4m*EA_1_41+e5m*BMI_0_31+e6m*BMI_1_41
EA_0_10~e3m*EA_0_31+e4m*EA_1_41+e5m*BMI_0_31+e6m*BMI_1_41
EA_1_1~e3f*EA_0_31+e4f*EA_1_41+e5f*BMI_0_31+e6f*BMI_1_41
EA_1_2~e3f*EA_0_31+e4f*EA_1_41+e5f*BMI_0_31+e6f*BMI_1_41
EA_1_10~e3f*EA_0_31+e4f*EA_1_41+e5f*BMI_0_31+e6f*BMI_1_41
EA_0_1~~c1m*BMI_0_1
EA_0_2~~c1m*BMI_0_2
EA_0_10~~c1m*BMI_0_10
EA_1_1~~c1f*BMI_1_1
EA_1_2~~c1f*BMI_1_2
EA_1_10~~c1f*BMI_1_10
EA_0_31~~c1mp*BMI_0_31
EA_1_41~~c1fp*BMI_1_41
EA_0_31~~c2*EA_1_41
BMI_0_31~~c3*BMI_1_41
EA_0_31~~c4*BMI_1_41
BMI_0_31~~c5*EA_1_41
BMI_0_1~~v1*BMI_0_1
BMI_0_2~~v1*BMI_0_2
BMI_0_10~~v1*BMI_0_10
BMI_1_1~~v2*BMI_1_1
BMI_1_2~~v2*BMI_1_2
BMI_1_10~~v2*BMI_1_10
EA_0_1~~v3*EA_0_1
EA_0_2~~v3*EA_0_2
EA_0_10~~v3*EA_0_10
EA_1_1~~v4*EA_1_1
EA_1_2~~v4*EA_1_2
EA_1_10~~v4*EA_1_10
'

fit1eb4r<-sem(model1eb4r,data=twindata, estimator="ML", missing="fiml.x")

summary(fit1eb4r,fit.measures=TRUE,standardized=TRUE)

model1eb5r<-'
BMI_0_1~b3m*EA_0_31+b4m*EA_1_41+b5*BMI_0_31+b6m*BMI_1_41
BMI_0_2~b3m*EA_0_31+b4m*EA_1_41+b5*BMI_0_31+b6m*BMI_1_41
BMI_0_10~b3m*EA_0_31+b4m*EA_1_41+b5*BMI_0_31+b6m*BMI_1_41
BMI_1_1~b3f*EA_0_31+b4f*EA_1_41+b5*BMI_0_31+b6f*BMI_1_41
BMI_1_2~b3f*EA_0_31+b4f*EA_1_41+b5*BMI_0_31+b6f*BMI_1_41
BMI_1_10~b3f*EA_0_31+b4f*EA_1_41+b5*BMI_0_31+b6f*BMI_1_41
EA_0_1~e3m*EA_0_31+e4m*EA_1_41+e5m*BMI_0_31+e6m*BMI_1_41
EA_0_2~e3m*EA_0_31+e4m*EA_1_41+e5m*BMI_0_31+e6m*BMI_1_41
EA_0_10~e3m*EA_0_31+e4m*EA_1_41+e5m*BMI_0_31+e6m*BMI_1_41
EA_1_1~e3f*EA_0_31+e4f*EA_1_41+e5f*BMI_0_31+e6f*BMI_1_41
EA_1_2~e3f*EA_0_31+e4f*EA_1_41+e5f*BMI_0_31+e6f*BMI_1_41
EA_1_10~e3f*EA_0_31+e4f*EA_1_41+e5f*BMI_0_31+e6f*BMI_1_41
EA_0_1~~c1m*BMI_0_1
EA_0_2~~c1m*BMI_0_2
EA_0_10~~c1m*BMI_0_10
EA_1_1~~c1f*BMI_1_1
EA_1_2~~c1f*BMI_1_2
EA_1_10~~c1f*BMI_1_10
EA_0_31~~c1mp*BMI_0_31
EA_1_41~~c1fp*BMI_1_41
EA_0_31~~c2*EA_1_41
BMI_0_31~~c3*BMI_1_41
EA_0_31~~c4*BMI_1_41
BMI_0_31~~c5*EA_1_41
BMI_0_1~~v1*BMI_0_1
BMI_0_2~~v1*BMI_0_2
BMI_0_10~~v1*BMI_0_10
BMI_1_1~~v2*BMI_1_1
BMI_1_2~~v2*BMI_1_2
BMI_1_10~~v2*BMI_1_10
EA_0_1~~v3*EA_0_1
EA_0_2~~v3*EA_0_2
EA_0_10~~v3*EA_0_10
EA_1_1~~v4*EA_1_1
EA_1_2~~v4*EA_1_2
EA_1_10~~v4*EA_1_10
'

fit1eb5r<-sem(model1eb5r,data=twindata, estimator="ML", missing="fiml.x")

summary(fit1eb5r,fit.measures=TRUE,standardized=TRUE)


model1eb6r<-'
BMI_0_1~b3m*EA_0_31+b4m*EA_1_41+b5m*BMI_0_31+b6*BMI_1_41
BMI_0_2~b3m*EA_0_31+b4m*EA_1_41+b5m*BMI_0_31+b6*BMI_1_41
BMI_0_10~b3m*EA_0_31+b4m*EA_1_41+b5m*BMI_0_31+b6*BMI_1_41
BMI_1_1~b3f*EA_0_31+b4f*EA_1_41+b5f*BMI_0_31+b6*BMI_1_41
BMI_1_2~b3f*EA_0_31+b4f*EA_1_41+b5f*BMI_0_31+b6*BMI_1_41
BMI_1_10~b3f*EA_0_31+b4f*EA_1_41+b5f*BMI_0_31+b6*BMI_1_41
EA_0_1~e3m*EA_0_31+e4m*EA_1_41+e5m*BMI_0_31+e6m*BMI_1_41
EA_0_2~e3m*EA_0_31+e4m*EA_1_41+e5m*BMI_0_31+e6m*BMI_1_41
EA_0_10~e3m*EA_0_31+e4m*EA_1_41+e5m*BMI_0_31+e6m*BMI_1_41
EA_1_1~e3f*EA_0_31+e4f*EA_1_41+e5f*BMI_0_31+e6f*BMI_1_41
EA_1_2~e3f*EA_0_31+e4f*EA_1_41+e5f*BMI_0_31+e6f*BMI_1_41
EA_1_10~e3f*EA_0_31+e4f*EA_1_41+e5f*BMI_0_31+e6f*BMI_1_41
EA_0_1~~c1m*BMI_0_1
EA_0_2~~c1m*BMI_0_2
EA_0_10~~c1m*BMI_0_10
EA_1_1~~c1f*BMI_1_1
EA_1_2~~c1f*BMI_1_2
EA_1_10~~c1f*BMI_1_10
EA_0_31~~c1mp*BMI_0_31
EA_1_41~~c1fp*BMI_1_41
EA_0_31~~c2*EA_1_41
BMI_0_31~~c3*BMI_1_41
EA_0_31~~c4*BMI_1_41
BMI_0_31~~c5*EA_1_41
BMI_0_1~~v1*BMI_0_1
BMI_0_2~~v1*BMI_0_2
BMI_0_10~~v1*BMI_0_10
BMI_1_1~~v2*BMI_1_1
BMI_1_2~~v2*BMI_1_2
BMI_1_10~~v2*BMI_1_10
EA_0_1~~v3*EA_0_1
EA_0_2~~v3*EA_0_2
EA_0_10~~v3*EA_0_10
EA_1_1~~v4*EA_1_1
EA_1_2~~v4*EA_1_2
EA_1_10~~v4*EA_1_10
'

fit1eb6r<-sem(model1eb6r,data=twindata, estimator="ML", missing="fiml.x")

summary(fit1eb6r,fit.measures=TRUE,standardized=TRUE)



model1ec1r<-'
BMI_0_1~b3m*EA_0_31+b4m*EA_1_41+b5m*BMI_0_31+b6m*BMI_1_41
BMI_0_2~b3m*EA_0_31+b4m*EA_1_41+b5m*BMI_0_31+b6m*BMI_1_41
BMI_0_10~b3m*EA_0_31+b4m*EA_1_41+b5m*BMI_0_31+b6m*BMI_1_41
BMI_1_1~b3f*EA_0_31+b4f*EA_1_41+b5f*BMI_0_31+b6f*BMI_1_41
BMI_1_2~b3f*EA_0_31+b4f*EA_1_41+b5f*BMI_0_31+b6f*BMI_1_41
BMI_1_10~b3f*EA_0_31+b4f*EA_1_41+b5f*BMI_0_31+b6f*BMI_1_41
EA_0_1~e3m*EA_0_31+e4m*EA_1_41+e5m*BMI_0_31+e6m*BMI_1_41
EA_0_2~e3m*EA_0_31+e4m*EA_1_41+e5m*BMI_0_31+e6m*BMI_1_41
EA_0_10~e3m*EA_0_31+e4m*EA_1_41+e5m*BMI_0_31+e6m*BMI_1_41
EA_1_1~e3f*EA_0_31+e4f*EA_1_41+e5f*BMI_0_31+e6f*BMI_1_41
EA_1_2~e3f*EA_0_31+e4f*EA_1_41+e5f*BMI_0_31+e6f*BMI_1_41
EA_1_10~e3f*EA_0_31+e4f*EA_1_41+e5f*BMI_0_31+e6f*BMI_1_41
EA_0_1~~c1*BMI_0_1
EA_0_2~~c1*BMI_0_2
EA_0_10~~c1*BMI_0_10
EA_1_1~~c1*BMI_1_1
EA_1_2~~c1*BMI_1_2
EA_1_10~~c1*BMI_1_10
EA_0_31~~c1mp*BMI_0_31
EA_1_41~~c1fp*BMI_1_41
EA_0_31~~c2*EA_1_41
BMI_0_31~~c3*BMI_1_41
EA_0_31~~c4*BMI_1_41
BMI_0_31~~c5*EA_1_41
BMI_0_1~~v1*BMI_0_1
BMI_0_2~~v1*BMI_0_2
BMI_0_10~~v1*BMI_0_10
BMI_1_1~~v2*BMI_1_1
BMI_1_2~~v2*BMI_1_2
BMI_1_10~~v2*BMI_1_10
EA_0_1~~v3*EA_0_1
EA_0_2~~v3*EA_0_2
EA_0_10~~v3*EA_0_10
EA_1_1~~v4*EA_1_1
EA_1_2~~v4*EA_1_2
EA_1_10~~v4*EA_1_10
'

fit1ec1r<-sem(model1ec1r,data=twindata, estimator="ML", missing="fiml.x")

summary(fit1ec1r,fit.measures=TRUE,standardized=TRUE)


#anova tests for model fit of submodels with equality constraints across genders of offspring
anova(fit1r, fit1ee3r)
anova(fit1r, fit1ee4r)
anova(fit1r, fit1ee5r)
anova(fit1r, fit1ee6r)
anova(fit1r,fit1eb3r)
anova(fit1r,fit1eb4r)
anova(fit1r,fit1eb5r)
anova(fit1r,fit1eb6r)
anova(fit1r,fit1ec1r)





#model1repar is for omnibus test of constraining coefficients to be equal across parents
model1repar<-'
BMI_0_1~b3m*EA_0_31+b3m*EA_1_41+b5m*BMI_0_31+b5m*BMI_1_41
BMI_0_2~b3m*EA_0_31+b3m*EA_1_41+b5m*BMI_0_31+b5m*BMI_1_41
BMI_0_10~b3m*EA_0_31+b3m*EA_1_41+b5m*BMI_0_31+b5m*BMI_1_41
BMI_1_1~b3f*EA_0_31+b3f*EA_1_41+b5f*BMI_0_31+b5f*BMI_1_41
BMI_1_2~b3f*EA_0_31+b3f*EA_1_41+b5f*BMI_0_31+b5f*BMI_1_41
BMI_1_10~b3f*EA_0_31+b3f*EA_1_41+b5f*BMI_0_31+b5f*BMI_1_41
EA_0_1~e3m*EA_0_31+e3m*EA_1_41+e5m*BMI_0_31+e5m*BMI_1_41
EA_0_2~e3m*EA_0_31+e3m*EA_1_41+e5m*BMI_0_31+e5m*BMI_1_41
EA_0_10~e3m*EA_0_31+e3m*EA_1_41+e5m*BMI_0_31+e5m*BMI_1_41
EA_1_1~e3f*EA_0_31+e3f*EA_1_41+e5f*BMI_0_31+e5f*BMI_1_41
EA_1_2~e3f*EA_0_31+e3f*EA_1_41+e5f*BMI_0_31+e5f*BMI_1_41
EA_1_10~e3f*EA_0_31+e3f*EA_1_41+e5f*BMI_0_31+e5f*BMI_1_41
EA_0_1~~c1m*BMI_0_1
EA_0_2~~c1m*BMI_0_2
EA_0_10~~c1m*BMI_0_10
EA_1_1~~c1f*BMI_1_1
EA_1_2~~c1f*BMI_1_2
EA_1_10~~c1f*BMI_1_10
EA_0_31~~c1mp*BMI_0_31
EA_1_41~~c1fp*BMI_1_41
EA_0_31~~c2*EA_1_41
BMI_0_31~~c3*BMI_1_41
EA_0_31~~c4*BMI_1_41
BMI_0_31~~c5*EA_1_41
BMI_0_1~~v1*BMI_0_1
BMI_0_2~~v1*BMI_0_2
BMI_0_10~~v1*BMI_0_10
BMI_1_1~~v2*BMI_1_1
BMI_1_2~~v2*BMI_1_2
BMI_1_10~~v2*BMI_1_10
EA_0_1~~v3*EA_0_1
EA_0_2~~v3*EA_0_2
EA_0_10~~v3*EA_0_10
EA_1_1~~v4*EA_1_1
EA_1_2~~v4*EA_1_2
EA_1_10~~v4*EA_1_10
'

fit1repar<-sem(model1repar,data=twindata, estimator="ML", missing="fiml.x")



model1reb34m<-'
BMI_0_1~b3m*EA_0_31+b3m*EA_1_41+b5m*BMI_0_31+b6m*BMI_1_41
BMI_0_2~b3m*EA_0_31+b3m*EA_1_41+b5m*BMI_0_31+b6m*BMI_1_41
BMI_0_10~b3m*EA_0_31+b3m*EA_1_41+b5m*BMI_0_31+b6m*BMI_1_41
BMI_1_1~b3f*EA_0_31+b4f*EA_1_41+b5f*BMI_0_31+b6f*BMI_1_41
BMI_1_2~b3f*EA_0_31+b4f*EA_1_41+b5f*BMI_0_31+b6f*BMI_1_41
BMI_1_10~b3f*EA_0_31+b4f*EA_1_41+b5f*BMI_0_31+b6f*BMI_1_41
EA_0_1~e3m*EA_0_31+e4m*EA_1_41+e5m*BMI_0_31+e6m*BMI_1_41
EA_0_2~e3m*EA_0_31+e4m*EA_1_41+e5m*BMI_0_31+e6m*BMI_1_41
EA_0_10~e3m*EA_0_31+e4m*EA_1_41+e5m*BMI_0_31+e6m*BMI_1_41
EA_1_1~e3f*EA_0_31+e4f*EA_1_41+e5f*BMI_0_31+e6f*BMI_1_41
EA_1_2~e3f*EA_0_31+e4f*EA_1_41+e5f*BMI_0_31+e6f*BMI_1_41
EA_1_10~e3f*EA_0_31+e4f*EA_1_41+e5f*BMI_0_31+e6f*BMI_1_41
EA_0_1~~c1m*BMI_0_1
EA_0_2~~c1m*BMI_0_2
EA_0_10~~c1m*BMI_0_10
EA_1_1~~c1f*BMI_1_1
EA_1_2~~c1f*BMI_1_2
EA_1_10~~c1f*BMI_1_10
EA_0_31~~c1mp*BMI_0_31
EA_1_41~~c1fp*BMI_1_41
EA_0_31~~c2*EA_1_41
BMI_0_31~~c3*BMI_1_41
EA_0_31~~c4*BMI_1_41
BMI_0_31~~c5*EA_1_41
BMI_0_1~~v1*BMI_0_1
BMI_0_2~~v1*BMI_0_2
BMI_0_10~~v1*BMI_0_10
BMI_1_1~~v2*BMI_1_1
BMI_1_2~~v2*BMI_1_2
BMI_1_10~~v2*BMI_1_10
EA_0_1~~v3*EA_0_1
EA_0_2~~v3*EA_0_2
EA_0_10~~v3*EA_0_10
EA_1_1~~v4*EA_1_1
EA_1_2~~v4*EA_1_2
EA_1_10~~v4*EA_1_10
'

fit1reb34m<-sem(model1reb34m,data=twindata, estimator="ML", missing="fiml.x")


model1reb34f<-'
BMI_0_1~b3m*EA_0_31+b4m*EA_1_41+b5m*BMI_0_31+b6m*BMI_1_41
BMI_0_2~b3m*EA_0_31+b4m*EA_1_41+b5m*BMI_0_31+b6m*BMI_1_41
BMI_0_10~b3m*EA_0_31+b4m*EA_1_41+b5m*BMI_0_31+b6m*BMI_1_41
BMI_1_1~b3f*EA_0_31+b3f*EA_1_41+b5f*BMI_0_31+b6f*BMI_1_41
BMI_1_2~b3f*EA_0_31+b3f*EA_1_41+b5f*BMI_0_31+b6f*BMI_1_41
BMI_1_10~b3f*EA_0_31+b3f*EA_1_41+b5f*BMI_0_31+b6f*BMI_1_41
EA_0_1~e3m*EA_0_31+e4m*EA_1_41+e5m*BMI_0_31+e6m*BMI_1_41
EA_0_2~e3m*EA_0_31+e4m*EA_1_41+e5m*BMI_0_31+e6m*BMI_1_41
EA_0_10~e3m*EA_0_31+e4m*EA_1_41+e5m*BMI_0_31+e6m*BMI_1_41
EA_1_1~e3f*EA_0_31+e4f*EA_1_41+e5f*BMI_0_31+e6f*BMI_1_41
EA_1_2~e3f*EA_0_31+e4f*EA_1_41+e5f*BMI_0_31+e6f*BMI_1_41
EA_1_10~e3f*EA_0_31+e4f*EA_1_41+e5f*BMI_0_31+e6f*BMI_1_41
EA_0_1~~c1m*BMI_0_1
EA_0_2~~c1m*BMI_0_2
EA_0_10~~c1m*BMI_0_10
EA_1_1~~c1f*BMI_1_1
EA_1_2~~c1f*BMI_1_2
EA_1_10~~c1f*BMI_1_10
EA_0_31~~c1mp*BMI_0_31
EA_1_41~~c1fp*BMI_1_41
EA_0_31~~c2*EA_1_41
BMI_0_31~~c3*BMI_1_41
EA_0_31~~c4*BMI_1_41
BMI_0_31~~c5*EA_1_41
BMI_0_1~~v1*BMI_0_1
BMI_0_2~~v1*BMI_0_2
BMI_0_10~~v1*BMI_0_10
BMI_1_1~~v2*BMI_1_1
BMI_1_2~~v2*BMI_1_2
BMI_1_10~~v2*BMI_1_10
EA_0_1~~v3*EA_0_1
EA_0_2~~v3*EA_0_2
EA_0_10~~v3*EA_0_10
EA_1_1~~v4*EA_1_1
EA_1_2~~v4*EA_1_2
EA_1_10~~v4*EA_1_10
'

fit1reb34f<-sem(model1reb34f,data=twindata, estimator="ML", missing="fiml.x")


model1reb56m<-'
BMI_0_1~b3m*EA_0_31+b4m*EA_1_41+b5m*BMI_0_31+b5m*BMI_1_41
BMI_0_2~b3m*EA_0_31+b4m*EA_1_41+b5m*BMI_0_31+b5m*BMI_1_41
BMI_0_10~b3m*EA_0_31+b4m*EA_1_41+b5m*BMI_0_31+b5m*BMI_1_41
BMI_1_1~b3f*EA_0_31+b4f*EA_1_41+b5f*BMI_0_31+b6f*BMI_1_41
BMI_1_2~b3f*EA_0_31+b4f*EA_1_41+b5f*BMI_0_31+b6f*BMI_1_41
BMI_1_10~b3f*EA_0_31+b4f*EA_1_41+b5f*BMI_0_31+b6f*BMI_1_41
EA_0_1~e3m*EA_0_31+e4m*EA_1_41+e5m*BMI_0_31+e6m*BMI_1_41
EA_0_2~e3m*EA_0_31+e4m*EA_1_41+e5m*BMI_0_31+e6m*BMI_1_41
EA_0_10~e3m*EA_0_31+e4m*EA_1_41+e5m*BMI_0_31+e6m*BMI_1_41
EA_1_1~e3f*EA_0_31+e4f*EA_1_41+e5f*BMI_0_31+e6f*BMI_1_41
EA_1_2~e3f*EA_0_31+e4f*EA_1_41+e5f*BMI_0_31+e6f*BMI_1_41
EA_1_10~e3f*EA_0_31+e4f*EA_1_41+e5f*BMI_0_31+e6f*BMI_1_41
EA_0_1~~c1m*BMI_0_1
EA_0_2~~c1m*BMI_0_2
EA_0_10~~c1m*BMI_0_10
EA_1_1~~c1f*BMI_1_1
EA_1_2~~c1f*BMI_1_2
EA_1_10~~c1f*BMI_1_10
EA_0_31~~c1mp*BMI_0_31
EA_1_41~~c1fp*BMI_1_41
EA_0_31~~c2*EA_1_41
BMI_0_31~~c3*BMI_1_41
EA_0_31~~c4*BMI_1_41
BMI_0_31~~c5*EA_1_41
BMI_0_1~~v1*BMI_0_1
BMI_0_2~~v1*BMI_0_2
BMI_0_10~~v1*BMI_0_10
BMI_1_1~~v2*BMI_1_1
BMI_1_2~~v2*BMI_1_2
BMI_1_10~~v2*BMI_1_10
EA_0_1~~v3*EA_0_1
EA_0_2~~v3*EA_0_2
EA_0_10~~v3*EA_0_10
EA_1_1~~v4*EA_1_1
EA_1_2~~v4*EA_1_2
EA_1_10~~v4*EA_1_10
'

fit1reb56m<-sem(model1reb56m,data=twindata, estimator="ML", missing="fiml.x")


model1reb56f<-'
BMI_0_1~b3m*EA_0_31+b4m*EA_1_41+b5m*BMI_0_31+b6m*BMI_1_41
BMI_0_2~b3m*EA_0_31+b4m*EA_1_41+b5m*BMI_0_31+b6m*BMI_1_41
BMI_0_10~b3m*EA_0_31+b4m*EA_1_41+b5m*BMI_0_31+b6m*BMI_1_41
BMI_1_1~b3f*EA_0_31+b4f*EA_1_41+b5f*BMI_0_31+b5f*BMI_1_41
BMI_1_2~b3f*EA_0_31+b4f*EA_1_41+b5f*BMI_0_31+b5f*BMI_1_41
BMI_1_10~b3f*EA_0_31+b4f*EA_1_41+b5f*BMI_0_31+b5f*BMI_1_41
EA_0_1~e3m*EA_0_31+e4m*EA_1_41+e5m*BMI_0_31+e6m*BMI_1_41
EA_0_2~e3m*EA_0_31+e4m*EA_1_41+e5m*BMI_0_31+e6m*BMI_1_41
EA_0_10~e3m*EA_0_31+e4m*EA_1_41+e5m*BMI_0_31+e6m*BMI_1_41
EA_1_1~e3f*EA_0_31+e4f*EA_1_41+e5f*BMI_0_31+e6f*BMI_1_41
EA_1_2~e3f*EA_0_31+e4f*EA_1_41+e5f*BMI_0_31+e6f*BMI_1_41
EA_1_10~e3f*EA_0_31+e4f*EA_1_41+e5f*BMI_0_31+e6f*BMI_1_41
EA_0_1~~c1m*BMI_0_1
EA_0_2~~c1m*BMI_0_2
EA_0_10~~c1m*BMI_0_10
EA_1_1~~c1f*BMI_1_1
EA_1_2~~c1f*BMI_1_2
EA_1_10~~c1f*BMI_1_10
EA_0_31~~c1mp*BMI_0_31
EA_1_41~~c1fp*BMI_1_41
EA_0_31~~c2*EA_1_41
BMI_0_31~~c3*BMI_1_41
EA_0_31~~c4*BMI_1_41
BMI_0_31~~c5*EA_1_41
BMI_0_1~~v1*BMI_0_1
BMI_0_2~~v1*BMI_0_2
BMI_0_10~~v1*BMI_0_10
BMI_1_1~~v2*BMI_1_1
BMI_1_2~~v2*BMI_1_2
BMI_1_10~~v2*BMI_1_10
EA_0_1~~v3*EA_0_1
EA_0_2~~v3*EA_0_2
EA_0_10~~v3*EA_0_10
EA_1_1~~v4*EA_1_1
EA_1_2~~v4*EA_1_2
EA_1_10~~v4*EA_1_10
'

fit1reb56f<-sem(model1reb56f,data=twindata, estimator="ML", missing="fiml.x")


model1ree34m<-'
BMI_0_1~b3m*EA_0_31+b4m*EA_1_41+b5m*BMI_0_31+b6m*BMI_1_41
BMI_0_2~b3m*EA_0_31+b4m*EA_1_41+b5m*BMI_0_31+b6m*BMI_1_41
BMI_0_10~b3m*EA_0_31+b4m*EA_1_41+b5m*BMI_0_31+b6m*BMI_1_41
BMI_1_1~b3f*EA_0_31+b4f*EA_1_41+b5f*BMI_0_31+b6f*BMI_1_41
BMI_1_2~b3f*EA_0_31+b4f*EA_1_41+b5f*BMI_0_31+b6f*BMI_1_41
BMI_1_10~b3f*EA_0_31+b4f*EA_1_41+b5f*BMI_0_31+b6f*BMI_1_41
EA_0_1~e3m*EA_0_31+e3m*EA_1_41+e5m*BMI_0_31+e6m*BMI_1_41
EA_0_2~e3m*EA_0_31+e3m*EA_1_41+e5m*BMI_0_31+e6m*BMI_1_41
EA_0_10~e3m*EA_0_31+e3m*EA_1_41+e5m*BMI_0_31+e6m*BMI_1_41
EA_1_1~e3f*EA_0_31+e4f*EA_1_41+e5f*BMI_0_31+e6f*BMI_1_41
EA_1_2~e3f*EA_0_31+e4f*EA_1_41+e5f*BMI_0_31+e6f*BMI_1_41
EA_1_10~e3f*EA_0_31+e4f*EA_1_41+e5f*BMI_0_31+e6f*BMI_1_41
EA_0_1~~c1m*BMI_0_1
EA_0_2~~c1m*BMI_0_2
EA_0_10~~c1m*BMI_0_10
EA_1_1~~c1f*BMI_1_1
EA_1_2~~c1f*BMI_1_2
EA_1_10~~c1f*BMI_1_10
EA_0_31~~c1mp*BMI_0_31
EA_1_41~~c1fp*BMI_1_41
EA_0_31~~c2*EA_1_41
BMI_0_31~~c3*BMI_1_41
EA_0_31~~c4*BMI_1_41
BMI_0_31~~c5*EA_1_41
BMI_0_1~~v1*BMI_0_1
BMI_0_2~~v1*BMI_0_2
BMI_0_10~~v1*BMI_0_10
BMI_1_1~~v2*BMI_1_1
BMI_1_2~~v2*BMI_1_2
BMI_1_10~~v2*BMI_1_10
EA_0_1~~v3*EA_0_1
EA_0_2~~v3*EA_0_2
EA_0_10~~v3*EA_0_10
EA_1_1~~v4*EA_1_1
EA_1_2~~v4*EA_1_2
EA_1_10~~v4*EA_1_10
'

fit1ree34m<-sem(model1ree34m,data=twindata, estimator="ML", missing="fiml.x")


model1ree34f<-'
BMI_0_1~b3m*EA_0_31+b4m*EA_1_41+b5m*BMI_0_31+b6m*BMI_1_41
BMI_0_2~b3m*EA_0_31+b4m*EA_1_41+b5m*BMI_0_31+b6m*BMI_1_41
BMI_0_10~b3m*EA_0_31+b4m*EA_1_41+b5m*BMI_0_31+b6m*BMI_1_41
BMI_1_1~b3f*EA_0_31+b4f*EA_1_41+b5f*BMI_0_31+b6f*BMI_1_41
BMI_1_2~b3f*EA_0_31+b4f*EA_1_41+b5f*BMI_0_31+b6f*BMI_1_41
BMI_1_10~b3f*EA_0_31+b4f*EA_1_41+b5f*BMI_0_31+b6f*BMI_1_41
EA_0_1~e3m*EA_0_31+e4m*EA_1_41+e5m*BMI_0_31+e6m*BMI_1_41
EA_0_2~e3m*EA_0_31+e4m*EA_1_41+e5m*BMI_0_31+e6m*BMI_1_41
EA_0_10~e3m*EA_0_31+e4m*EA_1_41+e5m*BMI_0_31+e6m*BMI_1_41
EA_1_1~e3f*EA_0_31+e3f*EA_1_41+e5f*BMI_0_31+e6f*BMI_1_41
EA_1_2~e3f*EA_0_31+e3f*EA_1_41+e5f*BMI_0_31+e6f*BMI_1_41
EA_1_10~e3f*EA_0_31+e3f*EA_1_41+e5f*BMI_0_31+e6f*BMI_1_41
EA_0_1~~c1m*BMI_0_1
EA_0_2~~c1m*BMI_0_2
EA_0_10~~c1m*BMI_0_10
EA_1_1~~c1f*BMI_1_1
EA_1_2~~c1f*BMI_1_2
EA_1_10~~c1f*BMI_1_10
EA_0_31~~c1mp*BMI_0_31
EA_1_41~~c1fp*BMI_1_41
EA_0_31~~c2*EA_1_41
BMI_0_31~~c3*BMI_1_41
EA_0_31~~c4*BMI_1_41
BMI_0_31~~c5*EA_1_41
BMI_0_1~~v1*BMI_0_1
BMI_0_2~~v1*BMI_0_2
BMI_0_10~~v1*BMI_0_10
BMI_1_1~~v2*BMI_1_1
BMI_1_2~~v2*BMI_1_2
BMI_1_10~~v2*BMI_1_10
EA_0_1~~v3*EA_0_1
EA_0_2~~v3*EA_0_2
EA_0_10~~v3*EA_0_10
EA_1_1~~v4*EA_1_1
EA_1_2~~v4*EA_1_2
EA_1_10~~v4*EA_1_10
'

fit1ree34f<-sem(model1ree34f,data=twindata, estimator="ML", missing="fiml.x")


model1ree56m<-'
BMI_0_1~b3m*EA_0_31+b4m*EA_1_41+b5m*BMI_0_31+b6m*BMI_1_41
BMI_0_2~b3m*EA_0_31+b4m*EA_1_41+b5m*BMI_0_31+b6m*BMI_1_41
BMI_0_10~b3m*EA_0_31+b4m*EA_1_41+b5m*BMI_0_31+b6m*BMI_1_41
BMI_1_1~b3f*EA_0_31+b4f*EA_1_41+b5f*BMI_0_31+b6f*BMI_1_41
BMI_1_2~b3f*EA_0_31+b4f*EA_1_41+b5f*BMI_0_31+b6f*BMI_1_41
BMI_1_10~b3f*EA_0_31+b4f*EA_1_41+b5f*BMI_0_31+b6f*BMI_1_41
EA_0_1~e3m*EA_0_31+e4m*EA_1_41+e5m*BMI_0_31+e5m*BMI_1_41
EA_0_2~e3m*EA_0_31+e4m*EA_1_41+e5m*BMI_0_31+e5m*BMI_1_41
EA_0_10~e3m*EA_0_31+e4m*EA_1_41+e5m*BMI_0_31+e5m*BMI_1_41
EA_1_1~e3f*EA_0_31+e4f*EA_1_41+e5f*BMI_0_31+e6f*BMI_1_41
EA_1_2~e3f*EA_0_31+e4f*EA_1_41+e5f*BMI_0_31+e6f*BMI_1_41
EA_1_10~e3f*EA_0_31+e4f*EA_1_41+e5f*BMI_0_31+e6f*BMI_1_41
EA_0_1~~c1m*BMI_0_1
EA_0_2~~c1m*BMI_0_2
EA_0_10~~c1m*BMI_0_10
EA_1_1~~c1f*BMI_1_1
EA_1_2~~c1f*BMI_1_2
EA_1_10~~c1f*BMI_1_10
EA_0_31~~c1mp*BMI_0_31
EA_1_41~~c1fp*BMI_1_41
EA_0_31~~c2*EA_1_41
BMI_0_31~~c3*BMI_1_41
EA_0_31~~c4*BMI_1_41
BMI_0_31~~c5*EA_1_41
BMI_0_1~~v1*BMI_0_1
BMI_0_2~~v1*BMI_0_2
BMI_0_10~~v1*BMI_0_10
BMI_1_1~~v2*BMI_1_1
BMI_1_2~~v2*BMI_1_2
BMI_1_10~~v2*BMI_1_10
EA_0_1~~v3*EA_0_1
EA_0_2~~v3*EA_0_2
EA_0_10~~v3*EA_0_10
EA_1_1~~v4*EA_1_1
EA_1_2~~v4*EA_1_2
EA_1_10~~v4*EA_1_10
'

fit1ree56m<-sem(model1ree56m,data=twindata, estimator="ML", missing="fiml.x")

model1ree56f<-'
BMI_0_1~b3m*EA_0_31+b4m*EA_1_41+b5m*BMI_0_31+b6m*BMI_1_41
BMI_0_2~b3m*EA_0_31+b4m*EA_1_41+b5m*BMI_0_31+b6m*BMI_1_41
BMI_0_10~b3m*EA_0_31+b4m*EA_1_41+b5m*BMI_0_31+b6m*BMI_1_41
BMI_1_1~b3f*EA_0_31+b4f*EA_1_41+b5f*BMI_0_31+b6f*BMI_1_41
BMI_1_2~b3f*EA_0_31+b4f*EA_1_41+b5f*BMI_0_31+b6f*BMI_1_41
BMI_1_10~b3f*EA_0_31+b4f*EA_1_41+b5f*BMI_0_31+b6f*BMI_1_41
EA_0_1~e3m*EA_0_31+e4m*EA_1_41+e5m*BMI_0_31+e6m*BMI_1_41
EA_0_2~e3m*EA_0_31+e4m*EA_1_41+e5m*BMI_0_31+e6m*BMI_1_41
EA_0_10~e3m*EA_0_31+e4m*EA_1_41+e5m*BMI_0_31+e6m*BMI_1_41
EA_1_1~e3f*EA_0_31+e4f*EA_1_41+e5f*BMI_0_31+e5f*BMI_1_41
EA_1_2~e3f*EA_0_31+e4f*EA_1_41+e5f*BMI_0_31+e5f*BMI_1_41
EA_1_10~e3f*EA_0_31+e4f*EA_1_41+e5f*BMI_0_31+e5f*BMI_1_41
EA_0_1~~c1m*BMI_0_1
EA_0_2~~c1m*BMI_0_2
EA_0_10~~c1m*BMI_0_10
EA_1_1~~c1f*BMI_1_1
EA_1_2~~c1f*BMI_1_2
EA_1_10~~c1f*BMI_1_10
EA_0_31~~c1mp*BMI_0_31
EA_1_41~~c1fp*BMI_1_41
EA_0_31~~c2*EA_1_41
BMI_0_31~~c3*BMI_1_41
EA_0_31~~c4*BMI_1_41
BMI_0_31~~c5*EA_1_41
BMI_0_1~~v1*BMI_0_1
BMI_0_2~~v1*BMI_0_2
BMI_0_10~~v1*BMI_0_10
BMI_1_1~~v2*BMI_1_1
BMI_1_2~~v2*BMI_1_2
BMI_1_10~~v2*BMI_1_10
EA_0_1~~v3*EA_0_1
EA_0_2~~v3*EA_0_2
EA_0_10~~v3*EA_0_10
EA_1_1~~v4*EA_1_1
EA_1_2~~v4*EA_1_2
EA_1_10~~v4*EA_1_10
'

fit1ree56f<-sem(model1ree56f,data=twindata, estimator="ML", missing="fiml.x")

anova(fit1r,fit1repar)
anova(fit1r,fit1ree34m)
anova(fit1r,fit1ree34f)
anova(fit1r,fit1ree56m)
anova(fit1r,fit1ree56f)
anova(fit1r,fit1reb34m)
anova(fit1r,fit1reb34f)
anova(fit1r,fit1reb56m)
anova(fit1r,fit1reb56f)



#model5* tests gender differences across offspring after constraining all coefficients to be equal across parents
model5eb3<-'
BMI_0_1~b3*EA_0_31+b3*EA_1_41+b5m*BMI_0_31+b5m*BMI_1_41
BMI_0_2~b3*EA_0_31+b3*EA_1_41+b5m*BMI_0_31+b5m*BMI_1_41
BMI_0_10~b3*EA_0_31+b3*EA_1_41+b5m*BMI_0_31+b5m*BMI_1_41
BMI_1_1~b3*EA_0_31+b3*EA_1_41+b5f*BMI_0_31+b5f*BMI_1_41
BMI_1_2~b3*EA_0_31+b3*EA_1_41+b5f*BMI_0_31+b5f*BMI_1_41
BMI_1_10~b3*EA_0_31+b3*EA_1_41+b5f*BMI_0_31+b5f*BMI_1_41
EA_0_1~e3m*EA_0_31+e3m*EA_1_41+e5m*BMI_0_31+e5m*BMI_1_41
EA_0_2~e3m*EA_0_31+e3m*EA_1_41+e5m*BMI_0_31+e5m*BMI_1_41
EA_0_10~e3m*EA_0_31+e3m*EA_1_41+e5m*BMI_0_31+e5m*BMI_1_41
EA_1_1~e3f*EA_0_31+e3f*EA_1_41+e5f*BMI_0_31+e5f*BMI_1_41
EA_1_2~e3f*EA_0_31+e3f*EA_1_41+e5f*BMI_0_31+e5f*BMI_1_41
EA_1_10~e3f*EA_0_31+e3f*EA_1_41+e5f*BMI_0_31+e5f*BMI_1_41
EA_0_1~~c1m*BMI_0_1
EA_0_2~~c1m*BMI_0_2
EA_0_10~~c1m*BMI_0_10
EA_1_1~~c1f*BMI_1_1
EA_1_2~~c1f*BMI_1_2
EA_1_10~~c1f*BMI_1_10
EA_0_31~~c1mp*BMI_0_31
EA_1_41~~c1fp*BMI_1_41
EA_0_31~~c2*EA_1_41
BMI_0_31~~c3*BMI_1_41
EA_0_31~~c4*BMI_1_41
BMI_0_31~~c5*EA_1_41
BMI_0_1~~v1*BMI_0_1
BMI_0_2~~v1*BMI_0_2
BMI_0_10~~v1*BMI_0_10
BMI_1_1~~v2*BMI_1_1
BMI_1_2~~v2*BMI_1_2
BMI_1_10~~v2*BMI_1_10
EA_0_1~~v3*EA_0_1
EA_0_2~~v3*EA_0_2
EA_0_10~~v3*EA_0_10
EA_1_1~~v4*EA_1_1
EA_1_2~~v4*EA_1_2
EA_1_10~~v4*EA_1_10
'

fit5eb3<-sem(model5eb3,data=twindata, estimator="ML", missing="fiml.x")

model5eb5<-'
BMI_0_1~b3m*EA_0_31+b3m*EA_1_41+b5*BMI_0_31+b5*BMI_1_41
BMI_0_2~b3m*EA_0_31+b3m*EA_1_41+b5*BMI_0_31+b5*BMI_1_41
BMI_0_10~b3m*EA_0_31+b3m*EA_1_41+b5*BMI_0_31+b5*BMI_1_41
BMI_1_1~b3f*EA_0_31+b3f*EA_1_41+b5*BMI_0_31+b5*BMI_1_41
BMI_1_2~b3f*EA_0_31+b3f*EA_1_41+b5*BMI_0_31+b5*BMI_1_41
BMI_1_10~b3f*EA_0_31+b3f*EA_1_41+b5*BMI_0_31+b5*BMI_1_41
EA_0_1~e3m*EA_0_31+e3m*EA_1_41+e5m*BMI_0_31+e5m*BMI_1_41
EA_0_2~e3m*EA_0_31+e3m*EA_1_41+e5m*BMI_0_31+e5m*BMI_1_41
EA_0_10~e3m*EA_0_31+e3m*EA_1_41+e5m*BMI_0_31+e5m*BMI_1_41
EA_1_1~e3f*EA_0_31+e3f*EA_1_41+e5f*BMI_0_31+e5f*BMI_1_41
EA_1_2~e3f*EA_0_31+e3f*EA_1_41+e5f*BMI_0_31+e5f*BMI_1_41
EA_1_10~e3f*EA_0_31+e3f*EA_1_41+e5f*BMI_0_31+e5f*BMI_1_41
EA_0_1~~c1m*BMI_0_1
EA_0_2~~c1m*BMI_0_2
EA_0_10~~c1m*BMI_0_10
EA_1_1~~c1f*BMI_1_1
EA_1_2~~c1f*BMI_1_2
EA_1_10~~c1f*BMI_1_10
EA_0_31~~c1mp*BMI_0_31
EA_1_41~~c1fp*BMI_1_41
EA_0_31~~c2*EA_1_41
BMI_0_31~~c3*BMI_1_41
EA_0_31~~c4*BMI_1_41
BMI_0_31~~c5*EA_1_41
BMI_0_1~~v1*BMI_0_1
BMI_0_2~~v1*BMI_0_2
BMI_0_10~~v1*BMI_0_10
BMI_1_1~~v2*BMI_1_1
BMI_1_2~~v2*BMI_1_2
BMI_1_10~~v2*BMI_1_10
EA_0_1~~v3*EA_0_1
EA_0_2~~v3*EA_0_2
EA_0_10~~v3*EA_0_10
EA_1_1~~v4*EA_1_1
EA_1_2~~v4*EA_1_2
EA_1_10~~v4*EA_1_10
'

fit5eb5<-sem(model5eb5,data=twindata, estimator="ML", missing="fiml.x")

model5ee3<-'
BMI_0_1~b3m*EA_0_31+b3m*EA_1_41+b5m*BMI_0_31+b5m*BMI_1_41
BMI_0_2~b3m*EA_0_31+b3m*EA_1_41+b5m*BMI_0_31+b5m*BMI_1_41
BMI_0_10~b3m*EA_0_31+b3m*EA_1_41+b5m*BMI_0_31+b5m*BMI_1_41
BMI_1_1~b3f*EA_0_31+b3f*EA_1_41+b5f*BMI_0_31+b5f*BMI_1_41
BMI_1_2~b3f*EA_0_31+b3f*EA_1_41+b5f*BMI_0_31+b5f*BMI_1_41
BMI_1_10~b3f*EA_0_31+b3f*EA_1_41+b5f*BMI_0_31+b5f*BMI_1_41
EA_0_1~e3*EA_0_31+e3*EA_1_41+e5m*BMI_0_31+e5m*BMI_1_41
EA_0_2~e3*EA_0_31+e3*EA_1_41+e5m*BMI_0_31+e5m*BMI_1_41
EA_0_10~e3*EA_0_31+e3*EA_1_41+e5m*BMI_0_31+e5m*BMI_1_41
EA_1_1~e3*EA_0_31+e3*EA_1_41+e5f*BMI_0_31+e5f*BMI_1_41
EA_1_2~e3*EA_0_31+e3*EA_1_41+e5f*BMI_0_31+e5f*BMI_1_41
EA_1_10~e3*EA_0_31+e3*EA_1_41+e5f*BMI_0_31+e5f*BMI_1_41
EA_0_1~~c1m*BMI_0_1
EA_0_2~~c1m*BMI_0_2
EA_0_10~~c1m*BMI_0_10
EA_1_1~~c1f*BMI_1_1
EA_1_2~~c1f*BMI_1_2
EA_1_10~~c1f*BMI_1_10
EA_0_31~~c1mp*BMI_0_31
EA_1_41~~c1fp*BMI_1_41
EA_0_31~~c2*EA_1_41
BMI_0_31~~c3*BMI_1_41
EA_0_31~~c4*BMI_1_41
BMI_0_31~~c5*EA_1_41
BMI_0_1~~v1*BMI_0_1
BMI_0_2~~v1*BMI_0_2
BMI_0_10~~v1*BMI_0_10
BMI_1_1~~v2*BMI_1_1
BMI_1_2~~v2*BMI_1_2
BMI_1_10~~v2*BMI_1_10
EA_0_1~~v3*EA_0_1
EA_0_2~~v3*EA_0_2
EA_0_10~~v3*EA_0_10
EA_1_1~~v4*EA_1_1
EA_1_2~~v4*EA_1_2
EA_1_10~~v4*EA_1_10
'

fit5ee3<-sem(model5ee3,data=twindata, estimator="ML", missing="fiml.x")


model5ee5<-'
BMI_0_1~b3m*EA_0_31+b3m*EA_1_41+b5m*BMI_0_31+b5m*BMI_1_41
BMI_0_2~b3m*EA_0_31+b3m*EA_1_41+b5m*BMI_0_31+b5m*BMI_1_41
BMI_0_10~b3m*EA_0_31+b3m*EA_1_41+b5m*BMI_0_31+b5m*BMI_1_41
BMI_1_1~b3f*EA_0_31+b3f*EA_1_41+b5f*BMI_0_31+b5f*BMI_1_41
BMI_1_2~b3f*EA_0_31+b3f*EA_1_41+b5f*BMI_0_31+b5f*BMI_1_41
BMI_1_10~b3f*EA_0_31+b3f*EA_1_41+b5f*BMI_0_31+b5f*BMI_1_41
EA_0_1~e3m*EA_0_31+e3m*EA_1_41+e5*BMI_0_31+e5*BMI_1_41
EA_0_2~e3m*EA_0_31+e3m*EA_1_41+e5*BMI_0_31+e5*BMI_1_41
EA_0_10~e3m*EA_0_31+e3m*EA_1_41+e5*BMI_0_31+e5*BMI_1_41
EA_1_1~e3f*EA_0_31+e3f*EA_1_41+e5*BMI_0_31+e5*BMI_1_41
EA_1_2~e3f*EA_0_31+e3f*EA_1_41+e5*BMI_0_31+e5*BMI_1_41
EA_1_10~e3f*EA_0_31+e3f*EA_1_41+e5*BMI_0_31+e5*BMI_1_41
EA_0_1~~c1m*BMI_0_1
EA_0_2~~c1m*BMI_0_2
EA_0_10~~c1m*BMI_0_10
EA_1_1~~c1f*BMI_1_1
EA_1_2~~c1f*BMI_1_2
EA_1_10~~c1f*BMI_1_10
EA_0_31~~c1mp*BMI_0_31
EA_1_41~~c1fp*BMI_1_41
EA_0_31~~c2*EA_1_41
BMI_0_31~~c3*BMI_1_41
EA_0_31~~c4*BMI_1_41
BMI_0_31~~c5*EA_1_41
BMI_0_1~~v1*BMI_0_1
BMI_0_2~~v1*BMI_0_2
BMI_0_10~~v1*BMI_0_10
BMI_1_1~~v2*BMI_1_1
BMI_1_2~~v2*BMI_1_2
BMI_1_10~~v2*BMI_1_10
EA_0_1~~v3*EA_0_1
EA_0_2~~v3*EA_0_2
EA_0_10~~v3*EA_0_10
EA_1_1~~v4*EA_1_1
EA_1_2~~v4*EA_1_2
EA_1_10~~v4*EA_1_10
'

fit15ee5<-sem(model5ee5,data=twindata, estimator="ML", missing="fiml.x")


model5eall<-'
BMI_0_1~b3*EA_0_31+b3*EA_1_41+b5*BMI_0_31+b5*BMI_1_41
BMI_0_2~b3*EA_0_31+b3*EA_1_41+b5*BMI_0_31+b5*BMI_1_41
BMI_0_10~b3*EA_0_31+b3*EA_1_41+b5*BMI_0_31+b5*BMI_1_41
BMI_1_1~b3*EA_0_31+b3*EA_1_41+b5*BMI_0_31+b5*BMI_1_41
BMI_1_2~b3*EA_0_31+b3*EA_1_41+b5*BMI_0_31+b5*BMI_1_41
BMI_1_10~b3*EA_0_31+b3*EA_1_41+b5*BMI_0_31+b5*BMI_1_41
EA_0_1~e3*EA_0_31+e3*EA_1_41+e5*BMI_0_31+e5*BMI_1_41
EA_0_2~e3*EA_0_31+e3*EA_1_41+e5*BMI_0_31+e5*BMI_1_41
EA_0_10~e3*EA_0_31+e3*EA_1_41+e5*BMI_0_31+e5*BMI_1_41
EA_1_1~e3*EA_0_31+e3*EA_1_41+e5*BMI_0_31+e5*BMI_1_41
EA_1_2~e3*EA_0_31+e3*EA_1_41+e5*BMI_0_31+e5*BMI_1_41
EA_1_10~e3*EA_0_31+e3*EA_1_41+e5*BMI_0_31+e5*BMI_1_41
EA_0_1~~c1m*BMI_0_1
EA_0_2~~c1m*BMI_0_2
EA_0_10~~c1m*BMI_0_10
EA_1_1~~c1f*BMI_1_1
EA_1_2~~c1f*BMI_1_2
EA_1_10~~c1f*BMI_1_10
EA_0_31~~c1mp*BMI_0_31
EA_1_41~~c1fp*BMI_1_41
EA_0_31~~c2*EA_1_41
BMI_0_31~~c3*BMI_1_41
EA_0_31~~c4*BMI_1_41
BMI_0_31~~c5*EA_1_41
BMI_0_1~~v1*BMI_0_1
BMI_0_2~~v1*BMI_0_2
BMI_0_10~~v1*BMI_0_10
BMI_1_1~~v2*BMI_1_1
BMI_1_2~~v2*BMI_1_2
BMI_1_10~~v2*BMI_1_10
EA_0_1~~v3*EA_0_1
EA_0_2~~v3*EA_0_2
EA_0_10~~v3*EA_0_10
EA_1_1~~v4*EA_1_1
EA_1_2~~v4*EA_1_2
EA_1_10~~v4*EA_1_10
'

fit5eall<-sem(model5eall,data=twindata, estimator="ML", missing="fiml.x")


anova(fit1repar,fit5eall)
anova(fit1repar,fit5eb3)
anova(fit1repar,fit5eb5)
anova(fit1repar,fit5ee3)
anova(fit1repar,fit15ee5)


#model 6 is the most parsimonious model based on equality tests above
model6<-'
BMI_0_1~b3*EA_0_31+b3*EA_1_41+b5*BMI_0_31+b6m*BMI_1_41
BMI_0_2~b3*EA_0_31+b3*EA_1_41+b5*BMI_0_31+b6m*BMI_1_41
BMI_0_10~b3*EA_0_31+b3*EA_1_41+b5*BMI_0_31+b6m*BMI_1_41
BMI_1_1~b3*EA_0_31+b3*EA_1_41+b5*BMI_0_31+b6f*BMI_1_41
BMI_1_2~b3*EA_0_31+b3*EA_1_41+b5*BMI_0_31+b6f*BMI_1_41
BMI_1_10~b3*EA_0_31+b3*EA_1_41+b5*BMI_0_31+b6f*BMI_1_41
EA_0_1~e3*EA_0_31+e3*EA_1_41+e5*BMI_0_31+e5*BMI_1_41
EA_0_2~e3*EA_0_31+e3*EA_1_41+e5*BMI_0_31+e5*BMI_1_41
EA_0_10~e3*EA_0_31+e3*EA_1_41+e5*BMI_0_31+e5*BMI_1_41
EA_1_1~e3*EA_0_31+e3*EA_1_41+e5*BMI_0_31+e5*BMI_1_41
EA_1_2~e3*EA_0_31+e3*EA_1_41+e5*BMI_0_31+e5*BMI_1_41
EA_1_10~e3*EA_0_31+e3*EA_1_41+e5*BMI_0_31+e5*BMI_1_41
EA_0_1~~c1m*BMI_0_1
EA_0_2~~c1m*BMI_0_2
EA_0_10~~c1m*BMI_0_10
EA_1_1~~c1f*BMI_1_1
EA_1_2~~c1f*BMI_1_2
EA_1_10~~c1f*BMI_1_10
EA_0_31~~c1mp*BMI_0_31
EA_1_41~~c1fp*BMI_1_41
EA_0_31~~c2*EA_1_41
BMI_0_31~~c3*BMI_1_41
EA_0_31~~c4*BMI_1_41
BMI_0_31~~c5*EA_1_41
BMI_0_1~~v1*BMI_0_1
BMI_0_2~~v1*BMI_0_2
BMI_0_10~~v1*BMI_0_10
BMI_1_1~~v2*BMI_1_1
BMI_1_2~~v2*BMI_1_2
BMI_1_10~~v2*BMI_1_10
EA_0_1~~v3*EA_0_1
EA_0_2~~v3*EA_0_2
EA_0_10~~v3*EA_0_10
EA_1_1~~v4*EA_1_1
EA_1_2~~v4*EA_1_2
EA_1_10~~v4*EA_1_10
'
fit6<-sem(model6,data=twindata, estimator="ML", missing="fiml.x")
summary(fit6,fit.measures=TRUE,standardized=TRUE)


anova(fit1r,fit6)


#examine residuals of observed and model implied correlations
resid(fit1r, "cor")
resid(fit6, "cor")


#generate output and save to csv files
write.csv((standardizedSolution(fit1r)), file = "standardized solution 1r.csv")
write.csv((parameterestimates(fit1r)), file = "parameterestimares1r.csv")
write.csv((standardizedSolution(fit6)), file = "standardized solution 6.csv")
write.csv((parameterestimates(fit6)), file = "parameterestimates6.csv")
write.csv((fitmeasures(fit1r)), file = "fitmeasures1r.csv")
write.csv((fitmeasures(fit6)), file = "fitmeasures6.csv")
write.csv((lavCor(fit1s)), file = "observed_correlation_constrained.csv")
write.csv((lavCor(fit1r)), file = "observed_correlation.csv")
write.csv(inspect(fit1r, what="obs"), file = "observed-covariance.csv")
write.csv(inspect(fit1s, what="obs"), file = "observed_covariance_constrained.csv")
write.csv(inspect(fit1r, what="cor.all"), file = "model1_implied_correlation.csv")

# Sensitivity analysis

#twindatab1545 is the data frame used for sensitivity analysis of excluding individuals with BMI <15 and >45
fit1b1545<-sem(model1,data=twindatab1545, estimator="ML", missing="fiml.x")
write.csv((parameterestimates(fit1b1545)), file = "parameterestimares1b1545.csv")

#twindatanomz is the data frame used for sensitivity analysis of excluding individuals with BMI <15 and >45
fit1nomz<-sem(model1,data=twindatanomz, estimator="ML", missing="fiml.x")
write.csv((parameterestimates(fit1nomz)), file = "parameterestimates1nomz.csv")