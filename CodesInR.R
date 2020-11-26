require(foreign)
require(MASS)
require(pROC)
require(survey)
require(ResourceSelection)
require(ROCR)
require(car)
require(ggplot2)
require(maptools)
library(nnet)
library(FSA)
library(caret)
require(mapproj)

## importing dataset

setwd('F:\ResearchProject\Dropout\JSR - Published\\Data & Codes')

kr <- as.data.frame(read.spss('Age.sav',use.value.labels=F),stringsAsFactors = FALSE)

sapply(kr,function(x) sum(is.na(x)))

kr$MA1 <- factor(kr$MA1,levels=c(1,0),labels = c('Yes','No'))

kr$age <- factor(kr$WB2 ,levels=c("15","16", "17"),labels = c('f','s', 'sev'))

kr$HH6 <- factor(kr$HH6,levels=c(1,2),labels = c('Urban','Rural'))

kr$HH7 <- factor(kr$HH7,levels=c(10,20,30,40,50,55,60),
                 labels = c('Barisal','Chittagong','Dhaka','Khulna',
                            'Rajshahi','Rangpur','Sylhet'))

kr$windex5 <- factor(kr$windex5,levels=c(1,2,3,4,5),
                     labels = c('Poorest','Poor','Middle','Rich','Richest'))

kr$religion <- factor(kr$religion,levels=c(1,2,3,4,6),
                      labels = c('Islam','Others','Others',
                                 'Others','Others'))

kr$HL11 <- factor(kr$HL11,levels=c(1,2),labels = c('Yes','No'))

kr$HL13 <- factor(kr$HL13,levels=c(1,2),labels = c('Yes','No'))

kr$Dropout <- factor(kr$Dropout,levels=c(2,1),labels = c('Yes','No'))

kr$helevel <- factor(kr$helevel,levels=c(1,2,3,4,5),
                     labels = c('None','Primary Inc.', 'Primary Com.','Secondary Inc.', 'Secondary Com.'))

sapply(kr,function(x) sum(is.na(x)))

#missing
sapply(kr,function(x) sum(is.na(x)))

#logistic odds ratio

model <- glm( relevel(factor(kr$Dropout), ref = "No")~ kr$age + relevel(factor(kr$MA1), ref = "No") + kr$HH6 +
                kr$HH7 + relevel(factor(kr$windex5), ref = "Richest") + relevel(factor(kr$religion), ref = "Others") +
                relevel(factor(kr$HL11), ref = "No") + relevel(factor(kr$HL13), ref = "No") + kr$helevel,
             family=binomial(link='logit'),data=kr)

summary(model)

exp(cbind(coef(model), confint(model)))

