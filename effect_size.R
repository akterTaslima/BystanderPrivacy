library(ggpubr)
library(multcomp)
library(car)
library(FSA) 
library(ggplot2)
library(phia)
library(psych)
library(likert)
library(plyr)
library(tidyr)
library(tidyverse)
library(dplyr)
library(lsr)
library(car)
library(rstatix)
library(heplots)

data=read.csv("clean_data.csv", sep=",", stringsAsFactors=FALSE) [-c(1)]
data$comfort=as.numeric(data$comfort)
data$condition <- ordered(data$condition,
                                  levels = c("120", "360"))

comfort_data=data[data$ques_type =="comfort", ] 
useful_data=data[data$ques_type =="useful", ] 

comfort_vip=comfort_data[comfort_data$people =="vip", ]
comfort_bystander=comfort_data[comfort_data$people =="bystander", ] 

vipdata = rbind(comfort_vip, useful_data)
visual_vip=vipdata[vipdata$info_cat =="demographic", ] 

behavior_useful_vip=useful_data[useful_data$info_cat =="behavior", ]
behavior_visual_useful_vip=useful_data[useful_data$info_cat =="behavior" | useful_data$info_cat =="demographic", ]
distance_visual_useful_vip=useful_data[useful_data$info_cat =="distance" | useful_data$info_cat =="demographic", ]

behavior_visual_comfort_vip=comfort_vip[comfort_vip$info_cat =="behavior" | comfort_vip$info_cat =="demographic", ]
distance_visual_comfort_vip=comfort_vip[comfort_vip$info_cat =="distance" | comfort_vip$info_cat =="demographic", ]


behavior_bystander=comfort_bystander[comfort_bystander$info_cat =="behavior", ]
behavior_visual_bystander=comfort_bystander[comfort_bystander$info_cat =="behavior" | comfort_bystander$info_cat =="demographic", ]
behavior_distance_bystander=comfort_bystander[comfort_bystander$info_cat =="behavior" |comfort_bystander$info_cat =="distance", ]
distance_visual_bystander=comfort_bystander[comfort_bystander$info_cat =="distance" | comfort_bystander$info_cat =="demographic", ]

looking_data = comfort_data[comfort_data$information =="looking", ]
activity_data = comfort_data[comfort_data$information =="activity", ]

anova1 <- aov(comfort_vip$comfort ~ comfort_vip$info_cat)                  # run the ANOVA
summary(anova1)                                      # print the ANOVA table
etasq(anova1) 

anova1 <- aov(useful_data$comfort ~ useful_data$info_cat)                  # run the ANOVA
summary(anova1)                                      # print the ANOVA table
etasq(anova1) 


anova1 <- aov(comfort_bystander$comfort ~ comfort_bystander$info_cat)                  # run the ANOVA
summary(anova1)                                      # print the ANOVA table
etasq(anova1) 

#cohensD(comfort_vip$comfort ~ comfort_vip$info_cat, method = "paired")

cohensD(comfort_bystander$comfort ~ comfort_bystander$condition, method = "paired") 

cohensD(comfort_data$comfort ~ comfort_data$ques_type)
cohensD(comfort_bystander$comfort ~ comfort_bystander$condition) 

cohensD(vipdata$comfort ~ vipdata$ques_type) 
cohensD(visual_vip$comfort ~ visual_vip$ques_type) 

cohensD(behavior_bystander$comfort ~ behavior_bystander$condition) 
cohensD(behavior_visual_bystander$comfort ~ behavior_visual_bystander$info_cat)
cohensD(behavior_distance_bystander$comfort ~ behavior_distance_bystander$info_cat) 
cohensD(distance_visual_bystander$comfort ~ distance_visual_bystander$info_cat) 


cohensD(behavior_useful_vip$comfort ~ behavior_useful_vip$condition) 
cohensD(behavior_visual_useful_vip$comfort ~ behavior_visual_useful_vip$info_cat) 
cohensD(distance_visual_useful_vip$comfort ~ distance_visual_useful_vip$info_cat) 

cohensD(looking_data$comfort ~ looking_data$people) 
cohensD(activity_data$comfort ~ activity_data$people) 

cohensD(behavior_visual_comfort_vip$comfort ~ behavior_visual_comfort_vip$info_cat) 
cohensD(distance_visual_comfort_vip$comfort ~ distance_visual_comfort_vip$info_cat) 
