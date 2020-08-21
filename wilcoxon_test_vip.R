options(jupyter.rich_display=FALSE)
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

########################  Function 
data_gen <- function(file_name, cond, degree) {
  v1=paste(cond,"activity", degree, sep="_" )
  v2= paste(cond,"distance", degree, sep="_")
  v3=paste(cond,"attire", degree, sep="_")
  v4=paste(cond,"conversation", degree, sep="_")
  v5=paste(cond,"expression", degree, sep="_")
  v6=paste(cond,"looking", degree, sep="_")
  v7=paste(cond,"gender", degree, sep="_")
  v8=paste(cond,"age", degree, sep="_")
  v9=paste(cond,"ethnicity", degree, sep="_")
  v10=paste(cond,"height", degree, sep="_")
  v11=paste(cond,"weight", degree, sep="_")
  
  data_x=read.csv(file_name, sep=",", stringsAsFactors=FALSE)[ -c(1),c(v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11)]
  colnames(data_x) <- c("activity", "distance", "attire", "conversation", "expression", "looking", "gender", 
                        "age", "ethnicity", "height", "weight")
  data_x=data_x[!apply(data_x == "", 1, all),]
  
  data_x$activity = factor(data_x$activity,
                           levels = c("1", "2", "3", "4", "5"),
                           ordered = TRUE)
  data_x$distance = factor(data_x$distance,
                           levels = c("1", "2", "3", "4", "5"),
                           ordered = TRUE)
  data_x$attire = factor(data_x$attire,
                         levels = c("1", "2", "3", "4", "5"),
                         ordered = TRUE)
  data_x$conversation = factor(data_x$conversation,
                               levels = c("1", "2", "3", "4", "5"),
                               ordered = TRUE)
  data_x$expression = factor(data_x$expression,
                             levels = c("1", "2", "3", "4", "5"),
                             ordered = TRUE)
  data_x$looking = factor(data_x$looking,
                          levels = c("1", "2", "3", "4", "5"),
                          ordered = TRUE)
  data_x$gender = factor(data_x$gender,
                         levels = c("1", "2", "3", "4", "5"),
                         ordered = TRUE)
  data_x$age = factor(data_x$age,
                      levels = c("1", "2", "3", "4", "5"),
                      ordered = TRUE)
  data_x$ethnicity = factor(data_x$ethnicity,
                            levels = c("1", "2", "3", "4", "5"),
                            ordered = TRUE)
  data_x$height = factor(data_x$height,
                         levels = c("1", "2", "3", "4", "5"),
                         ordered = TRUE)
  data_x$weight = factor(data_x$weight,
                         levels = c("1", "2", "3", "4", "5"),
                         ordered = TRUE)
  
  nrow_matrix=nrow(data_x)
  degree_label= rep(degree,nrow_matrix)
  data_x = cbind(degree=degree_label, data_x)
  #   colnames(data_x) <- c("audience", "credit_card", "face", "food", "prescription", "mess", "laptop_screen")
  return(data_x)
}

# ---------------------------- Mean calculation -----------------------
mean_sd_ci_calculate <- function(list) {
  list_mean=mean(list, na.rm=TRUE)
  list_sd=sd(list, na.rm=TRUE)
  error <- qt(0.975,df=length(list)-1)*sd(list)/sqrt(length(list))
  # error
  list_left_err <- mean(list)-error
  list_right_err <- mean(list)+error
  result = c(list_mean, list_sd , list_left_err, list_right_err)
  return(result) 
}

########################  End Function 
########################  Comfort data from gather data --------------------------------------------
comfort_data=read.csv("clean_data.csv", sep=",", stringsAsFactors=FALSE) [-c(1)]
comfort_data$comfort=as.numeric(comfort_data$comfort)
comfort_data$condition <- ordered(comfort_data$condition,
                               levels = c("120", "360"))

comfort_data=comfort_data[comfort_data$ques_type =="comfort", ] 


# ------------- comparison between VIP and Bystander
#--------------- Distance (Not significant)
distance_comfort_data=comfort_data[comfort_data$information =="distance", ] 
wilcox.test(comfort~ people, data = distance_comfort_data)
wilcox.test(comfort~ condition, data = distance_comfort_data)


#--------------- Activity (Not significant)
activity_comfort_data=comfort_data[comfort_data$information =="activity", ] 
wilcox.test(comfort~ people, data = activity_comfort_data)
wilcox.test(comfort~ condition, data = activity_comfort_data)

#--------------- Expression (Significant)
expression_comfort_data=comfort_data[comfort_data$information =="expression", ] 
wilcox.test(comfort~ people, data = expression_comfort_data)
wilcox.test(comfort~ condition, data = expression_comfort_data)

#--------------- Looking (Significant)
looking_comfort_data=comfort_data[comfort_data$information =="looking", ] 
wilcox.test(comfort~ people, data = looking_comfort_data)
wilcox.test(comfort~ condition, data = looking_comfort_data)

#--------------- Conversation (Significant)
conversation_comfort_data=comfort_data[comfort_data$information =="conversation", ] 
wilcox.test(comfort~ people, data = conversation_comfort_data)
wilcox.test(comfort~ condition, data = conversation_comfort_data)


#--------------- Overall Wilcoxon test
behavior_comfort_data=comfort_data[comfort_data$info_cat =="behavior", ] 
distance_comfort_data=comfort_data[comfort_data$info_cat =="distance", ] 

cobine_behave_distance = rbind(behavior_comfort_data, distance_comfort_data)
wilcox.test(comfort~ people, data = cobine_behave_distance)

pairwise.wilcox.test(cobine_behave_distance$comfort, cobine_behave_distance$info_cat,
                     p.adjust.method = "BH")

wilcox.test(comfort~ people, data = behavior_comfort_data)





# ------------- comparison between two FoV for VIPs
comfort_vip=comfort_data[comfort_data$people =="vip", ]

pairwise.wilcox.test(comfort_vip$comfort, comfort_vip$information, 
                     p.adjust.method = "BH")

# Looking
looking_comfort_vip=comfort_vip[comfort_vip$information =="looking", ] 
wilcox.test(comfort~ condition, data = looking_comfort_vip)

# expression
expression_comfort_vip=comfort_vip[comfort_vip$information =="expression", ] 
wilcox.test(comfort~ condition, data = expression_comfort_vip)

# conversation
conversation_comfort_vip=comfort_vip[comfort_vip$information =="conversation", ] 
wilcox.test(comfort~ condition, data = conversation_comfort_vip)

# distance
distance_comfort_vip=comfort_vip[comfort_vip$information =="distance", ] 
wilcox.test(comfort~ condition, data = distance_comfort_vip)

# activity
activity_comfort_vip=comfort_vip[comfort_vip$information =="activity", ] 
wilcox.test(comfort~ condition, data = activity_comfort_vip)


# ------------- comparison between two FoV for Bystander
comfort_bystander=comfort_data[comfort_data$people =="bystander", ]

# expression
expression_comfort_bystander=comfort_bystander[comfort_bystander$information =="expression", ] 
wilcox.test(comfort~ condition, data = expression_comfort_bystander)

# conversation
conversation_comfort_bystander=comfort_bystander[comfort_bystander$information =="conversation", ] 
wilcox.test(comfort~ condition, data = conversation_comfort_bystander)

# distance
distance_comfort_bystander=comfort_bystander[comfort_bystander$information =="distance", ] 
wilcox.test(comfort~ condition, data = distance_comfort_bystander)

# looking
looking_comfort_bystander=comfort_bystander[comfort_bystander$information =="looking", ] 
wilcox.test(comfort~ condition, data = looking_comfort_bystander)

# activity
activity_comfort_bystander=comfort_bystander[comfort_bystander$information =="activity", ] 
wilcox.test(comfort~ condition, data = activity_comfort_bystander)

# ------------ Mean calculation ------------

mean_sd_ci_calculate(looking_comfort_bystander$comfort)
mean_sd_ci_calculate(looking_comfort_vip$comfort)

mean_sd_ci_calculate(activity_comfort_bystander$comfort)
mean_sd_ci_calculate(activity_comfort_vip$comfort)

