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
library(lme4)
library(lmerTest)
library(emmeans)

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
# ---------------------------- Mean calculation -----------------------


mean_information <- function(file_name, cond, degree) {
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
  
  data_x=read.csv(file_name, sep=",", stringsAsFactors=FALSE)[ -c(1),c(v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11, "gender", "condition")]
  colnames(data_x) <- c("activity", "distance", "attire", "conversation", "expression", "looking", "gender", 
                        "age", "ethnicity", "height", "weight", "gender_bystander", "condition")
  data_x=data_x[!apply(data_x == "", 1, all),]
  gender=data_x$gender_bystander
  condition=data_x$condition
  
  nrow_matrix=nrow(data_x)
  data_x = data_x[, 1:11]
  data_x = as.numeric((unlist(data_x)));
  data_x = matrix(data_x, nrow = nrow_matrix);
  
  physical_attributes=data_x[,c(3, 7:11)]
  physical_attributes=rowMeans(physical_attributes, na.rm = TRUE)
  
  
  distance=data_x[,c(2)]
  #distance=rowMeans(distance, na.rm = TRUE)
  
  behavior=data_x[,c(1, 4:6)]
  behavior=rowMeans(behavior, na.rm = TRUE)
  
  nrow_matrix=nrow(data_x)
  if (degree=="120"){
    degree_label= rep("FoV sighted",nrow_matrix)  
  }
  if (degree=="360"){
    degree_label= rep("FoV extended",nrow_matrix)  
  }
  
  #degree_label= rep(degree,nrow_matrix)
  
  mean_data = cbind(degree_label, physical_attributes, distance, behavior, gender, condition )
  colnames(mean_data)=c("degree", "Visual_attributes", "Distance", "Behavior", "Gender", "condition")
  mean_data=as.data.frame(mean_data)
  return(mean_data)
  
}


##### ---------------------------  Comfort analysis -------------------------------

comfort_120=mean_information("bystander_243_clean.csv", "comf", "120")
comfort_360=mean_information("bystander_243_clean.csv", "comf", "360")
comfort_120=na.omit(comfort_120)
comfort_360=na.omit(comfort_360)

comfort_data=rbind(comfort_120, comfort_360)
comfort_data=na.omit(comfort_data)

ID <- seq.int(nrow(comfort_data))
comfort_data=cbind(ID, comfort_data)

comfort_data$Gender[comfort_data$Gender=="f" | comfort_data$Gender=="female" | comfort_data$Gender=="Female" |
                      comfort_data$Gender=="female " | comfort_data$Gender=="Cis female" | 
                      comfort_data$Gender=="FEMALE"] <- "Female"

comfort_data$Gender[comfort_data$Gender=="m" | comfort_data$Gender=="male" | comfort_data$Gender=="Male" |
                          comfort_data$Gender=="M" | comfort_data$Gender=="Demiboy" | 
                          comfort_data$Gender=="Male " | comfort_data$Gender=="man"|
                      comfort_data$Gender=="M A L E"] <- "Male"

table(comfort_data$Gender)
comfort_data = gather(comfort_data, "information", "comfort", 3:5 )

dim(comfort_data)
table(comfort_data$information)
comfort_data$comfort = as.numeric(comfort_data$comfort)

wilcox.test(comfort~ Gender, data = comfort_data)

physical_comfort_data=comfort_data[comfort_data$information =="Visual_attributes", ]
distance_comfort_data=comfort_data[comfort_data$information =="Distance", ] 
behavior_comfort_data=comfort_data[comfort_data$information =="Behavior", ] 
wilcox.test(comfort~ Gender, data = physical_comfort_data)
wilcox.test(comfort~ Gender, data = distance_comfort_data)
wilcox.test(comfort~ Gender, data = behavior_comfort_data)

ggline(comfort_data, x = "information", y = "comfort", color = "Gender", 
       add = c("mean_ci"), ylab="Comfort level", xlab="Information type"
       #palette = c("#FF0000", "#006400", "#0000CD", "#8B008B")
)


# ---------------------------- Female and Male Mean calculation -----------------------
physical_comfort_Female=physical_comfort_data[physical_comfort_data$Gender =="Female", ]
mean_sd_ci_calculate(physical_comfort_Female$comfort)

physical_comfort_Male=physical_comfort_data[physical_comfort_data$Gender =="Male", ]
mean_sd_ci_calculate(physical_comfort_Male$comfort)

# -------------- 120 -------------------
comfort_120_gather=comfort_data[comfort_data$degree =="FoV sighted", ]

physical_comfort_data=comfort_120_gather[comfort_120_gather$information =="Visual_attributes", ]
distance_comfort_data=comfort_120_gather[comfort_120_gather$information =="Distance", ] 
behavior_comfort_data=comfort_120_gather[comfort_120_gather$information =="Behavior", ] 
wilcox.test(comfort~ Gender, data = physical_comfort_data)
wilcox.test(comfort~ Gender, data = distance_comfort_data)
wilcox.test(comfort~ Gender, data = behavior_comfort_data)

ggline(comfort_120_gather, x = "information", y = "comfort", color = "Gender", 
       add = c("mean_ci"), ylab="Comfort level", xlab="Information type"
       #palette = c("#FF0000", "#006400", "#0000CD", "#8B008B")
)


# -------------- 360 -------------------
comfort_360_gather=comfort_data[comfort_data$degree =="FoV extended", ]
physical_comfort_data=comfort_360_gather[comfort_360_gather$information =="Visual_attributes", ]
distance_comfort_data=comfort_360_gather[comfort_360_gather$information =="Distance", ] 
behavior_comfort_data=comfort_360_gather[comfort_360_gather$information =="Behavior", ] 
wilcox.test(comfort~ Gender, data = physical_comfort_data)
wilcox.test(comfort~ Gender, data = distance_comfort_data)
wilcox.test(comfort~ Gender, data = behavior_comfort_data)

ggline(comfort_360_gather, x = "information", y = "comfort", color = "Gender", 
       add = c("mean_ci"), ylab="Comfort level", xlab="Information type"
       #palette = c("#FF0000", "#006400", "#0000CD", "#8B008B")
)




##### ---------------------------  Useful analysis -------------------------------
useful_120=mean_information("vip_clean.csv", "useful", "120") 
useful_360=mean_information("vip_clean.csv", "useful", "360") 
useful_120=na.omit(useful_120)
useful_360=na.omit(useful_360)

useful_combine=rbind(useful_120,useful_360)
ID <- seq.int(nrow(useful_combine))
useful_combine=cbind(ID, useful_combine)


useful_combine = gather(useful_combine, "information", "useful", 3:5 )

table(useful_combine$information)

useful_combine$useful = as.numeric(as.character(useful_combine$useful))


useful_combine$Gender <- ordered(useful_combine$Gender,
                                 levels = c("M", "F"))

wilcox.test(useful~ Gender, data = useful_combine)

physical_useful_data=useful_combine[useful_combine$information =="Visual_attributes", ]
distance_useful_data=useful_combine[useful_combine$information =="Distance", ] 
behavior_useful_data=useful_combine[useful_combine$information =="Behavior", ] 

wilcox.test(useful~ Gender, data = physical_useful_data)
wilcox.test(useful~ Gender, data = distance_useful_data)
wilcox.test(useful~ Gender, data = behavior_useful_data)




female_data=useful_combine[useful_combine$Gender =="F", ]
mean_sd_ci_calculate(female_data$useful)

male_data=useful_combine[useful_combine$Gender =="M", ]
mean_sd_ci_calculate(male_data$useful)




ggline(useful_combine, x = "Gender", y = "useful", color = "Gender", 
       add = c("mean_ci"), ylab="Useful level", xlab="Information type"
       #palette = c("#FF0000", "#006400", "#0000CD", "#8B008B")
)
ggline(useful_combine, x = "information", y = "useful", color = "Gender", 
       add = c("mean_ci"), ylab="Useful level", xlab="Information type"
       #palette = c("#FF0000", "#006400", "#0000CD", "#8B008B")
)

ggline(useful_combine, x = "degree", y = "useful", color = "Gender", 
       add = c("mean_ci"), ylab="Useful level", xlab="Information type"
       #palette = c("#FF0000", "#006400", "#0000CD", "#8B008B")
)
