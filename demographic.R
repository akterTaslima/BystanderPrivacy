library(ggpubr)
library(multcomp)
library(car)
library(FSA) 
library(ggplot2)
library(phia)
#library(IRdisplay)
#library(repr)
library(plyr)
library(dplyr)
library(tidyr)
library(psych)
library(likert)

#data_x=read.csv("vip_clean.csv", sep=",",  stringsAsFactors=FALSE)[ -c(1)]

#female_data=data_x[data_x$gender =="F", ] 
#male_data=data_x[data_x$gender =="M", ] 

#write.csv(female_data, file="female_data.csv") # Write in a new csv file
#write.csv(male_data, file="male_data.csv") # Write in a new csv file


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
  
  data_x=read.csv(file_name, sep=",", stringsAsFactors=FALSE)[ -c(1),c(v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11, "gender", "blind_level", "age", "blind_since")]
  colnames(data_x) <- c("activity", "distance", "attire", "conversation", "expression", "looking", "gender", 
                        "age", "ethnicity", "height", "weight", "M_F", "blind_level", "age_p", "blind_since" )
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

group_information <- function(dataset) {
  dataset$information[dataset$information=="activity" | 
                        dataset$information=="looking"| 
                        dataset$information=="conversation" |
                        dataset$information=="expression"] <- "Behavior"
  dataset$information[dataset$information=="age" | 
                        dataset$information=="gender"| 
                        dataset$information=="attire" |
                        dataset$information=="height"|
                        dataset$information=="weight" |
                        dataset$information=="ethnicity"] <- "Physical attributes"
  #dataset$information[dataset$information=="gender"| dataset$information=="ethnicity"] <- "Social construct"
  dataset$information[dataset$information=="distance"] <- "Distance"
  return(dataset) 
}


# ----------------------- comfort data ---------------------------
comfortable_120=data_gen("vip_clean.csv", "comf", "120") 
comfortable_360=data_gen("vip_clean.csv", "comf", "360") 

comf_combine=rbind(comfortable_120,comfortable_360)

comf_combine = gather(comf_combine, "information", "comfort", 2:12 )
comfort_data_gather=comf_combine

comfort_data_gather=group_information(comfort_data_gather)

table(comfort_data_gather$information)

comfort_data_gather$comfort = as.numeric(as.character(comfort_data_gather$comfort))
comfort_data_gather=na.omit(comfort_data_gather)


physical_comfort_data=comfort_data_gather[comfort_data_gather$information =="Physical attributes", ]
distance_comfort_data=comfort_data_gather[comfort_data_gather$information =="Distance", ] 
behavior_comfort_data=comfort_data_gather[comfort_data_gather$information =="Behavior", ] 
#social_comfort_data=comfort_data_gather[comfort_data_gather$information =="Social construct", ] 

comfort_data_gather$degree <- ordered(comfort_data_gather$degree,
                                           levels = c("120", "360"))

#comfort_data_gather_120 = comfort_data_gather[comfort_data_gather$degree =="120", ]
#comfort_data_gather_360 = comfort_data_gather[comfort_data_gather$degree =="360", ]

# ---------------------------- Blind Level -----------------------
comfort_data_gather$blind_level <- ordered(comfort_data_gather$blind_level,
                                   levels = c("Total blind", "Low vision"))
wilcox.test(comfort~ blind_level, data = comfort_data_gather)

cohensD(comfort_data_gather$comfort ~ comfort_data_gather$blind_level) 


total_blind_data=comfort_data_gather[comfort_data_gather$blind_level =="Total blind", ]
mean_sd_ci_calculate(total_blind_data$comfort)

low_vision_blind_data=comfort_data_gather[comfort_data_gather$blind_level =="Low vision", ]
mean_sd_ci_calculate(low_vision_blind_data$comfort)


ggline(comfort_data_gather, x = "blind_level", y = "comfort", color = "blind_level", 
       add = c("mean_ci"), ylab="Comfort level", xlab="blind_level"
       #palette = c("#FF0000", "#006400", "#0000CD", "#8B008B", "#556B2F")
)

# ---------------------------- Blind since -----------------------
comfort_data_gather$blind_since <- ordered(comfort_data_gather$blind_since,
                                           levels = c("Since birth", "Blind later"))
wilcox.test(comfort~ blind_since, data = comfort_data_gather)
ggline(comfort_data_gather, x = "blind_since", y = "comfort", color = "blind_since", 
       add = c("mean_ci"), ylab="Comfort level", xlab="blind_since"
       #palette = c("#FF0000", "#006400", "#0000CD", "#8B008B", "#556B2F")
)


# ---------------------------- Gender -----------------------
comfort_data_gather$M_F <- ordered(comfort_data_gather$M_F,
                               levels = c("M", "F"))
wilcox.test(comfort~ M_F, data = comfort_data_gather)

cohensD(comfort_data_gather$comfort ~ comfort_data_gather$M_F) 

female_data=comfort_data_gather[comfort_data_gather$M_F =="F", ]
mean_sd_ci_calculate(female_data$comfort)

male_data=comfort_data_gather[comfort_data_gather$M_F =="M", ]
mean_sd_ci_calculate(male_data$comfort)

wilcox.test(comfort~ M_F, data = physical_comfort_data)

wilcox.test(comfort~ M_F, data = distance_comfort_data)

wilcox.test(comfort~ M_F, data = behavior_comfort_data)

wilcox.test(comfort~ M_F, data = social_comfort_data)

wilcox.test(comfort~ degree, data = female_data)

wilcox.test(comfort~ degree, data = male_data)






ggline(comfort_data_gather, x = "information", y = "comfort", color = "M_F", 
       add = c("mean_ci"), ylab="Comfort level", xlab="Information type"
       #palette = c("#FF0000", "#006400", "#0000CD", "#8B008B")
)


# ---------------------------- Age -----------------------
comfort_data_gather$age_p <- ordered(comfort_data_gather$age_p,
                                   levels = c("18-29", "30-39", "40-49", "50-64", "65 or Older"))
#wilcox.test(comfort~ age_p, data = comfort_data_gather)
kruskal.test(comfort ~ age_p, data = comfort_data_gather)
pairwise.wilcox.test(comfort_data_gather$comfort, comfort_data_gather$age_p,
                     p.adjust.method = "BH")
ggline(comfort_data_gather, x = "age_p", y = "comfort", color = "age_p", 
       add = c("mean_ci"), ylab="Comfort level", xlab="Age"
       #palette = c("#FF0000", "#006400", "#0000CD", "#8B008B", "#556B2F")
)


# ---------------------------- Useful data -----------------------

useful_120=data_gen("vip_clean.csv", "useful", "120") 
useful_360=data_gen("vip_clean.csv", "useful", "360") 

useful_combine=rbind(useful_120,useful_360)

useful_combine = gather(useful_combine, "information", "useful", 2:12 )

useful_combine=na.omit(useful_combine)
useful_combine=group_information(useful_combine)

#useful_combine$information[useful_combine$information=="age" | useful_combine$information=="gender"| 
#                             useful_combine$information=="height" |useful_combine$information=="weight" |
#                             useful_combine$information=="ethnicity" | useful_combine$information=="attire"] <- "Physical_attributes"

#useful_combine$information[useful_combine$information=="activity" | useful_combine$information=="looking"| 
#                                  useful_combine$information=="conversation" | useful_combine$information=="expression"] <- "Activity"

#useful_combine$information[useful_combine$information=="expression" ] <- "Behavior"

#useful_combine$information[useful_combine$information=="distnace" ] <- "Distance"

table(useful_combine$information)

useful_combine$useful = as.numeric(as.character(useful_combine$useful))

# ---------------------------- Blind Level -----------------------
useful_combine$blind_level <- ordered(useful_combine$blind_level,
                                           levels = c("Total blind", "Low vision"))
wilcox.test(useful~ blind_level, data = useful_combine)

total_blind_data=useful_combine[useful_combine$blind_level =="Total blind", ]
mean_sd_ci_calculate(total_blind_data$useful)

low_vision_blind_data=useful_combine[useful_combine$blind_level =="Low vision", ]
mean_sd_ci_calculate(low_vision_blind_data$useful)


ggline(useful_combine, x = "blind_level", y = "useful", color = "blind_level", 
       add = c("mean_ci"), ylab="useful level", xlab="blind_level"
       #palette = c("#FF0000", "#006400", "#0000CD", "#8B008B", "#556B2F")
)

# ---------------------------- Blind since -----------------------
useful_combine$blind_since <- ordered(useful_combine$blind_since,
                                           levels = c("Since birth", "Blind later"))
wilcox.test(useful~ blind_since, data = useful_combine)
ggline(useful_combine, x = "blind_since", y = "useful", color = "blind_since", 
       add = c("mean_ci"), ylab="useful level", xlab="blind_since"
       #palette = c("#FF0000", "#006400", "#0000CD", "#8B008B", "#556B2F")
)

# ---------------------------- Gender -----------------------
useful_combine$M_F <- ordered(useful_combine$M_F,
                                   levels = c("M", "F"))
wilcox.test(useful~ M_F, data = useful_combine)

female_data=useful_combine[useful_combine$M_F =="F", ]
mean_sd_ci_calculate(female_data$useful)

male_data=useful_combine[useful_combine$M_F =="M", ]
mean_sd_ci_calculate(male_data$useful)

ggline(useful_combine, x = "M_F", y = "useful", color = "M_F", 
       add = c("mean_ci"), ylab="Comfort level", xlab="Information type"
       #palette = c("#FF0000", "#006400", "#0000CD", "#8B008B")
)

