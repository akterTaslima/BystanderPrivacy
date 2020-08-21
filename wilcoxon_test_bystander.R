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


group_information <- function(dataset) {
  dataset$information[dataset$information=="activity" | 
                        dataset$information=="looking"| 
                        dataset$information=="conversation" |
                        dataset$information=="expression"] <- "Behavior"
  dataset$information[dataset$information=="age" | 
                        #dataset$information=="gender"| 
                        dataset$information=="attire" |
                        dataset$information=="height"|
                        dataset$information=="weight" | 
                        dataset$information=="gender"| 
                        dataset$information=="ethnicity"] <- "Visual_attributes"
  #dataset$information[dataset$information=="gender"| dataset$information=="ethnicity"] <- "Social construct"
  dataset$information[dataset$information=="distance"] <- "Distance"
  return(dataset) 
}


comf_bystander_data=read.csv("comf_combine_bystander.csv", sep=",", stringsAsFactors=FALSE)[ -c(1)]

ID <- seq.int(nrow(comf_bystander_data))
comf_bystander_data=cbind(ID, comf_bystander_data)
#comf_bystander_data_120=comf_bystander_data[comf_bystander_data$condition =="120-no-example", ]
#comf_bystander_data_360=comf_bystander_data[comf_bystander_data$condition =="360-no-example", ]

comf_bystander_data_gather = gather(comf_bystander_data, "information", "comfort", 3:13 )
comf_bystander_data_gather=group_information(comf_bystander_data_gather)

table(comf_bystander_data_gather$information)

comf_bystander_data_gather$condition[comf_bystander_data_gather$condition=="120-no-example"] <- "Fov sighted"
comf_bystander_data_gather$condition[comf_bystander_data_gather$condition=="360-no-example"] <- "Fov extended"

comf_bystander_data_gather$comfort = as.numeric(as.character(comf_bystander_data_gather$comfort))
comf_bystander_data_gather$condition <- ordered(comf_bystander_data_gather$condition,
                                                levels = c("Fov sighted", "Fov extended"))

wilcox.test(comfort~ condition, data = comf_bystander_data_gather)

physical_comfort_data=comf_bystander_data_gather[comf_bystander_data_gather$information =="Physical_attributes", ]
distance_comfort_data=comf_bystander_data_gather[comf_bystander_data_gather$information =="Distance", ] 
behavior_comfort_data=comf_bystander_data_gather[comf_bystander_data_gather$information =="Behavior", ] 
#social_comfort_data=comf_bystander_data_gather[comf_bystander_data_gather$information =="Social construct", ] 

wilcox.test(comfort~ condition, data = physical_comfort_data)

wilcox.test(comfort~ condition, data = distance_comfort_data)

wilcox.test(comfort~ condition, data = behavior_comfort_data)

wilcox.test(comfort~ condition, data = social_comfort_data)

ggline(comf_bystander_data_gather, x = "condition", y = "comfort", color = "information", add = c("mean_ci"), 
       ylab="Comfort level", xlab="Field of view",
       palette = c("#FF0000", "#006400", "#0000CC", "#8B008B")
)

ggline(comf_bystander_data_gather, x = "information", y = "comfort", color = "information", add = c("mean_ci"), 
       ylab="Comfort level", xlab="Information",
       palette = c("#FF0000", "#006400", "#0000CC", "#8B008B")
)

nrow_matrix=nrow(comf_bystander_data)
comf_avg = as.numeric((unlist(comf_bystander_data[, 3:13])));
comf_avg = matrix(comf_avg, nrow = nrow_matrix);
comf_avg=rowMeans(comf_avg, na.rm = TRUE)

comf_avg = cbind(comf_bystander_data[,1:2], comf_avg)
colnames(comf_avg)<-cbind("ID", "condition", "comfort")

#comf_avg$degree = as.numeric(as.character(comf_avg$degree))

wilcox.test(comfort~condition,  data=comf_avg)

comf_avg_120=comf_avg[comf_avg$condition =="120-no-example", ] 
mean_sd_ci_calculate(comf_avg_120$comfort)
comf_avg_360=comf_avg[comf_avg$condition =="360-no-example", ] 
mean_sd_ci_calculate(comf_avg_360$comfort)

ggline(comf_avg, x = "condition", y = "comfort", color = "condition", add = c("mean_ci"), 
       ylab="Comfort level", xlab="Field of view",
       palette = c("#FF0000", "#006400", "#0000CC", "#8B008B")
)
