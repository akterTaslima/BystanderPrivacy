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
  
  data_x=read.csv(file_name, sep=",", stringsAsFactors=FALSE)[ -c(1),c(v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11)]
  colnames(data_x) <- c("activity", "distance", "attire", "conversation", "expression", "looking", "gender", 
                        "age", "ethnicity", "height", "weight")
  data_x=data_x[!apply(data_x == "", 1, all),]
  
  nrow_matrix=nrow(data_x)
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
  
  mean_data = cbind(degree_label, physical_attributes, distance, behavior )
  colnames(mean_data)=c("degree", "Visual_attributes", "Distance", "Behavior")
  mean_data=as.data.frame(mean_data)
  return(mean_data)

}

##### ---------------------------  Comfort analysis -------------------------------
comfort_120=mean_information("vip_clean.csv", "comf", "120")
comfort_360=mean_information("vip_clean.csv", "comf", "360")
comfort_data=rbind(comfort_120, comfort_360)
comfort_data=na.omit(comfort_data)

ID <- seq.int(nrow(comfort_data))
comfort_data=cbind(ID, comfort_data)

comfort_data = gather(comfort_data, "information", "comfort", 3:5 )

dim(comfort_data)
table(comfort_data$information)
comfort_data$comfort = as.numeric(comfort_data$comfort)

# Combined
friedman.test(comfort ~ information | ID,
              data = comfort_data)


physical_comfort_data=comfort_data[comfort_data$information =="Visual_attributes", ]
distance_comfort_data=comfort_data[comfort_data$information =="Distance", ] 
behavior_comfort_data=comfort_data[comfort_data$information =="Behavior", ] 
#social_comfort_data=comfort_data[comfort_data$information =="Social construct", ] 


wilcox.test(comfort~ degree, data = physical_comfort_data)

wilcox.test(comfort~ degree, data = distance_comfort_data)

wilcox.test(comfort~ degree, data = behavior_comfort_data)


pairwise.wilcox.test(comfort_data$comfort, comfort_data$information, paired=TRUE,
                     p.adjust.method = "BH")



comfort_data$degree <- ordered(comfort_data$degree,
                                      levels = c("FoV sighted", "FoV extended"))

pdf("Interaction-comfort-vip.pdf", 5, 5)
ggline(comfort_data, x = "degree", y = "comfort", color = "information", add = c("mean_ci"), 
       ylab="Comfort level", xlab="Field of View",
       palette = c("#FF0000", "#006400", "#0000CC")
)
dev.off()

ggline(comfort_data, x = "information", y = "comfort", color = "information", numeric.x.axis = FALSE,
       add = c("mean_ci"), ylab="Comfort level", xlab="Information type"
       #palette = c("#FF0000", "#006400", "#0000CD", "#8B008B")
)

# ---------------------- 120 ------------------------------
comfort_120_gather=comfort_data[comfort_data$degree =="FoV sighted", ]
friedman.test(comfort ~ information | ID,
              data = comfort_120_gather)

pairwise.wilcox.test(comfort_120_gather$comfort, comfort_120_gather$information, paired=TRUE,
                     p.adjust.method = "BH")

ggline(comfort_120_gather, x = "information", y = "comfort", color = "information", add = c("mean_ci"), 
       ylab="Comfort level", xlab="Field of view"
       #palette = c("#FF0000", "#006400", "#0000CD", "#8B008B")
)
# ---------------------- 360 ------------------------------
comfort_360_gather=comfort_data[comfort_data$degree =="FoV extended", ]
friedman.test(comfort ~ information | ID,
              data = comfort_360_gather)

pairwise.wilcox.test(comfort_360_gather$comfort, comfort_360_gather$information, paired=TRUE,
                     p.adjust.method = "BH")

ggline(comfort_360_gather, x = "information", y = "comfort", color = "information", add = c("mean_ci"), 
       ylab="Comfort level", xlab="Field of view"
       #palette = c("#FF0000", "#006400", "#0000CD", "#8B008B")
)


##### ---------------------------  Useful data analysis -------------------------------
useful_120=mean_information("vip_clean.csv", "useful", "120")
useful_360=mean_information("vip_clean.csv", "useful", "360")
useful_data=rbind(useful_120, useful_360)
useful_data=na.omit(useful_data)

ID <- seq.int(nrow(useful_data))
useful_data=cbind(ID, useful_data)

useful_data = gather(useful_data, "information", "useful", 3:5 )

dim(useful_data)
table(useful_data$information)
useful_data$useful = as.numeric(useful_data$useful)

physical_useful_data=useful_data[useful_data$information =="Visual_attributes", ]
distance_useful_data=useful_data[useful_data$information =="Distance", ] 
behavior_useful_data=useful_data[useful_data$information =="Behavior", ] 
#social_comfort_data=comfort_data[comfort_data$information =="Social construct", ] 


wilcox.test(useful~ degree, data = physical_useful_data)

wilcox.test(useful~ degree, data = distance_useful_data)

wilcox.test(useful~ degree, data = behavior_useful_data)

behavior_useful_120=behavior_useful_data[behavior_useful_data$degree =="FoV sighted", ] 
behavior_useful_360=behavior_useful_data[behavior_useful_data$degree =="FoV extended", ] 
mean_sd_ci_calculate(behavior_useful_120$useful)
mean_sd_ci_calculate(behavior_useful_360$useful)



friedman.test(useful ~ information | ID,
              data = useful_data)

pairwise.wilcox.test(useful_data$useful, useful_data$information, paired=TRUE,
                     p.adjust.method = "BH")

useful_data$degree <- ordered(useful_data$degree,
                                     levels = c("FoV sighted", "FoV extended"))

pdf("Interaction-useful-vip.pdf", 5, 5)
ggline(useful_data, x = "degree", y = "useful", color = "information", add = c("mean_ci"), 
       ylab="Usefulness level", xlab="Field of View",
       palette = c("#FF0000", "#006400", "#0000CC")
)
dev.off()
ggline(useful_data, x = "information", y = "useful", color = "information", numeric.x.axis = FALSE,
       add = c("mean_ci"), ylab="Usefulness level", xlab="Information type"
       #palette = c("#FF0000", "#006400", "#0000CD", "#8B008B")
)

# ---------------------- 120 ------------------------------
useful_120_gather=useful_data[useful_data$degree =="FoV sighted", ]
friedman.test(useful ~ information | ID,
              data = useful_120_gather)

pairwise.wilcox.test(useful_120_gather$useful, useful_120_gather$information, paired=TRUE,
                     p.adjust.method = "BH")

ggline(useful_120_gather, x = "information", y = "useful", color = "information", add = c("mean_ci"), 
       ylab="Useful level", xlab="Field of view"
       #palette = c("#FF0000", "#006400", "#0000CD", "#8B008B")
)
# ---------------------- 360 ------------------------------
useful_360_gather=useful_data[useful_data$degree =="FoV extended", ]
friedman.test(useful ~ information | ID,
              data = useful_360_gather)

pairwise.wilcox.test(useful_360_gather$useful, useful_360_gather$information, paired=TRUE,
                     p.adjust.method = "BH")

ggline(useful_360_gather, x = "information", y = "useful", color = "information", add = c("mean_ci"), 
       ylab="Comfort level", xlab="Field of view"
       #palette = c("#FF0000", "#006400", "#0000CD", "#8B008B")
)




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

# ---------------------------- Comfort Mean calculation -----------------------
physical_comfort_data=comfort_data[comfort_data$information =="Visual_attributes", ]
mean_sd_ci_calculate(physical_comfort_data$comfort)


distance_comfort_data=comfort_data[comfort_data$information =="Distance", ] 
mean_sd_ci_calculate(distance_comfort_data$comfort)

behavior_comfort_data=comfort_data[comfort_data$information =="Behavior", ] 
mean_sd_ci_calculate(behavior_comfort_data$comfort)

# ---------------------------- 120 Mean calculation -----------------------
physical_comfort_data=comfort_120_gather[comfort_120_gather$information =="Visual_attributes", ]
mean_sd_ci_calculate(physical_comfort_data$comfort)


distance_comfort_data=comfort_120_gather[comfort_120_gather$information =="Distance", ] 
mean_sd_ci_calculate(distance_comfort_data$comfort)

behavior_comfort_data=comfort_120_gather[comfort_120_gather$information =="Behavior", ] 
mean_sd_ci_calculate(behavior_comfort_data$comfort)

# ---------------------------- 360 Mean calculation -----------------------
physical_comfort_data=comfort_360_gather[comfort_360_gather$information =="Visual_attributes", ]
mean_sd_ci_calculate(physical_comfort_data$comfort)


distance_comfort_data=comfort_360_gather[comfort_360_gather$information =="Distance", ] 
mean_sd_ci_calculate(distance_comfort_data$comfort)

behavior_comfort_data=comfort_360_gather[comfort_360_gather$information =="Behavior", ] 
mean_sd_ci_calculate(behavior_comfort_data$comfort)

# ---------------------------- Useful Mean calculation -----------------------
physical_useful_data=useful_data[useful_data$information =="Visual_attributes", ]
mean_sd_ci_calculate(physical_useful_data$useful)


distance_useful_data=useful_data[useful_data$information =="Distance", ] 
mean_sd_ci_calculate(distance_useful_data$useful)

behavior_useful_data=useful_data[useful_data$information =="Behavior", ] 
mean_sd_ci_calculate(behavior_useful_data$useful)

# ---------------------------- 120 Mean calculation -----------------------
physical_useful_data=useful_120_gather[useful_120_gather$information =="Visual_attributes", ]
mean_sd_ci_calculate(physical_useful_data$useful)


distance_useful_data=useful_120_gather[useful_120_gather$information =="Distance", ] 
mean_sd_ci_calculate(distance_useful_data$useful)

behavior_useful_data=useful_120_gather[useful_120_gather$information =="Behavior", ] 
mean_sd_ci_calculate(behavior_useful_data$useful)

# ---------------------------- 360 Mean calculation -----------------------
physical_useful_data=useful_360_gather[useful_360_gather$information =="Visual_attributes", ]
mean_sd_ci_calculate(physical_useful_data$useful)


distance_useful_data=useful_360_gather[useful_360_gather$information =="Distance", ] 
mean_sd_ci_calculate(distance_useful_data$useful)

behavior_useful_data=useful_360_gather[useful_360_gather$information =="Behavior", ] 
mean_sd_ci_calculate(behavior_useful_data$useful)
