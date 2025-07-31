projDir <-""
setwd(projDir)

source("functions.R")
package.list <- c("reshape2","dplyr","tidyr")
load.packages(package.list)


################################## cleaning ################################################
rawdot <- read.csv("dot_raw.csv")
rawnum <- read.csv("num_raw.csv")

rawdot <- rawdot[!(rawdot$Choice.RT < 150 ),]
rawnum <- rawnum[!(rawnum$Choice.RT < 150 ),]

dot <- rawdot%>%
  dplyr::group_by(Subject,Group.y,Visit, VisitSession, Task,Distance_Cond )%>%
  dplyr::summarise(
    median_RT = median(Choice.RT, na.rm = TRUE),
    accuracy = mean(Choice.ACC)
  )

num <- rawnum%>%
  dplyr::group_by(Subject,Group.y,Visit, VisitSession, Task,Distance_Cond  )%>%
  dplyr::summarise(
    median_RT = median(Choice.RT, na.rm = TRUE ),
    accuracy = mean(Choice.ACC)
  )


data = num

near <- data[data$Distance_Cond == "near",]
colnames(near) <- c("Subject", "Group", "Time", "Session", "Task", "distance", "median_RT_near", "accuracy_near" )
far <- data[data$Distance_Cond == "far",]
colnames(far) <- c("Subject", "Group", "Time","Session", "Task", "distance", "median_RT_far", "accuracy_far" )

dot_all<- rawdot%>%
  dplyr::group_by(Subject,Group.y,Visit)%>%
  dplyr::summarise(
    median_RT_all = median(Choice.RT, na.rm = TRUE ),
    accuracy_all = mean(Choice.ACC)
  )
colnames(dot_all) <- c("Subject", "Group", "Time", "median_RT_all", "accuracy_all" )

num_all<- rawnum%>%
  dplyr::group_by(Subject,Group.y,Visit)%>%
  dplyr::summarise(
    median_RT_all = median(Choice.RT, na.rm = TRUE ),
    accuracy_all = mean(Choice.ACC)
  )
colnames(num_all) <- c("Subject", "Group", "Time", "median_RT_all", "accuracy_all" )


dot_combined <- cbind(dot_all, near[,c(7:8)], far[,c(7:8)])
num_combined <- cbind(num_all, near[,c(7:8)], far[,c(7:8)])

dot_combined$Efficiency_all <- (dot_combined$accuracy_all/dot_combined$median_RT_all)*1000
dot_combined$Efficiency_near <- (dot_combined$accuracy_near/dot_combined$median_RT_near)*1000
dot_combined$Efficiency_far <- (dot_combined$accuracy_far/dot_combined$median_RT_far)*1000

num_combined$Efficiency_all <- (num_combined$accuracy_all/num_combined$median_RT_all)*1000
num_combined$Efficiency_near <- (num_combined$accuracy_near/num_combined$median_RT_near)*1000
num_combined$Efficiency_far <- (num_combined$accuracy_far/num_combined$median_RT_far)*1000


dot_combined$task <- "Nonsymbolic"
num_combined$task <- "Symbolic"

write.csv(dot_combined,  "final_dotcomp_cleaned.csv")
write.csv(num_combined,  "final_numcomp_cleaned.csv")

