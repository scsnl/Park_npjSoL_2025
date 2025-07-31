projDir <-""
setwd(projDir)

source("functions.R")
package.list <- c("psych","reshape2","rstatix","lmerTest","lme4","afex","car","dplyr","ggplot2","Hmisc", "purrr",
                  "broom","tidyr","corrplot","ggpubr","Matrix","tibble","ggeffects","effects","stringr","readxl","readr","purrr",
                  "patternplot","ggpattern", "effsize", "BayesFactor", "ez")
load.packages(package.list)


################################## Overall efficiency, acc, rt #######################################
dot<-read.csv("final_dotcomp_cleaned.csv")
num<-read.csv("final_numcomp_cleaned.csv")

dot <- dot %>%
  convert_as_factor(Subject, Group, Time,task)
num <- num %>%
  convert_as_factor(Subject, Group, Time,task)


DF <- rbind(dot,num)
DF$Time <- ifelse(DF$Time=="1", "pre", "post")
DF$Time <- ordered(DF$Time , c("pre", "post"))

DF <- DF %>%
  convert_as_factor(Subject, Group, Time,task)



library(ez)
# For Efficiency 
anova_rt_dot <- ezANOVA(
  data = dot,
  dv = median_RT_all,
  wid = Subject,
  within = .(Time),
  between = .(Group),
  detailed = TRUE,
  type = 3  
)


anova_rt_num <- ezANOVA(
  data = num,
  dv = median_RT_all,
  wid = Subject,
  within = .(Time),
  between = .(Group),
  detailed = TRUE,
  type = 3  
)


dot_pre <- dot[dot$Time == 1,]
dot_post <- dot[dot$Time == 2,]
dot_MD <- dot[dot$Group == "MD",]
dot_TD <- dot[dot$Group == "TD",]
dot_normal <-dot[dot$Group == "MD" & dot$Time == 2| dot$Group == "TD"&dot$Time == 1,]

num_pre <- num[num$Time == 1,]
num_post <- num[num$Time == 2,]
num_MD <- num[num$Group == "MD",]
num_TD <- num[num$Group == "TD",]
num_normal <-num[num$Group == "MD" & num$Time == 2| num$Group == "TD"&num$Time == 1,]



######### group t-tests 

datasets <- list(
  dot_pre = dot_pre,
  dot_post = dot_post,
  dot_normal = dot_normal,
  num_pre = num_pre,
  num_post = num_post,
  num_normal = num_normal
)


dependent_vars <- c("Efficiency_all", "accuracy_all", "median_RT_all")

all_group_t_results <- list()

for (dataset_name in names(datasets)) {
  # Initialize a sublist for the current dataset
  all_group_t_results[[dataset_name]] <- list()
  
  # Inner loop over dependent variables
  for (dv in dependent_vars) {
    # Run the analysis for each dataset and dependent variable
    results <- ttests_w_effectsize(
      data = datasets[[dataset_name]],
      dv = dv,
      group = "Group",
      paired = FALSE,
      var_equal = TRUE
    )
    
    # Store results in the appropriate sublist
    all_group_t_results[[dataset_name]][[dv]] <- results
    
    # Print the results for the current dataset and dependent variable
    cat("Results for", dv, "in", dataset_name, ":\n")
    print(results$t_test)
    print(results$cohens_d)
    print(results$bayes_factor)
    cat("\n--------------------------------------\n")
  }
}
print(all_group_t_results[["dot_pre"]][["Efficiency_all"]])
print(all_group_t_results[["dot_normal"]][["Efficiency_all"]])
print(all_group_t_results[["dot_pre"]][["accuracy_all"]])
print(all_group_t_results[["dot_normal"]][["accuracy_all"]])
print(all_group_t_results[["dot_pre"]][["median_RT_all"]])
print(all_group_t_results[["dot_normal"]][["median_RT_all"]])

print(all_group_t_results[["num_pre"]][["Efficiency_all"]])
print(all_group_t_results[["num_normal"]][["Efficiency_all"]])
print(all_group_t_results[["num_pre"]][["accuracy_all"]])
print(all_group_t_results[["num_normal"]][["accuracy_all"]])
print(all_group_t_results[["num_pre"]][["median_RT_all"]])
print(all_group_t_results[["num_normal"]][["median_RT_all"]])

######## paired t-tests 
num_TD_prepost <- ttests_w_effectsize(
  data = num_TD,
  dv = "Efficiency_all",
  group = "Time",
  paired = TRUE,
  var_equal = TRUE
)

num_TD_prepost.acc <- ttests_w_effectsize(
  data = num_TD,
  dv = "accuracy_all",
  group = "Time",
  paired = TRUE,
  var_equal = TRUE
)


num_MD_prepost.acc <- ttests_w_effectsize(
  data = num_MD,
  dv = "accuracy_all",
  group = "Time",
  paired = TRUE,
  var_equal = TRUE
)

dot_MD_prepost.acc <- ttests_w_effectsize(
  data = dot_MD,
  dv = "accuracy_all",
  group = "Time",
  paired = TRUE,
  var_equal = TRUE
)

dot_TD_prepost.rt <- ttests_w_effectsize(
  data = dot_TD,
  dv = "median_RT_all",
  group = "Time",
  paired = TRUE,
  var_equal = TRUE
)

dot_MD_prepost.rt <- ttests_w_effectsize(
  data = dot_MD,
  dv = "median_RT_all",
  group = "Time",
  paired = TRUE,
  var_equal = TRUE
)

num_TD_prepost.rt <- ttests_w_effectsize(
  data = num_TD,
  dv = "median_RT_all",
  group = "Time",
  paired = TRUE,
  var_equal = TRUE
)

num_MD_prepost.rt <- ttests_w_effectsize(
  data = num_MD,
  dv = "median_RT_all",
  group = "Time",
  paired = TRUE,
  var_equal = TRUE
)


# Create the plots for each metric
efficiency_all <- plot_data(DF, "Efficiency_all", "Efficiency", c(0.6, 1.3))
accuracy_all <- plot_data(DF, "accuracy_all", "Accuracy", c(0.8, 1))
median_RT_all <- plot_data(DF, "median_RT_all", "Reaction Times", c(600, 1400))

tiff(filename = "revision_figures/efficiency_all_groupxtime.tiff", width=2000,height=1200, units="px", res = 300)
print(efficiency_all)
dev.off()

tiff(filename = "revision_figures/accuracy_all_groupxtime.tiff", width=2000,height=1200,  units="px", res = 300)
print(accuracy_all)
dev.off()

tiff(filename = "revision_figures/median_RT_all_groupxtime.tiff",  width=2000,height=1200,   units="px", res = 300)
print(median_RT_all)
dev.off()

