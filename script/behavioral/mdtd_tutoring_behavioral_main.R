projDir <-""
setwd(projDir)

source("functions.R")
package.list <- c("psych","reshape2","rstatix","lmerTest","lme4","afex","car","dplyr","ggplot2","Hmisc", "purrr",
                  "broom","tidyr","corrplot","ggpubr","Matrix","tibble","ggeffects","effects","stringr","readxl","readr","purrr",
                  "patternplot","ggpattern", "effsize", "BayesFactor", "ez")
load.packages(package.list)



####################### ####################### ####################### ####################### ####################### 
####################### ####################### nonsym-sym  dissimilarities ####################### ####################### 
####################### ####################### ####################### ####################### ####################### 
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

DF_eff <- DF %>%
  select(Subject, Group, Time, task, Efficiency_all, accuracy_all, median_RT_all) %>%
  pivot_wider(
    names_from = task,
    values_from = c(Efficiency_all, accuracy_all, median_RT_all)
  )
DF_eff$Eff.dissim <- abs(DF_eff$Efficiency_all_Nonsymbolic - DF_eff$Efficiency_all_Symbolic)
DF_eff$acc.dissim <- abs(DF_eff$accuracy_all_Nonsymbolic - DF_eff$accuracy_all_Symbolic)
DF_eff$rt.dissim <- abs(DF_eff$median_RT_all_Nonsymbolic - DF_eff$median_RT_all_Symbolic)

DF_eff <- DF_eff %>%
  convert_as_factor(Subject, Group, Time)


DF_eff.pre <- DF_eff[DF_eff$Time == "pre",]
DF_eff.post <- DF_eff[DF_eff$Time == "post",]
DF_eff.md <- DF_eff[DF_eff$Group == "MD",]
DF_eff.td <- DF_eff[DF_eff$Group == "TD",]
DF_eff.normalization <-DF_eff[DF_eff$Group == "MD" & DF_eff$Time == "post"| DF_eff$Group == "TD" & DF_eff$Time == "pre",]


DF_eff$GroupTime <- str_c(DF_eff$Group,' ', DF_eff$Time)
DF_eff_norm <- DF_eff[DF_eff$GroupTime != "TD post",]
DF_eff_norm$GroupTime <- ordered(DF_eff_norm$GroupTime, c( "MD pre", "MD post","TD pre"))

eff_dissim.plot <- create_normalization_plot(DF_eff_norm, "Eff.dissim", "Between-format dissimilarity \n |Nonsymbolic - Symbolic|", y_limit = c(0, 0.25))
acc_dissim.plot <- create_normalization_plot(DF_eff_norm, "acc.dissim", "Between-format dissimilarity \n |Nonsymbolic - Symbolic|", y_limit = c(0, 0.1))
rt_dissim.plot <- create_normalization_plot(DF_eff_norm, "rt.dissim", "Between-format dissimilarity \n |Nonsymbolic - Symbolic|")

tiff(filename = "revision_figures/eff_dissim.plot_norm.tiff", width=1800,height=1400, units="px", res = 300)
print(eff_dissim.plot)
dev.off()

tiff(filename = "revision_figures/acc_dissim.plot_norm.tiff", width=1800,height=1400,  units="px", res = 300)
print(acc_dissim.plot)
dev.off()

tiff(filename = "revision_figures/rt_dissim.plot_norm.tiff",width=1800,height=1400, units="px", res = 300)
print(rt_dissim.plot)
dev.off()


eff_dissim_int.plot <- create_interaction_plot(DF_eff, "Eff.dissim", "Between-format dissimilarity \n |Nonsymbolic - Symbolic|", y_limit = c(0, 0.25))
acc_dissim_int.plot <- create_interaction_plot(DF_eff, "acc.dissim", "Between-format dissimilarity \n |Nonsymbolic - Symbolic| accuracies", y_limit = c(0, 0.1))
rt_dissim_int.plot <- create_interaction_plot(DF_eff, "rt.dissim", "Between-format dissimilarity \n |Nonsymbolic - Symbolic| reaction times")

tiff(filename = "eff_dissim_int.plot.tiff", width=1800,height=1600, units="px", res = 300)
print(eff_dissim_int.plot)
dev.off()

tiff(filename = "acc_dissim_int.plot.tiff", width=1800,height=1600, units="px", res = 300)
print(acc_dissim_int.plot)
dev.off()

tiff(filename = "rt_dissim_int.plot.tiff",width=1800,height=1600, units="px", res = 300)
print(rt_dissim_int.plot)
dev.off()




library(ez)
# For Eff.dissim
anova_eff_dissim <- ezANOVA(
  data = DF_eff,
  dv = Eff.dissim,
  wid = Subject,
  within = .(Time),
  between = .(Group),
  detailed = TRUE,
  type = 3  
)
anova_eff_dissim

# For acc.dissim
anova_acc_dissim <- ezANOVA(
  data = DF_eff,
  dv = acc.dissim,
  wid = Subject,
  within = .(Time),
  between = .(Group),
  detailed = TRUE,
  type = 3  
)

anova_acc_dissim

# For rt.dissim
anova_rt_dissim <- ezANOVA(
  data = DF_eff,
  dv = rt.dissim,
  wid = Subject,
  within = .(Time),
  between = .(Group),
  detailed = TRUE,
  type = 3  
)

anova_rt_dissim

library(BayesFactor)


######### group t-tests dissim ############

datasets <- list(
  DF_eff_pre = DF_eff.pre,
  DF_eff_post = DF_eff.post,
  DF_eff_normalization = DF_eff.normalization
)

dependent_vars <- c("Eff.dissim", "acc.dissim", "rt.dissim")

all_results <- list()

for (dataset_name in names(datasets)) {
  # Initialize a sublist for the current dataset
  all_results[[dataset_name]] <- list()
  
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
    all_results[[dataset_name]][[dv]] <- results
    
    # Print the results for the current dataset and dependent variable
    cat("Results for", dv, "in", dataset_name, ":\n")
    print(results$t_test)
    print(results$cohens_d)
    print(results$bayes_factor)
    cat("\n--------------------------------------\n")
  }
}
print(all_results[["DF_eff_pre"]][["Eff.dissim"]])
print(all_results[["DF_eff_normalization"]][["Eff.dissim"]])
print(all_results[["DF_eff_pre"]][["rt.dissim"]])
print(all_results[["DF_eff_pre"]][["acc.dissim"]])
print(all_results[["DF_eff_post"]][["acc.dissim"]])

print(all_results[["DF_eff_normalization"]][["acc.dissim"]])  
print(all_results[["DF_eff_normalization"]][["rt.dissim"]]) 
print(all_results[["DF_eff_pre"]][["Eff.dissim"]])


########################## paired t-tests pre vs. post ##########################

datasets2 <- list(
  DF_eff_md = DF_eff.md,
  DF_eff_td = DF_eff.td
)

dependent_vars <- c("Eff.dissim", "acc.dissim", "rt.dissim")

all_paired_t_results <- list()


for (dataset_name in names(datasets2)) {
  # Initialize a sublist for the current dataset
  all_paired_t_results[[dataset_name]] <- list()
  
  # Inner loop over dependent variables
  for (dv in dependent_vars) {
    # Run the analysis for each dataset and dependent variable
    results <- ttests_w_effectsize(
      data = datasets[[dataset_name]],
      dv = dv,
      group = "Time",
      paired = TRUE,
      var_equal = TRUE
    )
    
    # Store results in the appropriate sublist
    all_paired_t_results[[dataset_name]][[dv]] <- results
    
    # Print the results for the current dataset and dependent variable
    cat("Results for", dv, "in", dataset_name, ":\n")
    print(results$t_test)
    print(results$cohens_d)
    print(results$bayes_factor)
    cat("\n--------------------------------------\n")
  }
}





### fluency '##### 
beh <- read_excel("../fluency.xlsx")

fluency <- melt(beh[,c(1:2,9:10)], id.vars = c("participant","group"), variable.name = "Time")
fluency <- na.omit(total)
fluency$Time <- ifelse(total$Time=="fluency.pre", "pre", "post")
fluency$Time  <- ordered(total$Time , c("pre", "post"))
colnames(fluency) <- c("Subject","Group","Time", "fluency")
colnames(DF_eff)
DF_eff_wide <- DF_eff %>%
  select(Subject, Group,Time, Efficiency_all_Nonsymbolic, Efficiency_all_Symbolic, 
         accuracy_all_Nonsymbolic,accuracy_all_Symbolic, median_RT_all_Nonsymbolic, median_RT_all_Symbolic,
         Eff.dissim, acc.dissim,rt.dissim ) %>%
  pivot_wider(
    names_from = Time,
    values_from = c(Efficiency_all_Nonsymbolic, Efficiency_all_Symbolic, 
                    accuracy_all_Nonsymbolic,accuracy_all_Symbolic, median_RT_all_Nonsymbolic, median_RT_all_Symbolic,
                    Eff.dissim, acc.dissim,rt.dissim)
  )

fluency_wide <- fluency %>%
  pivot_wider(
    names_from = Time,
    values_from = fluency
  )
colnames(fluency_wide) <- c("Subject","Group","fluency.pre", "fluency.post")

write.csv(dissim_corr, "dissim_corr_revision.csv")

dissim_corr <- merge(DF_eff_wide,fluency_wide , by =c("Subject",  "Group"))
colnames(dissim_corr)

dissim_corr$ch_Eff_dissim <- dissim_corr$Eff.dissim_post - dissim_corr$Eff.dissim_pre
dissim_corr$ch_flu <- dissim_corr$fluency.post - dissim_corr$fluency.pre
dissim_corr$ch_acc_dissim <- dissim_corr$acc.dissim_post - dissim_corr$acc.dissim_pre
dissim_corr$ch_rt_dissim <- dissim_corr$rt.dissim_post - dissim_corr$rt.dissim_pre

dissim_corr_MD <- dissim_corr[dissim_corr$Group == "MD",]
dissim_corr_TD <- dissim_corr[dissim_corr$Group == "TD",]
rcorr(as.matrix(dissim_corr_MD[,c(23:31)]))


# Apply function to each pair of variables
correlation_results <- list(
  Eff_dissim = cor_summary(dissim_corr, ch_flu, ch_Eff_dissim),
  rt_dissim = cor_summary(dissim_corr, ch_flu, ch_rt_dissim),
  Eff_sym = cor_summary(dissim_corr, ch_flu, ch_Eff_sym),
  Eff_nonsym = cor_summary(dissim_corr, ch_flu, ch_Eff_nonsym)

)



########################## normalization  ####################### 

library(stringr)
fluency$GroupTime <- str_c(total$Group,' ', total$Time)
flu_norm <- total[total$GroupTime != "TD post",]
flu_norm$GroupTime <- ordered(flu_norm$GroupTime, c( "MD pre", "MD post","TD pre"))

anova_flu  <- anova_test(
  data =  total , dv = fluency , wid = Subject,
  within = Time, between = Group
)
get_anova_table(anova_flu )



TD_flu <- total[total$Group=="TD" ,]
MD_flu <- total[total$Group=="MD" ,]

MD_flu$Flu_criteria <- 90
Post_flu <- total[total$Time=="post" ,]
Pre_flu <- total[total$Time=="pre" ,]
flu_norm <- total[total$Group == "MD" & total$Time == "post"| total$Group == "TD"&total$Time == "pre",]


datasets_fluency <- list(
  Pre_flu = Pre_flu,
  Post_flu = Post_flu,
  flu_norm = flu_norm
)

fluency_group_ttest <- list()

for (dataset_name in names(datasets_fluency)) {
  
  fluency_group_ttest[[dataset_name]] <- list()

  results <- ttests_w_effectsize(
    data = datasets_fluency[[dataset_name]],
    dv = "fluency",
    group = "Group",
    paired = FALSE,
    var_equal = TRUE
  )
  
  # Store results in the appropriate sublist
  fluency_group_ttest[[dataset_name]][["fluency"]] <- results
  
  # Print the results for the current dataset and dependent variable
  cat("Results for fluency in", dataset_name, ":\n")
  print(results$t_test)
  print(results$cohens_d)
  print(results$bayes_factor)
  cat("\n--------------------------------------\n")
  }

print(fluency_group_ttest[["Pre_flu"]])
print(fluency_group_ttest[["Pre_flu"]])
print(fluency_group_ttest[["flu_norm"]])



flu_bar <- total %>%
  dplyr::group_by(Group,Time)%>%
  dplyr::summarise(Avg=mean(fluency,na.rm=T),
                   Std=sd(fluency),
                   n=n())%>%
  mutate(Stderr=Std/sqrt(n))%>%
    ggplot(aes(x=Group,y=Avg, group=Time, fill=Group, pattern=Time))+
  geom_bar_pattern(stat = "identity",position = position_dodge(preserve = "single"),
                   color = "black", 
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.1,
                   pattern_spacing = 0.025,
                   pattern_key_scale_factor = 0.6) +
  scale_fill_manual(values = c("dodgerblue", "tomato1"))+
  scale_pattern_manual(values = c(pre = "none", post = "stripe")) +
  theme_classic() + 
  theme(strip.text = element_text(size=14))+ 
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14))  +
  geom_errorbar(aes(ymax=Avg+Stderr,
                    ymin=Avg-Stderr),width=0.2, position=position_dodge(0.9)) +
  geom_hline(yintercept=90, linetype="dashed", color = "black") +
  scale_y_continuous(name = "Score" )+  ggtitle("Arithmetic Fluency") + coord_cartesian(ylim = c(60,105)) +
  theme(axis.title.x = element_text(colour="Black", size=16, face = "bold"),  axis.text.x  = element_text(angle=0, vjust=0.5, size=16, face = "bold")) +
  theme(axis.title.y = element_text(colour="Black", size=16, face = "bold"),  axis.text.y  = element_text(angle=0, vjust=0.5, size=16, face = "bold")) + 
  theme(plot.title = element_text(hjust = 0.5,color="Black", size=16, face = "bold"))+
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none")))


 ggarrange(eff_bar, flu_bar, 
          labels = c("A", "B"),
          ncol = 2, nrow = 1)


