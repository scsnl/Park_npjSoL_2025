 
projDir <- "~/Library/CloudStorage/Box-Box/YP Projects/MathFun/0_npj_revision2_md_td_tutoring/roi"
setwd(projDir)
source("../../script/functions_basic_stats.R")
package.list <- c("psych","reshape2","lmerTest","lme4","afex","car","QuantPsyc","dplyr","ggplot2","Hmisc",
                  "broom","tidyr","corrplot","ggpubr","Matrix","tibble","ggeffects","effects","stringr","ggpattern", "effsize","rstatix",
                  "stringr")
load.packages(package.list)

rm(list=ls())

group = read.csv("GroupAssignment.csv")

############################### different learning trajectories - interaction ####################


pre = read.table("data/16md_pre_post_vs_20td_pre_post_Pre/roi_beta_average.tsv", header = TRUE)
post =read.table("data/16md_pre_post_vs_20td_pre_post_Post/roi_beta_average.tsv", header = TRUE)

pre$Time <- "pre"
post$Time <- "post"

total <- rbind(pre, post)
merged <- merge(group, total , by=c("Subject","Time"))
merged$Time <- ordered(merged$Time, c("pre","post"))


colnames(merged)
int <- merged[,c(1:3,10:14)]


library(stringr)

int_figure <-int[,c("Subject","Time","Group" , "L Cerebellum", "L FG", "L PHG")]


int_long <-int_figure %>% gather(Regions,NRS,c(4:6))
int_long$Regions <- ordered(int_long$Regions,c("L PHG", "L FG", "L Cerebellum") )

interactions<-int_long  %>%
  dplyr::group_by(Time, Group,Regions)%>%
  dplyr::summarise(Avg=mean(NRS,na.rm=T),
                   Std=sd(NRS),
                   n=n())%>%
  mutate(Stderr=Std/sqrt(n))%>%
  ggplot(aes(x=Group,y=Avg, group=Time, fill=Group, pattern=Time))+
  facet_wrap(~Regions, nrow = 1) +
  geom_bar_pattern(stat = "identity",position = position_dodge(preserve = "single"),
                   color = "black", 
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.1,
                   pattern_spacing = 0.025,
                   pattern_key_scale_factor = 0.6) +
  scale_fill_manual(values = c("dodgerblue", "tomato1"))+
  scale_pattern_manual(values = c(pre = "none", post = "stripe")) +
  coord_cartesian(ylim = c(-0.4, 0.45))+
  theme_classic() + 
  theme(strip.text = element_text(size=12, face = "bold"))+ 
  theme(legend.position="none") +
  geom_errorbar(aes(ymax=Avg+Stderr,
                    ymin=Avg-Stderr),width=0.2, position=position_dodge(0.9)) +
  scale_y_continuous(name = "cross-format NRS" )+ 
  theme(axis.title.x = element_text(colour="Black", size=10, face = "bold"),  axis.text.x  = element_text(angle=0, vjust=0.5, size=12, face = "bold")) +
  theme(axis.title.y = element_text(colour="Black", size=12, face = "bold"),  axis.text.y  = element_text(angle=0, vjust=0.5, size=12, face = "bold")) + 
  theme(plot.title = element_text(hjust = 0.5,color="Black", size=13, face = "bold"))+
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none")))


tiff(filename = "rois_interactions.tiff", width=1800,height=1000, units="px", res = 300)
print(interactions)
dev.off()


####  rois anova MD (post-pre) vs. TD (post-pre) ######
colnames(int)
int_df = int

anova_df = int_df
colnames(anova_df)


col.names <- c("df", "statistic","p.values")
row.names <- c("Time","Group","Time:Group","Residuals")
matrix.names  <- c("L Cerebellum", "L FG", "L Cerebellum -20 -70 -28", "L PHG", "L Cerebellum -16 -46 -19") 
output <- array(dim = c(4,3,5),dimnames = list(row.names,col.names,matrix.names))

for (i in 1:5){
  anova = aov( anova_df[,3+i]  ~ Time*Group , data=anova_df)
  result = anova %>% broom::tidy(.)
  output[,1,i] <- result$df
  output[,2,i] <- result$statistic
  output[,3,i] <- round(result$p.value, 3)
  
}



aov.roi.result <-as.data.frame(output)



############################ t-tests ROIs for anova results #################### 
int_MD <- int[int$Group =="MD" ,]
int_TD <- int[int$Group =="TD" ,]
int_pre <- int[int$Time =="pre" ,]
int_post <- int[int$Time =="post" ,]


#### for group t-tests ###### 
datasets <- list(int_pre = int_pre, int_pre = int_post)
labels <- c("pre MD vs. TD", "post MD vs. TD")

# Initialize a list to store results
results <- list()


for (i in 1:length(datasets)) {
  t_df <- datasets[[i]]
  
  # Initialize the output matrix
  output <- matrix(ncol=7, nrow=5)
  output[,1] <- c("L Cerebellum", "L FG", "L Cerebellum -20 -70 -28", "L PHG", "L Cerebellum -16 -46 -19") 
  colnames(output) <- c("regions", "df", "t", "d", "lower", "upper", "p")
  
  # Perform t-tests and compute effect sizes
  for (j in 1:5) {
    t <- t.test(t_df[, 3 + j] ~ Group, data=t_df)
    cohensD <- cohen.d(t_df[, 3 + j] ~ t_df$Group, var.equal=TRUE)
    
    output[j, 2:7] <- c(t$parameter, t$statistic, cohensD$estimate, 
                        cohensD$conf.int[1], cohensD$conf.int[2], t$p.value)
  }
  
  # Convert to data frame and apply FDR correction
  output_df <- as.data.frame(output)
  fdr <- p.adjust(as.numeric(output_df$p), "fdr")
  output_df$fdr <- fdr
  
  # Add stats label
  output_df$stats <- labels[i]
  
  # Store the result in the list
  results[[i]] <- output_df
}

# Combine results from both datasets
final_output_group_ttest <- do.call(rbind, results)



#### for paired t-tests ######
datasets <- list(int_MD = int_MD, int_TD = int_TD)
labels <- c("MD pre vs. post", "TD pre vs. post")

# Initialize a list to store results
results <- list()


for (i in 1:length(datasets)) {
  t_df <- datasets[[i]]
  
  # Initialize the output matrix
  output <- matrix(ncol=7, nrow=5)
  output[,1] <- c("L Cerebellum", "L FG", "L Cerebellum -20 -70 -28", "L PHG", "L Cerebellum -16 -46 -19") 
  colnames(output) <- c("regions", "df", "t", "d", "lower", "upper", "p")
  
  # Perform t-tests and compute effect sizes
  for (j in 1:5) {
    t <- t.test(t_df[, 3 + j][t_df$Time == "pre"], t_df[, 3 + j][t_df$Time == "post"], paired = TRUE)
    cohensD <- cohen.d(t_df[, 3 + j] ~ t_df$Time, var.equal=TRUE)
    
    output[j, 2:7] <- c(
      round(as.numeric(t$parameter), 2),
      round(as.numeric(t$statistic),2),
      round(as.numeric(cohensD$estimate),2),
      as.numeric(cohensD$conf.int[1]), 
      as.numeric(cohensD$conf.int[2]), 
      round(as.numeric(t$p.value),3)
    ) 
  }
  
  # Convert to data frame and apply FDR correction
  output_df <- as.data.frame(output)
  fdr <- p.adjust(as.numeric(output_df$p), "fdr")
  output_df$fdr <- fdr
  
  # Add stats label
  output_df$stats <- labels[i]
  
  # Store the result in the list
  results[[i]] <- output_df
}

# Combine results from both datasets
final_output_paired_ttest <- do.call(rbind, results)






