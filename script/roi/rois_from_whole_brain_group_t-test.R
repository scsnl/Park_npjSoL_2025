projDir <- "~/npj"
setwd(projDir)
source("../../script/functions_basic_stats.R")
package.list <- c("psych","reshape2","lmerTest","lme4","afex","car","QuantPsyc","dplyr","ggplot2","Hmisc",
                  "broom","tidyr","corrplot","ggpubr","Matrix","tibble","ggeffects","effects","stringr","ggpattern", "effsize","rstatix",
                  "stringr")
load.packages(package.list)

rm(list=ls())

group = read.csv("GroupAssignment.csv")


################### Normalization - from whole brain group differences at pre ####################

pre =read.table("data/20td_pre_vs_16md_pre_Pre/roi_beta_average.tsv", header = TRUE)
post =read.table("data/20td_pre_vs_16md_pre_Post/roi_beta_average.tsv", header = TRUE)

pre$Time <- "pre"
post$Time <- "post"

total <- rbind(pre, post)
merged <- merge(group, total , by=c("Subject","Time"))
merged$Time <- ordered(merged$Time, c("pre","post"))
colnames(merged)

norm<- merged[,c(1:3,6,10:19)]


norm$GroupTime <- str_c(norm$Group,' ', norm$Time)

norm_main <- norm[norm$GroupTime != "TD post",]
norm_main <- norm_main[ ,c(1:2,15, 5:14)]


norm_figure <- norm_main[ ,c("Subject", "Time","GroupTime", "R Cerebellum", "L PHG", "L LOC/IPS", "L SPL/IPS", "L Premotor", "L PreCG")]

norm_long <- norm_figure %>% gather(Regions,NRS,c(4:9))
norm_long$GroupTime <- ordered(norm_long$GroupTime, c("MD pre","MD post", "TD pre"))
norm_long$Regions <- ordered(norm_long$Regions, c("L LOC/IPS", "L SPL/IPS","L PHG", "L PreCG" ,"L Premotor","R Cerebellum" ))

figure3B <- norm_long  %>%
  dplyr::group_by(Time, GroupTime,  Regions)%>%
  dplyr::summarise(Avg=mean(NRS,na.rm=T),
                   Std=sd(NRS),
                   n=n())%>%
  mutate(Stderr=Std/sqrt(n))%>%
  ggplot(aes(x=GroupTime,y=Avg, group=Time, fill=GroupTime, pattern=Time))+
  facet_wrap(~Regions, nrow =2) +
  geom_bar_pattern(stat = "identity",position = position_dodge(preserve = "single"),
                   color = "black", 
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.1,
                   pattern_spacing = 0.025,
                   pattern_key_scale_factor = 0.6) +
  scale_fill_manual(values = c("dodgerblue", "dodgerblue","tomato1"))+
  scale_pattern_manual(values = c(pre = "none", post = "stripe")) +
  coord_cartesian(ylim = c(-0.5, 0.5))+
  theme_classic() + 
  theme(strip.text = element_text(size=12, face = "bold"))+ 
  theme(legend.position="none") +
  geom_errorbar(aes(ymax=Avg+Stderr,
                    ymin=Avg-Stderr),width=0.2, position=position_dodge(0.9)) +
  scale_y_continuous(name = "cross-format NRS" )+ 
  theme(axis.title.x = element_text(colour="Black", size=10, face = "bold"),  axis.text.x  = element_text(angle=0, vjust=0.5, size=11, face = "bold")) +
  theme(axis.title.y = element_text(colour="Black", size=14, face = "bold"),  axis.text.y  = element_text(angle=0, vjust=0.5, size=12, face = "bold")) + 
  theme(plot.title = element_text(hjust = 0.5,color="Black", size=10, face = "bold"))+
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none")))


tiff(filename = "figure3B.tiff", width=1800,height=1350, units="px", res = 300)

print(figure3B)
dev.off()




#### normalization test  MD post vs TD pre - residuals ######
norm_MDpostTDpre <- norm_main[norm_main$GroupTime !="MD pre",]
norm_MDpreTDpre <- norm_main[norm_main$GroupTime !="MD post",]



t_df = norm_MDpreTDpre
t_df$GroupTime<-as.factor(t_df$GroupTime)
colnames(t_df)

names = colnames(t_df[,c(4:13)])

output <- matrix(ncol=6, nrow=10)


colnames(output) <- c("regions", "tstats" ,"d", "lower", "upper", "p")

for (i in 1:10){
  t = t.test( t_df[,3+i]  ~GroupTime  , data=t_df)
  cohensD = cohen.d(t_df[,3+i]  ~ t_df$GroupTime, var.equal = TRUE)
  output[i,2] <- round(t$statistic, 3)
  output[i,3] <- round(cohensD$estimate, 2)
  output[i,4] <- cohensD$conf.int[1]
  output[i,5] <- cohensD$conf.int[2]
  output[i,6] <-round(t$p.value, 3)
}
output<-as.data.frame(output)

uncorrected <- as.numeric(matrix(unlist(output[,6]))[,1])
fdr <- p.adjust(uncorrected,"fdr")
fdr <- as.data.frame(fdr)
fdr <- round(fdr, 3)

MDpreTDpre_p005<-cbind(output,fdr)
MDpostTDpre_p005<-cbind(output,fdr)



############################ t-tests ROIs for pre vs post #################### 
norm_MD <- norm[norm$Group =="MD" ,]
norm_TD <- norm[norm$Group =="TD" ,]


#### for paired t-tests ######
datasets <- list(norm_MD = norm_MD, norm_TD = norm_TD)
labels <- c("MD pre vs. post", "TD pre vs. post")

# Initialize a list to store results
results <- list()

for (i in 1:length(datasets)) {
  t_df <- datasets[[i]]
  
  # Initialize the output matrix
  output <- matrix(ncol=7, nrow=10)
  #output[,1] <- c()
  colnames(output) <- c("regions", "df", "t", "d", "lower", "upper", "p")
  
  # Perform t-tests and compute effect sizes
  for (j in 1:10) {
    t <- t.test(t_df[, 4 + j][t_df$Time == "pre"], t_df[, 4 + j][t_df$Time == "post"], paired = TRUE)
    cohensD <- cohen.d(t_df[, 4 + j] ~ t_df$Time, var.equal=TRUE)
    
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
  output_df$fdr <- round(fdr,3)
  
  # Add stats label
  output_df$stats <- labels[i]
  
  # Store the result in the list
  results[[i]] <- output_df
}

# Combine results from both datasets
final_output_paired_ttest <- do.call(rbind, results)

