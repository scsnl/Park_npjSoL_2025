#functions for loading packages, labeling boxplot outliers, and creating and saving pairwise and listwise deleted correlation tables, box plots, histograms, scatter plots, bar graphs, and multi-individual line graphs####

#package loading function####
load.packages <- function(package.list){ 
new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
for (i in 1:length(package.list)){
library(package.list[i],character.only=TRUE)
}
}

ttests_w_effectsize <- function(data, dv, group, paired = FALSE, var_equal = TRUE) {
  
  formula <- as.formula(paste(dv, "~", group))
  
  # Perform the t-test
  t_test_result <- t.test(formula, data = data, paired = paired, var.equal = var_equal)
  
  # Calculate Cohen's d
  cohen_d_result <- cohen.d(data[[dv]], data[[group]], paired = paired)
  
  # Calculate Bayesian factor
  bayes_factor_result <- ttestBF(formula = formula, data = data)
  
  # Return results as a list
  list(
    t_test = t_test_result,
    cohens_d = cohen_d_result,
    bayes_factor = bayes_factor_result
  )
}


plot_data <- function(data, y_var, y_label, y_limits) {
  data %>%
    group_by(Group, Time, task) %>%
    summarise(Avg = mean(!!sym(y_var), na.rm = TRUE),
              Std = sd(!!sym(y_var), na.rm = TRUE),
              n = n()) %>%
    mutate(Stderr = Std / sqrt(n)) %>%
    ggplot(aes(x = Group, y = Avg, group = Time, fill = Group, pattern = Time)) +
    geom_bar_pattern(
      stat = "identity", position = position_dodge(preserve = "single"),
      color = "black", 
      pattern_fill = "black",
      pattern_angle = 45,
      pattern_density = 0.1,
      pattern_spacing = 0.025,
      pattern_key_scale_factor = 0.6
    ) +
    scale_fill_manual(values = c("dodgerblue", "tomato1")) +
    scale_pattern_manual(values = c(pre = "none", post = "stripe")) +
    facet_wrap(~task) +
    theme_classic() +
    theme(
      strip.text = element_text(size = 16, face = "bold"),
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      axis.title.x = element_text(colour = "Black", size = 16, face = "bold"),
      axis.text.x = element_text(angle = 0, vjust = 0.5, size = 16, face = "bold"),
      axis.title.y = element_text(colour = "Black", size = 16, face = "bold"),
      axis.text.y = element_text(angle = 0, vjust = 0.5, size = 16, face = "bold"),
      plot.title = element_text(hjust = 0.5, color = "Black", size = 16, face = "bold")
    ) +
    geom_errorbar(aes(ymax = Avg + Stderr, ymin = Avg - Stderr), width = 0.2, position = position_dodge(0.9)) +
    coord_cartesian(ylim = y_limits) +
    scale_y_continuous(name = y_label) +
    guides(
      pattern = guide_legend(override.aes = list(fill = "white")),
      fill = guide_legend(override.aes = list(pattern = "none"))
    )
}

create_normalization_plot <- function(data, value_column, y_axis_label, y_limit = NULL) {
  data %>%
    group_by(Time, GroupTime) %>%
    summarise(
      Avg = mean(!!sym(value_column), na.rm = TRUE),
      Std = sd(!!sym(value_column), na.rm = TRUE),
      n = n()
    ) %>%
    mutate(Stderr = Std / sqrt(n)) %>%
    ggplot(aes(x = GroupTime, y = Avg, group = Time, fill = GroupTime, pattern = Time)) +
    geom_bar_pattern(
      stat = "identity", position = position_dodge(preserve = "single"),
      color = "black", pattern_fill = "black",
      pattern_angle = 45, pattern_density = 0.1,
      pattern_spacing = 0.025, pattern_key_scale_factor = 0.6
    ) +
    scale_fill_manual(values = c("dodgerblue", "dodgerblue", "tomato1")) +
    scale_pattern_manual(values = c(pre = "none", post = "stripe")) +
    coord_cartesian(ylim = y_limit) +
    theme_classic() +
    theme(
      strip.text = element_text(size = 16, face = "bold"),
      legend.position = "none",
      axis.title.x = element_text(colour = "Black", size = 12, face = "bold"),
      axis.text.x = element_text(angle = 0, vjust = 0.5, size = 12, face = "bold"),
      axis.title.y = element_text(colour = "Black", size = 16, face = "bold"),
      axis.text.y = element_text(angle = 0, vjust = 0.5, size = 16, face = "bold"),
      plot.title = element_text(hjust = 0.5, color = "Black", size = 16, face = "bold")
    ) +
    geom_errorbar(aes(ymax = Avg + Stderr, ymin = Avg - Stderr), width = 0.2, position = position_dodge(0.9)) +
    scale_y_continuous(name = y_axis_label) +
    guides(
      pattern = guide_legend(override.aes = list(fill = "white")),
      fill = guide_legend(override.aes = list(pattern = "none"))
    )
}

create_interaction_plot <- function(data, value_column, y_axis_label, y_limit = NULL) {
  data %>%
    group_by(Time, Group) %>%
    summarise(
      Avg = mean(!!sym(value_column), na.rm = TRUE),
      Std = sd(!!sym(value_column), na.rm = TRUE),
      n = n()
    ) %>%
    mutate(Stderr = Std / sqrt(n)) %>%
    ggplot(aes(x = Group, y = Avg, group = Time, fill = Group, pattern = Time)) +
    geom_bar_pattern(
      stat = "identity", position = position_dodge(preserve = "single"),
      color = "black", pattern_fill = "black",
      pattern_angle = 45, pattern_density = 0.1,
      pattern_spacing = 0.025, pattern_key_scale_factor = 0.6
    ) +
    scale_fill_manual(values = c("dodgerblue",  "tomato1")) +
    scale_pattern_manual(values = c(pre = "none", post = "stripe")) +
    coord_cartesian(ylim = y_limit) +
    theme_classic() +
    theme(
      strip.text = element_text(size = 16, face = "bold"),
      legend.position = "none",
      axis.title.x = element_text(colour = "Black", size = 12, face = "bold"),
      axis.text.x = element_text(angle = 0, vjust = 0.5, size = 12, face = "bold"),
      axis.title.y = element_text(colour = "Black", size = 16, face = "bold"),
      axis.text.y = element_text(angle = 0, vjust = 0.5, size = 16, face = "bold"),
      plot.title = element_text(hjust = 0.5, color = "Black", size = 16, face = "bold")
    ) +
    geom_errorbar(aes(ymax = Avg + Stderr, ymin = Avg - Stderr), width = 0.2, position = position_dodge(0.9)) +
    scale_y_continuous(name = y_axis_label) +
    guides(
      pattern = guide_legend(override.aes = list(fill = "white")),
      fill = guide_legend(override.aes = list(pattern = "none"))
    )
}


cor_summary <- function(data, x, y) {
  data %>%
    group_by(Group) %>%
    summarise(
      COR = cor({{ x }}, {{ y }}),
      p = cor.test({{ x }}, {{ y }})$p.value,
      n = n(),
      .groups = "drop"
    )
}
