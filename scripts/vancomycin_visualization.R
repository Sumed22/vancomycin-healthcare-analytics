# Vancomycin Healthcare Analytics & Data Visualization
# Author: Sumed Seeyakmani
# Description:
# This script contains the R workflow used to generate visualizations
# for Vancomycin therapy analysis in ICU patients.
#
# Note:
# The original clinical dataset is not included due to privacy considerations.
# File paths and dataset loading steps may need to be adjusted before running.


# Load necessary libraries
library(ggplot2)
library(dplyr)
library(patchwork)  # For arranging plots
library(RColorBrewer)  # Improved color palettes
library(reshape2)   # For heatmap data transformation
library(tidyr)

#Load dataset
load("C:/Users/Sumed Seeyakmani/Downloads/vancomycin.RData")
setwd("C:/Users/Sumed Seeyakmani/Downloads")
write.csv(dat, "vancomycin.csv", row.names = FALSE )
df<-dat

# ---- Detect Missing Values ----
missing_summary <- colSums(is.na(df))

# Print the summary of missing values per column
cat("Missing values per column:\n")
print(missing_summary)

# Show only columns that contain missing values
columns_with_na <- names(missing_summary[missing_summary > 0])
cat("\nColumns with missing values:\n")
print(columns_with_na)

# Remove missing values for critical columns (C24, C48, C72, SAPS, SOFA)


df_clean <- df %>%
  filter(!is.na(C24), !is.na(C48), !is.na(C72), !is.na(SAPS), !is.na(SOFA))

# Select relevant columns for analysis
df_selected <- df_clean %>% select(Culture, C24, C48, C72)

# Convert Culture to a categorical variable
df_selected$Culture <- as.factor(df_selected$Culture)

# Remove missing and infinite values
df_selected <- df_selected %>%
  filter_all(all_vars(!is.na(.))) %>%
  filter_all(all_vars(!is.infinite(.)))

# Convert data to long format for Violin Plot
df_long <- melt(df_selected, id.vars = "Culture", variable.name = "Timepoint", value.name = "Vancomycin_Level")

# Generate Violin Plot with renamed x-axis
violin_plot <- ggplot(df_long, aes(x = Timepoint, y = Vancomycin_Level, fill = Culture)) +
  geom_violin(trim = FALSE, alpha = 0.7) +  # Violin plot to show distribution
  geom_boxplot(width = 0.2, position = position_dodge(0.9), outlier.shape = NA, alpha = 0.7) + # Boxplot inside violin
  scale_fill_manual(values = c("red", "green")) +  # Blue = Resistant, Red = Sensitive
  scale_x_discrete(labels = c("C24" = "24H", "C48" = "48H", "C72" = "72H")) +  # Rename x-axis
  labs(title = "Vancomycin Serum Levels by Bacterial Sensitivity",
       x = "Timepoint After Start of Therapy",
       y = "Vancomycin Serum Level (mg/L)",
       fill = "Bacterial Sensitivity") +
  theme_minimal() +
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5, color = "black"),
        axis.title.x = element_text(size = 22, face = "bold", color = "black"),
        axis.title.y = element_text(size = 22, face = "bold", color = "black"),
        axis.text.x = element_text(size = 20, face = "bold", color = "black"),
        axis.text.y = element_text(size = 20, face = "bold", color = "black"),
        legend.text = element_text(size = 19, face = "bold"),
        legend.title = element_text(size = 19, face = "bold"),
        legend.background = element_rect(fill = "white", color = "black")
        )

print(violin_plot)
# Define output directory (Update path accordingly)
output_dir <- "C:/Users/Sumed Seeyakmani/Downloads/"
# Save violin plots
ggsave(filename = paste0(output_dir, "violin_plot.pdf"), plot = violin_plot, dpi = 300, width = 15, height = 10)#This is the plot that I used in the report



# ---- Categorize SAPS and SOFA into severity groups (REORDERED) ----
# Compute Quantiles for SAPS and SOFA
saps_quantiles <- quantile(df_clean$SAPS, probs = c(1/3, 2/3), na.rm = TRUE)
sofa_quantiles <- quantile(df_clean$SOFA, probs = c(1/3, 2/3), na.rm = TRUE)

# Categorize SAPS and SOFA Based on Their Quantiles
df_clean <- df_clean %>%
  mutate(
    SAPS_Category = factor(case_when(
      SAPS <= saps_quantiles[1] ~ "Low",
      SAPS > saps_quantiles[1] & SAPS <= saps_quantiles[2] ~ "Medium",
      SAPS > saps_quantiles[2] ~ "High"
    ), levels = c("Low", "Medium", "High")),  
    
    SOFA_Category = factor(case_when(
      SOFA <= sofa_quantiles[1] ~ "Low",
      SOFA > sofa_quantiles[1] & SOFA <= sofa_quantiles[2] ~ "Medium",
      SOFA > sofa_quantiles[2] ~ "High"
    ), levels = c("Low", "Medium", "High"))
  )

# ---- Define Improved Color Palettes (Using RColorBrewer) ----
saps_colors <- brewer.pal(3, "Blues")  # Blues for SAPS (colorblind-friendly)
sofa_colors <- brewer.pal(3, "Reds")   # Reds for SOFA (colorblind-friendly)

# ---- Determine common y-axis limits across all Vancomycin concentrations ----
y_min <- min(df_clean$C24, df_clean$C48, df_clean$C72, na.rm = TRUE)
y_max <- max(df_clean$C24, df_clean$C48, df_clean$C72, na.rm = TRUE)
y_limits <- c(floor(y_min), ceiling(y_max))

# ---- Function to format boxplots with consistent y-axis limits ----
format_boxplot_with_median <- function(data, x_var, y_var, fill_var, title, colors, y_limits = NULL) {
  medians <- data %>%
    group_by(!!sym(x_var)) %>%
    summarise(median_value = median(!!sym(y_var), na.rm = TRUE))
  
  p <- ggplot(data, aes(x = !!sym(x_var), y = !!sym(y_var), fill = !!sym(fill_var))) +
    geom_boxplot(coef = 1.5, width = 0.6, fatten = 1.2, outlier.shape = 16, 
                 outlier.size = 2, outlier.alpha = 0.6, staplewidth = 1) +
    geom_text(data = medians, aes(x = !!sym(x_var), y = median_value, 
                                  label = round(median_value, 1)),
              position = position_dodge(width = 0.75), vjust = -0.5, 
              size = 5, fontface = "bold") +
    labs(title = title, x = "Severity Category by Quantile", y = "Vancomycin Concentration (mg/L)") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 20, face = "bold", hjust = 0.5, color = "black"),
      axis.title.x = element_text(size = 18, face = "bold", color = "black"),
      axis.title.y = element_text(size = 18, face = "bold", color = "black"),
      axis.text.x = element_text(size = 18, face = "bold", color = "black"),
      axis.text.y = element_text(size = 18, face = "bold", color = "black"),
      legend.position = "none"
    )+
    scale_fill_manual(values = colors)
  
  if (!is.null(y_limits)) {
    p <- p + scale_y_continuous(limits = y_limits)
  }
  
  return(p)
}

# ---- Create Boxplots Using New Quantile-Based Categories ----
p_c24_saps <- format_boxplot_with_median(df_clean, "SAPS_Category", "C24", "SAPS_Category", "C24 Across SAPS Severity", saps_colors, y_limits)
p_c24_sofa <- format_boxplot_with_median(df_clean, "SOFA_Category", "C24", "SOFA_Category", "C24 Across SOFA Severity", sofa_colors, y_limits)
p_c48_saps <- format_boxplot_with_median(df_clean, "SAPS_Category", "C48", "SAPS_Category", "C48 Across SAPS Severity", saps_colors, y_limits)
p_c48_sofa <- format_boxplot_with_median(df_clean, "SOFA_Category", "C48", "SOFA_Category", "C48 Across SOFA Severity", sofa_colors, y_limits)
p_c72_saps <- format_boxplot_with_median(df_clean, "SAPS_Category", "C72", "SAPS_Category", "C72 Across SAPS Severity", saps_colors, y_limits)
p_c72_sofa <- format_boxplot_with_median(df_clean, "SOFA_Category", "C72", "SOFA_Category", "C72 Across SOFA Severity", sofa_colors, y_limits)

# ---- Arrange All Plots in a Grid ----
final_plot <- (p_c24_saps | p_c48_saps | p_c72_saps) /
  (p_c24_sofa | p_c48_sofa | p_c72_sofa)

saps_only <- (p_c24_saps | p_c48_saps | p_c72_saps) 
sofa_only <- (p_c24_sofa | p_c48_sofa | p_c72_sofa)

# ---- Print the combined plot ----
print(final_plot)
print(saps_only)
print(sofa_only)



# Define output directory (Update path accordingly)
output_dir <- "C:/Users/Sumed Seeyakmani/Downloads/"

# Save combined plots
#ggsave(filename = paste0(output_dir, "final_plot.pdf"), plot = final_plot, dpi = 300, width = 17, height = 10)
ggsave(filename = paste0(output_dir, "final_plot_narrow.pdf"), plot = final_plot, dpi = 300, width = 12, height = 10)#This is the plot that I used in the report
#ggsave(filename = paste0(output_dir, "saps_only.pdf"), plot = saps_only, dpi = 300, width = 10, height = 5)
#ggsave(filename = paste0(output_dir, "sofa_only.pdf"), plot = sofa_only, dpi = 300, width = 10, height = 5)



# Reshape eGFR data from wide to long format
df_eGFR_long <- df_clean %>%
  select(SAPS_Category, SOFA_Category, eGFRStart, eGFR24, eGFR48, eGFR72, eGFREnd) %>%
  pivot_longer(cols = starts_with("eGFR"), 
               names_to = "Timepoint", 
               values_to = "eGFR")

# Ensure correct order for Timepoint
df_eGFR_long <- df_eGFR_long %>%
  mutate(Timepoint = factor(Timepoint, levels = c("eGFRStart", "eGFR24", "eGFR48", "eGFR72", "eGFREnd")))

# Compute Median & IQR for SAPS
saps_eGFR_summary <- df_eGFR_long %>%
  group_by(SAPS_Category, Timepoint) %>%
  summarise(
    median_eGFR = median(eGFR, na.rm = TRUE),
    Q1 = quantile(eGFR, 0.25, na.rm = TRUE),
    Q3 = quantile(eGFR, 0.75, na.rm = TRUE)
  )

# Compute Median & IQR for SOFA
sofa_eGFR_summary <- df_eGFR_long %>%
  group_by(SOFA_Category, Timepoint) %>%
  summarise(
    median_eGFR = median(eGFR, na.rm = TRUE),
    Q1 = quantile(eGFR, 0.25, na.rm = TRUE),
    Q3 = quantile(eGFR, 0.75, na.rm = TRUE)
  )

# Define a theme for larger, bold axis text
custom_theme <- theme_minimal() +
  theme(
    axis.text.x = element_text(size = 22, face = "bold", color = "black"),
    axis.text.y = element_text(size = 22, face = "bold", color = "black"),
    axis.title.x = element_text(size = 22, face = "bold", color = "black"),
    axis.title.y = element_text(size = 22, face = "bold", color = "black"),
    plot.title = element_text(size = 24, face = "bold", hjust = 0.5, color = "black")
  )


scale_x_discrete(labels = c(
  "eGFRStart" = "Start",
  "eGFR24" = "24H",
  "eGFR48" = "48H",
  "eGFR72" = "72H",
  "eGFREnd" = "End"
))


# SAPS eGFR Change Plot (Fixed Legend Order)
p_saps <- ggplot(saps_eGFR_summary, aes(x = Timepoint, y = median_eGFR, group = SAPS_Category, color = SAPS_Category)) +
  geom_hline(aes(yintercept = 90, linetype = "Normal eGFR"), color = "black", size = 1) +
  geom_line(size = 1.2) +  
  geom_ribbon(aes(ymin = Q1, ymax = Q3, fill = SAPS_Category), alpha = 0.2, color = NA) +  
  scale_linetype_manual(name = "Reference Line", values = c("Normal eGFR" = "dashed")) +
  scale_color_manual(name = "eGFR by SAPS Severity",
                     labels = c("Low", "Medium", "High"),
                     values = c("Low" = "blue", "Medium" = "orange", "High" = "red")) +
  scale_fill_manual(name = "IQR of eGFR by SAPS Severity",
                    labels = c("Low", "Medium", "High"),
                    values = c("Low" = "blue", "Medium" = "orange", "High" = "red")) +
  scale_x_discrete(labels = c("eGFRStart" = "Start", "eGFR24" = "24H", "eGFR48" = "48H", "eGFR72" = "72H", "eGFREnd" = "End")) +
  scale_y_continuous(limits = c(50, 130), breaks = seq(50, 130, by = 20)) +
  labs(title = "eGFR Change Over Time (SAPS Categories)",
       x = "Timepoint in the Therapy", y = "eGFR (mL/min/1.73m²)") +
  custom_theme +
  theme(
    legend.text = element_text(size = 20, , face = "bold"),
    legend.title = element_text(size = 20, face = "bold"),
    legend.key.size = unit(0.4, "cm"),
    legend.position = c(0.75, 0.23),
    legend.spacing.y = unit(0.1, "cm"),       
    legend.margin = margin(2, 2, 2, 2), 
    legend.background = element_rect(fill = "white", color = "black")
  ) +
  guides(
    linetype = guide_legend(order = 1),
    color = guide_legend(order = 2),
    fill = guide_legend(order = 3)
  )



# SOFA eGFR Change Plot (Fixed Legend Order)
p_sofa <- ggplot(sofa_eGFR_summary, aes(x = Timepoint, y = median_eGFR, group = SOFA_Category, color = SOFA_Category)) +
  geom_hline(aes(yintercept = 90, linetype = "Normal eGFR"), color = "black", size = 1) +
  geom_line(size = 1.2) +  
  geom_ribbon(aes(ymin = Q1, ymax = Q3, fill = SOFA_Category), alpha = 0.2, color = NA) +  
  scale_linetype_manual(name = "Reference Line", values = c("Normal eGFR" = "dashed")) +
  scale_color_manual(name = "eGFR by SOFA Severity",
                     labels = c("Low", "Medium", "High"),
                     values = c("Low" = "blue", "Medium" = "orange", "High" = "red")) +
  scale_fill_manual(name = "IQR of eGFR by SOFA Severity",
                    labels = c("Low", "Medium", "High"),
                    values = c("Low" = "blue", "Medium" = "orange", "High" = "red")) +
  scale_x_discrete(labels = c("eGFRStart" = "Start", "eGFR24" = "24H", "eGFR48" = "48H", "eGFR72" = "72H", "eGFREnd" = "End")) +
  scale_y_continuous(limits = c(50, 130), breaks = seq(50, 130, by = 20)) +
  labs(title = "eGFR Change Over Time (SOFA Categories)",
       x = "Timepoint in the Therapy", y = "eGFR (mL/min/1.73m²)") +
  custom_theme +
  theme(
    legend.text = element_text(size = 20, face = "bold"),
    legend.title = element_text(size = 20, face = "bold"),
    legend.key.size = unit(0.4, "cm"),
    legend.position = c(0.75, 0.23),
    legend.spacing.y = unit(0.1, "cm"),       
    legend.margin = margin(2, 2, 2, 2), 
    legend.background = element_rect(fill = "white", color = "black")
  ) +
  guides(
    linetype = guide_legend(order = 1),
    color = guide_legend(order = 2),
    fill = guide_legend(order = 3)
  )



# Combine SAPS and SOFA eGFR plots
final_eGFR_plot <- p_saps | p_sofa
saps_eGFR_plot<- p_saps
sofa_eGFR_plot<-p_sofa

# Print Final Plot
print(final_eGFR_plot)
print(saps_eGFR_plot)
print(sofa_eGFR_plot)

# Save combined plots
ggsave(filename = paste0(output_dir, "final_eGFR_plot.pdf"), plot = final_eGFR_plot, dpi = 300, width = 18, height = 14) #This is the plot that i show in the report
#ggsave(filename = paste0(output_dir, "saps_eGFR_plot.pdf"), plot = saps_eGFR_plot, dpi = 300, width = 8, height = 10)
#ggsave(filename = paste0(output_dir, "sofa_eGFR_plot.pdf"), plot = sofa_eGFR_plot, dpi = 300, width = 8, height = 10)


load("C:/Users/Sumed Seeyakmani/Downloads/vancomycin.RData", verbose = TRUE) 
library(ggplot2)
library(reshape2)
library(alluvial)
library(ggalluvial)

# Nephrotoxine
sum(dat$ACEI == "yes")              # 177
sum(dat$ARB == "yes")               # 34
sum(dat$Aminoglycosides == "yes")   # 22
sum(dat$Loop == "yes")              # 670
sum(dat$NSAID == "yes")             # 43
sum(dat$PipTaz == "yes")            # 24
sum(dat$Vasopressors == "yes")      # 701

pdf(file="nephrotoxines_usage.pdf", width=15, height=8)
old.par <- par(mar = c(5,8,4,2)+0.1)
values <- c(177, 34, 22, 670, 43, 24, 701)

#This is the plot that I used in the report
barplot_obj <- barplot(
  values,
  names.arg = c("ACEI", "ARB", "Aminoglycosides", "Loop", "NSAID", "PipTaz", "Vasopressors"),
  col = "lightblue",
  main = "Nephrotoxines Usage",
  ylab = "Number of Observants Using the Nephrotoxines",
  ylim = c(0, max(values) * 1.1),
  
  # Text sizes
  cex.names = 1.5,       # x-axis labels
  cex.axis = 1.5,        # y-axis tick labels
  cex.lab = 1.6,         # axis titles
  cex.main = 2.0,        # main title
  
  # Text styles
  font.main = 2,         # bold title
  font.lab = 2,          # bold axis labels
  font.axis = 2,         # bold axis ticks
  
  # Colors
  col.lab = "black",     # axis labels
  col.axis = "black",    # tick labels
  col.main = "black"     # main title
)



text(x = barplot_obj, y = values, labels = values, pos = 3, cex = 1.5, col = "black")
par(old.par)
dev.off()

# ueberdosis
dat$kidney_failure <- "no"
dat$kidney_failure[dat$SCrEnd > 1.7] = "yes"
dat$kidney_failure[dat$SCr72 > 1.7] = "yes"
dat$kidney_failure[dat$SCr48 > 1.7] = "yes"
dat$kidney_failure[dat$SCr24 > 1.7] = "yes"
dat$kidney_failure[dat$SCrStart > 1.7] = "yes"

# ----------------------------------------- Alluvial Diagramm
pdf(file="scr_level_nephrotoxines_gender.pdf", width=15, height=8)

dat_diseases = dat[, c("Gender", "ACEI", "ARB", "Aminoglycosides", "Loop", "NSAID", "PipTaz", "Vasopressors", "kidney_failure")]

dat_diseases$people = 1
dat_diseases = dat_diseases[, c("Gender", "ACEI", "Loop", "Vasopressors", "people", "kidney_failure")]

dat_diseases_agg <- aggregate(people ~ ., dat_diseases, FUN = sum)

#This is the plot that I used in the report
ggplot(dat_diseases_agg, 
       aes(axis1 = Gender, axis2 = kidney_failure, axis3 = ACEI, 
           axis4 = Loop, axis5 = Vasopressors, y = people)) +
  geom_alluvium(aes(fill = kidney_failure), alpha = 0.8) +
  geom_stratum() +
  geom_text(stat = "stratum", 
            aes(label = after_stat(stratum)), 
            size = 6, 
            fontface = "bold", 
            color = "black") +
  scale_fill_manual(values = c("no" = "lightgreen", "yes" = "brown1")) +
  scale_x_discrete(limits = c("Gender", "SCr level > 1.7", "ACEI", "Loop", "Vasopressors")) +
  labs(title = "Alluvial Diagram of high SCr level, often used Nephrotoxines and gender",
       x = "",
       y = "Number of Observants") +
  theme_minimal() +
  theme(
    legend.position = "none",
    text = element_text(size = 20),
    axis.text.x = element_text(size = 16, face = "bold", color = "black"),
    axis.text.y = element_text(size = 16, face = "bold", color = "black"),
    axis.title.x = element_text(size = 16, face = "bold", color = "black"),
    axis.title.y = element_text(size = 16, face = "bold", color = "black")
  )



dev.off()




