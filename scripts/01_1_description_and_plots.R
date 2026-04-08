################################################################################
# PROJECT: MENTAL HEALTH DATA PRIZE
# PURPOSE: Part 1 - Data Preparation, Sample Description, and Visualizations
# Strategy Documentation: Evaluates outcomes based on delivery groups (Shamiri 
#                         vs Partner vs Control) and granular implementers 
#                         (Shamiri vs AMHRTF vs PDO vs Control). 

################################################################################

#-------------------------------------------------------------------------------
# 1. ENVIRONMENT SETUP & DATA IMPORT
#-------------------------------------------------------------------------------

# Set working directory to script location
if (requireNamespace("rstudioapi", quietly = TRUE)) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

# Clear workspace and set global options
rm(list = ls())
options(scipen = 999, stringsAsFactors = FALSE)

# Package Management
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,    # Data wrangling & ggplot2
  readxl,       # Reading Excel files
  writexl,      # Writing Excel files
  conflicted    # Namespace safety
)

# Resolve package conflicts
conflict_prefer("filter", "dplyr")
conflict_prefer("mutate", "dplyr")
conflict_prefer("select", "dplyr")

# Create Objective 1 Output Directories
dir.create("../results/objective_1/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("../results/objective_1/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("../results/objective_1/models", recursive = TRUE, showWarnings = FALSE)

cat(strrep("=", 80), "\n")
cat("1. IMPORTING AND PREPARING DATASET\n")
cat(strrep("-", 80), "\n")

mhdp <- read_excel("../datasets/processed/mhdp_data.xlsx", col_types = "text")

# Safely convert psychometric and demographic columns to numeric
mhdp <- mhdp %>%
  mutate(across(c(
    age, form, gender, timepoint,
    starts_with("phq_"), starts_with("gad_"), starts_with("swemwbs_"), 
    starts_with("epoch_"), starts_with("pils_"), starts_with("mspss_"),
    starts_with("gq_"), starts_with("pcs_"), starts_with("ses_")
  ), as.numeric))

# Define Implementation Contexts based on existing 'implementer' and 'condition' columns
mhdp <- mhdp %>%
  mutate(
    # Consolidate Waitlist into Control
    condition_clean = ifelse(condition %in% c("TAU", "Waitlist"), "Control", condition),
    
    # 1. Broad Comparison
    delivery_group = case_when(
      condition_clean == "Control" ~ "Control",
      implementer %in% c("AMHRTF", "PDO") ~ "Partner",
      TRUE ~ "Shamiri"
    ),
    
    # 2. Granular Comparison
    implementer_group = case_when(
      condition_clean == "Control" ~ "Control",
      implementer == "SHAMIRI" ~ "Shamiri",
      TRUE ~ implementer # Keeps "AMHRTF" and "PDO" distinct
    )
  ) %>%
  # Set factor levels to ensure Shamiri is always the reference group
  mutate(
    delivery_group = factor(delivery_group, levels = c("Shamiri", "Partner", "Control")),
    implementer_group = factor(implementer_group, levels = c("Shamiri", "AMHRTF", "PDO", "Control"))
  )

# Define universal color palette for plots
impl_colors <- c(
  "Shamiri" = "#132964", # Dark Blue
  "Partner" = "#9A8EE6", # Purple
  "AMHRTF"  = "#9A8EE6", # Purple
  "PDO"     = "#E63973", # Shamiri Pink/Red
  "Control" = "#A6A6A6"  # Grey
)

cat("✓ Master dataset loaded and Implementation Context assigned.\n\n")


#-------------------------------------------------------------------------------
# 2. CALCULATE SCALE TOTALS & CLINICAL CUTOFFS
#-------------------------------------------------------------------------------

cat("2. CALCULATING SCALE TOTALS AND CLINICAL CLASSIFICATIONS\n")
cat(strrep("-", 80), "\n")

mhdp <- mhdp %>%
  mutate(
    phq_total = if_else(if_any(phq_1:phq_8, is.na), NA_real_, rowSums(across(phq_1:phq_8))),
    gad_total = if_else(if_any(gad_1:gad_7, is.na), NA_real_, rowSums(across(gad_1:gad_7))),
    swemwbs_total = if_else(if_any(swemwbs_1:swemwbs_7, is.na), NA_real_, rowSums(across(swemwbs_1:swemwbs_7)))
  ) %>%
  mutate(
    Clinical_Dep_Sample = case_when(
      phq_total <= 9 ~ "No",
      phq_total >= 10 ~ "Yes",
      TRUE ~ NA_character_
    ),
    Clinical_Anx_Sample = case_when(
      gad_total <= 9 ~ "No",
      gad_total >= 10 ~ "Yes",
      TRUE ~ NA_character_
    )
  )

cat("✓ Psychometric scales computed and clinical thresholds applied\n\n")


#-------------------------------------------------------------------------------
# 3. OVERALL SAMPLE DESCRIPTION (BY IMPLEMENTER)
#-------------------------------------------------------------------------------

cat("3. SAMPLE DESCRIPTION (BY IMPLEMENTATION CONTEXT)\n")
cat(strrep("-", 80), "\n")

cat("Broad Delivery Group Breakdown:\n")
print(table(mhdp$dataset, mhdp$delivery_group))

cat("\nGranular Implementer Group Breakdown:\n")
print(table(mhdp$dataset, mhdp$implementer_group))
cat("\n")


#-------------------------------------------------------------------------------
# 4. GENDER DISTRIBUTION PLOTS (FULL & CLINICAL)
#-------------------------------------------------------------------------------

cat("4. GENERATING GENDER DISTRIBUTION PLOTS\n")
cat(strrep("-", 80), "\n")

mhdp_baseline <- mhdp %>% filter(timepoint == 0)

plot_gender <- function(data_subset, title_text, filename) {
  gender_data <- data_subset %>%
    mutate(gender_label = case_when(gender == 1 ~ "Female", gender == 2 ~ "Male", TRUE ~ "Not Answered")) %>%
    count(gender_label) %>%
    rename(gender = gender_label, count = n) %>%
    mutate(percentage = round((count / sum(count)) * 100, 1), label_text = paste0(count, " (", percentage, "%)"))
  
  p <- ggplot(gender_data, aes(x = gender, y = count, fill = gender)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = label_text), vjust = -0.5, fontface = "bold") +
    scale_fill_manual(values = c("Male" = "#9A8EE6", "Female" = "#132964", "Not Answered" = "grey")) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(title = title_text, x = "Gender", y = "Number of Participants") +
    theme_minimal(base_size = 14) + theme(legend.position = "none")
  
  ggsave(paste0("../results/objective_1/figures/", filename), p, width = 8, height = 6, dpi = 300)
}

plot_gender(mhdp_baseline, "Gender Distribution of Participants (Full Sample)", "gender_distribution_bar.png")

clinical_ids <- mhdp_baseline %>% filter(Clinical_Dep_Sample == "Yes" | Clinical_Anx_Sample == "Yes") %>% pull(participant_id)
plot_gender(mhdp_baseline %>% filter(participant_id %in% clinical_ids), "Gender Distribution (Clinical Subsample)", "gender_distribution_clinical_bar.png")

cat("✓ Gender plots saved\n\n")


#-------------------------------------------------------------------------------
# 5. PREVALENCE RATES & COMPARISON BAR CHARTS 
#-------------------------------------------------------------------------------

cat("5. CALCULATING PREVALENCE RATES\n")
cat(strrep("-", 80), "\n")

# Reusable function for prevalence plotting
plot_prevalence <- function(grouping_var, file_name, title_suffix) {
  df_comp <- mhdp %>% filter(timepoint %in% c(0, 4)) %>% mutate(time_label = ifelse(timepoint == 0, "Baseline", "Endline"))
  
  dep_summary <- df_comp %>% group_by(.data[[grouping_var]], time_label) %>%
    summarise(n = n(), cases = sum(Clinical_Dep_Sample == "Yes", na.rm = TRUE), prevalence = (cases / n) * 100, .groups = "drop") %>% mutate(measure = "Depression")
  
  anx_summary <- df_comp %>% group_by(.data[[grouping_var]], time_label) %>%
    summarise(n = n(), cases = sum(Clinical_Anx_Sample == "Yes", na.rm = TRUE), prevalence = (cases / n) * 100, .groups = "drop") %>% mutate(measure = "Anxiety")
  
  combined_comp <- bind_rows(dep_summary, anx_summary)
  
  p <- ggplot(combined_comp, aes(x = time_label, y = prevalence, fill = .data[[grouping_var]])) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = round(prevalence, 1)), position = position_dodge(width = 0.9), vjust = -0.5) +
    scale_fill_manual(values = impl_colors) +
    facet_wrap(~ measure) +
    labs(title = paste("Baseline vs Endline Prevalence\n", title_suffix), x = "Timepoint", y = "Prevalence (%)", fill = "Group") +
    theme_minimal(base_size = 14) + theme(legend.position = "bottom")
  
  ggsave(paste0("../results/objective_1/figures/", file_name), p, width = 10, height = 7, dpi = 300)
}

# Generate both broad and granular prevalence plots
plot_prevalence("delivery_group", "prevalence_delivery_group.png", "(Shamiri vs Partner vs Control)")
plot_prevalence("implementer_group", "prevalence_implementer_group.png", "(Shamiri vs AMHRTF vs PDO vs Control)")

cat("✓ Prevalence comparison plots saved\n\n")


#-------------------------------------------------------------------------------
# 6. SCORE TRAJECTORY SUMMARY EXPORT & PLOTS 
#-------------------------------------------------------------------------------

cat("6. GENERATING SCORE SUMMARIES AND TRAJECTORY PLOTS\n")
cat(strrep("-", 80), "\n")

generate_trajectory_report <- function(data_subset, grouping_var, file_suffix, title_suffix) {
  
  summary_df <- data_subset %>%
    group_by(dataset, .data[[grouping_var]], timepoint) %>%
    summarise(
      n = sum(!is.na(phq_total)),
      mean_phq = mean(phq_total, na.rm = TRUE), sd_phq = sd(phq_total, na.rm = TRUE),
      ci95_phq_low = mean_phq - 1.96 * (sd_phq / sqrt(n)), ci95_phq_high = mean_phq + 1.96 * (sd_phq / sqrt(n)),
      mean_gad = mean(gad_total, na.rm = TRUE), sd_gad = sd(gad_total, na.rm = TRUE),
      ci95_gad_low = mean_gad - 1.96 * (sd_gad / sqrt(n)), ci95_gad_high = mean_gad + 1.96 * (sd_gad / sqrt(n)),
      .groups = "drop"
    ) %>% arrange(dataset, .data[[grouping_var]], timepoint)
  
  write_xlsx(summary_df, paste0("../results/objective_1/tables/trajectories_", grouping_var, file_suffix, ".xlsx"))
  
  # Plot PHQ
  p_phq <- ggplot(summary_df, aes(x = timepoint, y = mean_phq, color = .data[[grouping_var]], group = .data[[grouping_var]])) +
    geom_line(linewidth = 1.2) + geom_point(size = 2) +
    geom_errorbar(aes(ymin = ci95_phq_low, ymax = ci95_phq_high), width = 0.3) +
    scale_color_manual(values = impl_colors) + facet_wrap(~ dataset, scales = "free_x") +
    labs(title = paste("PHQ-8 Trajectories", title_suffix), x = "Weeks", y = "Mean PHQ Score", color = "Group") + 
    theme_minimal(base_size = 14) + theme(legend.position = "bottom")
  ggsave(paste0("../results/objective_1/figures/phq_trajectory_", grouping_var, file_suffix, ".png"), p_phq, width = 10, height = 7, dpi = 300)
  
  # Plot GAD
  p_gad <- ggplot(summary_df, aes(x = timepoint, y = mean_gad, color = .data[[grouping_var]], group = .data[[grouping_var]])) +
    geom_line(linewidth = 1.2) + geom_point(size = 2) +
    geom_errorbar(aes(ymin = ci95_gad_low, ymax = ci95_gad_high), width = 0.3) +
    scale_color_manual(values = impl_colors) + facet_wrap(~ dataset, scales = "free_x") +
    labs(title = paste("GAD-7 Trajectories", title_suffix), x = "Weeks", y = "Mean GAD Score", color = "Group") + 
    theme_minimal(base_size = 14) + theme(legend.position = "bottom")
  ggsave(paste0("../results/objective_1/figures/gad_trajectory_", grouping_var, file_suffix, ".png"), p_gad, width = 10, height = 6, dpi = 300)
}

# Run Broad Comparisons (delivery_group)
generate_trajectory_report(mhdp, "delivery_group", "", "(Broad Comparison)")
generate_trajectory_report(mhdp %>% filter(participant_id %in% clinical_ids), "delivery_group", "_clinical", "(Clinical Sample)")

# Run Granular Comparisons (implementer_group)
generate_trajectory_report(mhdp, "implementer_group", "", "(Granular Implementers)")
generate_trajectory_report(mhdp %>% filter(participant_id %in% clinical_ids), "implementer_group", "_clinical", "(Clinical Sample)")

cat("✓ Trajectory summaries and plots saved.\n")


#-------------------------------------------------------------------------------
# 7. CHANGE SCORE PLOTS (PRE-POST)
#-------------------------------------------------------------------------------

cat("\n7. GENERATING PRE-POST CHANGE SCORE PLOTS\n")
cat(strrep("-", 80), "\n")

create_change_plot <- function(data_subset, grouping_var, outcome_col, title_txt, filename) {
  prep_df <- data_subset %>%
    filter(timepoint %in% c(0, 4)) %>%
    select(participant_id, dataset, all_of(grouping_var), timepoint, all_of(outcome_col)) %>%
    pivot_wider(names_from = timepoint, values_from = all_of(outcome_col), names_prefix = "T") %>%
    mutate(change_score = T4 - T0) %>% filter(!is.na(change_score)) %>%
    group_by(dataset, .data[[grouping_var]]) %>%
    summarise(n = n(), mean_change = mean(change_score, na.rm = TRUE), se_change = sd(change_score, na.rm = TRUE) / sqrt(n), .groups = "drop")
  
  p <- ggplot(prep_df, aes(x = .data[[grouping_var]], y = mean_change, fill = .data[[grouping_var]])) +
    geom_col(width = 0.6, position = position_dodge()) +
    geom_errorbar(aes(ymin = mean_change - se_change, ymax = mean_change + se_change), width = 0.2, linewidth = 0.8) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    scale_fill_manual(values = impl_colors) + facet_wrap(~ dataset, scales = "free_x") +
    labs(title = title_txt, subtitle = "Negative values indicate symptom reduction (Week 4 - Baseline)", x = "Group", y = "Mean Change", fill = "Group") +
    theme_minimal(base_size = 14) + theme(legend.position = "bottom", axis.text.x = element_text(angle = 15, hjust = 1))
  
  ggsave(paste0("../results/objective_1/figures/", filename), p, width = 10, height = 6, dpi = 300)
}

# --- Broad Comparisons (delivery_group) ---
create_change_plot(mhdp, "delivery_group", "phq_total", "Change in PHQ-8 Scores (Broad)", "phq_change_delivery_group.png")
create_change_plot(mhdp, "delivery_group", "gad_total", "Change in GAD-7 Scores (Broad)", "gad_change_delivery_group.png")

# --- Granular Comparisons (implementer_group) ---
create_change_plot(mhdp, "implementer_group", "phq_total", "Change in PHQ-8 Scores (Implementers)", "phq_change_implementer_group.png")
create_change_plot(mhdp, "implementer_group", "gad_total", "Change in GAD-7 Scores (Implementers)", "gad_change_implementer_group.png")


cat("✓ Pre-Post Change bar plots generated and saved.\n\n")
cat(strrep("=", 80), "\n")
cat("DESCRIPTIVES SCRIPT COMPLETE\n")
cat(strrep("=", 80), "\n\n")

################################################################################
# END OF SCRIPT
################################################################################