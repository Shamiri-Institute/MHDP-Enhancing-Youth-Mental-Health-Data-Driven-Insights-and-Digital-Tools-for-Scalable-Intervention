################################################################################
# PROJECT: MENTAL HEALTH DATA PRIZE (OBJECTIVE 2)
# PURPOSE: Part 1 - Data Preparation, Sample Description, and Visualizations
# Strategy Documentation: Import the Objective 2 datasets (Shamiri 1.0, 2.0, 3.0).
#                         Retains specific intervention arms (Growth, Gratitude, 
#                         Wellness, etc.). Generates trajectory summaries and 
#                         pre-post change plots using strict brand colors.
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

# Create Objective 2 Output Directories
dir.create("../results/objective_2/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("../results/objective_2/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("../results/objective_2/models", recursive = TRUE, showWarnings = FALSE)

cat(strrep("=", 80), "\n")
cat("1. IMPORTING AND PREPARING DATASET\n")
cat(strrep("-", 80), "\n")

mhdp <- read_excel("../datasets/processed/mhdp_data_2.xlsx", col_types = "text")
colnames(mhdp) <- tolower(colnames(mhdp))

# SURGICAL CLEANING: Drop known duplicated student
mhdp <- mhdp %>%
  filter(!(dataset == "Shamiri_3.0" & participant_id == "40281"))

# Safely convert psychometric and demographic columns to numeric
# (NOTE: 'gender' is explicitly excluded here so it remains text)
mhdp <- suppressWarnings(
  mhdp %>%
    mutate(across(c(
      age, form, timepoint, financial_status, religion, siblings,
      parents_home, parents_dead, fathers_education, mothers_education,
      co_curricular, sports, perceived_academic_abilities, home,
      starts_with("phq_"), starts_with("gad_"), starts_with("swemwbs_"), 
      starts_with("pils_"), starts_with("mspss_"),
      starts_with("gq_"), starts_with("pcs_"),
      starts_with("moc_"), starts_with("sps_"), starts_with("scs_")
    ), as.numeric))
)

# Clean and Standardize Gender Strings
mhdp <- mhdp %>%
  mutate(
    gender = case_when(
      tolower(gender) == "female" ~ "Female",
      tolower(gender) == "male" ~ "Male",
      TRUE ~ "Not Answered" # Catches NAs and blanks
    )
  )


# condition and dataset
table(mhdp$dataset, mhdp$condition, useNA = "ifany") # Check unique combinations for cleaning
# Clean up Condition Labels (Strictly per study plan)
mhdp <- mhdp %>%
  mutate(condition = tolower(condition)) %>%
  mutate(
    condition = case_when(
      dataset == "Shamiri_1.0" & condition == "intervention" ~ "shamiri",
      dataset == "Shamiri_1.0" & condition == "control" ~ "study skills",
      dataset == "Shamiri_2.0" & condition == "wellness" ~ "shamiri",
      condition == "study-skills" ~ "study skills", # Standardize spelling
      TRUE ~ condition
    )
  ) %>%
  filter(!is.na(condition) & condition != "na") %>%
  # Set factor levels so Shamiri is first and Control is ALWAYS last
  mutate(condition = factor(condition, levels = c(
    "shamiri", "gratitude", "growth", "values", "study skills"
  )))

table(mhdp$dataset, mhdp$condition, useNA = "ifany")

# Define Official  Brand Color Palette (Using tints/shades for multiple arms)
obj2_colors <- c(
  "shamiri"      = "#132964", # Dark Blue
  "gratitude"    = "#43528A", # Lighter Blue
  "growth"       = "#E63973", # Shamiri Pink/Red
  "values"       = "#EFA3B8", # Lighter Pink
  "study skills" = "#C6BFEA" # Lighter Purple
)

cat("✓ Master dataset 'mhdp_data_2.xlsx' loaded:", nrow(mhdp), "rows\n")
cat("✓ Duplicates removed, 'na' dropped, and conditions retained.\n\n")


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
# 3. OVERALL SAMPLE DESCRIPTION
#-------------------------------------------------------------------------------

cat("3. SAMPLE DESCRIPTION\n")
cat(strrep("-", 80), "\n")

cat("Timepoint Distribution by Dataset:\n")
print(table(mhdp$dataset, mhdp$timepoint, useNA = "ifany"))

cat("\nCondition Distribution by Dataset:\n")
print(table(mhdp$dataset, mhdp$condition, useNA = "ifany"))
cat("\n")


#-------------------------------------------------------------------------------
# 4. GENDER DISTRIBUTION PLOTS (FULL & CLINICAL)
#-------------------------------------------------------------------------------

cat("4. GENERATING GENDER DISTRIBUTION PLOTS BY DATASET\n")
cat(strrep("-", 80), "\n")

mhdp_baseline <- mhdp %>% filter(timepoint == 0)

plot_gender <- function(data_subset, title_text, filename) {
  gender_data <- data_subset %>%
    mutate(gender = factor(gender, levels = c("Female", "Male", "Not Answered"))) %>% 
    group_by(dataset, gender) %>%
    summarise(count = n(), .groups = "drop_last") %>%
    mutate(
      percentage = round((count / sum(count)) * 100, 1), 
      label_text = paste0(count, " (", percentage, "%)")
    ) %>%
    ungroup()
  
  p <- ggplot(gender_data, aes(x = gender, y = count, fill = gender)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = label_text), vjust = -0.5, fontface = "bold", size = 3.5) +
    scale_fill_manual(values = c("Male" = "#9A8EE6", "Female" = "#132964", "Not Answered" = "#A6A6A6")) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    facet_wrap(~ dataset) +
    labs(title = title_text, x = "Gender", y = "Number of Participants") +
    theme_minimal(base_size = 14) + theme(legend.position = "none")
  
  ggsave(paste0("../results/objective_2/figures/", filename), p, width = 12, height = 6, dpi = 300)
}

plot_gender(mhdp_baseline, "Gender Distribution of Participants (Full Sample)", "gender_distribution_bar.png")

clinical_ids <- mhdp_baseline %>% filter(Clinical_Dep_Sample == "Yes" | Clinical_Anx_Sample == "Yes") %>% pull(participant_id)
plot_gender(mhdp_baseline %>% filter(participant_id %in% clinical_ids), "Gender Distribution (Clinical Subsample)", "gender_distribution_clinical_bar.png")

cat("✓ Gender plots saved\n\n")


#-------------------------------------------------------------------------------
# 5. PREVALENCE RATES & COMPARISON BAR CHARTS 
#-------------------------------------------------------------------------------

cat("5. CALCULATING PREVALENCE RATES (BASELINE VS WEEK 4)\n")
cat(strrep("-", 80), "\n")

# Baseline vs Week 4 Comparison
df_comp <- mhdp %>% filter(timepoint %in% c(0, 4)) %>% mutate(time_label = ifelse(timepoint == 0, "Baseline", "Week 4"))

dep_summary <- df_comp %>% group_by(condition, time_label) %>%
  summarise(n = n(), cases = sum(Clinical_Dep_Sample == "Yes", na.rm = TRUE), prevalence = (cases / n) * 100, .groups = "drop") %>% mutate(measure = "Depression")

anx_summary <- df_comp %>% group_by(condition, time_label) %>%
  summarise(n = n(), cases = sum(Clinical_Anx_Sample == "Yes", na.rm = TRUE), prevalence = (cases / n) * 100, .groups = "drop") %>% mutate(measure = "Anxiety")

combined_comp <- bind_rows(dep_summary, anx_summary)

p_prev <- ggplot(combined_comp, aes(x = time_label, y = prevalence, fill = condition)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(prevalence, 1)), position = position_dodge(width = 0.9), vjust = -0.5, size = 3.5) +
  scale_fill_manual(values = obj2_colors) + 
  facet_wrap(~ measure) +
  labs(title = "Baseline vs Week 4 Clinical Prevalence", x = "Timepoint", y = "Prevalence (%)", fill = "Condition") +
  theme_minimal(base_size = 14) + theme(legend.position = "bottom")

ggsave("../results/objective_2/figures/baseline_endline_comparison.png", p_prev, width = 12, height = 7, dpi = 300)
cat("✓ Prevalence comparison plot saved\n\n")


#-------------------------------------------------------------------------------
# 6. SCORE TRAJECTORY SUMMARY EXPORT & PLOTS 
#-------------------------------------------------------------------------------

cat("6. GENERATING SCORE SUMMARIES AND TRAJECTORY PLOTS\n")
cat(strrep("-", 80), "\n")

generate_trajectory_report <- function(data_subset, file_suffix, title_suffix) {
  
  summary_df <- data_subset %>%
    group_by(dataset, condition, timepoint) %>%
    summarise(
      n = sum(!is.na(phq_total)),
      mean_phq = mean(phq_total, na.rm = TRUE), sd_phq = sd(phq_total, na.rm = TRUE),
      ci95_phq_low = mean_phq - 1.96 * (sd_phq / sqrt(n)), ci95_phq_high = mean_phq + 1.96 * (sd_phq / sqrt(n)),
      mean_gad = mean(gad_total, na.rm = TRUE), sd_gad = sd(gad_total, na.rm = TRUE),
      ci95_gad_low = mean_gad - 1.96 * (sd_gad / sqrt(n)), ci95_gad_high = mean_gad + 1.96 * (sd_gad / sqrt(n)),
      .groups = "drop"
    ) %>% arrange(dataset, condition, timepoint)
  
  write_xlsx(summary_df, paste0("../results/objective_2/tables/trajectories_summary", file_suffix, ".xlsx"))
  
  summary_df$timepoint_fac <- factor(summary_df$timepoint, levels = c(0, 2, 4, 6, 8, 28, 156))
  
  # Plot PHQ
  p_phq <- ggplot(summary_df, aes(x = timepoint_fac, y = mean_phq, color = condition, group = condition)) +
    geom_line(linewidth = 1.2) + geom_point(size = 2) +
    geom_errorbar(aes(ymin = ci95_phq_low, ymax = ci95_phq_high), width = 0.3) +
    scale_color_manual(values = obj2_colors) + facet_wrap(~ dataset, scales = "free_x") +
    labs(title = paste("PHQ-8 Trajectories", title_suffix), x = "Timepoint (Weeks)", y = "Mean PHQ Score", color = "Condition") + 
    theme_minimal(base_size = 14) + theme(legend.position = "bottom")
  ggsave(paste0("../results/objective_2/figures/phq_trajectory", file_suffix, ".png"), p_phq, width = 12, height = 6, dpi = 300)
  
  # Plot GAD
  p_gad <- ggplot(summary_df, aes(x = timepoint_fac, y = mean_gad, color = condition, group = condition)) +
    geom_line(linewidth = 1.2) + geom_point(size = 2) +
    geom_errorbar(aes(ymin = ci95_gad_low, ymax = ci95_gad_high), width = 0.3) +
    scale_color_manual(values = obj2_colors) + facet_wrap(~ dataset, scales = "free_x") +
    labs(title = paste("GAD-7 Trajectories", title_suffix), x = "Timepoint (Weeks)", y = "Mean GAD Score", color = "Condition") + 
    theme_minimal(base_size = 14) + theme(legend.position = "bottom")
  ggsave(paste0("../results/objective_2/figures/gad_trajectory", file_suffix, ".png"), p_gad, width = 12, height = 6, dpi = 300)
}

generate_trajectory_report(mhdp, "", "(Full Sample)")
generate_trajectory_report(mhdp %>% filter(participant_id %in% clinical_ids), "_clinical", "(Clinical Sample)")

cat("✓ Trajectory summaries and plots saved.\n")


#-------------------------------------------------------------------------------
# 7. CHANGE SCORE PLOTS (PRE-POST)
#-------------------------------------------------------------------------------

cat("\n7. GENERATING PRE-POST CHANGE SCORE PLOTS\n")
cat(strrep("-", 80), "\n")

create_change_plot <- function(data_subset, outcome_col, title_txt, filename) {
  prep_df <- data_subset %>%
    filter(timepoint %in% c(0, 4)) %>%
    select(participant_id, dataset, condition, timepoint, all_of(outcome_col)) %>%
    pivot_wider(names_from = timepoint, values_from = all_of(outcome_col), names_prefix = "T") %>%
    mutate(change_score = T4 - T0) %>% filter(!is.na(change_score)) %>%
    group_by(dataset, condition) %>%
    summarise(n = n(), mean_change = mean(change_score, na.rm = TRUE), se_change = sd(change_score, na.rm = TRUE) / sqrt(n), .groups = "drop")
  
  p <- ggplot(prep_df, aes(x = condition, y = mean_change, fill = condition)) +
    geom_col(width = 0.6, position = position_dodge()) +
    geom_errorbar(aes(ymin = mean_change - se_change, ymax = mean_change + se_change), width = 0.2, linewidth = 0.8) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    scale_fill_manual(values = obj2_colors) + facet_wrap(~ dataset, scales = "free_x") +
    labs(title = title_txt, subtitle = "Negative values indicate symptom reduction (Week 4 - Baseline)", x = "Condition", y = "Mean Change", fill = "Condition") +
    theme_minimal(base_size = 14) + theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave(paste0("../results/objective_2/figures/", filename), p, width = 12, height = 6, dpi = 300)
}

create_change_plot(mhdp, "phq_total", "Change in PHQ-8 Scores (Full Sample)", "phq_change_full.png")
create_change_plot(mhdp, "gad_total", "Change in GAD-7 Scores (Full Sample)", "gad_change_full.png")
create_change_plot(mhdp %>% filter(participant_id %in% clinical_ids), "phq_total", "Change in PHQ-8 Scores (Clinical Sample)", "phq_change_clinical.png")
create_change_plot(mhdp %>% filter(participant_id %in% clinical_ids), "gad_total", "Change in GAD-7 Scores (Clinical Sample)", "gad_change_clinical.png")

cat("✓ Pre-Post Change bar plots generated and saved.\n\n")

cat(strrep("=", 80), "\n")
cat("OBJECTIVE 2 DESCRIPTIVES SCRIPT COMPLETE\n")
cat(strrep("=", 80), "\n\n")

################################################################################
# END OF SCRIPT
################################################################################