################################################################################
# PROJECT: MENTAL HEALTH DATA PRIZE
# PURPOSE: Part 1 - Data Preparation, Sample Description, and Visualizations
# Strategy Documentation: Import the cleaned MHDP dataset, calculate psychometric 
#                         totals, define clinical thresholds, and generate all 
#                         descriptive statistics, prevalence rates, and plots.
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

cat(strrep("=", 80), "\n")
cat("1. IMPORTING AND PREPARING DATASET\n")
cat(strrep("-", 80), "\n")

mhdp <- read_excel("../datasets/mhdp_data.xlsx", col_types = "text")

# Safely convert psychometric and demographic columns to numeric (No index numbers!)
mhdp <- mhdp %>%
  mutate(across(c(
    age, form, gender, timepoint,
    starts_with("phq_"), starts_with("gad_"), starts_with("swemwbs_"), 
    starts_with("epoch_"), starts_with("pils_"), starts_with("mspss_"),
    starts_with("gq_"), starts_with("pcs_"), starts_with("ses_")
  ), as.numeric))

cat("✓ Master dataset 'mhdp_data.xlsx' loaded:", nrow(mhdp), "rows\n\n")


#-------------------------------------------------------------------------------
# 2. CALCULATE SCALE TOTALS & CLINICAL CUTOFFS
#-------------------------------------------------------------------------------

cat("2. CALCULATING SCALE TOTALS AND CLINICAL CLASSIFICATIONS\n")
cat(strrep("-", 80), "\n")

# Compute Totals
mhdp <- mhdp %>%
  mutate(
    phq_total = if_else(if_any(phq_1:phq_8, is.na), NA_real_, rowSums(across(phq_1:phq_8))),
    gad_total = if_else(if_any(gad_1:gad_7, is.na), NA_real_, rowSums(across(gad_1:gad_7))),
    swemwbs_total = if_else(if_any(swemwbs_1:swemwbs_7, is.na), NA_real_, rowSums(across(swemwbs_1:swemwbs_7)))
  ) %>%
  relocate(phq_total, .after = phq_8) %>%
  relocate(gad_total, .after = gad_7) %>%
  relocate(swemwbs_total, .after = swemwbs_7)

# Classify Severity Levels
mhdp <- mhdp %>%
  mutate(
    Depression_Level = case_when(
      phq_total <= 4 ~ "Minimal depression",
      between(phq_total, 5, 9) ~ "Mild depression",
      between(phq_total, 10, 14) ~ "Moderate depression",
      phq_total >= 15 ~ "Severe depression",
      TRUE ~ NA_character_
    ),
    Anxiety_Level = case_when(
      gad_total <= 4 ~ "Minimal anxiety",
      between(gad_total, 5, 9) ~ "Mild anxiety",
      between(gad_total, 10, 14) ~ "Moderate anxiety",
      gad_total >= 15 ~ "Severe anxiety",
      TRUE ~ NA_character_
    ),
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
  ) %>%
  relocate(Depression_Level, .after = phq_total) %>%
  relocate(Anxiety_Level, .after = gad_total) %>%
  relocate(Clinical_Dep_Sample, .after = Depression_Level) %>%
  relocate(Clinical_Anx_Sample, .after = Anxiety_Level)

cat("✓ Psychometric scales computed and clinical thresholds applied\n\n")


#-------------------------------------------------------------------------------
# 3. OVERALL SAMPLE DESCRIPTION
#-------------------------------------------------------------------------------

cat("3. SAMPLE DESCRIPTION\n")
cat(strrep("-", 80), "\n")

cat("Timepoint Distribution by County:\n")
print(table(mhdp$school_county, mhdp$timepoint, useNA = "ifany"))

# --- Dataset A ---
cat("\n--- Dataset A ---\n")
cat("Participants:", length(unique(mhdp$participant_id[mhdp$dataset == "Dataset A"])), "\n")
cat("Schools:", length(unique(mhdp$school_name[mhdp$dataset == "Dataset A"])), "\n")
print(mhdp %>% filter(dataset == "Dataset A") %>% distinct(participant_id, school_county) %>% count(school_county) %>% mutate(percentage = 100 * n / sum(n)))

# --- Dataset B ---
cat("\n--- Dataset B ---\n")
cat("Participants (Excluding OrandoKisumu):", length(unique(mhdp$participant_id[mhdp$dataset == "Dataset B" & mhdp$school_name != "OrandoKisumu"])), "\n")
print(mhdp %>% filter(dataset == "Dataset B", school_name != "OrandoKisumu") %>% distinct(participant_id, school_county) %>% count(school_county) %>% mutate(percentage = 100 * n / sum(n)))

# --- Dataset C ---
cat("\n--- Dataset C ---\n")
cat("Participants:", length(unique(mhdp$participant_id[mhdp$dataset == "Dataset C"])), "\n")
cat("Schools:", length(unique(mhdp$school_name[mhdp$dataset == "Dataset C"])), "\n")
print(mhdp %>% filter(dataset == "Dataset C") %>% distinct(participant_id, school_county) %>% count(school_county) %>% mutate(percentage = 100 * n / sum(n)))
cat("\n")


#-------------------------------------------------------------------------------
# 4. GENDER DISTRIBUTION PLOTS (FULL & CLINICAL)
#-------------------------------------------------------------------------------

cat("4. GENERATING GENDER DISTRIBUTION PLOTS\n")
cat(strrep("-", 80), "\n")

mhdp_baseline <- mhdp %>% filter(timepoint == 0)

# Create plotting function for gender
plot_gender <- function(data_subset, title_text, filename) {
  gender_data <- data_subset %>%
    mutate(gender_label = case_when(
      gender == 1 ~ "Female",
      gender == 2 ~ "Male",
      TRUE ~ "Not Answered"
    )) %>%
    count(gender_label) %>%
    rename(gender = gender_label, count = n) %>%
    mutate(
      percentage = round((count / sum(count)) * 100, 1),
      label_text = paste0(count, " (", percentage, "%)")
    )
  
  p <- ggplot(gender_data, aes(x = gender, y = count, fill = gender)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = label_text), vjust = -0.5, fontface = "bold") +
    scale_fill_manual(values = c("Male" = "#9A8EE6", "Female" = "#132964", "Not Answered" = "grey")) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(title = title_text, x = "Gender", y = "Number of Participants") +
    theme_minimal(base_size = 14) +
    theme(legend.position = "none")
  
  ggsave(paste0("../plots/", filename), p, width = 8, height = 6, dpi = 300)
}

# Full Sample Gender Plot
plot_gender(mhdp_baseline, "Gender Distribution of Participants (Full Sample)", "gender_distribution_bar.png")

# Clinical Subsample Gender Plot
clinical_ids <- mhdp_baseline %>%
  filter(Clinical_Dep_Sample == "Yes" | Clinical_Anx_Sample == "Yes") %>%
  pull(participant_id)

plot_gender(mhdp_baseline %>% filter(participant_id %in% clinical_ids), 
            "Gender Distribution (Clinical Subsample)", "gender_distribution_clinical_bar.png")

cat("✓ Gender plots saved\n\n")


#-------------------------------------------------------------------------------
# 5. PREVALENCE RATES & COMPARISON BAR CHARTS
#-------------------------------------------------------------------------------

cat("5. CALCULATING PREVALENCE RATES\n")
cat(strrep("-", 80), "\n")

# --- Baseline vs Endline Comparison (Shamiri vs Control) ---
df_comp <- mhdp %>%
  filter(timepoint %in% c(0, 4)) %>%
  mutate(time_label = ifelse(timepoint == 0, "Baseline", "Endline"))

dep_summary <- df_comp %>%
  group_by(condition, time_label) %>%
  summarise(n = n(), clinical_cases = sum(Clinical_Dep_Sample == "Yes", na.rm = TRUE), prevalence = (clinical_cases / n) * 100, .groups = "drop") %>%
  mutate(measure = "Depression")

anx_summary <- df_comp %>%
  group_by(condition, time_label) %>%
  summarise(n = n(), clinical_cases = sum(Clinical_Anx_Sample == "Yes", na.rm = TRUE), prevalence = (clinical_cases / n) * 100, .groups = "drop") %>%
  mutate(measure = "Anxiety")

combined_comp <- bind_rows(dep_summary, anx_summary)

ggplot(combined_comp, aes(x = time_label, y = prevalence, fill = condition)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(prevalence, 1)), position = position_dodge(width = 0.9), vjust = -0.5) +
  scale_fill_manual(values = c("#9A8EE6", "#132964")) +
  facet_wrap(~ measure) +
  labs(title = "Baseline vs Endline Prevalence\nShamiri vs Control", x = "Timepoint", y = "Prevalence (%)", fill = "Condition") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")

ggsave("../plots/baseline_endline_comparison.png", width = 10, height = 7, dpi = 300)
cat("✓ Prevalence comparison plot saved\n\n")


#-------------------------------------------------------------------------------
# 6. SCORE TRAJECTORY SUMMARY EXPORT & PLOTS
#-------------------------------------------------------------------------------

cat("6. GENERATING SCORE SUMMARIES AND TRAJECTORY PLOTS\n")
cat(strrep("-", 80), "\n")

# Reusable function to calculate summaries and generate plots
generate_trajectory_report <- function(data_subset, file_suffix, title_suffix) {
  
  summary_df <- data_subset %>%
    group_by(dataset, condition, timepoint) %>%
    summarise(
      n             = sum(!is.na(phq_total)),
      mean_phq      = mean(phq_total, na.rm = TRUE),
      sd_phq        = sd(phq_total, na.rm = TRUE),
      ci95_phq_low  = mean_phq - 1.96 * (sd_phq / sqrt(n)),
      ci95_phq_high = mean_phq + 1.96 * (sd_phq / sqrt(n)),
      
      mean_gad      = mean(gad_total, na.rm = TRUE),
      sd_gad        = sd(gad_total, na.rm = TRUE),
      ci95_gad_low  = mean_gad - 1.96 * (sd_gad / sqrt(n)),
      ci95_gad_high = mean_gad + 1.96 * (sd_gad / sqrt(n)),
      
      mean_swe      = mean(swemwbs_total, na.rm = TRUE),
      sd_swe        = sd(swemwbs_total, na.rm = TRUE),
      ci95_swe_low  = mean_swe - 1.96 * (sd_swe / sqrt(n)),
      ci95_swe_high = mean_swe + 1.96 * (sd_swe / sqrt(n)),
      .groups = "drop"
    ) %>%
    arrange(dataset, condition, timepoint)
  
  write_xlsx(summary_df, paste0("../results/phq_gad_swemwbs", file_suffix, ".xlsx"))
  
  # Plot PHQ
  p_phq <- ggplot(summary_df, aes(x = timepoint, y = mean_phq, color = condition, group = condition)) +
    geom_line(linewidth = 1.2) + geom_point(size = 2) +
    geom_errorbar(aes(ymin = ci95_phq_low, ymax = ci95_phq_high), width = 0.3) +
    scale_color_manual(values = c("#9A8EE6", "#132964")) +
    facet_wrap(~ dataset, scales = "free_x") +
    labs(title = paste("PHQ Scores Across Timepoints", title_suffix), x = "Timepoint (weeks)", y = "Mean PHQ Score", color = "Condition") + 
    theme_minimal(base_size = 14) + theme(legend.position = "bottom")
  ggsave(paste0("../plots/phq_scores", file_suffix, ".png"), p_phq, width = 10, height = 7, dpi = 300)
  
  # Plot GAD
  p_gad <- ggplot(summary_df, aes(x = timepoint, y = mean_gad, color = condition, group = condition)) +
    geom_line(linewidth = 1.2) + geom_point(size = 2) +
    geom_errorbar(aes(ymin = ci95_gad_low, ymax = ci95_gad_high), width = 0.3) +
    scale_color_manual(values = c("#9A8EE6", "#132964")) +
    facet_wrap(~ dataset, scales = "free_x") +
    labs(title = paste("GAD Scores Across Timepoints", title_suffix), x = "Timepoint (weeks)", y = "Mean GAD Score", color = "Condition") + 
    theme_minimal(base_size = 14) + theme(legend.position = "bottom")
  ggsave(paste0("../plots/gad_scores", file_suffix, ".png"), p_gad, width = 10, height = 6, dpi = 300)
}

# Generate for Full Sample
generate_trajectory_report(mhdp, "", "")

# Generate for Clinical Subsample
generate_trajectory_report(mhdp %>% filter(participant_id %in% clinical_ids), "_clinical", "(Clinical Subsample)")

cat("✓ Trajectory summaries written to Excel and plots saved.\n")
cat(strrep("=", 80), "\n")
cat("DESCRIPTIVES SCRIPT COMPLETE\n")
cat(strrep("=", 80), "\n\n")

#-------------------------------------------------------------------------------
# 7. CHANGE SCORE PLOTS (PRE-POST)
#-------------------------------------------------------------------------------

cat("\n7. GENERATING PRE-POST CHANGE SCORE PLOTS\n")
cat(strrep("-", 80), "\n")

# Function 1: Prepare data by calculating Endpoint - Baseline change scores
prep_change_plot_data <- function(df, outcome_col) {
  df %>%
    filter(timepoint %in% c(0, 4)) %>%
    select(participant_id, dataset, condition, timepoint, all_of(outcome_col)) %>%
    # Pivot to wide format to calculate the difference
    pivot_wider(
      names_from = timepoint, 
      values_from = all_of(outcome_col), 
      names_prefix = "T"
    ) %>%
    # Calculate change (Endpoint - Baseline)
    mutate(change_score = T4 - T0) %>%
    filter(!is.na(change_score)) %>%
    # Group and summarize for plotting
    group_by(dataset, condition) %>%
    summarise(
      n = n(),
      mean_change = mean(change_score, na.rm = TRUE),
      sd_change = sd(change_score, na.rm = TRUE),
      se_change = sd_change / sqrt(n),
      .groups = "drop"
    )
}

# Function 2: Generate the Bar Plot
create_change_bar_plot <- function(prep_df, outcome_name, title_suffix) {
  ggplot(prep_df, aes(x = condition, y = mean_change, fill = condition)) +
    geom_col(width = 0.6, position = position_dodge()) +
    geom_errorbar(
      aes(ymin = mean_change - se_change, ymax = mean_change + se_change),
      width = 0.2, linewidth = 0.8, position = position_dodge(0.6)
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    scale_fill_manual(values = c("Shamiri" = "#9A8EE6", "TAU" = "#132964")) +
    facet_wrap(~ dataset, scales = "free_x") +
    labs(
      title = paste("Change in", outcome_name, "Scores", title_suffix),
      subtitle = "Negative values indicate symptom reduction (Endpoint - Baseline)",
      x = "Condition",
      y = paste("Mean Change in", outcome_name),
      fill = "Condition"
    ) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "bottom")
}

# --- FULL SAMPLE CHANGE PLOTS ---

# PHQ Change (Full Sample)
phq_change_full <- prep_change_plot_data(mhdp, "phq_total")
p_phq_change <- create_change_bar_plot(phq_change_full, "PHQ-8", "(Full Sample)")
ggsave("../plots/phq_change_full.png", p_phq_change, width = 10, height = 6, dpi = 300)

# GAD Change (Full Sample)
gad_change_full <- prep_change_plot_data(mhdp, "gad_total")
p_gad_change <- create_change_bar_plot(gad_change_full, "GAD-7", "(Full Sample)")
ggsave("../plots/gad_change_full.png", p_gad_change, width = 10, height = 6, dpi = 300)


# --- CLINICAL SUBSAMPLE CHANGE PLOTS ---

# Define the clinical subset explicitly using the clinical_ids we found earlier
mhdp_clinical <- mhdp %>% filter(participant_id %in% clinical_ids)

# PHQ Change (Clinical)
phq_change_clin <- prep_change_plot_data(mhdp_clinical, "phq_total")
p_phq_change_clin <- create_change_bar_plot(phq_change_clin, "PHQ-8", "(Clinical Subsample)")
ggsave("../plots/phq_change_clinical.png", p_phq_change_clin, width = 10, height = 6, dpi = 300)

# GAD Change (Clinical)
gad_change_clin <- prep_change_plot_data(mhdp_clinical, "gad_total")
p_gad_change_clin <- create_change_bar_plot(gad_change_clin, "GAD-7", "(Clinical Subsample)")
ggsave("../plots/gad_change_clinical.png", p_gad_change_clin, width = 10, height = 6, dpi = 300)

cat("✓ Pre-Post Change bar plots generated and saved.\n\n")

cat(strrep("=", 80), "\n")
cat("DESCRIPTIVES SCRIPT COMPLETE\n")
cat(strrep("=", 80), "\n\n")

################################################################################
# END OF SCRIPT
################################################################################