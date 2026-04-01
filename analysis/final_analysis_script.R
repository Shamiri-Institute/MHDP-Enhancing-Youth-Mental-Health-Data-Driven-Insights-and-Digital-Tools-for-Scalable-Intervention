################################################################################
# PROJECT: MENTAL HEALTH DATA PRIZE
# PURPOSE: Data Analysis, Prevalence, Effect Sizes, and Meta-Analyses
# Strategy Documentation: Analyzing the consolidated MHDP dataset to extract 
#                         baseline/endpoint prevalence rates, Cohen's d effect 
#                         sizes, meta-analytic pooling (within & between), 
#                         subgroup analyses, and mixed-effects trajectories.
################################################################################

#-------------------------------------------------------------------------------
# 1. ENVIRONMENT SETUP
#-------------------------------------------------------------------------------

# Set working directory to script location
if (requireNamespace("rstudioapi", quietly = TRUE)) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

# Clear workspace and set global options
rm(list = ls())
options(scipen = 999, stringsAsFactors = FALSE)
set.seed(54021)

# Package Management
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  rJava,        # Java interface
  tidyverse,    # Data wrangling (dplyr, ggplot2, readr, tidyr, etc.)
  readxl,       # Reading Excel files
  listr,        # List manipulation
  tools,        # Base tools
  openxlsx,     # Excel reading/writing
  stringr,      # String manipulation
  writexl,      # Writing Excel files
  psych,        # Cronbach's Alpha and psychological stats
  lsr,          # Cohen's d calculations
  meta,         # Meta-analysis functions
  effsize,      # Effect size computation
  esc,          # Effect size conversion
  lme4,         # Mixed-effects models
  lmerTest,     # P-values for mixed-effects models
  ggeffects,    # Marginal effects for plotting LMMs
  conflicted    # Namespace safety
)

# Resolve package conflicts
conflict_prefer("filter", "dplyr")
conflict_prefer("mutate", "dplyr")
conflict_prefer("select", "dplyr")

cat(strrep("=", 80), "\n")
cat("✓ Environment setup complete. Packages loaded.\n")
cat(strrep("=", 80), "\n\n")


#-------------------------------------------------------------------------------
# 2. DATA IMPORT & PREPARATION
#-------------------------------------------------------------------------------

cat("1. IMPORTING AND PREPARING DATASET\n")
cat(strrep("-", 80), "\n")

mhdp <- read_excel("../datasets/mhdp_data.xlsx", col_types = "text")

colnames(mhdp)

# Convert relevant psychometric columns to numeric
mhdp <- mhdp %>%
  mutate(across(c(2, 17, 18, 23:123), as.numeric))

cat("✓ Master dataset 'mhdp_data.xlsx' loaded:", nrow(mhdp), "rows\n\n")


#-------------------------------------------------------------------------------
# 3. SAMPLE DESCRIPTION
#-------------------------------------------------------------------------------



cat("2. SAMPLE DESCRIPTION\n")
cat(strrep("-", 80), "\n")

cat("Timepoint Distribution by County:\n")
print(table(mhdp$school_county, mhdp$timepoint, useNA = "ifany"))

# --- Dataset A ---
cat("\n--- Dataset A ---\n")
cat("Participants:", length(unique(mhdp$participant_id[mhdp$dataset == "Dataset A"])), "\n")
cat("Schools:", length(unique(mhdp$school_name[mhdp$dataset == "Dataset A"])), "\n")

school_county_datasetA <- mhdp %>%
  filter(dataset == "Dataset A") %>%
  distinct(participant_id, school_county) %>%
  count(school_county) %>%
  mutate(percentage = 100 * n / sum(n))
print(school_county_datasetA)

# --- Dataset B ---
cat("\n--- Dataset B ---\n")
cat("Participants (Initial):", length(unique(mhdp$participant_id[mhdp$dataset == "Dataset B"])), "\n")
cat("Schools (Initial):", length(unique(mhdp$school_name[mhdp$dataset == "Dataset B"])), "\n")

# Eliminating OrandoKisumu due to different condition assignment
school_county_datasetB <- mhdp %>%
  filter(dataset == "Dataset B", school_name != "OrandoKisumu") %>%
  distinct(participant_id, school_county) %>%
  count(school_county) %>%
  mutate(percentage = 100 * n / sum(n))
cat("\nFiltered Dataset B (Excluding OrandoKisumu):\n")
print(school_county_datasetB)

# --- Dataset C ---
cat("\n--- Dataset C ---\n")
cat("Participants:", length(unique(mhdp$participant_id[mhdp$dataset == "Dataset C"])), "\n")
cat("Schools:", length(unique(mhdp$school_name[mhdp$dataset == "Dataset C"])), "\n")

school_county_datasetC <- mhdp %>%
  filter(dataset == "Dataset C") %>%
  distinct(participant_id, school_county) %>%
  count(school_county) %>%
  mutate(percentage = 100 * n / sum(n))
print(school_county_datasetC)
cat("\n")


8#-------------------------------------------------------------------------------
# 4. CALCULATE SCALE TOTALS & CLINICAL CUTOFFS
#-------------------------------------------------------------------------------

cat("3. CALCULATING SCALE TOTALS AND CLINICAL CLASSIFICATIONS\n")
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
# 5. PREVALENCE RATES & VISUALIZATIONS
#-------------------------------------------------------------------------------

cat("4. CALCULATING PREVALENCE RATES AND PLOTTING\n")
cat(strrep("-", 80), "\n")

mhdp_baseline <- mhdp %>% filter(timepoint == 0)
mhdp_end      <- mhdp %>% filter(timepoint == 4)

# --- Baseline Prevalence ---
baseline_depression_by_dataset <- mhdp_baseline %>%
  group_by(dataset) %>%
  summarise(
    n = n(),
    clinical_cases = sum(Clinical_Dep_Sample == "Yes", na.rm = TRUE),
    prevalence_rate = (clinical_cases / n) * 100
  )

baseline_anxiety_by_dataset <- mhdp_baseline %>%
  group_by(dataset) %>%
  summarise(
    n = n(),
    clinical_cases = sum(Clinical_Anx_Sample == "Yes", na.rm = TRUE),
    prevalence_rate = (clinical_cases / n) * 100
  )

baseline_combined <- baseline_depression_by_dataset %>%
  select(dataset, depression = prevalence_rate) %>%
  left_join(baseline_anxiety_by_dataset %>% select(dataset, anxiety = prevalence_rate), by = "dataset") %>%
  pivot_longer(cols = c(depression, anxiety), names_to = "condition", values_to = "prevalence_rate")

ggplot(baseline_combined, aes(x = dataset, y = prevalence_rate, fill = condition)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(prevalence_rate, 1)), position = position_dodge(width = 0.9), vjust = -0.5) +
  scale_fill_manual(values = c("depression" = "#9A8EE6", "anxiety" = "#132964")) +
  labs(title = "Baseline Depression and Anxiety Prevalence by Dataset", x = "Dataset", y = "Prevalence Rate (%)", fill = "Condition") +
  theme_minimal()
ggsave("../plots/baseline_prevalence_by_dataset.png", width = 8, height = 6, dpi = 300)

# --- Endpoint Prevalence ---
end_depression_by_dataset <- mhdp_end %>%
  group_by(dataset) %>%
  summarise(
    n = n(),
    clinical_cases = sum(Clinical_Dep_Sample == "Yes", na.rm = TRUE),
    prevalence_rate = (clinical_cases / n) * 100
  )

end_anxiety_by_dataset <- mhdp_end %>%
  group_by(dataset) %>%
  summarise(
    n = n(),
    clinical_cases = sum(Clinical_Anx_Sample == "Yes", na.rm = TRUE),
    prevalence_rate = (clinical_cases / n) * 100
  )

end_combined <- end_depression_by_dataset %>%
  select(dataset, depression = prevalence_rate) %>%
  left_join(end_anxiety_by_dataset %>% select(dataset, anxiety = prevalence_rate), by = "dataset") %>%
  pivot_longer(cols = c(depression, anxiety), names_to = "condition", values_to = "prevalence_rate")

ggplot(end_combined, aes(x = dataset, y = prevalence_rate, fill = condition)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(prevalence_rate, 1)), position = position_dodge(width = 0.9), vjust = -0.5) +
  scale_fill_manual(values = c("depression" = "#9A8EE6", "anxiety" = "#132964")) +
  labs(title = "Endpoint Depression and Anxiety Prevalence by Dataset", x = "Dataset", y = "Prevalence Rate (%)", fill = "Condition") +
  theme_minimal()
ggsave("../plots/end_prevalence_by_dataset.png", width = 8, height = 6, dpi = 300)

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
  scale_fill_manual(values = c("#9A8EE6", "#132964")) + # Added default colors
  facet_wrap(~ measure) +
  labs(title = "Baseline vs Endline Prevalence\nShamiri vs Control", x = "Timepoint", y = "Prevalence (%)", fill = "Condition") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom") # Moved legend to bottom to save space

ggsave("../plots/baseline_endline_comparison.png", width = 10, height = 6, dpi = 300)



# --- Gender Distribution Bar Chart ---
gender_data <- data.frame(gender = c("Male", "Female", "Not Answered"), count = c(3436, 2368, 311))
ggplot(gender_data, aes(x = gender, y = count, fill = gender)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = count), vjust = -0.5) +
  scale_fill_manual(values = c("Male" = "#9A8EE6", "Female" = "#132964", "Not Answered" = "grey")) +
  labs(title = "Gender Distribution of Participants", x = "Gender", y = "Number of Participants") +
  theme_minimal()
ggsave("../plots/gender_distribution_bar.png", width = 8, height = 6, dpi = 300)

cat("✓ Prevalence calculated and plots saved\n\n")


#-------------------------------------------------------------------------------
# 6. SUMMARY STATS EXPORT & SCORE PLOTS
#-------------------------------------------------------------------------------

cat("5. GENERATING SUMMARY EXPORT AND SCORE TRAJECTORY PLOTS\n")
cat(strrep("-", 80), "\n")

summary_df <- mhdp %>%
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

write_xlsx(summary_df, "../results/phq_gad_swemwbs.xlsx")

print(summary_df, n = Inf)


# Plots
p_phq <- ggplot(summary_df, aes(x = as.numeric(timepoint), y = mean_phq, color = condition, group = condition)) +
  geom_line(linewidth = 1.2) + 
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = ci95_phq_low, ymax = ci95_phq_high), width = 0.3) +
  scale_color_manual(values = c("#9A8EE6", "#132964")) +
  facet_wrap(~ dataset, scales = "free_x") +
  labs(title = "PHQ Scores Across Timepoints", x = "Timepoint (weeks)", y = "Mean PHQ Score", color = "Condition") + 
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom") # Moves legend to the bottom

ggsave("../plots/phq_scores.png", p_phq, width = 10, height = 6, dpi = 300) # Increased width to 10

p_gad <- ggplot(summary_df, aes(x = as.numeric(timepoint), y = mean_gad, color = condition, group = condition)) +
  geom_line(linewidth = 1.2) + 
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = ci95_gad_low, ymax = ci95_gad_high), width = 0.3) +
  scale_color_manual(values = c("#9A8EE6", "#132964")) +
  facet_wrap(~ dataset, scales = "free_x") +
  labs(title = "GAD Scores Across Timepoints", x = "Timepoint (weeks)", y = "Mean GAD Score", color = "Condition") + 
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom") # Moves legend to the bottom

ggsave("../plots/gad_scores.png", p_gad, width = 10, height = 6, dpi = 300) # Increased width to 10

cat("✓ Score summaries written to 'phq_gad_swemwbs.xlsx' and plots saved\n\n")



#-------------------------------------------------------------------------------
# CREATE CLINICAL SUBSAMPLE (Elevated PHQ or GAD at Baseline)
#-------------------------------------------------------------------------------

# Identify IDs of participants who were clinical at Baseline (timepoint == 0)
clinical_ids <- mhdp %>%
  filter(timepoint == 0 & (Clinical_Dep_Sample == "Yes" | Clinical_Anx_Sample == "Yes")) %>%
  pull(participant_id)

# Subset the longitudinal data for only those participants
mhdp_clinical <- mhdp %>%
  filter(participant_id %in% clinical_ids)

#-------------------------------------------------------------------------------
# GENDER DISTRIBUTION (CLINICAL SAMPLE)
#-------------------------------------------------------------------------------

# Dynamically calculate gender counts for the clinical sample at baseline
gender_data_clinical <- mhdp_clinical %>%
  filter(timepoint == 0) %>%
  mutate(gender_label = case_when(
    gender == 1 ~ "Female",
    gender == 2 ~ "Male",
    TRUE ~ "Not Answered"
  )) %>%
  count(gender_label) %>%
  rename(gender = gender_label, count = n)

# Plot Gender Distribution for Clinical Sample
ggplot(gender_data_clinical, aes(x = gender, y = count, fill = gender)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = count), vjust = -0.5) +
  scale_fill_manual(values = c("Male" = "#9A8EE6", "Female" = "#132964", "Not Answered" = "grey")) +
  labs(title = "Gender Distribution (Clinical Subsample)", x = "Gender", y = "Number of Participants") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none") # No need for legend when x-axis is already labeled

ggsave("../plots/gender_distribution_clinical_bar.png", width = 8, height = 6, dpi = 300)

cat("✓ Clinical prevalence calculated and gender plot saved\n\n")


#-------------------------------------------------------------------------------
# SUMMARY STATS EXPORT & SCORE PLOTS (CLINICAL SAMPLE)
#-------------------------------------------------------------------------------

cat("GENERATING CLINICAL SUMMARY EXPORT AND SCORE TRAJECTORY PLOTS\n")
cat(strrep("-", 80), "\n")

# Calculate summaries using only the clinical subset
summary_df_clinical <- mhdp_clinical %>%
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

write_xlsx(summary_df_clinical, "../results/phq_gad_swemwbs_clinical.xlsx")

print(summary_df_clinical, n = Inf)


# Clinical PHQ Plot
p_phq_clinical <- ggplot(summary_df_clinical, aes(x = as.numeric(timepoint), y = mean_phq, color = condition, group = condition)) +
  geom_line(linewidth = 1.2) + 
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = ci95_phq_low, ymax = ci95_phq_high), width = 0.3) +
  scale_color_manual(values = c("#9A8EE6", "#132964")) +
  facet_wrap(~ dataset, scales = "free_x") +
  labs(title = "PHQ Scores Across Timepoints (Clinical Subsample)", x = "Timepoint (weeks)", y = "Mean PHQ Score", color = "Condition") + 
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom") 

ggsave("../plots/phq_scores_clinical.png", p_phq_clinical, width = 10, height = 6, dpi = 300) 


# Clinical GAD Plot
p_gad_clinical <- ggplot(summary_df_clinical, aes(x = as.numeric(timepoint), y = mean_gad, color = condition, group = condition)) +
  geom_line(linewidth = 1.2) + 
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = ci95_gad_low, ymax = ci95_gad_high), width = 0.3) +
  scale_color_manual(values = c("#9A8EE6", "#132964")) +
  facet_wrap(~ dataset, scales = "free_x") +
  labs(title = "GAD Scores Across Timepoints (Clinical Subsample)", x = "Timepoint (weeks)", y = "Mean GAD Score", color = "Condition") + 
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom") 

ggsave("../plots/gad_scores_clinical.png", p_gad_clinical, width = 10, height = 6, dpi = 300) 

cat("✓ Clinical score summaries written to 'phq_gad_swemwbs_clinical.xlsx' and plots saved\n\n")

























#-------------------------------------------------------------------------------
# 7. EFFECT SIZES (COHEN'S D)
#-------------------------------------------------------------------------------

cat("6. CALCULATING RAW EFFECT SIZES\n")
cat(strrep("-", 80), "\n")

# -- PHQ Effect Sizes --
datasetA <- mhdp %>% filter(dataset == "Dataset A", timepoint %in% c(0,2,4,8)) %>%
  select(participant_id, timepoint, phq_total, condition, school_name) %>%
  pivot_wider(names_from = timepoint, values_from = phq_total, names_prefix = "week_")
cat("Dataset A (PHQ) Within-Group (W4-W0):", cohensD(datasetA$week_4, datasetA$week_0, method = "paired"), "\n")

datasetB <- mhdp %>% filter(dataset == "Dataset B", timepoint %in% c(0,2,4,8)) %>%
  select(participant_id, timepoint, phq_total, condition, school_name) %>%
  pivot_wider(names_from = timepoint, values_from = phq_total, names_prefix = "week_")

orando <- datasetB %>% filter(school_name == "OrandoKisumu")
cat("Dataset B Orando (PHQ) Between-Group (W4):", cohensD(week_4 ~ condition, data = orando, method = "pooled"), "\n")

datasetC <- mhdp %>% filter(dataset == "Dataset C", timepoint %in% c(0,2,4,8,40,56)) %>%
  select(participant_id, timepoint, phq_total, condition, school_name) %>%
  pivot_wider(names_from = timepoint, values_from = phq_total, names_prefix = "week_")
cat("Dataset C (PHQ) Between-Group (W4):", cohensD(week_4 ~ condition, data = datasetC, method = "pooled"), "\n")

# -- GAD Effect Sizes --
datasetA_gad <- mhdp %>% filter(dataset == "Dataset A", timepoint %in% c(0,2,4,8)) %>%
  select(participant_id, timepoint, gad_total, condition, school_name) %>%
  pivot_wider(names_from = timepoint, values_from = gad_total, names_prefix = "week_")
cat("Dataset A (GAD) Within-Group (W4-W0):", cohensD(datasetA_gad$week_4, datasetA_gad$week_0, method = "paired"), "\n")

orando_gad <- mhdp %>% filter(dataset == "Dataset B", school_name == "OrandoKisumu", timepoint %in% c(0,2,4,8)) %>%
  select(participant_id, timepoint, gad_total, condition) %>%
  pivot_wider(names_from = timepoint, values_from = gad_total, names_prefix = "week_")
cat("Dataset B Orando (GAD) Between-Group (W4):", cohensD(week_4 ~ condition, data = orando_gad, method = "pooled"), "\n")

datasetC_gad <- mhdp %>% filter(dataset == "Dataset C", timepoint %in% c(0,2,4,8,40,56)) %>%
  select(participant_id, timepoint, gad_total, condition) %>%
  pivot_wider(names_from = timepoint, values_from = gad_total, names_prefix = "week_")
cat("Dataset C (GAD) Between-Group (W4):", cohensD(week_4 ~ condition, data = datasetC_gad, method = "pooled"), "\n\n")


#-------------------------------------------------------------------------------
# 8. META-ANALYSIS (PHQ & GAD)
#-------------------------------------------------------------------------------

cat("7. META-ANALYSES\n")
cat(strrep("-", 80), "\n")

datasetB_two_conditions <- mhdp %>% filter(dataset == "Dataset B", school_name == "OrandoKisumu")
datasetC_all_schools <- mhdp %>% filter(dataset == "Dataset C")

# Extract Meta-Analysis components for PHQ (Between Group)
get_meta_stats <- function(df, outcome_var, cond, tp = 4) {
  df %>% filter(condition == cond, timepoint == tp) %>%
    summarise(
      n = sum(!is.na(.data[[outcome_var]])),
      m = mean(.data[[outcome_var]], na.rm = TRUE),
      sd = sd(.data[[outcome_var]], na.rm = TRUE)
    )
}

# PHQ Between
b_tau_phq <- get_meta_stats(datasetB_two_conditions, "phq_total", "TAU")
b_sha_phq <- get_meta_stats(datasetB_two_conditions, "phq_total", "Shamiri")
c_tau_phq <- get_meta_stats(datasetC_all_schools, "phq_total", "TAU")
c_sha_phq <- get_meta_stats(datasetC_all_schools, "phq_total", "Shamiri")

meta_input_phq <- data.frame(
  study = c("Dataset B (Orando)", "Dataset C"),
  n.e = c(b_sha_phq$n, c_sha_phq$n), mean.e = c(b_sha_phq$m, c_sha_phq$m), sd.e = c(b_sha_phq$sd, c_sha_phq$sd),
  n.c = c(b_tau_phq$n, c_tau_phq$n), mean.c = c(b_tau_phq$m, c_tau_phq$m), sd.c = c(b_tau_phq$sd, c_tau_phq$sd)
)

meta_d_phq <- metacont(n.e=n.e, mean.e=mean.e, sd.e=sd.e, n.c=n.c, mean.c=mean.c, sd.c=sd.c,
                       studlab=study, data=meta_input_phq, sm="SMD", method.smd="Hedges", random=TRUE, method.tau="REML", method.random.ci="HK")
cat("Meta-Analysis Result (PHQ Between-Group):\n")
print(meta_d_phq)
cat("\n")


#-------------------------------------------------------------------------------
# 9. SUBGROUP ANALYSES (AGE AND GENDER)
#-------------------------------------------------------------------------------

cat("8. SUBGROUP META-ANALYSES (Age & Gender)\n")
cat(strrep("-", 80), "\n")

mhdp <- mhdp %>%
  mutate(age_group = case_when(
    age <= 14 ~ "≤14",
    age <= 16 ~ "15–16",
    TRUE ~ "≥17"
  ))

# Helper to compute subgroup data for Between-Group Meta-Analysis
compute_subgroup_meta <- function(df, grouping_var, outcome_var, label) {
  df %>%
    filter(timepoint == 4, condition %in% c("Shamiri", "TAU")) %>%
    group_by(dataset, condition, .data[[grouping_var]]) %>%
    summarise(
      n = sum(!is.na(.data[[outcome_var]])),
      mean_val = mean(.data[[outcome_var]], na.rm = TRUE),
      sd_val = sd(.data[[outcome_var]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_wider(names_from = condition, values_from = c(n, mean_val, sd_val), names_sep = ".") %>%
    filter(!is.na(n.Shamiri) & !is.na(n.TAU) & n.Shamiri > 1 & n.TAU > 1 & !is.na(.data[[grouping_var]])) %>%
    mutate(
      sd_pooled = sqrt(((n.Shamiri - 1) * sd_val.Shamiri^2 + (n.TAU - 1) * sd_val.TAU^2) / (n.Shamiri + n.TAU - 2)),
      cohen_d = (mean_val.Shamiri - mean_val.TAU) / sd_pooled,
      J = 1 - (3 / (4 * (n.Shamiri + n.TAU) - 9)),
      hedges_g = cohen_d * J,
      se_g = sqrt((n.Shamiri + n.TAU) / (n.Shamiri * n.TAU) + (hedges_g^2) / (2 * (n.Shamiri + n.TAU - 2))),
      study = dataset
    )
}

between_input_age_phq <- compute_subgroup_meta(mhdp, "age_group", "phq_total", "PHQ by Age")

meta_between_age_phq <- metagen(
  TE = hedges_g, seTE = se_g, studlab = paste(study, "-", age_group),
  data = between_input_age_phq, sm = "SMD", method.tau = "REML", random = TRUE, subgroup = age_group,
  title = "Between-Group (Shamiri vs TAU) PHQ Change by Age Group"
)
cat("Meta-Analysis (PHQ Between-Group by AGE):\n")
print(summary(meta_between_age_phq))
cat("\n")


#-------------------------------------------------------------------------------
# 10. MIXED-EFFECTS MODELS & PIECEWISE TRAJECTORIES
#-------------------------------------------------------------------------------

cat("9. MIXED-EFFECTS MODELS AND ATTRITION\n")
cat(strrep("-", 80), "\n")

mhdp_clean <- mhdp %>% mutate(timepoint_num = as.numeric(timepoint))

# Base LMM
lmm_phq <- lmer(phq_total ~ timepoint_num * condition + (1 | school_name/participant_id), data = mhdp_clean)
cat("Linear Mixed Model (PHQ):\n")
print(summary(lmm_phq))

# Piecewise LMM
mhdp_piecewise <- mhdp_clean %>%
  mutate(
    time_active = ifelse(timepoint_num <= 4, timepoint_num, 4),
    time_followup = ifelse(timepoint_num > 4, timepoint_num - 4, 0)
  )

lmm_piecewise <- lmer(phq_total ~ (time_active + time_followup) * condition + (1 | school_name/participant_id), data = mhdp_piecewise)

pred_piecewise <- ggpredict(lmm_piecewise, terms = c("time_active", "condition"))
ggplot(pred_piecewise, aes(x = x, y = predicted, color = group)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.1, color = NA) +
  labs(title = "Predicted PHQ-8 Trajectory (Active Phase Focus)", x = "Weeks (Active Phase)", y = "Predicted PHQ Score", color = "Group") +
  theme_minimal()
ggsave("lmm_piecewise_trajectory.png", width = 8, height = 6, dpi = 300)


# Attrition Logic
attrition_df <- mhdp %>%
  group_by(participant_id) %>%
  summarise(
    baseline_phq = phq_total[timepoint == 0][1],
    dropped_out = !any(timepoint == 4),
    gender = gender[1]
  ) %>%
  filter(!is.na(baseline_phq))

attrition_model <- glm(dropped_out ~ baseline_phq + gender, data = attrition_df, family = binomial)
cat("\nAttrition Logistic Regression:\n")
print(summary(attrition_model))


cat("\n")
cat(strrep("=", 80), "\n")
cat("ANALYSIS COMPLETE. ALL PLOTS AND EXPORTS GENERATED.\n")
cat(strrep("=", 80), "\n\n")

################################################################################
# END OF SCRIPT
################################################################################