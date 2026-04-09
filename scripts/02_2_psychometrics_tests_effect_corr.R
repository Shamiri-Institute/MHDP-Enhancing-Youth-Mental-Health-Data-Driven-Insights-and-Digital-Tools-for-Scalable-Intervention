################################################################################
# PROJECT: MENTAL HEALTH DATA PRIZE (OBJECTIVE 2)
# PURPOSE: Part 2 - Psychometrics, Significance Testing, and Effect Sizes
# Strategy Documentation: Calculates Cronbach's alpha and baseline correlations. 
#                         Significance testing (t-tests/Cohen's d) to evaluate 
#                         the efficacy of specific micro-interventions (Growth, 
#                         Gratitude, Values, etc.) against Study Skills control.
################################################################################

#-------------------------------------------------------------------------------
# 1. ENVIRONMENT SETUP & DATA IMPORT
#-------------------------------------------------------------------------------

if (requireNamespace("rstudioapi", quietly = TRUE)) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}
rm(list = ls())
options(scipen = 999, stringsAsFactors = FALSE)

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,    # Data manipulation
  readxl,       # Reading Excel files
  psych,        # Cronbach's Alpha and correlations
  lsr,          # Cohen's d calculations
  officer,      # Exporting to Word
  flextable,    # Formatting tables for Word
  conflicted    # Namespace safety
)

conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")

# Create Objective 2 Output Directories
dir.create("../results/objective_2/tables", recursive = TRUE, showWarnings = FALSE)

cat(strrep("=", 80), "\n")
cat("1. IMPORTING AND PREPARING DATASET\n")
cat(strrep("-", 80), "\n")

mhdp <- read_excel("../datasets/processed/mhdp_data_2.xlsx", col_types = "text")
colnames(mhdp) <- tolower(colnames(mhdp))

# CLEANING: Drop known duplicated student
mhdp <- mhdp %>% filter(!(dataset == "Shamiri_3.0" & participant_id == "40281"))

# Convert psychometric columns to numeric
mhdp <- suppressWarnings(
  mhdp %>%
    mutate(across(c(
      timepoint, starts_with("phq_"), starts_with("gad_"), starts_with("swemwbs_"),
      starts_with("mspss_")), as.numeric))
)

# Compute Totals
mhdp <- mhdp %>%
  mutate(
    phq_total = if_else(if_any(phq_1:phq_8, is.na), NA_real_, rowSums(across(phq_1:phq_8))),
    gad_total = if_else(if_any(gad_1:gad_7, is.na), NA_real_, rowSums(across(gad_1:gad_7))),
    swemwbs_total = if_else(if_any(swemwbs_1:swemwbs_7, is.na), NA_real_, rowSums(across(swemwbs_1:swemwbs_7))),
    mspss_total = if_else(if_any(mspss_1:mspss_12, is.na), NA_real_, rowSums(across(mspss_1:mspss_12)))
  )

# Clean Condition Labels (Specific to Obj 2, per study plan updates)
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
  # Set factor levels so Shamiri is first and "Study Skills" acts as the baseline Control
  mutate(condition = factor(condition, levels = c(
    "shamiri", "gratitude", "growth", "values", "study skills"
  )))

cat("✓ Master dataset loaded, totals computed, and Obj 2 conditions mapped\n\n")


#-------------------------------------------------------------------------------
# GLOBAL DATAFRAMES TO STORE RESULTS FOR WORD EXPORT
#-------------------------------------------------------------------------------
alpha_results <- tibble(Dataset = character(), PHQ_8 = numeric(), GAD_7 = numeric(), SWEMWBS = numeric(), MSPSS = numeric())
stats_results <- tibble(Sample = character(), Intervention = character(), Measure = character(), Comparison = character(), Mean_Diff = numeric(), P_Value = character(), Cohens_D = numeric())


#-------------------------------------------------------------------------------
# 2. INTERNAL CONSISTENCY (CRONBACH'S ALPHA AT WEEK 4)
#-------------------------------------------------------------------------------

cat("2. CALCULATING CRONBACH'S ALPHA (WEEK 4)\n")
cat(strrep("-", 80), "\n")

calculate_reliability <- function(data_subset, dataset_label) {
  a_phq <- NA; a_gad <- NA; a_swe <- NA; a_msp <- NA
  
  phq_items <- data_subset %>% select(phq_1:phq_8) %>% drop_na()
  if(nrow(phq_items) > 0) a_phq <- round(psych::alpha(phq_items, check.keys = TRUE, warnings = FALSE)$total$raw_alpha, 3)
  
  gad_items <- data_subset %>% select(gad_1:gad_7) %>% drop_na()
  if(nrow(gad_items) > 0) a_gad <- round(psych::alpha(gad_items, check.keys = TRUE, warnings = FALSE)$total$raw_alpha, 3)
  
  swe_items <- data_subset %>% select(swemwbs_1:swemwbs_7) %>% drop_na()
  if(nrow(swe_items) > 0) a_swe <- round(psych::alpha(swe_items, check.keys = TRUE, warnings = FALSE)$total$raw_alpha, 3)
  
  msp_items <- data_subset %>% select(mspss_1:mspss_12) %>% drop_na()
  if(nrow(msp_items) > 0) a_msp <- round(psych::alpha(msp_items, check.keys = TRUE, warnings = FALSE)$total$raw_alpha, 3)
  
  alpha_results <<- alpha_results %>% add_row(Dataset = dataset_label, PHQ_8 = a_phq, GAD_7 = a_gad, SWEMWBS = a_swe, MSPSS = a_msp)
}

calculate_reliability(mhdp %>% filter(dataset == "Shamiri_1.0", timepoint == 4), "Shamiri 1.0")
calculate_reliability(mhdp %>% filter(dataset == "Shamiri_2.0", timepoint == 4), "Shamiri 2.0")
calculate_reliability(mhdp %>% filter(dataset == "Shamiri_3.0", timepoint == 4), "Shamiri 3.0")
calculate_reliability(mhdp %>% filter(timepoint == 4), "Full Sample")

cat("✓ Reliability statistics computed\n\n")


#-------------------------------------------------------------------------------
# 3. BASELINE CORRELATIONS
#-------------------------------------------------------------------------------

cat("3. CORRELATION BETWEEN SCALES (BASELINE)\n")
cat(strrep("-", 80), "\n")

cor_data <- mhdp %>% filter(timepoint == 0) %>% select(phq_total, gad_total, swemwbs_total, mspss_total) %>% drop_na()
cor_matrix <- round(cor(cor_data), 3)

cat("Pearson Correlation Matrix:\n")
print(cor_matrix)
cor_df <- as.data.frame(cor_matrix) %>% rownames_to_column(var = "Scale")
cat("\n")


#-------------------------------------------------------------------------------
# HELPER FUNCTIONS FOR SIGNIFICANCE & EFFECT SIZES 
#-------------------------------------------------------------------------------

report_paired <- function(post, pre, label, focus, measure, sample_type) {
  valid <- complete.cases(post, pre)
  p_clean <- post[valid]; pr_clean <- pre[valid]
  if(length(p_clean) < 2) return(NULL)
  
  t_res <- t.test(p_clean, pr_clean, paired = TRUE)
  d_res <- cohensD(p_clean, pr_clean, method = "paired")
  
  mean_diff <- round(unname(t_res$estimate), 3) 
  p_val <- format.pval(t_res$p.value, digits=3, eps=0.001)
  
  cat(sprintf("%-15s | %-5s | %-20s | Diff: %-6.2f | p: %-6s | d: %.3f\n", focus, measure, label, mean_diff, p_val, d_res))
  stats_results <<- stats_results %>% add_row(Sample = sample_type, Intervention = focus, Measure = measure, Comparison = label, Mean_Diff = mean_diff, P_Value = p_val, Cohens_D = round(d_res, 3))
}

report_between <- function(formula, data, label, focus, measure, sample_type) {
  if(length(unique(data$condition)) < 2) return(NULL)
  
  t_res <- t.test(formula, data = data, var.equal = TRUE)
  d_res <- cohensD(formula, data = data, method = "pooled")
  
  mean_diff <- round(unname(t_res$estimate[1] - t_res$estimate[2]), 3)
  p_val <- format.pval(t_res$p.value, digits=3, eps=0.001)
  
  cat(sprintf("%-15s | %-5s | %-20s | Diff: %-6.2f | p: %-6s | d: %.3f\n", focus, measure, label, mean_diff, p_val, d_res))
  stats_results <<- stats_results %>% add_row(Sample = sample_type, Intervention = focus, Measure = measure, Comparison = label, Mean_Diff = mean_diff, P_Value = p_val, Cohens_D = round(d_res, 3))
}


#-------------------------------------------------------------------------------
# 4. PREPARE WIDE DATA (FULL & CLINICAL SAMPLES)
#-------------------------------------------------------------------------------

mhdp_wide <- mhdp %>%
  filter(timepoint %in% c(0, 4)) %>%
  pivot_wider(
    id_cols = c(participant_id, condition), 
    names_from = timepoint, values_from = c(phq_total, gad_total), names_prefix = "W"
  )

clinical_ids <- mhdp %>% filter(timepoint == 0 & (phq_total >= 10 | gad_total >= 10)) %>% pull(participant_id)
mhdp_wide_clin <- mhdp_wide %>% filter(participant_id %in% clinical_ids)


#-------------------------------------------------------------------------------
# 5. RUN TESTING FUNCTION (AUTOMATED ACROSS ALL CONDITIONS)
#-------------------------------------------------------------------------------

run_all_tests <- function(data_wide, sample_label) {
  
  cat("--------------------------------------------------------------------------\n")
  cat(toupper(sample_label), "ANALYSIS\n")
  cat("--------------------------------------------------------------------------\n")
  
  cond_levels <- levels(data_wide$condition)
  
  # NOTE: "study skills" is now the control arm we compare everything against!
  active_arms <- cond_levels[cond_levels != "study skills"]
  
  for(arm in active_arms) {
    df_arm <- data_wide %>% filter(condition == arm)
    df_comp <- data_wide %>% filter(condition %in% c(arm, "study skills")) %>% mutate(condition = droplevels(condition))
    
    # 1. PHQ Tests
    if(nrow(df_arm) > 1) {
      report_paired(df_arm$phq_total_W4, df_arm$phq_total_W0, "W4 vs W0 (Paired)", str_to_title(arm), "PHQ-8", sample_label)
    }
    if(sum(df_comp$condition == arm) > 1 && sum(df_comp$condition == "study skills") > 1) {
      report_between(phq_total_W4 ~ condition, df_comp, "vs Control (Indep)", str_to_title(arm), "PHQ-8", sample_label)
    }
    
    # 2. GAD Tests
    if(nrow(df_arm) > 1) {
      report_paired(df_arm$gad_total_W4, df_arm$gad_total_W0, "W4 vs W0 (Paired)", str_to_title(arm), "GAD-7", sample_label)
    }
    if(sum(df_comp$condition == arm) > 1 && sum(df_comp$condition == "study skills") > 1) {
      report_between(gad_total_W4 ~ condition, df_comp, "vs Control (Indep)", str_to_title(arm), "GAD-7", sample_label)
    }
    cat("\n") 
  }
  
  # Run the "Study Skills" control group on its own for within-group tracking
  df_ctrl <- data_wide %>% filter(condition == "study skills")
  if(nrow(df_ctrl) > 1) {
    report_paired(df_ctrl$phq_total_W4, df_ctrl$phq_total_W0, "W4 vs W0 (Paired)", "Study Skills (Ctrl)", "PHQ-8", sample_label)
    report_paired(df_ctrl$gad_total_W4, df_ctrl$gad_total_W0, "W4 vs W0 (Paired)", "Study Skills (Ctrl)", "GAD-7", sample_label)
  }
  cat("✓ Tests computed and added to results table.\n\n")
}

cat("4. TESTING SIGNIFICANCE & EFFECT SIZES\n\n")
run_all_tests(mhdp_wide, "Full Sample")
run_all_tests(mhdp_wide_clin, "Clinical Subsample")


#-------------------------------------------------------------------------------
# 6. EXPORT TO WORD DOCUMENT
#-------------------------------------------------------------------------------

cat("5. EXPORTING RESULTS TO WORD DOCUMENT\n")
cat(strrep("-", 80), "\n")

doc <- read_docx() %>%
  body_add_par("MHDP (Obj 2) - Psychometrics and Efficacy Effect Sizes", style = "heading 1") %>%
  
  # 1. Correlations
  body_add_par("1. Baseline Correlations Matrix", style = "heading 2") %>%
  body_add_flextable(flextable(cor_df) %>% autofit()) %>%
  body_add_par("") %>%
  
  # 2. Reliability 
  body_add_par("2. Internal Consistency (Cronbach's Alpha at Week 4)", style = "heading 2") %>%
  body_add_flextable(flextable(alpha_results) %>% autofit()) %>%
  body_add_par("") %>%
  
  # 3. T-Tests and Effect Sizes
  body_add_par("3. Micro-Intervention Efficacy & Effect Sizes (Cohen's d)", style = "heading 2") %>%
  body_add_par("Comparing Within-Group reductions (Week 4 vs 0) and Between-Group efficacy (Intervention vs Study Skills Control at Week 4).", style = "Normal") %>%
  body_add_flextable(
    flextable(stats_results) %>% 
      merge_v(j = c("Sample", "Intervention", "Measure")) %>%  
      valign(j = c("Sample", "Intervention", "Measure"), valign = "top") %>%
      autofit()
  )

print(doc, target = "../results/objective_2/tables/Psychometrics_and_EffectSizes.docx")

cat("✓ Word document successfully saved to '../results/objective_2/tables/Psychometrics_and_EffectSizes.docx'\n")
cat(strrep("=", 80), "\n")
cat("PSYCHOMETRICS & SIGNIFICANCE SCRIPT COMPLETE\n")
cat(strrep("=", 80), "\n\n")