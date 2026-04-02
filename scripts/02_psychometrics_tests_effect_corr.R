################################################################################
# PROJECT: MENTAL HEALTH DATA PRIZE
# PURPOSE: Part 2 - Psychometrics, Significance Testing, and Effect Sizes
# Strategy Documentation: Calculates Cronbach's alpha for scale reliability, 
#                         baseline correlations, and conducts t-tests combined 
#                         with Cohen's d. Exports all results to a Word doc.
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

# Create results directory if it doesn't exist
dir.create("../results", showWarnings = FALSE)

cat(strrep("=", 80), "\n")
cat("1. IMPORTING AND PREPARING DATASET\n")
cat(strrep("-", 80), "\n")

mhdp <- read_excel("../datasets/processed/mhdp_data.xlsx", col_types = "text")

# Convert psychometric columns to numeric safely
mhdp <- mhdp %>%
  mutate(across(c(
    timepoint, starts_with("phq_"), starts_with("gad_"), starts_with("swemwbs_"),
    starts_with("mspss_")), as.numeric))

# Compute Totals (Needed for correlations and t-tests)
mhdp <- mhdp %>%
  mutate(
    phq_total = if_else(if_any(phq_1:phq_8, is.na), NA_real_, rowSums(across(phq_1:phq_8))),
    gad_total = if_else(if_any(gad_1:gad_7, is.na), NA_real_, rowSums(across(gad_1:gad_7))),
    swemwbs_total = if_else(if_any(swemwbs_1:swemwbs_7, is.na), NA_real_, rowSums(across(swemwbs_1:swemwbs_7))),
    mspss_total = if_else(if_any(mspss_1:mspss_12, is.na), NA_real_, rowSums(across(mspss_1:mspss_12)))
  )

cat("✓ Master dataset loaded and scale totals computed\n\n")


#-------------------------------------------------------------------------------
# GLOBAL DATAFRAMES TO STORE RESULTS FOR WORD EXPORT
#-------------------------------------------------------------------------------
# These will hold the results as we calculate them
alpha_results <- tibble(Dataset = character(), PHQ_8 = numeric(), GAD_7 = numeric(), SWEMWBS = numeric())
stats_results <- tibble(Dataset = character(), Category = character(), Comparison = character(), P_Value = character(), Cohens_D = numeric())


#-------------------------------------------------------------------------------
# 2. INTERNAL CONSISTENCY (CRONBACH'S ALPHA)
#-------------------------------------------------------------------------------

cat("2. CALCULATING CRONBACH'S ALPHA BY DATASET\n")
cat(strrep("-", 80), "\n")

# Reusable function for calculating reliability and storing results
calculate_reliability <- function(data_subset, dataset_label) {
  cat("\n---", dataset_label, "---\n")
  
  a_phq <- NA; a_gad <- NA; a_swe <- NA
  
  phq_items <- data_subset %>% select(phq_1:phq_8) %>% drop_na()
  if(nrow(phq_items) > 0) {
    a_phq <- round(psych::alpha(phq_items, check.keys = TRUE, warnings = FALSE)$total$raw_alpha, 3)
    cat("PHQ-8 Cronbach's Alpha:   ", a_phq, "\n")
  }
  
  gad_items <- data_subset %>% select(gad_1:gad_7) %>% drop_na()
  if(nrow(gad_items) > 0) {
    a_gad <- round(psych::alpha(gad_items, check.keys = TRUE, warnings = FALSE)$total$raw_alpha, 3)
    cat("GAD-7 Cronbach's Alpha:   ", a_gad, "\n")
  }
  
  swe_items <- data_subset %>% select(swemwbs_1:swemwbs_7) %>% drop_na()
  if(nrow(swe_items) > 0) {
    a_swe <- round(psych::alpha(swe_items, check.keys = TRUE, warnings = FALSE)$total$raw_alpha, 3)
    cat("SWEMWBS Cronbach's Alpha: ", a_swe, "\n")
  }
  
  # Save to global results
  alpha_results <<- alpha_results %>% add_row(Dataset = dataset_label, PHQ_8 = a_phq, GAD_7 = a_gad, SWEMWBS = a_swe)
}

calculate_reliability(mhdp %>% filter(dataset == "Dataset A"), "Dataset A")
calculate_reliability(mhdp %>% filter(dataset == "Dataset B"), "Dataset B")
calculate_reliability(mhdp %>% filter(dataset == "Dataset C"), "Dataset C")
calculate_reliability(mhdp %>% filter(dataset %in% c("Dataset A", "Dataset B", "Dataset C")), "Full Sample")

cat("\n✓ Reliability statistics computed\n\n")


#-------------------------------------------------------------------------------
# 3. BASELINE CORRELATIONS
#-------------------------------------------------------------------------------

cat("3. CORRELATION BETWEEN SCALES (BASELINE)\n")
cat(strrep("-", 80), "\n")

mhdp_base <- mhdp %>% filter(timepoint == 0)

cor_data <- mhdp_base %>% select(phq_total, gad_total, swemwbs_total, mspss_total) %>% drop_na()
cor_matrix <- round(cor(cor_data), 3)

cat("Pearson Correlation Matrix:\n")
print(cor_matrix)
cat("\n(Note: Positive correlation expected between PHQ & GAD. Negative correlation expected with MSPSS & SWEMWBS)\n\n")

# Format for Word export
cor_df <- as.data.frame(cor_matrix) %>% rownames_to_column(var = "Scale")


#-------------------------------------------------------------------------------
# HELPER FUNCTIONS FOR SIGNIFICANCE & EFFECT SIZES
#-------------------------------------------------------------------------------

report_paired <- function(post, pre, label, dataset_name, category) {
  valid <- complete.cases(post, pre)
  p_clean <- post[valid]; pr_clean <- pre[valid]
  if(length(p_clean) < 2) return(cat(label, ": Not enough data\n"))
  
  t_res <- t.test(p_clean, pr_clean, paired = TRUE)
  d_res <- cohensD(p_clean, pr_clean, method = "paired")
  
  p_val <- format.pval(t_res$p.value, digits=3, eps=0.001)
  d_val <- round(d_res, 3)
  
  cat(sprintf("%-16s p = %-7s | d = %.3f\n", label, p_val, d_val))
  stats_results <<- stats_results %>% add_row(Dataset = dataset_name, Category = category, Comparison = label, P_Value = p_val, Cohens_D = d_val)
}

report_between <- function(formula, data, label, dataset_name, category) {
  t_res <- t.test(formula, data = data, var.equal = TRUE)
  d_res <- cohensD(formula, data = data, method = "pooled")
  
  p_val <- format.pval(t_res$p.value, digits=3, eps=0.001)
  d_val <- round(d_res, 3)
  
  cat(sprintf("%-16s p = %-7s | d = %.3f\n", label, p_val, d_val))
  stats_results <<- stats_results %>% add_row(Dataset = dataset_name, Category = category, Comparison = label, P_Value = p_val, Cohens_D = d_val)
}


#-------------------------------------------------------------------------------
# 4. TESTING: DATASET A (TEMPLETON 2)
#-------------------------------------------------------------------------------

cat("4. TESTING & EFFECT SIZES: DATASET A (TEMPLETON 2)\n")
cat(strrep("-", 80), "\n")

datasetA_phq <- mhdp %>% filter(dataset == "Dataset A", timepoint %in% c(0,2,4,8)) %>% pivot_wider(id_cols=c(participant_id, condition, school_name), names_from = timepoint, values_from = phq_total, names_prefix = "week_")
datasetA_gad <- mhdp %>% filter(dataset == "Dataset A", timepoint %in% c(0,2,4,8)) %>% pivot_wider(id_cols=c(participant_id, condition, school_name), names_from = timepoint, values_from = gad_total, names_prefix = "week_")

cat("--- PHQ-8 Within-Group (Paired) ---\n")
report_paired(datasetA_phq$week_4, datasetA_phq$week_0, "Week 4 vs 0", "Dataset A", "PHQ-8 Within-Group")
report_paired(datasetA_phq$week_8, datasetA_phq$week_4, "Week 8 vs 4", "Dataset A", "PHQ-8 Within-Group")
report_paired(datasetA_phq$week_8, datasetA_phq$week_0, "Week 8 vs 0", "Dataset A", "PHQ-8 Within-Group")

cat("\n--- GAD-7 Within-Group (Paired) ---\n")
report_paired(datasetA_gad$week_4, datasetA_gad$week_0, "Week 4 vs 0", "Dataset A", "GAD-7 Within-Group")
report_paired(datasetA_gad$week_8, datasetA_gad$week_4, "Week 8 vs 4", "Dataset A", "GAD-7 Within-Group")
report_paired(datasetA_gad$week_8, datasetA_gad$week_0, "Week 8 vs 0", "Dataset A", "GAD-7 Within-Group")
cat("\n")


#-------------------------------------------------------------------------------
# 5. TESTING: DATASET B (WELLSPRINGS)
#-------------------------------------------------------------------------------

cat("5. TESTING & EFFECT SIZES: DATASET B (WELLSPRINGS)\n")
cat(strrep("-", 80), "\n")

datasetB_phq <- mhdp %>% filter(dataset == "Dataset B", timepoint %in% c(0,2,4,8)) %>% pivot_wider(id_cols=c(participant_id, condition, school_name), names_from = timepoint, values_from = phq_total, names_prefix = "week_")
datasetB_gad <- mhdp %>% filter(dataset == "Dataset B", timepoint %in% c(0,2,4,8)) %>% pivot_wider(id_cols=c(participant_id, condition, school_name), names_from = timepoint, values_from = gad_total, names_prefix = "week_")

single_phq <- datasetB_phq %>% filter(school_name %in% c("ElbargonNakuru", "RidoreKisumu"))
single_gad <- datasetB_gad %>% filter(school_name %in% c("ElbargonNakuru", "RidoreKisumu"))

cat("--- Single-Condition Schools: PHQ-8 Within-Group ---\n")
report_paired(single_phq$week_4, single_phq$week_0, "Week 4 vs 0", "Dataset B (Single Cond)", "PHQ-8 Within-Group")
report_paired(single_phq$week_8, single_phq$week_4, "Week 8 vs 4", "Dataset B (Single Cond)", "PHQ-8 Within-Group")
report_paired(single_phq$week_8, single_phq$week_0, "Week 8 vs 0", "Dataset B (Single Cond)", "PHQ-8 Within-Group")

cat("\n--- Single-Condition Schools: GAD-7 Within-Group ---\n")
report_paired(single_gad$week_4, single_gad$week_0, "Week 4 vs 0", "Dataset B (Single Cond)", "GAD-7 Within-Group")
report_paired(single_gad$week_8, single_gad$week_4, "Week 8 vs 4", "Dataset B (Single Cond)", "GAD-7 Within-Group")
report_paired(single_gad$week_8, single_gad$week_0, "Week 8 vs 0", "Dataset B (Single Cond)", "GAD-7 Within-Group")

orando_phq <- datasetB_phq %>% filter(school_name == "OrandoKisumu")
orando_gad <- datasetB_gad %>% filter(school_name == "OrandoKisumu")

cat("\n--- OrandoKisumu: PHQ-8 Between-Group (Shamiri vs TAU) ---\n")
report_between(week_0 ~ condition, orando_phq, "Week 0", "Dataset B (Orando)", "PHQ-8 Between-Group")
report_between(week_2 ~ condition, orando_phq, "Week 2", "Dataset B (Orando)", "PHQ-8 Between-Group")
report_between(week_4 ~ condition, orando_phq, "Week 4", "Dataset B (Orando)", "PHQ-8 Between-Group")
report_between(week_8 ~ condition, orando_phq, "Week 8", "Dataset B (Orando)", "PHQ-8 Between-Group")

cat("\n--- OrandoKisumu: GAD-7 Between-Group (Shamiri vs TAU) ---\n")
report_between(week_0 ~ condition, orando_gad, "Week 0", "Dataset B (Orando)", "GAD-7 Between-Group")
report_between(week_2 ~ condition, orando_gad, "Week 2", "Dataset B (Orando)", "GAD-7 Between-Group")
report_between(week_4 ~ condition, orando_gad, "Week 4", "Dataset B (Orando)", "GAD-7 Between-Group")
report_between(week_8 ~ condition, orando_gad, "Week 8", "Dataset B (Orando)", "GAD-7 Between-Group")

cat("\n--- OrandoKisumu: PHQ-8 Within-Group (Shamiri vs TAU) ---\n")
report_paired(orando_phq$week_4[orando_phq$condition=="Shamiri"], orando_phq$week_0[orando_phq$condition=="Shamiri"], "[Shamiri] W4 v W0", "Dataset B (Orando)", "PHQ-8 Within-Group")
report_paired(orando_phq$week_4[orando_phq$condition=="TAU"],     orando_phq$week_0[orando_phq$condition=="TAU"],     "[TAU] W4 v W0", "Dataset B (Orando)", "PHQ-8 Within-Group")

cat("\n--- OrandoKisumu: GAD-7 Within-Group (Shamiri vs TAU) ---\n")
report_paired(orando_gad$week_4[orando_gad$condition=="Shamiri"], orando_gad$week_0[orando_gad$condition=="Shamiri"], "[Shamiri] W4 v W0", "Dataset B (Orando)", "GAD-7 Within-Group")
report_paired(orando_gad$week_4[orando_gad$condition=="TAU"],     orando_gad$week_0[orando_gad$condition=="TAU"],     "[TAU] W4 v W0", "Dataset B (Orando)", "GAD-7 Within-Group")
cat("\n")


#-------------------------------------------------------------------------------
# 6. TESTING: DATASET C (ANANSI TRIAL 1)
#-------------------------------------------------------------------------------

cat("6. TESTING & EFFECT SIZES: DATASET C (ANANSI TRIAL 1)\n")
cat(strrep("-", 80), "\n")

datasetC_phq <- mhdp %>% filter(dataset == "Dataset C", timepoint %in% c(0,2,4,8,40,52)) %>% pivot_wider(id_cols=c(participant_id, condition, school_name), names_from = timepoint, values_from = phq_total, names_prefix = "week_")
datasetC_gad <- mhdp %>% filter(dataset == "Dataset C", timepoint %in% c(0,2,4,8,40,52)) %>% pivot_wider(id_cols=c(participant_id, condition, school_name), names_from = timepoint, values_from = gad_total, names_prefix = "week_")

cat("--- Dataset C: PHQ-8 Between-Group (Shamiri vs TAU) ---\n")
report_between(week_0 ~ condition, datasetC_phq, "Week 0", "Dataset C", "PHQ-8 Between-Group")
report_between(week_4 ~ condition, datasetC_phq, "Week 4", "Dataset C", "PHQ-8 Between-Group")
report_between(week_52 ~ condition, datasetC_phq, "Week 52", "Dataset C", "PHQ-8 Between-Group")

cat("\n--- Dataset C: GAD-7 Between-Group (Shamiri vs TAU) ---\n")
report_between(week_0 ~ condition, datasetC_gad, "Week 0", "Dataset C", "GAD-7 Between-Group")
report_between(week_4 ~ condition, datasetC_gad, "Week 4", "Dataset C", "GAD-7 Between-Group")
report_between(week_52 ~ condition, datasetC_gad, "Week 52", "Dataset C", "GAD-7 Between-Group")

cat("\n--- Dataset C: PHQ-8 Within-Group (Shamiri vs TAU) ---\n")
report_paired(datasetC_phq$week_4[datasetC_phq$condition=="Shamiri"],  datasetC_phq$week_0[datasetC_phq$condition=="Shamiri"],  "[Shamiri] W4 v W0", "Dataset C", "PHQ-8 Within-Group")
report_paired(datasetC_phq$week_4[datasetC_phq$condition=="TAU"],      datasetC_phq$week_0[datasetC_phq$condition=="TAU"],      "[TAU] W4 v W0", "Dataset C", "PHQ-8 Within-Group")

cat("\n--- Dataset C: GAD-7 Within-Group (Shamiri vs TAU) ---\n")
report_paired(datasetC_gad$week_4[datasetC_gad$condition=="Shamiri"],  datasetC_gad$week_0[datasetC_gad$condition=="Shamiri"],  "[Shamiri] W4 v W0", "Dataset C", "GAD-7 Within-Group")
report_paired(datasetC_gad$week_4[datasetC_gad$condition=="TAU"],      datasetC_gad$week_0[datasetC_gad$condition=="TAU"],      "[TAU] W4 v W0", "Dataset C", "GAD-7 Within-Group")


#-------------------------------------------------------------------------------
# 7. EXPORT TO WORD DOCUMENT
#-------------------------------------------------------------------------------

cat("\n7. EXPORTING RESULTS TO WORD DOCUMENT\n")
cat(strrep("-", 80), "\n")

# Create the Word Document Object
doc <- read_docx() %>%
  body_add_par("Psychometrics and Statistics", style = "heading 1") %>%
  
  # 1. Correlations
  body_add_par("1. Baseline Correlations Matrix", style = "heading 2") %>%
  body_add_flextable(flextable(cor_df) %>% autofit()) %>%
  body_add_par("") %>%
  
  # 2. Reliability
  body_add_par("2. Internal Consistency (Cronbach's Alpha)", style = "heading 2") %>%
  body_add_flextable(flextable(alpha_results) %>% autofit()) %>%
  body_add_par("") %>%
  
  # 3. T-Tests and Effect Sizes
  body_add_par("3. Significance Testing & Effect Sizes (Cohen's d)", style = "heading 2") %>%
  body_add_flextable(
    flextable(stats_results) %>% 
      merge_v(j = c("Dataset", "Category")) %>%  # Merges duplicate rows for a clean look
      valign(j = c("Dataset", "Category"), valign = "top") %>%
      autofit()
  )

# Save the document
print(doc, target = "../results/tables/Psychometrics_and_EffectSizes.docx")

cat("✓ Word document successfully saved to '../results/Psychometrics_and_EffectSizes.docx'\n")
cat(strrep("=", 80), "\n")
