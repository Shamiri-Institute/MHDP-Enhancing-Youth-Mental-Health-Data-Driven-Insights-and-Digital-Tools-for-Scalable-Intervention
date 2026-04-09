################################################################################
# PROJECT: MENTAL HEALTH DATA PRIZE
# PURPOSE: Part 2 - Psychometrics, Significance Testing, and Effect Sizes
# Strategy Documentation: Calculates Cronbach's alpha and baseline correlations. 
#                         significance testing (t-tests/Cohen's d) to 
#                         compare Shamiri Hubs vs. specific NGO Partners to 
#                         evaluate real-world scalability. Exports to Word.
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

# Create Objective 1 Output Directories
dir.create("../results/objective_1/tables", recursive = TRUE, showWarnings = FALSE)

cat(strrep("=", 80), "\n")
cat("1. IMPORTING AND PREPARING DATASET\n")
cat(strrep("-", 80), "\n")

mhdp <- read_excel("../datasets/processed/mhdp_data.xlsx", col_types = "text")

# Convert psychometric columns to numeric
mhdp <- mhdp %>%
  mutate(across(c(
    timepoint, starts_with("phq_"), starts_with("gad_"), starts_with("swemwbs_"),
    starts_with("mspss_")), as.numeric))

# Compute Totals
mhdp <- mhdp %>%
  mutate(
    phq_total = if_else(if_any(phq_1:phq_8, is.na), NA_real_, rowSums(across(phq_1:phq_8))),
    gad_total = if_else(if_any(gad_1:gad_7, is.na), NA_real_, rowSums(across(gad_1:gad_7))),
    swemwbs_total = if_else(if_any(swemwbs_1:swemwbs_7, is.na), NA_real_, rowSums(across(swemwbs_1:swemwbs_7))),
    mspss_total = if_else(if_any(mspss_1:mspss_12, is.na), NA_real_, rowSums(across(mspss_1:mspss_12)))
  )

# Define Implementation Contexts
mhdp <- mhdp %>%
  mutate(
    condition_clean = ifelse(condition %in% c("TAU", "Waitlist"), "Control", condition),
    delivery_group = case_when(
      condition_clean == "Control" ~ "Control",
      implementer %in% c("AMHRTF", "PDO") ~ "Partner",
      TRUE ~ "Shamiri"
    ),
    implementer_group = case_when(
      condition_clean == "Control" ~ "Control",
      implementer == "SHAMIRI" ~ "Shamiri",
      TRUE ~ implementer
    )
  )

cat("✓ Master dataset loaded, totals computed, and implementers mapped\n\n")


#-------------------------------------------------------------------------------
# GLOBAL DATAFRAMES TO STORE RESULTS FOR WORD EXPORT
#-------------------------------------------------------------------------------
alpha_results <- tibble(Dataset = character(), PHQ_8 = numeric(), GAD_7 = numeric(), SWEMWBS = numeric())
stats_results <- tibble(Analysis_Focus = character(), Measure = character(), Comparison = character(), P_Value = character(), Cohens_D = numeric())


#-------------------------------------------------------------------------------
# 2. INTERNAL CONSISTENCY (CRONBACH'S ALPHA)
#-------------------------------------------------------------------------------

cat("2. CALCULATING CRONBACH'S ALPHA\n")
cat(strrep("-", 80), "\n")

calculate_reliability <- function(data_subset, dataset_label) {
  a_phq <- NA; a_gad <- NA; a_swe <- NA
  
  phq_items <- data_subset %>% select(phq_1:phq_8) %>% drop_na()
  if(nrow(phq_items) > 0) a_phq <- round(psych::alpha(phq_items, check.keys = TRUE, warnings = FALSE)$total$raw_alpha, 3)
  
  gad_items <- data_subset %>% select(gad_1:gad_7) %>% drop_na()
  if(nrow(gad_items) > 0) a_gad <- round(psych::alpha(gad_items, check.keys = TRUE, warnings = FALSE)$total$raw_alpha, 3)
  
  swe_items <- data_subset %>% select(swemwbs_1:swemwbs_7) %>% drop_na()
  if(nrow(swe_items) > 0) a_swe <- round(psych::alpha(swe_items, check.keys = TRUE, warnings = FALSE)$total$raw_alpha, 3)
  
  alpha_results <<- alpha_results %>% add_row(Dataset = dataset_label, PHQ_8 = a_phq, GAD_7 = a_gad, SWEMWBS = a_swe)
}

calculate_reliability(mhdp %>% filter(dataset == "Dataset A", timepoint == 4), "Dataset A")
calculate_reliability(mhdp %>% filter(dataset == "Dataset B", timepoint == 4), "Dataset B")
calculate_reliability(mhdp %>% filter(dataset == "Dataset C", timepoint == 4), "Dataset C")
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

report_paired <- function(post, pre, label, focus, measure) {
  valid <- complete.cases(post, pre)
  p_clean <- post[valid]; pr_clean <- pre[valid]
  if(length(p_clean) < 2) return(NULL)
  
  t_res <- t.test(p_clean, pr_clean, paired = TRUE)
  d_res <- cohensD(p_clean, pr_clean, method = "paired")
  p_val <- format.pval(t_res$p.value, digits=3, eps=0.001)
  
  cat(sprintf("%-20s | %-15s | p = %-7s | d = %.3f\n", focus, label, p_val, d_res))
  stats_results <<- stats_results %>% add_row(Analysis_Focus = focus, Measure = measure, Comparison = label, P_Value = p_val, Cohens_D = round(d_res, 3))
}

report_between <- function(formula, data, label, focus, measure) {
  t_res <- t.test(formula, data = data, var.equal = TRUE)
  d_res <- cohensD(formula, data = data, method = "pooled")
  p_val <- format.pval(t_res$p.value, digits=3, eps=0.001)
  
  cat(sprintf("%-20s | %-15s | p = %-7s | d = %.3f\n", focus, label, p_val, d_res))
  stats_results <<- stats_results %>% add_row(Analysis_Focus = focus, Measure = measure, Comparison = label, P_Value = p_val, Cohens_D = round(d_res, 3))
}


#-------------------------------------------------------------------------------
# 4. PREPARE WIDE DATA FOR T-TESTS
#-------------------------------------------------------------------------------

# Pivot data wide to allow easy Week 4 vs Week 0 paired testing

mhdp_wide <- mhdp %>%
  filter(timepoint %in% c(0, 4)) %>%
  pivot_wider(
    id_cols = c(participant_id, condition, delivery_group, implementer_group), 
    names_from = timepoint, 
    values_from = c(phq_total, gad_total), 
    names_prefix = "W"
  )


#-------------------------------------------------------------------------------
# 5. TESTING: WITHIN-GROUP (PRE-POST EFFECTIVENESS)
#-------------------------------------------------------------------------------

cat("4. WITHIN-GROUP EFFECT SIZES (Week 4 vs Baseline)\n")
cat(strrep("-", 80), "\n")
cat("Testing if each specific implementer successfully reduced symptoms.\n\n")

# SHAMIRI HUBS
hub <- mhdp_wide %>% filter(implementer_group == "Shamiri")
report_paired(hub$phq_total_W4, hub$phq_total_W0, "W4 vs W0", "1. Shamiri Hubs", "PHQ-8")
report_paired(hub$gad_total_W4, hub$gad_total_W0, "W4 vs W0", "1. Shamiri Hubs", "GAD-7")

# AMHRTF (Partner)
amhrtf <- mhdp_wide %>% filter(implementer_group == "AMHRTF")
report_paired(amhrtf$phq_total_W4, amhrtf$phq_total_W0, "W4 vs W0", "2. AMHRTF (Partner)", "PHQ-8")
report_paired(amhrtf$gad_total_W4, amhrtf$gad_total_W0, "W4 vs W0", "2. AMHRTF (Partner)", "GAD-7")

# PDO (Partner)
pdo <- mhdp_wide %>% filter(implementer_group == "PDO")
report_paired(pdo$phq_total_W4, pdo$phq_total_W0, "W4 vs W0", "3. PDO (Partner)", "PHQ-8")
report_paired(pdo$gad_total_W4, pdo$gad_total_W0, "W4 vs W0", "3. PDO (Partner)", "GAD-7")

# CONTROL 1: TAU (Treatment As Usual)
ctrl_tau <- mhdp_wide %>% filter(condition == "TAU")
report_paired(ctrl_tau$phq_total_W4, ctrl_tau$phq_total_W0, "W4 vs W0", "4a. Control (TAU)", "PHQ-8")
report_paired(ctrl_tau$gad_total_W4, ctrl_tau$gad_total_W0, "W4 vs W0", "4a. Control (TAU)", "GAD-7")

# CONTROL 2: Waitlist
ctrl_wl <- mhdp_wide %>% filter(condition == "Waitlist")
report_paired(ctrl_wl$phq_total_W4, ctrl_wl$phq_total_W0, "W4 vs W0", "4b. Control (Waitlist)", "PHQ-8")
report_paired(ctrl_wl$gad_total_W4, ctrl_wl$gad_total_W0, "W4 vs W0", "4b. Control (Waitlist)", "GAD-7")
cat("\n")


#-------------------------------------------------------------------------------
# 6. TESTING: BETWEEN-GROUP (SCALABILITY COMPARISONS)
#-------------------------------------------------------------------------------

cat("5. BETWEEN-GROUP EFFECT SIZES (Week 4)\n")
cat(strrep("-", 80), "\n")
cat("Testing Shamiri vs Control (Efficacy), and Shamiri vs Partners (Scalability).\n\n")

# 1. Efficacy (Shamiri Hubs vs Control)
eff_df <- mhdp_wide %>% filter(implementer_group %in% c("Shamiri", "Control"))
report_between(phq_total_W4 ~ implementer_group, eff_df, "Hub vs Control", "Efficacy Check", "PHQ-8")
report_between(gad_total_W4 ~ implementer_group, eff_df, "Hub vs Control", "Efficacy Check", "GAD-7")

# 2. Broad Scalability (Shamiri Hubs vs All Partners Combined)
scale_df <- mhdp_wide %>% filter(delivery_group %in% c("Shamiri", "Partner"))
report_between(phq_total_W4 ~ delivery_group, scale_df, "Hub vs Partners", "Broad Scalability", "PHQ-8")
report_between(gad_total_W4 ~ delivery_group, scale_df, "Hub vs Partners", "Broad Scalability", "GAD-7")

# 3. Specific Scalability (Shamiri vs AMHRTF)
amhrtf_df <- mhdp_wide %>% filter(implementer_group %in% c("Shamiri", "AMHRTF"))
report_between(phq_total_W4 ~ implementer_group, amhrtf_df, "Hub vs AMHRTF", "Specific Partner", "PHQ-8")
report_between(gad_total_W4 ~ implementer_group, amhrtf_df, "Hub vs AMHRTF", "Specific Partner", "GAD-7")

# 4. Specific Scalability (Shamiri vs PDO)
pdo_df <- mhdp_wide %>% filter(implementer_group %in% c("Shamiri", "PDO"))
report_between(phq_total_W4 ~ implementer_group, pdo_df, "Hub vs PDO", "Specific Partner", "PHQ-8")
report_between(gad_total_W4 ~ implementer_group, pdo_df, "Hub vs PDO", "Specific Partner", "GAD-7")


#-------------------------------------------------------------------------------
# 7. EXPORT TO WORD DOCUMENT
#-------------------------------------------------------------------------------

cat("\n6. EXPORTING RESULTS TO WORD DOCUMENT\n")
cat(strrep("-", 80), "\n")

doc <- read_docx() %>%
  body_add_par("MHDP - Psychometrics and Implementation Effect Sizes", style = "heading 1") %>%
  
  # 1. Correlations
  body_add_par("1. Baseline Correlations Matrix", style = "heading 2") %>%
  body_add_flextable(flextable(cor_df) %>% autofit()) %>%
  body_add_par("") %>%
  
  # 2. Reliability
  body_add_par("2. Internal Consistency (Cronbach's Alpha)", style = "heading 2") %>%
  body_add_flextable(flextable(alpha_results) %>% autofit()) %>%
  body_add_par("") %>%
  
  # 3. T-Tests and Effect Sizes
  body_add_par("3. Intervention Scalability & Effect Sizes (Cohen's d)", style = "heading 2") %>%
  body_add_flextable(
    flextable(stats_results) %>% 
      merge_v(j = c("Analysis_Focus", "Measure")) %>%  
      valign(j = c("Analysis_Focus", "Measure"), valign = "top") %>%
      autofit()
  )

print(doc, target = "../results/objective_1/tables/Psychometrics_and_EffectSizes.docx")

cat("✓ Word document successfully saved to '../results/objective_1/tables/Psychometrics_and_EffectSizes.docx'\n")
cat(strrep("=", 80), "\n")
cat("PSYCHOMETRICS & SIGNIFICANCE SCRIPT COMPLETE\n")
cat(strrep("=", 80), "\n\n")

################################################################################
# END OF SCRIPT
################################################################################