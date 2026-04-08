################################################################################
# PROJECT: MENTAL HEALTH DATA PRIZE
# PURPOSE: Part 3 - Advanced Mixed-Effects Modeling (LMM)
# Strategy Documentation: Builds Conditional Growth Curve Models. Includes the 
#                         "Big Enough" analysis to test how school resources 
#                         and urban/rural context influence intervention efficacy.
#                         Exports EACH model to a separate Word document.
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
  lme4,         # Mixed-effects models
  lmerTest,     # P-values for mixed-effects models
  broom.mixed,  # Tidying model outputs for Word
  officer,      # Exporting to Word
  flextable,    # Formatting tables for Word
  conflicted    # Namespace safety
)

conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("lmer", "lmerTest")

dir.create("../results/objective_1/models", recursive = TRUE, showWarnings = FALSE)

cat(strrep("=", 80), "\n")
cat("1. IMPORTING AND PREPARING DATASET\n")
cat(strrep("-", 80), "\n")

mhdp <- read_excel("../datasets/processed/mhdp_data.xlsx", col_types = "text")

# Convert psychometric and demographic columns to numeric safely
mhdp <- suppressWarnings(
  mhdp %>%
    mutate(across(c(
      timepoint, age, gender, starts_with("phq_"), starts_with("gad_"), starts_with("swemwbs_")
    ), as.numeric))
)

# Compute Totals & Define Delivery Groups
mhdp <- mhdp %>%
  mutate(
    phq_total = if_else(if_any(phq_1:phq_8, is.na), NA_real_, rowSums(across(phq_1:phq_8))),
    gad_total = if_else(if_any(gad_1:gad_7, is.na), NA_real_, rowSums(across(gad_1:gad_7))),
    swemwbs_total = if_else(if_any(swemwbs_1:swemwbs_7, is.na), NA_real_, rowSums(across(swemwbs_1:swemwbs_7))),
    
    condition_clean = ifelse(condition %in% c("TAU", "Waitlist"), "Control", condition),
    delivery_group = case_when(
      condition_clean == "Control" ~ "Control",
      implementer %in% c("AMHRTF", "PDO") ~ "Partner",
      TRUE ~ "Shamiri"
    )
  )


#-------------------------------------------------------------------------------
# 2. PREPARING DATA FOR CONDITIONAL GROWTH CURVE MODELS
#-------------------------------------------------------------------------------

cat("2. PREPARING LONGITUDINAL COVARIATES\n")
cat(strrep("-", 80), "\n")

mhdp_model_data <- mhdp %>%
  mutate(timepoint_num = as.numeric(timepoint)) %>%
  group_by(participant_id) %>%
  arrange(timepoint_num, .by_group = TRUE) %>%
  mutate(
    phq_baseline = phq_total[timepoint_num == 0][1],
    gad_baseline = gad_total[timepoint_num == 0][1],
    swemwbs_baseline = swemwbs_total[timepoint_num == 0][1]
  ) %>%
  ungroup() %>%
  mutate(
    school_demographic = replace_na(school_demographic, "Unknown"),
    school_classification = replace_na(school_classification, "Unknown"),
    school_type = replace_na(school_type, "Unknown")
  ) %>%
  mutate(across(c(school_demographic, school_classification, school_type), as.factor)) %>%
  drop_na(age, gender, condition_clean, group_leader, school_name, timepoint_num, phq_baseline)

mhdp_model_data$condition_clean <- factor(mhdp_model_data$condition_clean, levels = c("Control", "Shamiri"))

cat("✓ Baseline scores extracted and school context variables formatted.\n\n")


#-------------------------------------------------------------------------------
# 3. DYNAMIC FORMULA BUILDER
#-------------------------------------------------------------------------------

covariates_base <- c("age", "gender")
if(length(levels(mhdp_model_data$school_demographic)) > 1) covariates_base <- c(covariates_base, "school_demographic")
if(length(levels(mhdp_model_data$school_classification)) > 1) covariates_base <- c(covariates_base, "school_classification")
if(length(levels(mhdp_model_data$school_type)) > 1) covariates_base <- c(covariates_base, "school_type")

cov_string <- paste(covariates_base, collapse = " + ")
formula_str_phq <- paste("phq_total ~ timepoint_num * condition_clean + phq_baseline +", cov_string, "+ (1 | school_name / group_leader / participant_id)")
formula_str_gad <- paste("gad_total ~ timepoint_num * condition_clean + gad_baseline +", cov_string, "+ (1 | school_name / group_leader / participant_id)")
formula_str_swe <- paste("swemwbs_total ~ timepoint_num * condition_clean + swemwbs_baseline +", cov_string, "+ (1 | school_name / group_leader / participant_id)")


#-------------------------------------------------------------------------------
# 4. RUN LMMs: PHASE 1 SHORT-TERM (FULL SAMPLE)
#-------------------------------------------------------------------------------

cat("4. RUNNING PHASE 1 SHORT-TERM MODELS (FULL SAMPLE)\n")
cat(strrep("-", 80), "\n")

mhdp_short <- mhdp_model_data %>% filter(timepoint_num <= 8)

lmm_phq_short <- lmer(as.formula(formula_str_phq), data = mhdp_short)
lmm_gad_short <- lmer(as.formula(formula_str_gad), data = mhdp_short)
lmm_swe_short <- lmer(as.formula(formula_str_swe), data = mhdp_short)


#-------------------------------------------------------------------------------
# 5. RUN LMMs: PHASE 1 SHORT-TERM (CLINICAL SUBSAMPLE)
#-------------------------------------------------------------------------------

cat("5. RUNNING PHASE 1 SHORT-TERM MODELS (CLINICAL SUBSAMPLE)\n")
cat(strrep("-", 80), "\n")

clinical_ids_model <- mhdp_model_data %>% filter(timepoint_num == 0 & (phq_baseline >= 10 | gad_baseline >= 10)) %>% pull(participant_id)
mhdp_short_clinical <- mhdp_short %>% filter(participant_id %in% clinical_ids_model)

# Re-check levels for subset
covariates_clin <- c("age", "gender")
if(length(unique(mhdp_short_clinical$school_demographic)) > 1) covariates_clin <- c(covariates_clin, "school_demographic")
if(length(unique(mhdp_short_clinical$school_classification)) > 1) covariates_clin <- c(covariates_clin, "school_classification")
if(length(unique(mhdp_short_clinical$school_type)) > 1) covariates_clin <- c(covariates_clin, "school_type")

cov_string_clin <- paste(covariates_clin, collapse = " + ")
f_clin_phq <- paste("phq_total ~ timepoint_num * condition_clean + phq_baseline +", cov_string_clin, "+ (1 | school_name / group_leader / participant_id)")
f_clin_gad <- paste("gad_total ~ timepoint_num * condition_clean + gad_baseline +", cov_string_clin, "+ (1 | school_name / group_leader / participant_id)")
f_clin_swe <- paste("swemwbs_total ~ timepoint_num * condition_clean + swemwbs_baseline +", cov_string_clin, "+ (1 | school_name / group_leader / participant_id)")

lmm_phq_clin <- lmer(as.formula(f_clin_phq), data = mhdp_short_clinical)
lmm_gad_clin <- lmer(as.formula(f_clin_gad), data = mhdp_short_clinical)
lmm_swe_clin <- lmer(as.formula(f_clin_swe), data = mhdp_short_clinical)


#-------------------------------------------------------------------------------
# 6. RUN LMMs: PHASE 2 LONG-TERM (DATASET C ONLY)
#-------------------------------------------------------------------------------

cat("6. RUNNING PHASE 2 LONG-TERM MODELS (WEEKS 8-52)\n")
cat(strrep("-", 80), "\n")

mhdp_long <- mhdp %>%
  mutate(timepoint_num = as.numeric(timepoint)) %>%
  filter(dataset == "Dataset C" & timepoint_num >= 8) %>%
  group_by(participant_id) %>%
  mutate(
    phq_baseline = phq_total[timepoint_num == 8][1], 
    gad_baseline = gad_total[timepoint_num == 8][1],
    swemwbs_baseline = swemwbs_total[timepoint_num == 8][1]
  ) %>%
  ungroup() %>%
  mutate(
    condition_clean = ifelse(condition %in% c("TAU", "Waitlist"), "Control", condition),
    condition_clean = factor(condition_clean, levels = c("Control", "Shamiri")),
    time_post_8 = timepoint_num - 8
  ) %>%
  drop_na(age, gender, condition_clean, school_name, timepoint_num)

f_long_phq <- "phq_total ~ time_post_8 * condition_clean + phq_baseline + age + gender + (1 | school_name / participant_id)"
f_long_gad <- "gad_total ~ time_post_8 * condition_clean + gad_baseline + age + gender + (1 | school_name / participant_id)"
f_long_swe <- "swemwbs_total ~ time_post_8 * condition_clean + swemwbs_baseline + age + gender + (1 | school_name / participant_id)"

lmm_phq_long <- lmer(as.formula(f_long_phq), data = mhdp_long)
lmm_gad_long <- lmer(as.formula(f_long_gad), data = mhdp_long)
lmm_swe_long <- lmer(as.formula(f_long_swe), data = mhdp_long)


#-------------------------------------------------------------------------------
# 7. RUN LMMs: "BIG ENOUGH" (CONTEXTUAL MODERATION)
#-------------------------------------------------------------------------------

cat("7. RUNNING 'BIG ENOUGH' CONTEXTUAL MODEL\n")
cat(strrep("-", 80), "\n")

mhdp_intervention_short <- mhdp_short %>% filter(condition_clean == "Shamiri")

int_terms <- c()
if(length(unique(mhdp_intervention_short$school_demographic)) > 1) int_terms <- c(int_terms, "timepoint_num * school_demographic")
if(length(unique(mhdp_intervention_short$school_classification)) > 1) int_terms <- c(int_terms, "timepoint_num * school_classification")

if(length(int_terms) == 0) {
  f_context <- "phq_total ~ timepoint_num + phq_baseline + age + gender + (1 | school_name / group_leader / participant_id)"
} else {
  f_context <- paste("phq_total ~", paste(int_terms, collapse = " + "), "+ phq_baseline + age + gender + (1 | school_name / group_leader / participant_id)")
}

lmm_context_phq <- lmer(as.formula(f_context), data = mhdp_intervention_short)


#-------------------------------------------------------------------------------
# 8. EXPORT EACH MODEL TO A SEPARATE WORD DOCUMENT
#-------------------------------------------------------------------------------

cat("8. EXPORTING EACH MODEL TO A SEPARATE WORD DOCUMENT\n")
cat(strrep("-", 80), "\n")

# Helper function to tidy and export a single model to its own Word document
export_model_to_word <- function(model_obj, doc_title, file_name) {
  # Clean up the model output
  tidy_df <- tidy(model_obj, effects = "fixed") %>% 
    mutate(
      across(where(is.numeric), ~round(., 3)), 
      p.value = format.pval(p.value, digits=3, eps=0.001)
    )
  
  # Format the formula as a single continuous string (Prevents the as_xml_document crash)
  form_string <- paste(format(formula(model_obj)), collapse = " ")
  
  # Create a new Word document
  doc <- read_docx() %>%
    body_add_par(doc_title, style = "heading 1") %>%
    body_add_par(paste("Model Formula:", form_string), style = "Normal") %>%
    body_add_par("") %>%
    body_add_flextable(flextable(tidy_df) %>% autofit())
  
  # Save it
  print(doc, target = paste0("../results/objective_1/models/", file_name))
  cat("Saved:", file_name, "\n")
}

# --- Export Phase 1 (Full Sample) ---
export_model_to_word(lmm_phq_short, "Phase 1 (Full Sample): Depression (PHQ-8)", "LMM_Phase1_FullSample_PHQ.docx")
export_model_to_word(lmm_gad_short, "Phase 1 (Full Sample): Anxiety (GAD-7)", "LMM_Phase1_FullSample_GAD.docx")
export_model_to_word(lmm_swe_short, "Phase 1 (Full Sample): Wellbeing (SWEMWBS)", "LMM_Phase1_FullSample_SWE.docx")

# --- Export Phase 1 (Clinical Subsample) ---
export_model_to_word(lmm_phq_clin, "Phase 1 (Clinical Subsample): Depression (PHQ-8)", "LMM_Phase1_Clinical_PHQ.docx")
export_model_to_word(lmm_gad_clin, "Phase 1 (Clinical Subsample): Anxiety (GAD-7)", "LMM_Phase1_Clinical_GAD.docx")
export_model_to_word(lmm_swe_clin, "Phase 1 (Clinical Subsample): Wellbeing (SWEMWBS)", "LMM_Phase1_Clinical_SWE.docx")

# --- Export Phase 2 (Long-Term) ---
export_model_to_word(lmm_phq_long, "Phase 2 (Long-Term): Depression (PHQ-8)", "LMM_Phase2_LongTerm_PHQ.docx")
export_model_to_word(lmm_gad_long, "Phase 2 (Long-Term): Anxiety (GAD-7)", "LMM_Phase2_LongTerm_GAD.docx")
export_model_to_word(lmm_swe_long, "Phase 2 (Long-Term): Wellbeing (SWEMWBS)", "LMM_Phase2_LongTerm_SWE.docx")

# --- Export Contextual Model ---
export_model_to_word(lmm_context_phq, "Contextual Moderation (Urban/Rural & Resources)", "LMM_Contextual_Moderation_PHQ.docx")

cat("\n")
cat(strrep("=", 80), "\n")
cat("ADVANCED MODELING SCRIPT COMPLETE\n")
cat(strrep("=", 80), "\n\n")

################################################################################
# END OF SCRIPT
################################################################################