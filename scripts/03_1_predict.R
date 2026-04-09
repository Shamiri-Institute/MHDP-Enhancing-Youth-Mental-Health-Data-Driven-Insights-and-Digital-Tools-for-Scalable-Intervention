################################################################################
# PROJECT: MENTAL HEALTH DATA PRIZE
# PURPOSE: Part 3 - Meta-Analyses, Subgroup Analyses, and Advanced Modeling
# Strategy Documentation: Evaluates overall effectiveness using Random-Effects 
#                         Meta-Analysis. Investigates Age & Gender subgroups. 
#                         Explores LMMs. Exports all results to Word (.docx).
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
  meta,         # Meta-analysis functions
  lme4,         # Mixed-effects models
  lmerTest,     # P-values for mixed-effects models
  ggeffects,    # Marginal effects for plotting LMMs
  broom.mixed,  # Tidying model outputs for Word
  officer,      # Exporting to Word
  flextable,    # Formatting tables for Word
  conflicted    # Namespace safety
)

conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("lmer", "lmerTest")

dir.create("../plots", showWarnings = FALSE)
dir.create("../results", showWarnings = FALSE)

cat(strrep("=", 80), "\n")
cat("1. IMPORTING AND PREPARING DATASET\n")
cat(strrep("-", 80), "\n")

mhdp <- read_excel("../datasets/processed/mhdp_data.xlsx", col_types = "text")

# Safely convert columns to numeric
mhdp <- mhdp %>%
  mutate(across(c(
    timepoint, age, gender, starts_with("phq_"), starts_with("gad_")
  ), as.numeric))

# Compute Totals & Create Subgroup Labels
mhdp <- mhdp %>%
  mutate(
    phq_total = if_else(if_any(phq_1:phq_8, is.na), NA_real_, rowSums(across(phq_1:phq_8))),
    gad_total = if_else(if_any(gad_1:gad_7, is.na), NA_real_, rowSums(across(gad_1:gad_7))),
    age_group = case_when(
      age <= 14 ~ "≤14",
      age <= 16 ~ "15–16",
      TRUE ~ "≥17"
    ),
    gender_label = case_when(
      gender == 1 ~ "Female",
      gender == 2 ~ "Male",
      TRUE ~ NA_character_
    )
  )

cat("✓ Master dataset loaded, totals computed, and subgroups defined\n\n")


#-------------------------------------------------------------------------------
# HELPER FUNCTIONS TO EXTRACT META-ANALYSIS DATA FOR WORD
#-------------------------------------------------------------------------------
extract_meta_overall <- function(m, label) {
  tibble(
    Comparison = label,
    Studies_k = m$k,
    Hedges_g = round(m$TE.random, 3),
    CI_95 = paste0("[", round(m$lower.random, 3), ", ", round(m$upper.random, 3), "]"),
    P_Value = format.pval(m$pval.random, digits = 3, eps = 0.001),
    I2_Heterogeneity = paste0(round(m$I2 * 100, 1), "%")
  )
}

extract_meta_subgroup <- function(m, label) {
  tibble(
    Analysis = label,
    Subgroup = as.character(m$bylevs),
    Studies_k = as.numeric(m$k.w),
    Hedges_g = round(m$TE.random.w, 3),
    CI_95 = paste0("[", round(m$lower.random.w, 3), ", ", round(m$upper.random.w, 3), "]"),
    P_Value = format.pval(m$pval.random.w, digits = 3, eps = 0.001)
  )
}

# Empty dataframes to store results
meta_overall_results <- tibble()
meta_subgroup_results <- tibble()


#-------------------------------------------------------------------------------
# 2. OVERALL META-ANALYSIS (BETWEEN-GROUP: SHAMIRI VS TAU)
#-------------------------------------------------------------------------------

cat("2. OVERALL META-ANALYSES (Week 4)\n")
cat(strrep("-", 80), "\n")

datasetB_orando <- mhdp %>% filter(dataset == "Dataset B", school_name == "OrandoKisumu")
datasetC_all <- mhdp %>% filter(dataset == "Dataset C")

get_meta_stats <- function(df, outcome_var, cond, tp = 4) {
  df %>% filter(condition == cond, timepoint == tp) %>%
    summarise(n = sum(!is.na(.data[[outcome_var]])), m = mean(.data[[outcome_var]], na.rm = TRUE), sd = sd(.data[[outcome_var]], na.rm = TRUE))
}

# PHQ-8 
b_tau_phq <- get_meta_stats(datasetB_orando, "phq_total", "TAU"); b_sha_phq <- get_meta_stats(datasetB_orando, "phq_total", "Shamiri")
c_tau_phq <- get_meta_stats(datasetC_all, "phq_total", "TAU");    c_sha_phq <- get_meta_stats(datasetC_all, "phq_total", "Shamiri")

meta_input_phq <- data.frame(
  study = c("Dataset B (Orando)", "Dataset C"),
  n.e = c(b_sha_phq$n, c_sha_phq$n), mean.e = c(b_sha_phq$m, c_sha_phq$m), sd.e = c(b_sha_phq$sd, c_sha_phq$sd),
  n.c = c(b_tau_phq$n, c_tau_phq$n), mean.c = c(b_tau_phq$m, c_tau_phq$m), sd.c = c(b_tau_phq$sd, c_tau_phq$sd)
)

meta_d_phq <- metacont(n.e=n.e, mean.e=mean.e, sd.e=sd.e, n.c=n.c, mean.c=mean.c, sd.c=sd.c, studlab=study, data=meta_input_phq, sm="SMD", method.smd="Hedges", random=TRUE, method.tau="REML", method.random.ci="HK")
meta_overall_results <- bind_rows(meta_overall_results, extract_meta_overall(meta_d_phq, "PHQ-8: Shamiri vs TAU"))
print(meta_d_phq)

# GAD-7 
b_tau_gad <- get_meta_stats(datasetB_orando, "gad_total", "TAU"); b_sha_gad <- get_meta_stats(datasetB_orando, "gad_total", "Shamiri")
c_tau_gad <- get_meta_stats(datasetC_all, "gad_total", "TAU");    c_sha_gad <- get_meta_stats(datasetC_all, "gad_total", "Shamiri")

meta_input_gad <- data.frame(
  study = c("Dataset B (Orando)", "Dataset C"),
  n.e = c(b_sha_gad$n, c_sha_gad$n), mean.e = c(b_sha_gad$m, c_sha_gad$m), sd.e = c(b_sha_gad$sd, c_sha_gad$sd),
  n.c = c(b_tau_gad$n, c_tau_gad$n), mean.c = c(b_tau_gad$m, c_tau_gad$m), sd.c = c(b_tau_gad$sd, c_tau_gad$sd)
)

meta_d_gad <- metacont(n.e=n.e, mean.e=mean.e, sd.e=sd.e, n.c=n.c, mean.c=mean.c, sd.c=sd.c, studlab=study, data=meta_input_gad, sm="SMD", method.smd="Hedges", random=TRUE, method.tau="REML", method.random.ci="HK")
meta_overall_results <- bind_rows(meta_overall_results, extract_meta_overall(meta_d_gad, "GAD-7: Shamiri vs TAU"))
print(meta_d_gad)


#-------------------------------------------------------------------------------
# 3. SUBGROUP META-ANALYSES (AGE & GENDER)
#-------------------------------------------------------------------------------

cat("\n3. SUBGROUP META-ANALYSES (AGE & GENDER)\n")
cat(strrep("-", 80), "\n")

compute_subgroup_meta <- function(df, grouping_var, outcome_var) {
  df %>%
    filter(timepoint == 4, condition %in% c("Shamiri", "TAU")) %>%
    group_by(dataset, condition, .data[[grouping_var]]) %>%
    summarise(n = sum(!is.na(.data[[outcome_var]])), mean_val = mean(.data[[outcome_var]], na.rm = TRUE), sd_val = sd(.data[[outcome_var]], na.rm = TRUE), .groups = "drop") %>%
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

between_age_phq <- compute_subgroup_meta(mhdp, "age_group", "phq_total")
meta_age_phq <- metagen(TE = hedges_g, seTE = se_g, studlab = paste(study, "-", age_group), data = between_age_phq, sm = "SMD", method.tau = "REML", random = TRUE, subgroup = age_group, title = "PHQ by Age")
meta_subgroup_results <- bind_rows(meta_subgroup_results, extract_meta_subgroup(meta_age_phq, "PHQ-8 by Age Group"))

between_age_gad <- compute_subgroup_meta(mhdp, "age_group", "gad_total")
meta_age_gad <- metagen(TE = hedges_g, seTE = se_g, studlab = paste(study, "-", age_group), data = between_age_gad, sm = "SMD", method.tau = "REML", random = TRUE, subgroup = age_group, title = "GAD by Age")
meta_subgroup_results <- bind_rows(meta_subgroup_results, extract_meta_subgroup(meta_age_gad, "GAD-7 by Age Group"))

between_gen_phq <- compute_subgroup_meta(mhdp, "gender_label", "phq_total")
meta_gen_phq <- metagen(TE = hedges_g, seTE = se_g, studlab = paste(study, "-", gender_label), data = between_gen_phq, sm = "SMD", method.tau = "REML", random = TRUE, subgroup = gender_label, title = "PHQ by Gender")
meta_subgroup_results <- bind_rows(meta_subgroup_results, extract_meta_subgroup(meta_gen_phq, "PHQ-8 by Gender"))

between_gen_gad <- compute_subgroup_meta(mhdp, "gender_label", "gad_total")
meta_gen_gad <- metagen(TE = hedges_g, seTE = se_g, studlab = paste(study, "-", gender_label), data = between_gen_gad, sm = "SMD", method.tau = "REML", random = TRUE, subgroup = gender_label, title = "GAD by Gender")
meta_subgroup_results <- bind_rows(meta_subgroup_results, extract_meta_subgroup(meta_gen_gad, "GAD-7 by Gender"))


#-------------------------------------------------------------------------------
# 4. MIXED-EFFECTS MODELS & PIECEWISE TRAJECTORIES
#-------------------------------------------------------------------------------

cat("\n4. MIXED-EFFECTS MODELS (LMMs)\n")
cat(strrep("-", 80), "\n")

mhdp_clean <- mhdp %>% mutate(timepoint_num = as.numeric(timepoint))

# Base LMM
lmm_phq <- lmer(phq_total ~ timepoint_num * condition + (1 | school_name/participant_id), data = mhdp_clean)

# Piecewise LMM
mhdp_piecewise <- mhdp_clean %>%
  mutate(time_active = ifelse(timepoint_num <= 4, timepoint_num, 4), time_followup = ifelse(timepoint_num > 4, timepoint_num - 4, 0))
lmm_piecewise <- lmer(phq_total ~ (time_active + time_followup) * condition + (1 | school_name/participant_id), data = mhdp_piecewise)

# Plot
pred_piecewise <- ggpredict(lmm_piecewise, terms = c("time_active", "condition"))
p_lmm <- ggplot(pred_piecewise, aes(x = x, y = predicted, color = group)) +
  geom_line(linewidth = 1.2) + geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.15, color = NA) +
  scale_color_manual(values = c("Shamiri" = "#9A8EE6", "TAU" = "#132964")) + scale_fill_manual(values = c("Shamiri" = "#9A8EE6", "TAU" = "#132964")) +
  labs(title = "Predicted PHQ-8 Trajectory (Active Phase)", x = "Weeks", y = "Predicted PHQ Score", color = "Condition", fill = "Condition") +
  theme_minimal(base_size = 14) + theme(legend.position = "bottom")
ggsave("../results/models/lmm_piecewise_trajectory.png", p_lmm, width = 10, height = 6, dpi = 300)


#-------------------------------------------------------------------------------
# 5. ATTRITION ANALYSIS (LOGISTIC REGRESSION)
#-------------------------------------------------------------------------------

attrition_df <- mhdp %>%
  group_by(participant_id) %>%
  summarise(baseline_phq = phq_total[timepoint == 0][1], dropped_out = !any(timepoint == 4), gender = gender_label[1], .groups = "drop") %>%
  filter(!is.na(baseline_phq))

attrition_model <- glm(dropped_out ~ baseline_phq + gender, data = attrition_df, family = binomial)


#-------------------------------------------------------------------------------
# 6. EXPORT EVERYTHING TO WORD
#-------------------------------------------------------------------------------

cat("\n5. EXPORTING ALL RESULTS TO WORD DOCUMENT\n")
cat(strrep("-", 80), "\n")

# Format LMMs and GLMs cleanly using broom.mixed::tidy
tidy_lmm_base <- tidy(lmm_phq, effects = "fixed") %>% mutate(across(where(is.numeric), ~round(., 3))) %>% mutate(p.value = format.pval(p.value, digits=3, eps=0.001))
tidy_lmm_piece <- tidy(lmm_piecewise, effects = "fixed") %>% mutate(across(where(is.numeric), ~round(., 3))) %>% mutate(p.value = format.pval(p.value, digits=3, eps=0.001))
tidy_attrition <- tidy(attrition_model) %>% mutate(across(where(is.numeric), ~round(., 3))) %>% mutate(p.value = format.pval(p.value, digits=3, eps=0.001))

# Build Word Doc
doc <- read_docx() %>%
  body_add_par("MHDP - Meta-Analyses and Advanced Modeling Results", style = "heading 1") %>%
  
  body_add_par("1. Overall Meta-Analyses (Shamiri vs TAU)", style = "heading 2") %>%
  body_add_flextable(flextable(meta_overall_results) %>% autofit()) %>%
  body_add_par("") %>%
  
  body_add_par("2. Subgroup Meta-Analyses", style = "heading 2") %>%
  body_add_flextable(flextable(meta_subgroup_results) %>% merge_v(j = "Analysis") %>% valign(j = "Analysis", valign = "top") %>% autofit()) %>%
  body_add_par("") %>%
  
  body_add_par("3. Linear Mixed Model (Base Continuous Time)", style = "heading 2") %>%
  body_add_flextable(flextable(tidy_lmm_base) %>% autofit()) %>%
  body_add_par("") %>%
  
  body_add_par("4. Linear Mixed Model (Piecewise Time)", style = "heading 2") %>%
  body_add_flextable(flextable(tidy_lmm_piece) %>% autofit()) %>%
  body_add_par("") %>%
  
  body_add_par("5. Attrition Analysis (Logistic Regression)", style = "heading 2") %>%
  body_add_flextable(flextable(tidy_attrition) %>% autofit())

print(doc, target = "../results/models/Meta_Analysis_and_Models.docx")

cat("✓ Word document successfully saved to '../results/Meta_Analysis_and_Models.docx'\n")
cat(strrep("=", 80), "\n")
cat("META-ANALYSIS & ADVANCED MODELING.\n")
cat(strrep("=", 80), "\n\n")
