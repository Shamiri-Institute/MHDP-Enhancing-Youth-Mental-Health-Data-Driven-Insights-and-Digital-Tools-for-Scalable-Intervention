################################################################################
# PROJECT: MENTAL HEALTH DATA PRIZE (OBJECTIVE 2)
# PURPOSE: Part 3 - Complete-Case Moderator Analysis & Machine Learning
# Strategy Documentation: Identifies differential treatment outcomes (Moderators).
#                         Uses PURE complete-case analysis (dropping NAs dynamically).
#                         Runs LMM 3-way interactions on both Full and Clinical 
#                         samples. Exports EACH model to its own Word doc.
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
  partykit,     # MACHINE LEARNING: Tree-based moderator analysis
  broom.mixed,  # Tidying model outputs for Word
  officer,      # Exporting to Word
  flextable,    # Formatting tables for Word
  conflicted    # Namespace safety
)

conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("lmer", "lmerTest")

# Create output directories
dir.create("../results/objective_2/models/full_sample", recursive = TRUE, showWarnings = FALSE)
dir.create("../results/objective_2/models/clinical_sample", recursive = TRUE, showWarnings = FALSE)
dir.create("../results/objective_2/figures", recursive = TRUE, showWarnings = FALSE)

cat(strrep("=", 80), "\n")
cat("1. IMPORTING AND PREPARING DATASET\n")
cat(strrep("-", 80), "\n")

mhdp <- read_excel("../datasets/processed/mhdp_data_2.xlsx", col_types = "text")
colnames(mhdp) <- tolower(colnames(mhdp))

# Drop known duplicated student
mhdp <- mhdp %>% filter(!(dataset == "Shamiri_3.0" & participant_id == "40281"))

# Convert psychometric and demographic columns to numeric safely
mhdp <- suppressWarnings(
  mhdp %>% mutate(across(c(
    timepoint, age, form, financial_status, home, siblings, religion, parents_home,
    parents_dead, fathers_education, mothers_education, co_curricular, sports, 
    perceived_academic_abilities, starts_with("phq_"), starts_with("gad_")
  ), as.numeric))
)

# Compute Totals and Clean Labels (Study Skills = Control baseline)
mhdp <- mhdp %>%
  mutate(
    phq_total = if_else(if_any(phq_1:phq_8, is.na), NA_real_, rowSums(across(phq_1:phq_8))),
    condition = tolower(condition),
    condition = case_when(
      dataset == "Shamiri_1.0" & condition == "intervention" ~ "shamiri",
      dataset == "Shamiri_1.0" & condition == "control" ~ "study skills",
      dataset == "Shamiri_2.0" & condition == "wellness" ~ "shamiri",
      condition == "study-skills" ~ "study skills", 
      TRUE ~ condition
    )
  ) %>%
  filter(!is.na(condition) & condition != "na") %>%
  mutate(condition = factor(condition, levels = c("study skills", "shamiri", "gratitude", "growth", "values", "control")))

cat("✓ Master dataset loaded and numeric conversions completed.\n\n")


#-------------------------------------------------------------------------------
# 2. DATA PREP: CATEGORIZING TRAITS (LEAVING NAs AS NA)
#-------------------------------------------------------------------------------

cat("2. CATEGORIZING TRAITS FOR PURE COMPLETE-CASE ANALYSIS\n")
cat(strrep("-", 80), "\n")

# We create clean, binned categories for our demographics. 
# IMPORTANT: We use NA_character_ instead of "Unknown" to enable dynamic complete-case filtering.
mhdp_mod_data <- mhdp %>%
  mutate(timepoint_num = as.numeric(timepoint)) %>%
  group_by(participant_id) %>%
  arrange(timepoint_num, .by_group = TRUE) %>%
  mutate(phq_baseline = phq_total[timepoint_num == 0][1]) %>%
  ungroup() %>%
  mutate(
    # Core Demographics
    age_group    = case_when(age <= 14 ~ "12-14 yrs", age <= 16 ~ "15-16 yrs", age >= 17 ~ "17-20 yrs", TRUE ~ NA_character_),
    gender_cat   = case_when(tolower(gender) == "female" ~ "Female", tolower(gender) == "male" ~ "Male", TRUE ~ NA_character_),
    form_cat     = case_when(tolower(gender) == "female" ~ "Female", tolower(gender) == "male" ~ "Male", TRUE ~ NA_character_),
    form_cat     = case_when(form %in% c(1) ~ "Form 1", form %in% c(2) ~ "Form 2", 
                             form %in% c(3) ~ "Form 3", form %in% c(4) ~ "Form 3",TRUE ~ NA_character_),
    
    # Socioeconomic & Home Context
    home_cat     = case_when(home %in% c(1) ~ "Rural", home %in% c(2) ~ "Small Town",
                             home %in% c(3) ~ "Big Town", home %in% c(3, 4) ~ "City", TRUE ~ NA_character_),
    sibling_cat  = case_when(siblings <= 2 ~ "0-2 Siblings", siblings >= 3 ~ "3+ Siblings", TRUE ~ NA_character_),

    
    # Extracurriculars & School Context
    acad_cat     = case_when(perceived_academic_abilities <= 2 ~ "Poor/Satisfactory", perceived_academic_abilities >= 3 ~ "Good/Excellent", TRUE ~ NA_character_),
    sports_cat   = case_when(sports == 1 ~ "Yes", sports %in% c(2, 5) ~ "No/Other", TRUE ~ NA_character_),
    club_cat     = case_when(co_curricular <= 2 ~ "Not/Slightly Involved", co_curricular >= 3 ~ "Highly Involved", TRUE ~ NA_character_)
  ) %>%
  # Filter to the baseline and endpoint for the Shamiri vs Study Skills moderation test
  filter(timepoint_num %in% c(0, 4)) %>%
  filter(condition %in% c("shamiri", "study skills")) %>%
  mutate(condition = droplevels(condition)) %>%
  drop_na(phq_total, phq_baseline, condition)

# Define the Two Samples
obj2_full <- mhdp_mod_data
obj2_clinical <- mhdp_mod_data %>% filter(phq_baseline >= 10)

cat("✓ Data categorized. NAs retained for dynamic exclusion.\n")
cat("✓ Full Sample Base N:", nrow(obj2_full), "| Clinical Sample Base N:", nrow(obj2_clinical), "\n\n")


#-------------------------------------------------------------------------------
# 3. AUTOMATED LMM MODERATOR FUNCTION (COMPLETE-CASE & EXPORT)
#-------------------------------------------------------------------------------

# This master function drops NAs for the target variable, runs the LMM, prints
# to console, and exports to a dedicated Word document.
run_and_export_lmm <- function(data_subset, moderator_var, mod_label, file_safe_name, sample_name, export_folder) {
  
  cat(sprintf(">>> %s | Testing Moderator: %s <<<\n", toupper(sample_name), mod_label))
  
  # 1. PURE COMPLETE-CASE ISOLATION: Drop NAs and convert to factor
  pure_data <- data_subset %>%
    drop_na(any_of(moderator_var)) %>%
    mutate(!!sym(moderator_var) := as.factor(.data[[moderator_var]]))
  
  valid_levels <- levels(pure_data[[moderator_var]])
  n_pure <- nrow(pure_data)
  
  if (length(valid_levels) < 2) {
    cat(sprintf("  ⚠ WARNING: '%s' only has %d level (%s) in pure data. Skipping.\n\n", moderator_var, length(valid_levels), valid_levels[1]))
    return(NULL)
  }
  
  cat(sprintf("  ✓ Pure Complete-Case N: %d | Levels: %s\n", n_pure, paste(valid_levels, collapse = " vs ")))
  
  # 2. RUN THE LMM
  f_mod <- paste("phq_total ~ timepoint_num * condition *", moderator_var, "+ phq_baseline + (1 | school_name / participant_id)")
  lmm_model <- lmer(as.formula(f_mod), data = pure_data)
  
  # 3. PRINT RESULTS TO CONSOLE
  summary_stats <- summary(lmm_model)$coefficients
  search_string <- paste0("timepoint_num:conditionshamiri:", moderator_var)
  interaction_rows <- rownames(summary_stats)[str_detect(rownames(summary_stats), fixed(search_string))]
  
  if(length(interaction_rows) > 0) {
    for(row in interaction_rows) {
      p_val <- summary_stats[row, "Pr(>|t|)"]
      est <- summary_stats[row, "Estimate"]
      clean_level <- str_replace(row, fixed(search_string), "")
      
      if(p_val < 0.05) cat(sprintf("  ★ SIGNIFICANT [%s]: Est: %.3f, p = %.3f\n", clean_level, est, p_val))
      else cat(sprintf("  - Not Sig     [%s]: Est: %.3f, p = %.3f\n", clean_level, est, p_val))
    }
  } else {
    cat("  No interaction terms found targeting Shamiri.\n")
  }
  cat("\n")
  
  # 4. EXPORT TO INDIVIDUAL WORD DOCUMENT
  tidy_df <- tidy(lmm_model, effects = "fixed") %>% 
    mutate(across(where(is.numeric), ~round(., 3)), p.value = format.pval(p.value, digits=3, eps=0.001))
  
  doc_title <- paste(sample_name, "Moderation Analysis:", mod_label)
  doc <- read_docx() %>%
    body_add_par(doc_title, style = "heading 1") %>%
    body_add_par(paste("Pure Complete-Case Sample Size:", n_pure, "observations."), style = "Normal") %>%
    body_add_par(paste("Formula:", paste(format(formula(lmm_model)), collapse = " ")), style = "Normal") %>%
    body_add_par("Note: Look for 3-way interactions (timepoint_num:condition:moderator) to confirm specific subgroup efficacy.", style = "Normal") %>%
    body_add_par("") %>%
    body_add_flextable(flextable(tidy_df) %>% autofit())
  
  print(doc, target = paste0("../results/objective_2/models/", export_folder, "/", file_safe_name, "_", sample_name, ".docx"))
  
  return(lmm_model)
}

#-------------------------------------------------------------------------------
# 4. EXECUTE MODELS (FULL SAMPLE & CLINICAL SUBSAMPLE)
#-------------------------------------------------------------------------------

cat("3. CLASSICAL MODERATOR ANALYSIS (3-Way LMMs)\n")
cat(strrep("-", 80), "\n")

# List of all comprehensive variables to test
mod_vars <- list(
  list(col = "age_group",    lbl = "Age Group",            fn = "Mod_Age"),
  list(col = "gender_cat",   lbl = "Gender",               fn = "Mod_Gender"),
  list(col = "form_cat",     lbl = "School Form",          fn = "Mod_Form"),
  list(col = "home_cat",     lbl = "Home Environment",     fn = "Mod_Home"),
  list(col = "sibling_cat",  lbl = "Number of Siblings",   fn = "Mod_Siblings"),
  list(col = "acad_cat",     lbl = "Perceived Academics",  fn = "Mod_Academics"),
  list(col = "sports_cat",   lbl = "Sports Involvement",   fn = "Mod_Sports"),
  list(col = "club_cat",     lbl = "Club Involvement",     fn = "Mod_Clubs")
)

# Run loop for FULL SAMPLE
for(m in mod_vars) {
  run_and_export_lmm(obj2_full, m$col, m$lbl, m$fn, "Full_Sample", "full_sample")
}

cat(strrep("-", 80), "\n")

# Run loop for CLINICAL SAMPLE
for(m in mod_vars) {
  run_and_export_lmm(obj2_clinical, m$col, m$lbl, m$fn, "Clinical_Sample", "clinical_sample")
}


#-------------------------------------------------------------------------------
# 5. TREE-BASED MODERATOR ANALYSIS (MACHINE LEARNING)
#-------------------------------------------------------------------------------

cat("4. TREE-BASED MODERATOR ANALYSIS (Recursive Partitioning)\n")
cat(strrep("-", 80), "\n")

# For the ML tree, we use a wide, flattened format of the complete-cases subset of demographics
# We will focus on the most universally completed demographics to give the tree enough power to split
ml_data <- obj2_full %>%
  select(participant_id, timepoint_num, condition, phq_total, phq_baseline, 
         age_group, gender_cat, home_cat, acad_cat) %>%
  pivot_wider(names_from = timepoint_num, values_from = phq_total, names_prefix = "W") %>%
  mutate(phq_reduction = W0 - W4) %>% 
  drop_na(phq_reduction, age_group, gender_cat,  home_cat, acad_cat) %>%
  mutate(across(where(is.character), as.factor))

cat(sprintf("ML Algorithm trained on pure demographic complete-cases: n = %d\n", nrow(ml_data)))

# Run the Recursive Partitioning algorithm
tree_model <- lmtree(
  phq_reduction ~ condition | age_group + gender_cat +  home_cat + acad_cat + phq_baseline, 
  data = ml_data,
  minsize = 40 # Minimum group size required for a split
)

cat("\n--- Detected Subgroups (Tree Nodes) ---\n")
print(tree_model)

# Export the visualization of the machine learning tree
png("../results/objective_2/figures/MachineLearning_Subgroup_Tree.png", width = 1400, height = 900, res = 120)
plot(tree_model, main = "Machine Learning Detected Subgroups (Treatment Response)")
dev.off()

cat("✓ Machine Learning Tree plotted and saved to results/objective_2/figures/.\n")
cat(strrep("=", 80), "\n")
cat("MODERATOR & INDIVIDUAL DIFFERENCES SCRIPT COMPLETE\n")
cat(strrep("=", 80), "\n\n")
