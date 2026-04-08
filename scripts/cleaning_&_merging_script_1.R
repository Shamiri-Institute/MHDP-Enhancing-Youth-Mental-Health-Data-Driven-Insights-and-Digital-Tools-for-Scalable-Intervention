################################################################################
# PROJECT: MHDP DATA CLEANING
# PURPOSE: Consolidate and Clean Templeton 2, Wellsprings, and Anansi Trial 1
# DATE:    2025-10-01
# Strategy Documentation: Merging three distinct datasets (Templeton 2 Phase 2, 
#                         Wellsprings, and Anansi Trial 1) into a single 
#                         longitudinal dataset, standardizing variables, 
#                         demographics, and condition assignments.
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
  conflicted    # Namespace safety
)

# Resolve package conflicts
conflict_prefer("filter", "dplyr")
conflict_prefer("mutate", "dplyr")
conflict_prefer("rename", "dplyr")
conflict_prefer("select", "dplyr")

cat(strrep("=", 80), "\n")
cat("✓ Environment setup complete. Packages loaded.\n")
cat(strrep("=", 80), "\n\n")

#-------------------------------------------------------------------------------
# 2. DATA IMPORT
#-------------------------------------------------------------------------------

cat("1. IMPORTING DATASETS\n")
cat(strrep("-", 80), "\n")

# Dataset A: Templeton 2 Phase 2 (2022)
data22        <- read_excel("../datasets/raw/Templeton_2_Phase_2_Long_dataset.xlsx", col_types = "text")    
school_info22 <- read_excel("../datasets/raw/school_info_2022.xlsx")
cat("✓ Dataset A (Templeton 2) loaded:", nrow(data22), "rows\n")

# Dataset B: Wellsprings (2023)
data23_wells  <- read_excel("../datasets/raw/WellspringLongUnimputedPrepped.xlsx", col_types = "text") 
cat("✓ Dataset B (Wellsprings) loaded:", nrow(data23_wells), "rows\n")

# Dataset C: Anansi Trial 1 (2023) 
data23_1 <- read_csv("../datasets/raw/Anansi_basto1yfu_2023Trial1.csv", col_types = cols(.default = "c"))
school_info23 <- read_excel("../datasets/raw/school_info_2023.xlsx")
cat("✓ Dataset C (Anansi Trial 1) loaded:", nrow(data23_1), "rows\n\n")


#-------------------------------------------------------------------------------
# 3. CLEAN DATASET A: TEMPLETON 2 (2022)
#-------------------------------------------------------------------------------

cat("2. CLEANING DATASET A: TEMPLETON 2 (2022)\n")
cat(strrep("-", 80), "\n")

# Standardize column names
colnames(data22) <- tolower(colnames(data22))
colnames(data22)[89] <- "group_leader2"

colnames(data22)

# Add uniform identifiers and empty columns for future binding
data22$study_name            <- "shamiri_4"
data22$study_year            <- 2022
data22$condition             <- ""
data22$school_type           <- ""
data22$school_demographic    <- ""
data22$school_classification <- ""
data22$id                    <- ""

# Recode and Rename
data22 <- data22 %>%
  mutate(
    implementer = recode(
      implementer, 
      "Shamiri" = "SHAMIRI"
    )
  ) %>%
  rename(
    participant_id = shamiri_id,
    timepoint      = time,
    school_name    = school,
    school_county  = county
  )


data22 <- data22 %>%
  rename(
    # Templeton2_A
    feedback1_all_helpful_templeton2_A            = feedback1_all_helpful,
    feedback2_recommend_templeton2_A              = feedback2_recommend,
    feedback3_most_helpful_lesson_templeton2_A    = feedback3_most_helpful_lesson,
    feedback4_least_helpful_lesson_templeton2_A   = feedback4_least_helpful_lesson,
    feedback5_confusing_templeton2_A              = feedback5_confusing,
    feedback6_favorite_thing_templeton2_A         = feedback6_favorite_thing,
    feedback6_group_leader_templeton2_A           = group_leader2,
    feedback6_life_skills_templeton2_A            = life_skills,
    feedback6_socializing_templeton2_A            = socializing_positive_interaction,
    feedback6_other_templeton2_A                  = other...92,
    feedback7_change_templeton2_A                 = feedback7_change,
    feedback7_moresessions_templeton2_A           = more_sessions,
    feedback7_widersessions_templeton2_A          = wider_content,
    feedback7_moregifts_templeton2_A              = more_gifts, 
    feedback7_foodsnacks_templeton2_A             = food_snacks,
    feedback7_funativities_templeton2_A           = more_fun_activities,
    feedback7_change_other_templeton2_A           = other...99,
    feedback8_other_comments_templeton2_A         = feedback8_other_comments,
    
  )



# Merge school information 
data22 <- data22 %>%
  left_join(school_info22, by = "school_name", suffix = c("", ".info22")) %>%
  mutate(
    school_type           = if_else(school_type == "", school_type.info22, school_type),
    school_demographic    = if_else(school_demographic == "", school_demographic.info22, school_demographic),
    school_classification = if_else(school_classification == "", school_classification.info22, school_classification)
  ) %>%
  select(-ends_with(".info22"))

colnames(data22)

# Final column cleanup for Dataset A
data22 <- data22 %>%
  select(c(1:5, 8, 9, 10, 6, 7, 104:109, 11:100, everything()))

data22 <- data22 %>%
  select(-c(107:110))

data22$project <- "Templeton_2"

colnames(data22)

cat("✓ Dataset A cleaned and formatted\n\n")


#-------------------------------------------------------------------------------
# 4. CLEAN DATASET B: WELLSPRINGS (2023)
#-------------------------------------------------------------------------------

cat("3. CLEANING DATASET B: WELLSPRINGS (2023)\n")
cat(strrep("-", 80), "\n")

colnames(data23_wells) <- tolower(colnames(data23_wells))

colnames(data23_wells)


# Remove unnecessary columns
data23_wells <- data23_wells %>%
  select(-c(1, 2, 6, 13, 16, 17, 67:70, 67:95, 97:102))

table(data23_wells$implementer)

# Add uniform identifiers and empty columns
data23_wells$study_name            <- "shamiri_5"
data23_wells$study_year            <- 2023
data23_wells$school_demographic    <- ""
data23_wells$school_classification <- ""
data23_wells$school_county         <- ""
data23_wells$id                    <- ""
data23_wells$school_type           <- ""
data23_wells$project               <- "Wellsprings"

data23_wells <- data23_wells %>%
  rename(participant_id = "unique_participant_id")


data23_wells <- data23_wells %>%
  rename(
    # Wellsprings_B
    feedback1_all_helpful_wellsprings_B           = fb1helpul,
    feedback2_recommend_wellsprings_B             = fb2recommend,
    feedback3_confusing_wellsprings_B             = fb3confusing,
    feedback4_enjoy_wellsprings_B                 = fb4enjoy,
    feedback5_skilled_facilitators_wellsprings_B  = fb5skilledfacilitators,
    feedback6_enough_time_wellsprings_B           = fb6enoughtime,
    feedback7_life_influence_wellsprings_B        = `how do you think this program has influenced your life (at school or in your personal life)?`,
    feedback8_gains_wellsprings_B                 = `what are some things you have gained/learned from this program?`,
    feedback9_other_comments_wellsprings_B        = `do you have any other comments about the program ?`,
    
  )


colnames(data23_wells)

table(data23_wells$condition)






cat("✓ Dataset B cleaned and formatted\n\n")


#-------------------------------------------------------------------------------
# 5. CLEAN DATASET C: ANANSI TRIAL 1 (2023)
#-------------------------------------------------------------------------------

cat("4. CLEANING DATASET C: ANANSI TRIAL 1 (2023)\n")
cat(strrep("-", 80), "\n")

colnames(data23_1) <- tolower(colnames(data23_1))

# Add uniform identifiers and empty columns
data23_1$study_name            <- "shamiri_5"
data23_1$study_year            <- 2023
data23_1$implementer           <- "SHAMIRI"
data23_1$school_demographic    <- ""
data23_1$school_classification <- ""
data23_1$school_county         <- ""
data23_1$id                    <- ""
data23_1$school_type           <- ""
data23_1$project               <- "Anansi_1"


# Re-arrange and drop columns


data23_1 <- data23_1 %>%
  select(
    # 1. Core Identifiers & Study Metadata
    participant_id, time, study_name, study_year, project, implementer,
    
    # 2. Implementation Context
    condition, school_name, school_type, school_demographic,
    school_classification, school_county, group_name, group_leader_id, 
    
    # 3. Student Demographics (Common + Unique to 2023_1)
    age, form, gender, tribe, county, home, siblings, religion, parents_home,
    parents_dead, fathers_education, mothers_education, co_curricular,
    sports, percieved_academic_performance,
    
    # 4. Clinical Scales - PHQ (Depression)
    phq_1, phq_2, phq_3, phq_4, phq_5, phq_6, phq_7, phq_8, phq_functioning,
    
    # 5. Clinical Scales - GAD (Anxiety)
    gad_1, gad_2, gad_3, gad_4, gad_5, gad_6, gad_7, gad_check, gad_functioning,
    
    # 6. Clinical Scales - SWEMWBS (Wellbeing)
    swemwbs_1, swemwbs_2, swemwbs_3, swemwbs_4, swemwbs_5, swemwbs_6, swemwbs_7,
    
    # 7. Other Scales (MSPSS, GQ, PCS, SES)
    starts_with("mspss_"), starts_with("gq_"), starts_with("pcs_"), starts_with("ses_"),
    
    # 8. Feedback & Interest
    interview_interest, pfb_1_helpful, pfb_2_recommend, pfb_3_helpful_lesson,
    pfb_4_lsthelpful_lesson, pfb_5_confusing, pfb_6_favorite, pfb_6_group_leader, pfb_6_life_skills,
    pfb_6_socializing, pfb_6_other, pfb_7_changeimprove,
    pfb_7_moresessions, pfb_7_morecontent, pfb_7_funactivities, pfb_7_other, pfb_8_comments,
    
    # 9. System / Miscellaneous
    everything() # Catches "...1" and anything missed
  )



data23_1 <- data23_1 %>%
  rename(
    feedback1_all_helpful_anansi1_C               = pfb_1_helpful,
    feedback2_recommend_anansi1_C                 = pfb_2_recommend,
    feedback3_most_helpful_lesson_anansi1_C       = pfb_3_helpful_lesson,
    feedback4_least_helpful_lesson_anansi1_C      = pfb_4_lsthelpful_lesson,
    feedback5_confusing_anansi1_C                 = pfb_5_confusing,
    feedback6_favorite_thing_anansi1_C            = pfb_6_favorite,
    feedback6_favorit_groupleader_anansi1_C       = pfb_6_group_leader,
    feedback6_favorite_lifeskills_anansi1_C       = pfb_6_life_skills,
    feedback6_favorite_socialising_anansi1_C      = pfb_6_socializing,
    feedback6_favorite_thing_other_anansi1_C      = pfb_6_other,
    feedback7_change_anansi1_C                    = pfb_7_changeimprove,
    feedback7_moresessions_anansi1_C              = pfb_7_moresessions,
    feedback7_morecontent_anansi1_C               = pfb_7_morecontent,
    feedback7_funactivities_anansi1_C             = pfb_7_funactivities,
    feedback7_change_other_anansi1_C              = pfb_7_other,
    feedback8_other_comments_anansi1_C            = pfb_8_comments
    )



# Standardize school names to uppercase in both datasets to ensure a perfect match
data23_1 <- data23_1 %>%
  mutate(school_name = toupper(school_name)) %>%
  left_join(
    school_info23 %>% mutate(school_name = toupper(school_name)), 
    by = "school_name", 
    suffix = c("", ".info23")
  ) %>%
  mutate(
    school_type           = if_else(school_type == "", school_type.info23, school_type),
    school_demographic    = if_else(school_demographic == "", school_demographic.info23, school_demographic),
    school_classification = if_else(school_classification == "", school_classification.info23, school_classification),
    school_county         = if_else(school_county == "", school_county.info23, school_county)
  ) %>%
  select(-ends_with(".info23"))

data23_1 <- data23_1 %>%
  arrange(participant_id, as.numeric(time))

table(data23_1$school_name, data23_1$time)

# drop time point 16, seems to have data only from Kamuku school and is causing 

data23_1 <- data23_1 %>%
  filter(time != "16")
table(data23_1$school_name, data23_1$time)

cat("✓ Dataset C cleaned and formatted\n\n")


#-------------------------------------------------------------------------------
# 6. MERGE DATASETS
#-------------------------------------------------------------------------------

cat("5. MERGING ALL DATASETS\n")
cat(strrep("-", 80), "\n")

dfs <- list(data22, data23_1, data23_wells)

# Convert every column in every dataframe to character for safe binding
dfs <- lapply(dfs, function(df) {
  df[] <- lapply(df, as.character)
  df
})

mhdp_data <- dplyr::bind_rows(dfs)


cat("✓ Datasets merged successfully. Total rows:", nrow(mhdp_data), "\n\n")


#-------------------------------------------------------------------------------
# 7. COALESCE AND RENAME COLUMNS
#-------------------------------------------------------------------------------

cat("6. COALESCING AND STANDARDIZING COLUMNS\n")
cat(strrep("-", 80), "\n")

# Coalesce similar columns that came in under different names
mhdp_data <- mhdp_data %>%
  mutate(
    timepoint   = coalesce(timepoint, time),
    school_name = coalesce(school_name, school),
    age         = coalesce(age, ageuc),
    group       = coalesce(group, group_name),
    study_name   = coalesce(study_name, study)
  ) %>%
  select(-c(time, school, ageuc, group_name, study))




cat("✓ Variables coalesced and feedback columns renamed\n\n")



#-------------------------------------------------------------------------------
# 8. STRICT COLUMN WHITELIST (Replaces fragile numeric indexing)
#-------------------------------------------------------------------------------

master_cols <- c(
  # --- Core Identifiers & Study Metadata ---
  "timepoint", "participant_id", "study_name", "study_year",
  "project", "implementer", "condition",
  "group", "group_leader", "group_leader_id",
  
  # --- School Information ---
  "school_name", "school_county", "school_type",
  "school_demographic", "school_classification", "form_stream",
  
  # --- Demographics & Background ---
  "age", "form", "gender",
  "tribe", "county", "home",
  "siblings", "religion", "parents_home",
  "parents_dead", "fathers_education", "mothers_education",
  "co_curricular", "sports", "percieved_academic_performance",
  
  # --- Clinical Scales: PHQ-8 (Depression) & GAD-7 (Anxiety) ---
  "phq_1", "phq_2", "phq_3",
  "phq_4", "phq_5", "phq_6",
  "phq_7", "phq_8", "phq_functioning",
  "gad_1", "gad_2", "gad_3",
  "gad_4", "gad_5", "gad_6",
  "gad_7", "gad_functioning", "gad_check",
  
  # --- Wellbeing & Support Scales: SWEMWBS & MSPSS ---
  "swemwbs_1", "swemwbs_2", "swemwbs_3",
  "swemwbs_4", "swemwbs_5", "swemwbs_6",
  "swemwbs_7", "mspss_1", "mspss_2",
  "mspss_3", "mspss_4", "mspss_5",
  "mspss_6", "mspss_7", "mspss_8",
  "mspss_9", "mspss_10", "mspss_11",
  "mspss_12",
  
  # --- Other Psychometric Scales: PILS, EPOCH, GQ, PCS ---
  "pils_1", "pils_2", "pils_3",
  "pils_4", "pils_5", "pils_6",
  "pils_7", "pils_8", "pils_9",
  "pils_10", "pils_11", "pils_12",
  "epoch_o1", "epoch_o2", "epoch_o3",
  "epoch_o4", "epoch_h1", "epoch_h2",
  "epoch_h3", "epoch_h4", "epoch_p1",
  "epoch_p2", "epoch_p3", "epoch_p4",
  "gq_1", "gq_2", "gq_3",
  "gq_4", "gq_5", "gq_6",
  "pcs_1", "pcs_2", "pcs_3",
  "pcs_4", "pcs_5", "pcs_6",
  
  # --- Scale: SES ---
  "ses_1", "ses_2", "ses_3",
  "ses_4", "ses_5", "ses_6",
  "ses_7", "ses_8", "ses_9",
  "ses_10", "ses_11", "ses_12",
  "ses_13", "ses_14", "ses_15",
  "ses_16", "ses_17", "ses_18",
  "ses_19",
  
  # --- Feedback: Dataset A (Templeton 2) ---
  "feedback1_all_helpful_templeton2_A", "feedback2_recommend_templeton2_A", "feedback3_most_helpful_lesson_templeton2_A",
  "feedback4_least_helpful_lesson_templeton2_A", "feedback5_confusing_templeton2_A", "feedback6_favorite_thing_templeton2_A",
  "feedback6_group_leader_templeton2_A", "feedback6_life_skills_templeton2_A", "feedback6_socializing_templeton2_A",
  "feedback6_other_templeton2_A", "feedback7_change_templeton2_A", "feedback7_moresessions_templeton2_A",
  "feedback7_widersessions_templeton2_A", "feedback7_moregifts_templeton2_A", "feedback7_foodsnacks_templeton2_A",
  "feedback7_funativities_templeton2_A", "feedback7_change_other_templeton2_A", "feedback8_other_comments_templeton2_A",
  
  # --- Feedback: Dataset B (Wellsprings) ---
  "feedback1_all_helpful_wellsprings_B", "feedback2_recommend_wellsprings_B", "feedback3_confusing_wellsprings_B",
  "feedback4_enjoy_wellsprings_B", "feedback5_skilled_facilitators_wellsprings_B", "feedback6_enough_time_wellsprings_B",
  "feedback7_life_influence_wellsprings_B", "feedback8_gains_wellsprings_B", "feedback9_other_comments_wellsprings_B",
  
  # --- Feedback: Dataset C (Anansi Trial 1) ---
  # Anansi1_C
  "feedback1_all_helpful_anansi1_C", "feedback2_recommend_anansi1_C",
  "feedback3_most_helpful_lesson_anansi1_C", "feedback4_least_helpful_lesson_anansi1_C",
  "feedback5_confusing_anansi1_C", "feedback6_favorite_thing_anansi1_C",
  "feedback6_favorit_groupleader_anansi1_C","feedback6_favorite_lifeskills_anansi1_C", 
  "feedback6_favorite_socialising_anansi1_C","feedback6_favorite_thing_other_anansi1_C",
  "feedback7_change_anansi1_C", "feedback7_moresessions_anansi1_C",
  "feedback7_morecontent_anansi1_C", "feedback7_funactivities_anansi1_C",
  "feedback7_change_other_anansi1_C", "feedback8_other_comments_anansi1_C"
 )


# Funnel the dataset through the whitelist 
mhdp_data <- mhdp_data %>% 
  select(c(master_cols, everything()))

colnames(mhdp_data)

mhdp_data <- mhdp_data %>%
  select(-c(16,167:169))

colnames(mhdp_data)

cat("✓ Variables coalesced, renamed, and strictly filtered using whitelist.\n\n")

#-------------------------------------------------------------------------------
# 8. FIX CONDITIONS AND CLEANUP DEMOGRAPHICS
#-------------------------------------------------------------------------------

cat("7. FIXING CONDITIONS AND FINAL DEMOGRAPHICS\n")
cat(strrep("-", 80), "\n")

cat("Pre-cleanup Condition Distribution by Project:\n")
print(table(mhdp_data$condition, mhdp_data$project))

# Standardize conditions to "Shamiri" or "TAU"
mhdp_data <- mhdp_data %>%
  mutate(
    condition = case_when(
      project == "Templeton_2"                               ~ "Shamiri",
      project == "Wellsprings" & condition == "Active"       ~ "Shamiri",
      TRUE ~ condition
    )
  )

cat("\nPost-cleanup Condition Distribution by Project:\n")
print(table(mhdp_data$condition, mhdp_data$project))

# Convert timepoint to numeric
mhdp_data$timepoint <- as.numeric(as.character(mhdp_data$timepoint))


# Review Time Points
cat("\nTimepoints across projects:\n")
cat("Templeton 2:\n"); print(table(mhdp_data$timepoint[mhdp_data$project == "Templeton_2"]))
cat("Wellsprings:\n"); print(table(mhdp_data$timepoint[mhdp_data$project == "Wellsprings"]))
cat("Anansi 1 (Before Filtering):\n"); print(table(mhdp_data$timepoint[mhdp_data$project == "Anansi_1"]))

# Remove Anansi 1 six months follow-up (timepoint 28) 
# This issue at timepoint 6
# was fixed after re-cleaning the dataset, so this step is no longer necessary.
# If we need to reintroduce it, we can uncomment the code below.

#mhdp_data <- mhdp_data %>%
#  filter(!(project == "Anansi_1" & timepoint == 28))

# cat("\nAnansi 1 Timepoints (After Filtering):\n")
# print(table(mhdp_data$timepoint[mhdp_data$project == "Anansi_1"]))

# Introduce Dataset grouping column
mhdp_data <- mhdp_data %>%
  mutate(
    dataset = case_when(
      project == "Templeton_2" ~ "Dataset A",
      project == "Wellsprings" ~ "Dataset B",
      project == "Anansi_1"    ~ "Dataset C",
      TRUE ~ NA_character_
    )
  ) %>%
  select(dataset, everything())

table(mhdp_data$school_name, mhdp_data$school_county)

# Clean up School Counties
mhdp_data <- mhdp_data %>%
  mutate(
    school_county = case_when(
      project == "Wellsprings" & school_name == "ElbargonNakuru" ~ "Nakuru",
      project == "Wellsprings" & school_name == "OrandoKisumu"   ~ "Kisumu",
      project == "Wellsprings" & school_name == "RidoreKisumu"   ~ "Kisumu",
      TRUE ~ school_county
    )
  )

table(mhdp_data$school_name, mhdp_data$school_county)

cat("\nPre-cleanup Gender Distribution by Project:\n")
print(table(mhdp_data$gender, mhdp_data$project))

# Standardize gender formatting
mhdp_data <- mhdp_data %>%
  mutate(
    gender = case_when(
      tolower(gender) == "female" ~ "1",
      tolower(gender) == "male"   ~ "2",
      TRUE ~ as.character(gender)
    ),
    gender = as.numeric(gender)
  )

cat("\nPost-cleanup Gender Distribution by Project:\n")
print(table(mhdp_data$gender, mhdp_data$project))


#-------------------------------------------------------------------------------
# 9. EXPORT CLEANED DATASET
#-------------------------------------------------------------------------------

cat("\n8. EXPORTING FINAL DATASET\n")
cat(strrep("-", 80), "\n")

write_xlsx(mhdp_data, "../datasets/processed/mhdp_data.xlsx")

cat("✓ Master dataset successfully saved as 'mhdp_data.xlsx'\n")
cat(strrep("=", 80), "\n")
cat("ANALYSIS COMPLETE\n")
cat(strrep("=", 80), "\n\n")

################################################################################
# END OF SCRIPT
################################################################################














