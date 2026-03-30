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
data22        <- read_excel("../datasets/Templeton_2_Phase_2_Long_dataset.xlsx", col_types = "text")    
school_info22 <- read_excel("../datasets/school_info_2022.xlsx")
cat("✓ Dataset A (Templeton 2) loaded:", nrow(data22), "rows\n")

# Dataset B: Wellsprings (2023)
data23_wells  <- read_excel("../datasets/WellspringLongUnimputedPrepped.xlsx", col_types = "text") 
cat("✓ Dataset B (Wellsprings) loaded:", nrow(data23_wells), "rows\n")

# Dataset C: Anansi Trial 1 (2023) 
data23_1 <- read_csv("../datasets/Anansi_basto1yfu_2023Trial1.csv", col_types = cols(.default = "c"))
school_info23 <- read_excel("../datasets/school_info_2023.xlsx")
cat("✓ Dataset C (Anansi Trial 1) loaded:", nrow(data23_1), "rows\n\n")


#-------------------------------------------------------------------------------
# 3. CLEAN DATASET A: TEMPLETON 2 (2022)
#-------------------------------------------------------------------------------

cat("2. CLEANING DATASET A: TEMPLETON 2 (2022)\n")
cat(strrep("-", 80), "\n")

# Standardize column names
colnames(data22) <- tolower(colnames(data22))
colnames(data22)[89] <- "group_leader2"

# Add uniform identifiers and empty columns for future binding
data22$study_name            <- "shamiri_4"
data22$study_year            <- 2022
data22$condition             <- ""
data22$school_type           <- ""
data22$school_demographic    <- ""
data22$school_classification <- ""
data22$id                    <- ""
data22$pcs_academic_1        <- ""
data22$pcs_academic_2        <- ""
data22$pcs_academic_3        <- ""
data22$pcs_academic_4        <- ""
data22$pcs_academic_5        <- ""
data22$pcs_academic_6        <- ""
data22$pcs_academic_7        <- ""
data22$pcs_academic_8        <- ""

# Recode and Rename
data22 <- data22 %>%
  mutate(
    implementer = recode(
      implementer, 
      "AMHRTF" = "Shamiri_Partner",
      "Shamiri" = "Shamiri_Hubs"
    )
  ) %>%
  rename(
    participant_id = shamiri_id,
    timepoint      = time,
    school_name    = school,
    school_county  = county
  )

# Re-arrange columns
data22 <- data22 %>%
  select(2, 1, 104, 105, 106, 5, 6, 107, 108, 109, 7, 3, 110, 8:10, 17:25, 26:33, 
         52:58, 72, 77, 78, 80, 73:75, 82, 71, 76, 79, 81, 111:118, 46:51, 83:87,
         88, 93, 100, everything()) %>%
  select(1:66, 82:105, 67:74, 76:81, everything())

# Merge school information 
data22 <- data22 %>%
  left_join(school_info22, by = "school_name", suffix = c("", ".info22")) %>%
  mutate(
    school_type           = if_else(school_type == "", school_type.info22, school_type),
    school_demographic    = if_else(school_demographic == "", school_demographic.info22, school_demographic),
    school_classification = if_else(school_classification == "", school_classification.info22, school_classification)
  ) %>%
  select(-ends_with(".info22"))

# Final column cleanup for Dataset A
data22 <- data22 %>%
  select(-c(53:60)) %>%
  select(c(1:82, 91:96, everything()))

data22$project <- "Templeton_2"

cat("✓ Dataset A cleaned and formatted\n\n")


#-------------------------------------------------------------------------------
# 4. CLEAN DATASET B: WELLSPRINGS (2023)
#-------------------------------------------------------------------------------

cat("3. CLEANING DATASET B: WELLSPRINGS (2023)\n")
cat(strrep("-", 80), "\n")

colnames(data23_wells) <- tolower(colnames(data23_wells))

# Remove unnecessary columns
data23_wells <- data23_wells %>%
  select(-c(1, 2, 6, 13, 16, 17, 67:70, 75:95, 97:102))

# Add uniform identifiers and empty columns
data23_wells$study_name            <- "shamiri_5"
data23_wells$study_year            <- 2023
data23_wells$implementer           <- "Shamiri_Hubs"
data23_wells$school_demographic    <- ""
data23_wells$school_classification <- ""
data23_wells$school_county         <- ""
data23_wells$id                    <- ""
data23_wells$school_type           <- ""
data23_wells$project               <- "Wellsprings"

data23_wells <- data23_wells %>%
  rename(participant_id = "unique_participant_id")

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
data23_1$implementer           <- "Shamiri_Hubs"
data23_1$school_demographic    <- ""
data23_1$school_classification <- ""
data23_1$school_county         <- ""
data23_1$id                    <- ""
data23_1$school_type           <- ""
data23_1$project               <- "Anansi_1"

# Rename columns
data23_1 <- data23_1 %>%
  rename(
    timepoint = time,
    swemwbs_1 = swembs_1,
    swemwbs_2 = swembs_2,
    swemwbs_3 = swembs_3,
    swemwbs_4 = swembs_4,
    swemwbs_5 = swembs_5,
    swemwbs_6 = swembs_6,
    swemwbs_7 = swembs_7
  )

# Re-arrange and drop columns
data23_1 <- data23_1 %>%
  select(1:54, 57:62, 64:82, 96:101, 105, everything()) %>%
  select(-c(5, 6, 29, 86, 91:93, 100, 104, 106:170, 172:190))

# Merge school information 
data23_1 <- data23_1 %>%
  left_join(school_info23, by = "school_name", suffix = c("", ".info23")) %>%
  mutate(
    school_type           = if_else(school_type == "", school_type.info23, school_type),
    school_demographic    = if_else(school_demographic == "", school_demographic.info23, school_demographic),
    school_classification = if_else(school_classification == "", school_classification.info23, school_classification),
    school_county         = if_else(school_county == "", school_county.info23, school_county)
  ) %>%
  select(-ends_with(".info23"))

# Final column cleanup for Dataset C
data23_1 <- data23_1 %>%
  select(c(1:51, 83, 84, 52:57, 85, 58:64, 87:92, 77, 78, 82, 95, everything()))

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

mhdp_data <- mhdp_data %>%
  select(1:88, 112:130, everything())

cat("✓ Datasets merged successfully. Total rows:", nrow(mhdp_data), "\n\n")


#-------------------------------------------------------------------------------
# 7. COALESCE AND RENAME COLUMNS
#-------------------------------------------------------------------------------

cat("6. COALESCING AND STANDARDIZING COLUMNS\n")
cat(strrep("-", 80), "\n")

# Drop unnecessary artifacts
mhdp_data <- mhdp_data %>%
  select(-c(13, 116, 169:172))

# Coalesce similar columns that came in under different names
mhdp_data <- mhdp_data %>%
  mutate(
    timepoint   = coalesce(timepoint, time),
    school_name = coalesce(school_name, school),
    age         = coalesce(age, ageuc)
  ) %>%
  select(-c(time, school, ageuc))

# Rename feedback columns to maintain origin tracking
mhdp_data <- mhdp_data %>%
  rename(
    # Templeton2_A
    feedback1_all_helpful_templeton2_A          = feedback1_all_helpful,
    feedback2_recommend_templeton2_A            = feedback2_recommend,
    feedback3_most_helpful_lesson_templeton2_A  = feedback3_most_helpful_lesson,
    feedback4_least_helpful_lesson_templeton2_A = feedback4_least_helpful_lesson,
    feedback5_confusing_templeton2_A            = feedback5_confusing,
    feedback6_favorite_thing_templeton2_A       = feedback6_favorite_thing,
    feedback7_change_templeton2_A               = feedback7_change,
    feedback8_other_comments_templeton2_A       = feedback8_other_comments,
    
    # Wellsprings_B
    feedback1_all_helpful_wellsprings_B         = fb1helpul,
    feedback2_recommend_wellsprings_B           = fb2recommend,
    feedback3_confusing_wellsprings_B           = fb3confusing,
    feedback4_enjoy_wellsprings_B               = fb4enjoy,
    feedback5_skilled_facilitators_wellsprings_B= fb5skilledfacilitators,
    feedback6_enough_time_wellsprings_B         = fb6enoughtime,
    feedback7_life_influence_wellsprings_B      = `how do you think this program has influenced your life (at school or in your personal life)?`,
    feedback8_gains_wellsprings_B               = `what are some things you have gained/learned from this program?`,
    feedback9_other_comments_wellsprings_B      = `do you have any other comments about the program ?`,
    
    # Anansi1_C
    feedback1_all_helpful_anansi1_C             = pfb_1,
    feedback2_recommend_anansi1_C               = pfb_2,
    feedback3_most_helpful_lesson_anansi1_C     = pfb_3,
    feedback4_least_helpful_lesson_anansi1_C    = pfb_4,
    feedback5_confusing_anansi1_C               = pfb_5,
    feedback6_favorite_thing_anansi1_C          = pfb_6,
    feedback6_favorite_thing_other_anansi1_C    = pfb_6_other,
    feedback7_change_anansi1_C                  = pfb_7,
    feedback7_change_other_anansi1_C            = pfb_7_other,
    feedback8_other_comments_anansi1_C          = pfb_8,
    feedback9_questionnaire_frustration_anansi1_C = questionnaire_frustration
  )

# Final structural cleanup
mhdp_data <- mhdp_data %>%
  select(-c(115:127, 148:150, 152, 155)) %>%
  select(c(1:106, 126:135, 107:114, 138:146, 116:125, 136, everything())) %>%
  select(-146)

cat("✓ Variables coalesced and feedback columns renamed\n\n")


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
      project == "Wellsprings" & condition == "Waitlist"     ~ "TAU",
      project == "Anansi_1" & condition == "Control"         ~ "TAU",
      TRUE ~ condition
    )
  )

cat("\nPost-cleanup Condition Distribution by Project:\n")
print(table(mhdp_data$condition, mhdp_data$project))

# Review Time Points
cat("\nTimepoints across projects:\n")
cat("Templeton 2:\n"); print(table(mhdp_data$timepoint[mhdp_data$project == "Templeton_2"]))
cat("Wellsprings:\n"); print(table(mhdp_data$timepoint[mhdp_data$project == "Wellsprings"]))
cat("Anansi 1 (Before Filtering):\n"); print(table(mhdp_data$timepoint[mhdp_data$project == "Anansi_1"]))

# Remove Anansi 1 six months follow-up (timepoint 28)
mhdp_data <- mhdp_data %>%
  filter(!(project == "Anansi_1" & timepoint == 28))

cat("\nAnansi 1 Timepoints (After Filtering):\n")
print(table(mhdp_data$timepoint[mhdp_data$project == "Anansi_1"]))

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

# Clean up School Counties
mhdp_data <- mhdp_data %>%
  mutate(
    school_county = case_when(
      project == "Wellsprings" & school_name == "ElbargonNakuru" ~ "Nakuru",
      project == "Wellsprings" & school_name == "OrandoKisumu"   ~ "Kisumu",
      project == "Wellspring"  & school_name == "RidoreKisumu"   ~ "Kisumu",
      TRUE ~ school_county
    )
  )

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

write_xlsx(mhdp_data, "mhdp_data.xlsx")

cat("✓ Master dataset successfully saved as 'mhdp_data.xlsx'\n")
cat(strrep("=", 80), "\n")
cat("ANALYSIS COMPLETE\n")
cat(strrep("=", 80), "\n\n")

################################################################################
# END OF SCRIPT
################################################################################