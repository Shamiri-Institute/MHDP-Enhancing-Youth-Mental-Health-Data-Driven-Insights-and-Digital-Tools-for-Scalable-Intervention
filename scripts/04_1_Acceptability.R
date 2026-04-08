################################################################################
# PROJECT: MENTAL HEALTH DATA PRIZE
# PURPOSE: Part 4 - Acceptability & Implementation Fidelity ("Simple Enough")
# Strategy Documentation: Analyzes student feedback surveys completed at Week 4.
#                         Generates percentage summaries and visualizations to 
#                         demonstrate program acceptability and provider skill.
#                         Includes both general and dataset-specific breakdowns.
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
  tidyverse,    # Data manipulation & plotting
  readxl,       # Reading Excel files
  writexl,      # Writing Excel files
  officer,      # Exporting to Word
  flextable,    # Formatting tables for Word
  conflicted    # Namespace safety
)

conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")

dir.create("../results/objective_1/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("../results/objective_1/tables", recursive = TRUE, showWarnings = FALSE)

cat(strrep("=", 80), "\n")
cat("1. IMPORTING AND PREPARING FEEDBACK DATA\n")
cat(strrep("-", 80), "\n")

mhdp <- read_excel("../datasets/processed/mhdp_data.xlsx", col_types = "text")

# Filter to ONLY students who received the Shamiri intervention and completed the Week 4 feedback survey
mhdp_feedback <- mhdp %>%
  mutate(
    condition_clean = ifelse(condition %in% c("TAU", "Waitlist"), "Control", condition),
    delivery_group = case_when(
      condition_clean == "Control" ~ "Control",
      implementer %in% c("AMHRTF", "PDO") ~ "Partner",
      TRUE ~ "Shamiri"
    )
  ) %>%
  filter(timepoint == "4", condition_clean == "Shamiri")

cat("✓ Feedback dataset isolated for intervention students at Week 4 (n =", nrow(mhdp_feedback), ")\n\n")


#-------------------------------------------------------------------------------
# 2. CONSOLIDATING FEEDBACK METRICS ACROSS DATASETS
#-------------------------------------------------------------------------------

cat("2. STANDARDIZING ACCEPTABILITY METRICS\n")
cat(strrep("-", 80), "\n")

# Consolidate the two most critical metrics: "Was it helpful?" and "Would you recommend it?"
mhdp_feedback <- mhdp_feedback %>%
  mutate(
    rating_helpful = coalesce(
      as.numeric(feedback1_all_helpful_templeton2_A),
      as.numeric(feedback1_all_helpful_wellsprings_B),
      as.numeric(feedback1_all_helpful_anansi1_C)
    ),
    rating_recommend = coalesce(
      as.numeric(feedback2_recommend_templeton2_A),
      as.numeric(feedback2_recommend_wellsprings_B),
      as.numeric(feedback2_recommend_anansi1_C)
    )
  ) %>%
  # Convert 1-5 Likert scales into clear "Positive vs Negative/Neutral" categories
  mutate(
    Helpful_Category = case_when(
      rating_helpful >= 4 ~ "Helpful",
      rating_helpful < 4 ~ "Neutral / Not Helpful",
      TRUE ~ NA_character_
    ),
    Recommend_Category = case_when(
      rating_recommend >= 4 ~ "Would Recommend",
      rating_recommend < 4 ~ "Neutral / Would Not",
      TRUE ~ NA_character_
    )
  )

cat("✓ Feedback scales consolidated and categorized.\n\n")


#-------------------------------------------------------------------------------
# 3. GENERATING SUMMARY TABLES
#-------------------------------------------------------------------------------

cat("3. CALCULATING ACCEPTABILITY PERCENTAGES\n")
cat(strrep("-", 80), "\n")

# 1. Overall Acceptability (Grand Total)
overall_acceptability <- mhdp_feedback %>%
  summarise(
    Total_Responses = sum(!is.na(rating_helpful)),
    Pct_Found_Helpful = round(sum(Helpful_Category == "Helpful", na.rm = TRUE) / sum(!is.na(rating_helpful)) * 100, 1),
    Pct_Would_Recommend = round(sum(Recommend_Category == "Would Recommend", na.rm = TRUE) / sum(!is.na(rating_recommend)) * 100, 1)
  )

# 2. Acceptability by Delivery Group ONLY (General Comparison)
group_acceptability <- mhdp_feedback %>%
  group_by(delivery_group) %>%
  summarise(
    Total_Responses = sum(!is.na(rating_helpful)),
    Pct_Found_Helpful = round(sum(Helpful_Category == "Helpful", na.rm = TRUE) / sum(!is.na(rating_helpful)) * 100, 1),
    Pct_Would_Recommend = round(sum(Recommend_Category == "Would Recommend", na.rm = TRUE) / sum(!is.na(rating_recommend)) * 100, 1),
    .groups = "drop"
  ) %>%
  filter(Total_Responses > 0)

# 3. Acceptability by Dataset AND Delivery Group (Detailed Breakdown)
dataset_acceptability <- mhdp_feedback %>%
  group_by(dataset, delivery_group) %>%
  summarise(
    Total_Responses = sum(!is.na(rating_helpful)),
    Pct_Found_Helpful = round(sum(Helpful_Category == "Helpful", na.rm = TRUE) / sum(!is.na(rating_helpful)) * 100, 1),
    Pct_Would_Recommend = round(sum(Recommend_Category == "Would Recommend", na.rm = TRUE) / sum(!is.na(rating_recommend)) * 100, 1),
    .groups = "drop"
  ) %>%
  filter(Total_Responses > 0) %>%
  arrange(dataset, delivery_group)

cat("--- Overall Acceptability ---\n")
print(overall_acceptability)
cat("\n--- Acceptability by Delivery Group (General) ---\n")
print(group_acceptability)
cat("\n--- Acceptability by Dataset & Delivery Group (Detailed) ---\n")
print(dataset_acceptability)
cat("\n")

write_xlsx(list(
  "Overall" = overall_acceptability, 
  "By_Group" = group_acceptability,
  "By_Dataset_and_Group" = dataset_acceptability
), "../results/objective_1/tables/Acceptability_Summaries.xlsx")


#-------------------------------------------------------------------------------
# 4. VISUALIZING ACCEPTABILITY
#-------------------------------------------------------------------------------

cat("4. PLOTTING ACCEPTABILITY\n")
cat(strrep("-", 80), "\n")

# --- PLOT 1: GENERAL COMPARISON (Delivery Group Only) ---
plot_data_general <- group_acceptability %>%
  pivot_longer(cols = c(Pct_Found_Helpful, Pct_Would_Recommend), names_to = "Metric", values_to = "Percentage") %>%
  mutate(Metric = ifelse(Metric == "Pct_Found_Helpful", "Found Program Helpful", "Would Recommend to a Friend"))

p_accept_general <- ggplot(plot_data_general, aes(x = delivery_group, y = Percentage, fill = Metric)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = paste0(Percentage, "%")), position = position_dodge(width = 0.8), vjust = -0.5, fontface = "bold", size = 4.5) +
  scale_fill_manual(values = c("Found Program Helpful" = "#9A8EE6", "Would Recommend to a Friend" = "#132964")) +
  scale_y_continuous(limits = c(0, 110), breaks = seq(0, 100, 20)) +
  labs(
    title = "Program Acceptability by Delivery Model (Overall)",
    subtitle = "Percentage of students responding positively (4 or 5 out of 5)",
    x = "Delivery Model", y = "Percentage (%)", fill = ""
  ) +
  theme_minimal(base_size = 14) + theme(legend.position = "bottom")

ggsave("../results/objective_1/figures/acceptability_general_bar_chart.png", p_accept_general, width = 10, height = 6, dpi = 300)


# --- PLOT 2: DETAILED COMPARISON (Faceted by Dataset) ---
plot_data_detailed <- dataset_acceptability %>%
  pivot_longer(cols = c(Pct_Found_Helpful, Pct_Would_Recommend), names_to = "Metric", values_to = "Percentage") %>%
  mutate(Metric = ifelse(Metric == "Pct_Found_Helpful", "Found Program Helpful", "Would Recommend to a Friend"))

p_accept_detailed <- ggplot(plot_data_detailed, aes(x = delivery_group, y = Percentage, fill = Metric)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = paste0(Percentage, "%")), position = position_dodge(width = 0.8), vjust = -0.5, fontface = "bold", size = 4) +
  scale_fill_manual(values = c("Found Program Helpful" = "#9A8EE6", "Would Recommend to a Friend" = "#132964")) +
  scale_y_continuous(limits = c(0, 115), breaks = seq(0, 100, 20)) +
  facet_wrap(~ dataset) +
  labs(
    title = "Program Acceptability by Dataset & Delivery Model",
    subtitle = "Percentage of students responding positively (4 or 5 out of 5)",
    x = "Delivery Model", y = "Percentage (%)", fill = ""
  ) +
  theme_minimal(base_size = 14) + 
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 15, hjust = 1))

ggsave("../results/objective_1/figures/acceptability_detailed_bar_chart.png", p_accept_detailed, width = 12, height = 6, dpi = 300)

cat("✓ Both general and detailed acceptability bar charts saved.\n\n")


#-------------------------------------------------------------------------------
# 5. EXPORT TO WORD DOCUMENT
#-------------------------------------------------------------------------------

cat("5. EXPORTING TO WORD\n")
cat(strrep("-", 80), "\n")

doc <- read_docx() %>%
  body_add_par("MHDP - Program Acceptability & Implementation Fidelity", style = "heading 1") %>%
  
  body_add_par("1. Overall Acceptability (All Interventions)", style = "heading 2") %>%
  body_add_flextable(flextable(overall_acceptability) %>% autofit()) %>%
  body_add_par("") %>%
  
  body_add_par("2. Acceptability by Delivery Model (General)", style = "heading 2") %>%
  body_add_par("Tests if students found the intervention equally helpful when delivered by external partners overall.", style = "Normal") %>%
  body_add_flextable(flextable(group_acceptability) %>% autofit()) %>%
  body_add_par("") %>%
  
  body_add_par("3. Acceptability by Dataset and Delivery Model (Detailed)", style = "heading 2") %>%
  body_add_flextable(
    flextable(dataset_acceptability) %>% 
      merge_v(j = "dataset") %>% 
      valign(j = "dataset", valign = "top") %>% 
      autofit()
  )

output_path <- "../results/objective_1/tables/Acceptability_Results.docx"
print(doc, target = output_path)

cat("✓ Word document successfully saved to:", output_path, "\n")
cat(strrep("=", 80), "\n")
cat("ACCEPTABILITY SCRIPT COMPLETE\n")
cat(strrep("=", 80), "\n\n")

################################################################################
# END OF SCRIPT
################################################################################