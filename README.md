# MHDP–Africa: Mental Health Data Project

## Project Title: Evaluating the Longitudinal Effectiveness of the Shamiri Mental Health Intervention for Kenyan Adolescents

## Overview

This repository contains the code, metadata, and analysis outputs for the MHDP–Africa project titled **Enhancing Youth Mental Health: Data-Driven Insights and Digital Tools for Scalable Interventions** conducted by the Shamiri Institute.

What is the research about?
This project evaluates the effectiveness of a school-based, lay-provider mental health intervention ("Shamiri") compared to Treatment As Usual (TAU). It synthesizes longitudinal survey data from three distinct clinical trials: Templeton 2 (2022), Wellsprings (2023), and Anansi Trial 1 (2023).
What problem does it address?
It addresses the high prevalence of depression and anxiety among adolescents in Sub-Saharan Africa by providing robust, meta-analytic evidence on the scalability and long-term efficacy of brief, lay-provider interventions.
What are the key objectives and expected outputs?
The primary objectives are to:
Standardize and merge psychometric data (PHQ-8, GAD-7, SWEMWBS) across three distinct trials.
Establish baseline prevalence rates for clinical depression and anxiety across 5 Kenyan counties.
Calculate within-group and between-group effect sizes (Cohen's d) across multiple timepoints (up to 56 weeks).
Conduct random-effects meta-analyses (overall and by age/gender subgroups) and longitudinal Linear Mixed Models (LMMs) to determine symptom trajectories.
Outputs include automated Word documents with publication-ready statistical tables and high-resolution trajectory visualizations.
The project aims to advance mental health research by generating high-quality, FAIR-aligned, reproducible evidence across Africa.
Project Information
Project Name: MHDP - Shamiri Intervention Analysis
Team Name: Shamiri Institute Research Team
Repository Name: mhdp-shamiri-analysis
Principal Investigator / Lead Analyst: Rachael Kilonzo
Institution(s): Shamiri Institute
Contact Email: rachael.kilonzo@shamiri.institute
Repository Structure
This repository follows the standardized MHDP-Africa structure:
code
Text
├── data/
│   ├── raw/
│   └── processed/
├── scripts/
│   ├── 01_mhdp_descriptives_and_plots.R
│   ├── 02_mhdp_psychometrics_and_stats.R
│   └── 03_mhdp_meta_analysis_and_lmms.R
├── results/
│   ├── figures/
│   ├── tables/
│   └── models/
├── metadata/
├── LICENSE
└── CITATION.cff
Folder Descriptions
data/raw/ → Original, unmodified Excel datasets (Templeton, Wellsprings, Anansi). (Do not edit)
data/processed/ → Cleaned, standardized, and merged longitudinal dataset (mhdp_data.xlsx).
scripts/ → R code and modular analysis workflows.
results/ → Outputs including Word documents (.docx), Excel summaries (.xlsx), and generated trajectory plots (.png).
metadata/ → Documentation including codebooks and metadata summaries.
Data Description
Type of Data: Quantitative, longitudinal self-report survey data.
Unit of Analysis: Individual students (Adolescents).
Geographic Coverage: Kenya (Kiambu, Kisumu, Makueni, Nairobi, and Nakuru counties).
Time Period: 2022 – 2023.
Summary: The dataset consists of ~29,000+ observations tracking students from baseline (Week 0) through active intervention (Weeks 2, 4) and longitudinal follow-up (Weeks 8, 28, 40, 52/56). Core metrics include the PHQ-8 (Depression), GAD-7 (Anxiety), and SWEMWBS (Wellbeing).
Reproducibility Instructions
Place the raw datasets in data/raw/.
Ensure the cleaned dataset mhdp_data.xlsx is placed in data/processed/ (or generated via the data cleaning script).
Run the analysis scripts in scripts/ in sequential order:
01_mhdp_descriptives_and_plots.R (Generates sample summaries, prevalence rates, and base plots)
02_mhdp_psychometrics_and_stats.R (Calculates Cronbach's alpha, correlations, and t-tests/effect sizes)
03_mhdp_meta_analysis_and_lmms.R (Runs meta-analyses, piecewise LMMs, and attrition regressions)
All tables, Word documents, and figures will automatically save to the results/ folder.
Requirements
Software: R (>= 4.1.0), RStudio
Packages:
The scripts utilize the pacman package manager to automatically install missing dependencies. Key packages include:
tidyverse, readxl, writexl, psych, lsr, meta, lme4, lmerTest, ggeffects, officer, flextable, broom.mixed, and conflicted.
Metadata and Documentation
See metadata/ for:
Codebook: Detailed variable definitions, scoring logic for psychometrics (PHQ-8, GAD-7, SWEMWBS), and demographic categorizations.
Metadata document: Protocol and trial alignment details.
Data Access
Access Level: [Restricted / Controlled Access]
Instructions: De-identified data necessary to reproduce these analyses is available [upon reasonable request / in the data/processed folder]. Due to the sensitive nature of adolescent mental health data, raw identifying data is strictly withheld.
License: [Insert Data License, e.g., CC BY-NC 4.0]
Related Resources
OSF: [Insert link to Open Science Framework pre-registration if applicable]
Parent Repo: [Insert link to main Shamiri repo if applicable]
Publications: [Insert link to pre-print or published paper if available]
Citation
Kilonzo, R., & Shamiri Institute (2025). Evaluating the Longitudinal Effectiveness of the Shamiri Mental Health Intervention for Kenyan Adolescents. GitHub Repository. [Insert DOI/URL]
License
Code: MIT License
Data: [Insert Data License, e.g., CC BY-NC 4.0]
Acknowledgements
MHDP-Africa: For funding, framework standardization, and support.
APHRC: African Population and Health Research Center.
Shamiri Institute Data Team: For data collection and curation across the Templeton, Wellsprings, and Anansi trials.
Contact
Name: Rachael Kilonzo
Email: rachael.kilonzo@shamiri.institute
Notes
Do not modify raw data inside data/raw/.
All steps must be reproducible. Ensure the working directory is set to the source file location prior to executing R scripts.
