# MHDP–Africa: Mental Health Data   Prize Project

## Project Title: Enhancing Youth Mental Health: Data-Driven Insights and Digital Tools for Scalable Interventions

## Overview

This repository contains the code, metadata, and analysis outputs for the MHDP–Africa project titled **Enhancing Youth Mental Health: Data-Driven Insights and Digital Tools for Scalable Interventions**. Additionally, these repository links to t

This repository contains the code, metadata, and analysis outputs for the MHDP–Africa project titled Enhancing Youth Mental Health: Data-Driven Insights and Digital Tools for Scalable Interventions. Additionally, this repository links to the overarching goal of the project: utilizing these data-driven insights to build the key [ShamiriOS](https://github.com/Shamiri-Institute/digitalhub) and [ShamiriAI—digital](https://huggingface.co/shamiri-ai/models) infrastructure designed to enable precision mental healthcare delivery at scale.


***What is the research about?***

While the broader and key role of the project is to develop digital ecosystems for mental health logistics, this specific repository houses the secondary data analysis and insights generation. It evaluates the effectiveness of a school-based, lay-provider mental health intervention ("Shamiri") compared to Treatment As Usual (TAU). It synthesizes longitudinal survey data (over 6,000 participants) from three distinct clinical trials: dataset A (2022), dataset B (2023), and dataset C (2023). The insights generated here identify which interventions work best, for whom, and under what conditions, serving as the foundation for the ShamiriAI matching algorithms.

***What problem does it address?***

This project addresses two deeply connected problems in Sub-Saharan Africa:
The Clinical Gap: The high prevalence of depression and anxiety among adolescents, requiring robust, meta-analytic evidence to prove that brief, lay-provider interventions are highly effective and scalable.
The Infrastructure Gap: The lack of affordable, adaptable digital platforms for community-based mental health delivery. Currently, task-shifted care relies on fragmented tools (spreadsheets, WhatsApp) prone to data loss. This research powers shamiriOS, replacing these ineffective workflows with a unified platform for supervision, attendance, triage, and real-time clinical monitoring.

***What are the key objectives and expected outputs?***
- The primary objectives of the Data Insights (Repository) are to:
- Standardize and merge psychometric data (PHQ-8, GAD-7, SWEMWBS) across three distinct trials to create a harmonized dataset.
- Establish baseline prevalence rates for clinical depression and anxiety across 5 Kenyan counties.
- Calculate within-group and between-group effect sizes (Cohen's d) across multiple timepoints (up to 56 weeks).
- Conduct random-effects meta-analyses and longitudinal Linear Mixed Models (LMMs) to understand individual differences in intervention outcomes (by age and gender).
- Output automated, publication-ready statistical tables and high-resolution trajectory visualizations.
- The primary objectives of the Broader Digital Project are to:
- Translate these clinical insights into ShamiriAI to drive targeted therapeutic interventions and optimize patient-provider matching.
- Deploy shamiriOS to connect implementers, providers, and youths via real-time dashboards, streamlining community-based mental healthcare delivery.

The project aims to advance mental health research by generating high-quality, FAIR-aligned, reproducible evidence across Africa.

### Project Information
- Project Name: MHDP - Enhancing Youth Mental Health: Data-Driven Insights and Digital Tools for Scalable Interventions
- Team Name: Shamiri 
- Repository Name: mhdp-shamiri-analysis
- Principal Investigator: Tom Osborn
- Tech Lead: Shadrack Lilan
- Institution(s): Shamiri
- Contact Email: osborn@shamiri.institute, shadrack.lilan@shamiri.institute
  
### Repository Structure

This repository follows the standardized MHDP-Africa structure:
```
├── datasets/
│   ├── raw/
│   └── processed/
├── scripts/
├── results/
│   ├── figures/
│   ├── tables/
│   └── models/
├── metadata/
├── LICENSE
└── CITATION.cff
```

### Folder Descriptions
- data/raw/ → Original, unmodified Excel datasets (Templeton, Wellsprings, Anansi). (Do not edit)
- data/processed/ → Cleaned, standardized, and merged longitudinal dataset (mhdp_data.xlsx).
- scripts/ → R code and modular analysis workflows.
- results/ → Outputs including Word documents (.docx), Excel summaries (.xlsx), and generated trajectory plots (.png).
- metadata/ → Documentation including codebooks and metadata summaries.

  
### Data Description
- Type of Data: Quantitative, longitudinal self-report survey data.
- Unit of Analysis: Individual students (Adolescents).
- Geographic Coverage: Kenya (Kiambu, Kisumu, Makueni, Nairobi, and Nakuru counties).
- Time Period: 2022 – 2023.
- Summary: The dataset consists of ~29,000+ observations tracking students from baseline (Week 0) through active intervention (Weeks 2, 4) and longitudinal follow-up (Weeks 8, 28, 40, 52). Core metrics include the PHQ-8 (Depression), GAD-7 (Anxiety), and SWEMWBS (Wellbeing).

  
### Reproducibility Instructions
1. Place the raw datasets in data/raw/.
2. Ensure the cleaned dataset mhdp_data.xlsx is placed in data/processed/ (or generated via the data cleaning script).
3. Run the analysis scripts in scripts/ in sequential order:
    - 01_mhdp_descriptives_and_plots.R (Generates sample summaries, prevalence rates, and base plots)
    - 02_mhdp_psychometrics_and_stats.R (Calculates Cronbach's alpha, correlations, and t-tests/effect sizes)
    - 03_mhdp_meta_analysis_and_lmms.R (Runs meta-analyses, piecewise LMMs, and attrition regressions)
4. All tables, Word documents, and figures will automatically save to the results/ folder.



### Requirements
- Software: R (4.5.2), RStudio
- Packages: The scripts utilize the pacman package manager to automatically install missing dependencies. Key packages include: tidyverse, readxl, writexl, psych, lsr, meta, lme4, lmerTest, ggeffects, officer, flextable, broom.mixed, and conflicted.


### Metadata and Documentation
See `metadata/` for:
- Codebook: Detailed variable definitions, scoring logic for psychometrics (PHQ-8, GAD-7, SWEMWBS), and demographic categorizations.
- Metadata document: Protocol and trial alignment details.


### Data Access
- Access Level: [Restricted / Controlled Access]
- Instructions: De-identified data necessary to reproduce these analyses is available [upon reasonable request / in the data/processed folder]. Due to the sensitive nature of adolescent mental health data, raw identifying data is strictly withheld.
- License: May be licensed under a CC BY license on publication 


### Related Resources
- OSF: https://osf.io/4c3sk/overview, https://osf.io/nu9tz/overview 
- Parent Repo: https://github.com/Shamiri-Institute/digitalhub
- Publications: https://preprints.jmir.org/preprint/95063], https://huggingface.co/shamiri-ai/models

### Citation
- Provided once manuscript is printed.
  

### License
- Code: [MIT License](LICENSE)
- Data:
    - *Raw data:* Restricted – available upon request. Access is governed by a Data Use Agreement (DUA). To request access, [contact us](rachael.kilonzo@shamiri.institute) or [open a request issue](../../issues).
    - *Processed data:* [![License: CC BY 4.0](https://img.shields.io/badge/License-CC%20BY%204.0-lightgrey.svg)](https://creativecommons.org/licenses/by/4.0/) Free to use with attribution.


### Acknowledgements
- MHDP-Africa: For funding, framework standardization, and support.
- APHRC: African Population and Health Research Center.
- Shamiri Institute Data Team: For data collection and curation across the Templeton, Wellsprings, - and Anansi trials.


### Contact
- Name: Rachael Kilonzo, Shadrack Lilan,
- Email: rachael.kilonzo@shamiri.institute, shadrack.lilan@shamiri.institute


### Notes
- Do not modify raw data inside data/raw/.
- All steps must be reproducible. Ensure the working directory is set to the source file location prior to executing R scripts.
