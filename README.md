# MHDP–Africa: Mental Health Data   Prize Project

## Project Title: Enhancing Youth Mental Health: Data-Driven Insights and Digital Tools for Scalable Interventions

## Overview

This repository contains the code, metadata, and analysis outputs for the MHDP–Africa project titled Enhancing Youth Mental Health: Data-Driven Insights and Digital Tools for Scalable Interventions. Additionally, this repository links to the overarching goal of the project: utilizing these data-driven insights to build the key [ShamiriOS](https://github.com/Shamiri-Institute/digitalhub) and [ShamiriAI—digital](https://huggingface.co/shamiri-ai/models) digital infrastructure designed to enable precision mental healthcare delivery at scale.


***What is the research about?***

While the broader role of the project is to develop digital ecosystems for mental health logistics, this specific repository houses the secondary data analysis and insights generation. It synthesizes longitudinal survey data (representing over 7,000 participants) from major clinical trials conducted between 2018 and 2023 (Shamiri Datasets A, B, and C; Shamiri 1.0, 2.0, and 3.0). The insights generated here identify which low-touch, character-strength interventions work best, for whom they work best, and under what implementation conditions they remain effective, directly serving as the foundational clinical logic for the ShamiriAI matching algorithms.

***What problem does it address?***

This project addresses two deeply connected public health challenges in Sub-Saharan Africa:
The Clinical Treatment Gap: The high prevalence of youth depression and anxiety combined with a severe lack of psychiatric resources. This requires robust, meta-analytic evidence to prove that brief, lay-provider interventions (task-shifting) are highly effective and scalable in real-world school settings.
The Infrastructure Gap: The lack of affordable, adaptable digital platforms for community-based mental health delivery. Currently, task-shifted care relies on fragmented, error-prone tools (spreadsheets, WhatsApp). This research powers shamiriOS, replacing these ineffective workflows with a unified platform for supervision, triage, and real-time clinical monitoring.

***What are the key objectives and expected outputs?***
The primary objectives of the Data Insights (Repository) are to:
Objective 1 (Implementation Effectiveness): Apply the "Four Enoughs" framework to test intervention scalability. Generate multi-level models comparing the efficacy of Shamiri Hubs vs. external NGO Partner delivery across different geographic and socioeconomic school contexts.
Objective 2 (Individual Differences): Conduct classical moderator analyses (LMMs) and Machine Learning tree-based moderator analyses (Recursive Partitioning) to automatically detect distinct youth subgroups and predict personalized treatment outcomes based on age, gender, and baseline severity.
Output: Automated, publication-ready statistical tables and high-resolution trajectory visualizations (.docx, .png).
The primary objectives of the Broader Digital Project are to:
Precision Care (ShamiriAI): Translate these clinical insights into algorithms to drive targeted therapeutic interventions and optimize patient-provider matching.
Delivery & Support (shamiriOS): Deploy a digital hub to connect implementers, providers, and youths, utilizing LLMs to automate supervision and measure treatment fidelity.
This project aims to advance mental health research by generating high-quality, FAIR-aligned, reproducible evidence across Africa.

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
│   ├── objective_1/
│   │   ├── figures/
│   │   ├── tables/
│   │   └── models/
│   └── objective_2/
│       ├── figures/
│       ├── tables/
│       └── models/
├── metadata/
├── LICENSE
└── CITATION.cff
```

### Folder Descriptions
- datasets/raw/ → Original, unmodified Excel datasets (Do not edit).
- datasets/processed/ → Cleaned, standardized, and merged longitudinal datasets (mhdp_data.xlsx, mhdp_data_2.xlsx).
- scripts/ → Modular R code handling data preparation, descriptive statistics, and advanced LMM / Machine Learning modeling for Objectives 1 and 2.
- results/ → Separated by Objective, containing outputs including Word documents (.docx), Excel summaries (.xlsx), and generated trajectory/decision-tree plots (.png).
- metadata/ → Documentation including codebooks and metadata links.

  
### Data Description
- Type of Data: Quantitative, longitudinal self-report survey data from RCTs and real-world dissemination trials.
- Unit of Analysis: Individual youths (Aged 12–20).
- Geographic Coverage: Kenya (Nairobi, Kiambu, Makueni, Kisumu, and Nakuru counties).
- Time Period: 2018 – 2023.
- Summary: The dataset consists of comprehensive longitudinal observations tracking students from baseline through active intervention (Weeks 2, 4) and long-term - follow-up (up to 3 years / 156 weeks). Core clinical metrics include the PHQ-8 (Depression) and GAD-7 (Anxiety), supplemented by wellbeing and social support scales (SWEMWBS, MSPSS).

  
### Reproducibility Instructions
1. Place the raw datasets in datasets/raw/.
2. Ensure the cleaned datasets (mhdp_data.xlsx, mhdp_data_2.xlsx) are placed in datasets/processed/.
3. Run the R analysis scripts located in scripts/ in sequential order:
4. 01...R (Generates sample summaries, prevalence rates, and base plots)
5. 02...R (Calculates Cronbach's alpha, baseline correlations, and t-tests/effect sizes)
6. 03...R (Runs models)
7. 04...R (Runs Acceptability)
All tables, Word documents, and figures will automatically save to their respective Objective folders inside results/.



### Requirements
- Software: R (4.5.2), RStudio
- Packages include: tidyverse, readxl, writexl, psych, lsr, meta, lme4, lmerTest, ggeffects, partykit (for ML trees), officer, flextable, broom.mixed, and conflicted.

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
