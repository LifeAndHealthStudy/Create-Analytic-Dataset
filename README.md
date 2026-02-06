# Life + Health Study: Analytic Dataset [![](<https://img.shields.io/badge/Dataverse DOI-10.7910/DVN/QNJV0A-orange>)](https://doi.org/10.7910/DVN/QNJV0A)

## Grant Information

-   Project Title: Advancing novel methods to measure and analyze multiple types of discrimination for population health research
-   Project Number: 5R01MD012793-05
-   Opportunity Number: PA-18-484
-   Contact PI Project Leader: KRIEGER, NANCY
-   Awardee Organization: HARVARD SCHOOL OF PUBLIC HEALTH
-   Project Start Date: 19-June-2019
-   Project End Date: 31-January-2026
-   For more information please visit: [https://reporter.nih.gov/project-details/10363700](https://reporter.nih.gov/search/abvMBW7oBkWqKKJRHntaZQ/project-details/10551734)

## Study Description

The Life + Health Study was a cross-sectional population-based study designed to advance novel methods to measure and analyze discrimination for population health research using implicit and self-report measures of discrimination. Our study population comprised 699 participants recruited between May 28, 2020 and August 4, 2022 from three community health centers in Boston, Massachusetts: Fenway Health (FH), Mattapan Community Health Center (MCHC), and Harvard Street Neighborhood Health Center (HSNHC). These community health centers were selected to ensure adequate representation participants in relation to the types of discrimination being studied. Participants were eligible if they had visited one of the health centers in the last two years; were born in the US to ensure comparability in potential exposure to discrimination in the US; and were ages 25–64 years.

## Analytic Dataset Description

The analytic dataset used in the Life + Health study consists of one record per participant and includes both primary variables (collected via study screeners or questionnaires) and derived variables (constructed using study data). Variables represent sociodemographic characteristics; primary exposures of interest, including implicit and explicit self-report measures of discrimination; primary study outcomes (psychological distress, sleep disturbance, sleep impairment, and sleep duration); and several additional variables of analytic interest.

A complete description of all variables, scientific justification, and value ranges is provided in the accompanying [Life+Health Public Data Dictionary](data_dictionary/L+H_data_dictionary.xlsx)

## Data Access

The Life + Health analytic dataset is not publicly available and is only accessible to approved users under applicable data use agreements. Researchers interested in accessing the de-identified analytic dataset, must first apply for access through the [Life + Health Data Access Application](https://harvard.az1.qualtrics.com/jfe/form/SV_0dhd6tjArmB9fKK). Data access is granted only with the explicit approval of both scientific and community reviewers, ensuring that your research supports—not compromises—the priorities and values of the participant communities. All approved projects require completion of a Data Use Agreement (DUA) with Harvard before data release.

For a summary of data access and terms of use please click [here](L+H_ANALYTIC_DATASET_ACCESS+TERMS_OF_USE.pdf)

## Repository Intended Use

This repository is intended to support transparency and reproducibility by documenting variable construction and analytic decisions. It is **not** designed to recreate the restricted analytic dataset or serve as a fully executable pipeline outside of the secure analysis environment. For this reason, certain functions and file references related to raw data import are intentionally omitted from this repository due to data use restrictions and human subjects protections. Some function names and analytic logic are retained to allow readers to follow the workflow used to construct analytic measures.

### Repository Structure

Create-Analytic-Dataset/

📁**├── R**/ \# Code used to construct analytic variables

```         
  │ ├── 01_height_weight_and_medications.R
  
  │ ├── 02_create_residential_absms.R
  
  │ ├── 03_construct_discrim_measures.R
  
  │ ├── 04_construct_k6.R
  
  │ ├── 05_create_political_concern_scale.R
  
  │ ├── 06_create_social_desirability_scale.R
  
  │ ├── 07_classify_sleep_medications.R
  
  │ ├── 08_construct_sleep_measures.R
  
  │ ├── 09_helpers.R
  
  │ ├── 10_create_merged_analytic_dataset.R
  
  │ └── README.md
```

📁**├── data_dictionary**/ \# Documentation of variable definitions and values

```         
│ ├── L+H_data_dictionary.xlsx
```

📁**├── images/** \# Figures and images used in documentation

📁**├── med_classifications**/ \# Supporting data files used by R scripts

```         
│ ├── L+H_EMR_medication_classifications.xlsx
```

📁**├── study_materials**/ \# Survey instruments and materials

```         
│ ├── L+H_Participant_Resource_List.pdf

│ ├── L+H_Participant_Screener.pdf

│ ├── L+H_Qualtrics_Survey_Instrument.pdf

│ ├── LICENSE

│ └── README.md
```

**├── .gitignore**/

**├── Create-Analytic-Dataset.Rproj**/

**├── LICENSE**/ **├── L+H_ANALYTIC_DATASET_ACCESS+TERMS_OF_USE.pdf**/ \# Overview of Data Access & Terms of Use **└── README.md** \# Study material sub-folder overview

## License

This repository uses **multiple licenses**:

-   **Code** in the `R/` directory is licensed under the **MIT License**.
-   **Study materials and survey instruments** in `study_materials/` are licensed under the **Creative Commons Attribution-NonCommercial 4.0 International License (CC BY-NC 4.0)**.

See the `LICENSE` file and the `study_materials/LICENSE.txt` file for details.

## Citation
If you use code or sudy materials from this repository, please cite it as follows:

Krieger, Nancy; Chen, Jarvis; Reisner, Sari; LeBlanc, Merrily; Johnson, Nykesha; Matthews, Margaret; Marini, Maddalena; Mayer, Kenneth; Bright, Donna; Oendari, Apriani; Valdez, Guale; Khan, Tanveer; Waterman, Pamela; Testa, Christian, 2026, "Life + Health Study: Advancing novel methods to measure and analyze multiple types of discrimination for population health research (R01 MD012793, 2019–2026)", https://doi.org/10.7910/DVN/QNJV0A, Harvard Dataverse.
