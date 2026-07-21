# Malaria Insights Hub

An interactive surveillance dashboard and publication-standard report analysing the Kenya Malaria Indicator Survey (MIS) 2020. This repository contains the R Shiny application source code and Quarto reporting documents necessary to reproduce the analysis of malaria prevalence, intervention coverage, and socio-economic determinants in Kenya.

## 🚨 Important Data Governance Notice
**The raw MIS 2020 dataset (`KEPR81FL.DTA`) is NOT included in this repository.**

In compliance with the DHS Program Authorization Letter (Ref: SPA_225470), raw micro-data cannot be redistributed or shared publicly. Only aggregated and anonymised findings are presented in the final outputs. If you wish to reproduce this analysis, you must independently request and download the 2020 Kenya MIS Person Recode dataset from the [DHS Program website](https://dhsprogram.com/).

## Project Structure

```
Malaria_Insights_Hub/
├── data/               # Empty directory for the KEPR81FL.DTA dataset (gitignored)
├── docs/               
│   ├── Malaria_Report_Publication.pdf  # Final rendered publication report
│   ├── Malaria_Report_Publication.qmd  # Quarto publication report source
│   ├── references.bib                  # BibTeX references for the report
│   └── AuthLetter_SPA_225470.pdf       # DHS Authorization Letter
├── src/                
│   ├── app.R                           # Main Shiny application script
│   └── fct_parser.R                    # Helper function for parsing STATA labels
├── .gitignore          # Ignores sensitive data and OS files
├── DATA_DICTIONARY.md  # Definitions for all variables used in the analysis
├── LICENSE             # Project license
└── README.md           # This file
```

## Features

- **Epidemiological Dashboard (`src/app.R`):** An R Shiny web application providing sub-national, interactive filtering of malaria indicators. 
- **Publication Report (`docs/Malaria_Report_Publication.pdf`):** An academic-style report presenting the findings, structured with an abstract, literature review, methodology, and results (including heatmaps, regression analysis, and gap analyses).
- **Small-Cell Suppression:** Both the dashboard and the report dynamically suppress estimates based on unweighted counts (n < 25) to protect privacy.
- **DHS Weighting:** All point estimates appropriately apply DHS complex survey weights (`hv005/1000000`).

## Setup and Reproduction

### Prerequisites
- R (version 4.3.0 or higher)
- RStudio (recommended for Quarto)
- `quarto` CLI installed on your system.

### Installation

1. Clone this repository:
   ```bash
   git clone https://github.com/TrushalPrevail/Malaria-Project.git
   cd Malaria-Project
   ```

2. Install the required R packages. Run the following in your R console:
   ```R
   install.packages(c("shiny", "bslib", "dplyr", "tidyr", "stringr", "haven", "ggplot2", "plotly"))
   ```

3. **Obtain the dataset (Crucial Step):**
   The raw dataset is restricted. To reproduce this analysis, you must request it directly from the DHS Program:
   - Go to the [DHS Program Website](https://dhsprogram.com/data/new-user-registration.cfm) and register for a new user account.
   - Once registered, log in and create a new project request. Describe your research/study purpose.
   - Request access to the **Kenya 2020 Malaria Indicator Survey (MIS)** datasets.
   - Wait for approval (usually takes 1–3 business days).
   - Once approved, download the **Stata (.DTA) format** of the **Person Recode** dataset.
   - Extract the downloaded ZIP file, locate `KEPR81FL.DTA`, and place it exactly inside the `data/` folder of this repository.

### Running the Dashboard
Navigate to the `src/` directory in RStudio, open `app.R`, and click **Run App**, or execute:
```R
shiny::runApp("src/app.R")
```

### Rendering the Publication Report
Navigate to the `docs/` directory and render the `.qmd` file to a PDF:
```bash
quarto render docs/Malaria_Report_Publication.qmd
```
*(Ensure you have a working LaTeX distribution installed, such as TinyTeX, to generate the PDF).*

## License
The source code and documentation in this repository are licensed under the [Creative Commons Attribution-NonCommercial 4.0 International (CC-BY-NC 4.0)](https://creativecommons.org/licenses/by-nc/4.0/) license. 

**Note:** This license applies only to the code and written reports created for this project, NOT to the underlying DHS dataset.
