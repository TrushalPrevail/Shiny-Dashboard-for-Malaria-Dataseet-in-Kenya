# Malaria Insights Hub

## General Details
* **Author**: Trushal Hirani
* **Start Date**: February 2026
* **Live Dashboard**:  https://trushalhirani.shinyapps.io/MalariaProject/

## Project Structure
This project bridges the gap between raw epidemiological data (from the 2020 Malaria Indicator Survey) and actionable public health insights. 

### Key Files
1. **`app.R`**: 
   Contains the detailed structure of the Shiny User Interface (UI) and the backend Server logic. It handles data filtering, variable decoding, layout architecture, and visualizations.
2. **`Malaria_Report.qmd`**: 
   Contains the technical and epidemiological implementation details, the mathematical formulations, and the resulting insights. It serves as the comprehensive report of the findings.
3. **`fct_parser.R`**: 
   A custom metadata parser script that maps numeric raw codes to human-readable labels using Regex on the Stata description text files.
4. **Data Files**: 
   * `KEPR81FL.DTA` (Raw dataset - **Not Shared Publicly**)
   * `MIS_2020_PR.txt` (Variable descriptions)
   * `label_define.txt` (Value labels)

## Data Confidentiality
**Important Notice**: The primary dataset used in this project (`KEPR81FL.DTA`) contains sensitive health and demographic information.The dashboard and reports aggregate and anonymize this information to comply with privacy protocols. 

## Technical Details
* **Language**: R
* **Framework**: Shiny, `bslib`
* **Data Manipulation**: `dplyr`, `tidyr`, `stringr`
* **Visualization**: `ggplot2`, `plotly`
* **Machine Learning**: Base R `glm` (Logistic Regression) to evaluate overall socio-economic feature importance using Deviance tests.
