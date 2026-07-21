# Data Dictionary: MIS 2020 (KEPR81FL.DTA)

This data dictionary outlines the specific variables extracted and derived from the 2020 Kenya Malaria Indicator Survey Person Recode dataset (`KEPR81FL.DTA`) for use in the Malaria Insights Hub dashboard and publication report.

## Raw Source Variables

| DHS Code | Label/Description | Variable Type | Domain |
|:---|:---|:---|:---|
| `hv005` | Sample weight (divide by 1,000,000 to use) | Integer | Weighting |
| `hv024` | Region (Province level) | Categorical (8 levels) | Geography |
| `hv025` | Type of place of residence | Binary (Urban / Rural) | Geography |
| `hv104` | Sex of household member | Binary (Male / Female) | Demographics |
| `hv105` | Age of household member | Integer (0–98) | Demographics |
| `hv270` | Wealth index quintile | Ordinal (Poorest to Richest) | Demographics |
| `shcounty` | County | Categorical (47 levels) | Geography |
| `shzone` | Malaria endemicity zone | Categorical (5 levels) | Geography |
| `hml32` | Final result of malaria from blood smear | Categorical (Positive/Negative) | Diagnostics |
| `hml32a` | Plasmodium falciparum observed | Categorical (Yes/No) | Diagnostics |
| `hml32b` | Plasmodium malariae observed | Categorical (Yes/No) | Diagnostics |
| `hml32c` | Plasmodium ovale observed | Categorical (Yes/No) | Diagnostics |
| `hml32d` | Plasmodium vivax observed | Categorical (Yes/No) | Diagnostics |
| `hml35` | Result of malaria rapid test (RDT) | Categorical (Positive/Negative) | Diagnostics |
| `hv227` | Household has mosquito net for sleeping | Categorical (Yes/No) | Vector Control |
| `hml10` | Slept under an insecticide-treated net (ITN) | Categorical (Yes/No) | Vector Control |
| `hml12` | Type of mosquito net slept under | Categorical | Vector Control |
| `hv213` | Main material of the floor | Categorical | Household Feature |
| `hv214` | Main material of the exterior walls | Categorical | Household Feature |
| `hv215` | Main material of the roof | Categorical | Household Feature |
| `hv201` | Source of drinking water | Categorical | Household Feature |
| `hv205` | Type of toilet facility | Categorical | Household Feature |
| `hv206` | Household has electricity | Categorical (Yes/No) | Household Feature |
| `hv207` | Household has a radio | Categorical (Yes/No) | Household Feature |

## Derived Analysis Variables

These variables are engineered within the R scripts (`app.R` and `Malaria_Report_Publication.qmd`) prior to statistical modelling and visualisation.

| Variable Name | Definition & Logic | Data Type |
|:---|:---|:---|
| `Weight` | True sample weight: `hv005 / 1000000` | Continuous Float |
| `Malaria_Pos` | Composite microscopy positivity. `1` if *any* of `hml32`, `hml32a`, `hml32b`, `hml32c`, or `hml32d` contains "Positive" or "Yes". `0` otherwise. | Binary Integer (0, 1) |
| `Slept_ITN` | Individual ITN usage. `1` if `hml12` indicates they slept under a treated net (ITN). `0` if they did not sleep under a net or only used untreated nets. `NA` if missing. | Binary Integer (0, 1, NA) |
| `Has_Net` | Household net ownership. `1` if `hv227` is "Yes". `0` if "No". | Binary Integer (0, 1, NA) |

## Data Governance Note
The data described above is subject to the DHS Program's strict data use restrictions. While this dictionary is public, the dataset itself is restricted and requires authorization (via ICF Institutional Review Board) to access.
