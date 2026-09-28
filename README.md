# College Student Retention Analysis in R

## Overview
This repository contains a data-driven statistical analysis evaluating factors that influence student retention and second-year enrollment return probabilities using RStudio.

![College Retention Dashboard](executive_retention_dashboard.png)

## Key Highlights
* **Exploratory Data Analysis:** Processed institutional enrollment data in R (`tidyverse`, `readxl`), establishing baseline return rates (66%) and median GPA thresholds (2.74).
* **Logistic Regression:** Modeled retention probability (`Return ~ GPA + Program`) to isolate critical predictors of academic persistence.
* **Strategic Insights:** Binned student metrics into actionable GPA performance tiers to enable targeted intervention strategies for at-risk cohorts.

## Repository Contents
* `college_retention_analysis.R` - Data cleaning, statistical modeling, and visualization scripts.
* `executive_retention_dashboard.png` - High-resolution executive summary dashboard.

## R Packages
`tidyverse`, `readxl`, `ggplot2`, `patchwork`, `glm`
