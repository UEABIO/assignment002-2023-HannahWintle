# Size changes in butterflies with temperature

This solo project focused on creating a letter-style report on the universal ecological responses to climate change in univoltine butterflies, Hesperia comma. The forewing length, sex of the specimen, year of the dated museum specimen, mean temperature in June of that year (celsius) and total rainfall (mm) for that year are the variables looked at for this project. Multiple linear regressions were produced and the data was interpreted using statistical analysis.

## R programming language: version R 4.3.0

## Packages used:
- library(tidyverse)
- library(janitor)
- library (lubridate)
- library(ggpubr)
- library(here)
- library(kableExtra)
- library(broom.helpers)
- library(GGally)
- library(emmeans)
- library(performance)
- library(patchwork)
- library(broom)
- library(knitr)

## Project description
This is a solo project assignment for module BIO 5023Y - Data Science for Biologists. This assignment requires data cleaning, exploratory analysis, data visualisation & statistical model building and inference.

## Data
univoltine_butterfly.csv

| Variable| Definition|
|----|----|
| Year| year of dated museum specimen|
| forewing length| length (mm) of measured forewing|
| sex| sex of butterfly specimen|
| JUN_mean| mean temperature in June of that year (celsius)|
| rain_JUN| total rainfall(mm) for that year|




