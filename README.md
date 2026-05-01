# EVS Data Explorer

A Shiny app for exploring European Values Study (EVS) data, built for SURV 605 / Modern Workflows in Data Science.

## App

**Live app:**
https://alyssa-hyde.shinyapps.io/evs-shiny-app/

## Overview

This app allows users to interactively explore two attitudinal outcomes from the EVS dataset:

* **v72** – Child suffers when the mother works
* **v80** – Job should be given to a national

Users can:

* Filter by country
* Select outcomes
* Add controls (sex, education)
* Adjust the age polynomial
* Download a dynamic HTML report

## Structure

```
evs-shiny-app/
├── app.R          # Main Shiny application
├── report.Rmd     # Dynamic HTML report template
├── README.md
└── data/
    └── evs_clean.rds
```

## How to Run Locally

```r
# Install dependencies
install.packages(c(
  "shiny", "dplyr", "ggplot2", "broom",
  "bslib", "rmarkdown", "gridExtra"
))

# Run the app
shiny::runApp("app.R")
```

```


