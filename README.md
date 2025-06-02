# Machine learning-based short-term forecasting of COVID-19 hospital admissions using routine hospital patient data

This repository contains the code of the analysis of the paper "Machine learning-based short-term forecasting of COVID-19 hospital admissions using routine hospital patient data" by Martin Wohlfender et al. A preprint is available on [medRxiv](https://www.medrxiv.org/content/10.1101/2025.05.21.25328056v1).

## (A) Overview of content of repository
The aim of this repository is to provide all necessary code (written in R and Python) to reproduce the statistical analysis of the paper cited above.

## (B) Remarks
* The whole R code is structured in an R-project (`hospital_admission_forecasting.Rproj`).
* Before running any other R file, the file `setup.R` (contained in folder `R`) needs to be run. In this file, all paths to data and results files are defined (with respect to the path of `hospital_admission_forecasting.Rproj`).
* R files are grouped by topic (data processing, creating plots, ...).
* All models except last observation carried forward and linear regression were run on the high performance computing cluster of the University of Bern, [UBELIX](https://ubelix.hpc.unibe.ch).
