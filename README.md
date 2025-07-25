# Machine learning-based short-term forecasting of COVID-19 hospital admissions using routine hospital patient data

This repository contains the code of the analysis of the paper "Machine learning-based short-term forecasting of COVID-19 hospital admissions using routine hospital patient data" by Martin Wohlfender et al. A preprint is available on [medRxiv](https://www.medrxiv.org/content/10.1101/2025.05.21.25328056v1).

## Licences
* The code stored in this repository is available for download and use under the [GNU Affero General Public License Version 3](https://www.gnu.org/licenses/agpl-3.0.html).
* Electronic health records data has been obtained from the Insel Data Science Center [IDSC](https://idsc.io/de/). Some of this data is made available in this repository in aggregated and anonymized form for download and use under the creative commons license [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/).
* Wastewater data has been retrieved from the Swiss Federal Institute of Aquatic Science and Technology [eawag](https://sensors-eawag.ch/sars/laupen.html) under the creative commons license [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/).

## Overview of content of repository
* The aim of this repository is to provide all necessary code (written in R and Python) to reproduce the statistical analysis of the paper cited above.
* The whole R code is structured in an R-project (`hospital_admission_forecasting.Rproj`).
* Before running any other R file, the file `setup.R` (contained in folder `R`) needs to be run. In this file, all paths to data and results files are defined (with respect to the path of `hospital_admission_forecasting.Rproj`).
* R files are grouped by topic (data processing, creating plots, ...).
* All models except last observation carried forward and linear regression were run on the high performance computing cluster of the University of Bern, [UBELIX](https://ubelix.hpc.unibe.ch).


