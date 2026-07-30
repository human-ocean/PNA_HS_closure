# High seas pocket closure

Repository containing data and code to test for the effects of a High seas closure
by the PNA. A PDF version of our preregistration file is available [here](PNA_HS_closure_prereg.pdf).

## Repository structure

The general structure of the repository is as follows. The `data/raw` folder contains
datasets that have not been modified by our code. These include the data downloaded
from the [WCPFC Public Domain Aggregated Catch/Effort data download page](https://www.wcpfc.int/wcpfc-public-domain-aggregated-catcheffort-data-download-page),
as well as vector files for Exclusive Economic Zones and Marine Protected Areas, for example.
Our `scripts` folder contains all our code. Individual R scripts are divided into three main
categories, based on their main objective (to process data, analyze data, or create content for
manuscripts / slides). The `results` folder then includes fitted models, tables, and figures,
most of which are produced by scripts in `scripts/02_analysis` or `scripts/03_content`. The DAG below 
shows how all components are connected.

![](dag.png)

## Data sources

- Catch and Effort data come from the [WCPFC Public Domain Aggregated Catch/Effort data download page](https://www.wcpfc.int/wcpfc-public-domain-aggregated-catcheffort-data-download-page) (downloaded on Oct 31, 2025)
  - Tuna Purse Seine data: Aggregated data, grouped by 1°x1° latitude/longitude grids, FLAG, YEAR and QUARTER. [PURSE SEINE fishery. Data cover 1950 to 2023 for the WCPFC Convention Area.](https://www.wcpfc.int/file/1016779/download?token=obsDV8q3)
  - Tuna Longline data: Aggregated data, grouped by 5°x5° latitude/longitude grids, YEAR and MONTH. [LONGLINE fishery. Data cover 1950 to 2023 for the WCPFC Convention Area.](https://www.wcpfc.int/file/1016770/download?token=W9FwwLOC)
