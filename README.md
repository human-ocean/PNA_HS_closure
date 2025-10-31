# PNA_HS_closure

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
most of which are produced by scripts in `scripts/02_analysis` or `scripts/03_content`.
