# Replication Package

**Comparing child and caregiver reports of food insecurity: Agreement and associations with child skills**

**Authors:** Allan Lee (Princeton University), Elisabetta Aurino (University of Barcelona), Sharon Wolf (University of Pennsylvania)

**Contact:** Allan Lee, allanlee@princeton.edu

## Citation

If you use this code, please cite:

> Lee, Allan, Elisabetta Aurino, and Sharon Wolf, “Comparing child and caregiver reports of food insecurity: Agreement and associations with child skills,” *Economics & Human Biology*, December 2026, 63, 101641.

## Overview

Reproduces all tables and appendices in the paper from the PNP Ghana midline/endline survey data. Run `run_all.R` to regenerate everything from raw data. All paths in the code are relative to this folder, so the package runs on any computer once the required software is installed - no manual path edits needed.

## Folder structure

```
replication/
  run_all.R            <- master script; run this from top to bottom
  README.md            <- this file
  data/
    raw/               <- input data (see Data below)
    derived/           <- created by the data-cleaning scripts
  code/
    header.R           <- shared packages/functions used by every script
    00_data_cleaning/  <- cleans raw data into data/derived/
    01_analysis/       <- one script per table/appendix
  tables/              <- output .tex files land here
```

`run_all.R` creates `data/raw/`, `data/derived/` and `tables/` if they are missing (e.g. if empty folders were dropped when the package was unzipped).

## Software requirements

- **R (4.x).** Required packages are listed at the top of `run_all.R`; it offers to install any that are missing.
- **Stata** (any recent edition), to run `code/00_data_cleaning/3_ppi_ghana_dofile.do`. `run_all.R` tries to locate a Stata executable automatically; if it can't find one, it stops and asks you to run that one do-file by hand first.

The results in the paper were produced with the following versions. Newer versions should work, but package updates can change table formatting or deprecate functions used here.

- R 4.5.2 (Windows)
- StataNow 19.5 MP

| Package | Version | Package | Version | Package | Version |
|---|---|---|---|---|---|
| AER | 1.2.17 | kableExtra | 1.4.0 | purrr | 1.2.2 |
| broom | 1.0.13 | knitr | 1.51 | readr | 2.2.0 |
| dplyr | 1.2.1 | lfe | 3.1.1 | sandwich | 3.1.2 |
| fastDummies | 1.7.6 | lmtest | 0.9.40 | stargazer | 5.2.3 |
| fixest | 0.14.2 | ltm | 1.2.0 | tidyr | 1.3.2 |
| flextable | 0.10.0 | lubridate | 1.9.4 | tidyverse | 2.0.0 |
| ggpubr | 1.0.0 | MASS | 7.3.65 | tinytable | 0.17.0 |
| glue | 1.8.0 | modelsummary | 2.6.0 | writexl | 1.5.4 |
| haven | 2.5.5 | officer | 0.7.5 | xtable | 1.8.8 |
| janitor | 2.2.1 | psych | 2.6.5 | | |

## How to run

1. Install R and Stata.
2. Place the raw data files listed under [Data](#data) below into `data/raw/`.
3. From this folder, run:
   ```
   Rscript run_all.R
   ```
   (or open `run_all.R` in RStudio/another IDE and source it - it finds this folder automatically either way)
4. `run_all.R` installs any missing R packages, runs the 4 data-cleaning steps in order, then every `table_*.R` and `appendix_*.R` script, and prints a summary of what succeeded/failed.
5. Output `.tex` files appear in `tables/`, plus `tables/workpaper.csv` (supplementary statistics cited in the text).

Stata's batch-mode log file is deleted automatically after the do-file runs; if the do-file fails, the end of the log is printed to the console first.

To run a single script by hand instead of the full pipeline: the data-cleaning steps must be run in numbered order (1-4) first, since each script needs `data/derived/regression_build_w.rds` to already exist.

## Data

The data are not included in this package. `data/raw/` should contain these 6 files, all part of the PNP Ghana survey on the World Bank Microdata Catalog: <https://microdata.worldbank.org/index.php/catalog/6492>

- `01_PNP_Endline_HouseholdSurvey.dta`
- `02_PNP_Endline_CaregiverSurvey.dta`
- `02_PNP_Midline_CaregiverSurvey.dta`
- `03_PNP_Endline_ChildSurvey.dta`
- `03_PNP_Midline_ChildSurvey.dta`
- `Enrolment & Caregiver Survey_depii.dta`

Access to the data is subject to the World Bank Microdata Library's terms of use; users must obtain the data directly from the catalog above.

Appendix B is created by hand and can be accessed via the PNP Ghana survey documentation: <https://microdata.worldbank.org/catalog/6492/pdf-documentation>

## Controlled randomness

No random seed is used or needed - no bootstrap, simulation, or randomized estimator appears in any table or appendix in this package.
