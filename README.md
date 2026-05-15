# AGES-Chicago-Overview-Paper-Software-Data
This repository contains software used to make figures for the paper "Overview of the 2023 AGES+ Chicago Field Experiment". It also contains some data files used to make the figures. Most of the data is stored in the archives references in the paper.

*Journal of Geophysical Research: Atmospheres* — Dickens et al.

## Description

The scripts in this repository were used to produce the figures in the main text and Supporting Information (SI). See the crosswalk tables below for a mapping of scripts to figures.

## Authors

Angela F. Dickens, Paytsar Muradyan, Joseph O'Brien, Martina Rogers, Ping Jing, and John Burden

## Repository Structure

### CROCUS/
Authors: Paytsar Muradyan and Joseph O'Brien

- `ages_dl_pbl.ipynb` — Jupyter Notebook; creates Figure 3
- `plot_rwp_winds_20230802_CST.ipynb` — Jupyter Notebook; creates Figure S3
- Input and output files for `plot_rwp_winds_20230802_CST.ipynb`

### Dickens/
Author: Angela F. Dickens

- `AGES Chicago Chiwaukee vertical plot-repository.R` — R script; Creates Figures 7, 8, 9, S12, S15, S16, S17, and S18
- `AGES Chicago timeseries plot-repository.R` — R script; Creates Figures 2, 4, and S9
- `AQS data - early August 2023-repository.R` — R script; Creates Figures S5, S8, S10, and S14
- `GMAP route data for paper.R` — R script; Creates Figure S1
- `GMAP_final_dataset.R` — R script; Called by `GMAP_Final_Mapping_Functions.R`
- `GMAP_Final_Mapping_Functions.R` — R script; Creates Figure S4
- Nine `.csv` files containing location or name information for regulatory monitors

### PTRMS/
Author: Martina Rogers

- `figures.m` — MATLAB script; creates Figures 6 and S7
- Four MATLAB data files (`.mat`) used as input to `figures.m`
- Note that an additional `.mat` file is in the Minds@UW archive referenced in the paper

### PurpleAir/
Author: Ping Jing

- `Plot_PA_Aug2nd_2023.R` — R script; creates Figure S6

### UAH/
Author: John Burden

- `HSRLvsSEAREYtempospatialalign.py` — Python script; creates Figure S13

## Figure–Code Crosswalk

### Main Document

| Figure | Code or Program |
|--------|----------------|
| 1 | ArcGIS Pro |
| 2 | `AGES Chicago timeseries plot-repository.R` |
| 3 | `ages_dl_pbl.ipynb` |
| 4 | `AGES Chicago timeseries plot-repository.R` |
| 5 | IgorPro |
| 6 | `figures.m` |
| 7 | `AGES Chicago Chiwaukee vertical plot-repository.R` |
| 8 | `AGES Chicago Chiwaukee vertical plot-repository.R` |
| 9 | `AGES Chicago Chiwaukee vertical plot-repository.R` |
| 10 | Python (see Acdan GitHub) |
| 11 | Python (see Acdan GitHub) |

### Supporting Information

| Figure | Code or Program |
|--------|----------------|
| S1 | `GMAP route data for paper.R` |
| S2 | ArcGIS |
| S3 | `plot_rwp_winds_20230802_CST.ipynb` |
| S4 | `GMAP_Final_Mapping_Functions.R` |
| S5 | `AQS data - early August 2023-repository.R` |
| S6 | `Plot_PA_Aug2nd_2023.R` |
| S7 | `figures.m` |
| S8 | `AQS data - early August 2023-repository.R` |
| S9 | `AGES Chicago timeseries plot-repository.R` |
| S10 | `AQS data - early August 2023-repository.R` |
| S11 | IgorPro |
| S12 | `AGES Chicago Chiwaukee vertical plot-repository.R` |
| S13 | `HSRLvsSEAREYtempospatialalign.py` |
| S14 | `AQS data - early August 2023-repository.R` |
| S15 | `AGES Chicago Chiwaukee vertical plot-repository.R` |
| S16 | `AGES Chicago Chiwaukee vertical plot-repository.R` |
| S17 | `AGES Chicago Chiwaukee vertical plot-repository.R` |
| S18 | `AGES Chicago Chiwaukee vertical plot-repository.R` |

## License

This repository is licensed under the MIT License. See the LICENSE file for details.

## Contact

Angela F. Dickens, dickens@ladco.org
