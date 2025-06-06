
## Data
Data for this repo is staged in `/data`.

Data from WIN is pulled manually for each program and put into `data/`.
This data is staged at [this box.com link](https://usf.app.box.com/s/6j6ipvgh1ncu8qcmgw9o0ygxogcg8371).

Additional data is provided in custom formats by some providers:
  * AOML SFER data harvested from [this github repo](https://github.com/Ecosystem-Assessment-Lab/SFER/blob/main/DATA/SFER_data.csv) (private)
  * Older historical data (from STORET) has been collected into [this box.com folder](https://usf.box.com/s/m40d8rsugrp9t4joieh6ce0vo4i1vvkc).
  * newer FIU data from a custom file format
  * MiamiBeach data is a custom format

* Some datasets are missing crucial values
  * STORET data has no lat, lon. (can we add these based on station names?)
  * newer FIU data has no lat, lon. (can we add these based on station names?)

### Notes about the final data
* getData applies depth filtering >=1m dropped
* getData files attempt to align all columns to WIN column names
  * for column mappings between projects see relevant `R/get*Data.R` and `R/align_*_df.R` files
* exported .csv files do not contain all columns. Many more are returned by getData.

## tests
```bash
Rscript -e "testthat::test_dir(here::here('tests/testthat'))"
```

or

```R
testthat::test_dir(here::here('tests/testthat'))
```

## plans & TODO items
* slopes files due tuesday. upload to [this folder](https://drive.google.com/drive/u/0/folders/1aJoe4-hS959vFNoU8aN-dyK7KyyjqVSE). add 
  * full file there
  * two subfolders:
    * one for slope files (seasonal-mann-kendall)
    * one for samples files (unified-wq-db)

* check slope p-value (expect [1,near-0) & significance (expect ~1e5)

* new FIU dataset should be separate from WIN data?
