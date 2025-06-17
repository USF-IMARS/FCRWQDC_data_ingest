
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
  * STORET DERM_BBWQ has no depth

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
* SFER data in micromoles/L. Needs to convert to mg/L like others. Dan will email conversions.
* slopes files due tuesday. upload to [this folder](https://drive.google.com/drive/u/0/folders/1aJoe4-hS959vFNoU8aN-dyK7KyyjqVSE). add 
  * full file there
  * two subfolders:
    * one for slope files (seasonal-mann-kendall)
    * one for samples files (unified-wq-db)

* check slope p-value (expect [1,near-0) & significance (expect ~1e5)

* new FIU dataset should be separate from WIN data?

* code for loading old STORET file formats no longer needed (discuss w/ Dan)

FIU data:
- Sites do not have coordinates
- 2017 data has missing site names
- The dates are formatted differently
- The units and sample depths are all NA
- Orthophosphate values are NA.
- There are lots of NA values in general for the different analytes
- Looks like there might be some data missing in Florida Bay for FIU
- Site names are different, don't have the "-W"
- Analytes have different names then the others

Miami Beach:
- Some sites do not have coordinates (some of those sites are only present in the 2024 data and we could not find coordinates for previous years)
- Some sites had an extra '#' in front

Palm Beach:
- Some Dates were formatted differently with quotation marks and no time stamp
- Some analytes values are NA for SFER; BBAP; BROWARD, DERM_WQ, MiamiBeach, PALMBEACH, FIU_WQMP.

For the slopes:
- Some NA values; would it be possible to include site coordinates in the slope tables? 
- Also we thought moving forward we could include two columns with the year when sampling started and when sampling ended for that site, which could be useful?

 
