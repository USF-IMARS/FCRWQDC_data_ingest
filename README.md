
## Data
Data for this repo is staged in `/data`.

Data from WIN is pulled manually for each program and put into `data/`.
This data is staged at [this box.com link](https://usf.app.box.com/s/6j6ipvgh1ncu8qcmgw9o0ygxogcg8371).

Additional data is provided in custom formats by some providers:
  * AOML SFER data harvested from [this github repo](https://github.com/Ecosystem-Assessment-Lab/SFER/blob/main/DATA/SFER_data.csv) (private)
  * Older historical data (from STORET) has been collected into [this box.com folder](https://usf.box.com/s/m40d8rsugrp9t4joieh6ce0vo4i1vvkc).
  * newer FIU data from a custom file format
  * MiamiBeach data is a custom format

# tests
```bash
Rscript -e "testthat::test_dir(here::here('tests/testthat'))"
```

or

```R
testthat::test_dir(here::here('tests/testthat'))
```

# plans & TODO items
* slopes files due tuesday. upload to [this folder](https://drive.google.com/drive/u/0/folders/1aJoe4-hS959vFNoU8aN-dyK7KyyjqVSE). add 
  * full file there
  * two subfolders:
    * one for slope files (seasonal-mann-kendall)
    * one for samples files (unified-wq-db)

* reduce analytes down to emailed list from Dan

* depth filtering >=1m dropped

* add to slope p-value (expect [1,near-0) & significance (expect ~1e5)

* csv files expected (reduce cols down to only what is needed):
  * one for each analyte (slope file)
    * cols: program (source), Monitoring.Location.ID (site), trend (slope), p-value, significance, mean, min, max, coefficient.of.variation
  * one for each analyte (samples file)
    * cols: program (source), Monitoring.Location.ID (site), lat, lon, month, day, year, value, units, sample depth (NOTE: samples >1m not included) 
* csv file with everything (same cols as above, + analyte column)

* re-pull WIN data. it has updated
  * 6 + AOML. Miami Beach coming
  * new one called DEP

* Some datasets are missing crucial values
  * STORET data has no lat, lon. (can we add these based on station names?)
  * newer FIU data has no lat, lon. (can we add these based on station names?)

* older data also available:
  * https://usf.box.com/s/m40d8rsugrp9t4joieh6ce0vo4i1vvkc
  * check alignment & merge with newer data
  * 3 there now, 1 more coming

* new FIU dataset with custom import method (excel files)
  * 2 excel files merged, report should be separate from WIN data (for now)
  * many columns, some will be dropped, Dan will give list of columns to keep
  * FIU data also comes from WIN so there will be some overlap

