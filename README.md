
## Data
Data for this repo is staged in `/data`.
Details on data sources are below.

Data from WIN is pulled manually for each program and put into `data/`.
This data is staged at [this box.com link](https://usf.app.box.com/s/6j6ipvgh1ncu8qcmgw9o0ygxogcg8371).

Additional data is provided in custom formats by some providers:
* AOML SFER data harvested from [this github repo](https://github.com/Ecosystem-Assessment-Lab/SFER/blob/main/DATA/SFER_data.csv) (private)

Older historical data has been collected into [this box.com folder](https://usf.box.com/s/m40d8rsugrp9t4joieh6ce0vo4i1vvkc).

Additional historical data is provided in unique format from some providers:
* FIU [TODO link here](?)

# tests
```bash
Rscript -e "testthat::test_dir(here::here('tests/testthat'))"
```

or

```R
testthat::test_dir(here::here('tests/testthat'))


# plans & TODO items
* re-pull WIN data. it has updated
  * 6 + AOML. Miami Beach coming
  * new one called DEP

* STORET data has no lat,lon. Need to contact upstream.
  * modifications needed once data is updated:
    * tests/testthat/test-getSTORETData.R : re-enable test
    * R/align_storet_df.R : add lat,lon columns
    * getData.R : re-include STORET data

* older data also available:
  * https://usf.box.com/s/m40d8rsugrp9t4joieh6ce0vo4i1vvkc
  * check alignment & merge with newer data
  * 3 there now, 1 more coming

* new FIU dataset with custom import method (excel files)
  * 2 excel files merged, report should be separate from WIN data (for now)
  * many columns, some will be dropped, Dan will give list of columns to keep
  * FIU data also comes from WIN so there will be some overlap

## DEP WIN ingest
* new WIN ingest files from Dan. 1 from each program:
https://usf.box.com/s/6j6ipvgh1ncu8qcmgw9o0ygxogcg8371
   * TODO: need to update files here; files in box updated
   * expect 7 or 8
     * 6 + AOML. Miami Beach coming.
     * new one called DEP
   * palm beach lat/lon is string in deg,min,sec. need to convert to decimal

* AOML.SFER file is additional. (all years, even pre-2015)

* TODO: new FIU dataset with custom import method (excel files)
  * 2 excel files merged, report should be separate from WIN data (for now)
  * many columns, some will be dropped, Dan will give list of columns to keep
  * FIU data also comes from WIN so there will be some overlap

* include links to download each .csv

* provide merged file csv download

* older data also available:
  * https://usf.box.com/s/m40d8rsugrp9t4joieh6ce0vo4i1vvkc
  * check alignment & merge with newer data
  * 3 there now, 1 more coming

## Trend
.csv export a trend for each station.
R code from Dan to calculate trend.
