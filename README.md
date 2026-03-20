# TODO:
Test out the provider listing summary and merge it into main branch if it looks good.
If feeling frisky: create icons with tooltips in the summary

## Data
Data for this repo is staged in `/data`.

Final data is also stored in gdrive [here](https://drive.google.com/drive/folders/1QMUpJOwfAMG9dB_fYT0Yva3IBVRg27fr?usp=drive_link).

-------------------------------------------------------------

The final data produced by this ingestion is visualized further using [this shiny data dashboard](https://github.com/USF-IMARS/FCRWQDC_data_dashboard).

-------------------------------------------------------------

Details on the data files ingested by program is in the [SEACAR program name/id mapping gsheet](https://docs.google.com/spreadsheets/d/1GVJ4kyze9s_XfiOls81FaKKY4B1dH7Ex08UM7B6A_pc/edit?usp=sharing).

Additional details on the upstream data files are below.

Data from WIN is pulled manually for each program and put into `data/`.
This data is staged at [this box.com link](https://usf.app.box.com/s/6j6ipvgh1ncu8qcmgw9o0ygxogcg8371).

Additional data is provided in custom formats by some providers:
  * AOML SFER data harvested from [this github repo](https://github.com/Ecosystem-Assessment-Lab/SFER/blob/main/DATA/SFER_data.csv) (private)
  * Older historical data (from STORET) has been collected into [this box.com folder](https://usf.box.com/s/m40d8rsugrp9t4joieh6ce0vo4i1vvkc).
  * newer FIU data from a custom file format
  * MiamiBeach data is a custom format

### Known Issues
  * AOML_FBBB :
    * Missing Units for most analytes
  * BBAP, BROWARD, DEP, DERM, FIU_WQMP, PALMBEACH :
    * source data has rows with missing critical fields when Activity.Type is "Blank", "Replicate", or similar. These rows are dropped by getWINData().
      * This filtering is also applied to other WIN datasets.
  * DEP :
    * small number of rows missing Lat+Lon
  * STORET data (BROWARD_STORET, DERM_BBWQ_STORET, PALMBEACH_STORET)
    * no latitude+longitude included in raw source files
  * FIU_WQMP_RECENT (`data/FIU_recent_all.csv` )
    * no lat+lon included in raw source files
    * raw source file has no units
  * SFER data has no units
  * Miami Beach some sites have an extra '#' in front (site1 and #site1)

### Notes about the final data
* getData files attempt to align all columns to WIN column names
  * for column mappings between projects see relevant `R/get*Data.R` and `R/align_*_df.R` files
* most exported .csv files do not contain all columns. Many more are returned by getData. For all data see the allDataRaw.csv

#### chlorophyll a
Chlorophyll a values are special because some are  corrected for pheophytin.

At time of writing corrected/uncorrected is not known for some programs:
* FIU_Estuaries
* MiamiBeach
* SFER

For these programs the chlorophyll_a values are not included.

## tests
```bash
Rscript -e "testthat::test_dir(here::here('tests/testthat'))"
```

or

```R
testthat::test_dir(here::here('tests/testthat'))
```

## Common Workflows
### Add a Provider
1. Add provider data files to `./data/`.
2. Check `R/getListOfPrograms.R`
3. If custom file reader is needed
  * create file `get{provider}Data.R`
    * map columns to standard column names `DEP.Result.ID Activity.ID year month day  time   Activity.Start.Date.Time lat_deg lat_min Org.Decimal.Latitude lon_deg lon_min   Org.Decimal.Longitude Monitoring.Location.ID Activity.Type Activity.Depth   Activity.Depth.Unit Activity.Depth.Top.Bottom.Unit Sample.Collection.Type   Activity.Top.Depth Activity.Bottom.Depth Value.Qualifier Result.Comments   DEP.Analyte.Name DEP.Result.Value.Number DEP.Result.Unit`
  * include relevant logic in `getData.R`
    * new `get{provider}Data` call
    * analyte name mappings
 
