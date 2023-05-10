# obstinit

## Background

This is the draft space for our OBI "tools" package.

## Installation

Install the development version directly from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("obianalysts-umich/obstinit")
```

## Functions

The `structure_data()` function creates a data frame containing the numerator, denominator, rate, CL (center line) UCL (upper control limit) and LCL (lower control limit) for a variable of interest (ex. Cesarean); the data frame is in long format by default so it can be piped directly into the `plot_ctrl_chart()` function. The dataset is limited to complete cases; additional date or other constraints must be added manually.

The `plot_ctrl_chart()` function takes the data frame constructed by `structure_data()` and plots it using ggplot2. Plot title, axis titles, and caption must be added manually.

## Status

* 5/10/2023 - color legend fix added
* 4/5/2023 - Added color gradient and new OBI colors to `plot_ctrl_chart()`. Note that there is still some debugging that needs to be done, primarily in regards to the color legend which is often incorrect.
* 3/29/2023 - `plot_ctrl_chart()` function is usable.
* 3/28/2023 - `structure_data()` function is usable. 
* 3/23/2023 - under construction

## Example workflow

```r
library(obstinit)
library(tidyverse)

obi = data.table::fread("obi_filepath")

obi_cohort = obi %>% mutate(infant_dob_dt = lubridate::dmy_hms(infant_dob_dt)) %>% filter(flg_complete == 1, birth_year > 2019, locked == 1)

ctrl_cohort = obi_cohort %>% structure_data(infant_dob_dt, year_mon, overall_dystocia_compliance_num, overall_dystocia_den_all, increase_is_bad = F)

ctrl_chart = ctrl_cohort %>% plot_ctrl_chart()
```
