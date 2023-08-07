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

The `calculate_dys_comp()` function calculates dystocia compliance, overall and by type (if `include_types == T`).

The `structure_data()` function creates a data frame containing the numerator, denominator, rate, CL (center line) UCL (upper control limit) and LCL (lower control limit) for a variable of interest (ex. Cesarean). The dataset is limited to complete cases; additional date or other constraints must be added manually. To use this function with `plot_ctrl_hchart()`, set `for_highchart == T`.

The `plot_ctrl_chart()` function takes the data frame constructed by `structure_data()` and plots it using ggplot2. Plot title, axis titles, and caption must be added manually.

The `plot_ctrl_hchart()` function is identical to the `plot_ctrl_chart()` function, except that it formats everything to be used with the `highchart` rather than `ggplot2` package.

The `create_90_day_lock_dt()` function doesn't take any arguments but provides the date at which cases pulled on a given day will lock. Can be used with `create_obi_cohort()`.

The `create_obi_cohort()` function limits an OBI nightly export dataset to locked, complete cases with a delivery date after 2019.

### Data processing functions
`sort_muti_selection_var()` takes in variable name and sort multiple selection values. For example sort {10,5,3,4} to {3,4,5,10}. use `?sort_muti_selection_var` to see examples to run

`add_CI_values`: This function create upper and lower CI for observed rate. Two new variables will be added LC and UC variables. use `?add_CI_values` to see examples to run


## Example workflow

```r
library(obstinit)
library(tidyverse)

obi = data.table::fread("obi_filepath")

obi_cohort = obi %>% mutate(infant_dob_dt = lubridate::dmy_hms(infant_dob_dt)) %>% filter(flg_complete == 1, birth_year > 2019, locked == 1)

ctrl_cohort = obi_cohort %>% structure_data(infant_dob_dt, year_mon, overall_dystocia_compliance_num, overall_dystocia_den_all, increase_is_bad = F)

ctrl_chart = ctrl_cohort %>% plot_ctrl_chart()
```


## Status

* 8/7/2023 - added `calculate_dys_comp()`
* 5/23/2023 - `create_90_day_lock_dt()` and `create_obi_cohort()` are live
* 5/19/2023 - `plot_ctrl_hchart()` is live
* 5/10/2023 - color legend fix added to plot_ctrl_chart
* 4/5/2023 - Added color gradient and new OBI colors to `plot_ctrl_chart()`. Note that there is still some debugging that needs to be done, primarily in regards to the color legend which is often incorrect.
* 3/29/2023 - `plot_ctrl_chart()` function is usable.
* 3/28/2023 - `structure_data()` function is usable. 
* 3/23/2023 - under construction
