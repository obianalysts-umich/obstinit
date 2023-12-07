# obstinit

## Background

Obstinit is our OBI "tools" package, meant to provide a series of tools we regularly use in data processing and visualization and to save time used on repetitive coding tasks.

## Installation

Install the development version directly from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("obianalysts-umich/obstinit")
```

## Functions

### Reading data

* `read_current_data()` reads the most recent (today's) OBI registry data from the Turbo drive. This function does not take any arguments and is built to work with both Mac and PC operating systems.
* `read_crosswalk()` reads in the current crosswalk file from Turbo and cleans the column names. This function does not take any arguments.

### Creating OBI cohort

* `create_obi_cohort()` limits an OBI nightly export dataset to locked, complete cases with a delivery date after 2019. Note that `case_lock_dt` is programmed to match what is in the AMx workstation and represents the LAST DAY a case can be edited by a hospital; the case LOCKS at midnight that night. Therefore, cases are considered locked when `case_lock_dt` < `today()`.
* `create_90_day_lock_dt()` doesn't take any arguments but provides the date at which cases pulled on a given day will lock. Can be used with `create_obi_cohort()`.

### OBI measure specs

* The `create_*_cohort()` family of functions limits the dataset to denominator criteria based on the measure of interest (Cat II, IA) 
* The `calculate_*_comp()` family of functions calculates compliance with OBI measures (usually P4P scorecard measures) for Cat II, IA, TeamBirth, and Labor Dystocia. For dystocia compliance, individual types can be added by using `include_types == T`.

### Plotting

* `create_ctrl_df()` creates a data frame containing the numerator, denominator, rate, CL (center line) UCL (upper control limit) and LCL (lower control limit) for a variable of interest (ex. Cesarean). The dataset is limited to complete cases; additional date or other constraints must be added manually. To use this function with `plot_ctrl_hchart()`, set `for_highchart == T`. This function used to be called `structure_data()`.
* `plot_ctrl_chart()` takes the data frame constructed by `structure_data()` and plots it using ggplot2. Plot title, axis titles, and caption must be added manually.
* `plot_ctrl_hchart()` is identical to the `plot_ctrl_chart()` function, except that it formats everything to be used with the `highchart` rather than `ggplot2` package.
* `create_caterpillar_df()` structures data for caterpillar (ranking) plots; each row of the data frame is a site name by default and the data frame includes a numerator, denominator, rate, and (0-1 bounded) 95% CI.
* `plot_caterpillar_chart()` plots a caterpillar chart, presumably using the dataframe created by `create_caterpillar_df()`. Plot title, axis titles, and caption must be added manually.
* `theme_obi()` is a ggplot2 theme that standardizes formatting of plots for semi-annual meetings, presentations, and more. It uses `theme_minimal()` as a base and additionally centers the plot title and subtitle, left-justifies the plot caption, standardizes text sizes across the plot, and makes all text OBI's dark blue. All OBI plots should use this theme.

### Other data processing
* `sort_muti_selection_var()` takes in variable name and sort multiple selection values. For example sort {10,5,3,4} to {3,4,5,10}. use `?sort_muti_selection_var` to see examples to run.
* `add_CI_values`: This function creates upper and lower CI for an observed rate. Two new variables (`LC` and `UC`) are added. use `?add_CI_values` to see examples to run.


## Example workflow - reading in data, structuring data, and plotting control chart in ggplot

```r
library(obstinit)
library(tidyverse)

obi = read_current_data()

obi_cohort = obi %>% 
  create_obi_cohort()

ctrl_cohort = obi_cohort %>% 
  create_ctrl_df(infant_dob_dt,
                 overall_dystocia_compliance_num, 
                 overall_dystocia_den_all, 
                 date_gran = "quarter",
                 increase_is_bad = F)

ctrl_chart = ctrl_cohort %>% 
  plot_ctrl_chart()
```


## Status updates

* 12/6/2023 - obstinit is fully functional and is considered to be in a finalized format. New functions are added as needed. Package is in maintenance phase and status updates will no longer be posted unless major changes are made.
* 9/19/2023 - added `read_current_data`
* 8/7/2023 - added `calculate_dys_comp()`
* 5/23/2023 - `create_90_day_lock_dt()` and `create_obi_cohort()` are live
* 5/19/2023 - `plot_ctrl_hchart()` is live
* 5/10/2023 - color legend fix added to plot_ctrl_chart
* 4/5/2023 - Added color gradient and new OBI colors to `plot_ctrl_chart()`. Note that there is still some debugging that needs to be done, primarily in regards to the color legend which is often incorrect.
* 3/29/2023 - `plot_ctrl_chart()` function is usable.
* 3/28/2023 - `structure_data()` function is usable. 
* 3/23/2023 - under construction
