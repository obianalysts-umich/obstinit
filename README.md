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

The `structure_data()` function creates a data frame containing the numerator, denominator, rate, CL (center line) UCL (upper control limit) and LCL (lower control limit) for a variable of interest (ex. Cesarean); the data frame is in long format by default so it can be piped directly into the `plot_ctrl_chart()` function. The dataset is automatically limited to complete cases after 2019; additional date constraints must be added manually.

The `plot_ctrl_chart()` function takes the data frame constructed by `structure_data()` and plots it using ggplot2. Plot title, axis titles, and caption must be added manually.

## Status

* 3/29/2023 - `plot_ctrl_chart()` function is usable
* 3/28/2023 - `structure_data()` function is usable. 
* 3/23/2023 - under construction
