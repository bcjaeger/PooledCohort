
<!-- README.md is generated from README.Rmd. Please edit that file -->

# PooledCohort

<!-- badges: start -->

<!-- badges: end -->

The goal of `PooledCohort` is to give researchers who study risk
prediction for cardiovascular disease a clean interface to implement the
Pooled Cohort Risk prediction equations.

*Why do we want to use Pooled Cohort Risk prediction equations?*

The 2017 American College of Cardiology and American Heart Association
blood pressure guideline recommends using 10-year predicted
atherosclerotic cardiovascular disease risk to guide the decision to
initiate or intensify antihypertensive medication. The guideline
recommends using the Pooled Cohort Risk prediction equations to predict
10-year atherosclerotic cardiovascular disease risk. Therefore, a new
method for predicting atherosclerotic cardiovascular disease risk should
be evaluated based on how much it can improve risk prediction relative
to the Pooled Cohort Risk prediction equations.

## Installation

<!-- You can install the released version of PooledCohort from [CRAN](https://CRAN.R-project.org) with: -->

<!-- ``` r -->

<!-- install.packages("PooledCohort") -->

<!-- ``` -->

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("bcjaeger/PooledCohort")
```

## Example

A basic example below computes 10-year atherosclerotic cardiovascular
risk using the original Pooled Cohort Risk equations for a person who is
black/white and male/female with

  - 55 years of age
  - 213 mg/dL total cholesterol
  - 50 mg/dL high density lipoprotein (HDL) cholesterol
  - 120 mm Hg systolic blood pressure
  - no antihypertensive medication use
  - no current smoking
  - no prevalent diabetes

First we will use a dataset that requires no modification of any
variable to be plugged into `predict_10yr_ascvd_risk()`

``` r

library(PooledCohort)
library(dplyr, warn.conflicts = FALSE)

example_data <- data.frame(
  sex = c('female', 'female', 'male', 'male'),
  race = c('black', 'white', 'black', 'white'),
  age_years = rep(55, times = 4),
  chol_total_mgdl = rep(213, times = 4),
  chol_hdl_mgdl = rep(50, times = 4),
  bp_sys_mmhg = rep(120, times = 4),
  bp_meds = rep('no', times = 4),
  smoke_current = rep('no', times = 4),
  diabetes = rep('no', times = 4),
  stringsAsFactors = FALSE
)

example_data
#>      sex  race age_years chol_total_mgdl chol_hdl_mgdl bp_sys_mmhg bp_meds
#> 1 female black        55             213            50         120      no
#> 2 female white        55             213            50         120      no
#> 3   male black        55             213            50         120      no
#> 4   male white        55             213            50         120      no
#>   smoke_current diabetes
#> 1            no       no
#> 2            no       no
#> 3            no       no
#> 4            no       no
```

A convenient way to use `predict_10yr_ascvd_risk()` is within
`dplyr::mutate()`:

``` r

example_risk <- example_data %>% 
  mutate(
    risk = predict_10yr_ascvd_risk(
      sex = sex,
      race = race,
      age_years = age_years,
      chol_total_mgdl = chol_total_mgdl,
      chol_hdl_mgdl = chol_hdl_mgdl,
      bp_sys_mmhg = bp_sys_mmhg,
      bp_meds = bp_meds,
      smoke_current = smoke_current,
      diabetes = diabetes
    )
  ) %>% 
  select(sex, race, risk)

example_risk
#>      sex  race       risk
#> 1 female black 0.02998196
#> 2 female white 0.02054450
#> 3   male black 0.06061116
#> 4   male white 0.05378606
```

## Data formatting

Data usually need to be modified slightly before being plugged into the
Pooled Cohort Risk equations. For example, instead of a `race` variable
with values of `black` and `white`, the data may have a `race` variable
with values of `african_american`, `caucasian`, and `other`.

``` r

example_data_granular <- data.frame(
  sex = c('female', 'female', 'male', 'male'),
  race = c('african_american', 'caucasian', 'african_american', 'other'),
  age_years = rep(55, times = 4),
  chol_total_mgdl = rep(213, times = 4),
  chol_hdl_mgdl = rep(50, times = 4),
  bp_sys_mmhg = rep(120, times = 4),
  bp_meds = rep('no', times = 4),
  smoke_current = rep('no', times = 4),
  diabetes = rep('no', times = 4),
  stringsAsFactors = FALSE
)
```

While you can always modify variables in your data so that they meet the
same format as the variables in `example_data` above, you may prefer to
let `predict_10yr_ascvd_risk()` modify those variables for you:

``` r

# a mapping from the current race categories to 
# the 'black' and 'white' categories used by the
# Pooled Cohort Risk equations.

race_levels <- list(
  black = 'african_american',
  white = c('caucasian', 'other')
)

example_risk_granular <- example_data_granular %>% 
  mutate(
    risk = predict_10yr_ascvd_risk(
      sex = sex,
      race = race,
      race_levels = race_levels,
      age_years = age_years,
      chol_total_mgdl = chol_total_mgdl,
      chol_hdl_mgdl = chol_hdl_mgdl,
      bp_sys_mmhg = bp_sys_mmhg,
      bp_meds = bp_meds,
      smoke_current = smoke_current,
      diabetes = diabetes
    )
  ) %>% 
  select(sex, race, risk)

example_risk_granular
#>      sex             race       risk
#> 1 female african_american 0.02998196
#> 2 female        caucasian 0.02054450
#> 3   male african_american 0.06061116
#> 4   male            other 0.05378606
```

## A picky estimator

`predict_10yr_ascvd_risk()` is a picky estimator that will throw hard
stops at you if it doesn’t like the data you give it. In particular, if
the input data has continuous variables with values outside the range of
recommended values for the Pooled Cohort equations, you will get an
error message.

``` r

predict_10yr_ascvd_risk(
  sex = 'male',
  race = 'black',
  age_years = 35, # age recommendation: 40-79
  chol_total_mgdl = 213,
  chol_hdl_mgdl = 55,
  bp_sys_mmhg = 120,
  bp_meds = 'no',
  smoke_current = 'no',
  diabetes = 'no'
)
#> Error: min(age_years) is 35 but should be >= 40
```

This is meant to help you avoid mis-use of the Pooled Cohort Risk
equations. However, if you *must* go outside the range of recommended
values, you can set the argument `override_boundary_errors` to `TRUE`.

``` r

predict_10yr_ascvd_risk(
  sex = 'male',
  race = 'black',
  age_years = 35,
  chol_total_mgdl = 213,
  chol_hdl_mgdl = 55,
  bp_sys_mmhg = 120,
  bp_meds = 'no',
  smoke_current = 'no',
  diabetes = 'no',
  override_boundary_errors = TRUE
)
#> [1] 0.01969643
```
