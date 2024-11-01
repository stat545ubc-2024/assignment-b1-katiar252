Assignment B1
================
Katia Rosenflanz
2024-10-28

``` r
library(palmerpenguins)
```

    ## Warning: package 'palmerpenguins' was built under R version 4.4.1

``` r
library(datateachr)
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 4.4.1

    ## Warning: package 'readr' was built under R version 4.4.1

    ## Warning: package 'stringr' was built under R version 4.4.1

    ## Warning: package 'forcats' was built under R version 4.4.1

    ## Warning: package 'lubridate' was built under R version 4.4.1

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(testthat)
```

    ## Warning: package 'testthat' was built under R version 4.4.1

    ## 
    ## Attaching package: 'testthat'
    ## 
    ## The following object is masked from 'package:dplyr':
    ## 
    ##     matches
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     is_null
    ## 
    ## The following objects are masked from 'package:readr':
    ## 
    ##     edition_get, local_edition
    ## 
    ## The following object is masked from 'package:tidyr':
    ## 
    ##     matches

``` r
#' Calculate Summary Statistics by Group
#' 
#' @description 
#' The function calculates four summary statistics (min, max, mean, standard deviation) of a variable
#' by group within a data table. The variables used for grouping and summarizing are input by the user.
#' 
#' @param data Input data.  
#' @param sort_var Variable used for grouping; named as sort_var to indicate that it is used to sort 
#' the data into groups for subsequent analysis 
#' @param stat_var Numeric variable which is used to calculate summary statistics. Named stat_var as
#' this is the variable with which the summary statistics are calculated 
#'
#' @return A tibble is returned which contains the grouping variable and summary statistics as columns (min, max, mean, and standard deviation) 
sum_stats <- function(data, sort_var, stat_var) {
  #check to see if the variable for summary statistics calculations is numeric 
  if (!is.numeric(pull(data, !!enquo(stat_var)))) {
    stop("The variable you are trying to summarize is not numeric.")
  }
  new_data <- {{data}} %>%
    group_by({{sort_var}}) %>%
    summarize(min = if(all(is.na({{stat_var}}))) NA_real_   #if all values are NA, treat NA as double/numeric missing value
              else min({{stat_var}}, na.rm = TRUE),         #remove all NA values before calculation
              max = if(all(is.na({{stat_var}}))) NA_real_   #if all values are NA, treat NA as double/numeric missing value
              else max({{stat_var}}, na.rm = TRUE), 
              mean= mean({{stat_var}}, na.rm = TRUE), 
              stdev = sd({{stat_var}}, na.rm = TRUE))
  return(new_data)
}
```

After the function has been defined, several examples can be used to
demonstrate its function. The expected output is a tibble containing the
grouped variable and the four summary statistics columns.

``` r
#Example 1: penguins dataset
#group by island, summarize the body_mass_g variable
sum_stats(penguins, island, body_mass_g)
```

    ## # A tibble: 3 × 5
    ##   island      min   max  mean stdev
    ##   <fct>     <int> <int> <dbl> <dbl>
    ## 1 Biscoe     2850  6300 4716.  783.
    ## 2 Dream      2700  4800 3713.  417.
    ## 3 Torgersen  2900  4700 3706.  445.

``` r
#Example 2: cancer_sample dataset
#group by diagnosis (benign or malignant), summarize the area_mean variable
sum_stats(cancer_sample, diagnosis, area_mean)
```

    ## # A tibble: 2 × 5
    ##   diagnosis   min   max  mean stdev
    ##   <chr>     <dbl> <dbl> <dbl> <dbl>
    ## 1 B          144.  992.  463.  134.
    ## 2 M          362. 2501   978.  368.

Next, I will test a use of the sum_stats function that I know will
produce an error: calculating summary statistics on a variable which is
non-numeric. In this case, the sum_stats function will try to summarize
the root_barrier variable, which is not numeric; this will cause an
error.

``` r
#Example 3: vancouver_trees dataset
#group by genus, calculate statistics on root barrier (which is nonnumeric)
sum_stats(vancouver_trees, genus_name, root_barrier)
```

    ## Error in sum_stats(vancouver_trees, genus_name, root_barrier): The variable you are trying to summarize is not numeric.

Finally, I can run several formal tests on the function using the
testthat package.

``` r
#### Testing the function ####
#test case 1: vector with no NAs
test_that("Sum_stats testing with data with no NAs works", {
  data_noNA <- data.frame(
    sort_var = c("A", "A", "B", "B", "C", "C"),
    stat_var = c(1,2,3,4,5,6)
  )
  expect_equal(sum_stats(data_noNA, sort_var, stat_var)$min, c(1, 3, 5))
})
```

    ## Test passed 🎉

``` r
#test case 2: vector with NAs 
test_that("Sum_stats testing with data including NAs should also work", {

  data_withNA <- data.frame(
    sort_var = c("A", "A", "B", "B", "C", "C"),
    stat_var = c(1,2, NA, NA, NA, 6)
  )
  expect_equal(sum_stats(data_withNA, sort_var, stat_var)$mean, c(1.5, NA, 6))
})
```

    ## Test passed 🥇

``` r
#test case 3: Data has non-numeric input as the variable for summarizing
test_that("Sum_stats fails with non-numeric input as summarizing variable", {
  
  data_nonNumeric <- data.frame(
    sort_var = c("A", "A", "B", "B", "C", "C"),
    stat_var = c("Hello", "Yes", "Mountain", "River", "Forest", "Desert")
  )
  expect_error(sum_stats(data_nonNumeric))
})
```

    ## Test passed 🌈
