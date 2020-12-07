P8130 Final Project
================
Pengyuan Su (ps3195), Shuhong Xiang (sx2289), Yali Zhai (yz3959),
Zhixing Wu (zw2709)
12/6/2020

``` r
library(tidyverse)
library(modelr)
library(mgcv)
```

    ## Loading required package: nlme

    ## 
    ## Attaching package: 'nlme'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     collapse

    ## This is mgcv 1.8-33. For overview type 'help("mgcv-package")'.

# Introduction

# Abstract

# Data Analysis

## Tidy:

`rate`: hate\_crimes\_per\_100k\_splc

`med_income`: median\_household\_income

`high_degree`: perc\_population\_with\_high\_school\_degree

`non_citizen`: perc\_non\_citizen

`non_white`: perc\_non\_white

``` r
hc_df = 
  read_csv(here::here("data/HateCrimes.csv")) %>% 
  janitor::clean_names() %>% 
  mutate(hate_crimes_per_100k_splc = as.numeric(hate_crimes_per_100k_splc)) %>% 
  drop_na() %>% 
  rename(., 
         rate = hate_crimes_per_100k_splc, 
         med_income = median_household_income, 
         high_degree = perc_population_with_high_school_degree, 
         non_citizen = perc_non_citizen, 
         non_white = perc_non_white)
```

    ## 
    ## -- Column specification --------------------------------------------------------
    ## cols(
    ##   state = col_character(),
    ##   unemployment = col_character(),
    ##   urbanization = col_character(),
    ##   median_household_income = col_double(),
    ##   perc_population_with_high_school_degree = col_double(),
    ##   perc_non_citizen = col_double(),
    ##   gini_index = col_double(),
    ##   perc_non_white = col_double(),
    ##   hate_crimes_per_100k_splc = col_character()
    ## )

``` r
head(hc_df)
```

    ## # A tibble: 6 x 9
    ##   state unemployment urbanization med_income high_degree non_citizen gini_index
    ##   <chr> <chr>        <chr>             <dbl>       <dbl>       <dbl>      <dbl>
    ## 1 Alab~ high         low               42278       0.821        0.02      0.472
    ## 2 Alas~ high         low               67629       0.914        0.04      0.422
    ## 3 Ariz~ high         high              49254       0.842        0.1       0.455
    ## 4 Arka~ high         low               44922       0.824        0.04      0.458
    ## 5 Cali~ high         high              60487       0.806        0.13      0.471
    ## 6 Colo~ low          high              60940       0.893        0.06      0.457
    ## # ... with 2 more variables: non_white <dbl>, rate <dbl>

``` r
hc_df %>% 
  ggplot(aes(x = rate, y = ..density..)) +
  geom_histogram(fill = "blue", alpha = .4) +
  geom_density(aes( x = rate, y = ..density..)) +
  theme_bw() +
  labs(title = "Distribution for hate crimes per 100k",
       x = "hate crimes per 100k splc",
       y = " Count") +
  theme(plot.title = element_text(hjust = .5 ))
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

<img src="P8130_Final_Project_files/figure-gfm/plot distribution of rate-1.png" width="90%" />

``` r
hc_df %>% 
  mutate(lg = log(rate)) %>% 
  ggplot(aes(x = lg, y = ..density..)) +
  geom_histogram(fill = "blue", alpha = .4) +
  geom_density(aes( lg, y = ..density..)) +
  theme_bw() +
  labs(title = "Distribution for Log ",
       x = "Log (hate crimes per 100k splc)",
       y = " Count") +
  theme(plot.title = element_text(hjust = .5 ))
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

<img src="P8130_Final_Project_files/figure-gfm/plot distribution of rate-2.png" width="90%" />
