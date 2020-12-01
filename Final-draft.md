Examining the MTA ridership between 2019 and 2020
================

link to project rubric: <https://p8105.com/final_project.html> \#\#\#
Project Title

### Members:

Aishwarya Anuraj aa4517, Michelle Lee mjl2241, Julián Ponce jp3999,
Adarsh Ramakrishnan ar4040, Allison Stewart als2377

### About the Data:

mta: <https://new.mta.info/coronavirus/ridership>

``` r
mta_data = read_csv (file = "./data/MTA_recent_ridership_data_20201123_0.csv",
                     col_types = cols(
                       date = col_date(format = "%mm/%dd/%yy"),
                       `Subways: % Change From 2019 Equivalent Day` = col_number(),
                       `Buses: % Change From 2019 Equivalent Day` = col_number(),
                       `Bridges and Tunnels: % Change From 2019 Equivalent Day` = col_number()
                       ) #only changed the formats of important variables 
) %>%
  janitor::clean_names()
skimr::skim(mta_data)
```

|                                                  |           |
| :----------------------------------------------- | :-------- |
| Name                                             | mta\_data |
| Number of rows                                   | 268       |
| Number of columns                                | 13        |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |           |
| Column type frequency:                           |           |
| character                                        | 4         |
| numeric                                          | 9         |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |           |
| Group variables                                  | None      |

Data summary

**Variable type: character**

| skim\_variable                                                                            | n\_missing | complete\_rate | min | max | empty | n\_unique | whitespace |
| :---------------------------------------------------------------------------------------- | ---------: | -------------: | --: | --: | ----: | --------: | ---------: |
| date                                                                                      |          0 |           1.00 |   6 |   8 |     0 |       268 |          0 |
| lirr\_percent\_change\_from\_2019\_monthly\_weekday\_saturday\_sunday\_average            |         31 |           0.88 |   4 |   4 |     0 |        41 |          0 |
| metro\_north\_percent\_change\_from\_2019\_monthly\_weekday\_saturday\_sunday\_average    |         31 |           0.88 |   4 |   4 |     0 |        39 |          0 |
| access\_a\_ride\_percent\_change\_from\_2019\_monthly\_weekday\_saturday\_sunday\_average |          0 |           1.00 |   5 |   7 |     0 |       222 |          0 |

**Variable type: numeric**

| skim\_variable                                                      | n\_missing | complete\_rate |       mean |        sd |       p0 |       p25 |       p50 |        p75 |      p100 | hist  |
| :------------------------------------------------------------------ | ---------: | -------------: | ---------: | --------: | -------: | --------: | --------: | ---------: | --------: | :---- |
| subways\_total\_estimated\_ridership                                |          0 |           1.00 | 1188754.06 | 907206.69 | 198693.0 | 590202.25 | 1082366.5 | 1606546.00 | 5515945.0 | ▇▅▁▁▁ |
| subways\_percent\_change\_from\_2019\_equivalent\_day               |          0 |           1.00 |    \-74.53 |     17.80 |   \-93.5 |   \-87.40 |    \-75.2 |    \-69.90 |      23.9 | ▇▂▁▁▁ |
| buses\_total\_estimated\_ridership                                  |          0 |           1.00 |  898386.33 | 367033.69 | 279100.0 | 591025.00 |  948100.0 | 1109492.00 | 2244500.0 | ▇▇▇▁▁ |
| buses\_percent\_change\_from\_2019\_equivalent\_day                 |          0 |           1.00 |    \-51.25 |     18.09 |   \-84.0 |   \-63.50 |    \-52.0 |    \-43.00 |      40.0 | ▃▇▁▁▁ |
| lirr\_total\_estimated\_ridership                                   |         31 |           0.88 |   49484.81 |  29227.32 |   1900.0 |  22800.00 |   46800.0 |   76600.00 |   92500.0 | ▆▅▅▅▇ |
| metro\_north\_total\_estimated\_ridership                           |         31 |           0.88 |   40194.51 |  19249.81 |   5100.0 |  21200.00 |   43600.0 |   58200.00 |   77300.0 | ▇▅▇▇▅ |
| access\_a\_ride\_total\_scheduled\_trips                            |          0 |           1.00 |   14138.28 |   6772.45 |   2506.0 |   8404.50 |   13073.0 |   19894.75 |   34304.0 | ▇▇▇▂▁ |
| bridges\_and\_tunnels\_total\_traffic                               |          0 |           1.00 |  668395.02 | 189440.49 | 177590.0 | 541411.50 |  744720.0 |  815673.75 |  938167.0 | ▁▃▃▆▇ |
| bridges\_and\_tunnels\_percent\_change\_from\_2019\_equivalent\_day |          0 |           1.00 |    \-28.59 |     20.48 |   \-80.1 |   \-43.02 |    \-20.4 |    \-13.57 |      27.4 | ▃▃▇▇▁ |

``` r
mta_data =
  mta_data %>%
  subset(select = -c(lirr_total_estimated_ridership, lirr_percent_change_from_2019_monthly_weekday_saturday_sunday_average, metro_north_total_estimated_ridership, metro_north_percent_change_from_2019_monthly_weekday_saturday_sunday_average, access_a_ride_total_scheduled_trips, access_a_ride_percent_change_from_2019_monthly_weekday_saturday_sunday_average, bridges_and_tunnels_total_traffic, bridges_and_tunnels_percent_change_from_2019_equivalent_day))
#exclude data for lirr, metronorth, access-a-ride, bridges & tunnel
```

Now we will calculate the actual 2019 ridership based on the percent
change data

``` r
mta_data %>%
  mutate( 
    '2019_subway' = subways_total_estimated_ridership/(1+(subways_percent_change_from_2019_equivalent_day/100)),
    '2019_bus'=
      buses_total_estimated_ridership/(1+(buses_percent_change_from_2019_equivalent_day/100))
    ) %>%
  rename(
    "2020_subway" = subways_total_estimated_ridership,
    "subway_pct_change" = subways_percent_change_from_2019_equivalent_day,
    "2020_bus" = buses_total_estimated_ridership,
    "bus_pct_change" = buses_percent_change_from_2019_equivalent_day
    )
```

    ## # A tibble: 268 x 7
    ##    date  `2020_subway` subway_pct_chan… `2020_bus` bus_pct_change `2019_subway`
    ##    <chr>         <dbl>            <dbl>      <dbl>          <dbl>         <dbl>
    ##  1 11/2…       1641253            -71.3     930655            -59      5718652.
    ##  2 11/2…        816461            -63.8     536678            -42      2255417.
    ##  3 11/2…       1157063            -63.7     743813            -45      3187501.
    ##  4 11/2…       1802844            -68.9    1060041            -52      5796926.
    ##  5 11/1…       1760491            -70.7    1052172            -55      6008502.
    ##  6 11/1…       1790760            -69.8    1069304            -54      5929669.
    ##  7 11/1…       1792119            -69.6    1091406            -53      5895128.
    ##  8 11/1…       1745086            -68.9    1050451            -52      5611209.
    ##  9 11/1…        821525            -66.3     524187            -48      2437760.
    ## 10 11/1…       1215461            -62.4     755379            -43      3232609.
    ## # … with 258 more rows, and 1 more variable: `2019_bus` <dbl>

Todo: 1. need to change the date to date format to sort by date 2.
statistical testing
