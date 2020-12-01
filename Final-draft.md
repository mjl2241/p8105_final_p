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
mta_data = mta_data %>%
  mutate( 
    'subway_2019' = subways_total_estimated_ridership/(1+(subways_percent_change_from_2019_equivalent_day/100)),
    'bus_2019'=
      buses_total_estimated_ridership/(1+(buses_percent_change_from_2019_equivalent_day/100))
    ) %>%
  rename(
    "subway_2020" = subways_total_estimated_ridership,
    "subway_pct_change" = subways_percent_change_from_2019_equivalent_day,
    "bus_2020" = buses_total_estimated_ridership,
    "bus_pct_change" = buses_percent_change_from_2019_equivalent_day
    )
```

Let us first look at the average ridership during 2019 and 2020 for each
month

``` r
mta_ridership = mta_data%>%
  separate(date, into = c("month", "day", "year"))%>%
  mutate(month = as.numeric(month))%>%
  group_by(month)%>%
  summarize(
    avg_subway_2019 = mean(subway_2019),
    avg_subway_2020 = mean(subway_2020)
  )
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
mta_ridership%>%knitr::kable()
```

| month | avg\_subway\_2019 | avg\_subway\_2020 |
| ----: | ----------------: | ----------------: |
|     3 |           4746353 |         2381477.6 |
|     4 |           4873551 |          391671.7 |
|     5 |           4682924 |          493526.1 |
|     6 |           4883863 |          798982.6 |
|     7 |           4516652 |         1050498.6 |
|     8 |           4343573 |         1137601.2 |
|     9 |           4886223 |         1428918.6 |
|    10 |           4964620 |         1549459.3 |
|    11 |           4879758 |         1522150.0 |

Let us now compare 2019 and 2020 ridership by month and check if they
are significantly different.

First we need to create our dataframe that nests the observations for
2019 and 2020 subway ridership for each month.

``` r
mta_2019_sample = 
  mta_data%>%
  separate(date, into = c("month", "day", "year"))%>%
  mutate(month = as.numeric(month))%>%
  select(month, subway_2019)%>%
  nest(subway_2019)%>%
  mutate("subway_2019_sample" = data)%>%
  select(-data)

mta_2020_sample = 
  mta_data%>%
  separate(date, into = c("month", "day", "year"))%>%
  mutate(month = as.numeric(month))%>%
  select(month, subway_2020)%>%
  nest(subway_2020)%>%
  mutate("subway_2020_sample" = data)%>%
  select(-data)

mta_samples = 
  bind_cols(mta_2019_sample, mta_2020_sample)%>%
  select(-month...3)%>%
  rename(month = month...1)
```

    ## New names:
    ## * month -> month...1
    ## * month -> month...3

Now we need to perform a t-test to check if the ridership is
signficantly different between 2019 and 2020 data. We map across each
month to test if the values of the samples of 2019 and 2020 ridership
data differs for each month. We then compare the p-value obtained from
the tests to determine whether the differene in ridership numbers is
significant or not

``` r
mta_t_test = mta_samples%>%
  mutate(t_test = map2(.x = subway_2019_sample, .y = subway_2020_sample, ~t.test(.x , .y) ),
         t_test_results = map(t_test, broom::tidy))%>%
  select(month, t_test_results)%>%
  unnest(t_test_results)%>%
  select(month,p.value)%>%
  mutate(difference = case_when(
   
    p.value >= 0.05 ~ "insignificant",
    p.value < 0.05 ~ "significant"
    
  ))%>%
  arrange(month)

mta_t_test%>%
  knitr::kable()
```

| month | p.value | difference  |
| ----: | ------: | :---------- |
|     3 |   9e-07 | significant |
|     4 |   0e+00 | significant |
|     5 |   0e+00 | significant |
|     6 |   0e+00 | significant |
|     7 |   0e+00 | significant |
|     8 |   0e+00 | significant |
|     9 |   0e+00 | significant |
|    10 |   0e+00 | significant |
|    11 |   0e+00 | significant |

We can merge the t-test results with the average riderships from
before..

``` r
mta_year_ttest = 
  
  bind_cols(mta_ridership, mta_t_test)%>%
  select(-month...4)%>%
  rename(month = month...1)
```

    ## New names:
    ## * month -> month...1
    ## * month -> month...4

``` r
mta_year_ttest%>%knitr::kable()
```

| month | avg\_subway\_2019 | avg\_subway\_2020 | p.value | difference  |
| ----: | ----------------: | ----------------: | ------: | :---------- |
|     3 |           4746353 |         2381477.6 |   9e-07 | significant |
|     4 |           4873551 |          391671.7 |   0e+00 | significant |
|     5 |           4682924 |          493526.1 |   0e+00 | significant |
|     6 |           4883863 |          798982.6 |   0e+00 | significant |
|     7 |           4516652 |         1050498.6 |   0e+00 | significant |
|     8 |           4343573 |         1137601.2 |   0e+00 | significant |
|     9 |           4886223 |         1428918.6 |   0e+00 | significant |
|    10 |           4964620 |         1549459.3 |   0e+00 | significant |
|    11 |           4879758 |         1522150.0 |   0e+00 | significant |

Using this final dataframe, we can plot a graph showing the average
ridership for each month comparing the 2019 to 2020 ridership and then
labelling the signficance or lack of significance in ridership
differences..

\[Plot needs to be created\]
