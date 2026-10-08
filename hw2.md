hw2_fb2731
================
fan bu
2026-10-07

# problem 1

``` r
subway_raw =
  read_csv("data/NYC_Transit_Subway_Entrance_And_Exit_Data.csv")
```

    ## Rows: 1868 Columns: 32
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (22): Division, Line, Station Name, Route1, Route2, Route3, Route4, Rout...
    ## dbl  (8): Station Latitude, Station Longitude, Route8, Route9, Route10, Rout...
    ## lgl  (2): ADA, Free Crossover
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
glimpse(subway_raw)
```

    ## Rows: 1,868
    ## Columns: 32
    ## $ Division             <chr> "BMT", "BMT", "BMT", "BMT", "BMT", "BMT", "BMT", …
    ## $ Line                 <chr> "4 Avenue", "4 Avenue", "4 Avenue", "4 Avenue", "…
    ## $ `Station Name`       <chr> "25th St", "25th St", "36th St", "36th St", "36th…
    ## $ `Station Latitude`   <dbl> 40.66040, 40.66040, 40.65514, 40.65514, 40.65514,…
    ## $ `Station Longitude`  <dbl> -73.99809, -73.99809, -74.00355, -74.00355, -74.0…
    ## $ Route1               <chr> "R", "R", "N", "N", "N", "R", "R", "R", "R", "R",…
    ## $ Route2               <chr> NA, NA, "R", "R", "R", NA, NA, NA, NA, NA, NA, NA…
    ## $ Route3               <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    ## $ Route4               <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    ## $ Route5               <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    ## $ Route6               <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    ## $ Route7               <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    ## $ Route8               <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    ## $ Route9               <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    ## $ Route10              <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    ## $ Route11              <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    ## $ `Entrance Type`      <chr> "Stair", "Stair", "Stair", "Stair", "Stair", "Sta…
    ## $ Entry                <chr> "YES", "YES", "YES", "YES", "YES", "YES", "YES", …
    ## $ `Exit Only`          <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    ## $ Vending              <chr> "YES", "YES", "YES", "YES", "YES", "YES", "YES", …
    ## $ Staffing             <chr> "FULL", "NONE", "FULL", "FULL", "FULL", "FULL", "…
    ## $ `Staff Hours`        <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    ## $ ADA                  <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, …
    ## $ `ADA Notes`          <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    ## $ `Free Crossover`     <lgl> FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,…
    ## $ `North South Street` <chr> "4th Ave", "4th Ave", "4th Ave", "4th Ave", "4th …
    ## $ `East West Street`   <chr> "25th St", "25th St", "36th St", "36th St", "36th…
    ## $ Corner               <chr> "SE", "SW", "NW", "NE", "NW", "NE", "NW", "NE", "…
    ## $ `Entrance Latitude`  <dbl> 40.66032, 40.66049, 40.65449, 40.65436, 40.65468,…
    ## $ `Entrance Longitude` <dbl> -73.99795, -73.99822, -74.00450, -74.00411, -74.0…
    ## $ `Station Location`   <chr> "(40.660397, -73.998091)", "(40.660397, -73.99809…
    ## $ `Entrance Location`  <chr> "(40.660323, -73.997952)", "(40.660489, -73.99822…

``` r
names(subway_raw)
```

    ##  [1] "Division"           "Line"               "Station Name"      
    ##  [4] "Station Latitude"   "Station Longitude"  "Route1"            
    ##  [7] "Route2"             "Route3"             "Route4"            
    ## [10] "Route5"             "Route6"             "Route7"            
    ## [13] "Route8"             "Route9"             "Route10"           
    ## [16] "Route11"            "Entrance Type"      "Entry"             
    ## [19] "Exit Only"          "Vending"            "Staffing"          
    ## [22] "Staff Hours"        "ADA"                "ADA Notes"         
    ## [25] "Free Crossover"     "North South Street" "East West Street"  
    ## [28] "Corner"             "Entrance Latitude"  "Entrance Longitude"
    ## [31] "Station Location"   "Entrance Location"

``` r
subway_df =
  subway_raw |>
  clean_names()
names(subway_df)
```

    ##  [1] "division"           "line"               "station_name"      
    ##  [4] "station_latitude"   "station_longitude"  "route1"            
    ##  [7] "route2"             "route3"             "route4"            
    ## [10] "route5"             "route6"             "route7"            
    ## [13] "route8"             "route9"             "route10"           
    ## [16] "route11"            "entrance_type"      "entry"             
    ## [19] "exit_only"          "vending"            "staffing"          
    ## [22] "staff_hours"        "ada"                "ada_notes"         
    ## [25] "free_crossover"     "north_south_street" "east_west_street"  
    ## [28] "corner"             "entrance_latitude"  "entrance_longitude"
    ## [31] "station_location"   "entrance_location"

``` r
subway_df =
  subway_df |>
  select(
    line,
    station_name,
    station_latitude,
    station_longitude,
    starts_with("route"),
    entry,
    vending,
    entrance_type,
    ada
  )
glimpse(subway_df)
```

    ## Rows: 1,868
    ## Columns: 19
    ## $ line              <chr> "4 Avenue", "4 Avenue", "4 Avenue", "4 Avenue", "4 A…
    ## $ station_name      <chr> "25th St", "25th St", "36th St", "36th St", "36th St…
    ## $ station_latitude  <dbl> 40.66040, 40.66040, 40.65514, 40.65514, 40.65514, 40…
    ## $ station_longitude <dbl> -73.99809, -73.99809, -74.00355, -74.00355, -74.0035…
    ## $ route1            <chr> "R", "R", "N", "N", "N", "R", "R", "R", "R", "R", "R…
    ## $ route2            <chr> NA, NA, "R", "R", "R", NA, NA, NA, NA, NA, NA, NA, N…
    ## $ route3            <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
    ## $ route4            <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
    ## $ route5            <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
    ## $ route6            <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
    ## $ route7            <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
    ## $ route8            <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
    ## $ route9            <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
    ## $ route10           <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
    ## $ route11           <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
    ## $ entry             <chr> "YES", "YES", "YES", "YES", "YES", "YES", "YES", "YE…
    ## $ vending           <chr> "YES", "YES", "YES", "YES", "YES", "YES", "YES", "YE…
    ## $ entrance_type     <chr> "Stair", "Stair", "Stair", "Stair", "Stair", "Stair"…
    ## $ ada               <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FAL…

``` r
subway_df =
  subway_df |>
  mutate(
    entry = case_match(
      entry,
      "YES" ~ TRUE,
      "NO" ~ FALSE
    )
  )
```

    ## Warning: There was 1 warning in `mutate()`.
    ## ℹ In argument: `entry = case_match(entry, "YES" ~ TRUE, "NO" ~ FALSE)`.
    ## Caused by warning:
    ## ! `case_match()` was deprecated in dplyr 1.2.0.
    ## ℹ Please use `recode_values()` instead.

``` r
count(subway_df, entry)
```

    ## # A tibble: 2 × 2
    ##   entry     n
    ##   <lgl> <int>
    ## 1 FALSE   115
    ## 2 TRUE   1753

``` r
dim(subway_df)
```

    ## [1] 1868   19

``` r
station_count =
  subway_df |>
  distinct(line, station_name) |>
  nrow()

station_count
```

    ## [1] 465

``` r
count(subway_df, ada)
```

    ## # A tibble: 2 × 2
    ##   ada       n
    ##   <lgl> <int>
    ## 1 FALSE  1400
    ## 2 TRUE    468

``` r
ada_station_count =
  subway_df |>
  filter(ada == TRUE) |>
  distinct(line, station_name) |>
  nrow()

ada_station_count
```

    ## [1] 84

``` r
count(subway_df, vending)
```

    ## # A tibble: 2 × 2
    ##   vending     n
    ##   <chr>   <int>
    ## 1 NO        183
    ## 2 YES      1685

``` r
no_vending_entry_prop =
  subway_df |>
  filter(vending == "NO") |>
  summarize(
    proportion = mean(entry, na.rm = TRUE)
  ) |>
  pull(proportion)

no_vending_entry_prop
```

    ## [1] 0.3770492

``` r
subway_long =
  subway_df |>
  mutate(
    across(starts_with("route"), as.character)
  ) |>
  pivot_longer(
    cols = starts_with("route"),
    names_to = "route_number",
    values_to = "route_name"
  ) |>
  filter(!is.na(route_name))

a_train_station_count =
  subway_long |>
  filter(route_name == "A") |>
  distinct(line, station_name) |>
  nrow()

a_train_station_count
```

    ## [1] 60

``` r
a_train_ada_count =
  subway_long |>
  filter(
    route_name == "A",
    ada == TRUE
  ) |>
  distinct(line, station_name) |>
  nrow()

a_train_ada_count
```

    ## [1] 17

``` r
# Problem 2

trash_file = "data/202610 Trash Wheel Collection Data.xlsx"

excel_sheets(trash_file)
```

    ## [1] "Mr. Trash Wheel"                 "Professor Trash Wheel"          
    ## [3] "Captain Trash Wheel"             "Gwynnda the Good Wheel of the W"
    ## [5] "Sampling Methodology"            "Homes powered note"

``` r
mr_trash =
  read_excel(
    trash_file,
    sheet = 1,
    range = "A2:N1000"
  ) |>
  clean_names() |>
  filter(!is.na(dumpster)) |>
  mutate(
    month = as.character(month),
    year = as.integer(parse_number(as.character(year))),
    date = as.character(date),

    across(
      any_of(c(
        "weight_tons",
        "volume_cubic_yards",
        "plastic_bottles",
        "polystyrene",
        "cigarette_butts",
        "glass_bottles",
        "plastic_bags",
        "wrappers",
        "homes_powered"
      )),
      ~ parse_number(as.character(.x))
    ),

    sports_balls =
      as.integer(
        round(
          parse_number(as.character(sports_balls))
        )
      ),

    trash_wheel = "Mr. Trash Wheel"
  )



professor_trash =
  read_excel(
    trash_file,
    sheet = 2,
    range = "A2:N1000"
  ) |>
  clean_names() |>
  filter(!is.na(dumpster)) |>
  mutate(
    month = as.character(month),
    year = as.integer(parse_number(as.character(year))),
    date = as.character(date),

    across(
      any_of(c(
        "weight_tons",
        "volume_cubic_yards",
        "plastic_bottles",
        "polystyrene",
        "cigarette_butts",
        "glass_bottles",
        "plastic_bags",
        "wrappers",
        "sports_balls",
        "homes_powered"
      )),
      ~ parse_number(as.character(.x))
    ),

    trash_wheel = "Professor Trash Wheel"
  )
```

    ## New names:
    ## • `` -> `...14`

    ## Warning: There was 1 warning in `mutate()`.
    ## ℹ In argument: `across(...)`.
    ## Caused by warning:
    ## ! 1 parsing failure.
    ## row col expected actual
    ## 135  -- a number   dive

``` r
gwynnda_trash =
  read_excel(
    trash_file,
    sheet = 4,
    range = "A2:N1000"
  ) |>
  clean_names() |>
  filter(!is.na(dumpster)) |>
  mutate(
    month = as.character(month),
    year = as.integer(parse_number(as.character(year))),
    date = as.character(date),

    across(
      any_of(c(
        "weight_tons",
        "volume_cubic_yards",
        "plastic_bottles",
        "polystyrene",
        "cigarette_butts",
        "glass_bottles",
        "plastic_bags",
        "wrappers",
        "sports_balls",
        "homes_powered"
      )),
      ~ parse_number(as.character(.x))
    ),

    trash_wheel = "Gwynnda"
  )
```

    ## New names:
    ## • `` -> `...13`
    ## • `` -> `...14`

``` r
trash_wheel_df =
  bind_rows(
    mr_trash,
    professor_trash,
    gwynnda_trash
  )

glimpse(trash_wheel_df)
```

    ## Rows: 1,269
    ## Columns: 17
    ## $ dumpster           <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, …
    ## $ month              <chr> "May", "May", "May", "May", "May", "May", "May", "M…
    ## $ year               <int> 2014, 2014, 2014, 2014, 2014, 2014, 2014, 2014, 201…
    ## $ date               <chr> "2014-05-16", "2014-05-16", "2014-05-16", "2014-05-…
    ## $ weight_tons        <dbl> 4.31, 2.74, 3.45, 3.10, 4.06, 2.71, 1.91, 3.70, 2.5…
    ## $ volume_cubic_yards <dbl> 18, 13, 15, 15, 18, 13, 8, 16, 14, 18, 15, 19, 15, …
    ## $ plastic_bottles    <dbl> 1450, 1120, 2450, 2380, 980, 1430, 910, 3580, 2400,…
    ## $ polystyrene        <dbl> 1820, 1030, 3100, 2730, 870, 2140, 1090, 4310, 2790…
    ## $ cigarette_butts    <dbl> 126000, 91000, 105000, 100000, 120000, 90000, 56000…
    ## $ glass_bottles      <dbl> 72, 42, 50, 52, 72, 46, 32, 58, 49, 75, 38, 45, 58,…
    ## $ plastic_bags       <dbl> 584, 496, 1080, 896, 368, 672, 416, 1552, 984, 448,…
    ## $ wrappers           <dbl> 1162, 874, 2032, 1971, 753, 1144, 692, 3015, 1988, …
    ## $ sports_balls       <int> 7, 5, 6, 6, 7, 5, 3, 6, 6, 7, 6, 8, 6, 6, 6, 6, 5, …
    ## $ homes_powered      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ trash_wheel        <chr> "Mr. Trash Wheel", "Mr. Trash Wheel", "Mr. Trash Wh…
    ## $ x14                <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ x13                <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…

``` r
trash_wheel_df |>
  count(trash_wheel)
```

    ## # A tibble: 3 × 2
    ##   trash_wheel               n
    ##   <chr>                 <int>
    ## 1 Gwynnda                 392
    ## 2 Mr. Trash Wheel         740
    ## 3 Professor Trash Wheel   137

``` r
nrow(trash_wheel_df)
```

    ## [1] 1269

``` r
professor_total_weight =
  trash_wheel_df |>
  filter(trash_wheel == "Professor Trash Wheel") |>
  summarize(
    total_weight = sum(weight_tons, na.rm = TRUE)
  ) |>
  pull(total_weight)

professor_total_weight
```

    ## [1] 294.96

``` r
gwynnda_june_2022 =
  trash_wheel_df |>
  filter(
    trash_wheel == "Gwynnda",
    year == 2022,
    month == "June"
  ) |>
  summarize(
    total_cigarette_butts =
      sum(cigarette_butts, na.rm = TRUE)
  ) |>
  pull(total_cigarette_butts)

gwynnda_june_2022
```

    ## [1] 18120

The combined Trash Wheel dataset contains 1269 observations. Each
observation represents a dumpster collection from Mr. Trash Wheel,
Professor Trash Wheel, or Gwynnda. Key variables include dumpster
number, collection date, weight, volume, cigarette butts, plastic
bottles, and Trash Wheel identifier. Professor Trash Wheel collected a
total of 294.96 tons of trash. Gwynnda collected 1.812^{4} cigarette
butts in June 2022.

``` r
# Problem 3

baseline_df =
  read_csv(
    "data/MCI_baseline.csv",
    skip = 1,
    na = c("", ".", "NA")
  ) |>
  clean_names()
```

    ## Rows: 483 Columns: 6
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (6): ID, Current Age, Sex, Education, apoe4, Age at onset
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
amyloid_df =
  read_csv(
    "data/mci_amyloid.csv",
    skip = 1,
    na = c("", ".", "NA")
  ) |>
  clean_names()
```

    ## Rows: 487 Columns: 6
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (5): Baseline, Time 2, Time 4, Time 6, Time 8
    ## dbl (1): Study ID
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
baseline_df =
  baseline_df |>
  mutate(
    id = as.integer(id),
    current_age = as.numeric(current_age),
    education = as.numeric(education),
    age_at_onset = as.numeric(age_at_onset),

    sex = case_match(
      sex,
      0 ~ "Female",
      1 ~ "Male"
    ),

    apoe4 = case_match(
      apoe4,
      0 ~ "Non-carrier",
      1 ~ "Carrier"
    )
  ) |>
  filter(
    is.na(age_at_onset) |
      age_at_onset > current_age
  )

glimpse(baseline_df)
```

    ## Rows: 479
    ## Columns: 6
    ## $ id           <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17…
    ## $ current_age  <dbl> 63.1, 65.6, 62.5, 69.8, 66.0, 62.5, 66.5, 67.2, 66.7, 64.…
    ## $ sex          <chr> "Female", "Female", "Male", "Female", "Male", "Male", "Ma…
    ## $ education    <dbl> 16, 20, 16, 16, 16, 16, 18, 18, 16, 18, 16, 18, 12, 20, 2…
    ## $ apoe4        <chr> "Carrier", "Carrier", "Carrier", "Non-carrier", "Non-carr…
    ## $ age_at_onset <dbl> NA, NA, 66.8, NA, 68.7, NA, 74.0, NA, NA, NA, NA, NA, 69.…

``` r
count(baseline_df, sex)
```

    ## # A tibble: 2 × 2
    ##   sex        n
    ##   <chr>  <int>
    ## 1 Female   210
    ## 2 Male     269

``` r
count(baseline_df, apoe4)
```

    ## # A tibble: 2 × 2
    ##   apoe4           n
    ##   <chr>       <int>
    ## 1 Carrier       144
    ## 2 Non-carrier   335

``` r
n_participants =
  nrow(baseline_df)

n_participants
```

    ## [1] 479

``` r
n_mci =
  baseline_df |>
  filter(!is.na(age_at_onset)) |>
  nrow()

n_mci
```

    ## [1] 93

``` r
mean_baseline_age =
  baseline_df |>
  summarize(
    mean_age = mean(current_age, na.rm = TRUE)
  ) |>
  pull(mean_age)

mean_baseline_age
```

    ## [1] 65.0286

``` r
female_apoe4_prop =
  baseline_df |>
  filter(sex == "Female") |>
  summarize(
    proportion = mean(apoe4 == "Carrier", na.rm = TRUE)
  ) |>
  pull(proportion)

female_apoe4_prop
```

    ## [1] 0.3

``` r
female_apoe4_prop * 100
```

    ## [1] 30

``` r
amyloid_long =
  amyloid_df |>
  mutate(
    across(
      c(baseline, time_2, time_4, time_6, time_8),
      as.numeric
    )
  ) |>
  pivot_longer(
    cols = c(baseline, time_2, time_4, time_6, time_8),
    names_to = "visit",
    values_to = "amyloid_42_40"
  ) |>
  mutate(
    time = case_match(
      visit,
      "baseline" ~ 0,
      "time_2" ~ 2,
      "time_4" ~ 4,
      "time_6" ~ 6,
      "time_8" ~ 8
    )
  ) |>
  select(
    study_id,
    time,
    amyloid_42_40
  )
```

    ## Warning: There were 5 warnings in `mutate()`.
    ## The first warning was:
    ## ℹ In argument: `across(c(baseline, time_2, time_4, time_6, time_8),
    ##   as.numeric)`.
    ## Caused by warning:
    ## ! NAs introduced by coercion
    ## ℹ Run `dplyr::last_dplyr_warnings()` to see the 4 remaining warnings.

``` r
glimpse(amyloid_long)
```

    ## Rows: 2,435
    ## Columns: 3
    ## $ study_id      <dbl> 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 4,…
    ## $ time          <dbl> 0, 2, 4, 6, 8, 0, 2, 4, 6, 8, 0, 2, 4, 6, 8, 0, 2, 4, 6,…
    ## $ amyloid_42_40 <dbl> 0.1105487, NA, 0.1093252, 0.1047561, 0.1072577, 0.107481…

``` r
baseline_only =
  baseline_df |>
  anti_join(
    amyloid_long |>
      distinct(study_id),
    by = c("id" = "study_id")
  )

baseline_only
```

    ## # A tibble: 8 × 6
    ##      id current_age sex    education apoe4       age_at_onset
    ##   <int>       <dbl> <chr>      <dbl> <chr>              <dbl>
    ## 1    14        58.4 Female        20 Non-carrier         66.2
    ## 2    49        64.7 Male          16 Non-carrier         68.4
    ## 3    92        68.6 Female        20 Non-carrier         NA  
    ## 4   179        68.1 Male          16 Non-carrier         NA  
    ## 5   268        61.4 Female        18 Carrier             67.5
    ## 6   304        63.8 Female        16 Non-carrier         NA  
    ## 7   389        59.3 Female        16 Non-carrier         NA  
    ## 8   412        67   Male          16 Carrier             NA

``` r
nrow(baseline_only)
```

    ## [1] 8

``` r
amyloid_only =
  amyloid_long |>
  distinct(study_id) |>
  anti_join(
    baseline_df,
    by = c("study_id" = "id")
  )

amyloid_only
```

    ## # A tibble: 16 × 1
    ##    study_id
    ##       <dbl>
    ##  1       72
    ##  2      234
    ##  3      283
    ##  4      380
    ##  5      484
    ##  6      485
    ##  7      486
    ##  8      487
    ##  9      488
    ## 10      489
    ## 11      490
    ## 12      491
    ## 13      492
    ## 14      493
    ## 15      494
    ## 16      495

``` r
nrow(amyloid_only)
```

    ## [1] 16

``` r
mci_combined =
  amyloid_long |>
  inner_join(
    baseline_df,
    by = c("study_id" = "id")
  )

glimpse(mci_combined)
```

    ## Rows: 2,355
    ## Columns: 8
    ## $ study_id      <dbl> 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 4,…
    ## $ time          <dbl> 0, 2, 4, 6, 8, 0, 2, 4, 6, 8, 0, 2, 4, 6, 8, 0, 2, 4, 6,…
    ## $ amyloid_42_40 <dbl> 0.1105487, NA, 0.1093252, 0.1047561, 0.1072577, 0.107481…
    ## $ current_age   <dbl> 63.1, 63.1, 63.1, 63.1, 63.1, 65.6, 65.6, 65.6, 65.6, 65…
    ## $ sex           <chr> "Female", "Female", "Female", "Female", "Female", "Femal…
    ## $ education     <dbl> 16, 16, 16, 16, 16, 20, 20, 20, 20, 20, 16, 16, 16, 16, …
    ## $ apoe4         <chr> "Carrier", "Carrier", "Carrier", "Carrier", "Carrier", "…
    ## $ age_at_onset  <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 66.8, 66.8, 66.8…

``` r
nrow(mci_combined)
```

    ## [1] 2355

``` r
n_distinct(mci_combined$study_id)
```

    ## [1] 471

``` r
write_csv(
  mci_combined,
  "data/mci_combined.csv"
)
```

The baseline demographic dataset was imported after skipping the first
descriptive header row. Sex and APOE4 carrier status were recoded as
categorical variables, and participants who had developed MCI at or
before baseline were excluded. The cleaned baseline dataset contains 479
eligible participants, of whom 93 developed MCI during follow-up. The
mean baseline age was 65.03 years. Among female participants, 30% were
APOE4 carriers.

The amyloid dataset contains amyloid 42/40 ratio measurements at
baseline and at 2, 4, 6, and 8 years of follow-up. I reshaped the data
from wide to long format so that each row represents one participant at
one measurement time.

There were 8 participants appearing only in the baseline dataset and 16
participants appearing only in the amyloid dataset. The final dataset
was created using an inner join so that only participants appearing in
both datasets were retained.

The final combined dataset contains 2355 longitudinal observations from
471 unique participants.
