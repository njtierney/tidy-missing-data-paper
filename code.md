#



This report was automatically generated with the R package **knitr**
(version 1.41).


```r
library("tidyverse") 
library("gridExtra")
library("ggthemes")
library("png")
library("scales")
library("simputation")
library("naniar") 
library("lubridate")
library("magick")
library("rpart")
library("visdat")
library("patchwork")
library("withr")
library("targets")
library("tidyverse") 
library("janitor")
library("knitr")
library("cowplot")
```

```r
housing_raw <-
  read_csv("data/melbourne_housing_raw.csv") %>%
  clean_names() %>%
  rename(region_name = regionname,
         property_count = propertycount) %>%
  mutate(date = dmy(date)) %>%
  rename(latitude = lattitude,
         longitude = longtitude) %>%
  # let's create monthly quarters
  mutate(yr_qtr = as.ordered(quarter(date, 
                                     with_year = TRUE,
                                     fiscal_start = 1))) %>%
  # drop price
  drop_na(price) %>%
  # make price the log of price
  mutate(price = log10(price))
```

```
## Warning: One or more parsing issues, call
## `problems()` on your data frame for
## details, e.g.:
##   dat <- vroom(...)
##   problems(dat)
```

```
## Rows: 34857 Columns: 20
## ── Column specification ─────────────────
## Delimiter: ","
## chr  (7): Suburb, Type, Method, Selle...
## dbl (13): Rooms, Price, Distance, Pos...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
# compact down the seller levels
count_dat <- count(housing_raw,seller_g) %>% 
  arrange(-n) %>%
  mutate(cumu = cumsum(n),
         pct = cumu / nrow(housing_raw),
         pct_change = pct - lag(pct)) %>%
  rowid_to_column() %>%
  mutate(seller = case_when(
    pct < 0.52 ~ as.character(seller_g),
    pct > 0.52 & pct < 0.60 ~ "seller_g1",
    pct > 0.60 & pct < 0.65 ~ "seller_g2",
    pct > 0.65 & pct < 0.70 ~ "seller_g3",
    pct > 0.70 & pct < 0.75 ~ "seller_g4",
    pct > 0.75 & pct < 0.80 ~ "seller_g5",
    pct > 0.80 & pct < 0.85 ~ "seller_g6",
    pct > 0.85 & pct < 0.90 ~ "seller_g6",
    pct > 0.90 & pct < 0.95 ~ "seller_g6",
    pct > 0.95 ~ "seller_g5"
  ))

# drop these three variables, as we won't use them in analysis, they
# would need extensive recoding

housing <- housing_raw %>%
  mutate_if(is.character, as.factor) %>%
  mutate_at(vars(rooms,
                 bedroom2,
                 bathroom,
                 car),
            as.factor) %>%
  # recode / collapse factors with many levels and few obs
  mutate(bathroom = fct_other(bathroom,
                              drop = c("4", "5", "6", "7", "8", "9"),
                              other_level = "4+")) %>%
  mutate(bedroom2 = fct_other(bedroom2,
                              drop = c("5", "6", "7", "8", "9", "10", 
                                       "12", "16", "20"),
                              other_level = "5+")) %>%
  mutate(rooms = fct_other(rooms,
                           drop = c("6", "7", "8", "9", "10", 
                                    "12", "16"),
                           other_level = "6+")) %>%
  mutate(car = fct_other(car,
                         drop = c("4", "5", "6", "7", "8", "9", "10",
                                  "11", "18"),
                         other_level = "4+")) %>%
  left_join(select(count_dat, seller_g, seller), by = "seller_g")  %>%
  # drop seller_g
  select(-seller_g) %>%
  mutate(seller = as.factor(seller))
```

```r
p1 <- ggplot(oceanbuoys,
       aes(x = humidity,
           y = air_temp_c)) +
  geom_point(size = 0.5) +
  theme(aspect.ratio = 1) + 
  labs(x = "Humidity",
       y = "Air Temperature (C)")

p1 + grid::textGrob(
  label = "Warning message:
removed 171 rows containing 
missing values (geom_point).", 
gp = grid::gpar(fontsize = 14,
                fontfamily = "Courier",
                col = "salmon", 
                fontface = "bold"))
```

<img src="figure/code-Rmdwarning-1.png" alt="How \pkg{ggplot2} behaves when displaying missing values. A warning message is displayed, but missing values are not shown in the plot." width="75%" style="display: block; margin: auto;" />


```r
gg_boxplot <-  function(data){
  ggplot(data,
       aes_string(x = "year",
                  y = "score")) +
  geom_boxplot()
}

p1 <- gg_boxplot(school_scores_all %>% filter(NA_type == "complete"))
p2 <- gg_boxplot(school_scores_all %>% filter(NA_type == "year_NA"))
p3 <- gg_boxplot(school_scores_all %>% filter(NA_type == "score_NA"))
p4 <- gg_boxplot(school_scores_all %>% filter(NA_type == "both_NA"))

plot_grid(p1, p2, p3, p4, nrow = 1, labels = LETTERS[1:4])
```

```
## Warning: Removed 11 rows containing non-finite
## values (`stat_boxplot()`).
```

```
## Warning: Removed 13 rows containing non-finite
## values (`stat_boxplot()`).
```

<img src="figure/code-Rmdgg-box-na-1.png" alt="\pkg{ggplot2} provides different visualizations depending on what type of data has missing values for data of student test scores in school year. (A) Data is complete; (B) Missings are only in year - an additional &quot;NA&quot; boxplot is created; (C) Missings only in scores, no additional missingness information is shown; (D) Missings in both scores and year, additional missing information is shown. The missingness category is only shown when there are missings in categorical variables such as year (plots (B) and (D)). In (C), no missingness information is given on the graphic, despite there being missings in score, and a warning message is displayed about the number of missing values omitted." width="100%" style="display: block; margin: auto;" />

```r
dat_ms <- tribble(~x,    ~y,     ~z,
                   1,   "A",   -100,
                   3, "N/A",    -99,
                  NA,   NA,     -98,
                 -99,  "E",    -101,
                 -98,  "F",      -1)
```

```r
tibble(
  `Function` = c(
    "add_n_miss(data)",
    "add_any_miss(data)",
    "add_prop_miss(data)",
    "add_miss_cluster(data)"
    ),
  `Adds column which:` = c(
    "contains the number missing values in a row",
    "contains whether there are any missing values in a row",
    "contains the proportion of missing values in a row",
    "contains the missing value cluster"
  )) %>% 
    kable(caption = 'Overview of the "add" functions in naniar')
```



Table: Overview of the "add" functions in naniar

|Function               |Adds column which:                                     |
|:----------------------|:------------------------------------------------------|
|add_n_miss(data)       |contains the number missing values in a row            |
|add_any_miss(data)     |contains whether there are any missing values in a row |
|add_prop_miss(data)    |contains the proportion of missing values in a row     |
|add_miss_cluster(data) |contains the missing value cluster                     |

```r
nabular(dat_ms)
```

```
## # A tibble: 5 × 6
##       x y         z x_NA  y_NA  z_NA 
##   <dbl> <chr> <dbl> <fct> <fct> <fct>
## 1     1 A      -100 !NA   !NA   !NA  
## 2     3 N/A     -99 !NA   !NA   !NA  
## 3    NA <NA>    -98 NA    NA    !NA  
## 4   -99 E      -101 !NA   !NA   !NA  
## 5   -98 F        -1 !NA   !NA   !NA
```

```r
nabular(dat_ms) %>%
  recode_shadow(x = .where(x == -99 ~ "broken_sensor"))
```

```
## # A tibble: 5 × 6
##       x y         z x_NA      y_NA  z_NA 
##   <dbl> <chr> <dbl> <fct>     <fct> <fct>
## 1     1 A      -100 !NA       !NA   !NA  
## 2     3 N/A     -99 !NA       !NA   !NA  
## 3    NA <NA>    -98 NA        NA    !NA  
## 4   -99 E      -101 NA_broke… !NA   !NA  
## 5   -98 F        -1 !NA       !NA   !NA
```

```r
aq_imputed <- nabular(airquality) %>%
  as.data.frame() %>% 
  simputation::impute_lm(Ozone ~ Temp + Wind) %>%
  simputation::impute_lm(Solar.R ~ Temp + Wind) %>%
  add_label_shadow()

head(aq_imputed)
```

```
##       Ozone  Solar.R Wind Temp Month Day
## 1  41.00000 190.0000  7.4   67     5   1
## 2  36.00000 118.0000  8.0   72     5   2
## 3  12.00000 149.0000 12.6   74     5   3
## 4  18.00000 313.0000 11.5   62     5   4
## 5 -11.67673 127.4317 14.3   56     5   5
## 6  28.00000 159.5042 14.9   66     5   6
##   Ozone_NA Solar.R_NA Wind_NA Temp_NA
## 1      !NA        !NA     !NA     !NA
## 2      !NA        !NA     !NA     !NA
## 3      !NA        !NA     !NA     !NA
## 4      !NA        !NA     !NA     !NA
## 5       NA         NA     !NA     !NA
## 6      !NA         NA     !NA     !NA
##   Month_NA Day_NA any_missing
## 1      !NA    !NA Not Missing
## 2      !NA    !NA Not Missing
## 3      !NA    !NA Not Missing
## 4      !NA    !NA Not Missing
## 5      !NA    !NA     Missing
## 6      !NA    !NA     Missing
```
<img src="figure/code-Rmdtrack-impute-example-1.png" alt="Scatterplot (A) and density plots (B and C) of ozone and solar radiation from the airquality dataset containing imputed values from a linear model. Imputed values are colored green, and data values orange. Imputed values are similar, but slightly trended to the mean." width="100%" style="display: block; margin: auto;" />

Table: Output of \pkg{dplyr} summary statistics of imputed vs non imputed values for the variable "Ozone". The "any\_missing" column denotes imputed values ("Missing", since they were previously missing), and non-imputed values ("Not Missing"). The mean and median values are similar, but the minimum and maximum values are very different.

|any_missing |       min|     mean|  median| max|
|:-----------|---------:|--------:|-------:|---:|
|Missing     | -16.86418| 41.22494| 45.4734|  78|
|Not Missing |   1.00000| 42.09910| 31.0000| 168|
<img src="figure/code-Rmdgg-miss-case-var-1.png" alt="Graphical summaries of missingness in the airquality data. Missings in variables (A) and cases (B), and for an overview of all missingness as a heatmap in (C), and with clustering applied (D). There are missing values in Ozone and Solar.R, with Ozone having more missings. Not many cases have two missings. Most missingness is from cases with one missing value. The default output (C) and ordered by clustering on rows and columns (D). These overviews are made possible using the shadow matrix in long form. There are only missings in ozone and solar radiation, and there appears to be some structure to their missingness." width="90%" style="display: block; margin: auto;" />

```r
gg_miss_upset(airquality) 
```

<img src="figure/code-Rmdairquality-upset-1.png" alt="The pattern of missingness in the airquality dataset shown in an upset plot. Only Ozone and Solar.R have missing values, and Ozone has the most missing values. There are 2 cases where both Solar.R and Ozone have missing values." style="display: block; margin: auto;" />

```r
airquality %>%
  nabular() %>%
  impute_below_all() %>%
  ggplot(aes(x = Ozone,
             fill = Ozone_NA)) + 
  geom_histogram() + 
  scale_fill_brewer(palette = "Dark2") + 
  labs(tag = "A") + 
  theme(legend.position = "bottom")

airquality %>%
  nabular() %>%
ggplot(aes(x = Temp)) + 
  geom_histogram(na.rm = TRUE) + 
  facet_wrap(~Ozone_NA) + 
  labs(tag = "B")

airquality %>%
  nabular() %>%
ggplot(aes(x = Temp,
           colour = Ozone_NA)) + 
  geom_density(na.rm = TRUE) +
  scale_colour_brewer(palette = "Dark2")  + 
  theme(legend.position = "bottom") +
  labs(tag = "C")
```

<img src="figure/code-Rmdimpute-shift-histogram-1.png" alt="Univariate summaries of missingness. (A) A histogram using nabular data to show the values and missings in ozone. Values are imputed below the range to show the number of missings in ozone and colored according to missingness of ozone (`Ozone\_NA`). There are about 35 missings in Ozone. Panel C shows temperature according to missingness in ozone from in the airquality dataset. A histogram of temperature facetted by the missingness of ozone (B), or a density of temperature colored by missingness in ozone (C). These show a cluster of low temperature observations with missing ozone values, but temperature is otherwise similar." width="75%" style="display: block; margin: auto;" /><img src="figure/code-Rmdimpute-shift-histogram-2.png" alt="Univariate summaries of missingness. (A) A histogram using nabular data to show the values and missings in ozone. Values are imputed below the range to show the number of missings in ozone and colored according to missingness of ozone (`Ozone\_NA`). There are about 35 missings in Ozone. Panel C shows temperature according to missingness in ozone from in the airquality dataset. A histogram of temperature facetted by the missingness of ozone (B), or a density of temperature colored by missingness in ozone (C). These show a cluster of low temperature observations with missing ozone values, but temperature is otherwise similar." width="75%" style="display: block; margin: auto;" /><img src="figure/code-Rmdimpute-shift-histogram-3.png" alt="Univariate summaries of missingness. (A) A histogram using nabular data to show the values and missings in ozone. Values are imputed below the range to show the number of missings in ozone and colored according to missingness of ozone (`Ozone\_NA`). There are about 35 missings in Ozone. Panel C shows temperature according to missingness in ozone from in the airquality dataset. A histogram of temperature facetted by the missingness of ozone (B), or a density of temperature colored by missingness in ozone (C). These show a cluster of low temperature observations with missing ozone values, but temperature is otherwise similar." width="75%" style="display: block; margin: auto;" />

```r
p1 <-
ggplot(data = airquality,
       aes(x = Ozone,
           y = Solar.R)) + 
  geom_miss_point() + 
  scale_colour_brewer(palette = "Dark2") + 
  theme(legend.position = "bottom") + 
  labs(tag = "A")

p2 <- 
ggplot(data = airquality,
       aes(x = Temp,
           y = Ozone)) + 
  geom_miss_point() + 
  scale_colour_brewer(palette = "Dark2") +
  theme(legend.position = "bottom")  + 
  labs(tag = "B")

gridExtra::grid.arrange(p1, p2, ncol = 2)
```

<img src="figure/code-Rmdgeom-miss-1.png" alt="Scatterplots with missings displayed at 10 percent below for the airquality dataset. Scatterplots of ozone and solar radiation (A), and ozone and temperature (B). There are missings in ozone and solar radiation, but not temperature." style="display: block; margin: auto;" />

<img src="figure/code-Rmdparallel-cord-plot-1.png" alt="Parallel coordinate plot shows missing values imputed 10\% below range for the oceanbuoys dataset. Values are colored by missingness of humidity. Humidity is missing for low air and sea temperatures, and is missing for one year and one location." width="100%" style="display: block; margin: auto;" />

Table: Single number summaries of missingness and completeness of the airquality dataset. The functions follow consistent naming, making them easy to remember, and their use clear.

|Missing function | Missing value|Complete function  | Complete value|
|:----------------|-------------:|:------------------|--------------:|
|n_miss           |         44.00|n_complete         |         874.00|
|prop_miss        |          0.05|prop_complete      |           0.95|
|pct_miss         |          4.79|pct_complete       |          95.21|
|pct_miss_case    |         27.45|prop_complete_case |          72.55|
|pct_miss_var     |         33.33|pct_complete_var   |          66.67|

```r
miss_var_summary(airquality) %>% 
  kable(
    caption = "\\texttt{miss\\char`_var\\char`_summary(airquality)} provides the number and percent of missings in each variable in airquality. Only ozone and solar radiation have missing values.",
    digits = 1)
```



Table: \texttt{miss\char`_var\char`_summary(airquality)} provides the number and percent of missings in each variable in airquality. Only ozone and solar radiation have missing values.

|variable | n_miss| pct_miss|
|:--------|------:|--------:|
|Ozone    |     37|     24.2|
|Solar.R  |      7|      4.6|
|Wind     |      0|      0.0|
|Temp     |      0|      0.0|
|Month    |      0|      0.0|
|Day      |      0|      0.0|

```r
miss_var_table(airquality) %>% 
  kable(
    caption = "The output of \\code{miss\\char`_var\\char`_table(airquality)}, tabulating the amount of missing data in each variable in airquality. This shows the number of variables with 0, 7, and 37 missings, and the percentage of variables with those amounts of missingness. There are few missingness patterns.",
    digits = 1)
```



Table: The output of \code{miss\char`_var\char`_table(airquality)}, tabulating the amount of missing data in each variable in airquality. This shows the number of variables with 0, 7, and 37 missings, and the percentage of variables with those amounts of missingness. There are few missingness patterns.

| n_miss_in_var| n_vars| pct_vars|
|-------------:|------:|--------:|
|             0|      4|     66.7|
|             7|      1|     16.7|
|            37|      1|     16.7|

```r
airquality %>%
  group_by(Month) %>%
  miss_var_summary() %>%
  ungroup() %>%
  slice(1:10) %>%
  kable(caption = "Output of \\code{airquality \\%>\\% group\\char`_by(Month) \\%>\\% miss\\char`_var\\char`_summary()} provides a grouped summary of the missingness in each variable, for each month of the airquality dataset. Only the first 10 rows are shown. There are more ozone missings in June than May.",
               digits = 1)
```



Table: Output of \code{airquality \%>\% group\char`_by(Month) \%>\% miss\char`_var\char`_summary()} provides a grouped summary of the missingness in each variable, for each month of the airquality dataset. Only the first 10 rows are shown. There are more ozone missings in June than May.

| Month|variable | n_miss| pct_miss|
|-----:|:--------|------:|--------:|
|     5|Ozone    |      5|     16.1|
|     5|Solar.R  |      4|     12.9|
|     5|Wind     |      0|      0.0|
|     5|Temp     |      0|      0.0|
|     5|Day      |      0|      0.0|
|     6|Ozone    |     21|     70.0|
|     6|Solar.R  |      0|      0.0|
|     6|Wind     |      0|      0.0|
|     6|Temp     |      0|      0.0|
|     6|Day      |      0|      0.0|

```r
q1 <- gg_miss_var(housing, show_pct = TRUE) + labs(tag = "A")

q2 <- gg_miss_case(housing, show_pct = TRUE) + labs(tag = "B")

gridExtra::grid.arrange(q1, q2, ncol = 2)
```

<img src="figure/code-Rmdhousing-miss-case-var-1.png" alt="The amount of missings in variables (A) and cases (B) for Melbourne housing data. (A) Build area and year built have more than 50\% missing, and car, bathroom, bedroom2 and longitude and latitude have about 25\% missings. (B) Cases are missing 5 - 50\% of values. The majority of missingness is in selected cases and variables." width="100%" style="display: block; margin: auto;" />

```r
vis_miss(housing, cluster = TRUE, sort_miss = TRUE, show_perc_col = FALSE)
```

<img src="figure/code-Rmdapplic-vis-miss-1.png" alt="Heatmap of clustered missingness for housing data reveals structured missingness. Three groups of missingness are apparent. At the top: building area to longitude; the middle: building area and year built; the end: building area, year built, and landsize." width="85%" style="display: block; margin: auto;" />

```r
gg_miss_upset(housing, 
              nsets = 8,
              order.by = "freq")
```

<img src="figure/code-Rmdhousing-upset-1.png" alt="An upset plot of 8 sets of missingness in the housing data. Missingness for each variable is shown on the bottom left. Connected dots show co-occurences of missings in variables. Two missingness patterns are clear, year built and building area, and lattitude through to building area." width="100%" style="display: block; margin: auto;" />

```r
t1 <-  miss_var_table(housing) 
t2 <- miss_case_table(housing)

kable(list(t1, t2),
             caption = "Summary tables to help understand missingness patters. Output of \\code{miss\\_var\\_table(housing)} (left), tabulating missingness for variables, and output of \\code{miss\\_case\\_table(housing)}. There are 13 variables with 0-3 missings, 6 variables have 6000-10000 missings, 2 variables have 15000 – 17000 missings. About 30\\% of cases have no missings, 45\\% of cases have 1 - 6 missings, and about 23\\% of cases have 8 or more missings. There are different patterns of missingness in variables and cases, but they can be broken down into smaller groups.",
             digits = 1)
```



Table: Summary tables to help understand missingness patters. Output of \code{miss\_var\_table(housing)} (left), tabulating missingness for variables, and output of \code{miss\_case\_table(housing)}. There are 13 variables with 0-3 missings, 6 variables have 6000-10000 missings, 2 variables have 15000 – 17000 missings. About 30\% of cases have no missings, 45\% of cases have 1 - 6 missings, and about 23\% of cases have 8 or more missings. There are different patterns of missingness in variables and cases, but they can be broken down into smaller groups.

| n_miss_in_var| n_vars| pct_vars|
|-------------:|------:|--------:|
|             0|     10|     47.6|
|             1|      2|      9.5|
|             3|      1|      4.8|
|          6254|      2|      9.5|
|          6441|      1|      4.8|
|          6447|      1|      4.8|
|          6824|      1|      4.8|
|          9265|      1|      4.8|
|         15163|      1|      4.8|
|         16591|      1|      4.8|

| n_miss_in_case| n_cases| pct_cases|
|--------------:|-------:|---------:|
|              0|    8887|      32.6|
|              1|    3237|      11.9|
|              2|    7231|      26.5|
|              3|    1370|       5.0|
|              4|      79|       0.3|
|              5|       8|       0.0|
|              6|     203|       0.7|
|              8|    6229|      22.9|
|              9|       2|       0.0|
|             11|       1|       0.0|

```r
             # booktabs = TRUE)
  # kable_styling(latex_options = c("hold_position"))
```

```r
housing_cls <- add_miss_cluster(housing, n_clusters = 2)
```

```r
miss_cls_pre_fit <- housing_cls %>%
  mutate(year = lubridate::year(date),
         quarter = lubridate::quarter(date)) %>%
  mutate_at(vars(year, quarter),
            as.factor) %>%
  select(suburb,
         council_area,
         postcode,
         method, 
         region_name,
         rooms,
         seller,
         type,
         year,
         quarter,
         price,
         property_count,
         distance,
         latitude,
         longitude,
         miss_cluster)
miss_cls_fit_rpart <- rpart(factor(miss_cluster) ~ ., 
                            data = miss_cls_pre_fit)
```

```r
rpart.plot::prp(miss_cls_fit_rpart,
                type = 4,
                extra = 2,
                fallen.leaves = TRUE,
                prefix = "cluster = ",
                suffix = " \nObservations",
                box.col = "lightgrey",
                border.col = "grey",
                branch.col = "grey40")
```

<img src="figure/code-Rmdrpart-plot-1.png" alt="Decision tree output predicting missingness clusters. Type of house, year quarter, and year were important for predicting missingness cluster. The cluster with the most missingness was for quarters 1 and 4, for 2017 and 2018. Type of house, year, and year quarter are important features related to missingness structure." width="90%" style="display: block; margin: auto;" />

```r
data_in <- housing_cls %>%
  mutate(year = lubridate::year(date),
         month = lubridate::month(date),
         quarter = lubridate::quarter(date)) %>%
  mutate_at(vars(year, month, quarter),
            as.factor)

node_1 <- data_in %>% 
  filter(type == "h")

node_2 <- data_in %>% 
  filter(type != "h",
         quarter == 2 | quarter == 3) 

node_3 <- data_in %>%
  filter(type != "h",
         quarter == 1 | quarter == 4,
         year == 2016)

node_4 <- data_in %>%
  filter(type != "h",
         quarter == 1 | quarter == 4,
         year == 2017 | year == 2018)
```


```r
set.seed(2022-12-1)
dat_house_lm <- dat_house %>%
  nabular() %>%
  as.data.frame() %>%
  # postcode
  impute_knn(postcode ~ type + quarter + year + rooms + price + distance) %>%
  # distance
  impute_lm(distance ~ type + quarter + year + rooms + price) %>%
  # property_count
  impute_lm(property_count ~ type + quarter + year + rooms + price + distance) %>%
  # longitude
  impute_lm(longitude ~ type + quarter + year + rooms + price + distance) %>%
  # latitude
  impute_lm(latitude ~  type + quarter + year + rooms + price + distance) %>%
  # bedroom2
  impute_lm(bedroom2 ~  type + quarter + year + rooms + price + distance) %>%
  # bathroom
  impute_lm(bathroom ~  type + quarter + year + rooms + price + distance) %>%
  # car
  impute_lm(car ~ type + quarter + year + rooms + price + distance) %>%
  # landsize
  impute_lm(landsize ~  type + quarter + year + rooms + price + distance) %>%
  add_label_shadow() %>%
  # recode integers back
  mutate_at(vars(bedroom2, bathroom, car), as.integer)
```

```r
dat_house_cc <- dat_house %>% nabular() %>% add_label_shadow() %>% na.omit()

bound_models <- bind_rows(lm = dat_house_lm,
                          knn = dat_house_knn,
                          cc = dat_house_cc,
                          .id = "imp_model") %>%
  as_tibble()
```

```r
## Now we want to look at the distribution of the values, so we need to do
## some gathering
bound_models_gather <- bound_models %>%
  select(
         bedroom2,
         bathroom,
         car,
         landsize,
         any_missing,
         imp_model) %>%
  gather(key = "key",
         value = "value",
         -any_missing,
         -imp_model)

p1 <- bound_models %>%
  ggplot(aes(x = imp_model,
             y = bedroom2)) + 
  geom_boxplot()

p2 <- bound_models %>%
  ggplot(aes(x = imp_model,
             y = bathroom)) + 
  geom_boxplot()
  
p3 <- bound_models %>%
  ggplot(aes(x = imp_model,
             y = car)) + 
  geom_boxplot()

p4 <- bound_models %>%
  ggplot(aes(x = imp_model,
             y = landsize)) + 
  geom_boxplot() + 
  scale_y_log10()
  
cowplot::plot_grid(p1,
          p2,
          p3,
          p4,
          nrow = 1,
          labels = "AUTO")
```

```
## Warning in self$trans$transform(x): NaNs
## produced
```

```
## Warning: Transformation introduced infinite
## values in continuous y-axis
```

```
## Warning: Removed 5891 rows containing non-finite
## values (`stat_boxplot()`).
```

<img src="figure/code-Rmdimputed-by-model-1.png" alt="Boxplots of complete case data, and data imputed with KNN or linear model for different variables. (A) number of bedrooms, (B) number of bathrooms, (C) number of carspots, and (D) landsize (on a log10 scale). KNN had similar results to complete case, and linear model had a lower median for cars and fewer extreme values for bedrooms." width="95%" style="display: block; margin: auto;" />

```r
set.seed(2022-12-1)
dat_fit <- dat_house %>%
  mutate_at(vars(car,
                 bathroom,
                 bedroom2,
                 type,
                 year,
                 quarter,
                 rooms),
            .funs = as.factor)

dat_fit_knn <-  dat_house_knn %>%
  mutate_at(vars(car,
                 bathroom,
                 bedroom2,
                 type,
                 year,
                 quarter,
                 rooms),
            .funs = as.factor)

dat_fit_lm <-  dat_house_lm %>%
  mutate_at(vars(car,
                 bathroom,
                 bedroom2,
                 type,
                 year,
                 quarter,
                 rooms),
            .funs = as.factor)

dat_house_cc <- na.omit(dat_fit)

fit_lm_dat_house_cc <- lm(price ~ landsize + 
                            car + 
                            bathroom + 
                            bedroom2 +
                            type + 
                            year + 
                            quarter +
                            rooms +
                            distance,
                          dat_house_cc)

fit_lm_dat_house_knn <- lm(price ~ landsize + 
                            car + 
                            bathroom + 
                            bedroom2 +
                            type + 
                            year + 
                            quarter +
                            rooms +
                            distance,
                           dat_fit_knn)

fit_lm_dat_house_lm <- lm(price ~ landsize + 
                            car + 
                            bathroom + 
                            bedroom2 +
                            type + 
                            year + 
                            quarter +
                            rooms +
                            distance,
                          dat_fit_lm)
```

```r
tidy_fit_cc <- broom::tidy(fit_lm_dat_house_cc)
tidy_fit_knn <- broom::tidy(fit_lm_dat_house_knn)
tidy_fit_lm <- broom::tidy(fit_lm_dat_house_lm)

tidy_fits <- bind_rows(cc = tidy_fit_cc,
                       knn = tidy_fit_knn,
                       lm = tidy_fit_lm,
                       .id = "model")

tidy_fits_data <-  tidy_fits %>%
  filter(term != "(Intercept)") %>%
  filter(grepl("rooms", term)) %>%
  select(model, term, estimate) %>%
  spread(key = model,
         value = estimate)

tidy_fits %>%
  filter(term != "(Intercept)") %>%
  filter(grepl("rooms", term)) %>%
  select(model, term, estimate) %>%
  ggplot(aes(x = estimate,
             y = term,
             colour = model)) + 
  geom_point() +
  theme_minimal() +
  labs(x = "Estimate",
       y = "Term") + 
  guides(colour = guide_legend(title = "Model"))
```

<img src="figure/code-Rmdtidy-coefs-1.png" alt="The coefficient estimate for the number of rooms varies according to the imputed dataset. Complete case dataset produced lower coefficients, compared to imputed datasets." width="100%" style="display: block; margin: auto;" />

```r
aug_fit_cc <-  broom::augment(fit_lm_dat_house_cc) %>% as_tibble()
aug_fit_knn <- broom::augment(fit_lm_dat_house_knn) %>% as_tibble()
aug_fit_lm <-  broom::augment(fit_lm_dat_house_lm)%>% as_tibble()

aug_fits <- bind_rows(cc =  aug_fit_cc,
                      knn = aug_fit_knn,
                      lm =  aug_fit_lm,
                      .id = "model")

gg_hex <- function(data,
                   subset){
  
  data_subset <- data %>% 
    filter(model == subset)
  
    ggplot(data_subset, aes(x = .fitted,
               y = .resid)) + 
    geom_hex() + 
    scale_fill_viridis_c() +
    theme(legend.position = "bottom",
          aspect.ratio = 1) +
      labs(tag = subset)
}

gg_hex(aug_fits, "cc") +
gg_hex(aug_fits, "knn") +
gg_hex(aug_fits, "lm")
```

```
## Warning: Computation failed in `stat_binhex()`
## Computation failed in `stat_binhex()`
## Computation failed in `stat_binhex()`
## Caused by error in `compute_group()`:
## ! The package `hexbin` is
##   required for `stat_binhex()`
```

<img src="figure/code-Rmdpartial-resid-1.png" alt="Partial residual plot for each data set, complete cases (cc), and imputed with KNN (knn) or linear model (lm). These are plotted as hex bins, colored according to the number of points in a given hexagon. Brighter colors mean more points. Compared to complete cases, imputed data has more points clustered around zero." width="100%" style="display: block; margin: auto;" />

The R session information (including the OS info, R version and all
packages used):


```r
sessionInfo()
```

```
## R version 4.2.1 (2022-06-23)
## Platform: aarch64-apple-darwin20 (64-bit)
## Running under: macOS Monterey 12.3.1
## 
## Matrix products: default
## LAPACK: /Library/Frameworks/R.framework/Versions/4.2-arm64/Resources/lib/libRlapack.dylib
## 
## locale:
## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
## 
## attached base packages:
## [1] stats     graphics  grDevices
## [4] utils     datasets  methods  
## [7] base     
## 
## other attached packages:
##  [1] cowplot_1.1.1     knitr_1.41       
##  [3] janitor_2.1.0     targets_0.14.0   
##  [5] withr_2.5.0       patchwork_1.1.2  
##  [7] visdat_0.6.0.9000 rpart_4.1.19     
##  [9] magick_2.7.3      lubridate_1.9.0  
## [11] timechange_0.1.1  naniar_0.6.1     
## [13] simputation_0.2.8 scales_1.2.1     
## [15] png_0.1-7         ggthemes_4.2.4   
## [17] gridExtra_2.3     forcats_0.5.2    
## [19] stringr_1.4.1     dplyr_1.0.10     
## [21] purrr_0.3.5       readr_2.1.3      
## [23] tidyr_1.2.1       tibble_3.1.8     
## [25] ggplot2_3.4.0     tidyverse_1.3.2  
## [27] gert_1.9.1        clipr_0.8.0      
## [29] pak_0.3.1         fs_1.5.2         
## [31] testthat_3.1.5    devtools_2.4.5   
## [33] usethis_2.1.6    
## 
## loaded via a namespace (and not attached):
##   [1] googledrive_2.0.0  
##   [2] colorspace_2.0-3   
##   [3] ellipsis_0.3.2     
##   [4] rprojroot_2.0.3    
##   [5] snakecase_0.11.0   
##   [6] rstudioapi_0.14    
##   [7] farver_2.1.1       
##   [8] remotes_2.4.2      
##   [9] bit64_4.0.5        
##  [10] fansi_1.0.3        
##  [11] xml2_1.3.3         
##  [12] codetools_0.2-18   
##  [13] cachem_1.0.6       
##  [14] pkgload_1.3.2      
##  [15] jsonlite_1.8.3     
##  [16] broom_1.0.1        
##  [17] dbplyr_2.2.1       
##  [18] shiny_1.7.3        
##  [19] compiler_4.2.1     
##  [20] httr_1.4.4         
##  [21] backports_1.4.1    
##  [22] assertthat_0.2.1   
##  [23] fastmap_1.1.0      
##  [24] gargle_1.2.1       
##  [25] cli_3.4.1          
##  [26] later_1.3.0        
##  [27] htmltools_0.5.3    
##  [28] prettyunits_1.1.1  
##  [29] tools_4.2.1        
##  [30] igraph_1.3.5       
##  [31] gtable_0.3.1       
##  [32] glue_1.6.2         
##  [33] tinytex_0.42       
##  [34] Rcpp_1.0.9         
##  [35] cellranger_1.1.0   
##  [36] vctrs_0.5.1        
##  [37] gower_1.0.0        
##  [38] xfun_0.35          
##  [39] ps_1.7.2           
##  [40] brio_1.1.3         
##  [41] rvest_1.0.3        
##  [42] mime_0.12          
##  [43] miniUI_0.1.1.1     
##  [44] lifecycle_1.0.3    
##  [45] sys_3.4.1          
##  [46] googlesheets4_1.0.1
##  [47] MASS_7.3-58.1      
##  [48] vroom_1.6.0        
##  [49] ragg_1.2.4         
##  [50] hms_1.1.2          
##  [51] promises_1.2.0.1   
##  [52] credentials_1.3.2  
##  [53] parallel_4.2.1     
##  [54] RColorBrewer_1.1-3 
##  [55] rpart.plot_3.1.1   
##  [56] yaml_2.3.6         
##  [57] curl_4.3.3         
##  [58] memoise_2.0.1      
##  [59] UpSetR_1.4.0       
##  [60] stringi_1.7.8      
##  [61] highr_0.9          
##  [62] pkgbuild_1.4.0     
##  [63] systemfonts_1.0.4  
##  [64] rlang_1.0.6        
##  [65] pkgconfig_2.0.3    
##  [66] evaluate_0.18      
##  [67] labeling_0.4.2     
##  [68] htmlwidgets_1.5.4  
##  [69] bit_4.0.5          
##  [70] processx_3.8.0     
##  [71] tidyselect_1.2.0   
##  [72] here_1.0.1         
##  [73] plyr_1.8.8         
##  [74] magrittr_2.0.3     
##  [75] bookdown_0.30      
##  [76] R6_2.5.1           
##  [77] generics_0.1.3     
##  [78] profvis_0.3.7      
##  [79] base64url_1.4      
##  [80] DBI_1.1.3          
##  [81] pillar_1.8.1       
##  [82] haven_2.5.1        
##  [83] modelr_0.1.10      
##  [84] crayon_1.5.2       
##  [85] utf8_1.2.2         
##  [86] tzdb_0.3.0         
##  [87] rmarkdown_2.18     
##  [88] urlchecker_1.0.1   
##  [89] grid_4.2.1         
##  [90] readxl_1.4.1       
##  [91] data.table_1.14.6  
##  [92] callr_3.7.3        
##  [93] reprex_2.0.2       
##  [94] digest_0.6.30      
##  [95] xtable_1.8-4       
##  [96] httpuv_1.6.6       
##  [97] textshaping_0.3.6  
##  [98] openssl_2.0.4      
##  [99] munsell_0.5.0      
## [100] viridisLite_0.4.1  
## [101] sessioninfo_1.2.2  
## [102] askpass_1.1
```

```r
Sys.time()
```

```
## [1] "2022-12-02 10:56:53 AWST"
```

