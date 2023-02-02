## ----libs---------------------------------------------------------------------
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


## ----data-----------------------------------------------------------------
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

## ----warning, fig.cap = "How \\pkg{ggplot2} behaves when displaying missing values. A warning message is displayed, but missing values are not shown in the plot.", fig.height = 4, fig.width = 8, out.width = "75%", warning = FALSE, fig.show = 'hold'----
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

  

## ----make-school-scores-data, echo = FALSE------------------------------------
set.seed(2022-11-29)
school_scores <- data.frame(score = c(rnorm(20, 70, 10),
                                 rnorm(20, 85, 7.5),
                                 rnorm(20, 95, 5),
                                 rnorm(20, 115, 10)),
                       year = rep(c("1st", "2nd", "3rd", "4th"), each = 20)) %>%
  mutate(score = as.numeric(score),
         year = as.factor(year)) %>%
  as_tibble()

school_scores_year_miss <- school_scores %>%
  mutate_at(.funs = function(x){
    x[sample(c(TRUE, NA), 
             prob = c(0.85, 0.150), 
             size = length(x), 
             replace = TRUE)]
  },
  .vars = "year")


school_scores_score_miss <- school_scores %>%
  mutate_at(.funs = function(x){
    x[sample(c(TRUE, NA), 
             prob = c(0.85, 0.150), 
             size = length(x), 
             replace = TRUE)]
  },
  .vars = "score")


school_scores_both_miss <- school_scores %>%
  mutate_all(.funs = function(x){
    x[sample(c(TRUE, NA), 
             prob = c(0.85, 0.150), 
             size = length(x), 
             replace = TRUE)]
  })

school_scores_all <- bind_rows(
  complete = school_scores,
  year_NA = school_scores_year_miss,
  score_NA = school_scores_score_miss,
  both_NA = school_scores_both_miss,
  .id = "NA_type"
)


## ----gg-box-na, fig.height = 3, fig.width = 9, out.width = "100%", fig.align = "center", fig.cap = '\\pkg{ggplot2} provides different visualizations depending on what type of data has missing values for data of student test scores in school year. (A) Data is complete; (B) Missings are only in year - an additional "NA" boxplot is created; (C) Missings only in scores, no additional missingness information is shown; (D) Missings in both scores and year, additional missing information is shown. The missingness category is only shown when there are missings in categorical variables such as year (plots (B) and (D)). In (C), no missingness information is given on the graphic, despite there being missings in score, and a warning message is displayed about the number of missing values omitted.', fig.show = "asis"----
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


## ----setup-miss-scan-count----------------------------------------------------
dat_ms <- tribble(~x,    ~y,     ~z,
                   1,   "A",   -100,
                   3, "N/A",    -99,
                  NA,   NA,     -98,
                 -99,  "E",    -101,
                 -98,  "F",      -1)

## ----add-missing-info---------------------------------------------------------
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


## ----bind-shadow, echo = TRUE-------------------------------------------------
nabular(dat_ms)


## ----recode-shadow, eval = TRUE, echo = TRUE----------------------------------
nabular(dat_ms) %>%
  recode_shadow(x = .where(x == -99 ~ "broken_sensor"))


## ----bind-impute-label-example, echo = TRUE-----------------------------------
aq_imputed <- nabular(airquality) %>%
  as.data.frame() %>% 
  simputation::impute_lm(Ozone ~ Temp + Wind) %>%
  simputation::impute_lm(Solar.R ~ Temp + Wind) %>%
  add_label_shadow()

head(aq_imputed)


## ----track-impute-example, fig.show = "hold", fig.cap = "Scatterplot (A) and density plots (B and C) of ozone and solar radiation from the airquality dataset containing imputed values from a linear model. Imputed values are colored green, and data values orange. Imputed values are similar, but slightly trended to the mean.", fig.height = 4, fig.width = 12, out.width = "100%", echo = FALSE----
p1 <- 
ggplot(aq_imputed,
       aes(x = Ozone,
           y = Solar.R,
           color = any_missing)) + 
  geom_point() +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "none") + 
  labs(tag = "A")

p2 <- 
ggplot(aq_imputed,
       aes(x = Ozone,
           fill = any_missing)) + 
  geom_density(alpha = 0.3) + 
  scale_fill_brewer(palette = "Dark2") +
  theme(legend.position = "none") + 
  labs(tag = "B")

p3 <- 
ggplot(aq_imputed,
       aes(x = Solar.R,
           fill = any_missing)) + 
  geom_density(alpha = 0.3) + 
  scale_fill_brewer(palette = "Dark2") + 
  theme(legend.position = "none") + 
  labs(tag = "C")

gridExtra::grid.arrange(p1, p2, p3, nrow = 1)


## ----impute-summary-out, echo = FALSE-----------------------------------------
aq_imputed %>%
  group_by(any_missing) %>%
  summarise_at(.vars = vars(Ozone),
               .funs = lst(min, mean, median, max))  %>% 
  kable(caption = 'Output of \\pkg{dplyr} summary statistics of imputed vs non imputed values for the variable "Ozone". The "any\\_missing" column denotes imputed values ("Missing", since they were previously missing), and non-imputed values ("Not Missing"). The mean and median values are similar, but the minimum and maximum values are very different.')


## ----gg-miss-case-var, echo = FALSE, fig.show='hold', fig.cap = "Graphical summaries of missingness in the airquality data. Missings in variables (A) and cases (B), and for an overview of all missingness as a heatmap in (C), and with clustering applied (D). There are missing values in Ozone and Solar.R, with Ozone having more missings. Not many cases have two missings. Most missingness is from cases with one missing value. The default output (C) and ordered by clustering on rows and columns (D). These overviews are made possible using the shadow matrix in long form. There are only missings in ozone and solar radiation, and there appears to be some structure to their missingness.", fig.height = 7, fig.width = 6, out.width = "90%", fig.align="center"----
library("patchwork")
aq_miss_var <- gg_miss_var(airquality) + labs(title = "A")

aq_miss_case <- gg_miss_case(airquality) + labs(title = "B")

aq_vis_miss <- vis_miss(airquality) + labs(title = "C")

aq_vis_miss_cluster <- vis_miss(airquality, 
                                 cluster = TRUE) + labs(title = "D")

(aq_miss_var | aq_miss_case) / (aq_vis_miss | aq_vis_miss_cluster) 


## ----airquality-upset, fig.height = 2, fig.width = 3, fig.cap = "The pattern of missingness in the airquality dataset shown in an upset plot. Only Ozone and Solar.R have missing values, and Ozone has the most missing values. There are 2 cases where both Solar.R and Ozone have missing values."----
gg_miss_upset(airquality) 


## ----impute-shift-histogram, fig.height = 2.5, fig.width = 5, fig.cap = "Univariate summaries of missingness. (A) A histogram using nabular data to show the values and missings in ozone. Values are imputed below the range to show the number of missings in ozone and colored according to missingness of ozone (`Ozone\\_NA`). There are about 35 missings in Ozone. Panel C shows temperature according to missingness in ozone from in the airquality dataset. A histogram of temperature facetted by the missingness of ozone (B), or a density of temperature colored by missingness in ozone (C). These show a cluster of low temperature observations with missing ozone values, but temperature is otherwise similar.", error = FALSE, warning = FALSE, message = FALSE, out.width = "75%", fig.show = 'hold'----
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


## ----geom-miss, fig.height = 3, fig.width = 6, fig.cap = "Scatterplots with missings displayed at 10 percent below for the airquality dataset. Scatterplots of ozone and solar radiation (A), and ozone and temperature (B). There are missings in ozone and solar radiation, but not temperature."----
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


## ----geom-miss-point-impute-shift-long, include = FALSE, out.width = "70%"----
airquality %>%
  nabular() %>%
  impute_below_all() %>%
  add_label_shadow() %>%
  ggplot(aes(x = Ozone,
             y = Solar.R,
             colour = any_missing)) + 
  geom_point() + 
  scale_colour_brewer(palette = "Dark2") + 
  theme(legend.position = "bottom")


## ----parallel-cord-plot, fig.width = 8, fig.height = 4, out.width = "100%", echo = FALSE, fig.cap = "Parallel coordinate plot shows missing values imputed 10\\% below range for the oceanbuoys dataset. Values are colored by missingness of humidity. Humidity is missing for low air and sea temperatures, and is missing for one year and one location."----
range_01 <- function(x, ...){(x - min(x, ...)) / (max(x, ...) - min(x, ...))}

dat_paral <-  oceanbuoys %>%
  mutate(ID = 1:n(),
         ID = as.factor(ID)) %>%
  nabular() %>%
  impute_below_all() %>%
  add_label_shadow() %>%
  mutate_if(is.numeric, range_01) %>%
  gather(key, value, -c(9:19)) %>%
  select(ID, key, value, everything())

ggplot(dat_paral,
       aes(x = key,
           y = value,
           group = ID,
           colour = humidity_NA)) + 
  geom_line(alpha = 0.3) + 
  theme(legend.position = "bottom") + 
  scale_colour_brewer(palette = "Dark2")


## ----n-prop-pct-miss-complete, echo = FALSE-----------------------------------
table_of_fun <- tibble(
  "Missing function" = c("n_miss", 
                         "prop_miss", 
                         "pct_miss",
                         "pct_miss_case",
                         "pct_miss_var"),
  "Missing value" = c(n_miss(airquality),
                      prop_miss(airquality),
                      pct_miss(airquality),
                      pct_miss_case(airquality),
                      pct_miss_var(airquality)),
  "Complete function" = c("n_complete",
                          "prop_complete",
                          "pct_complete",
                          "prop_complete_case",
                          "pct_complete_var"),
  "Complete value" = c(n_complete(airquality), 
                       prop_complete(airquality), 
                       pct_complete(airquality),
                       pct_complete_case(airquality), 
                       pct_complete_var(airquality))
) %>%
  mutate_if(is.numeric,round,2)

kable(
  table_of_fun, 
  digits = 2,
  caption = "Single number summaries of missingness and completeness of the airquality dataset. The functions follow consistent naming, making them easy to remember, and their use clear.")


## ----miss-var-summary---------------------------------------------------------
miss_var_summary(airquality) %>% 
  kable(
    caption = "\\texttt{miss\\char`_var\\char`_summary(airquality)} provides the number and percent of missings in each variable in airquality. Only ozone and solar radiation have missing values.",
    digits = 1)


## ----miss-var-table-----------------------------------------------------------
miss_var_table(airquality) %>% 
  kable(
    caption = "The output of \\code{miss\\char`_var\\char`_table(airquality)}, tabulating the amount of missing data in each variable in airquality. This shows the number of variables with 0, 7, and 37 missings, and the percentage of variables with those amounts of missingness. There are few missingness patterns.",
    digits = 1)


## ----group-miss-var-summary---------------------------------------------------
airquality %>%
  group_by(Month) %>%
  miss_var_summary() %>%
  ungroup() %>%
  slice(1:10) %>%
  kable(caption = "Output of \\code{airquality \\%>\\% group\\char`_by(Month) \\%>\\% miss\\char`_var\\char`_summary()} provides a grouped summary of the missingness in each variable, for each month of the airquality dataset. Only the first 10 rows are shown. There are more ozone missings in June than May.",
               digits = 1)


## ----housing-miss-case-var, fig.cap = "The amount of missings in variables (A) and cases (B) for Melbourne housing data. (A) Build area and year built have more than 50\\% missing, and car, bathroom, bedroom2 and longitude and latitude have about 25\\% missings. (B) Cases are missing 5 - 50\\% of values. The majority of missingness is in selected cases and variables.", fig.height = 4, fig.width = 8, out.width = "100%"----
q1 <- gg_miss_var(housing, show_pct = TRUE) + labs(tag = "A")

q2 <- gg_miss_case(housing, show_pct = TRUE) + labs(tag = "B")

gridExtra::grid.arrange(q1, q2, ncol = 2)


## ----applic-vis-miss, fig.height = 4, fig.width = 6, out.width = "85%", fig.show='hold', fig.cap = "Heatmap of clustered missingness for housing data reveals structured missingness. Three groups of missingness are apparent. At the top: building area to longitude; the middle: building area and year built; the end: building area, year built, and landsize.", dev = "png", dpi = 300----
vis_miss(housing, cluster = TRUE, sort_miss = TRUE, show_perc_col = FALSE)



## ----housing-upset, fig.height = 5, fig.width = 8, fig.cap = "An upset plot of 8 sets of missingness in the housing data. Missingness for each variable is shown on the bottom left. Connected dots show co-occurences of missings in variables. Two missingness patterns are clear, year built and building area, and lattitude through to building area.", out.width = "100%"----
gg_miss_upset(housing, 
              nsets = 8,
              order.by = "freq")


## ----housing-miss-var-case-table----------------------------------------------
t1 <-  miss_var_table(housing) 
t2 <- miss_case_table(housing)

kable(list(t1, t2),
             caption = "Summary tables to help understand missingness patters. Output of \\code{miss\\_var\\_table(housing)} (left), tabulating missingness for variables, and output of \\code{miss\\_case\\_table(housing)}. There are 13 variables with 0-3 missings, 6 variables have 6000-10000 missings, 2 variables have 15000 – 17000 missings. About 30\\% of cases have no missings, 45\\% of cases have 1 - 6 missings, and about 23\\% of cases have 8 or more missings. There are different patterns of missingness in variables and cases, but they can be broken down into smaller groups.",
             digits = 1)
             # booktabs = TRUE)
  # kable_styling(latex_options = c("hold_position"))


## ----housing-cluster-miss-----------------------------------------------------
housing_cls <- add_miss_cluster(housing, n_clusters = 2)


## ----rpart-fit-cluster--------------------------------------------------------
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


## ----rpart-plot, fig.height = 4, fig.width = 8, out.width = "90%", fig.align = "center", fig.cap = "Decision tree output predicting missingness clusters. Type of house, year quarter, and year were important for predicting missingness cluster. The cluster with the most missingness was for quarters 1 and 4, for 2017 and 2018. Type of house, year, and year quarter are important features related to missingness structure."----
rpart.plot::prp(miss_cls_fit_rpart,
                type = 4,
                extra = 2,
                fallen.leaves = TRUE,
                prefix = "cluster = ",
                suffix = " \nObservations",
                box.col = "lightgrey",
                border.col = "grey",
                branch.col = "grey40")


## ----vis-miss-each-node, fig.width = 3, fig.height = 3, out.width = "49%", fig.show='hold'----
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


## ----clean-data-for-analysis, include = FALSE, cache = TRUE-------------------
# add memoise to store the output data for this
set.seed(2022-12-1)
impute_knn <- simputation::impute_knn
dat_house <- housing_cls %>%
  mutate(year = lubridate::year(date),
         quarter = lubridate::quarter(date)) %>%
  mutate_at(vars(car, bathroom, bedroom2), as.integer)

dat_house_knn <- dat_house %>% 
  nabular() %>%
  as.data.frame() %>% 
  # postcode
  impute_knn(postcode ~ type + quarter + year + rooms + price + distance) %>%
  # distance
  impute_knn(distance ~ type + quarter + year + rooms + price) %>%
  # property_count
  impute_knn(property_count ~ type + quarter + year + rooms + price + distance) %>%
  # longitude
  impute_knn(longitude ~ type + quarter + year + rooms + price + distance) %>%
  # latitude
  impute_knn(latitude ~ type + quarter + year + rooms + price + distance) %>%
  # bedroom2
  impute_knn(bedroom2 ~ type + quarter + year + rooms + price + distance) %>%
  # bathroom
  impute_knn(bathroom ~ type + quarter + year + rooms + price + distance) %>%
  # car
  impute_knn(car ~ type + quarter + year + rooms + price + distance) %>%
  # landsize
  impute_knn(landsize ~ type + quarter + year + rooms + price + distance) %>%
  add_label_shadow()


## ----impute-knn, cache = TRUE-------------------------------------------------
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


## ----compare-assess-imputations-----------------------------------------------
dat_house_cc <- dat_house %>% nabular() %>% add_label_shadow() %>% na.omit()

bound_models <- bind_rows(lm = dat_house_lm,
                          knn = dat_house_knn,
                          cc = dat_house_cc,
                          .id = "imp_model") %>%
  as_tibble()


## ----imputed-by-model, fig.cap = "Boxplots of complete case data, and data imputed with KNN or linear model for different variables. (A) number of bedrooms, (B) number of bathrooms, (C) number of carspots, and (D) landsize (on a log10 scale). KNN had similar results to complete case, and linear model had a lower median for cars and fewer extreme values for bedrooms.", fig.height = 2.5, fig.width = 7, out.width = "95%"----
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

## ----fit-each-model-----------------------------------------------------------
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


## ----tidy-coefs, fig.height = 2, fig.width = 7, fig.cap = "The coefficient estimate for the number of rooms varies according to the imputed dataset. Complete case dataset produced lower coefficients, compared to imputed datasets.", out.width = "100%"----
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


## ----partial-resid, fig.cap = "Partial residual plot for each data set, complete cases (cc), and imputed with KNN (knn) or linear model (lm). These are plotted as two dimensional filled density plots with 7 bins estimated. The bins are colored according to concentration of points in that area - brighter colors indicated higher concentration. Compared to complete cases, imputed data has more points clustered closer to 0 for residuals, and and around 6 fitted values.", fig.width = 9, fig.height = 3, out.width = "100%"----
aug_fit_cc <-  broom::augment(fit_lm_dat_house_cc) %>% as_tibble()
aug_fit_knn <- broom::augment(fit_lm_dat_house_knn) %>% as_tibble()
aug_fit_lm <-  broom::augment(fit_lm_dat_house_lm)%>% as_tibble()

aug_fits <- bind_rows(cc =  aug_fit_cc,
                      knn = aug_fit_knn,
                      lm =  aug_fit_lm,
                      .id = "model")

ggplot(aug_fits, aes(x = .fitted,
                     y = .resid)) + 
  stat_density_2d(
    geom = "polygon",
    contour = TRUE,
    aes(fill = after_stat(level)),
    bins = 7
  ) +
  facet_wrap(vars(model)) +
  scale_fill_viridis_c() + 
  theme_bw() +
  scale_x_continuous(n.breaks = 4) +
  scale_y_continuous(n.breaks = 4) +
  theme(legend.position = "none",
        aspect.ratio = 1)
  
