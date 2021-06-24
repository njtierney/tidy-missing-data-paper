#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author Nicholas Tierney
#' @export
clean_housing_data <- function() {

  
  housing_raw <-
    read_csv(here("data",
                  "melbourne_housing_raw.csv")) %>%
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
  
  housing
  

}
