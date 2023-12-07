rm(list=ls(all=TRUE))
library(tidyverse)
library(magrittr)
library(rlang)
library(sf) # converting to sf
library(tigris) #geojoin
library(raster) # making bboxes

# load and clean historical census data ####
# source: https://www.census.gov/library/working-papers/2002/demo/POP-twps0056.html
d.1790_1990 <- read_xlsx("census_race_ethnicity_1790_1990.xlsx",
               col_types = c("text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
                             "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
                             "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
                             "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))

# filter out state-level headings
d.1790_1990 %<>% filter(!(
              Area == "REGION AND DIVISION" |
              Area == "Northeast" |
              Area == "New England" |
              Area == "Middle Atlantic" |
              Area == "Midwest" |
              Area == "East North Central" |
              Area == "West North Central" |
              Area == "South" |
              Area == "South Atlantic" |
              Area == "East South Central" |
              Area == "West South Central" |
              Area == "West" |
              Area == "Mountain" |
              Area == "Pacific" |
              Area == "DIVISION AND STATE"))

# pivot from wide to long format
d.1790_1990 %<>% pivot_longer(c(3:36), names_to = "group", values_to = "value")

# rename and factorize group variable for readability and organization
d.1790_1990 %<>% mutate(group = factor(case_when(
        group == "count_chinese" ~ "Chinese",
        group == "count_white" ~ "White",
        group == "count_white_hisp" ~ "White (Hispanic or Latino)",
        group == "count_white_non_hisp" ~ "White (Not Hispanic or Latino)",
        group == "count_black" ~ "Black or African American",
        group == "count_black_hisp" ~ "Black or African American (Hispanic or Latino)",
        group == "count_black_non_hisp" ~ "Black or African American (Not Hispanic or Latino)",
        group == "count_american_indian" ~ "American Indian and Alaska Native",
        group == "count_american_indian_hisp" ~ "American Indian and Alaska Native (Hispanic or Latino)",
        group == "count_american_indian_non_hisp" ~ "American Indian and Alaska Native (Not Hispanic or Latino)",
        group == "count_asian" ~ "Asian",
        group == "count_nhpi" ~ "Native Hawaiian and Other Pacific Islander",
        group == "count_aapi_hisp" ~ "Asian American and Pacific Islander (Hispanic or Latino)",
        group == "count_aapi_non_hisp" ~ "Asian American and Pacific Islander (Not Hispanic or Latino)",
        group == "count_other" ~ "Some other race",
        group == "count_other_hisp" ~ "Some other race (Hispanic or Latino)",
        group == "count_other_non_hisp" ~ "Some other race (Not Hispanic or Latino)",
        group == "count_hispanic" ~ "Hispanic or Latino (of any race)",
        group == "count_total" ~ "Total population",
        group == "count_japanese" ~ "Japanese",
        group == "count_filipino" ~ "Filipino",
        group == "count_hindu_asian_indian" ~ "Asian Indian",
        group == "count_korean" ~ "Korean",
        group == "count_vietnamese" ~ "Vietnamese",
        group == "count_cambodian" ~ "Cambodian",
        group == "count_hmong" ~ "Hmong",
        group == "count_laotian" ~ "Laotian",
        group == "count_thai" ~ "Thai",
        group == "count_hawaiian" ~ "Native Hawaiian",
        group == "count_guamanian" ~ "Guamanian",
        group == "count_malay" ~ "Malaysian",
        group == "count_samoan" ~ "Samoan",
        group == "count_maori" ~ "Maori",
        group == "count_asian_other" ~ "Asian (some other race)"
                        ), levels = c("White", 
                                      "White (Hispanic or Latino)",
                                      "White (Not Hispanic or Latino)",
                                      "Black or African American", 
                                      "Black or African American (Hispanic or Latino)", 
                                      "Black or African American (Not Hispanic or Latino)", 
                                      "American Indian and Alaska Native", 
                                      "American Indian and Alaska Native (Hispanic or Latino)",
                                      "American Indian and Alaska Native (Not Hispanic or Latino)",
                                      "Asian",
                                      "Chinese",
                                      "Japanese",
                                      "Filipino",
                                      "Asian Indian",
                                      "Korean",
                                      "Vietnamese",
                                      "Cambodian",
                                      "Hmong",
                                      "Laotian",
                                      "Thai",
                                      "Malaysian",
                                      "Asian (some other race)",
                                      "Native Hawaiian and Other Pacific Islander",
                                      "Native Hawaiian",
                                      "Guamanian",
                                      "Samoan",
                                      "Maori",
                                      "Asian American and Pacific Islander (Hispanic or Latino)",
                                      "Asian American and Pacific Islander (Not Hispanic or Latino)",
                                      "Some other race", 
                                      "Some other race (Hispanic or Latino)",
                                      "Some other race (Not Hispanic or Latino)",
                                      "Hispanic or Latino (of any race)", 
                                      "Total population"))) %>%
        rename(area = Area,
               year = Year)

save(d.1790_1990, file = "d.1790_1990.rdata")
