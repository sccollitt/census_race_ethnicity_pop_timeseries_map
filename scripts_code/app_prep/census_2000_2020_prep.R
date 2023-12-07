rm(list=ls(all=TRUE))

library(tidycensus)
library(tidyverse)
library(magrittr)
library(readxl)

# download and clean data ####
## 2000 ####
# https://api.census.gov/data/2000/dec/sf1/variables.html
# PCT007005:  chinese alone or in any combination (not taiwanese)
# PCT007015: taiwanese
# P010003: white alone or in any combination (not hispanic)
# P010004: black alone or in any combination (not hispanic)
# P010005: american indian alone or in any combination (not hispanic)
# P010006: asian alone or in any combination (not hispanic)
# P010007: native hawaiian and other pacific islander alone or in any combination (not hispanic)
# P010008: some other race (not hispanic)
# P008010: hispanic or latino of any race
# P001001: total population
# P010002: total not hispanic or latino
# P010010: white alone or in any combination (hispanic)
# P010011: black (hispanic)
# P010012: american indian (hispanic)
# P010013: asian (hispanic)
# P010014: nhpi (hispanic)
# P010015: other (hispanic)
# P009002: white
# P009003: black
# P009004: american indian
# P009005: asian
# P009006: nhpi
# P009007: other
# PCT007002: asian indian
# PCT007004: cambodian
# PCT007006: filipino
# PCT007007: Hmong
# PCT007009: Japanese
# PCT007010: korean
# PCT007011: laotian
# PCT007012: malaysian
# PCT007016: Thai
# PCT007017: vietnamese
# PCT010003: native hawaiian
# PCT010004: samoan
# PCT010008: guamanian

# download state-level data
d.2000.states <- get_decennial(
        geography = "state",
        variables = c("P010003", "P010004", "P010005", "P010006", "P010007", "P010008", "PCT007005", "PCT007015", "P001001", 
                      "P010010", "P010011", "P010012", "P010013", "P010014", "P010015", 
                      "P009002", "P009003", "P009004", "P009005", "P009006", "P009007", "P008010", "P010002",
                      "PCT007002", "PCT007004", "PCT007006", "PCT007007", "PCT007009", "PCT007010",
                      "PCT007011", "PCT007012", "PCT007016", "PCT007017", "PCT010003", "PCT010004", "PCT010008"), 
        year = 2000
) 

# download national-level data
d.2000.us <- get_decennial(
        geography = "US",
        variables = c("P010003", "P010004", "P010005", "P010006", "P010007", "P010008", "PCT007005", "PCT007015", "P001001", 
                      "P010010", "P010011", "P010012", "P010013", "P010014", "P010015", 
                      "P009002", "P009003", "P009004", "P009005", "P009006", "P009007", "P008010", "P010002",
                      "PCT007002", "PCT007004", "PCT007006", "PCT007007", "PCT007009", "PCT007010",
                      "PCT007011", "PCT007012", "PCT007016", "PCT007017", "PCT010003", "PCT010004", "PCT010008"), 
        year = 2000
) 

d.2000 <- bind_rows(d.2000.states, d.2000.us)

# filter out puerto rico
d.2000 %<>% filter(NAME != "Puerto Rico")

# rename group variable values
d.2000 %<>% mutate(group = factor(case_when(
        variable == "PCT007005" ~ "Chinese",
        variable == "PCT007015" ~ "Taiwanese",
        variable == "P009002" ~ "White",
        variable == "P010010" ~ "White (Hispanic or Latino)",
        variable == "P010003" ~ "White (Not Hispanic or Latino)",
        variable == "P009003" ~ "Black or African American",
        variable == "P010011" ~ "Black or African American (Hispanic or Latino)",
        variable == "P010004" ~ "Black or African American (Not Hispanic or Latino)",
        variable == "P009004" ~ "American Indian and Alaska Native",
        variable == "P010012" ~ "American Indian and Alaska Native (Hispanic or Latino)",
        variable == "P010005" ~ "American Indian and Alaska Native (Not Hispanic or Latino)",
        variable == "P009005" ~ "Asian",
        variable == "P010013" ~ "Asian (Hispanic or Latino)",
        variable == "P010006" ~ "Asian (Not Hispanic or Latino)",
        variable == "P009006" ~ "Native Hawaiian and Other Pacific Islander",
        variable == "P010014" ~ "Native Hawaiian and Other Pacific Islander (Hispanic or Latino)",
        variable == "P010007" ~ "Native Hawaiian and Other Pacific Islander (Not Hispanic or Latino)",
        variable == "P009007" ~ "Some other race",
        variable == "P010015" ~ "Some other race (Hispanic or Latino)",
        variable == "P010008" ~ "Some other race (Not Hispanic or Latino)",
        variable == "P008010" ~ "Hispanic or Latino (of any race)",
        variable == "P010002" ~ "Not Hispanic or Latino (of any race)",
        variable == "P001001" ~ "Total population",
        variable == "PCT007002" ~ "Asian Indian",
        variable == "PCT007004" ~ "Cambodian",
        variable == "PCT007006" ~ "Filipino",
        variable == "PCT007007" ~ "Hmong",
        variable == "PCT007009" ~ "Japanese",
        variable == "PCT007010" ~ "Korean",
        variable == "PCT007011" ~ "Laotian",
        variable == "PCT007012" ~ "Malaysian",
        variable == "PCT007016" ~ "Thai",
        variable == "PCT007017" ~ "Vietnamese",
        variable == "PCT010003" ~ "Native Hawaiian",
        variable == "PCT010004" ~ "Samoan",
        variable == "PCT010008" ~ "Guamanian"
        ), 
        levels = c("White", 
              "White (Hispanic or Latino)",
              "White (Not Hispanic or Latino)",
              "Black or African American", 
              "Black or African American (Hispanic or Latino)",
              "Black or African American (Not Hispanic or Latino)",
              "American Indian and Alaska Native", 
              "American Indian and Alaska Native (Hispanic or Latino)",
              "American Indian and Alaska Native (Not Hispanic or Latino)",
              "Asian", 
              "Asian (Hispanic or Latino)",
              "Asian (Not Hispanic or Latino)",
              "Chinese",
              "Taiwanese",
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
              "Native Hawaiian and Other Pacific Islander", 
              "Native Hawaiian and Other Pacific Islander (Hispanic or Latino)",
              "Native Hawaiian and Other Pacific Islander (Not Hispanic or Latino)",
              "Native Hawaiian",
              "Guamanian",
              "Samoan",
              "Some other race", 
              "Some other race (Hispanic or Latino)",
              "Some other race (Not Hispanic or Latino)",
              "Hispanic or Latino (of any race)", 
              "Not Hispanic or Latino (of any race)", 
              "Total population")))

d.2000 %<>% mutate(year = 2000)

## 2010 ####
# https://api.census.gov/data/2010/dec/sf1/variables.html
# PCT007007:  chinese alone or in any combination (not taiwanese)
# PCT007018: taiwanese
# P007003: white alone or in any combination (not hispanic)
# P007004: black alone or in any combination (not hispanic)
# P007005: american indian alone or in any combination (not hispanic)
# P007006: asian alone or in any combination (not hispanic)
# P007007: native hawaiian and other pacific islander alone or in any combination (not hispanic)
# P007008: some other race (not hispanic)
# P009002: hispanic or latino of any race
# P001001: total population
# P009003: total not hispanic or latino 
# P007010: white alone or in any combination (hispanic)
# P007011: black (hispanic)
# P007012: american indian (hispanic)
# P007013: asian (hispanic)
# P007014: nhpi (hispanic)
# P007015: other (hispanic)
# P006002: white 
# P006003: black 
# P006004: american indian 
# P006005: asian 
# P006006: nhpi 
# P006007: other 
# PCT007002: asian indian
# PCT007006: cambodian
# PCT007008: filipino
# PCT007009: Hmong
# PCT007011: Japanese
# PCT007012: korean
# PCT007013: laotian
# PCT007014: malaysian
# PCT007019: Thai
# PCT007020: vietnamese
# PCT010003: native hawaiian
# PCT010004: samoan
# PCT010008: guamanian

d.2010.states <- get_decennial(
        geography = "state",
        variables = c("PCT007007", "PCT007018", "P007003", "P007004", "P007005", "P007006", "P007007", "P007008", "P001001",
                      "P009002", "P009003", "P007010", "P007011", "P007012", "P007013", "P007014", "P007015", 
                      "P006002", "P006003", "P006004", "P006005", "P006006", "P006007", 
                      "PCT007002", "PCT007006", "PCT007008", "PCT007009", "PCT007011", "PCT007012",
                      "PCT007013", "PCT007014", "PCT007019", "PCT007020", "PCT010003", "PCT010004", "PCT010008"), 
        year = 2010
) 

d.2010.us <- get_decennial(
        geography = "US",
        variables = c("PCT007007", "PCT007018", "P007003", "P007004", "P007005", "P007006", "P007007", "P007008", "P001001",
                      "P009002", "P009003", "P007010", "P007011", "P007012", "P007013", "P007014", "P007015", 
                      "P006002", "P006003", "P006004", "P006005", "P006006", "P006007",
                      "PCT007002", "PCT007006", "PCT007008", "PCT007009", "PCT007011", "PCT007012",
                      "PCT007013", "PCT007014", "PCT007019", "PCT007020", "PCT010003", "PCT010004", "PCT010008"), 
        year = 2010
) 

d.2010 <- bind_rows(d.2010.states, d.2010.us)

d.2010 %<>% filter(NAME != "Puerto Rico")

d.2010 %<>% mutate(group = factor(case_when(
        variable == "PCT007007" ~ "Chinese",
        variable == "PCT007018" ~ "Taiwanese",
        variable == "P007003" ~ "White (Not Hispanic or Latino)",
        variable == "P007004" ~ "Black or African American (Not Hispanic or Latino)",
        variable == "P007005" ~ "American Indian and Alaska Native (Not Hispanic or Latino)",
        variable == "P007006" ~ "Asian (Not Hispanic or Latino)",
        variable == "P007007" ~ "Native Hawaiian and Other Pacific Islander (Not Hispanic or Latino)",
        variable == "P007008" ~ "Some other race (Not Hispanic or Latino)",
        variable == "P001001" ~ "Total population",
        variable == "P007010" ~ "White (Hispanic or Latino)",
        variable == "P007011" ~ "Black or African American (Hispanic or Latino)",
        variable == "P007012" ~ "American Indian and Alaska Native (Hispanic or Latino)",
        variable == "P007013" ~ "Asian (Hispanic or Latino)",
        variable == "P007014" ~ "Native Hawaiian and Other Pacific Islander (Hispanic or Latino)",
        variable == "P007015" ~ "Some other race (Hispanic or Latino)",
        variable == "P006002" ~ "White",
        variable == "P006003" ~ "Black or African American",
        variable == "P006004" ~ "American Indian and Alaska Native",
        variable == "P006005" ~ "Asian",
        variable == "P006006" ~ "Native Hawaiian and Other Pacific Islander",
        variable == "P006007" ~ "Some other race",
        variable == "P009002" ~ "Hispanic or Latino (of any race)",
        variable == "P009003" ~ "Not Hispanic or Latino (of any race)",
        variable == "PCT007002" ~ "Asian Indian",
        variable == "PCT007006" ~ "Cambodian",
        variable == "PCT007008" ~ "Filipino",
        variable == "PCT007009" ~ "Hmong",
        variable == "PCT007011" ~ "Japanese",
        variable == "PCT007012" ~ "Korean",
        variable == "PCT007013" ~ "Laotian",
        variable == "PCT007014" ~ "Malaysian",
        variable == "PCT007019" ~ "Thai",
        variable == "PCT007020" ~ "Vietnamese",
        variable == "PCT010003" ~ "Native Hawaiian",
        variable == "PCT010004" ~ "Samoan",
        variable == "PCT010008" ~ "Guamanian"), 
                        levels = c("White", 
                                   "White (Hispanic or Latino)",
                                   "White (Not Hispanic or Latino)",
                                   "Black or African American", 
                                   "Black or African American (Hispanic or Latino)",
                                   "Black or African American (Not Hispanic or Latino)",
                                   "American Indian and Alaska Native", 
                                   "American Indian and Alaska Native (Hispanic or Latino)",
                                   "American Indian and Alaska Native (Not Hispanic or Latino)",
                                   "Asian", 
                                   "Asian (Hispanic or Latino)",
                                   "Asian (Not Hispanic or Latino)",
                                   "Chinese",
                                   "Taiwanese",
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
                                   "Native Hawaiian and Other Pacific Islander", 
                                   "Native Hawaiian and Other Pacific Islander (Hispanic or Latino)",
                                   "Native Hawaiian and Other Pacific Islander (Not Hispanic or Latino)",
                                   "Native Hawaiian",
                                   "Guamanian",
                                   "Samoan",
                                   "Some other race", 
                                   "Some other race (Hispanic or Latino)",
                                   "Some other race (Not Hispanic or Latino)",
                                   "Hispanic or Latino (of any race)", 
                                   "Not Hispanic or Latino (of any race)", 
                                   "Total population")))
d.2010 %<>% mutate(year = 2010)

## 2020 ####
# need to get aggregated group variables from one dataset and detailed from another, so split into 2 parts 

# https://api.census.gov/data/2020/dec/ddhca/variables/POPGROUP.json
# https://www.census.gov/data/developers/data-sets/decennial-census.html
# 3822:  chinese alone or in any combination (not taiwanese)
# 3827: taiwanese
# 3839: asian indian
# 3853: cambodian
# 3854: filipino
# 3823: Hmong
# 3824: Japanese
# 3825: korean
# 3856: laotian
# 3857: malaysian
# 3860: Thai
# 3861: vietnamese
# 3925: native hawaiian
# 3928: samoan
# 3940: guamanian
pop_groups <- c("3822", "3827", "3839", "3853", "3854", "3823", "3824", "3825", "3856", "3857", "3860", "3861", "3925", "3928", "3940")

d.2020.us <- get_decennial(
        geography = "US",
        variables = c("T01001_001N"), 
        year = 2020,
        sumfile = "ddhca",
        pop_group = "all",
        pop_group_label = T) 

d.2020.states <- get_decennial(
        geography = "state",
        variables = c("T01001_001N"), 
        year = 2020,
        sumfile = "ddhca",
        pop_group = "all",
        pop_group_label = T) 

d.2020.1 <- rbind(d.2020.us, d.2020.states)

d.2020.1 %<>% filter(pop_group %in% pop_groups)

d.2020.1 %<>% filter(NAME != "Puerto Rico")

d.2020.1 %<>% mutate(group = case_when(
        pop_group == "3822" ~ "Chinese",
        pop_group == "3827" ~ "Taiwanese",
        pop_group == "3839" ~ "Asian Indian",
        pop_group == "3853" ~ "Cambodian",
        pop_group == "3854" ~ "Filipino",
        pop_group == "3823" ~ "Hmong",
        pop_group == "3824" ~ "Japanese",
        pop_group == "3825" ~ "Korean",
        pop_group == "3856" ~ "Laotian",
        pop_group == "3857" ~ "Malaysian",
        pop_group == "3860" ~ "Thai",
        pop_group == "3861" ~ "Vietnamese",
        pop_group == "3925" ~ "Native Hawaiian",
        pop_group == "3928" ~ "Samoan",
        pop_group == "3940" ~ "Guamanian"))

d.2020.1 %<>% mutate(year = 2020)

# subset for consistency with other years
d.2020.1 %<>% subset(select = -c(GEOID, pop_group, pop_group_label, variable))

# https://api.census.gov/data/2020/dec/dhc/variables.html
# P4_003N: hispanic or latino
# P4_002N: not hispanic or latino
# P8_001N: total population
# P6_002N: white alone or in combination
# P7_003N: white alone or in combination (not hispanic or latino)
# P7_010N: white alone or in combination (hispanic or latino)
# P6_003N: black alone or in combination
# P7_004N: black alone or in combination (not hispanic or latino)
# P7_011N: black alone or in combination (hispanic or latino)
# P6_004N: american indian alone or in combination
# P7_005N: american indian alone or in combination (not hispanic or latino)
# P7_012N: american indian alone or in combination (hispanic or latino)
# P6_005N: asian alone or in combination
# P7_006N: asian alone or in combination (not hispanic or latino)
# P7_013N: asian alone or in combination (hispanic or latino)
# P6_006N: native hawaiian and other pacific islander alone or in combination
# P7_007N: native hawaiian and other pacific islander alone or in combination (not hispanic or latino)
# P7_014N: native hawaiian and other pacific islander alone or in combination (hispanic or latino)
# P6_007N: other race alone or in combination
# P7_008N: other race alone or in combination (not hispanic or latino)
# P7_015N: other race alone or in combination (hispanic or latino)

d.2020.us_agg <- get_decennial(
        geography = "US",
        variables = c("P4_003N", "P4_002N", "P8_001N", "P6_002N", "P7_003N", "P7_010N", "P6_003N", "P7_004N", "P7_011N", "P6_004N", "P7_005N",
                      "P7_012N", "P6_005N", "P7_006N", "P7_013N", "P6_006N", "P7_007N", "P7_014N", "P6_007N", "P7_008N", "P7_015N"), 
        year = 2020,
        sumfile = "dhc"
) 

d.2020.state_agg <- get_decennial(
        geography = "state",
        variables = c("P4_003N", "P4_002N", "P8_001N", "P6_002N", "P7_003N", "P7_010N", "P6_003N", "P7_004N", "P7_011N", "P6_004N", "P7_005N",
                      "P7_012N", "P6_005N", "P7_006N", "P7_013N", "P6_006N", "P7_007N", "P7_014N", "P6_007N", "P7_008N", "P7_015N"), 
        year = 2020,
        sumfile = "dhc"
) 

d.2020.2 <- rbind(d.2020.us_agg, d.2020.state_agg)

d.2020.2 %<>% filter(NAME != "Puerto Rico")

d.2020.2 %<>% mutate(group = case_when(
        variable == "P7_003N" ~ "White (Not Hispanic or Latino)",
        variable == "P7_004N" ~ "Black or African American (Not Hispanic or Latino)",
        variable == "P7_005N" ~ "American Indian and Alaska Native (Not Hispanic or Latino)",
        variable == "P7_006N" ~ "Asian (Not Hispanic or Latino)",
        variable == "P7_007N" ~ "Native Hawaiian and Other Pacific Islander (Not Hispanic or Latino)",
        variable == "P7_008N" ~ "Some other race (Not Hispanic or Latino)",
        variable == "P8_001N" ~ "Total population",
        variable == "P7_010N" ~ "White (Hispanic or Latino)",
        variable == "P7_011N" ~ "Black or African American (Hispanic or Latino)",
        variable == "P7_012N" ~ "American Indian and Alaska Native (Hispanic or Latino)",
        variable == "P7_013N" ~ "Asian (Hispanic or Latino)",
        variable == "P7_014N" ~ "Native Hawaiian and Other Pacific Islander (Hispanic or Latino)",
        variable == "P7_015N" ~ "Some other race (Hispanic or Latino)",
        variable == "P6_002N" ~ "White",
        variable == "P6_003N" ~ "Black or African American",
        variable == "P6_004N" ~ "American Indian and Alaska Native",
        variable == "P6_005N" ~ "Asian",
        variable == "P6_006N" ~ "Native Hawaiian and Other Pacific Islander",
        variable == "P6_007N" ~ "Some other race",
        variable == "P4_003N" ~ "Hispanic or Latino (of any race)",
        variable == "P4_002N" ~ "Not Hispanic or Latino (of any race)"))

d.2020.2 %<>% mutate(year = 2020)

d.2020.2 %<>% subset(select = -c(GEOID, variable))

d.2020 <- rbind(d.2020.1, d.2020.2)

d.2020$group <-  factor(d.2020$group, levels = c("White", 
                                             "White (Hispanic or Latino)",
                                             "White (Not Hispanic or Latino)",
                                             "Black or African American", 
                                             "Black or African American (Hispanic or Latino)",
                                             "Black or African American (Not Hispanic or Latino)",
                                             "American Indian and Alaska Native", 
                                             "American Indian and Alaska Native (Hispanic or Latino)",
                                             "American Indian and Alaska Native (Not Hispanic or Latino)",
                                             "Asian", 
                                             "Asian (Hispanic or Latino)",
                                             "Asian (Not Hispanic or Latino)",
                                             "Chinese",
                                             "Taiwanese",
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
                                             "Native Hawaiian and Other Pacific Islander", 
                                             "Native Hawaiian and Other Pacific Islander (Hispanic or Latino)",
                                             "Native Hawaiian and Other Pacific Islander (Not Hispanic or Latino)",
                                             "Native Hawaiian",
                                             "Guamanian",
                                             "Samoan",
                                             "Some other race", 
                                             "Some other race (Hispanic or Latino)",
                                             "Some other race (Not Hispanic or Latino)",
                                             "Hispanic or Latino (of any race)", 
                                             "Not Hispanic or Latino (of any race)", 
                                             "Total population"))

# merge decennial years and make df consistent with historical df ####
d.2000_2020 <- bind_rows(d.2000, d.2010, d.2020) %>%
        subset(select = -c(GEOID, variable)) %>%
        rename(area = NAME) 
save(d.2000_2020, file = "d.2000_2020.rdata")
