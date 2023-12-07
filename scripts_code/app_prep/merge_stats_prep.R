rm(list=ls(all=TRUE))

library(tidyverse)
library(magrittr)
library(rlang)
library(sf) # converting to sf
library(tigris) #geojoin
library(raster) # making bboxes

# load and merge census data ####

# load 1790-1990 df
load("d.1790_1990.rdata")

# load 2000-2020 df
load("d.2000_2020.rdata")

# rbind to one df
d <- bind_rows(d.1790_1990, d.2000_2020)

# create stats variables ####

# calculate percent change per year per area
d %<>% arrange(area, group, year) %>%
        group_by(area, group) %>%
        mutate(percent_change = (value/lag(value) - 1) * 100) %>%
        ungroup() %>%
        mutate(percent_change = case_when(
                is.nan(percent_change) ~ 0, # makes percent_change 0 when the current and lagged value are 0 for data visualization
                percent_change == Inf ~ value * 100, # effectively treats lagged value as 1 when it's 0 for data visualization
                T ~ percent_change)) %>%
        mutate(percent_change = round(percent_change, 1))

# calculate the proportion of a race of the total population per year per area
d %<>% arrange(area, group, year) %>%
        group_by(area, year) %>%
        mutate(total_population = sum(value[group == "Total population"])) %>%
        mutate(proportion = value/total_population * 100) %>%
        ungroup() %>%
        mutate(proportion = round(proportion, 1))


# calculate the change in proportion of a group per year per area
d %<>% arrange(area, group, year) %>%
        group_by(area, group) %>%
        mutate(proportion_change = (proportion/lag(proportion) - 1) * 100) %>%
        ungroup() %>%
        mutate(proportion_change = case_when(
                is.nan(proportion_change) ~ 0,
                proportion_change == Inf ~ proportion * 100,
                T ~ proportion_change)) %>%
        mutate(proportion_change = round(proportion_change, 1))


# clean for app 
d %<>% filter(area != "Dakota (Territory)" & # don't have polygon data
                              group != "Maori") # only available in one year


# create lookup df for user selections ####
# for app logic that displays error when data for a given year-group combination is unavailable

# Make df for all possible year-group combinations
all_combinations <- expand.grid(
        year = unique(d$year),
        group = unique(d$group)
)

# make df for whether a given stat is available in a given year-group combination
summarized_data <- d %>%
        group_by(year, group) %>%
        summarise(
                is_disabled_value = ifelse(all(is.na(value)), 1, 0),
                is_disabled_percent_change = ifelse(all(is.na(percent_change)), 1, 0),
                is_disabled_proportion = ifelse(all(is.na(proportion)), 1, 0),
                is_disabled_proportion_change = ifelse(all(is.na(proportion_change)), 1, 0)
        )

# Join and replace NA values with 1
lookup_df <- left_join(all_combinations, summarized_data, by = c("year", "group")) %>%
        replace(is.na(.), list(
                is_disabled_value = 1,
                is_disabled_percent_change = 1,
                is_disabled_proportion = 1,
                is_disabled_proportion_change = 1
        ))

save(lookup_df, file = "../app/lookup_df.rdata")

# load and merge map df ####

# load map df
load("../us_map/map_states.rdata")

# separate into timeseries df and map df because the "United States" area doesn't have geometry
d.ts <- d
save(d.ts, file = "../app/d.ts.rdata")

d.map <- d %>% filter(area != "United States") %>% left_join(states, by = c("area" = "state_name"))

d.map %<>% st_as_sf(crs = 4326) %>% 
        shift_geometry() %>%
        st_transform('+proj=longlat +datum=WGS84') # Note: this takes a very long time

save(d.map, file = "../app/d.map.rdata")
