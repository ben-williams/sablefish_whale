# sablefish whale depredation corrected catch data cleaning
# ben.williams@noaa.gov
# 2022-05

# load ----

library(tidyverse)
library(vroom)
library(tidytable)
library(lubridate)

# data ----

catch <- vroom::vroom(here::here('data', 'fsh1_catch_data.csv')) %>% 
  rename_all(tolower)

whale2021 <- vroom::vroom(here::here('data', 'mammal_v2021.csv')) %>% 
  rename_all(tolower) 

adfg_2021 <- vroom::vroom(here::here('data', 'sable_2021_cia_vw.csv')) %>% 
  tidytable::select.(long = POINT_X, lat = POINT_Y, year = YEAR, tons = Tons, 
                     grid_id, RA, sub_area = SubArea)

adfg_old <- vroom::vroom(here::here("data", "vw_CIA_SABL.csv")) %>% 
     tidytable::select.(long = longdd, lat = latDD, year = YEAR, tons = Tons, 
                        grid_id, RA, sub_area = SubArea) %>%
     tidytable::filter.(year >= 1995 & year < 2003)

vessels <- vroom::vroom(here::here("data", "vessels.csv")) %>%
  dplyr::rename(length = vessel_length)

fmp_key <- vroom::vroom(here::here("data", "FMP_key.csv")) %>% 
  rename_all(tolower)

# clean mammal data ----
whale2021 %>% 
  tidytable::select.(year, haul_date, haul_join, vessel, cruise, ifq, performance, gear_description,
                     gear_performance_description, depth = bottom_depth_fathoms, fmp_area,
                     fmp_subarea, lat = latdd_end,
                     long = londd_end, catch_80, catch_82,
                     catch_101, catch_102, catch_202, catch_203, total_hook_pots, mm_percent_monitored) %>%
  tidytable::filter.(!(fmp_subarea %in% c("SEI", "PWSI")),
                     !is.na(fmp_subarea),
                     depth >= 25 & depth <= 1200,
                     !is.na(catch_203),
                     gear_description == "LONGLINER",
                     performance %in% c(1, 8, 10),
                     ifq == "Y") %>%
  tidytable::mutate.(depth = depth * 1.8288,
                     depth1 = cut(depth, breaks = 4),
                     cpue_sable =     catch_203 / total_hook_pots,
                     cpue_gren =      catch_80 / total_hook_pots,
                     cpue_giantgren = catch_82 / total_hook_pots,
                     cpue_phal =      catch_101 / total_hook_pots,
                     cpue_gturb =     catch_102 / total_hook_pots,
                     cpue_pcod =      catch_202 / total_hook_pots,
                     date = lubridate::ymd(haul_date),
                     month = lubridate::month(date),
                     jul = lubridate::yday(date),
                     long = ifelse(long > 0, long - 360, long),
                     cluster = case_when(long >= -165 ~ 3,
                                         long <= -171 ~ 4,
                                         TRUE ~ 2),
                     long1 = floor(long * 3) / 3,
                     lat1 = floor(lat * 3) / 3,
                     haul_id = paste(vessel, cruise, gsub("-", "", haul_date), sep = "_"),
                     n = tidytable::n.(),
                     fmp_subarea = factor(fmp_subarea,
                                          levels = c("BS", "AI", "WG", "CG", "PWSI", "WY", "SE", "SEI")),
                     loc = paste(long1, lat1, sep = "_"),
                     dep = tidytable::case_when.(performance=="8" ~ "orca",
                                                 performance=="10" ~ "sperm",
                                                 performance =="1"~ "none"),
                     kwint = ifelse(dep == "orca", 1, 0),
                     swint = ifelse(dep == "sperm", 1, 0),
                     int = ifelse(kwint == 1 | swint == 1, 1, 0),
                     tots = 1 ,
                     kw_dep = ifelse(kwint == 1, "orca", "none"),
                     sw_dep = ifelse(swint == 1, "sperm", "none")) %>% 
  tidytable::filter.(!(month %in% c(1, 2, 12))) %>%
  tidytable::mutate.(haulnum =  paste(haul_id, 1:n(), sep = "_"), .by = haul_id) %>%
  tidytable::left_join.(vessels) %>%
  tidytable::mutate.(cpue = mean(cpue_sable), .by = c(loc, year)) %>%
  tidytable::left_join.(fmp_key) -> whales2

vroom::vroom_write(whales2, here::here('output', 'whales_share.csv'), delim = ',')

bind_rows(adfg_2021, adfg_old) %>%
  tidytable::mutate.(long = ifelse(long > 0, long - 360, long),
                     sub_area = tidytable::case_when.(sub_area == "CG" ~ "GOA",
                                     sub_area == "WG" ~ "GOA",
                                     sub_area == "WY" ~ "GOA",
                                     sub_area == "SE" ~ "GOA",
                                     TRUE ~ sub_area),
                     fmp_area = tidytable::case_when.(sub_area == "BS" ~ "BS",
                                     sub_area == "AI" ~ "AI",
                                     sub_area == "GOA" & long <= -140 & long > -147 ~ "WY",
                                     sub_area == "GOA" & long <= -147 & long > -159~ "CG",
                                     sub_area == "GOA" & long <= -159 ~ "WG",
                                     TRUE ~ "SE"),
                long1 = floor(long * 3) / 3,
                lat1 = floor(lat * 3) / 3,
                loc = paste(long1, lat1, sep = "_")) %>%
  tidytable::filter.(!(RA %in% c(649, 659))) %>% 
  tidytable:: mutate.(region = ifelse(fmp_area %in% c("CG", "SE", "WY"), "east", "west")) -> whale_catch


whales2 %>% 
  tidytable::summarise.(depredation = sum(int),
                   kw = sum(kwint),
                   sw = sum(swint),
                   totals = n(),
                   sable_cpue = mean(cpue_sable),
                   catch = sum(catch_203),
                   depth = mean(depth),
                   hooks = mean(total_hook_pots),
                   sum_hooks = sum(total_hook_pots),
                   length = mean(length),
                   monitored = mean(mm_percent_monitored),
                   cluster = floor(mean(cluster)), 
                   .by = c(loc, year)) %>%
    tidytable::left_join.(whale_catch, by = c("year", "loc")) -> whales3

vroom::vroom_write(whales3, here::here('output', 'whales_dep.csv'), delim = ',')


  tidytable::filter.(fmp_subarea %in% c("CG", "SE", "WY", "WG"),
                     year >= 2001) %>%  glimpse()



