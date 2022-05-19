# sablefish whale depredation corrected catch analysis
# ben.williams@noaa.gov
# 2022-05

# load ----

library(tidyverse)
library(vroom)
library(tidytable)
library(mgcv)

# data ----

dep <- vroom::vroom(here::here('output', 'whales_dep.csv'))

dep %>%
  tidytable::filter.(region == "west") %>%
  tidytable::mutate.(Cluster = factor(cluster)) -> west_data

dep %>%
  tidytable::filter.(region == "east") %>%
  tidytable::mutate.(Cluster = factor(cluster)) -> east_data

# models ---- 

orcafin <- gam(kw ~ s(tons, k=4) + s(length, k=4) + s(sable_cpue,k=4) + s(depth, k=4) +
                 year +  s(lat, long) + offset(log(totals)), data = west_data,
                 family = ziP(), select= T)

orca_negbin <- gam(kw ~ s(tons, k=4) + s(length, k=4) + s(sable_cpue,k=4) + s(depth, k=4) +
                     year +  s(lat, long) + offset(log(totals)), data = west_data,
                     family = nb(), select= T)

summary(orcafin)
summary(orca_negbin) 
