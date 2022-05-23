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

# catch reduction percents - no idea where these data are from...
reduct <- data.frame(sub_area = c("BS", "AI", "WG", "CG", "SE", "WY"),
                     reduct = c(0.457, 0.577, 0.694, 0.238, 0.294, 0.263),
                     reduct_se = c(0.05, 0.08, 0.06, 0.04, 0.07, 0.05))


# models ---- 

# run once - save to output folder 

# orcafin <- gam(kw ~ s(tons, k=4) + s(length, k=4) + s(sable_cpue,k=4) + s(depth, k=4) +
#                  year +  s(lat, long) + offset(log(totals)), data = west_data,
#                  family = ziP(), select= T)
# 
# orca_negbin <- gam(kw ~ s(tons, k=4) + s(length, k=4) + s(sable_cpue,k=4) + s(depth, k=4) +
#                      year +  s(lat, long) + offset(log(totals)), data = west_data,
#                      family = nb(), select= T)
# 
# saveRDS(orcafin, here::here('output', 'orcafin.rds'))
# saveRDS(orca_negbin, here::here('output', 'orca_negbin.rds'))

orca_fin <- readRDS(here::here('output', 'orcafin.rds'))
orca_negbin <- readRDS(here::here('output', 'orca_negbin.rds'))

summary(orca_fin)
summary(orca_negbin) 

# predictions 

fit <- predict(orca_fin, west_data, type = "repsponse", se = TRUE)

west_data %>% 
  left_join(reduct) %>% 
  mutate(prop_se = ((fit + fit_se) / totals) - fit / totals,
         ses = sqrt(prop_se * (fit / totals)^2 * reduct_se * reduct^2 +
                      reduct_se * reduct^2 * ( fit / totals)^2 +
                      prop_se * ( fit / totals)^2 * reduct^2))
