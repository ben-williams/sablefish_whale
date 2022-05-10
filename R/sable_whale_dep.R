# sablefish whale depredation corrected catch

# ben.williams@noaa.gov

# 2020-06



# notes:

library(usethis)

use_git_config(user.name = "mwilliamsOC", user.email = mwilliams@oceanconservancy.org)



# assumes you are connected to the VPN

# also assumes you have a current AKFIN account and password

# therefore you have tnsnames.ora file all loaded up by IT, etc.

# if this isn't the case, well then, don't bother...



# 1. load ----





# devtools::install_github("BenWilliams-NOAA/groundfishr")



library(groundfishr)
library(vroom)
library(dplyr)
library(ggplot2)
library(reshape2)
library(tidyr)
library(devtools)
library(lubridate)



# globals ----

year <- 2021
afsc_user = "your_username"
afsc_pwd = "your_pwd"
akfin_user = "your_username"
akfin_pwd = "your_pwd"



# setup folders ----

setup(year)


# query database ----



# 4 files will be output in the "data/raw" folder

# will take a long time cause whales...





sablefish(year, akfin_user, akfin_pwd, afsc_user, afsc_pwd)


setwd("C:/Documents/depredation_2021/Dana_tables")
catch<-read.csv("fsh1_catch_data.csv", header=TRUE)
#whales<-read.csv("whales.csv", header=TRUE)

whale2021<-read.csv("mammal_v2021.csv", header=TRUE)

vessels<-read.csv("vessels.csv", header=TRUE)%>%
  
  dplyr::rename(length = vessel_length)

fmp_key <- read.csv("FMP_key.csv", header = TRUE)







# data ----

#catch <- read.csv(here::here(year, "data", "raw", "fsh1_catch_data.csv"))

#whales <- read.csv(here::here(year, "data", "raw", "fsh1_whale_dep_data.csv"))

#vessels <- read.csv(here::here(year, "data", "raw", "vessels.csv")) %>%

dplyr::rename(length = vessel_length)



# The following files are from the google drive Dan sent me

# https://drive.google.com/drive/u/0/folders/10Bsni7WKtu3zBswjBR8oWxxei88oFH_g



adfg_2021 <- read.csv ("sable_2021_cia_vw.csv", header =TRUE)%>%
  
  dplyr::select(long = POINT_X,
                
                lat = POINT_Y,
                
                year = YEAR,
                
                tons = Tons,
                
                grid_id,
                
                RA,
                
                sub_area = SubArea)

head(adfg_2021)

table(adfg_2021$sub_area)##GOA lumped ##57NAs sub_area

table(adfg_2021$year)







##not working with original code

str(adfg_old)

#get old data file again

adfg_old <- read.csv ("vw_CIA_SABL.csv", header =TRUE)%>%                                                                   
  
  dplyr::select(long = longdd,
                
                lat = latDD,
                
                year = YEAR,
                
                tons = Tons,
                
                grid_id = grid_id,
                
                RA = RA,
                
                sub_area = SubArea) %>%
  
  dplyr::filter(year >= 1995 & year < 2003)



head(adfg_2021) ##goa lumped

table(adfg_old$sub_area)##has all subareas



table(adfg_old$year)##has all subareas





##original code_not working revised above

adfg_old <- vroom::vroom(here::here(year, "data", "user_input", "vw_CIA_SABL.csv"),
                         
                         col_types = vroom::cols(STAT_AREA = readr::col_character(),
                                                 
                                                 grid_id = readr::col_double()))  %>%
  
  dplyr::select(long = longdd,
                
                lat = latDD,
                
                year = YEAR,
                
                tons = Tons,
                
                grid_id,
                
                RA,
                
                sub_area = SubArea) %>%
  
  dplyr::filter(year >= 1995 & year < 2003)

adfg_2021$grid_id



# clean mammal data ----

table(whale2021$PERFORMANCE)

dim(whale2021)

h<-whale2021$GEAR_DESCRIPTION == "LONGLINER"

w1<-whale2021[h,]

dim(w1)



#look at only normal and depredated sets only

j<-w1$PERFORMANCE==8 | w1$PERFORMANCE==10 | w1$PERFORMANCE==1

w2<-w1[j,]

dim(w2)

str(w2)



#only IFQ sets

l<-w2$IFQ=="Y"

whales<-w2[l,]

dim(whales)







##add log_cpue



whales %>%
  
  rename_all(tolower) %>%
  
  dplyr::select(year, haul_date, haul_join, vessel, cruise, performance, gear_description,
                
                gear_performance_description, depth = bottom_depth_fathoms, fmp_area,
                
                fmp_subarea, lat = latdd_end,
                
                long = londd_end, catch_80, catch_82,
                
                catch_101, catch_102, catch_202, catch_203, total_hook_pots, mm_percent_monitored) %>%
  
  dplyr::filter(!(fmp_subarea %in% c("SEI", "PWSI")), 
                
                !is.na(fmp_subarea),
                
                !is.na(depth),
                
                depth >= 25 & depth <= 1200,
                
                !is.na(catch_203)) %>%
  
  dplyr::mutate(depth = depth * 1.8288,
                
                depth1 = cut(depth, breaks = 4),
                
                cpue_sable = catch_203 / total_hook_pots,
                
                cpue_gren = catch_80 / total_hook_pots,
                
                cpue_giantgren = catch_82 / total_hook_pots,
                
                cpue_phal = catch_101 / total_hook_pots,
                
                cpue_gturb = catch_102 / total_hook_pots,
                
                cpue_pcod = catch_202 / total_hook_pots,
                
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
                
                n = dplyr::n()) %>%
  
  dplyr::filter(!(month %in% c(1, 2, 12))) %>%
  
  dplyr::group_by(haul_id) %>%
  
  dplyr::mutate(haulnum =  paste(haul_id, 1:n(), sep = "_")) %>%
  
  dplyr::ungroup() %>%
  
  dplyr::mutate(fmp_subarea = factor(fmp_subarea,
                                     
                                     levels = c("BS", "AI", "WG", "CG", "PWSI", "WY", "SE", "SEI")),
                
                loc = paste(long1, lat1, sep = "_"),
                
                dep = dplyr::case_when(performance=="8" ~ "orca",
                                       
                                       performance=="10" ~ "sperm",
                                       
                                       performance =="1"~ "none"),
                
                kwint = ifelse(dep == "orca", 1, 0),
                
                swint = ifelse(dep == "sperm", 1, 0),
                
                int = ifelse(kwint == 1 | swint == 1, 1, 0),
                
                tots = 1 ,
                
                kw_dep = ifelse(kwint == 1, "orca", "none"),
                
                sw_dep = ifelse(swint == 1, "sperm", "none")) %>%
  
  dplyr::left_join(vessels) %>%
  
  group_by(loc, year) %>%
  
  mutate(cpue = mean(cpue_sable)) %>%
  
  ungroup() %>% 
  
  left_join(fmp_key) -> whales2



summary(whales2)

hist(whales2$depth)

summary(fmp_key)

table(whales2$dep)





table(whales2$FMP_SUBAREA)

table(whales2$fmp_subarea)

table(whales2$fmp_area)





write.csv(whales2, file = "whales_share.csv")





whales2$FMP_SUBAREA <- as.factor(whales2$FMP_SUBAREA)





str(whales2)



##quick visual proportion sets depredatated

str(goa3)

goa1<- whales2$fmp_subarea == "CG" | whales2$fmp_subarea == "SE" | whales2$fmp_subarea == "WY" |
  
  whales2$fmp_subarea == "WG"

goa <-whales2[goa1,]

goa2<-goa$year>=2001

goa3 <-goa[goa2,]

table(goa$year)

dat<-aggregate(cbind(goa3$swint,goa3$tots), list(FMP_SUBAREA=goa3$fmp_subarea, year=goa3$year),sum)

dat$prop<-dat$V1/dat$V2

datgoa <-dat

ggplot(datgoa, aes(year, prop))+ geom_col() +  facet_wrap(vars(FMP_SUBAREA)) +  geom_smooth(method=lm) +
  
  ggtitle("Proportion Observed Sets Depredated by Sperm Whales")



ggplot(datgoa, aes(year, prop))+ geom_col() +   geom_smooth(method=lm) +
  
  ggtitle("Proportion Observed Sets Depredated by Sperm Whales East")



table(goa3$year)



west1<- whales2$fmp_subarea == "BS" | whales2$fmp_subarea == "AI" | whales2$fmp_subarea == "WG" | whales2$fmp_subarea == "CG"

west <-whales2[west1,]

datwest<-aggregate(cbind(west$kwint,west$tots), list(FMP_SUBAREA=west$fmp_subarea, year=west$year),sum)

datwest$prop<-datwest$V1/datwest$V2



ggplot(datwest, aes(year, prop))+ geom_col() +  facet_wrap(vars(FMP_SUBAREA)) +  geom_smooth(method=lm) +
  
  ggtitle("Proportion Observed Sets Depredated by Killer Whales")

str(sperm)



##of note there may be an increasing frequency of kws depredating in the CG and depredation documented in SE 2014,15,16,2019

#also interesting that CG proportion depredation peaked in 2020 (0.05 sets impacted)

#############whale depredation frequency as a proportion from whales2



spermies<- tapply(goa$swint,goa$year,sum)

sp<-barplot(spermies)

tots<-tapply(goa$tots, goa$year,sum)

sperm_prop<-spermies/tots

sprop<-barplot(sperm_prop)

sp<-barplot((sperm_prop), main = "Sperm Whale Proportion Sets GOA")





kws<- tapply(west$kwint,west$year,sum)

barplot(kws)

tots_bsai<-tapply(west$tots, west$year,sum)

kw_prop<-kws/tots_bsai

orca<-barplot((kw_prop), main = "Killer Whale Proportion Sets West")

dep<-table(whales2$year, whales2$dep)





##visualizing new mm data

sperm <- aggregate(whales2$swint, list(Year=whales2$year), sum)

sperm

str(whales2)

dep<-aggregate(cbind(swint,kwint, tots) ~ fmp_subarea  + year, data = whales2, FUN = sum, na.rm = TRUE)

dep$sperm_prop <-dep$swint/dep$tots

dep$kw_prop <-dep$kwint/dep$tots







install.packages("easyGgplot2")

library(easyGgplot2)

install_github("easyGgplot2", "kassambara")

###west and east visual

par(mfrow=c(2,2))



###number of observed sets significantly down in 2021  in all reas except SE

ggplot(whales2, aes(year, tots))+ geom_col() +  facet_wrap(vars(fmp_subarea)) + ggtitle("Observed Sets")

s<-ggplot(whales2, aes(year, swint))+ geom_col() +  facet_wrap(vars(fmp_subarea)) + ggtitle("SW Dep Sets")

k<-ggplot(whales2, aes(year, kwint))+ geom_col() +  facet_wrap(vars(fmp_subarea)) + ggtitle("KW Dep Sets")



multiplot(s,k, cols=1)







ggplot(west, aes(factor(year), lat)) + geom_violin()



ggplot(subset(west, kwint=1), aes(factor(year), lat)) + geom_violin(fill = "blue")







#########################spatial shifts not particularly visible

klat<-ggplot(subset(west, kwint=1), aes(factor(year), lat)) + geom_boxplot(fill = "red") +
  
  ggtitle("Latitude of Killer Whale Depredated Sets")

klong<-ggplot(subset(west, kwint=1), aes(factor(year), long)) + geom_boxplot(fill = "red") +
  
  coord_flip() +   ggtitle("Longitude of Killer Whale Depredated Sets")



multiplot(klat, klong,cols=2)





slat<-ggplot(subset(goa,  (year >= 2005 & swint == 1)), aes(factor(year), lat)) + geom_boxplot(fill = "green")  +  
  
  ggtitle("Latitude of Sperm Whale Depredated Sets")

slong<-ggplot(subset(goa,  (year >= 2005 & swint == 1)), aes(factor(year), long)) + geom_boxplot(fill = "green") +
  
  coord_flip()  +   ggtitle("Longitude of Sperm Whale Depredated Sets")



multiplot(slat, slong,cols=2)







################################# combine adfg data ----

bind_rows(adfg_2021, adfg_old) %>%
  dplyr::mutate(long = ifelse(long > 0, long - 360, long),
                sub_area = case_when(sub_area == "CG" ~ "GOA",
                                     sub_area == "WG" ~ "GOA",
                                     sub_area == "WY" ~ "GOA",
                                     sub_area == "SE" ~ "GOA",
                                     TRUE ~ sub_area),
                fmp_area = case_when(sub_area == "BS" ~ "BS",
                                     sub_area == "AI" ~ "AI",
                                     sub_area == "GOA" & long <= -140 & long > -147 ~ "WY",
                                     sub_area == "GOA" & long <= -147 & long > -159~ "CG",
                                     sub_area == "GOA" & long <= -159 ~ "WG",
                                     TRUE ~ "SE"),
                long1 = floor(long * 3) / 3,
                lat1 = floor(lat * 3) / 3,
                loc = paste(long1, lat1, sep = "_")) %>%
  dplyr::filter(!(RA %in% c(649, 659))) %>%
  
  
  # group_by(loc, year, fmp_area) %>%
  # summarise(vms_catch = sum(tons)) %>%
  # ungroup() %>%
  mutate(region = ifelse(fmp_area %in% c("CG", "SE", "WY"), "east", "west")) -> whale_catch



head(adfg)

table(adfg$sub_area)

table(adfg_2021$sub_area)

table(adfg_old$sub_area)



summary(whale_catch)

table(whale_catch$subarea)

table(whale_catch$fmp_area)





str(adfg_2021)

table(adfg_old$sub_area)

table(adfg_old$sub_area)

table(adfg_2021$)





# combine catch and depredation datasets ---- 

whales2 %>%
  
  dplyr::group_by(loc, year) %>%
  
  dplyr::summarise(depredation = sum(int),
                   
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
                   
                   cluster = floor(mean(cluster))) %>%
  
  dplyr::left_join(whale_catch, by = c("year", "loc")) -> whales3

summary(whales3)

table(whales3$region)



whales3 %>%
  
  dplyr::filter(region == "west") %>%
  
  dplyr::mutate(Cluster = factor(cluster)) -> west_data



summary(west_data)



whales3 %>%
  
  dplyr::filter(region == "east") %>%
  
  dplyr::mutate(Cluster = factor(cluster)) -> east_data



summary(west_data)



# models ---- 



orcafin <- gam(kw ~ s(tons, k=4) + s(length, k=4)  +s(sable_cpue,k=4) + s(depth, k=4) +
                 
                 year +  s(lat, long) 
               
               + offset(log(totals)), data = west_data,
               
               family = ziP(), select= T)







orca_negbin <- gam(kw ~ s(tons, k=4) + s(length, k=4)  +s(sable_cpue,k=4) + s(depth, k=4) +
                     
                     year +  s(lat, long) 
                   
                   + offset(log(totals)), data = west_data,
                   
                   family = nb(), select= T)



summary(orcafin)

summary(orca_negbin) 



##newoutput

kw_deporca 

[1,]  0.4680641

[2,]  0.6458729

[3,]  0.7643552

[4,]  0.3584105

[5,]  0.3537434

[6,]  0.4120160





####newoutput >=2017



kw_deporca

[1,]  0.3384344

[2,]  0.8107506

[3,]  0.7956581

[4,]  0.4130514

[5,]  0.4408365

[6,]  0.4708475





> summary(bs.gammer)





reduct <- data.frame(sub_area = c("BS", "AI", "WG", "CG", "SE", "WY"),
                     
                     reduct = c(0.457, 0.577, 0.694, 0.238, 0.294, 0.263),
                     
                     reduct_se = c(0.05, 0.08, 0.06, 0.04, 0.07, 0.05))





ofin = predict(orcafin, west_data, type = "response", se = TRUE)



west_data$fit = as.numeric(ofin$fit)

west_data$fit_se = as.numeric(ofin$se.fit)





west_data %>%
  
  left_join(reduct) %>%
  
  mutate(prop_se = ((fit + fit_se) / totals) - fit / totals,
         
         ses = sqrt(prop_se * (fit / totals)^2 * reduct_se * reduct^2 +
                      
                      reduct_se * reduct^2 * ( fit / totals)^2 +
                      
                      prop_se * ( fit / totals)^2 * reduct^2))



west_data %>%
  
  mutate(fit = predict(orcafin, ., type = "response"))

