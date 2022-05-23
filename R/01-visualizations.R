# sablefish whale depredation corrected catch visualizations
# ben.williams@noaa.gov
# 2022-05

# load ----

library(tidyverse)
library(vroom)
library(tidytable)
library(scico)
library(ggridges)
library(gganimate)
library(PBSmapping)
data('nepacLLhigh') 
nepacLLhigh %>% 
  dplyr::select(group=PID, POS=POS,long=X, lat=Y) -> ak 

theme_set(ggplot2::theme_light() +
  ggplot2::theme(
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),

    strip.background = ggplot2::element_rect(fill = NA, colour = NA),
    strip.text.x = element_text(colour = "black"),
    strip.text.y = element_text(colour = "black"),
    panel.border = element_rect(fill = NA),
    legend.key.size = grid::unit(0.9, "lines"),
    legend.key = ggplot2::element_rect(colour = NA, fill = NA),
    legend.background = ggplot2::element_rect(colour = NA, fill = NA)
    )
  )


# data ----

whales <- vroom::vroom(here::here("output", "whales_share.csv"))

whales %>% 
  group_by(year, fmp_subarea) %>% 
  summarise(sprop = sum(swint) / sum(tots),
            kprop = sum(kwint) / sum(tots)) %>% 
  ungroup() -> interactions

# plots ----
# sets depredated
interactions %>% 
  filter.(!(fmp_subarea %in% c("BS", "AI")), year >= 2001) %>% 
  ggplot(aes(year, sprop)) +
  geom_col() + 
  facet_wrap(~fmp_subarea) + 
  stat_smooth(method=lm) +
  ggtitle("Proportion Observed Sets Depredated by Sperm Whales")

interactions %>% 
  filter.(!(fmp_subarea %in% c("BS", "AI")), year >= 2001) %>% 
  ggplot(aes(year, sprop)) +
  geom_col() + 
  # facet_wrap(~fmp_subarea) + 
  stat_smooth(method=lm) +
  ggtitle("Proportion Observed Sets Depredated by Sperm Whales East")

interactions %>% 
  filter.(fmp_subarea %in% c("BS", "AI", "WG", "CG"), year >= 2001) %>%
  ggplot(aes(year, kprop)) +
  geom_col() + 
  facet_wrap(~fmp_subarea) +
  stat_smooth(method=lm) +
  ggtitle("Proportion Observed Sets Depredated by Killer Whales")

interactions %>% 
  filter.(fmp_subarea %in% c("BS", "AI", "WG", "CG"), year >= 2001) %>%
  ggplot(aes(year, kprop)) +
  geom_col() + 
  # facet_wrap(~fmp_subarea) +
  stat_smooth(method=lm) +
  ggtitle("Proportion Observed Sets Depredated by Killer Whales West")

# direct numbers 

whales %>% 
  filter.(year >= 2001) %>% 
  pivot_longer(c(tots, swint, kwint)) %>% 
  ggplot(aes(year, value)) + 
  geom_col(aes(fill = name)) +
  facet_wrap(~fmp_area, scales = "free_y") + 
  scale_fill_scico_d(palette= 'grayC', begin = 0.2, end = 0.8, direction = -1,
                     name ="", labels = c("orca", "sperm whale", "total")) + 
  ylab("Number of sets\n") + 
  xlab("\nYear")

whales %>% 
  filter.(year >= 2001) %>% 
  pivot_longer(c(tots, swint, kwint)) %>% 
  ggplot(aes(year, value)) + 
  geom_col(aes(fill = name)) +
  facet_wrap(~fmp_subarea, scales = "free_y") + 
  scale_fill_scico_d(palette= 'grayC', begin = 0.2, end = 0.8, direction = -1,
                     name ="", labels = c("orca", "sperm whale", "total")) +  
  ylab("Number of sets") + 
  xlab("\nYear")

# location shifts
whales %>% 
  mutate.(month = month / 100,
          gif_date = year + month) %>% 
  filter.(year >= 2001) %>% 
  group_by(year, long, lat, fmp_subarea) %>% 
  summarise(swint = sum(swint),
            kwint = sum(kwint)) %>% 
  ungroup() %>% 
  pivot_longer(c(swint, kwint), names_to = "whales") %>% 
  filter(value!=0) -> dat

# annual timesteps 
# orcas
# longitude
dat %>% 
  filter(whales == "kwint") %>% 
  ggplot(aes(long, factor(year))) + 
  geom_density_ridges(fill = 4)

# sperm whales 
dat %>% 
  filter(whales == "swint") %>% 
  ggplot(aes(long, factor(year))) + 
  geom_density_ridges(fill = 4)

# same but reduce longitude range
dat %>% 
  filter(whales == "swint") %>% 
  ggplot(aes(long, factor(year))) + 
  geom_density_ridges(fill = 4) + 
  coord_cartesian(xlim = c(-160, -130))

# orcas
# latitude
dat %>% 
  filter(whales == "kwint") %>% 
  ggplot(aes(lat, factor(year))) + 
  geom_density_ridges(fill = 4)

dat %>% 
  filter(whales == "swint") %>% 
  ggplot(aes(lat, factor(year))) + 
  geom_density_ridges(fill = 4)

# median location by year 
whales %>% 
  filter.(year >= 2001) %>% 
  select.(year, long1, lat1, swint, kwint) %>% 
  pivot_longer(c(swint, kwint), names_to = "whales") %>% 
  filter(value!=0) %>% 
  group_by(year, whales) %>% 
  summarise(long = median(long1),
            lat = median(lat1)) %>% 
  group_by(whales) %>% 
  arrange(year) %>% 
  mutate(end.long = lead(long),
         end.lat = lead(lat)) %>% 
  ungroup() %>% 
  ggplot(aes(long, lat, color = year)) + 
  geom_point(alpha = 0.7) + 
  geom_segment(aes(xend=end.long, yend=end.lat)) + 
  facet_wrap(~whales, scales = "free") + 
  scale_color_scico(palette = 'roma') + 
  xlab(expression(paste(Longitude^o,~'W'))) +
  ylab(expression(paste(Latitude^o,~'N')))


# this will take a little while to process

whales %>% 
  mutate.(month = month / 100,
          gif_date = year + month) %>% 
  filter.(year >= 2001) %>% 
  group_by(gif_date, long, lat, fmp_subarea) %>% 
  summarise(swint = sum(swint),
            kwint = sum(kwint)) %>% 
  ungroup() %>% 
  pivot_longer(c(swint, kwint), names_to = "whales") %>% 
  filter(value!=0) -> mapdat

ggplot() + 
  geom_polygon(data = ak, aes(long, lat, group = group), fill=8, color='black') +
  theme(panel.background = element_rect(fill = 'white')) + 
  xlab(expression(paste(Longitude^o,~'W'))) +
  ylab(expression(paste(Latitude^o,~'W'))) + 
  coord_map(xlim = c(-179.9, -135), ylim = c(50, 61)) +
  geom_point(data = mapdat, aes(long, lat, color = whales, size = value), alpha = 0.7, show.legend = FALSE) + 
  scale_size(range = c(2, 12)) + 
  facet_wrap(~whales) + 
  labs(title = 'Date: {frame_time}', x = 'Longitude', y = 'Latitude') +
  transition_time(gif_date) +
  ease_aes('linear')
