
#Import libararies

library(tidyverse)
library(scales)
library(janitor)
library(ggrepel)
library(countrycode)
theme_set(theme_classic())

#Import dataset
crops <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/key_crop_yields.csv')
arable_land <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/arable_land_pin.csv')


#Data cleaning
yields <- crops %>%
  clean_names() %>%
  rename_all(str_remove, "_tonnes.*")

arable <- arable_land %>% 
  janitor::clean_names() %>% 
  rename(arable_land_needed = 4)
head(arable)

#Data analysis
yields_tidy <- yields %>%
  pivot_longer(wheat:bananas, names_to = "crop", values_to = "yield") %>%
  filter(!is.na(yield)) %>%
  mutate(crop = str_replace_all(crop, "_", " "),
         crop = str_to_title(crop))

yields_tidy %>%
  filter(code == "USA") %>%
  mutate(crop = fct_reorder(crop, -yield)) %>%
  ggplot(aes(year, yield)) +
  geom_line() +
  facet_wrap(~ crop)

crop_yields_50_years <- yields_tidy %>%
  arrange(entity, year) %>%
  filter(year >= 1968) %>%
  group_by(entity, code, crop) %>%
  summarize(year_start = min(year),
            year_end = max(year),
            yield_start = first(yield),
            yield_end = last(yield)) %>%
  ungroup() %>%
  filter(year_start == 1968) %>%
  mutate(yield_ratio = yield_end / yield_start)

#visualize
arable %>%
  filter(code == "USA") %>%
  ggplot(aes(year, arable_land_needed)) +
  geom_line()

yields_tidy %>%
  filter(code == "USA") %>%
  mutate(crop = fct_reorder(crop, -yield)) %>%
  ggplot(aes(year, yield, color = crop)) +
  geom_line() +
  labs(x = "Year",
       y = "Yield (tonnes per hectare)",
       title = "Crop yields in the Nepal over time",
       color = "Crop")

crop_yields_50_years %>%
  filter(is.na(code)) %>%
  filter(entity %in% c("Africa", "Asia", "Northern America", "South America", "Oceania")) %>%
  ggplot(aes(yield_start, yield_end, color = entity)) +
  geom_abline(color = "green") +
  geom_point() +
  expand_limits(y = 0, x = 0) +
  facet_wrap(~ crop, scales = "free") +
  labs(x = "Tonnes per hectare in 1968",
       y = "Tonnes per hectare in 2018",
       color = "Continent")

crop_yields_50_years %>%
  filter(crop == "Rice",
         !is.na(code)) %>%
  mutate(continent = countrycode(code, "iso3c", "continent")) %>%
  filter(!is.na(continent)) %>%
  ggplot(aes(yield_start, yield_ratio)) +
  geom_point(aes(color = continent)) +
  scale_x_log10() +
  scale_y_log10(breaks = c(.25, .5, 1, 2, 4),
                labels = c("1/4X", "1/2X", "Same", "2X", "4X")) +
  geom_hline(yintercept = 1, linetype = "dotted") +
  geom_text_repel(aes(label = entity), force = .1,
                  size = 2.5) +
  labs(x = "1968 yield (tonnes per hectare), log scale",
       y = "(2018 yield) / (1968 yield), log scale",
       color = "Continent",
       title = "How has paddy efficiency changed across countries?")

