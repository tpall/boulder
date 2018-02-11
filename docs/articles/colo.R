## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE,
  fig.align = "center"
)

## ------------------------------------------------------------------------
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(boulder)
tabs <- get_all_tables(lang = "en")
tabs

## ------------------------------------------------------------------------
tabs %>% 
  mutate(descr = str_to_lower(Title)) %>% 
  filter(str_detect(descr, "neoplasm")) %>% 
  select(Name, Title, Node, Updated) %>% 
  knitr::kable()

## ------------------------------------------------------------------------
pk30 <- pull_table("PK30", lang = "en")
pk30

## ------------------------------------------------------------------------
pk30 %>% 
  select(Site) %>% 
  distinct()

## ------------------------------------------------------------------------
colo <- pk30 %>% 
  mutate_at("Site", str_to_lower) %>% 
  filter(str_detect(Site, "colon"))
colo

## ------------------------------------------------------------------------
colo %>% 
  mutate(Year = as.numeric(Year),
         `Age group` = factor(`Age group`, levels = unique(colo$`Age group`)),
         Site = gsub("^[.]+([a-z])", "\\U\\1", Site, perl = TRUE)) %>% 
  ggplot(aes(Year, value, group = `Age group`, color = `Age group`)) +
  geom_line() +
  facet_wrap(~ Sex) +
  scale_color_viridis_d() +
  guides(color = guide_legend(ncol = 2)) +
  labs(title = "Incidence of colon (C18) cancer",
       y = "Age-specific incidence per 100 000")

## ------------------------------------------------------------------------
tabs %>% 
  mutate(Node = str_to_lower(Node)) %>% 
  filter(str_detect(Node, "deaths"))

## ------------------------------------------------------------------------
sd22 <- pull_table("SD22", lang = "en")
sd22

## ------------------------------------------------------------------------
mort <- sd22 %>% 
  mutate(`Cause of death` = str_to_lower(`Cause of death`)) %>% 
  filter(str_detect(`Cause of death`, "c18"))
mort

## ------------------------------------------------------------------------
colo_mort <- mort %>% 
  filter(Year >= 2000, 
         `Age group` %in% colo$`Age group`,
         !is.na(value))
colo_mort

## ------------------------------------------------------------------------
colo_mort %>% 
  mutate(Year = as.numeric(Year),
         `Age group` = factor(`Age group`, levels = unique(colo_mort$`Age group`)),
         `Cause of death` = gsub("^[.]+([a-z])", "\\U\\1", `Cause of death`, perl = TRUE)) %>% 
  ggplot(aes(Year, value, group = `Age group`, color = `Age group`)) +
  geom_line() +
  facet_wrap(~ Sex) +
  scale_color_viridis_d() +
  guides(color = guide_legend(ncol = 2)) +
  labs(title = "Colon cancer (C18) mortality",
       y = "Deaths per 100 000")

## ------------------------------------------------------------------------
c18 <- inner_join(
  colo %>% select(-Site) %>% rename(incidence = value), 
  colo_mort %>% select(-`Cause of death`) %>% rename(mortality = value)
  ) %>% 
  filter(incidence != 0)
c18

## ------------------------------------------------------------------------
c18 %>% 
  mutate(Year = as.numeric(Year)) %>% 
  gather(key, value, -c("Year", "Sex", "Age group")) %>%
  ggplot(aes(Year, value, color = key)) +
  geom_line() +
  facet_grid(Sex ~ `Age group`) +
  scale_color_viridis_d(direction = -1) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 6),
        legend.title = element_blank(),
        legend.position = "bottom") +
  labs(y = "Incidence vs. mortality per 100000")

