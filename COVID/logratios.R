# According to https://robjhyndman.com/hyndsight/logratios-covid19/
remotes::install_github("joachim-gassen/tidycovid19")
library(tidyverse)
library(tsibble)
library(tidycovid19)
library(dplyr)
library(ggplot2)

updates <- download_merged_data(cached = TRUE)
countries <- c("BRA", "ITA", "USA", "GBR")

updates %>%
  mutate(deaths_logratio = difference(log(deaths))) %>%
  filter(
    iso3c %in% countries,
    date >= as.Date("2020-03-01")
  ) %>%
  ggplot(aes(x = date, y = deaths_logratio, col = country)) +
  geom_point() +
  scale_y_continuous(limits = c(0,0.5)) +
  geom_smooth(method = "loess") +
  facet_wrap(. ~ country, ncol = 2) +
  xlab("Date") +
  ggthemes::scale_color_colorblind()
