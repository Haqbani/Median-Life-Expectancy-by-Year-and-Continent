library(tidyverse)
library(readr)
library(ggplot2)
library(dplyr)
library(gapminder)

gap <- gapminder
glimpse(gap)
gap_1 <- gap %>%
    group_by(year, continent) %>%
    summarise(med_lifeExp = median(lifeExp),
              med_pop = median(pop),
              med_gdpPercap = median(gdpPercap)
#Year breaks
              year_breaks <- seq(1952, 2007, 5)
              year_labels <- c("1952", sapply(year_breaks[-1], function(x) substr(as.character(x), 3, 4)))              
              
ggplot(gap_1, aes(x = year, y = med_lifeExp, color = continent)) +
    geom_line(size = 1.5) +
    geom_point(size = 3) +
    expand_limits(y = 0) +
    labs(title = "Median Life Expectancy by Year and Continent",
         subtitle = "Life expectancy has increased over time for all continents",
         x = "Year",
         y = "Median Life Expectancy") +
    theme_minimal() +
    theme(
        plot.title = element_text(size = 20, face = "bold", family = "Roboto", hjust = 0),
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        plot.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt")
    ) +
    scale_color_brewer(palette = "Set4") +
    scale_x_continuous(breaks = year_breaks, labels = year_labels) +
    scale_y_continuous(breaks = seq(0, 90, 10)) +
    geom_smooth(method = "lm", se = FALSE, aes(group = 1))





























































