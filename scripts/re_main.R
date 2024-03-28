# create a new R script file in the scripts folder

# create a simple plot to show the gdp-growth in relation the to number of billionaires

# import relevant packages

library(tidyverse)
library(here)
library(ggplot2)

# load the dataframe using here package

df_billionaires_gdp <- read_csv(here("data", "re_data.csv"))

# create aggregated number of billionaires per country

df_billionaires_gdp <- df_billionaires_gdp %>%
  group_by(country_of_residence) %>%
  summarize(number_of_billionaires = n(),
            GDP_growth = mean(GDP_growth, na.rm = TRUE))

# create a plot showing the gdp-growth on the x-axis and the number of billionaires on the y-axis

ggplot(df_billionaires_gdp, aes(x = GDP_growth, y = number_of_billionaires)) +
  geom_point() +
  labs(title = "Number of Billionaires vs. GDP Growth",
       x = "GDP Growth Rate (%)",
       y = "Number of Billionaires") +
  theme_minimal()

# save the plot as a png file

ggsave(here("output", "billionaires_vs_gdp.png"), width = 6, height = 4, dpi = 300)

