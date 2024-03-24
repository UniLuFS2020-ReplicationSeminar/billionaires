library(tidyverse)
library(here)
library(readr)

# Load data using here
billionaires <- here("data_prep", "df_ready.csv") %>%
  read_csv()

# View the data
View(billionaires)
