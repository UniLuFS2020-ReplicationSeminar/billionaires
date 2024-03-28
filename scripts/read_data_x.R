
# This script reads the data from the data_prep folder and loads it into the R environment.

library(tidyverse)
library(here)
library(readr)
library(ggplot2)


# Read the data from the data_prep folder

# Billionaires data

billionaires <- here("data_prep", "df_ready.csv") %>%
  read_csv()

# GDP_growth_data

GDP_data_raw <- here("data_prep", "GDP_growth.csv") %>%
  read_csv()



# Function to calculate the geometric mean of the GDP growth rate:

gm_mean <- function(x, na.rm=TRUE) {
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}

# rename colum for easier tidying

GDP_data_rename <- GDP_data_raw %>% 
  rename(GDP_gr = `17.13.1 - Annual GDP growth (%) - NY_GDP_MKTP_KD_ZG`)

# Average GDP growth rate for each country (from 2000-2021)

GDP_data <- GDP_data_rename %>%
  filter(Year > 2000) %>%
  group_by(Entity) %>% 
  summarize(GDP_growth = gm_mean(GDP_gr)) 

# Chose Key to join the data

GDP_ready_join <- GDP_data %>% 
  rename(country_of_residence = Entity)


# Join the data 

re_data <- left_join(billionaires, GDP_ready_join, by = "country_of_residence")

#saveRDS(re_data, here("data", "re_data.rds"))

# save the data as a csv file, as working with .rds was not possible. 

write_csv(re_data, here("data", "re_data.csv"))

        
        