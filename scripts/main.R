library(tidyverse)
library(here)
library(readr)
library(ggplot2)


# Load data using here

billionaires <- here("data_prep", "df_ready.csv") %>%
  read_csv()

# Aggregate the data to count the number of billionaires by industry

industry_counts <- billionaires %>%
  group_by(industry) %>%
  summarise(num_billionaires = n_distinct(full_name))

# Create a bar plot with rotated x-axis labels

ggplot(industry_counts, aes(x = reorder(industry, -num_billionaires), y = num_billionaires)) +
  geom_bar(stat = "identity") +
  labs(x = "Industry", y = "Number of Billionaires", title = "Distribution of Billionaires by Industry") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# Save the plot

ggsave(here("output", "billionaires_by_industry.png"))

