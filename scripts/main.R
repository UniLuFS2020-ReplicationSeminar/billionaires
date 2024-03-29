library(tidyverse)
library(here)
library(readr)
library(ggplot2)
library(dplyr)


# Load data using here

billionaires <- here("data_prep", "df_ready.csv") %>%
  read_csv()

# Aggregate the data to count the number of billionaires by industry

industry_counts <- billionaires %>%
  group_by(industry) %>%
  summarise(num_billionaires = n_distinct(full_name))


# Create df for swiss billionaires (citizenship)

swiss_billionaires <- billionaires %>%
  filter(citizenship == "Switzerland")

swiss_industry_counts <- swiss_billionaires %>%
  group_by(industry) %>%
  summarise(num_swiss_billionaires = n_distinct(full_name))


# Create a bar plot with rotated x-axis labels

ggplot(industry_counts, aes(x = reorder(industry, -num_billionaires), y = num_billionaires)) +
  geom_bar(stat = "identity") +
  labs(x = "Industry", y = "Number of Billionaires", title = "Distribution of Billionaires by Industry") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save the plot

ggsave(here("output", "billionaires_by_industry.png"))


# Create the same plot but only for Swiss billionaires

ggplot(swiss_industry_counts, aes(x = reorder(industry, -num_swiss_billionaires), y = num_swiss_billionaires)) +
  geom_bar(stat = "identity") +
  labs(x = "Industry", y = "Number of Billionaires", title = "Distribution of swiss Billionaires by Industry") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


# Save the plot

ggsave(here("output", "swiss_billionaires_by_industry.png"))

# commenting out the code below

# create a Plot comparing the wealth of billionaires by the tax rate of their country

#ggplot(billionaires, aes(x = tax_rate, y = wealth, color = country_lat)) +
  #geom_point() +
  #labs(x = "Tax Rate", y = "Wealth", title = "Wealth of Billionaires by Tax Rate") +
  #theme(legend.position = "bottom")

# create a plot comparing the number of billionaires per country with the gdp of each country

# aggregate the number of billionaires by country and assign it to object billionaires_by_country

billionaires_by_country <- billionaires %>%
  group_by(country_of_residence) %>%
  summarise(num_billionaires = n_distinct(full_name))

# create a dataframe with the gdp of each country in the billionaires dataframe

gdp_data <- data.frame(
  country_of_residence = c("United States", "China", "Germany", "India", "Russia", "Switzerland", "Brazil", "Hong Kong", "United Kingdom", "Italy"),
  gdp_country = c(65297, 10700, 4470, 2350, 1283, 678, 2140, 364, 2910, 2080)
)

## We should definitely use all the data we have by using the csv datafile for the gdp
gdp_growth_df <- here("data_prep", "GDP_growth.csv") %>%
  read_csv()

# I suppose we only use the newest year (2021 for simplicity reasons)
gdp_growth_2021 <- gdp_growth_df %>%
  filter(Year == 2021)

#rename column Entity and 17.13.1 - Annual GDP growth (%) - NY_GDP_MKTP_KD_ZG

gdp_growth_2021 <- gdp_growth_2021 %>%
  rename(country_of_residence = Entity) %>%
  rename(gdp_growth_country = "17.13.1 - Annual GDP growth (%) - NY_GDP_MKTP_KD_ZG")

# compare the number of billionaires by country with the gdp of each country

billionaires_by_country_gdp <- billionaires_by_country %>%
  left_join(gdp_growth_2021, by = "country_of_residence")

# create a regression plot comparing the gdp of each country with the number of billionaires. the x axis should state each country's name, the names should be rotated, so the plot is readable. 

ggplot(billionaires_by_country_gdp, aes(x = reorder(country_of_residence, -num_billionaires), y = num_billionaires)) +
  geom_point(aes(size = gdp_country), color = "blue") +
  labs(x = "Country", y = "Number of Billionaires", title = "Number of Billionaires by Country and GDP") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# remove the NA from the dataset

billionaires_by_country_gdp <- billionaires_by_country_gdp %>%
  drop_na()

ggplot(billionaires_by_country_gdp, aes(x = reorder(country_of_residence, -num_billionaires), y = num_billionaires)) +
  geom_point(aes(color = gdp_growth_country), size = 3) +
  scale_color_gradient2(low = "red", mid = "yellow", high = "green", midpoint = 0) +
  labs(x = "Country", y = "Number of Billionaires", title = "Number of Billionaires by Country and GDP") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# maybe we should try out the number of billioaires on y and the gdp on x axis

ggplot(billionaires_by_country_gdp, aes(x = gdp_growth_country, y = num_billionaires)) +
  geom_point(aes(color = gdp_growth_country), size = 3) +
  scale_color_gradient2(low = "red", mid = "orange", high = "green", midpoint = 0) +
  labs(x = "GDP", y = "Number of Billionaires", title = "Number of Billionaires by Country and GDP") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Calculate correlation
corr_billionaires_by_country_gdp <- cor.test(billionaires_by_country_gdp$num_billionaires, billionaires_by_country_gdp$gdp_growth_country, method = "pearson")

print(corr_billionaires_by_country_gdp)

#our p-value is 0.65 which basically means that its 65% chance that our results happened by chance
#=> we cant reject our null-hypothesis
#our correlation coefficient is 0.05 which is also really low
#=> even if the p-value was significant, the correlation would be really low


#running a regression modell
Regression_billionaires_gdp <- lm(gdp_growth_country ~ num_billionaires, data = billionaires_by_country_gdp)
summary(Regression_billionaires_gdp)

# save the plot in output folder

ggsave(here("output", "gdp_vs_num_billionaires.png"))

