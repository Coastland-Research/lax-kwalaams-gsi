# Code to process Lax Kw'alaams sockeye GSI data and create figures
# Coastland Research
# March, 2025

library(readxl)
library(tidyverse)
library(gt)

# Notes -------------------------------------------------------------------
#
# Inventory: summary of numbers of samples taken for each stat-area/week collection
#
# custom_estimates: 4 columns for each collection, showing (1) estimated percent of samples from each 
# *stock origin*, (2) SD, (3) low estimate, and (4) high estimate
#
# repunits_estimates: 4 columns for each collection, showing (1) estimated percent of samples from each 
# *Conservation Unit*, (2) SD, (3) low estimate, and (4) high estimate
#
# custom_table_ids: prob.1 is probability of first assignment ( > 80 ~high confidence) prob.2 is probability
# of an alternative stock
#
# sum: Nass other + Meziadin
#      Skeena other + Babine

# Figure showing each stat-area/week collection
# column chart with x = collection, y = proportion, fill = reporting group (stock origin)


# Data by 8 groups --------------------------------------------------------

inventory <- read_excel("data/LaxKwalaams_2024results_8groups.xlsx", sheet = "Inventory")

custom_estimates <- read_excel("data/LaxKwalaams_2024results_8groups.xlsx", sheet = "custom_est_long")

repunits_estimates <- read_excel("data/LaxKwalaams_2024results_8groups.xlsx", sheet = "repunits_estimates")

custom_table_ids <- read_excel("data/LaxKwalaams_2024results_8groups.xlsx", sheet = "custom_table_ids")


# Plot proportion of each stock by collection -----------------------------

custom_estimates %>%
  rename(`Stock` = stock) %>%
  ggplot(aes(x = collection, y = est, fill = Stock)) +
  geom_col() +
  scale_fill_brewer(palette = "Spectral") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=0.5)) +
  labs(x = "Week - Stat Area Collection", y = "Proportion of Samples") +
  ggtitle("Lax Kw'alaams GSI Results for All Reporting Groups")



# Data for Skeena and Nass only -------------------------------------------

est_skeena_nass <- read_excel("data/LaxKwalaams_2024results_3groups.xlsx" , sheet = "custom_est_long")

inventory_skn <- read_excel("data/LaxKwalaams_2024results_3groups.xlsx", sheet = "inventory_long")

repunits_estimates_skn <- read_excel("data/LaxKwalaams_2024results_3groups.xlsx", sheet = "repunits_estimates")

custom_table_ids_skn <- read_excel("data/LaxKwalaams_2024results_3groups.xlsx", sheet = "custom_table_ids")



# Plot proportions (just Skeena and Nass origin) --------------------------

est_skeena_nass %>%
  rename(`Stock` = stock) %>%
  ggplot(aes(x = collection, y = est, fill = Stock)) +
  geom_col() +
  scale_fill_brewer(palette = "Spectral") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=0.5)) +
  labs(x = "Week - Stat Area Collection", y = "Proportion of Samples") +
  ggtitle("Lax Kw'alaams GSI Results for Skeena and Nass Reporting Groups")


# Table for Skeena and Nass -----------------------------------------------
# include: collection, catch, percent Skeena, percent Nass, # Skeena, # Nass

skn_data <- left_join(est_skeena_nass, inventory_skn, by="collection")

# format dataframe with table info
skn_data <- skn_data %>%
  select(collection, stock, est, n_reported) %>% 
  pivot_wider(
    names_from = stock, 
    values_from = c(est, n_reported),
    names_prefix = "percent_"
  ) %>%
  rename(percent_Skeena = est_percent_Skeena, percent_Nass = est_percent_Nass,
         n_reported_Skeena = n_reported_percent_Skeena, n_reported_Nass = n_reported_percent_Nass) %>%
  mutate(
    number_Skeena = percent_Skeena * n_reported_Skeena * 0.01,
    number_Nass = percent_Nass * n_reported_Nass * 0.01,
    n_reported = coalesce(n_reported_Skeena, n_reported_Nass)
  ) %>%
  select(collection, n_reported, percent_Skeena, percent_Nass, number_Skeena, number_Nass)

# create a table using gt
skeena_nass_tbl <- gt(skn_data) %>% 
  cols_label(
    collection = "Stat Area-Week",
    n_reported = "Samples",
    percent_Skeena = "Percent Skeena",
    percent_Nass = "Percent Nass",
    number_Skeena = "Total Skeena",
    number_Nass = "Total Nass") %>%
  fmt_number(decimals = 0)


