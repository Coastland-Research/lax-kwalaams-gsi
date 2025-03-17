library(tidyverse)
library(gt)


GSI_samples <- read_csv("lax_kwalaams_GSI.csv") %>%
  mutate(date = as.Date(date, "%m/%d/%y"))

#barplot of samples by date and area:
GSI_samples %>%
  ggplot(aes(x = date, fill = location)) +
  geom_bar() +
  scale_fill_brewer(palette = "Spectral") +
  scale_x_date(date_breaks = "days" , date_labels = "%d-%b")+
  labs(x = "Date", y = "Number of sockeye sampled") +
 # ggtitle("Lax Kw'alaams Sockeye GSI samples collected by date and area")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45), legend.position = "bottom")

# table with number of samples by day by area
gsi_grouped <- GSI_samples %>%
  mutate(week = case_when(date == "6/26/24" | date == "6/29/24" ~ "week 1",
                          date == "6/30/24" | date == "7/1/24" | date == "7/2/24" | date == "7/3/24" | date == "7/6/24" ~ "week 2",
                          date == "7/7/24" | date == "7/11/24" ~ "week 3",
                          date == "7/14/24" ~ "week 4")) %>%
  rename(Location = location) %>%
  group_by(week, date) %>%
  summarise(Samples = n()) %>%
  ungroup()

gsi_table <- gt(gsi_grouped, groupname_col = 'week', rowname_col = 'date') %>%
  fmt_number(
    columns = c(Samples),
    drop_trailing_zeros = TRUE) %>%
  tab_header(title = "Total GSI samples collected by Week")
print(gsi_table)

# group samples by week
gsi_weekly <-GSI_samples %>%
  mutate(week = case_when(date == "6/26/24" | date == "6/29/24" ~ "week 1",
                          date == "6/30/24" | date == "7/1/24" | date == "7/2/24" | date == "7/3/24" | date == "7/6/24" ~ "week 2",
                          date == "7/7/24" | date == "7/11/24" ~ "week 3",
                          date == "7/14/24" ~ "week 4")) %>%
  rename(Location = location) %>%
  group_by(Location) %>%
  summarize(Samples = n())

#table of samples by Area
area_tbl <- gt(gsi_weekly) %>%
  fmt_number(
    columns = vars(Samples),
    drop_trailing_zeros = TRUE) %>%
  tab_header(title = "Total GSI samples collected by Area")
print(area_tbl)
  