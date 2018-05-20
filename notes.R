library(tidyverse)
library(plotly)
library(scales)
library(ggalt)
# data
# 1. get ugoe org data
h2020_org <- readr::read_delim("data/cordis-h2020organisations.csv", delim = ";",
                               locale = locale(decimal_mark = ",")) %>%
  mutate_if(is.integer, as.character) %>%
  filter(id == "999845640")
# 2. get project info
h2020_projects <- readr::read_delim("data/cordis-h2020projects.csv", delim = ";",
                                    locale = locale(decimal_mark = ",")) %>%
  mutate_if(is.integer, as.character) %>%
  inner_join(h2020_org, by = c("id" = "projectID")) %>%
  mutate(start_year = lubridate::year(startDate)) %>%
  separate(fundingScheme, "funding_scheme") -> ugoe_all
# charts
# signed grants per year
ugoe_all %>%
  group_by(funding_scheme, start_year, role) %>%
  summarize(n = n(),
            n_euro = sum(ecContribution)) %>%
  ggplot(aes(start_year, n, fill = role)) + 
  geom_bar(stat = "identity") +
  xlab("Year") +
  ylab("Signed Grants") +
  viridis::scale_fill_viridis("Role", discrete = TRUE) +
  theme_minimal()
# top funded projects
ugoe_all %>%
  group_by(funding_scheme) %>%
  summarise(sum(ecContribution, na.rm = TRUE))




# table 

# contributions
ugoe_all %>%
  
# collaborators

# open access

ugoe_all %>%
  