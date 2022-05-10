library(tidyverse)
library(rvest)
library(magrittr)
library(tidyr)
library(countrycode)
library(democracyData)
library(janitor)
library(waffle)

pko_members <- read_html("https://en.wikipedia.org/wiki/List_of_United_Nations_peacekeeping_missions")
pko_tables <- pko_members %>% html_table(header = TRUE, fill = TRUE)

pko_complete_africa <- pko_tables[[1]]
pko_complete_americas <- pko_tables[[2]]
pko_complete_asia <- pko_tables[[3]]
pko_complete_europe <- pko_tables[[4]]
pko_complete_mena <- pko_tables[[5]]

rbind(pko_complete_africa, pko_complete_americas, pko_complete_asia, pko_complete_europe, pko_complete_mena) -> pko_complete

pko_complete %<>% 
  mutate(complete = ifelse(!is.na(pko_complete$Location), "Complete", "Current"))

pko_current_africa <- pko_tables[[6]]
pko_current_asia <- pko_tables[[7]]
pko_current_europe <- pko_tables[[8]]
pko_current_mena <- pko_tables[[9]]

rbind(pko_current_europe, pko_current_mena, pko_current_asia, pko_current_africa) -> pko_current

pko_current %<>% 
  mutate(complete = ifelse(!is.na(pko_current$Location), "Current", "Complete"))

rbind(pko_complete, pko_current) -> pko

pacl <- redownload_pacl()

pacl %>% 
  select(cown = pacl_cowcode,
         un_region_name, un_continent_name) %>% 
  distinct(cown, .keep_all = TRUE) -> pacl_region

pko %>% 
  janitor::clean_names() %>% 
  group_by(conflict) %>%
  mutate(location = paste(location, collapse = ', ')) %>% 
  separate(location,  into = c("country_1", "country_2", "country_3", "country_4", "country_5"), sep = ", ")  %>% 
  ungroup() %>% 
  distinct(conflict, .keep_all = TRUE)  %>%  
  mutate(acronym = str_extract_all(name_of_operation, "\\([^()]+\\)")) %>% 
  mutate(acronym = substring(acronym, 2, nchar(acronym)-1)) %>% 
  separate(dates_of_operation, c("start_date", "end_date"), "-") %>% 
  mutate(end_date = ifelse(complete == "Current", 2022, end_date)) %>% 
  mutate(end_date = as.integer(end_date)) %>% 
  mutate(start_date = as.integer(start_date)) %>% 
  mutate(duration = ifelse(!is.na(end_date), end_date - start_date, 1)) %>% 
  mutate(cown = countrycode(country_1, "country.name", "cown")) %>% 
  mutate(cown = ifelse(country_1 == "Western Sahara", 605, 
                       ifelse(country_1 == "Serbia", 345, cown))) -> pko_graph

pko_graph %>% 
  inner_join(pacl_region, by = "cown") %>% 
  ggplot(mapping = aes(x = forcats::fct_reorder(un_region_name, duration), 
                       y = duration, 
                       fill = un_region_name)) +
  geom_boxplot(alpha = 0.4) +
  geom_jitter(aes(color = un_region_name),
              size = 6, alpha = 0.8, width = 0.15) +
  coord_flip() + 
  bbplot::bbc_style() + ggtitle("Duration of Peacekeeping Missions")


