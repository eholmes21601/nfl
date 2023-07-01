library(XML)
library(tidyverse)
library(rvest)
library(stringr)
library(magrittr)
teams <- read_csv("teams.csv")

conf <- function(year = 2020)
{
  URL <- paste("https://www.pro-football-reference.com/years/",year,"/", sep = "")
  nfl_html_data <-  read_html(URL)
  afc <- nfl_html_data %>% html_nodes("table.sortable") %>%  .[[1]] %>% html_table() 
  nfc <- nfl_html_data %>% html_nodes("table.sortable") %>%  .[[2]] %>% html_table()
  combined <- afc %>% select(-"W-L%") %>%  dplyr::filter(!grepl('AFC', Tm) )  %>% rename(team = Tm) %>% 
    bind_rows(nfc %>% select(-"W-L%") %>%  dplyr::filter(!grepl('NFC', Tm) )  %>% rename(team = Tm))
  return(combined %>% mutate("year" = year, playoff = if_else(grepl("[[:punct:]]",str_sub(team, -1)), "Y", "N"),
                             team = if_else(grepl("[[:punct:]]",str_sub(team, -1)), str_sub(team, 1, nchar(team)-1), team)) %>% 
           relocate("year", .after = team) %>%   
           mutate(team =dplyr::recode(team, "Washington Redskins" = "Washington Commanders","Washington Football Team" = "Washington Commanders", "San Diego Chargers" = "Los Angeles Chargers",
                                      "St. Louis Rams" = "Los Angeles Rams", 
                                      "Oakland Raiders" = "Las Vegas Raiders",
                                      "Los Angeles Raiders" = "Las Vegas Raiders"))%>% 
           left_join(teams, by = c("team")) %>% relocate(c(division, conf), .after = "year") %>%
           arrange(team))
}

nfl <- NULL
for(i in 2002: 2022)
{
  nfl <- nfl %>% bind_rows(conf(i))
}

saveRDS(nfl, file = "nfl.rds")