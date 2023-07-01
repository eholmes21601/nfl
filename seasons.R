rm(list = ls())
library(XML)
library(tidyverse)
library(rvest)
library(stringr)
library(magrittr)
nfl <- readRDS(file = "nfl.rds")

get_graph <- function(div = "E", con = "N")
{
  nfl %>% dplyr::filter(division == div & conf ==con) %>% mutate(W = as.numeric(W),L = as.numeric(L),T = as.numeric(T),
                                                                 T = ifelse(is.na(T), 0, T), winn_per = (W + 0.5 * T)/(W + L + T)) %>% 
    relocate(winn_per, .after = T) %>% ggplot() +geom_smooth(aes(x=year, y=winn_per, color=team), se = F)  + 
    labs(color = "Team",x = "Year", y = "Team's Season Winning Percentage",title = paste(con,"FC", sep="", "-", 
                                                                                         if_else(div=="E", "East", if_else(div=="N", "North", if_else(div=="S", "South", "West"))))) +
    theme(plot.title = element_text(hjust = 0.5)) +ylim(0,1)+ scale_x_continuous( n.breaks = 10) 
}





get_graph(div="N", con="A")
get_graph(div="S", con="A")
get_graph(div="E", con="A")
get_graph(div="W", con="A")
get_graph(div="N", con="N")
get_graph(div="S", con="N")
get_graph(div="E", con="N")
get_graph(div="W", con="N")
#Team Playoff berths
nfl %>% dplyr::filter(playoff =="Y") %>% dplyr::group_by(team) %>% dplyr::summarize(playoff, count = n()) %>% select(team, count) %>% arrange(desc(count)) %>% unique()