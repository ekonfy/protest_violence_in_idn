# prepare environment -----------------------------------------------------

# import libs
library(plotly)
library(scales)
library(shiny)
library(shinydashboard)
library(tidyverse)
library(leaflet)
library(dplyr)
library(lubridate) 
# tambahan --------------------------------
library(shiny)
library(ggplot2)
library(tidyr)
library(googleVis)
library(DT)
library(rgdal)
library(geojsonio)
library(shinythemes)
library(RColorBrewer)
library(shinyWidgets)
library(tm)
library(wordcloud)
library(memoise)
library(wordcloud2)
library(rsconnect)
library(htmltools)
library(dygraphs)
library(plotly)


# import dataset
data_ind <- read_csv("data/Indonesia.csv")


str(data_ind)
data_ind$event_type <- as.factor(data_ind$event_type)

selectregion <- unique(data_ind$admin2)
selectprovince <- unique(data_ind$admin1)

df <- data.frame(data_ind)

data_ind$event_date <- dmy(data_ind$event_date)

# data for table 

df <- data_ind %>% 
  mutate(admin1 = as.factor(admin1)) %>% 
  select(-notes)

df <- df %>% 
  mutate(admin2 = as.factor(admin2))

selectprovince <- unique(df$admin1) 
selectcity <- unique(df$admin2)
selectyear <- unique(df$year)

selectmap <- df %>%
  select(admin1,admin2)

selectmap <- selectmap %>% 
  mutate(admin2 = as.character(admin2))

df$month <- month(x = df$event_date)


