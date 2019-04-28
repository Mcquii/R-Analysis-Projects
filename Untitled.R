library(tidyverse)
library(lubridate)
library(stringr)

mtgstat <- read_csv("Downloads/MTG Stat Data - Data.csv", col_types = cols(Format = col_factor(levels = c("Bo1","Bo3", "Bo1 Sealed")),
Set = col_factor(levels = c("RNA","RIX", "C19", "GRN"))))

mtgstat$Date <- mtgstat$Date %>% parse_date_time(orders = "mdy")

mtgstat %>% glimpse

mtgstat %>% ggplot(mapping = aes(x = `Record (wins)`)) +
    geom_histogram(bins = 5)

wincolor <- mtgstat %>% 
  filter(`Record (ratio)` > 1) %>% 
  select(Colors)

wc <- data.frame(lapply(wincolor, function(x) {gsub("[, ]", "", x)}))

colorfreq <- table(unlist(strsplit(paste(wc$Colors, collapse=""), NULL)))

colorfreq
