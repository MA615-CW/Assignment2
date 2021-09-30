#Wrangle the following data sets into "tidy" tibbles
#Source: https://www.gapminder.org/data/

library(plyr)
library(tidyverse)
library(tidyr)

library(reshape)

#First csv file -> children_out_of_school_primary_female.csv
#Description: Children out of school are the number of primary-school-age 
#             children not enrolled in primary or secondary school

#Read in csv file
COSPF_raw <- as_tibble(read.csv("children_out_of_school_primary_female.csv"))

COSPF_1 <- COSPF_raw %>% 
  tidyr::pivot_longer(
    cols = starts_with("X"), 
    names_to = "year", 
    values_to = "count", 
    names_prefix = "X",na.rm = TRUE)

COSPF_1$count <- str_replace(COSPF_1$count, "k", "00")
COSPF_1$count <- str_replace(COSPF_1$count, "M", "0000")
COSPF_1$count <- as.numeric(str_replace_all(COSPF_1$count, "[^0-9]+", ""))

COSPF_2 <- subset.data.frame(COSPF_1, count != "NA") #Remove all CountryxYear with no counts

counts <- count(COSPF_2, year)

counts$max <- max(counts$n)

maxobs_year <- subset.data.frame(counts, max == n)

COSPF_3 <- left_join(COSPF_2,maxobs_year, by = "year")



#Second csv file -> children_per_woman_total_fertility.csv
#Description: total fertility rate. The number of children that would be born 
#             to each woman with prevailing age-specific fertility rates