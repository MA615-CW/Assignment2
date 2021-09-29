#Wrangle the following data sets into "tidy" tibbles
#Source: https://www.gapminder.org/data/

library(plyr)
library(tidyverse)
library(reshape)

#First csv file -> children_out_of_school_primary_female.csv
#Description: Children out of school are the number of primary-school-age 
#             children not enrolled in primary or secondary school

#Read in csv file
COSPF_raw <- read.csv("children_out_of_school_primary_female.csv", header = TRUE)

COSPF_1 <- melt(COSPF_raw,id = "country")
  
names(COSPF_1)[2] <- "year"
names(COSPF_1)[3] <- "Female_Not_Enrolled_Count_raw"



COSPF_1$year <- as.factor(str_replace_all(COSPF_1$year, "[^0-9]+", ""))
COSPF_1$Female_Not_Enrolled_Count_1 <- str_replace(COSPF_1$Female_Not_Enrolled_Count_raw, "k", "00")
COSPF_1$Female_Not_Enrolled_Count_2 <- str_replace(COSPF_1$Female_Not_Enrolled_Count_1, "M", "0000")
COSPF_1$Female_Not_Enrolled_Count <- as.integer(str_replace_all(COSPF_1$Female_Not_Enrolled_Count_2, "[^0-9]+", ""))
COSPF_1$Female_Not_Enrolled_Count_raw <- NULL
COSPF_1$Female_Not_Enrolled_Count_1 <- NULL
COSPF_1$Female_Not_Enrolled_Count_2 <- NULL

COSPF_2 <- subset.data.frame(COSPF_1, Female_Not_Enrolled_Count != "NA") #Remove all CountryxYear with no counts


counts <- count(COSPF_2, year)

counts$max <- max(counts$n)

maxobs_year <- subset.data.frame(counts, max == n)

COSPF_3 <- full_join(COSPF_3,maxobs_year, by = "year")



#Second csv file -> children_per_woman_total_fertility.csv
#Description: total fertility rate. The number of children that would be born 
#             to each woman with prevailing age-specific fertility rates