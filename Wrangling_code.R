#Wrangle the following data sets into "tidy" tibbles
#Source: https://www.gapminder.org/data/

library(plyr)
library(tidyverse)
library(tidyr)
library(ggplot2)


#First csv file -> children_out_of_school_primary_female.csv
#Description: Children out of school are the number of primary-school-age 
#             children not enrolled in primary or secondary school

#Read in csv file as a tibble
COSPF_raw <- as_tibble(read.csv("children_out_of_school_primary_female.csv"))


#Explore the data
str(COSPF_raw)
dim(COSPF_raw)


#Need to transform the data by stack the year columns by country and remove the "X" prefix
COSPF_1 <- COSPF_raw %>% 
  tidyr::pivot_longer(
    cols = starts_with("X"), 
    names_to = "year", 
    values_to = "count", 
    names_prefix = "X")

#change k and M formatting to show the values in thousands and millions
COSPF_1$count <- str_replace(COSPF_1$count, "k", "00")
COSPF_1$count <- str_replace(COSPF_1$count, "M", "0000")

#remove any decimals left over from the k and M notation
COSPF_1$count <- as.numeric(str_replace_all(COSPF_1$count, "[^0-9]+", ""))


sum(complete.cases(COSPF_1))

#Remove all CountryxYear combinations with missing counts
COSPF_2 <- filter(COSPF_1, count != "NA")


#Identify the year that has the most countries with reported data
counts <- count(COSPF_2, year) #count of countries by year
counts$max <- max(counts$n) 
maxobs_year <- subset.data.frame(counts, max == n)

#subset down the overall dataset to the year with the most country data
COSPF_3 <- right_join(COSPF_2,maxobs_year, by = "year")


#Pull in the gapminder data and country code library
library(gapminder)
library(countrycode)

#subset down gapminder data to just country and continent
continent <- gapminder %>% select(country, continent)

#create 1 obs per country x continent -- crosswalk of continent and country
continent <- unique(continent)

#Replace country with country code on the gapminder crosswalk
continent$country_code <- countrycode(continent$country, 'country.name', 'iso3c')
continent <- continent %>% select(country_code, continent)

#Add country code to subset data
COSPF_3$country_code <- countrycode(COSPF_3$country, 'country.name', 'iso3c')

#merge continent information onto the data
COSPF_4 <- full_join(COSPF_3, continent, by = "country_code")
COSPF_4 <- filter(COSPF_4, continent != "NA" & country != "NA")

#investigate cases that are on the COSPF data, but not on the gapminder crosswalk
missing_continent <- anti_join(COSPF_3, continent, by = "country_code")

#for now we will code these country's continents as missing, 
#but should in the future potentially look to do some case specific cleaning
missing_continent$continent <- "missing"

#set the remaining 22 cases onto the COSPF data with continent set to missing
COSPF_5 <- rbind(COSPF_4, missing_continent)


#Second csv file -> children_per_woman_total_fertility.csv
#Description: total fertility rate. The number of children that would be born 
#             to each woman with prevailing age-specific fertility rates