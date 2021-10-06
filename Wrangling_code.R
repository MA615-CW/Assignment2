#Wrangle the following data sets into "tidy" tibbles
#Source: https://www.gapminder.org/data/

#Pull in needed libraries
  library(plyr)
  library(tidyverse)
  library(tidyr)
  library(ggplot2)
  library(gapminder)
  library(countrycode)

  
#-------------------------------------------------------------------------------
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
    values_to = "Children_Out_Of_School", 
    names_prefix = "X")

#change k and M formatting to show the values in thousands and millions
COSPF_1$Children_Out_Of_School <- str_replace(COSPF_1$Children_Out_Of_School, "k", "00")
COSPF_1$Children_Out_Of_School <- str_replace(COSPF_1$Children_Out_Of_School, "M", "0000")

#remove any decimals left over from the k and M notation
COSPF_1$Children_Out_Of_School <- as.numeric(str_replace_all(COSPF_1$Children_Out_Of_School, "[^0-9]+", ""))


#Remove all CountryxYear combinations with missing counts
COSPF_2 <- filter(COSPF_1, Children_Out_Of_School != "NA")



#subset down gapminder data to just country and continent
continent <- gapminder %>% select(country, continent)

#create 1 obs per country x continent -- crosswalk of continent and country
continent <- unique(continent)

#Replace country with country code on the gapminder crosswalk
continent$country_code <- countrycode(continent$country, 'country.name', 'iso3c')
continent <- continent %>% select(country_code, continent)

#Add country code to subset data
COSPF_2$country_code <- countrycode(COSPF_2$country, 'country.name', 'iso3c')

#merge continent information onto the data
COSPF_4 <- full_join(COSPF_2, continent, by = "country_code")
COSPF_4 <- filter(COSPF_4, continent != "NA" & country != "NA")

#investigate cases that are on the COSPF data, but not on the gapminder crosswalk
missing_continent <- anti_join(COSPF_2, continent, by = "country_code")

#for now we will code these country's continents as missing, 
#but should in the future potentially look to do some case specific cleaning
missing_continent$continent <- "missing"

#set the remaining 711 cases onto the COSPF data with continent set to missing
COSPF_FINAL <- rbind(COSPF_4, missing_continent)

#-------------------------------------------------------------------------------

#Second csv file -> children_per_woman_total_fertility.csv
#Description: total fertility rate. The number of children that would be born 
#             to each woman with prevailing age-specific fertility rates

#Read in csv file as a tibble
CPWTF_raw <- as_tibble(read.csv("children_per_woman_total_fertility.csv"))

#Explore the data
str(CPWTF_raw)
dim(CPWTF_raw)

#Need to transform the data by stack the year columns by country and remove the "X" prefix
CPWTF_1 <- CPWTF_raw %>% 
  tidyr::pivot_longer(
    cols = starts_with("X"), 
    names_to = "year", 
    values_to = "Fertility_Rate", 
    names_prefix = "X")


CPWTF_1$country_code <- countrycode(CPWTF_1$country, 'country.name', 'iso3c')

#merge continent information onto the data
CPWTF_2 <- full_join(CPWTF_1, continent, by = "country_code")
CPWTF_3 <- filter(CPWTF_2, continent != "NA" & country != "NA")

#investigate cases that are on the CPWTF data, but not on the gapminder crosswalk
missing_continent2 <- anti_join(CPWTF_1, continent, by = "country_code")

#for now we will code these country's continents as missing, 
#but should in the future potentially look to do some case specific cleaning
missing_continent2$continent <- "missing"

#set the remaining 18060 cases onto the CPWTF_3 data with continent set to missing
CPWTF_FINAL <- rbind(CPWTF_3, missing_continent2)

#-------------------------------------------------------------------------------


#Merge datasets together - This data set will include all observations in both the COSPF_FINAL and thr CPWTF_FINAL tibbles
Full_GapMinder_Data <- as_tibble(merge(COSPF_FINAL, CPWTF_FINAL, all.x = TRUE, all.y = TRUE))

#Output data to project folder
write.csv(Full_GapMinder_Data, "Tidy_Gapminder_Data.csv")

