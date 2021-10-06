#Visualization function
library(plyr)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(reshape)
library(grid)
library(gridExtra)
library(viridis)

Tidy_data <- as_tibble(read.csv("Tidy_Gapminder_Data.csv"))


Visualization_years <- function(dat,yr1, yr2, hist, spaghet) {
  
  
  
  #Only keep data with full obs
  dat$complete <- complete.cases(dat)
  dat_complete <- filter(dat, complete == "TRUE")
  
  #subset data down to time frame of interest
  dat_complete <- filter(dat_complete, year>=yr1 & year <=yr2)
  
  
  #Specified theme for graphs
  mynamestheme <- theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15)), 
                        legend.title = element_text(colour = "#293352",  face = "bold.italic", family = "Helvetica"), 
                        legend.text = element_text(face = "italic", colour="#293352",family = "Helvetica"), 
                        axis.title = element_text(family = "Helvetica", size = (10), colour = "#293352"),
                        axis.text = element_text(family = "Courier", colour = "#000080", size = (10)))
  
  
  
  #Histograms
  if(hist = 1){
    #Prepare data
      data_hist1 <- dat_complete %>%
        group_by(continent) %>%
        summarize(Fertility_Rate = round(mean(Fertility_Rate), 2))
      data_hist2 <- dat_complete %>%
        group_by(continent) %>%
        summarize(Children_Out_Of_School = round(mean(Children_Out_Of_School), 2))
      
      data_hist1 <- filter(data_hist1, continent != "missing")
      data_hist2 <- filter(data_hist2, continent != "missing")
    
    #Plot the data
      plot1 <- ggplot(data_hist1, aes(x = continent, y =Fertility_Rate,  fill = continent ) ) +
        geom_bar(stat = "identity") +
        coord_flip() +
        theme(legend.position = "none") +
        labs(title = "Average Fertility Rate by Continent",x="Continent", y = "Average Fertility Rate", colour = "Continent") +
        mynamestheme +
        scale_color_viridis(discrete = TRUE, option = "D")+
        scale_fill_viridis(discrete = TRUE)
      
      plot2 <-ggplot(data_hist2, aes(x = continent, y =Children_Out_Of_School,  fill = continent )) +
        geom_bar(stat = "identity") +
        coord_flip()+
        labs(title = "Average Number of Female Children Out of School by Continent",x="Continent", y = "Average Number of Female Children Out of School", fill = "Continent") +
        mynamestheme +
        scale_color_viridis(discrete = TRUE, option = "D")+
        scale_fill_viridis(discrete = TRUE)
  }
  
  #Spaghetti plot
  
  #Prepare data
  
  if(spaghet = 1){
    data_hist3 <- dat_complete %>%
      group_by(continent, year) %>%
      summarize(Children_Out_Of_School = round(mean(Children_Out_Of_School), 2))
    
    data_hist4 <- dat_complete %>%
      group_by(continent, year) %>%
      summarize(Fertility_Rate = round(mean(Fertility_Rate), 2))
    
    data_hist3 <- filter(data_hist3, continent != "missing")
    data_hist4 <- filter(data_hist4, continent != "missing")
  
  #Plot the data
    plot3 <- ggplot(data = data_hist3, aes(x = year, y = Children_Out_Of_School, group = continent, colour = continent)) +
      mynamestheme +
      geom_line(size=.75) +
      labs(title = "Average Number of Female Children Out of School by Continent Over Time",x="Year", y = "Average Number of Female Children Out of School", fill = "Continent") +
      scale_color_viridis(discrete = TRUE, option = "D")+
      scale_fill_viridis(discrete = TRUE)
    
    plot4 <- ggplot(data = data_hist4, aes(x = year, y = Fertility_Rate, group = continent, colour = continent)) +
      mynamestheme +
      geom_line(size=.75) +
      theme(legend.position = "none") +
      labs(title = "Average Fertility Rate by Continent Over Time",x="Year", y = "Average Fertility Rate by Continent", fill = "Continent") +
      scale_color_viridis(discrete = TRUE, option = "D")+
      scale_fill_viridis(discrete = TRUE)
  }
  
  
  #Show the plots
  
  if(hist = 1 & spaghet=1){
    grid.arrange(plot1, plot2, plot4, plot3, nrow=2)
  }
  
  if(hist = 1 & spaghet=0){
    grid.arrange(plot1, plot2, nrow=2)
  } 
  if(hist = 0 & spaghet=1){
    grid.arrange(plot4, plot3, nrow=2)
  } 

}


Visualization_years(1995, 2007,1,1)