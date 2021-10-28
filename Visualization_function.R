#Visualization function

#read in needed packages
library(plyr)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(reshape)
library(grid)
library(gridExtra)
library(viridis)

#begin function
Visualization_years <- function(dat,yr1, yr2, hist, spaghet) {
  
  
  #Only keep data with full obs
  dat$complete <- complete.cases(dat)
  dat_complete <- filter(dat, complete == "TRUE")
  
  #subset data down to time frame of interest
  
  if(yr1 != -1 && yr2 != -1){
    dat_complete <- filter(dat_complete, year>=yr1 & year <=yr2)
  }
  
  #Specified theme for graphs
  mynamestheme <- theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (7)), 
                        legend.title = element_text(colour = "#293352",  face = "bold.italic", family = "Helvetica",size =(2)), 
                        legend.text = element_text(face = "italic", colour="#293352",family = "Helvetica",size =(2)), 
                        axis.title = element_text(family = "Helvetica", size = (5), colour = "#293352"),
                        axis.text = element_text(family = "Courier", colour = "#000080", size = (5)))
  
  
  
#Histogram - these will only be created if the user has specified wanting to see them in the function
  if(hist == 1){
    
    #Prepare data for graphing by grouping the data by continent and taking the mean of the variable of interest
      data_hist1 <- dat_complete %>%
        group_by(continent) %>%
        summarize(Fertility_Rate = round(mean(Fertility_Rate), 2))
      data_hist2 <- dat_complete %>%
        group_by(continent) %>%
        summarize(Children_Out_Of_School = round(mean(Children_Out_Of_School), 2))
      
      #remove data with missing continent information - this is not useful for now
      data_hist1 <- filter(data_hist1, continent != "missing")
      data_hist2 <- filter(data_hist2, continent != "missing")
    
    #Plot the data and assign to objects
      plot1 <- ggplot(data_hist1, aes(x = continent, y =Fertility_Rate,  fill = continent ) ) +
        geom_bar(stat = "identity") +
        coord_flip() +
        theme(legend.position = "none") +
        labs(title = "Average Fertility Rate by Continent",x="Continent", y = "Average Fertility Rate", colour = "Continent") +
        mynamestheme +
        scale_color_viridis(discrete = TRUE, option = "D")+
        scale_fill_viridis(discrete = TRUE) + 
        theme(plot.margin=unit(c(.5,1,.5,1),"cm"))
      
      plot2 <-ggplot(data_hist2, aes(x = continent, y =Children_Out_Of_School,  fill = continent )) +
        geom_bar(stat = "identity") +
        coord_flip()+
        theme(legend.position = "None")+
        labs(title = str_wrap("Average Number of Female Children Out of School by Continent",40),x="Continent", y = "Average Number of Female Children Out of School", fill = "Continent") +
        mynamestheme +
        scale_color_viridis(discrete = TRUE, option = "D")+
        scale_fill_viridis(discrete = TRUE) + 
        theme(plot.margin=unit(c(.5,1,.5,1),"cm"))
  }
  
  #Spaghetti plot - these will only be created if the user has specified wanting to see them in the function
  if(spaghet == 1){
    
    #Prepare data for graphing by grouping the data by continent and taking the mean of the variable of interest
      data_hist3 <- dat_complete %>%
        group_by(continent, year) %>%
        summarize(Children_Out_Of_School = round(mean(Children_Out_Of_School), 2))
      
      data_hist4 <- dat_complete %>%
        group_by(continent, year) %>%
        summarize(Fertility_Rate = round(mean(Fertility_Rate), 2))
      
      #remove data with missing continent information - this is not useful for now
      data_hist3 <- filter(data_hist3, continent != "missing")
      data_hist4 <- filter(data_hist4, continent != "missing")
  
  #Plot the data and assign to objects
    plot3 <- ggplot(data = data_hist3, aes(x = year, y = Children_Out_Of_School, group = continent, colour = continent)) +
      mynamestheme +
      geom_line(size=.75) +
      labs(title = str_wrap("Average Number of Female Children Out of School by Continent Over Time",40),x="Year", y = str_wrap("Average Number of Female Children Out of School",40), fill = "Continent") +
      scale_color_viridis(discrete = TRUE, option = "D")+
      scale_fill_viridis(discrete = TRUE) + 
      theme(legend.position = "none") +
      theme(plot.margin=unit(c(.5,1,.5,1),"cm"))+
      scale_x_discrete(breaks=seq(1900, 2030, 10))
    
    plot4 <- ggplot(data = data_hist4, aes(x = year, y = Fertility_Rate, group = continent, colour = continent)) +
      mynamestheme +
      geom_line(size=.75) +
      theme(legend.position = "none") +
      labs(title = "Average Fertility Rate by Continent Over Time",x="Year", y = "Average Fertility Rate by Continent", fill = "Continent") +
      scale_color_viridis(discrete = TRUE, option = "D")+
      scale_fill_viridis(discrete = TRUE) + 
      theme(plot.margin=unit(c(.5,1,.5,1),"cm")) +
      scale_x_discrete(breaks=seq(1900, 2030, 10))
  }

  #Show the plots
  
    #this will show both the histograms and the spaghetti plots
    if(hist == 1 & spaghet==1){
      grid.arrange(plot1, plot2, plot4, plot3, nrow=2)
    }
    
    #this will show just the histograms
    if(hist == 1 & spaghet==0){
      grid.arrange(plot1, plot2, ncol=2)
    } 
    
    #this will show just the spghetti plots
    if(hist == 0 & spaghet==1){
      grid.arrange(plot4, plot3, nrcol=2)
    } 

}

#Here is editor's visualization function
plot5 <- function(df){
  mynamestheme <- theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15)), 
                        #legend.title = element_text(colour = "#293352",  face = "bold.italic", family = "Helvetica",size =(2)), 
                        #legend.text = element_text(face = "italic", colour="#293352",family = "Helvetica",size =(2)), 
                        axis.title = element_text(family = "Helvetica", size = (10), colour = "#293352"),
                        axis.text = element_text(family = "Courier", colour = "#000080", size = (10)))
  
  cb_palette = c("#f4b6c2", "#6497b1", "#edc951", "#8874a3", "#4a91f2", "#ffffff")
  df <- df[df$year %in% c(2000:2020),]
  df$year = as.numeric(df$year)
  df$Fertility_Rate = as.numeric(df$Fertility_Rate)
  df = df[!(df$continent == "missing"), ]
  plot_new<-
    ggplot(df, aes(x=year, y=Fertility_Rate, group = continent, colour = continent)) + 
    mynamestheme +
    geom_point()+     
    scale_color_manual(values = cb_palette) +
    facet_grid(~cut_number(year, n = 2))+
    ggtitle("Average Fertility Rate by Continents Over Time")
  plot_new
}

#######################
#' Your code is already very perfect, and my R level is not enough to improve your visualization function, and
#' I even learned a lot from your work.
#' The above function was written at my own level, not a very good plot but there ere two tips here:
#' 1.cb_palette = c("#f4b6c2", "#6497b1", "#edc951", "#8874a3", "#4a91f2", "#ffffff") you can pick up your favorite colors
#'   by using this way
#' 2.facet_grid(~cut_number(year, n = 2))
#'   It helps divide the data into n bins each containing the same number of points, and in this case, n=2, it splits the year.
#' These are just a few tips that I hope will help.
########################
