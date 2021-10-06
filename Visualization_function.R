#Visualization function

library(ggplot2)
library(reshape)
library(gridExtra)
library(viridis)

Tidy_data <- as_tibble(read.csv("Tidy_Gapminder_Data.csv"))

#Only keep data with full obs
Tidy_data$complete <- complete.cases(Tidy_data)
Tidy_data_complete <- filter(Tidy_data, complete == "TRUE")

#Potentially turn mean into part of the function --ability to look at mean, min, max by continent
#Additionally can look at the children_out_of_school mean

data_hist1 <- Tidy_data_complete %>%
  group_by(continent) %>%
  summarize(Fertility_Rate = round(mean(Fertility_Rate), 2))
data_hist2 <- Tidy_data_complete %>%
  group_by(continent) %>%
  summarize(Children_Out_Of_School = round(mean(Children_Out_Of_School), 2))

data_hist1 <- filter(data_hist1, continent != "missing")
data_hist2 <- filter(data_hist2, continent != "missing")

mynamestheme <- theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15)), 
                      legend.title = element_text(colour = "#293352",  face = "bold.italic", family = "Helvetica"), 
                      legend.text = element_text(face = "italic", colour="#293352",family = "Helvetica"), 
                      axis.title = element_text(family = "Helvetica", size = (10), colour = "#293352"),
                      axis.text = element_text(family = "Courier", colour = "#000080", size = (10)))
# The palette with grey:
cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

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



#Spaghetti plot

#prepare data

data_hist3 <- Tidy_data_complete %>%
  group_by(continent, year) %>%
  summarize(Children_Out_Of_School = round(mean(Children_Out_Of_School), 2))

data_hist4 <- Tidy_data_complete %>%
  group_by(continent, year) %>%
  summarize(Fertility_Rate = round(mean(Fertility_Rate), 2))

data_hist3 <- filter(data_hist3, continent != "missing")
data_hist4 <- filter(data_hist4, continent != "missing")


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

grid.arrange(plot1, plot2, plot4, plot3,  ncol=2)

