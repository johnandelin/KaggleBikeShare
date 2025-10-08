library(tidyverse)
library(patchwork)
library(vroom)

BikeShareTrain <- vroom("train.csv")

str(BikeShareTrain)

BikeShareTrain <- BikeShareTrain|>
  mutate(season = as.factor(season))|>
  mutate(weather = as.factor(weather))|>
  mutate(holiday = as.factor(holiday))|>
  mutate(workingday = as.factor(workingday)) # season, weather, holiday, and workingday are all numeric indicators that need to be changed to factors.
#### Plots ####
# weather bar plot
Weather_plot <- BikeShareTrain|>
  ggplot(aes(x = weather, fill = weather))+ 
  geom_bar()+
  labs(
    x = "Weather",
    y = "Number of Rentals",
    title = "Distribution of Rentals by Weather",
    subtitle = "1: Favorable, 2: Little or no parcipitation, 3: Light parcipitation, 4: Heavy Parcipitaiton"
  )+
  theme(legend.position = "none")

# season box plot
Season_plot <- BikeShareTrain|>
  ggplot(aes(x = season , y = count, fill = season))+
  geom_boxplot()+
  labs(
    x = "Season",
    y = "Number of Rentals",
    title = "Distribution of Rentals by Season",
    subtitle = "1: Spring, 2: Summer, 3: Fall, 4: Winter"
  )+
  theme(legend.position = "none")

# Working day bar plot
Work_plot <- BikeShareTrain|>
  ggplot(aes(y = workingday, fill = workingday))+
  geom_bar()+
  labs(
    x = "Total number of Rentals",
    y = NULL,
    title = "Rentals on Work days and non Work days",
    subtitle = "0: Weekend or Holiday, 1: Working day"
  )+
  theme(legend.position = "none")

# Temp scatter plot. 
Temp_plot <- BikeShareTrain|>
  ggplot(aes(x = temp, y = count))+
  geom_point()+
  labs(
    x = "Temperature in Celsius",
    y = "Number of Rentals",
    title = "Relationship between Temp and Rentals"
  )+
  geom_smooth(method = "lm", color = "red")

#### patchwork ####
(Season_plot | Weather_plot)/(Temp_plot | Work_plot) +
  plot_annotation(caption = "Data source: Kaggle.com")






