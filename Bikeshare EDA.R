library(tidyverse)
library(patchwork)
library(vroom)

BikeShareTrain <- vroom("train.csv")

str(BikeShareTrain)

BikeShareTrain <- BikeShareTrain|>
  mutate(season = as.factor(season))|>
  mutate(weather = as.factor(weather))|>
  mutate(holiday = as.factor(holiday))|>
  mutate(workingday = as.factor(workingday))

weather <- BikeShareTrain|>
  ggplot(aes(x = weather, y = count, fill = weather))+
  geom_boxplot()+
  labs(
    x = "Weather",
    y = "Number of Rentals",
    title = "Distribution of Rentals by Weather",
    subtitle = "1: Favorable, 2: Little or no parcipitation, 3: Light parcipitation, 4: Heavy Parcipitaiton"
  )+
  theme(legend.position = "none")

season <- BikeShareTrain|>
  ggplot(aes(x = season , y = count, fill = season))+
  geom_boxplot()+
  labs(
    x = "Season",
    y = "Number of Rentals",
    title = "Distribution of Rentals by Season",
    subtitle = "1: Spring, 2: Summer, 3: Fall, 4: Winter"
  )+
  theme(legend.position = "none")

Work <- BikeShareTrain|>
  ggplot(aes(y = workingday, fill = workingday))+
  geom_bar()+
  labs(
    x = "Total number of Rentals",
    y = NULL,
    title = "Rentals on Work days and non Work days",
    subtitle = "0: Weekend or Holiday, 1: Working day"
  )+
  theme(legend.position = "none")

Temp <- BikeShareTrain|>
  ggplot(aes(x = temp, y = count))+
  geom_point()+
  labs(
    x = "Temperature in Celsius",
    y = "Number of Rentals",
    title = "Relationship between Temp and Rentals"
  )+
  geom_smooth(method = "lm", color = "red")

(season | weather)/(Temp | Work) +
  plot_annotation(caption = "Data source: Kaggle.com")






