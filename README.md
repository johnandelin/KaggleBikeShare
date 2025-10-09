# Kaggle Bike Share Analysis

This project originates from the Kaggle competition: Bike Sharing Demand. https://www.kaggle.com/competitions/bike-sharing-demand/overview.

I used this dataset to demonstrate various machine learning and predictive modeling techniques, with a focus on feature engineering and model evaluation.

All modeling and feature engineering were performed in R using the Tidyverse and Tidymodels frameworks.
The attached R script, Bikeshare Analysis.R, contains the full Tidymodels recipe and workflow.

# Models Used

I experimented with multiple predictive models, including:

- Linear Regression

- Penalized Regression (Ridge/LASSO)

- Regression Trees

- Random Forests

The model with the lowest prediction error was the Random Forest, which achieved the best performance among the tested approaches.

# Kaggle Bike Share EDA

This exploratory data analysis (EDA) examines how weather, season, and temperature (°C) affect the number of bike rentals.
Visualizations were created using ggplot2 and patchwork, as shown in the attached R script Bikeshare EDA.R.

# Data

All data for this project were obtained directly from the Kaggle competition dataset.

# Purpose

This project was completed for my Predictive Analytics (STAT 348) course at Brigham Young University.
The goal was to achieve a Kaggle score below 0.4, which I accomplished with a score of 0.396 using the Random Forest model.

# Considerations & Future Improvements

While the project met its target, several enhancements could further improve performance:
More extensive feature engineering to capture hidden relationships in the data

Experimentation with ensemble methods beyond Random Forests (e.g., Gradient Boosting or XGBoost)

Hyperparameter optimization and cross-validation for model tuning

I did not pursue additional modifications since the target score was reached for the course assignment.
