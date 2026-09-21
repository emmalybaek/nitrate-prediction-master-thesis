# Predicting Nitrate Concentrations in Danish Streams - Master Thesis Project in Business Intelligence at Aarhus University. 

## Project Overview 

The project investigates wheather machine learning models can predict nitrate concentrations in danish streams and generalize to geographically unseen areas. 

## Problem 

Environmental data are spatially dependent. A random train/test split can therefore produce overly optimistic estimates of model performance because nearby observations may appear in both training and test data. This project evaluates the models using spatially separated validation. 

## Data

The project integrates environmental, hydrological, meteorological, agricultural and spatial data from multiply danish data sources. The original nitrate dataset contained 63,471 measurements from danish streams between 2016-2023. 

## Methods

Models: XGBoost, Random Forest and Support Vector Regression

Evaluation: Nested spatial cross validation, RMSE, MAE, R^2, Moran's I and Area of Applicability 

## Results

XGBoost achieved the strongest predictive performance. 

- RMSE: 2.22
- MAE: 1.59
- R^2: 0.29

The analysis showed that predicting nitrate concentrations in geographically unseen areas is challegnging. Residual spatial autocorrelation suggested that important spatial processes were still not fully captured by the models. 

## Key takeaway

The Area of Applicability analysis showed that approximately 94.4% of observations were located within the model’s prediction space, while the remaining observations involved greater extrapolation uncertainty. The findings suggest that machine learning models can support prediction and spatial mapping of nitrate concentration in Danish streams. However, the results also reveal substantial challenges related to geographical generalization. Residual analysis and Moran’s I indicated persistent spatial autocorrelation in model residuals, suggesting that important spatial processes and explanatory variables remain unaccounted for. In addition, the models tended to underestimate high nitrate concentration, particularly for environmentally critical observations. Overall, the thesis demonstrates that machine learning models hold considerable potential as supplementary tools for environmental monitoring and prediction of nitrate concentrations in Danish streams. Nevertheless, the findings also highlight the importance of realistic spatial model evaluation, careful handling of spatial dependence, and explicit assessment of model applicability when applying machine learning to environmental data. The project demonstrates why realistic validation is important when machine learning models are applied to spatial data. High model performance from random train/test splits does not necessarily translate into reliable predictions in new geographical areas.  
