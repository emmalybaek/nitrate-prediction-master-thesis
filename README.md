# Predicting Nitrate Concentrations in Danish Streams - Master Thesis Project in Business Intelligence at Aarhus University. 

## Project Overview 

The project investigates wheather machine learning models can predict nitrate concentrations in danish streams and generalize to geographically unseen areas. 

## Problem 

Environmental data are spatially dependent. A random train/test split can therefore produce overly optimistic estimates of model performance because nearby observations may appear in both training and test data. This project evaluates the models using spatially separated validation. 

## Data

The project integrates environmental, hydrological, meteorological, agricultural and spatial data from multiple danish data sources. The original nitrate dataset contained 63,471 measurements from danish streams between 2016-2023. 

## Methods

Models: XGBoost, Random Forest and Support Vector Regression

Evaluation: Nested spatial cross validation, RMSE, MAE, R^2, Moran's I and Area of Applicability 

## Results

XGBoost achieved the strongest predictive performance. 

- RMSE: 2.22
- MAE: 1.59
- R^2: 0.29

The models showed substantially lower performance on geographically unseen test areas than on training data, highlighting the difficulty of spatial generalization.

![Model performance across spatial folds](figures/model-performance.png)

## Model limitations

The XGBoost model tended to underestimate high nitrate concentrations, particularly at environmentally critical levels.

![Observed vs predicted nitrate concentrations](figures/xgboost-observed-predicted.png)

## Geographical applicability

The Area of Applicability analysis showed that approximately 94.4% of observations were located within the model's prediction space. Areas with higher dissimilarity to the training data generally also showed larger prediction errors.

![Area of Applicability](figures/area-of-applicability.png)

## Key takeaway

- Machine learning can support spatial prediction of nitrate concentrations, but generalization to geographically unseen areas remains challenging.
- XGBoost achieved the strongest performance, although the model tended to underestimate high nitrate concentrations.
- Approximately 94.4% of observations were within the model's Area of Applicability, while predictions outside this space involved greater extrapolation uncertainty.
- Residual spatial autocorrelation indicated that important spatial processes were still not fully captured by the available features.
- The project demonstrates the importance of realistic spatial validation. Strong performance from random train/test splits does not necessarily translate into reliable predictions in new geographical areas.
