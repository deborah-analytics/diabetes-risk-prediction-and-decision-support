# Modelling Pipeline

This folder contains the machine learning workflow used in the predictive modelling component of the project.

## Main Script

* `thesis_model_pipeline.Rmd`
  End-to-end modelling workflow for multiclass diabetes status prediction.

## Key Modelling Tasks

* data loading and quality checks
* missing value handling and feature preparation
* train-test split and stratified sampling
* class imbalance handling through up-sampling
* feature selection and preprocessing
* model training and comparison
* hyperparameter tuning
* final model evaluation and model saving

## Models Implemented

* Logistic Regression
* Random Forest
* XGBoost
* TabNet
* LSTM
* Hybrid Random Forest + XGBoost

## Evaluation Metrics

The models were compared using:

* Accuracy
* Balanced Accuracy
* Macro-F1 Score

## Outcome

The modelling workflow identified high-performing multiclass classification models for predicting diabetes status (No Diabetes, Prediabetes, Diabetes). The final tuned Random Forest model was used in the Shiny prediction application contained in the dashboard folder.
