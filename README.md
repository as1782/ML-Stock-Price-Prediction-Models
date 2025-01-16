# Stock Price Prediction using Machine Learning Models

This repository contains the R code used in my project on predicting Apple Inc.'s daily closing stock price using various machine learning models. The study evaluates the performance of models while addressing challenges like multicollinearity, small dataset size, and overfitting.

## Overview

The project explores the application of machine learning techniques to financial time series data. The following models were implemented and analyzed:

- Linear Models: Multiple Linear Regression (MLR), Subset Selection, Ridge Regression, Lasso Regression

- Dimensionality Reduction: Principal Component Regression (PCR), Partial Least Squares (PLS)

- Tree-Based Models: Regression Trees, Bagging, Random Forests, Boosting

Each model's predictive performance was assessed using Mean Squared Error (MSE), and its suitability was discussed in the context of the dataset's characteristics. Cross-Validation was implemented using 10-fold CV to tune hyperparameters and minimize overfitting.

## Dataset

The dataset includes 90 daily observations of financial and macroeconomic variables, such as:

- AAPL stock data (Open, High, Low, Close, Volume)

- Technical indicators (SMA, Lagged Variables)

- Macroeconomic data (Treasury Yield, Crude Oil Prices, Exchange Rates)

The dataset was sourced from Alpha Vantage and processed for model compatibility.














