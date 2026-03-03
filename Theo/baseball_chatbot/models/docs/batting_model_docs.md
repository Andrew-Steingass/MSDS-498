# Ridge Regression Model Documentation

## Overview

This Ridge Regression model is a regularized linear engine designed to predict future hitter performance (Next Year OPS) with high stability. Unlike standard regression, Ridge penalizes redundant features, effectively mitigating the "noise" caused by multicollinearity among baseball statistics. This creates a reliable, repeatable signal for long-term contract evaluations where linear trends are the primary driver of value.

## Current Implementation
The model's outputs for all years are queried per player, enabling trend analysis of accuracy of past predictions and the player's overall OPS Trend for tasks such as trading.

## Model Training Requirements

To establish statistical relationships, the training pipeline requires the following features alongside the target variable:

* **AGE (INT):** Tracks the standard peak-and-decline aging curve of MLB hitters.

* **AGE_SQUARED (FLOAT):** Models the accelerating "Performance Cliff" that occurs in later career stages (Age 40+)

* **SO_RATE (FLOAT):** Strikeout rate; acts as a proxy for physical decline and decreasing pitch recognition

* **BB_RATE (FLOAT):** Walk rate; identifies plate discipline and offensive efficiency

* **RELATIVE_OPS (FLOAT):** Historically weighted "Alpha," comparing a player to league averages to isolate true value

* **SALARY (FLOAT):** Total annual compensation; allows the model to anchor predictions against market investment

* **OPS_TREND (FLOAT):** Performance momentum used to determine if an asset is rising or falling

* **Target Variable:** `[NEXT_YEAR_OPS (FLOAT)]` The predicted On-base Plus Slugging (OPS) for the following season

## Output and Interpretation

The model generates a predictive forecast and a risk assessment for each hitter:

* **PREDICTED_NEXT_YEAR_OPS (FLOAT)** The forecasted offensive production level for the upcoming year  
* **FEATURE_IMPORTANCE (FLOAT)** Quantifies exactly how much each variable (e.g., Age vs. Plate Discipline) weights the final prediction, providing full transparency for front-office audits

## Model Evaluation Alternatives

The Ridge Regression model was selected as our "Best Model" due to its superior balance of stability and predictive power over 7,000 player seasons. It outperformed simple Decision Trees and matched the reliability of complex ensemble methods while remaining more interpretable for financial auditing:

* **MAE** Mean Absolute Error - 0.0795

* **RMSE** Root Mean Squared Error - 0.1019

* **MAPE** Mean Absolute Percentage Error - 9.22%

* **R-SQUARED** R2 - 0.3339