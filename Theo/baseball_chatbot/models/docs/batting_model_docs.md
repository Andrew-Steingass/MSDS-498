# Hitters Performance Model Documentation

## Overview
This system uses a Ridge Regression model to evaluate and predict a baseball hitter's `Next_Year_OPS`. It compares expected On-Base Plus Slugging against actual outcomes to establish predictive baselines.

## Model Training Requirements
To establish statistical relationships, the training pipeline requires the following features alongside the target variable:
* **Age (Integer):** Biological age during the current season.
* **Age_Squared (Integer):** Captures the non-linear aging curve where athletic performance peaks and then declines.
* **SO_Rate (Float):** Strikeout rate from the current season.
* **BB_Rate (Float):** Walk rate from the current season.
* **Relative_OPS (Float):** Current season OPS relative to the league average.
* **salary (Float):** Player compensation.
* **Target Variable:** `Next_Year_OPS` (the actual recorded metric for the following season).

## Model Prediction Requirements (Inference)
To forecast upcoming performance, the inference pipeline requires the specific model features and identifiers:
* **Player Identification:** First and last name.
* **Target Season:** The specific year being projected.
* **Input Features:** `Age`, `Age_Squared`, `SO_Rate`, `BB_Rate`, `Relative_OPS`, and `salary`.

## Output and Interpretation
The system outputs a tabular format with three primary metrics per player:
* **Predicted (Float):** The forecasted `Next_Year_OPS`.
* **Actual (Float):** The observed real world metric.
* **Residual (Float):** The mathematical difference (Predicted minus Actual). A positive residual indicates an over projection.

## Model Evaluation Alternatives
When assessing overall system accuracy, relying solely on individual residuals is insufficient. Evidence-backed evaluation requires aggregated metrics:
* **Root Mean Squared Error (RMSE):** Calculates the standard deviation of the residuals to measure global variance.
* **Mean Absolute Error (MAE):** Measures the average magnitude of prediction errors.