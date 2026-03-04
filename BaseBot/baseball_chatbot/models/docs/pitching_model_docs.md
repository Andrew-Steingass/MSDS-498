# **Pitching Performance Prediction Model Documentation**

## **Overview**

This Pitching Performance Prediction model is a machine learning classification engine designed to forecast whether an MLB pitcher’s performance will improve in the following season. The model analyzes multi-year performance patterns to identify trajectories of improvement or decline, supporting high-stakes decisions such as contract negotiations, player acquisition, and development planning.

Unlike traditional scouting alone, the system converts complex statistical histories into a probabilistic risk signal, allowing teams to quantify uncertainty and reduce financial exposure when investing in pitchers.

The final production model uses **XGBoost**, an advanced gradient boosting algorithm that learns from prior prediction errors to capture nonlinear relationships across performance metrics.

## Current Implementation
The model is **fully** integrated into the chatbot. This allows for the user to use explore current stats of the pitcher going into 2016 (without having to type them); as well as prefrom "What-if" scenarios where certain stats like K9 can be changed.  

## **Model Training Requirements**

To establish predictive relationships, the training pipeline uses the following features alongside the target variable:

**ERA_LAST (FLOAT):** Most recent earned run average; primary indicator of current performance level.  
**ERA_MEAN3 (FLOAT):** Three-year average ERA capturing long-term consistency.  
**ERA_TREND (FLOAT):** Direction of performance change over time.

**K9_LAST (FLOAT):** Recent strikeouts per nine innings; reflects pitcher dominance.  
**K9_MEAN3 (FLOAT):** Sustained strikeout ability across multiple seasons.  
**K9_TREND (FLOAT):** Momentum in strikeout performance.

**BB9_LAST (FLOAT):** Recent control performance (walk rate).  
**BB9_MEAN3 (FLOAT):** Long-term control stability.  
**BB9_TREND (FLOAT):** Changes in command over time.

**HR9_LAST (FLOAT):** Recent susceptibility to home runs.  
**HR9_MEAN3 (FLOAT):** Sustained home run risk.  
**HR9_TREND (FLOAT):** Trend in power allowed.

**WHIP_LAST (FLOAT):** Recent baserunner prevention ability.  
**WHIP_MEAN3 (FLOAT):** Long-term efficiency at limiting traffic.  
**WHIP_TREND (FLOAT):** Changes in baserunner control.

**IP_LAST (FLOAT):** Recent workload.  
**IP_MEAN3 (FLOAT):** Sustained durability across seasons.  
**IP_TREND (FLOAT):** Workload trajectory.

**AGE (INT):** Player age to capture career stage effects.

### **Target Variable**

**PERFORMANCE_IMPROVEMENT (BINARY)**  
0 = Improved (ERA decreased ≥ 0.30)  
1 = Not Improved (No improvement or decline)

This converts a complex forecasting problem into a binary classification task aligned with roster decision needs.

## **Output and Interpretation**

The model generates the following outputs for each pitcher:

**PREDICTED_CLASS (INT)**  
Binary prediction indicating improvement or non-improvement risk.

**PREDICTED_PROBABILITY_NOT_IMPROVED (FLOAT)**  
Probability estimate representing financial and performance risk level.

**FEATURE_IMPORTANCE (FLOAT)**  
Quantifies how strongly each variable influenced the prediction, providing transparency for front-office evaluation and model auditing.

Feature importance analysis showed that **recent performance metrics (ERA_last and ERA_trend)** were the strongest drivers of prediction outcomes.

## **Model Evaluation and Selection**

Three algorithms were evaluated:

• Logistic Regression  
• Random Forest  
• XGBoost (Gradient Boosting)

The XGBoost model was selected as the final production model because it provided the best combination of predictive accuracy and risk detection performance.

### **Final Test Performance (XGBoost)**

**AUC:** 0.808  
**Accuracy:** 76.9%  
**Precision:** 0.816  
**Recall:** 0.803  
**F1 Score:** ~0.81

These results indicate a strong ability to distinguish between pitchers who will improve versus decline, with particularly high sensitivity for identifying at-risk players — a critical factor for financial decision-making.

## **Model Evaluation Alternatives**

XGBoost outperformed the alternative models due to its ability to learn complex nonlinear interactions between performance trajectory, workload, and skill indicators.

Comparative results:

| Model | AUC | Accuracy |
| ----- | ----- | ----- |
| Logistic Regression | 0.814 | 73.6% |
| Random Forest | 0.805 | 74.4% |
| XGBoost | 0.808 | **76.9%** |

While Logistic Regression provided strong interpretability, and Random Forest captured nonlinear relationships, XGBoost delivered the most reliable overall predictions and the highest risk-detection capability.

