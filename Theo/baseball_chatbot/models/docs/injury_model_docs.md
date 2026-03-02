# Injury Risk Model Documentation

## Overview
This model is a position-specific decision tree classifier (CART via rpart) trained separately for Pitcher, Catcher, Infield, and Outfield groups to predict next season single-season injury risk. The model outputs the probability that a player experiences an injury in a given season and assigns a Low/Medium/High injury risk classification based on group-specific baseline injury rates.

## Current Implementation
The model's outputs for all years are queries per player, enabling trend analysis for tasks such as trading.

## Model Training Requirements
To establish statistical relationships, the training pipeline requires the following features alongside the target variable:
* **prior_injury_count (INT):** Total number of injuries for the player in the previous season - a measure of injury frequency
* **log1p(prior_DL_days_total) (NUM):** log transformation of the sum of days the player was on the disabled list in the previous season - a measure of injury severity
* **log1p(prior_DEF) (NUM):** log transformation of the number of defensive innings the player played in the previous season -  one measure of workload
* **injured_last_season (NUM):** a 1 for players injured last season, a 0 for players not injured last season -  one measure of injury recency
* **log1p(prior_IP) (NUM):** log transformation of the number of innings pitched - a pitcher specific measure of workload
* **career_age (INT):** the number of years since the player debuted in MLB
* **log1p(prior_PA) (NUM):** log transformation of the number of plate appearances the player had in the previous season -  one measure of workload
* **weight (INT):** the players listed weight in pounds
* **Target Variable:** `InjuryFlag_current_year (BIN)` Whether a player is injured in the current year - value is 0 if not injured, value is 1 if injured

## Cleaning Steps
PlayerIDs were looked up from a master playerID table that was filtered to active players during the time of the injury data set (1999 - 2016). An exact match on name, then a fuzzy match on name were performed. Some name parsing was needed on the remaining names and an exact match repeated. Any injuries associated with players that could not be matched to a playerID were removed as their performance stats could not be correctly assigned. Log1p transformations were performed on performance data to reduce skew and to allow for easier comparison to previous logistic regression model. The decision tree model code eliminated about 50 additional rows from the injury by year by player table where any field had an NA left in it. All players were assigned to player groups for analysis: Pitchers and Catchers remained their own position groups. 1st base, 2nd base, 3rd base and Shortstop were compiled as Infield and Outfield, Right field, Center field, and Left field were compiled into Outfield. 

## Output and Interpretation
Model output a probability of risk in the current season. Post processing of the model output produced a Injury Risk Category of HIGH/MEDIUM/LOW based on a comparison to the probability of injury for an average player in the position group. Low risk players had 75% or lower risk of injury for their position and High risk players had 50% or higher risk of injury for their position:
* **OUTPUT VARIABLES- current_year_injury_flag (VARIABLE TYPE - BIN)** Whether a player is injured in the current year - value is 0 if not injured, value is 1 if injured.
* **OUTPUT VARIABLES- pred_p (VARIABLE TYPE - NUM)** players predicted risk of injury in current season
* **OUTPUT VARIABLES- Injury_risk (VARIABLE TYPE - Factor)** High, Medium or Low injury risk category. Assigned based on comparison of players predicted probability of injury to the average player in the position group.

## Model Evaluation Alternatives
Model was evaluated using ROC-AUC and F1 statistic. The ROC-AUC value indicates how much of the model prediction is explained by the model versus random chance. The F1 statistic is a balanced precision/recall metric that strives to optimize the correct identification of true positives and true negatives while minimizing the number of false positives and false negatives  Evidence-backed evaluation requires aggregated metrics:
* **ROC-AUC** Receiver Operating Characteristic – Area Under Curve - Pitchers = 0.695, Catchers = 0.762, Infield = 0.706, Outfield = 0.719
* **F1** The balance between precision and recall = 2*(PrecisionxRecall/(Precision+Recall)) - Pitchers = 0.522, Catchers = 0.537, Infield = 0.498, Outfield = 0.537
