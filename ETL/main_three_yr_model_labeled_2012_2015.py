#######################################################################################################################################################################
#######################################################################################################################################################################
#######################################################################################################################################################################


#=======================##=======================#  Scripts
#from utils_module.DB_ETL_Utils  import  process_file, setup_logging, get_db_connection, ensure_file_tracking_table, create_database_table, get_processed_files, get_files_to_process
#from code_modules.ETL.Imaging import perform_etl_epic_financials

from functions.utils import error_reporting, perform_etl_cleaning, setup_logging
from functions.etl_functions import process_file, get_db_connection, ensure_file_tracking_table, create_database_table, get_processed_files, get_files_to_process
#=======================##=======================# Script Specific Imports
#import pyodbc

#=======================##=======================# Core Imports
from dotenv import load_dotenv
load_dotenv()
import requests
import json
import pandas as pd
import numpy as np
import time
import os
import datetime
today = datetime.datetime.now().strftime('%Y-%m-%d')
username = os.getlogin()
script_start = time.time()
#=======================##=======================#

rename_dict = {
    "playerID": "player_id",
    "yearID": "season_year",
    "prior_innings_pitched": "prior_innings_pitched",
    "prior_games_started": "prior_games_started",
    "prior_batters_faced": "prior_batters_faced",
    "prior_plate_appearances": "prior_plate_appearances",
    "prior_stolen_base_attempts": "prior_stolen_base_attempts",
    "prior_hit_by_pitch": "prior_hit_by_pitch",
    "prior_defensive_innings": "prior_defensive_innings",
    "InjuryFlag_current_year": "injury_flag_current_year",
    "DL_days_current_year": "dl_days_current_year",
    "InjuryEvents_current_year": "injury_events_current_year",
    "teamID": "team_id",
    "TurfFlag": "turf_flag",
    "height": "height",
    "weight": "weight",
    "POS": "position",
    "debut": "debut",
    "finalGame": "final_game",
    "debut_year": "debut_year",
    "PositionGroup": "position_group",
    "career_age": "career_age",
    "log_prior_IP": "log_prior_ip",
    "log_prior_PA": "log_prior_pa",
    "log_prior_DEF": "log_prior_def",
    "log_prior_BFP": "log_prior_bfp",
    "log_prior_GS": "log_prior_gs",
    "log_prior_HBP": "log_prior_hbp",
    "log_prior_SBA": "log_prior_sba",
    "prior_injury_count": "prior_injury_count",
    "prior_DL_days_total": "prior_dl_days_total",
    "injured_last_year": "injured_last_year",
    "tree_prob_group": "tree_prob_group",
    "RiskCategory_group": "risk_category_group",
    "split": "data_split",
    "pred_p": "predicted_probability",
    "p": "actual_probability",
    "Injury_risk": "injury_risk_label"
}

schema_dict = {
    "player_id": "VARCHAR(255)",
    "season_year": "INT",
    "prior_innings_pitched": "FLOAT",
    "prior_games_started": "INT",
    "prior_batters_faced": "INT",
    "prior_plate_appearances": "INT",
    "prior_stolen_base_attempts": "INT",
    "prior_hit_by_pitch": "INT",
    "prior_defensive_innings": "INT",
    "injury_flag_current_year": "INT",
    "dl_days_current_year": "INT",
    "injury_events_current_year": "INT",
    "team_id": "VARCHAR(255)",
    "turf_flag": "VARCHAR(255)",
    "height": "INT",
    "weight": "INT",
    "position": "VARCHAR(255)",
    "debut": "VARCHAR(255)",
    "final_game": "VARCHAR(255)",
    "debut_year": "INT",
    "position_group": "VARCHAR(255)",
    "career_age": "INT",
    "log_prior_ip": "FLOAT",
    "log_prior_pa": "FLOAT",
    "log_prior_def": "FLOAT",
    "log_prior_bfp": "FLOAT",
    "log_prior_gs": "FLOAT",
    "log_prior_hbp": "FLOAT",
    "log_prior_sba": "FLOAT",
    "prior_injury_count": "INT",
    "prior_dl_days_total": "INT",
    "injured_last_year": "INT",
    "tree_prob_group": "FLOAT",
    "risk_category_group": "VARCHAR(255)",
    "data_split": "VARCHAR(255)",
    "predicted_probability": "FLOAT",
    "actual_probability": "FLOAT",
    "injury_risk_label": "VARCHAR(255)"
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
script_logging_name = 'Awardthree_yr_model_labeled_2012_2015sManager'  ##  Use the name of the main pipeline script. 
FILE_PATTERN =  "three_yr_model_labeled_2012_2015.csv"                                      ##  Pattern of file to look for within the datafolder
TABLE_NAME = 'three_yr_model_labeled_2012_2015'                          ##  What table do you want to push into


DATA_FOLDER = fr'C:\Users\Andy\Documents\msds 498 project'     ##  Folder location where the data lives

LOG_FOLDER = fr'C:\Users\Andy\Documents\msds 498 project\logs' ##  Location to save text logs to
SERVER_NAME = os.getenv('db_server')                                ##  Name of the SERVER_NAME 
DATABASE_NAME = 'master_baseball_capstone'                       ##  Database name the table lives in

FILE_TRACKING_TABLE = 'file_tracking_table'      ##  Name variable. This will create a new table if not exists, and record the file name, dates, and rows pushed into DB.
BATCH_SIZE = 1000                            ##  When pushing into the DB, how many rows should be pushed into DB at once
ETL_FUNCTION = perform_etl_cleaning  ##  Create this function for every ETL Pipeline in "code_modules\ETL" folder scripts. May need to update import

SORT_COLUMNS = {'columns': ['', '', ''], 
               'ascending_bool': False}                 ##  TRUE = I want ascending, False = I do NOT want ascending sort
DELIMITER = fr','                                       ##  For no delimiter set to None    otherwise common items would be [  ','  ,  '|'  ]
                                                        #      


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##      Setup pipeline
logger = setup_logging(DATABASE_NAME, LOG_FOLDER, TABLE_NAME)
conn, cur = get_db_connection(SERVER_NAME, DATABASE_NAME) #3
ensure_file_tracking_table(conn, cur, logger, FILE_TRACKING_TABLE, script_logging_name) #3

processed_files = get_processed_files(cur, logger, FILE_TRACKING_TABLE, script_logging_name) #3 ## Previously processed files
all_files = get_files_to_process(logger, DATA_FOLDER, FILE_PATTERN, script_logging_name) #3  ## new files yet to be processed


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
files_to_process = []
for filepath in all_files:
    filename = os.path.basename(filepath)
    if filename not in processed_files:
        files_to_process.append(filepath)
logger.info(f"Found {len(files_to_process)} new files to process.")

for filepath in files_to_process:
    
    records_count = process_file(filepath, conn, cur, logger, FILE_TRACKING_TABLE, 
                                 TABLE_NAME, BATCH_SIZE, DELIMITER, SORT_COLUMNS,  ETL_FUNCTION, DATABASE_NAME, script_logging_name,
                                 schema_dict, rename_dict)

    logger.info(f"Completed processing {os.path.basename(filepath)}: {records_count} records added.")
logger.info("ETL process completed successfully.")

cur.close()
conn.close()
logger.info("Database connection closed.")
