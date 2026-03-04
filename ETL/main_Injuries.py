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
    "Date": "injury_date",
    "Team": "team_name",
    "Notes": "notes",
    "Injury": "injury_flag",
    "DL_length": "dl_length_days",
    "Injury_Type": "injury_type",
    "Name": "player_name",
    "playerID": "player_id",
    "birthDate": "birth_date",
    "injury_Age": "injury_age",
    "injuryYear": "injury_year",
    "priorYear": "prior_year",
    "prior_innings_pitched": "prior_innings_pitched",
    "prior_games_pitched": "prior_games_pitched",
    "prior_games_started": "prior_games_started",
    "prior_batters_faced": "prior_batters_faced",
    "prior_postseason_innings_pitched": "prior_postseason_innings_pitched",
    "prior_defensive_innings": "prior_defensive_innings",
    "prior_plate_appearances": "prior_plate_appearances",
    "prior_games_batting": "prior_games_batting",
    "prior_stolen_base_attempts": "prior_stolen_base_attempts",
    "prior_hit_by_pitch": "prior_hit_by_pitch",
    "total_prior_position_workload": "total_prior_position_workload",
    "debut_year": "debut_year",
    "years_experience": "years_experience",
    "POS": "position",
    "TurfFlag": "turf_flag"
}

schema_dict = {
    "injury_date": "DATETIME2",
    "team_name": "VARCHAR(255)",
    "notes": "VARCHAR(255)",
    "injury_flag": "INT",
    "dl_length_days": "INT",
    "injury_type": "VARCHAR(255)",
    "player_name": "VARCHAR(255)",
    "player_id": "VARCHAR(255)",
    "birth_date": "VARCHAR(255)",
    "injury_age": "FLOAT",
    "injury_year": "INT",
    "prior_year": "INT",
    "prior_innings_pitched": "FLOAT",
    "prior_games_pitched": "INT",
    "prior_games_started": "INT",
    "prior_batters_faced": "INT",
    "prior_postseason_innings_pitched": "FLOAT",
    "prior_defensive_innings": "FLOAT",
    "prior_plate_appearances": "INT",
    "prior_games_batting": "INT",
    "prior_stolen_base_attempts": "INT",
    "prior_hit_by_pitch": "INT",
    "total_prior_position_workload": "FLOAT",
    "debut_year": "FLOAT",
    "years_experience": "FLOAT",
    "position": "VARCHAR(255)",
    "turf_flag": "VARCHAR(255)"
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
script_logging_name = 'Injuries'  ##  Use the name of the main pipeline script. 
FILE_PATTERN =  "final_injuries.csv"                                      ##  Pattern of file to look for within the datafolder
TABLE_NAME = 'Injuries'                          ##  What table do you want to push into


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
