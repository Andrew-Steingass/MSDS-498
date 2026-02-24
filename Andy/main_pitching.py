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
    "stint": "stint_number",
    "teamID": "team_id",
    "lgID": "league_id",

    "W": "wins",
    "L": "losses",
    "G": "games_played",
    "GS": "games_started",
    "CG": "complete_games",
    "SHO": "shutouts",
    "SV": "saves",

    "IPouts": "innings_pitched",
    "H": "hits_allowed",
    "ER": "earned_runs",
    "HR": "home_runs_allowed",
    "BB": "walks",
    "SO": "strikeouts",

    "BAOpp": "batting_avg_against",
    "ERA": "earned_run_average",
    "IBB": "intentional_walks",
    "WP": "wild_pitches",
    "HBP": "hit_by_pitch",
    "BK": "balks",

    "BFP": "batters_faced",
    "GF": "games_finished",
    "R": "runs_allowed",
    "SH": "sacrifice_hits",
    "SF": "sacrifice_flies",
    "GIDP": "grounded_into_double_play"
}


schema_dict = {
    "player_id": "VARCHAR(255)",
    "season_year": "INT",
    "stint_number": "INT",
    "team_id": "VARCHAR(255)",
    "league_id": "VARCHAR(255)",

    "wins": "INT",
    "losses": "INT",
    "games_played": "INT",
    "games_started": "INT",
    "complete_games": "INT",
    "shutouts": "INT",
    "saves": "INT",

    "innings_pitched": "FLOAT",
    "hits_allowed": "INT",
    "earned_runs": "INT",
    "home_runs_allowed": "INT",
    "walks": "INT",
    "strikeouts": "INT",

    "batting_avg_against": "FLOAT",
    "earned_run_average": "FLOAT",
    "intentional_walks": "FLOAT",
    "wild_pitches": "FLOAT",
    "hit_by_pitch": "FLOAT",
    "balks": "INT",

    "batters_faced": "FLOAT",
    "games_finished": "FLOAT",
    "runs_allowed": "INT",
    "sacrifice_hits": "FLOAT",
    "sacrifice_flies": "FLOAT",
    "grounded_into_double_play": "FLOAT"
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
DATA_FOLDER = fr'C:\Users\Andy\Documents\msds 498 project'     ##  Folder location where the data lives
FILE_PATTERN =  "Pitching.csv"                                      ##  Pattern of file to look for within the datafolder
LOG_FOLDER = fr'C:\Users\Andy\Documents\msds 498 project\logs' ##  Location to save text logs to
SERVER_NAME = os.getenv('db_server')                                ##  Name of the SERVER_NAME 
DATABASE_NAME = 'master_baseball_capstone'                       ##  Database name the table lives in
TABLE_NAME = 'pitching'                          ##  What table do you want to push into
FILE_TRACKING_TABLE = 'file_tracking_table'      ##  Name variable. This will create a new table if not exists, and record the file name, dates, and rows pushed into DB.
BATCH_SIZE = 1000                            ##  When pushing into the DB, how many rows should be pushed into DB at once
ETL_FUNCTION = perform_etl_cleaning  ##  Create this function for every ETL Pipeline in "code_modules\ETL" folder scripts. May need to update import

SORT_COLUMNS = {'columns': ['', '', ''], 
               'ascending_bool': False}                 ##  TRUE = I want ascending, False = I do NOT want ascending sort
DELIMITER = fr','                                       ##  For no delimiter set to None    otherwise common items would be [  ','  ,  '|'  ]
                                                        #      
script_logging_name = 'main_pitching'  ##  Use the name of the main pipeline script. 

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
