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
    "yearID": "season_year",
    "lgID": "league_id",
    "teamID": "team_id",
    "franchID": "franchise_id",
    "divID": "division_id",

    "Rank": "division_rank",
    "G": "games_played",
    "Ghome": "home_games",
    "W": "wins",
    "L": "losses",

    "DivWin": "division_winner_flag",
    "WCWin": "wildcard_winner_flag",
    "LgWin": "league_winner_flag",
    "WSWin": "world_series_winner_flag",

    "R": "runs_scored",
    "AB": "at_bats",
    "H": "hits",
    "2B": "doubles",
    "3B": "triples",
    "HR": "home_runs",
    "BB": "walks",
    "SO": "strikeouts",
    "SB": "stolen_bases",
    "CS": "caught_stealing",
    "HBP": "hit_by_pitch",
    "SF": "sacrifice_flies",

    "RA": "runs_allowed",
    "ER": "earned_runs_allowed",
    "ERA": "earned_run_average",
    "CG": "complete_games",
    "SHO": "shutouts",
    "SV": "saves",
    "IPouts": "outs_recorded",
    "HA": "hits_allowed",
    "HRA": "home_runs_allowed",
    "BBA": "walks_allowed",
    "SOA": "strikeouts_allowed",

    "E": "errors",
    "DP": "double_plays",
    "FP": "fielding_percentage",

    "name": "team_name",
    "park": "home_park",
    "attendance": "attendance",
    "BPF": "batting_park_factor",
    "PPF": "pitching_park_factor",

    "teamIDBR": "team_id_br",
    "teamIDlahman45": "team_id_lahman",
    "teamIDretro": "team_id_retro"
}

schema_dict = {
    "season_year": "INT",
    "league_id": "VARCHAR(255)",
    "team_id": "VARCHAR(255)",
    "franchise_id": "VARCHAR(255)",
    "division_id": "VARCHAR(255)",

    "division_rank": "INT",
    "games_played": "INT",
    "home_games": "FLOAT",
    "wins": "INT",
    "losses": "INT",

    "division_winner_flag": "VARCHAR(255)",
    "wildcard_winner_flag": "VARCHAR(255)",
    "league_winner_flag": "VARCHAR(255)",
    "world_series_winner_flag": "VARCHAR(255)",

    "runs_scored": "INT",
    "at_bats": "INT",
    "hits": "INT",
    "doubles": "INT",
    "triples": "INT",
    "home_runs": "INT",
    "walks": "INT",
    "strikeouts": "FLOAT",
    "stolen_bases": "FLOAT",
    "caught_stealing": "FLOAT",
    "hit_by_pitch": "FLOAT",
    "sacrifice_flies": "FLOAT",

    "runs_allowed": "INT",
    "earned_runs_allowed": "INT",
    "earned_run_average": "FLOAT",
    "complete_games": "INT",
    "shutouts": "INT",
    "saves": "INT",
    "outs_recorded": "INT",
    "hits_allowed": "INT",
    "home_runs_allowed": "INT",
    "walks_allowed": "INT",
    "strikeouts_allowed": "INT",

    "errors": "INT",
    "double_plays": "FLOAT",
    "fielding_percentage": "FLOAT",

    "team_name": "VARCHAR(255)",
    "home_park": "VARCHAR(255)",
    "attendance": "FLOAT",
    "batting_park_factor": "INT",
    "pitching_park_factor": "INT",

    "team_id_br": "VARCHAR(255)",
    "team_id_lahman": "VARCHAR(255)",
    "team_id_retro": "VARCHAR(255)"
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
script_logging_name = 'Teams'  ##  Use the name of the main pipeline script. 
FILE_PATTERN =  "Teams.csv"                                      ##  Pattern of file to look for within the datafolder
TABLE_NAME = 'Teams'                          ##  What table do you want to push into


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
