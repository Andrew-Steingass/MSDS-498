

#=======================##=======================#  Scripts
#=======================##=======================# Script Specific Imports
from datetime import datetime as dt
#=======================##=======================# Core Imports
from dotenv import load_dotenv
load_dotenv()
import requests
import json
import logging
import pandas as pd
import numpy as np
import time
import os
import datetime
today = datetime.datetime.now().strftime('%Y-%m-%d')
username = os.getlogin()
script_start = time.time()
#=======================##=======================#


def generate_create_table(schema_dict):
    columns = []
    
    for column_name, data_type in schema_dict.items():
        columns.append(f"{column_name} {data_type}")
    
    query = "CREATE TABLE {} (\n"
    query += ",\n".join(columns)
    query += "\n);"
    
    return query

def perform_etl_cleaning(data_list, logger, TABLE_NAME, filepath, header, script_logging_name, schema_dict, rename_dict):
    try:
        # Create DataFrame from data
        header = [s.strip() for s in header] 
        df = pd.DataFrame(data_list, columns=header)
        first_row = [s.strip() if isinstance(s, str) else s for s in list(df.iloc[0])]
        if list(df.columns) == first_row:
            df = df.drop(index=0).reset_index(drop=True)
        df = df.rename(columns=rename_dict)

        float_columns = [c for c, t in schema_dict.items() if t.startswith("FLOAT")]
        int_columns   = [c for c, t in schema_dict.items() if t.startswith("INT")]
        string_columns = [c for c, t in schema_dict.items() if t.startswith("VARCHAR")]
        date_columns  = [c for c, t in schema_dict.items() if t in ["TIMESTAMP", "DATETIME2"]]
        for col in float_columns:
            df[col] = df[col].replace('', np.nan).astype(float)
        for col in int_columns:
            df[col] = df[col].replace('', np.nan).astype("Int64")
        for col in string_columns:
            df[col] = df[col].replace('', np.nan).astype(str).str.strip()
        for col in date_columns:
            df[col] = df[col].replace('', np.nan)
            df[col] = pd.to_datetime(df[col])
    
        auto_gen_column = ['Table_ID']
        sql_query = generate_create_table(schema_dict)

        logger.info(f"perform_etl_epic_orders successful")
    except Exception as e:
        error_reporting(logger, f"Error perform_etl_epic_orders: {e}", module_name= script_logging_name) 
    return sql_query, df, auto_gen_column


def error_reporting(logger, error_message: str, module_name: str, environment: str = "PROD"):
    logger.error(error_message)
    try:
        timestamp = dt.utcnow().strftime("%Y-%m-%d %H:%M:%S UTC")
        formatted_error = (
            f"❌ ERROR REPORT\n\n"
            f"📌 Module: {module_name}\n\n"
            f"🌎 Environment: {environment}\n\n"
            f"⏰ Timestamp: {timestamp}\n\n"
            f"🛑 Error: {error_message}"
        )
        # error_msg = send_teams_message2(
        #         text_header ='test1',
        #         text_header_sub_text ='test2',
        #         message = formatted_error,
        #         workflow_url=os.getenv('error_chat_workflow')
        #     )
        if formatted_error:
             logger.error(formatted_error)
    except Exception as e:
         logger.error(f'Something broken with the Teams Api {e}')

    # Optionally send to Teams here
    return 


def generate_create_table_sql(df, table_name):
   ## Help make initial sql query
   type_mapping = {
       'int64': 'INT',
       'float64': 'FLOAT',
       'object': 'VARCHAR(255)',
       'datetime64[ns]': 'DATETIME2',
       'bool': 'BIT'
   }
   
   columns = []
   for col, dtype in df.dtypes.items():
       sql_type = type_mapping.get(str(dtype), 'VARCHAR(255)')
       columns.append(f"[{col}] {sql_type}")
   
   create_table = f"""
   CREATE TABLE {table_name} (
       {',\n        '.join(columns)}
   )
   """
   return create_table



# Set up logging
def setup_logging(database_name, LOG_FOLDER, TABLE_NAME):
    """Configure logging to write to both console and file"""
    if not os.path.exists(LOG_FOLDER):
        os.makedirs(LOG_FOLDER)
        
    log_file = os.path.join(LOG_FOLDER, f"{TABLE_NAME}.log")
    
    # Configure logging
    logging.basicConfig(
        level=logging.INFO,
        format='%(asctime)s - %(levelname)s - %(message)s',
        handlers=[
            logging.FileHandler(log_file, mode='a'),  # Append mode to keep adding to the same log
            logging.StreamHandler()
        ]
    )
    return logging.getLogger(f'{LOG_FOLDER}_etl')