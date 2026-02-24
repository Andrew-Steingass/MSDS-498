



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
        error_msg = send_teams_message2(
                text_header ='test1',
                text_header_sub_text ='test2',
                message = formatted_error,
                workflow_url=os.getenv('error_chat_workflow')
            )
        if error_msg:
             logger.error(error_msg)
    except Exception as e:
         logger.error(f'Something broken with the Teams Api {e}')

    # Optionally send to Teams here
    return 


def perform_etl_imaging_radimetrics(data_list, logger, TABLE_NAME, filepath, header, script_logging_name):
    try:
        # Create DataFrame from data
        header = [s.strip() for s in header] ## added 9.12.25 never tested...
        df = pd.DataFrame(data_list, columns=header)
        first_row = [s.strip() if isinstance(s, str) else s for s in list(df.iloc[0])]
        if list(df.columns) == first_row:
            df = df.drop(index=0).reset_index(drop=True)
        float_columns = ['CTDIvol Body mGy', 'CTDIvol Head mGy', 'DLP Body mGy-cm', 'DLP Head mGy-cm']
        for col in float_columns:
            df[col] = df[col].replace('', np.nan).astype(float)
        auto_gen_column = ['radimetrics_Table_ID']

        sql_query = '''
        CREATE TABLE {} (
        )'''

        logger.info(f"perform_etl_epic_orders successful")
    except Exception as e:
        error_reporting(logger, f"Error perform_etl_epic_orders: {e}", module_name= script_logging_name) 
    return sql_query, df, auto_gen_column



def generate_create_table_sql(df, table_name):
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



#######################################################################################################################################################################
#######################################################################################################################################################################
#######################################################################################################################################################################


#=======================##=======================# OSF Scripts
from utils_module.DB_ETL_Utils  import  process_file, setup_logging, get_db_connection, ensure_file_tracking_table, create_database_table, get_processed_files, get_files_to_process
from code_modules.ETL.Imaging import perform_etl_epic_financials
#=======================##=======================# Script Specific Imports
import pyodbc

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




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
DATA_FOLDER = r''     ##  Folder location where the data lives
FILE_PATTERN =  "**.txt"                                      ##  Pattern of file to look for within the datafolder
LOG_FOLDER = r'' ##  Location to save text logs to
SERVER_NAME = os.getenv('')                                ##  Name of the SERVER_NAME (probably PMC-PIA-DBFP)
DATABASE_NAME = ''                       ##  Database name the table lives in
TABLE_NAME = ''                          ##  What table do you want to push into
FILE_TRACKING_TABLE = ''      ##  Name variable. This will create a new table if not exists, and record the file name, dates, and rows pushed into DB.
BATCH_SIZE = 1000                            ##  When pushing into the DB, how many rows should be pushed into DB at once
ETL_FUNCTION = perform_etl_epic_financials  ##  Create this function for every ETL Pipeline in "code_modules\ETL" folder scripts. May need to update import

SORT_COLUMNS = {'columns': ['', '', ''], ##  TODO figure out exact logic for no variable. I believe if you have a wrong column name or no vars, it skips
               'ascending_bool': False}                 ##  TRUE = I want ascending, False = I do NOT want ascending sort
DELIMITER = fr'|'                                       ##  For no delimiter set to None    otherwise common items would be [  ','  ,  '|'  ]
                                                        #      NOTE csv doesnt really work as of 6.24. It will load csv as txt file with may mess up formatting
script_logging_name = ''  ##  Use the name of the main pipeline script. this will go to Teams error channel.

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
                                 TABLE_NAME, BATCH_SIZE, DELIMITER, SORT_COLUMNS,  ETL_FUNCTION, DATABASE_NAME, script_logging_name)

    logger.info(f"Completed processing {os.path.basename(filepath)}: {records_count} records added.")
logger.info("ETL process completed successfully.")

cur.close()
conn.close()
logger.info("Database connection closed.")



#######################################################################################################################################################################
#######################################################################################################################################################################
#######################################################################################################################################################################


#=======================##=======================# OSF Scripts
from api_module.Teams.Teams_sharepoint_API import error_reporting
#=======================##=======================# Script Specific Imports
import pyodbc
import glob
from pathlib import Path
import csv
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


def get_db_connection(server, database_name):
    server = server
    database = database_name
    connection_string = f'DRIVER={{ODBC Driver 17 for SQL Server}};SERVER={server};DATABASE={database};Trusted_Connection=yes;'
    connection = pyodbc.connect(connection_string)
    cursor = connection.cursor()
    return connection, cursor

def generate_create_table_sql(df, table_name):
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


def insert_dataframe_to_sql(df, table_name, conn, cur, batch_size=1000):
    df = df.replace({pd.NA: None, np.nan: None})
    columns = list(df.columns)
    columns_str = ','.join(f'[{col}]' for col in columns)
    placeholders = ','.join('?' for _ in columns)

    for i in range(0, len(df), batch_size):
        batch = df.iloc[i:i + batch_size]
        values = [tuple(x) for x in batch.values]
        cur.executemany(f"INSERT INTO {table_name} ({columns_str}) VALUES ({placeholders})", values)
        conn.commit()
    return

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

def ensure_file_tracking_table(conn, cur, logger, FILE_TRACKING_TABLE, script_logging_name):
    """
    Create file tracking table if it doesn't exist.
    
    Args:
        conn: Database connection
        cur: Database cursor
        logger: Logger instance
    """
    sql_query = f'''
    IF NOT EXISTS (SELECT * FROM sys.tables WHERE name = '{FILE_TRACKING_TABLE}')
    BEGIN
        CREATE TABLE {FILE_TRACKING_TABLE} (
            [FileName] VARCHAR(255) PRIMARY KEY,
            [ProcessedDate] DATETIME2,
            [FileCreationDate] DATETIME2,
            [RecordsProcessed] INT
        )
    END
    '''
    try:
        cur.execute(sql_query)
        conn.commit()
        logger.info(f"File tracking table '{FILE_TRACKING_TABLE}' verified.")
    except pyodbc.Error as e:
        error_reporting(logger, f"Error creating file tracking table: {e}", module_name = script_logging_name)
    return




def create_database_table(conn, cur, logger, TABLE_NAME, sql_query, script_logging_name):
    """
    Create the database table if it doesn't exist.
    
    Args:
        conn: Database connection
        cur: Database cursor
        logger: Logger instance
    """
    try:
        # Check if table exists
        cur.execute(f"IF NOT EXISTS (SELECT * FROM sys.tables WHERE name = '{TABLE_NAME}') BEGIN {sql_query} END")
        conn.commit()
        logger.info(f"Table '{TABLE_NAME}' verified in database.")
    except pyodbc.Error as e:
        error_reporting(logger, f"Error creating/verifying table: {e}", module_name=script_logging_name)
    return




def get_processed_files(cur, logger, FILE_TRACKING_TABLE, script_logging_name):
    """
    Get list of files that have already been processed and added to the database.
    
    Args:
        cur: Database cursor
        logger: Logger instance
        
    Returns:
        set: Set of processed filenames
    """
    try:
        cur.execute(f"SELECT [FileName] FROM {FILE_TRACKING_TABLE}")
        rows = cur.fetchall()
        logger.info(f"get_processed_files successful")
        return {row[0] for row in rows}
    except pyodbc.Error as e:
        error_reporting(logger, f"Error retrieving processed files: {e}", module_name = script_logging_name)
        return set()
    


def get_files_to_process(logger, DATA_FOLDER, file_and_extension, script_logging_name):
    """
    Get list of files that need to be processed, sorted by earliest timestamp 
    (created or modified).
    
    Args:
        logger: Logger instance
        DATA_FOLDER (str): Folder path to search
        file_and_extension (str): File pattern (e.g. '*.csv')
        
    Returns:
        list: Sorted list of file paths
    """
    try:
        file_pattern = os.path.join(DATA_FOLDER, file_and_extension)
        files = glob.glob(file_pattern)

        # Remove temp/locked files
        files = [f for f in files if '~' not in os.path.basename(f)]

        # Sort by earliest of creation time or last modified time
        files.sort(key=lambda f: min(os.path.getctime(f), os.path.getmtime(f)))

        logger.info(f"Found {len(files)} files matching pattern in {DATA_FOLDER}")
    except Exception as e:
        error_reporting(logger, f"Error retrieving processed files: {e}", module_name = script_logging_name)
    return files


###################################################################################################################
##       Main pipeline
###################################################################################################################

def process_data_batch(data_list, header, current_time, filename, conn, cur, logger, TABLE_NAME, BATCH_SIZE, sort_columns, ETL_func_object, filepath, script_logging_name):
    """
    Process a batch of data and insert it into the database.
    
    Args:
        data_list (list): List of data rows
        header (list): Column headers
        current_time (datetime): Current timestamp
        filename (str): Source file name
        conn: Database connection
        cur: Database cursor
        logger: Logger instance
        
    Returns:
        int: Number of rows processed
    """
    try:
        if not data_list:
            return 0
        
        create_table_sql_query, df, auto_gen_column = ETL_func_object(data_list, logger, TABLE_NAME, filepath, header, script_logging_name) #*********************************************************************************************
        df['Row_CreatedOn_Date'] = current_time
        df['FileName'] = filename
        
        def add_columns(sql_query):
            last_paren_pos = sql_query.rfind(')')
            sql_query = sql_query[:last_paren_pos] + ' ,      [Row_CreatedOn_Date] DATETIME2,\n        [FileName] VARCHAR(255)\n' + sql_query[last_paren_pos:]
            return sql_query
        
        create_table_sql_query = add_columns(create_table_sql_query)
        create_database_table(conn, cur, logger, TABLE_NAME, create_table_sql_query.format(TABLE_NAME), script_logging_name) ## this is the main table for the script to insert into

        # Get column order from database
        sql_query = f"""
        SELECT COLUMN_NAME
        FROM INFORMATION_SCHEMA.COLUMNS
        WHERE TABLE_NAME = '{TABLE_NAME}'
        AND COLUMN_NAME != 'ID'  -- Exclude the identity column
        ORDER BY ORDINAL_POSITION;
        """
        cur.execute(sql_query)
        columns = [row[0] for row in cur.fetchall()]
        
        # Check column count
        og_column = df.columns
        if len(og_column) != len(columns):
            logger.warning(f'Column count mismatch. File has {len(og_column)}, DB has {len(columns)}, (may be auto PK which is okay)')
        
        # Clean data and reorder columns
        df = df.replace({pd.NA: None, pd.NaT: None, np.nan: None})
        
        # Make sure all expected columns exist
        missing_cols = set(columns) - set(list(df.columns) + auto_gen_column)
        for col in missing_cols:
            df[col] = None
        
        df = df[list(set(columns) - set(auto_gen_column))]
        
        # Example: sort_columns = ['INVOICE_NUM', 'FIRST_HTR_TX_ID', 'TX_POST_DATE']
        if all(col in df.columns for col in sort_columns['columns']):
            df_sorted = df.sort_values(
                by=sort_columns['columns'],
                na_position='last',
                ascending=[sort_columns['ascending_bool']] * len(sort_columns['columns'])
            )
        else:
            df_sorted = df.copy()

        ####################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        total_rows = len(df_sorted)
        logger.info(f'Starting batch insert of {total_rows} rows')
        batch_count = (total_rows + BATCH_SIZE - 1) // BATCH_SIZE

        staging_table = f"{TABLE_NAME}_staging"
        try:
            cur.execute(f"""
            IF OBJECT_ID('{staging_table}', 'U') IS NULL
            BEGIN
                {create_table_sql_query.format(staging_table)}
            END
            ELSE
            BEGIN
                TRUNCATE TABLE {staging_table}
            END
            """)
            conn.commit()
        except Exception as e:
            error_reporting(logger
                            ,f"Error creating Staging Table: {TABLE_NAME}: \n *** NOTE *** Ensure Create table query in ETL func is correctly structured for var.format() \n {e}"
                            , module_name=script_logging_name)
            conn.rollback()


        insert_cols = df_sorted.columns.tolist()
        placeholders = ', '.join('?' for _ in insert_cols)
        column_names = ', '.join(f'[{col}]' for col in insert_cols)
        insert_sql = f"INSERT INTO {staging_table} ({column_names}) VALUES ({placeholders})"

        cur.fast_executemany = True
        for i in range(0, total_rows, BATCH_SIZE):
            batch_df = df_sorted.iloc[i:i + BATCH_SIZE]
            param_rows = [tuple(row) for row in batch_df.values]
            try:
                cur.executemany(insert_sql, param_rows)
                conn.commit()
                if i == 0 or i + BATCH_SIZE >= total_rows or batch_count == 1:
                    logger.info(f'Inserted batch {i//BATCH_SIZE + 1} of {batch_count} into staging')
            except pyodbc.Error as e:
                error_reporting(logger, f"Error inserting batch {i//BATCH_SIZE + 1} into staging: {e}", module_name=script_logging_name)
                conn.rollback()

        key_columns = [col for col in insert_cols if col not in ['Row_CreatedOn_Date', 'FileName']+ auto_gen_column ] 
        where_conditions = [
            f"(t.[{col}] = s.[{col}] OR (t.[{col}] IS NULL AND s.[{col}] IS NULL))"
            for col in key_columns
        ]
        where_clause = ' AND '.join(where_conditions)
        select_columns = ', '.join(f's.[{col}]' for col in insert_cols)

        final_insert = f"""
        INSERT INTO {TABLE_NAME} ({column_names})
        SELECT {select_columns}
        FROM {staging_table} s
        WHERE NOT EXISTS (
            SELECT 1 FROM {TABLE_NAME} t WHERE {where_clause}
        );
        """

        try:
            cur.execute(final_insert)
            conn.commit()
            logger.info(f"Inserted new rows from staging into {TABLE_NAME}")
        except pyodbc.Error as e:
            error_reporting(logger, f"Error inserting from staging to {TABLE_NAME}: {e}", module_name=script_logging_name)
            conn.rollback()

        try:
            cur.execute(f"TRUNCATE TABLE {staging_table}")
            conn.commit()
            logger.info(f"Truncated staging table {staging_table}")
        except pyodbc.Error as e:
            logger.warning(f"Failed to truncate staging table {staging_table}: {e}")

    except Exception as e:
        error_reporting(logger, f"Error process_data_batch {e}", module_name=script_logging_name)
    return total_rows


def process_file(filepath, conn, cur, logger, FILE_TRACKING_TABLE, TABLE_NAME, BATCH_SIZE, delimiter, sort_columns, ETL_func_object, database_name, script_logging_name):
    """
    Process a single file and insert its data into the database.
    
    Args:
        filepath (str): Path to the file to process
        conn: Database connection
        cur: Database cursor
        logger: Logger instance
        
    Returns:
        int: Number of records processed
    """
    filename = os.path.basename(filepath)   ## TODO DO the creation_date to be the modified date
    logger.info(f"Processing file: {filename}")
    
    # Find the oldest date attached to file and call it when the file was created.
    creation = os.path.getctime(filepath)
    modified  = os.path.getmtime(filepath)
    if creation < modified:
        timestamp = creation
    else:
        timestamp = modified
    creation_date = dt.fromtimestamp(timestamp)
    current_time = dt.now()
    logger.info(f"File creation date: {creation_date.strftime('%Y-%m-%d %H:%M:%S')}")
    
    # Add to tracking table with zero records initially
    try:
        sql = f"INSERT INTO {FILE_TRACKING_TABLE} VALUES (?, ?, ?, 0)"
        cur.execute(sql, (filename, current_time, creation_date))
        conn.commit()
        logger.info(f"File '{filename}' registered in tracking table.")
    except pyodbc.Error as e:
        error_reporting(logger, f"Error registering file: {e}", module_name=script_logging_name)
        return 0
    
    #counter = 0
    #data_list = []
    ext = Path(filepath).suffix.lower()
    # Process file line by line
    try:
        if ext in ('.csv', '.txt'):
            txt_load(filepath, current_time, filename, conn, cur, logger, TABLE_NAME, BATCH_SIZE, delimiter, ETL_func_object, sort_columns, script_logging_name)
        if ext in ('.xls', '.xlsx'):
            excel_load(filepath, sort_columns, current_time, filename, conn, cur, logger, TABLE_NAME, BATCH_SIZE, ETL_func_object, script_logging_name)
    except Exception as e:
        if hasattr(e, "errno") and e.errno == 13: ## Epic sometimes hangs when giving a file. it results in permission error. this prevents it
            error_reporting(logger, f"Error processing file {filename}: {str(e)} ***Permission error, **WILL RETRY**", module_name=script_logging_name)
            sql = f"DELETE FROM {FILE_TRACKING_TABLE} WHERE FileName = ?"
            cur.execute(sql, (filename,))
            conn.commit()
            return 0
        else:
            error_reporting(logger, f"Error processing file {filename}: {str(e)} ***(may be ETL function outputs)", module_name=script_logging_name)
            return 0
    # Update the record count in the tracking table
    try:
        sql_count = f"SELECT COUNT(*) FROM [{database_name}].dbo.[{TABLE_NAME}] WHERE FileName = ?"
        cur.execute(sql_count, (filename,))
        total_records = cur.fetchone()[0]
        sql = f"UPDATE {FILE_TRACKING_TABLE} SET RecordsProcessed = ? WHERE FileName = ?"
        cur.execute(sql, (total_records, filename))
        time.sleep(1)
        conn.commit()
        logger.info(f"Updated record count for '{filename}': {total_records} records")
    except pyodbc.Error as e:
        error_reporting(logger, f"Error updating record count: {e}", module_name=script_logging_name)  
    return total_records



def txt_load(filepath, current_time, filename, conn, cur, logger, TABLE_NAME, BATCH_SIZE, delimiter, ETL_func_object, sort_columns, script_logging_name):
    data_list = []
    counter = 0
    with open(filepath, 'r') as file:
        reader = csv.reader(file, delimiter=delimiter, skipinitialspace=True)
        for line_num, data in enumerate(reader):
            if line_num == 0:
                header = [field.strip() for field in data]  # Strip whitespace from header
                counter += 1
                continue
                
            data = [field.strip() for field in data]
            data_list.append(data)
            counter += 1
            
            # Process in batches of 1,000,000 rows for memory efficiency
            if counter % 500000 == 0:
                logger.info(f'Processed {counter} lines, starting batch insert')
                #data_list = [header] + data_list
                records_added = process_data_batch(
                    data_list, header, current_time, filename, conn, cur, logger, TABLE_NAME, BATCH_SIZE, sort_columns, ETL_func_object, filepath, script_logging_name
                )

                data_list = []
                time.sleep(5) ## this is so the memory can be emptied
    
    # Process remaining records
    if data_list:
        logger.info(f'Processing remaining {len(data_list)} records')
        data_list = [header] + data_list
        records_added = process_data_batch(
            data_list, header, current_time, filename, conn, cur, logger, TABLE_NAME, BATCH_SIZE, sort_columns, ETL_func_object, filepath, script_logging_name
        )

    return 



def excel_load(filepath, sort_columns, current_time, filename, conn, cur, logger, TABLE_NAME, BATCH_SIZE, ETL_func_object, script_logging_name):

    # read entire sheet, then slice
    df = pd.read_excel(filepath, dtype=str)

    header = list(df.columns)
    for start in range(0, len(df), BATCH_SIZE):
        batch_df = df.iloc[start:start + BATCH_SIZE]
        data_list = batch_df.values.tolist()
        logger.info(f"Batch rows {start}–{start + len(data_list)} from {filename}")
        added = process_data_batch(
            data_list, header, current_time, filename, conn, cur, logger, TABLE_NAME, BATCH_SIZE, sort_columns, ETL_func_object, filepath, script_logging_name
        )

        time.sleep(5)

    #if sort_columns:
        #df.sort_values(by=sort_columns, inplace=True, na_position='last')
    return 



def csv_load(filepath, current_time, filename, conn, cur, logger, TABLE_NAME, BATCH_SIZE, ETL_func_object, sort_columns, script_logging_name):

    ## WARNING NOTE BUG BUG This function really doesnt work because loading into a list can be messier. It is easier to first load csv, save as txt file | deliminated
    # Set chunk size to match BATCH_SIZE for direct 1:1 mapping
    chunk_size = BATCH_SIZE  # Using your existing BATCH_SIZE of 1000
    
    logger.info(f'Starting to load CSV file: {filepath}')
    

    # Read the CSV in chunks of BATCH_SIZE
    chunks = pd.read_csv(
        filepath,
        chunksize=chunk_size,
        low_memory=False  # Better for mixed data types
    )
    
    for i, chunk_df in enumerate(chunks):
        logger.info(f'Processing batch {i+1} with {len(chunk_df)} rows')
        
        # Convert chunk to list of lists for compatibility with process_data_batch
        header = chunk_df.columns.tolist()
        data_list = chunk_df.values.tolist()
        
        # Process this chunk/batch
        records_added = process_data_batch(
            data_list, header, current_time, filename, conn, cur, logger, 
            TABLE_NAME, BATCH_SIZE, sort_columns, ETL_func_object, filepath, script_logging_name
        )
        

        logger.info(f'Added {records_added} records from batch {i+1}')
        
    logger.info(f'Completed loading CSV file: {filepath}.')
    

    
    return 