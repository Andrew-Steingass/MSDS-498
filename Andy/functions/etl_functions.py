#=======================##=======================#  Scripts
from functions.utils import error_reporting
#=======================##=======================# Script Specific Imports
import pyodbc
#import psycopg2
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
    connection_string = (
        "DRIVER={ODBC Driver 18 for SQL Server};"
        f"SERVER={server};"##,{os.getenv('db_port')};"
        f"DATABASE={database_name};"
        f"UID={os.getenv('db_user')};"
        f"PWD={os.getenv('db_password')};"
        "Encrypt=yes;"
        "TrustServerCertificate=no;"
        "Connection Timeout=30;"
    )
    connection = pyodbc.connect(connection_string)
    cursor = connection.cursor()
    # connection = psycopg2.connect(
    #     host=server,
    #     database=database_name,
    #     user=os.getenv("db_user"),
    #     password=os.getenv("db_password"),
    #     port=os.getenv("db_port")
    # )
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

def process_data_batch(data_list, header, current_time, filename, conn, cur, logger, TABLE_NAME, BATCH_SIZE, sort_columns, ETL_func_object, filepath, script_logging_name, schema_dict, rename_dict):
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
        
        create_table_sql_query, df, auto_gen_column = ETL_func_object(data_list, logger, TABLE_NAME, filepath, header, script_logging_name, schema_dict, rename_dict) #*********************************************************************************************
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


def process_file(filepath, conn, cur, logger, FILE_TRACKING_TABLE, TABLE_NAME, BATCH_SIZE, delimiter, sort_columns, ETL_func_object, database_name, script_logging_name, schema_dict, rename_dict):
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
            txt_load(filepath, current_time, filename, conn, cur, logger, TABLE_NAME, BATCH_SIZE, delimiter, ETL_func_object, sort_columns, script_logging_name, schema_dict, rename_dict)
        if ext in ('.xls', '.xlsx'):
            excel_load(filepath, sort_columns, current_time, filename, conn, cur, logger, TABLE_NAME, BATCH_SIZE, ETL_func_object, script_logging_name, schema_dict, rename_dict)
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



def txt_load(filepath, current_time, filename, conn, cur, logger, TABLE_NAME, BATCH_SIZE, delimiter, ETL_func_object, sort_columns, script_logging_name, schema_dict, rename_dict):
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
                    data_list, header, current_time, filename, conn, cur, logger, TABLE_NAME, BATCH_SIZE, sort_columns, ETL_func_object, filepath, script_logging_name, schema_dict, rename_dict
                )

                data_list = []
                time.sleep(5) ## this is so the memory can be emptied
    
    # Process remaining records
    if data_list:
        logger.info(f'Processing remaining {len(data_list)} records')
        data_list = [header] + data_list
        records_added = process_data_batch(
            data_list, header, current_time, filename, conn, cur, logger, TABLE_NAME, BATCH_SIZE, sort_columns, ETL_func_object, filepath, script_logging_name, schema_dict, rename_dict
        )

    return 



def excel_load(filepath, sort_columns, current_time, filename, conn, cur, logger, TABLE_NAME, BATCH_SIZE, ETL_func_object, script_logging_name, schema_dict, rename_dict):

    # read entire sheet, then slice
    df = pd.read_excel(filepath, dtype=str)

    header = list(df.columns)
    for start in range(0, len(df), BATCH_SIZE):
        batch_df = df.iloc[start:start + BATCH_SIZE]
        data_list = batch_df.values.tolist()
        logger.info(f"Batch rows {start}–{start + len(data_list)} from {filename}")
        added = process_data_batch(
            data_list, header, current_time, filename, conn, cur, logger, TABLE_NAME, BATCH_SIZE, sort_columns, ETL_func_object, filepath, script_logging_name, schema_dict, rename_dict
        )

        time.sleep(5)

    #if sort_columns:
        #df.sort_values(by=sort_columns, inplace=True, na_position='last')
    return 



def csv_load(filepath, current_time, filename, conn, cur, logger, TABLE_NAME, BATCH_SIZE, ETL_func_object, sort_columns, script_logging_name, schema_dict, rename_dict):

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
            TABLE_NAME, BATCH_SIZE, sort_columns, ETL_func_object, filepath, script_logging_name, schema_dict, rename_dict
        )
        

        logger.info(f'Added {records_added} records from batch {i+1}')
        
    logger.info(f'Completed loading CSV file: {filepath}.')
    

    
    return 