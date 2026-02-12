import os
import sys
import re
from pathlib import Path
from io import StringIO
from dotenv import load_dotenv
import pandas as pd
import numpy as np
import psycopg2
from psycopg2 import sql
import warnings

warnings.filterwarnings("ignore")
# Load environment variables
load_dotenv()

# Database connection - all must be present in environment
DB_HOST = os.environ["POSTGRES_HOST"]
DB_PORT = os.environ["POSTGRES_PORT"]
DB_NAME = os.environ["POSTGRES_DB"]
DB_USER = os.environ["POSTGRES_USER"]
DB_PASSWORD = os.environ["POSTGRES_PASSWORD"]

# Folder containing CSV files to process
FOLDER_PATH = fr"C:\Users\Andy\Documents\msds 498 project"


def get_connection():
    """Create and return a psycopg2 connection."""
    return psycopg2.connect(
        host=DB_HOST,
        port=DB_PORT,
        dbname=DB_NAME,
        user=DB_USER,
        password=DB_PASSWORD,
    )


def sanitize_column_name(name: str) -> str:
    """Convert column name to valid PostgreSQL identifier."""
    clean = re.sub(r"[^\w]", "_", str(name).strip())
    clean = re.sub(r"_+", "_", clean)
    clean = clean.strip("_")
    if clean and clean[0].isdigit():
        clean = f"col_{clean}"
    clean = clean.lower()
    return clean if clean else "unnamed_column"


def sanitize_table_name(filename: str) -> str:
    """Convert filename to valid PostgreSQL table name."""
    name = Path(filename).stem
    return sanitize_column_name(name)


def detect_column_types(df: pd.DataFrame) -> tuple[pd.DataFrame, dict]:
    """
    Auto-detect and convert column types in a DataFrame.
    Returns the converted DataFrame and a dict mapping columns to PostgreSQL types.
    """
    pg_types = {}

    for col in df.columns:
        series = df[col]

        # Skip if all null
        if series.isna().all():
            pg_types[col] = "TEXT"
            continue

        non_null = series.dropna()

        # Try boolean detection first
        if non_null.dtype == object:
            unique_lower = set(str(v).lower().strip() for v in non_null.unique())
            bool_values = {"true", "false", "yes", "no", "1", "0", "t", "f", "y", "n"}
            if unique_lower.issubset(bool_values) and len(unique_lower) <= 2:
                true_vals = {"true", "yes", "1", "t", "y"}
                df[col] = series.apply(
                    lambda x: True if str(x).lower().strip() in true_vals else False if pd.notna(x) else None
                )
                df[col] = df[col].astype("boolean")
                pg_types[col] = "BOOLEAN"
                continue

        # Try numeric conversion
        if non_null.dtype == object:
            try:
                numeric_series = pd.to_numeric(series, errors="coerce")
                if numeric_series.notna().sum() >= non_null.shape[0] * 0.9:
                    non_null_numeric = numeric_series.dropna()
                    if (non_null_numeric == non_null_numeric.astype(int)).all():
                        max_val = non_null_numeric.max()
                        min_val = non_null_numeric.min()
                        if min_val >= -2147483648 and max_val <= 2147483647:
                            df[col] = numeric_series.astype("Int64")
                            pg_types[col] = "INTEGER"
                        else:
                            df[col] = numeric_series.astype("Int64")
                            pg_types[col] = "BIGINT"
                    else:
                        df[col] = numeric_series.astype("Float64")
                        pg_types[col] = "DOUBLE PRECISION"
                    continue
            except (ValueError, TypeError):
                pass

        # Try datetime conversion
        if non_null.dtype == object:
            try:
                datetime_series = pd.to_datetime(series, errors="coerce", infer_datetime_format=True)
                if datetime_series.notna().sum() >= non_null.shape[0] * 0.9:
                    non_null_dt = datetime_series.dropna()
                    if (non_null_dt.dt.time == pd.Timestamp("00:00:00").time()).all():
                        df[col] = datetime_series.dt.date
                        pg_types[col] = "DATE"
                    else:
                        df[col] = datetime_series
                        pg_types[col] = "TIMESTAMP"
                    continue
            except (ValueError, TypeError):
                pass

        # Handle existing numeric types
        if pd.api.types.is_integer_dtype(series):
            max_val = series.max()
            min_val = series.min()
            if min_val >= -2147483648 and max_val <= 2147483647:
                pg_types[col] = "INTEGER"
            else:
                pg_types[col] = "BIGINT"
            continue

        if pd.api.types.is_float_dtype(series):
            pg_types[col] = "DOUBLE PRECISION"
            continue

        if pd.api.types.is_bool_dtype(series):
            pg_types[col] = "BOOLEAN"
            continue

        if pd.api.types.is_datetime64_any_dtype(series):
            pg_types[col] = "TIMESTAMP"
            continue

        # Default to text
        df[col] = series.astype(str).replace("nan", None)
        pg_types[col] = "TEXT"

    return df, pg_types


def create_table(cursor, table_name: str, columns: list, pg_types: dict):
    """Create a PostgreSQL table with the specified columns and types."""
    # Drop existing table
    cursor.execute(sql.SQL("DROP TABLE IF EXISTS {} CASCADE").format(sql.Identifier(table_name)))

    # Build CREATE TABLE statement
    column_defs = []
    for col in columns:
        col_type = pg_types.get(col, "TEXT")
        column_defs.append(sql.SQL("{} {}").format(sql.Identifier(col), sql.SQL(col_type)))

    create_stmt = sql.SQL("CREATE TABLE {} ({})").format(
        sql.Identifier(table_name),
        sql.SQL(", ").join(column_defs)
    )

    cursor.execute(create_stmt)


def load_data_copy(cursor, table_name: str, df: pd.DataFrame):
    """Load data using PostgreSQL COPY command (fastest method)."""
    # Prepare DataFrame for COPY
    buffer = StringIO()

    # Convert DataFrame to CSV string, handling nulls properly
    df.to_csv(buffer, index=False, header=False, na_rep="\\N")
    buffer.seek(0)

    # Use COPY command
    columns = [sql.Identifier(col) for col in df.columns]
    copy_stmt = sql.SQL("COPY {} ({}) FROM STDIN WITH (FORMAT CSV, NULL '\\N')").format(
        sql.Identifier(table_name),
        sql.SQL(", ").join(columns)
    )

    cursor.copy_expert(copy_stmt.as_string(cursor), buffer)


def process_csv_file(filepath: Path, conn) -> dict:
    """
    Process a single CSV file: read, detect types, create table, and load data.
    Returns a summary dict.
    """
    filename = filepath.name
    table_name = sanitize_table_name(filename)

    print(f"\n{'='*60}")
    print(f"Processing: {filename}")
    print(f"Table name: {table_name}")

    # Read CSV
    df = pd.read_csv(filepath, low_memory=False)
    original_rows = len(df)

    print(f"Rows: {original_rows}, Columns: {len(df.columns)}")

    # Rename columns to valid PostgreSQL identifiers
    column_mapping = {col: sanitize_column_name(col) for col in df.columns}

    # Handle duplicate column names after sanitization
    seen = {}
    for orig, new in column_mapping.items():
        if new in seen:
            seen[new] += 1
            column_mapping[orig] = f"{new}_{seen[new]}"
        else:
            seen[new] = 0

    df.rename(columns=column_mapping, inplace=True)

    # Detect and convert types
    df, pg_types = detect_column_types(df)

    # Print column type summary
    print("\nColumn types detected:")
    for col in df.columns:
        print(f"  {col}: {pg_types.get(col, 'TEXT')}")

    # Create table and load data
    cursor = conn.cursor()
    try:
        create_table(cursor, table_name, list(df.columns), pg_types)
        load_data_copy(cursor, table_name, df)
        conn.commit()
        print(f"Successfully loaded {original_rows} rows into table '{table_name}'")
        return {
            "file": filename,
            "table": table_name,
            "rows": original_rows,
            "columns": len(df.columns),
            "status": "success",
        }
    except Exception as e:
        conn.rollback()
        raise e
    finally:
        cursor.close()


def main(FOLDER_PATH):
    folder_path = Path(FOLDER_PATH)

    if not folder_path.exists():
        print(f"Error: Folder '{folder_path}' does not exist")
        sys.exit(1)

    if not folder_path.is_dir():
        print(f"Error: '{folder_path}' is not a directory")
        sys.exit(1)

    # Find all CSV files
    csv_files = sorted(folder_path.glob("*.csv"))

    if not csv_files:
        print(f"No CSV files found in '{folder_path}'")
        sys.exit(0)

    print(f"Found {len(csv_files)} CSV file(s) in '{folder_path}'")

    # Create database connection
    try:
        conn = get_connection()
        print("Database connection successful")
    except Exception as e:
        print(f"Database connection failed: {e}")
        sys.exit(1)

    # Process each file
    results = []
    try:
        for csv_file in csv_files:
            try:
                result = process_csv_file(csv_file, conn)
                results.append(result)
            except Exception as e:
                print(f"\nError processing {csv_file.name}: {e}")
                results.append({
                    "file": csv_file.name,
                    "table": sanitize_table_name(csv_file.name),
                    "rows": 0,
                    "columns": 0,
                    "status": f"error: {e}",
                })
    finally:
        conn.close()

    # Print summary
    print(f"\n{'='*60}")
    print("SUMMARY")
    print(f"{'='*60}")
    successful = sum(1 for r in results if r["status"] == "success")
    print(f"Processed: {len(results)} files")
    print(f"Successful: {successful}")
    print(f"Failed: {len(results) - successful}")

    for r in results:
        status_icon = "✓" if r["status"] == "success" else "✗"
        print(f"  {status_icon} {r['file']} -> {r['table']} ({r['rows']} rows)")



main(FOLDER_PATH)
b = 1