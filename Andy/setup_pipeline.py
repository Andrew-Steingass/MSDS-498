import pandas as pd
from functions.utils import generate_create_table_sql
from pprint import pprint

name = 'final_injuries'
fpath = fr"C:\Users\Andy\Documents\msds 498 project\{name}.csv"
df = pd.read_csv(fpath)


query = generate_create_table_sql(df, 'delete me')

print(query) # use this in pipeline, adjust as needed
print( fr'file name = {name}')
b = 1