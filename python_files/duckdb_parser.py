#-------------------- CODE TO PARSE .dly FILES AND CREATE A DUCKDB --------------------#

import os
import glob
import pandas as pd
import duckdb
from tqdm import tqdm
import csv
import tarfile

#Make a set of all relevant station IDs from the filtered_stations_IDs.csv file
with open('stations_data/filtered_stations_IDs.csv', newline='') as csvfile:
    reader = csv.reader(csvfile)
    next(reader) #skip header
    good_IDs = {row[0] for row in reader}

#all .dly files are stored in folder al_stations
output_dir = 'all_stations'
os.makedirs(output_dir, exist_ok=True)

#Extract the .dly files from the tar file
tar_path = 'stations_data/ghcnd_all.tar'
with tarfile.open(tar_path, 'r') as tar:
    for member in tar.getmembers():
        if member.isfile() and member.name.endswith('.dly'):
            station_id = os.path.splitext(os.path.basename(member.name))[0]
            if station_id in good_IDs:
                tar.extract(member, path=output_dir)

#Create duckdb database
con = duckdb.connect("../data/weather.duckdb")

#Create weather table with station + date primary key
con.execute("""
CREATE TABLE IF NOT EXISTS weather (
  ID      VARCHAR  NOT NULL,
  date    DATE     NOT NULL,
  TMAX    DOUBLE,
  TMIN    DOUBLE,
  PRCP    DOUBLE,
  PRIMARY KEY (ID, date)
);
""")

#Fixed-width parser spec
col_names = (
    ['ID','YEAR','MONTH','ELEMENT']
    + [f"{name}_{day}" for day in range(1,32) for name in ('VALUE','MFLAG','QFLAG','SFLAG')]
)
widths = [11,4,2,4] + [w for _ in range(31) for w in (5,1,1,1)]

#Loop thru every station file with tqdm progress bar
files = glob.glob(os.path.join("all_stations", "**", "*.dly"), recursive=True)
for fn in tqdm(files, desc="Processing stations"):
    df = pd.read_fwf(fn, widths=widths, names=col_names, dtype=str)
    
    #Keep only TMAX/TMIN/PRCP and YEAR>=2000
    df = df[df['ELEMENT'].isin(['TMAX','TMIN','PRCP'])]
    df['YEAR'] = df['YEAR'].astype(int)
    df = df[df['YEAR'] >= 2000]
    
    #Long: one row per day per element
    value_cols = [f"VALUE_{i}" for i in range(1,32)]
    df_long = df.melt(
        id_vars=['ID','YEAR','MONTH','ELEMENT'],
        value_vars=value_cols,
        var_name='DAY',
        value_name='VALUE'
    )
    df_long['DAY']   = df_long['DAY'].str.replace('VALUE_','').astype(int)
    df_long['VALUE'] = pd.to_numeric(df_long['VALUE'], errors='coerce')
    
    #Filter out missing placeholders
    df_long = df_long[df_long['VALUE'].notna() & (df_long['VALUE'] != -9999)]
    
    #Date column
    df_long['date'] = pd.to_datetime(dict(
        year  = df_long['YEAR'],
        month = df_long['MONTH'],
        day   = df_long['DAY']
    ), errors='coerce')
    df_long = df_long[df_long['date'].notna()]
    
    #Pivot back to wide: one row per (ID, date)
    df_wide = df_long.pivot_table(
        index=['ID','date'],
        columns='ELEMENT',
        values='VALUE',
        aggfunc='first'
    ).reset_index()
    for col in ('TMAX','TMIN','PRCP'):
        if col not in df_wide.columns:
            df_wide[col] = None
    df_wide = df_wide[['ID','date','TMAX','TMIN','PRCP']]
    
    #Insert into DuckDB
    con.register('tmp_weather', df_wide)
    con.execute("INSERT INTO weather SELECT * FROM tmp_weather")
    con.unregister('tmp_weather')

#Create an index on date for faster aggregates
con.execute("CREATE INDEX IF NOT EXISTS idx_weather_date ON weather(date)")

con.close()
