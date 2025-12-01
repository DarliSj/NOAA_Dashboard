###some absurd values were stored in the .dly files, so we filter them out here

import duckdb

con = duckdb.connect("../data/weather.duckdb")

before = con.execute("SELECT COUNT(*) FROM weather").fetchone()[0]
print(f"Rows before cleaning: {before}")

con.execute("""
DELETE FROM weather
WHERE
    TMAX <  -1000 OR TMAX >  800  OR
    TMIN <  -1000 OR TMIN >  800  OR
    PRCP <      0 OR PRCP > 5000
""")

after = con.execute("SELECT COUNT(*) FROM weather").fetchone()[0]
print(f"Rows after cleaning:  {after}")
print(f"Rows removed:          {before - after}")

con.close()
