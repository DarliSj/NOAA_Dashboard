How to Run Locally

Prerequisites:

R

DuckDB

Installation:

Clone the repository:

Bash
git clone 
Install required R packages:

R
install.packages(c("shiny", "duckdb", "leaflet", "plotly", "dplyr", "DBI"))
Run .qmd file

R
library(shiny)
runApp("app.R")

Data Source

NOAA Global Historical Climatology Network daily (GHCNd): too big to upload to github
