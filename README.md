# RIPS: Rapid Intuitive Pathogen Surveillance
RIPS is a dashboard for pathogen surveillance build upon NCBI's Rapid Reports. It helps detect emerging signals from WGS.

The required libraries to run the dashboard are:
```
rvest, httr, Rcurl, dplyr, shiny, data.table, DT, purr, xml2, ggplot2, lubridate, plotly, stringr, tidyr, readr, shinydashboard, rintrojs
```

Usage:  
To get a brief overview of the dashboard click "Press for instructions" button on the bottom right of the dashboard. 

Step 1: In the top right,  select a taxon and date. Click the blue "Run" button to dowload the selected day's data.

If a report exists for the selected day, a grey popup box will show on the bottom right, indicating the progess of the dashboard. 

An ordered horizontal graph will popup on the center box containing all the clusters 
