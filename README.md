# RIPS: Rapid Intuitive Pathogen Surveillance
RIPS is a dashboard for pathogen surveillance build upon NCBI's Rapid Reports. It helps detect emerging signals from WGS.

## Setup
The required libraries to run the dashboard are:
```
rvest, httr, Rcurl, dplyr, shiny, data.table, DT, purr, xml2, ggplot2, lubridate, plotly, stringr, tidyr, readr, shinydashboard, shinyWidgets, rintrojs
```
 
In R studio, ensure the above libraries are installed. Run the install.R script to quickly install all libraries.

### **Required**
1) Download the app.R script and the www folder.
Ensure folder www exists and contains the file cfsan_logo_mod.png. This displays the page's graphic header.

2) A free NCBI developer's API key is required for making fast and sustainable metadata calls to NCBI. To apply for an API key see:
https://ncbiinsights.ncbi.nlm.nih.gov/2017/11/02/new-api-keys-for-the-e-utilities/

  Paste your key into line 22 of the app.R file.

Run the app.R file in R studio. 

### Verifying Install:
To verify RIPS is running successfully, click the gear icon on the bottom left ![gear_mod](./Images/gear_mod.png), from the 'Data source' drop down select 'test_set1'. Now click the blue button 'Run/Get report for newdate'![run](./Images/run.png).

## Usage
### Quick tutorial:
To get a brief overview of the dashboard, click the gear icon on the bottom left click "Press for instructions" button on the bottom right of the dashboard. 

### Step-by-Step
**Step 1**: 
In the top right,  select a taxon and date. Click the blue "Run" button to dowload the selected day's data.

If a report exists for the selected day, a grey popup box will show on the bottom right, indicating the progess of the dashboard. 

An ordered horizontal graph will popup on the center box containing all the clusters.

**Step 2**: 
Select a cluster by clicking on it. 

Details of the isolates in the cluster will display in the two seperate table below.

The Focal isolates are the new isolates that triggered the Rapid Report from NCBI.

The Neighbor isolates are isolates closely related to the focal isolates. These are ordered by the difference in age from the focal isolate.These are also ordered by the most recent environmentmal signals followed by the clinicals. 

### Tunable Filters
On the right side of the under the inputs, there are four main filters that operate simultaneously. These allow you to filter the clusters from the previous step. By default, the filter is set off to "No Filter". You can change the value by navigating to the filter and pressing the up/down arrows or by manually entering a new value. 

Each taxon has its own preset filter definitions and change when the you press the "Run/Get report for new date" button. 

The default presets are:
- Listeria:
  - Min Number of Clinicals: 3
  - Min Number of Env/other: 1
  - Max days from focal submission: 150
  - Max Allele Difference: 7
- Salmonella:
  - Min Number of Clinicals: 7
  - Min Number of Env/other: 1
  - Max days from focal submission: 90
  - Max Allele Difference: 10
- Ecoli:
  - Min Number of Clinicals: 5
  - Min Number of Env/other: 1
  - Max days from focal submission: 90
  - Max Allele Difference: 10

Example: For Listeria, the default filter preset mean, show all clusters that contain at least 1 environmental/other isolates and at least 3 clinical isolates and has at least one isolate within 150 days from the focal submisson date and has at least one isolate with a max allele difference of 7 alleles from the focal isolates.   

These presets can be changed by altering the values in code under "Taxon filter preset values" comment code section. 

**Filtering Hints**  
  - If you don't see data in horizontal bar graph, expand your filter criteria or quickly select "No Filter" to verify data from the Rapid Report exists. 
  - Reducing the value in the "Min Number of Clinicals" and "Min Number of Env/other" allows more clusters to pass the filter.  
  - Increasing the value of "Max days" and "Max Allele" allows more clusters to pass the filter.


### Links
These are available only after clicking on a cluster in the bar graph.

Blue buttons are clickable links to searches within the NCBI PD browser.

- In the "Focals" table, the links under the column "neighbor_cluster" searches the NCBI PD browser for all isolates within the focal and neighbor tables using the isolate's PDT accesion number. Once at NCBI, click the link under "Matched Clusters> SNP cluster" to view the phlyogenetic tree of all matched isolates. 

- In the "Neighbors" table these are the columns that contain links:
  - "neighbor_biosample_acc": links to the isolates NCBI's BioSample page containing all the metadata.
  - "neighbor_isolate" : Links to the NCBI PD browser search for that isolate's PDT accession number. You might need to use the horzontal scroll bar to view this.

### Data tabs





