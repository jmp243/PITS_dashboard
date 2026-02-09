
# PITS_dashboard

attempt to build a pits dashboard using quarto and posit cloud connect.
Posit cloud maxes out on free account. R shiny app created using pits
attempts dashboard script.

Use the PITS_suspensions_script.qmd to wrangle the data.

The data range is from January 1, 2023 to December 24, 2025 from a
mid-sized county library system.

the descriptions are all in English.

The Tabs_UI.R is the most up to date file for the shiny app. 
PITS_suspensions_script.qmd is the latest iteration of data wrangling. 

Title of dataset

Name/institution/contact information for: PCPL, Wichita State University
Jung Mee Park Data manager File name structure

.R files for code .qmd for data wrangling scripts

Structure: Provide the template you are using for your filenames
Attributes: Describe the attributes used to name the files Codes:
Provide a complete list of any codes/abbreviations used Provide examples
of the above items File formats

Provide a list of all file formats present in this dataset. If you need
to convert or migrate your data files from one format to another, be
aware of the potential risk of the loss or corruption of your data and
take appropriate steps to avoid/minimize it File Format Examples:
Databases: XML, CSV Geospatial: SHP, DBF, GeoTIFF, NetCDF Moving Images:
MOV, MPEG, AVI, MXF Audio: WAVE, AIFF, MP3, MXF Numbers/statistics:
ASCII, DTA, POR, SAS, SAV\Images: TIFF, JPEG 2000, PDF, PNG, GIF, BMP
Text: PDF/A, HTML, ASCII, XML, UTF-8 Graphs: JSON, YAML, XML Column
headings for tabular data

For tabular data, list and define column headings: Units of measurement
Data formats, such as YYYY/MM/DD Calculations Versioning: Establish a
procedure for documenting changes in files. One option is to create a
changelog in this README file, where every step that will change the
output files is listed.

DATA COLLECTION 
## how to pull data

### from PITS dashboard

only some categories, 

got to report 

Menu>reports> list reports> incidents

- remove PERPETRATOR and REPORTER
- check all the other boxes
- change date scope if necessary> Done
- run report
- export to excel

save as csv in this format - new_PITS_ListIncidents_Jan-Jun24_2025.csv

**Suspension**

remove perpetrator name

add suspension note

Menu> reports> list of suspensions > check Suspension Note> set date> run report

export to EXCEL

renAME in this format - PITS_ListSuspensions2023_june24_2025.csv 

save as csv

### from MyPC


login quistaff

reports> utilisation reports> **Site and Location Usage Report>**

report view > detailed csv

- change date if necessary> specific> change scope> View Report
- do it for one year at a time
- name in this format MyPC_Jun24_2025_siteAndLocationUsage

