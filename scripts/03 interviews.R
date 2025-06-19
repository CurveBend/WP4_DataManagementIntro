# ---- 01 Header ----------------------------------------------------------
# Purpose: Read and analyse coded interview data
# Author: Han Olff
# Started: 2025-06-19
# Input: 2025_interviews (Google sheets database)
# Input shared drive: Data Curvebend/WP4CS/2025_DataManagementIntro 
# Input url: 
browseURL("https://docs.google.com/spreadsheets/d/1hvSNGoxbLve4rrKM_pNMNRgggAnWzt4_3RaBvrctRBc/edit?gid=0#gid=0")
# Output: figures for frequency coded answers 
# Requirements: R 4.4.1, further see file renv.lock for library versions


# ---- 02 Restore environment and load libraries --------------------------
# restore your library versions to be the same as your collaborators
renv::restore() 
# Load necessary libraries
library(here) # to find the root folder for the project
library(tidyverse) # including libraries as ggplot, dplyr and readr
library(knitr)

# create a folder for output figures for this script (but note that this is ignored by git for syncing)
if (!dir.exists("figures")) dir.create("figures")
if (!dir.exists("figures/01")) dir.create("figures/01")


# ---- 03 Read the data from Google Sheets --------------------------------
options(timeout = 300)  # increase the timeout for readying online files
#  read the list of tables in  the database
MetTables_link<-"https://docs.google.com/spreadsheets/d/e/2PACX-1vRjv0h7adkomA-Z0oNSNVtZMtXdzdAoJI-RRSIGJBTFDWDkUnuVQ7YIp17o7DuZ0ShAJzsEFa5EyIku/pub?gid=1387882554&single=true&output=csv"
MetTables<-readr::read_csv(MetTables_link,show_col_types = F) 
MetTables
# read all other tables
MetVariables<-readr::read_csv(MetTables$CSV_link[MetTables$data_table == "MetVariables"],show_col_types = F) 
MetVariables
DimRespondentsConf<-readr::read_csv(MetTables$CSV_link[MetTables$data_table == "DimRespondentsConf"],show_col_types = F) 
DimRespondentsPub<-readr::read_csv(MetTables$CSV_link[MetTables$data_table == "DimRespondentsPub"],show_col_types = F) 
DimQuestions<-readr::read_csv(MetTables$CSV_link[MetTables$data_table == "DimQuestions"],show_col_types = F) 
DimCodes<-readr::read_csv(MetTables$CSV_link[MetTables$data_table == "DimCodes"],show_col_types = F) 
FactResponses <-readr::read_csv(MetTables$CSV_link[MetTables$data_table == "FactResponses"],show_col_types = F) 
FactResponses
FactCoding <-readr::read_csv(MetTables$CSV_link[MetTables$data_table == "FactCoding"],show_col_types = F) 
FactCoding


# ---- 04: Merge and Query the data to compile the dataset -----------------------------------
# using join, filter, mutate and select the data  using dplyr in a pipe  
AllData<-dplyr::left_join(FactCoding, FactResponses, by="Response_ID") |>
  dplyr::left_join(DimRespondentsPub, by="Respondent_ID") |>
  dplyr::left_join(DimRespondentsConf, by="Respondent_ID") |>
  dplyr::filter(Question_ID=="Q01_ClimateImpact") |>
  dplyr::select(!Response_txt)
# check the data
AllData

# make a tables of frequency of coded answers per gender
# simple tables
table(AllData$Code_ID,AllData$Gender)
table(AllData$Code_ID,AllData$Village)

# Nicer table
# Reorder Code_ID by frequency
AllData <- AllData %>%
  mutate(Code_ID = fct_infreq(Code_ID))

# Create count table
count_table <- AllData %>%
  count(Code_ID, Gender) %>%
  group_by(Code_ID) %>%
  mutate(
    pct = n / sum(n),
    label = str_glue("{n} ({round(pct * 100)}%)")
  ) %>%
  ungroup() %>%
  dplyr::select(Code_ID, Gender, label) %>%
  pivot_wider(names_from = Gender, values_from = label, values_fill = "0 (0%)")

# Show as simple table
knitr::kable(count_table, caption = DimQuestions$Question[DimQuestions$Question_ID == "Q01_ClimateImpact"])
