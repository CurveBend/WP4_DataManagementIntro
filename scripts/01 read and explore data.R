####Header ####
# Purpose: First excercise in reading and plotting data from an online data source
# Author: Han Olff
# Started: 2025-06-18
# Input: example Google Sheet (run following line to open it)
browseURL("https://docs.google.com/spreadsheets/d/1YDyz1Qw6MfW5C-c6MrdaipZZk8oxJpYPEpaZPvm2LYo/")
# study area: Loliondo (TZ) and Siana-Loita (KE)
# Output: figures with plots of the data for frequency and abundance of species 
# Requirements: see file renv.lock


####Section: Restore environment and load libraries ####
# restore your library versions to be the same as your collaborators
renv::restore() 
# Load necessary libraries
library(here) # to find the root folder for the project
library(tidyverse) # including libraries as ggplot, dplyr and readr
# create a folder for output figures (but note that this is ignored by git for syncing)
if (!dir.exists("figures")) dir.create("figures")



####Section:  Read the data from Google Sheets ####
# We use the  database as an example that is located in the following Google Drive: 
# Proj Curvebend/5 Curvebend Data/5.4 Data WP4 CS 
# go to drive.google.com and find it under 'shared drives
# You can create a URL for each data table when you click on "Share / Publish to Web" in Google Sheets
options(timeout = 300)  # increase the timeout for readying online files
#  read the list of tables in  the database
MetTables_link<-"https://docs.google.com/spreadsheets/d/e/2PACX-1vTPj8YQWWdG1GeIB0lfDhMZ0nDbIFt-AzQ7tscuOh8SjHiRZof49Q-JEkgRNDAgYciSO60kc-VLLpZZ/pub?gid=1387882554&single=true&output=csv"
MetTables<-readr::read_csv(MetTables_link,show_col_types = F) 
MetTables
# read the species list, of all potential species (dimension table)
DimSpecies<-readr::read_csv(MetTables$CSV_link[MetTables$data_table == "DimSpecies"],show_col_types = F) 
# read the transects list (dimension table)
DimTransect<-readr::read_csv(MetTables$CSV_link[MetTables$data_table == "DimTransect"],show_col_types = F) 
# read the section list (dimension table)
DimSection<-readr::read_csv(MetTables$CSV_link[MetTables$data_table == "DimSection"],show_col_types = F) 
# read the animal observations on sections of transects (fact table)
FactSectionAnimals<-readr::read_csv(MetTables$CSV_link[MetTables$data_table == "FactSectionAnimals"],show_col_types = F) 


####Section: Merge and Query the datasets ####
# using join, filter, mutate and select the data  using dplyr in a pipe  
AllData<-dplyr::left_join(FactSectionAnimals, DimSection, by="Section_ID") |>
  dplyr::left_join(DimTransect, by="Transect_ID") |>
  dplyr::left_join(DimSpecies, by="SpCode2") |>
  dplyr::filter(studyarea=="Loliondo Plains") |>
  dplyr::mutate(TotalCount=CountLeft+ CountRight) |>
  dplyr::select(FirstDate, studyarea,Transect_ID, Section_ID, Name_eng, Domestication,TotalCount)
# check the data
AllData


####Section: Plot the results ####
### Make a histogram of the frequency of observation different animal species in the dataset 
FigScript01_SpeciesFrequency<-ggplot(AllData, 
                                      aes(x = forcats::fct_infreq(Name_eng), fill=Domestication)) +
  geom_bar(stat = "count", color = "black") +
  coord_flip() +
  labs(title = "Frequency of occurrence",
       x = "Animal species",
       y = "Frequency")
FigScript01_SpeciesFrequency
# Save the figure to a png file (or pdf)
ggsave(filename=here::here("figures","FigScript01_SpeciesFrequency.png"),plot=FigScript01_SpeciesFrequency,
       width=1920, height=1200, units='px')

### Make a boxplot of the abundance per section of different species in the dataset 
# Grouped boxplot
FigScript01_SpeciesAbundance<-ggplot(AllData, aes(x = Name_eng, y=TotalCount, fill=Domestication)) +
  geom_boxplot() +
  labs(title = "Loliondo Plains", x = "Species", y = "count per section") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
FigScript01_SpeciesAbundance
# Save the figure to a png file (or pdf)
ggsave(filename=here::here("figures","FigScript01_SpeciesAbundance.png"),plot=FigScript01_SpeciesAbundance,
       width=1920, height=1200, units='px')

