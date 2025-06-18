###### Read data from an online Google sheets doc into R and make some first plots#########
# Script: Han Olff
# Date: 2025-06-18

#### Check and load libraries ####
# check if the library versions are up to data with other collaborators
renv::restore()
# Load necessary libraries
library(tidyverse)

######## Read data from Google Sheets #########
# We use the  database as an example that is located in the following Google Drive: 
# Proj Curvebend/5 Curvebend Data/5.4 Data WP4 CS 
# go to drive.google.com and find it under 'shared drives
# You can create a URL for each data table when you click on "Share / Publish to Web" in Google Sheets
# increase the timeout for readying online files
options(timeout = 300)  
# read the list of tables in  the database with explanation of contents and link to access each
MetTables_link<-"https://docs.google.com/spreadsheets/d/e/2PACX-1vTPj8YQWWdG1GeIB0lfDhMZ0nDbIFt-AzQ7tscuOh8SjHiRZof49Q-JEkgRNDAgYciSO60kc-VLLpZZ/pub?gid=1387882554&single=true&output=csv"
MetTables<-readr::read_csv(tables_link,show_col_types = F) 
MetTables
# read the species list, of all potential species (dimension table)
DimSpecies<-readr::read_csv(MetTables$CSV_link[MetTables$data_table == "DimSpecies"],show_col_types = F) 
# read the transects list (dimension table)
DimTransect<-readr::read_csv(MetTables$CSV_link[MetTables$data_table == "DimTransect"],show_col_types = F) 
# read the section list (dimension table)
DimSection<-readr::read_csv(MetTables$CSV_link[MetTables$data_table == "DimSection"],show_col_types = F) 
# read the animal observations on sections of transects (fact table)
FactSectionAnimals<-readr::read_csv(MetTables$CSV_link[MetTables$data_table == "FactSectionAnimals"],show_col_types = F) 

#######  query the data using join, filter, mutate and select the data  using dplyr in a pipe  #####
AllData<-dplyr::left_join(FactSectionAnimals, DimSection, by="Section_ID") |>
  dplyr::left_join(DimTransect, by="Transect_ID") |>
  dplyr::left_join(DimSpecies, by="SpCode2") |>
  dplyr::filter(studyarea=="Loliondo Plains") |>
  dplyr::mutate(TotalCount=CountLeft+ CountRight) |>
  dplyr::select(FirstDate, studyarea,Transect_ID, Section_ID, Name_eng, Domestication,TotalCount)
# check the data
AllData

######## Make a histogram of the frequency of observation different animal species in the dataset #########
# make the histogram
FigScript01_SpeciesFrequency<-ggplot(AllData, 
                                      aes(x = forcats::fct_infreq(Name_eng), fill=Domestication)) +
  geom_bar(stat = "count", color = "black") +
  coord_flip() +
  labs(title = "Frequency of occurrence",
       x = "Animal species",
       y = "Frequency")
FigScript01_SpeciesFrequency
# Save the figure to a png file (or pdf)
ggsave(filename="./figures/FigScript01_SpeciesFrequency.png",plot=FigScript01_SpeciesFrequency,
       width=1920, height=1200, units='px')

######## Make a boxplot of the abundance per section of different species in the dataset ######
# Grouped boxplot
FigScript01_SpeciesAbundance<-ggplot(AllData, aes(x = Name_eng, y=TotalCount, fill=Domestication)) +
  geom_boxplot() +
  labs(title = "Boxplot by Species", x = "Species", y = "Value")
FigScript01_SpeciesAbundance
# Save the figure to a png file (or pdf)
ggsave(filename="./figures/FigScript01_SpeciesAbundance.png",plot=FigScript01_SpeciesAbundance,
       width=1920, height=1200, units='px')

