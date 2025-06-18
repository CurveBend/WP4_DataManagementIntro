# ---- 01 Header ----------------------------------------------------------
# Purpose: First excercise in reading and plotting data from an online data source
# Author: Han Olff
# Started: 2025-06-18
# Input: Curvebend full proposal
browseURL("https://drive.google.com/file/d/1QzkCYul01leMi-z8WqIHOR0jP2pvVCMl/view?usp=drive_link")
# Output: figures for frequency of occurrence and abundance, and nmds for community structure 
# Requirements: R 4.4.1, further see file renv.lock for library versions


# ---- 02 Restore environment and load libraries --------------------------
# restore your library versions to be the same as your collaborators
renv::restore() 
# Load necessary libraries
library(here) # to find the root folder for the project
library(tidyverse) # including libraries as ggplot, dplyr and readr
library(vegan) # for multivariate analysis of community data

# create a folder for output figures for this script (but note that this is ignored by git for syncing)
if (!dir.exists("figures/01")) dir.create("figures/01")


# ---- 03 Read the data from Google Sheets --------------------------------
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



# ---- 04: Merge and Query the data to compile the dataset -----------------------------------
# using join, filter, mutate and select the data  using dplyr in a pipe  
AllData<-dplyr::left_join(FactSectionAnimals, DimSection, by="Section_ID") |>
  dplyr::left_join(DimTransect, by="Transect_ID") |>
  dplyr::left_join(DimSpecies, by="SpCode2") |>
  dplyr::filter(studyarea=="Loliondo Plains",AdultJuvenile=="ad",Name_eng!="impala") |>
  dplyr::mutate(TotalCount=CountLeft+ CountRight) |>
  dplyr::select(FirstDate, studyarea,Transect_ID, Section_ID, Name_eng, Domestication,TotalCount)
# check the data
unique(AllData$Name_eng)

# ---- 05: Plot the results of abundance and frequency  -----------------------------------------------
# Make a histogram of the frequency of observation different animal species in the dataset 
FigScript01_SpeciesFrequency<-ggplot(AllData, 
                                      aes(x = forcats::fct_infreq(Name_eng), fill=Domestication)) +
  geom_bar(stat = "count", color = "black") +
  coord_flip() +
  labs(title = "Frequency of occurrence",
       x = "Animal species",
       y = "Frequency")
FigScript01_SpeciesFrequency
# Save the figure to a png file (or pdf)
ggsave(filename=here::here("figures/01","FigScript01_SpeciesFrequency.png"),plot=FigScript01_SpeciesFrequency,
       width=1920, height=1200, units='px')

# Make a boxplot of the abundance per section of different species in the dataset 
# Grouped boxplot
FigScript01_SpeciesAbundance<-ggplot(AllData, aes(x = Name_eng, y=TotalCount, fill=Domestication)) +
  geom_boxplot() +
  labs(title = "Loliondo Plains", x = "Species", y = "count per section") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
FigScript01_SpeciesAbundance
# Save the figure to a png file (or pdf)
ggsave(filename=here::here("figures/01","FigScript01_SpeciesAbundance.png"),plot=FigScript01_SpeciesAbundance,
       width=1920, height=1200, units='px')


# ---- 06: NMDS ordination of species composition --------------------------

# Prepare community matrix: rows = sections (samples), columns = species,
# entries = total counts per section


com_matrix <- AllData |> select(Section_ID,Name_eng,TotalCount,FirstDate) |>
  dplyr::filter(FirstDate=="2-Feb-2023") |>
  pivot_wider(
    id_cols = Section_ID,
    names_from = Name_eng,
    values_from = TotalCount,
    values_fill = 0
  ) |>
  column_to_rownames(var = "Section_ID") |>
  as.matrix()

# Run NMDS with Bray-Curtis distance
set.seed(123)
nmds <- metaMDS(
  com_matrix,
  distance = "bray",
  k = 2,
  trymax = 100
)

# Examine stress
nmds
stressplot(nmds)  # Shepard plot

# Extract NMDS scores for plotting
site_scores <- as.data.frame(scores(nmds, display = "sites"))
site_scores$Section_ID <- rownames(site_scores)
# add metadata (e.g., transect)
site_scores <- left_join(site_scores, AllData %>% distinct(Section_ID, Transect_ID), by = "Section_ID")

# Optional: species scores
species_scores <- as.data.frame(scores(nmds, display = "species"))
species_scores$Species <- rownames(species_scores)

# ---- 06: Plot NMDS using ggplot2 -----------------------------------------

FigScript01_NMDS <- ggplot() +
  geom_point(data = site_scores, aes(NMDS1, NMDS2, color = Transect_ID), size = 3, alpha = .7) +
  geom_text(data = species_scores, aes(NMDS1, NMDS2, label = Species), color = "darkgrey", size = 3, alpha = .8) +
  theme_bw() +
  labs(title = "NMDS of species composition (Bray–Curtis)",
       subtitle = "Section-level observations colored by transect")

FigScript01_NMDS
# Save figure
ggsave("figures/01/FigScript01_NMDS.png", plot = FigScript01_NMDS, width = 7, height = 6, dpi = 300)


