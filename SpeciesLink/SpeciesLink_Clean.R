######################

#title: "Basic workflow for getting occurences from SpeciesLink and biodiversity data cleaning using R"
#author: "Julia Niemeyer"
#date: April 2020

######################

##Start by loading packages. If you don't have them you should install them frist
#install.packages("rgbif")
#install.packages("Taxonstand")
#install.packages("CoordinateCleaner")
#install.packages("maps")
#install.packages("devtools")

library(rgbif)
library(Taxonstand)
library(CoordinateCleaner)
library(maps)
library(dplyr)
library(devtools)

devtools::install_github("saramortara/rocc", force = TRUE)
library(rocc)


## Then run rspeciesLink function in SpeciesLink_function.R

##TEM COMO COLOCAR UMA CHAMADA PRA OUTRO SCRIPT AQUI DENTRO PRA RODAR AUTOMATICO?

######## WORK ON IT


# Read the spreadsheet with the names of all your species (should be located in the "data" folder).
Sptable <- read.csv("./data/Species_ex.csv")
scientificName <- as.vector(Sptable$Species)


for (a in 1:length(scientificName)) {
## get records from SpeciesLink and save ir to the "results" directory (the creation of the results directory is part of the rspeciesLink function)
rspeciesLink(filename = paste0("raw_data_SpeciesLink_",scientificName[a]), scientificName = scientificName[a], Coordinates = "Yes")

#read the above table as csv to work on it
file_path <- file.path("./results", paste0("raw_data_SpeciesLink_",scientificName[a], ".csv"))
spLink <- read.csv(file_path)
#View(spLink)

#Get occurence points from GBIF
gbif_input <- occ_search(scientificName = scientificName[a],
                   return = "data")

#Save it as a csv file
file_path2 <- file.path("./results", paste0("raw_data_GBIF_",scientificName[a], ".csv"))
write.csv(gbif_input, file_path2, row.names = FALSE)
gbif <- read.csv(file_path2)


# 2. Checking species taxonomy

#Let's check the unique entries for the species name we just searched.

########TAXONOMIC CLEANING

##Check status - function by Sara Mortara - ESTÁ DANDO PROBLEMA
check_status_gbif <- check_status(scientificName = gbif$scientificName)
head(check_status_gbif)

check_status_spLink <- check_status(scientificName = spLink$scientificName)
head(check_status_spLink)

##Check taxonomy - Function by sara Mortara




#nrow(spLink)
#nrow(gbif)
#View(gbif)

#Merge the two tables
#Get onlye species names, lat, long and ID. Rename columns of the two datasets to be the same. Add a column that provides info about data source.

splink_table <- data.frame(spLink$record_id,spLink$scientificName, spLink$decimalLatitude, spLink$decimalLongitude)

splink_table$DataSource <- "SpeciesLink"
names(splink_table)[names(splink_table) == "spLink.record_id"] <- "ID"
names(splink_table)[names(splink_table) == "spLink.scientificName"] <- "species"
splink_table$species <- check_status_spLink$scientificName_new
names(splink_table)[names(splink_table) == "spLink.decimalLatitude"] <- "decimalLatitude"
names(splink_table)[names(splink_table) == "spLink.decimalLongitude"] <- "decimalLongitude"

gbif_table <- data.frame(gbif$key, gbif$scientificName, gbif$decimalLatitude, gbif$decimalLongitude)

gbif_table$DataSource <- "GBIF"
names(gbif_table)[names(gbif_table) == "gbif.key"] <- "ID"
names(gbif_table)[names(gbif_table) == "gbif.scientificName"] <- "species"
gbif_table$species <- check_status_gbif$scientificName_new
names(gbif_table)[names(gbif_table) == "gbif.decimalLatitude"] <- "decimalLatitude"
names(gbif_table)[names(gbif_table) == "gbif.decimalLongitude"] <- "decimalLongitude"

#merge the two datasets to clean them together
merge <- bind_rows(splink_table, gbif_table)
#View(merge)
file_path3 <- file.path("./results", paste0("merged_",scientificName[a], ".csv"))
write.csv(merge, file_path3, row.names = FALSE)

# output w/ only potential correct coordinates

# Cleaning data -------------------------------------

#Clean NAs
merge_coord <- merge[!is.na(merge$decimalLatitude) & !is.na(merge$decimalLongitude),]
#nrow(merge_coord)

#Now we will use the the function `clean_coordinates()` from the `CoordinateCleaner` package to clean the species records. This function checks for common errors in coordinates such as institutional coordinates, sea coordinates, outliers, zeros, centroids, etc. This function does not accept not available information (here addressed as "NA") so we will first select only data that have a numerical value for both latitude and longitude.

#Note: at this moment having a specific ID code for each observation is essential. The raw data already provides an ID in the column `gbifID`.

geo.clean <- clean_coordinates(x = merge_coord,
                               lon = "decimalLongitude",
                               lat = "decimalLatitude",
                               species = "species",
                               value = "clean")

#nrow(geo.clean)
#View(geo.clean)
duplicata <- duplicated(geo.clean[, c('decimalLongitude', 'decimalLatitude')])
#which(duplicata)
#sum(duplicata)
geo.clean2 <- geo.clean[!duplicata, ]
#nrow(geo.clean2)


#plotting in maps to see points of occurence (if you want to keep track of what's going on)
#par(mfrow = c(1, 2))
#plot(decimalLatitude ~ decimalLongitude, data = merge)
#map(, , , add = TRUE)
#plot(decimalLatitude ~ decimalLongitude, data = geo.clean2)
#map(, , , add = TRUE)
#par(mfrow = c(1, 1))


write.csv(geo.clean2,
          paste0("./results/clean_data_",scientificName[a],".csv"),
          row.names = FALSE)
}
