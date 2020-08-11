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
#install.packages("dplyr")

library(rgbif)
library(Taxonstand)
library(CoordinateCleaner)
library(maps)
library(dplyr)
library(devtools)

#devtools::install_github("saramortara/rocc", force = TRUE)
library(rocc)


# Read the spreadsheet with the names of all your species (should be located in the "data" folder).
Sptable <- read.csv("./data/Species.csv")
# Create a vector with the names
scientificName <- as.vector(Sptable$Species)
#Create a different folder inside results for clean tables (keep it organized)
dir.create("./results/clean_data")

#start the data gathering & cleaning
for (a in 1:length(scientificName)) {
  message("starting the search for ", paste0(scientificName[a]))
## get records from SpeciesLink and save ir to the "results" directory (the creation of the results directory is part of the rspeciesLink function)
r <- rspeciesLink(filename = paste0("raw_data_SpeciesLink_",scientificName[a]), scientificName = scientificName[a], Coordinates = "Yes")

#Checking if the output is empty (no occurrence data available in SpeciesLink for this species. Will work with GBIF only)
if (is.null(dim(r$data))) {
  #Get occurence points from GBIF
  gbif_input <- occ_search(scientificName = scientificName[a], hasCoordinate = TRUE,
                           return = "data")

  if (is.null(dim(gbif_input))) {
    message("Species has no points of occurence")} else {

  message("SpeciesLink is empty. Working with GBIF only.")

  #Save it as a csv file
  file_path2 <- file.path("./results", paste0("raw_data_GBIF_",scientificName[a], ".csv"))
  write.csv(gbif_input, file_path2, row.names = FALSE)
  gbif <- read.csv(file_path2)
  #View(gbif)

  # 2. Checking species taxonomy

  #Let's check the unique entries for the species name we just searched.

  ########TAXONOMIC CLEANING

  ##Check status - function by Sara Mortara
  check_status_gbif <- check_string(scientificName = gbif$scientificName)
  head(check_status_gbif)
  gbif$scientificStatus <- check_status_gbif$scientificName_status
  gbif$scientificName <- check_status_gbif$scientificName_new

  ##WORK ON THIS
  ##Here the script needs to replace name only IF NAME IS VALID (e.g. if "indet" > discart ) ## TO DO // need to see if other type of status is not valid!
  gbif <- subset(gbif, gbif$scientificStatus != "indet")

  #Get onlye species names, lat, long and ID. Rename columns of the two datasets to be the same. Add a column that provides info about data source.
  gbif_table <- data.frame(gbif$key, gbif$scientificName, gbif$decimalLatitude, gbif$decimalLongitude, gbif$scientificStatus, gbif$order, gbif$country, gbif$stateProvince, gbif$locality)

  gbif_table$DataSource <- "GBIF"
  names(gbif_table)[names(gbif_table) == "gbif.key"] <- "ID"
  names(gbif_table)[names(gbif_table) == "gbif.scientificName"] <- "species"
  names(gbif_table)[names(gbif_table) == "gbif.decimalLatitude"] <- "decimalLatitude"
  names(gbif_table)[names(gbif_table) == "gbif.decimalLongitude"] <- "decimalLongitude"
  names(gbif_table)[names(gbif_table) == "gbif.scientificStatus"] <- "scientificStatus"
  names(gbif_table)[names(gbif_table) == "gbif.order"] <- "Order"
  names(gbif_table)[names(gbif_table) == "gbif.country"] <- "Country"
  names(gbif_table)[names(gbif_table) == "gbif.stateProvince"] <- "stateProvince"
  names(gbif_table)[names(gbif_table) == "gbif.locality"] <- "locality"

  # output w/ only potential correct coordinates
  merge <- bind_rows(gbif_table)
  # Cleaning data ------------------------------------
  #Clean NAs
gbif_table_coord <- merge[!is.na(merge$decimalLatitude) & !is.na(merge$decimalLongitude),]
  #Set rownames to NULL so it does not get errors in geo_clean function
rownames(gbif_table_coord) <- NULL
  #nrow(gbif_table_coord)
  #View(gbif_table_coord)

#In case geo_clean finds specific rows with errors, use this to clean it. Then continue manually.
#gbif_table_coord <- gbif_table_coord[-c(row1, row2, row1...), ]

  #Now we will use the the function `clean_coordinates()` from the `CoordinateCleaner` package to clean the species records. This function checks for common errors in coordinates such as institutional coordinates, sea coordinates, outliers, zeros, centroids, etc. This function does not accept not available information (here addressed as "NA") so we will first select only data that have a numerical value for both latitude and longitude.

  #Note: at this moment having a specific ID code for each observation is essential. The raw data already provides an ID in the column `gbifID`.

  geo.clean <- clean_coordinates(x = gbif_table_coord,
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

  #############################################
  ########### Important: TO DO

  ##Check if species has more than 20 occurences in total after cleaning. If not, than it can't be used in ENM and should not be save (Stockwell & Peterson, 2002).

  #############################################
  message("writing final output file")
  ### If is has more than 20 occurences, than keep it and sabe it to a csv file.
  write.csv(geo.clean2,
            paste0("./results/clean_data/clean_data_",scientificName[a],".csv"),
            row.names = FALSE)

} } else {

#read the above table as csv to work on it
file_path <- file.path("./results", paste0("raw_data_SpeciesLink_",scientificName[a], ".csv"))
spLink <- read.csv(file_path)
#View(spLink)

#Get occurence points from GBIF
gbif_input <- occ_search(scientificName = scientificName[a], hasCoordinate = TRUE,
                   return = "data")

if (is.null(dim(gbif_input))) {
  ## ESSA PARTE AQUI ESTÁ DANDO ERRO

  message("Gbif is empty. Working with SpeciesLink only.")

  check_status_spLink <- check_string(scientificName = spLink$scientificName)
  #head(check_status_spLink)
  spLink$scientificName <- check_status_spLink$scientificName_new
  spLink$scientificStatus <- check_status_spLink$scientificName_status
  spLink <- subset(spLink, spLink$scientificStatus != "indet")

  #Get onlye species names, lat, long and ID. Rename columns of the two datasets to be the same. Add a column that provides info about data source.

  splink_table <- data.frame(spLink$record_id,spLink$scientificName, spLink$decimalLatitude, spLink$decimalLongitude, spLink$scientificStatus, spLink$order, spLink$country, spLink$stateProvince, spLink$locality)

  splink_table$DataSource <- "SpeciesLink"
  names(splink_table)[names(splink_table) == "spLink.record_id"] <- "ID"
  names(splink_table)[names(splink_table) == "spLink.scientificName"] <- "species"
  names(splink_table)[names(splink_table) == "spLink.decimalLatitude"] <- "decimalLatitude"
  names(splink_table)[names(splink_table) == "spLink.decimalLongitude"] <- "decimalLongitude"
  names(splink_table)[names(splink_table) == "spLink.scientificStatus"] <- "scientificStatus"
  names(splink_table)[names(splink_table) == "spLink.order"] <- "Order"
  names(splink_table)[names(splink_table) == "spLink.country"] <- "Country"
  names(splink_table)[names(splink_table) == "spLink.stateProvince"] <- "stateProvince"
  names(splink_table)[names(splink_table) == "spLink.locality"] <- "locality"

    # output w/ only potential correct coordinates
  merge <- bind_rows(splink_table)
  # Cleaning data -------------------------------------

  #Clean NAs
  splink_table_coord <- merge[!is.na(splink_table$decimalLatitude) & !is.na(splink_table$decimalLongitude),]
  #Set rownames to NULL so it does not get errors in geo_clean function
  rownames(splink_table_coord) <- NULL

  #In case geo_clean finds specific rows with errors, use this to clean it. Then continue manually.
  #merge_coord <- merge_coord[-c(row1, row2, row1...), ]

  #nrow(splink_table_coord)
  #View(splink_table_coord)

  #Now we will use the the function `clean_coordinates()` from the `CoordinateCleaner` package to clean the species records. This function checks for common errors in coordinates such as institutional coordinates, sea coordinates, outliers, zeros, centroids, etc. This function does not accept not available information (here addressed as "NA") so we will first select only data that have a numerical value for both latitude and longitude.

  #Note: at this moment having a specific ID code for each observation is essential. The raw data already provides an ID in the column `gbifID`.

  geo.clean <- clean_coordinates(x = splink_table_coord,
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


  #############################################
  ########### Important: TO DO
  ##Check if species has more than 20 occurences in total after cleaning. If not, than it can't be used in ENM and should not be save (Stockwell & Peterson, 2002).
  #############################################
  message("writing final output file")
  ### If is has more than 20 occurences, than keep it and sabe it to a csv file.
  write.csv(geo.clean2,
            paste0("./results/clean_data/clean_data_",scientificName[a],".csv"),
            row.names = FALSE)


  } else {

message("GBIF & SeciesLink ok. Starting to clean")

#Save it as a csv file
file_path2 <- file.path("./results", paste0("raw_data_GBIF_",scientificName[a], ".csv"))
write.csv(gbif_input, file_path2, row.names = FALSE)
gbif <- read.csv(file_path2)
#View(gbif)

# 2. Checking species taxonomy

#Let's check the unique entries for the species name we just searched.

########TAXONOMIC CLEANING

##Check status - function by Sara Mortara

check_status_gbif <- check_string(scientificName = gbif$scientificName)
#head(check_status_gbif)
gbif$scientificStatus <- check_status_gbif$scientificName_status
gbif$scientificName <- check_status_gbif$scientificName_new

##WORK ON THIS
##Here the script needs to replace name only IF NAME IS VALID (e.g. if "indet" > discart )
gbif <- subset(gbif, gbif$scientificStatus != "indet")

check_status_spLink <- check_string(scientificName = spLink$scientificName)
#head(check_status_spLink)
spLink$scientificName <- check_status_spLink$scientificName_new
spLink$scientificStatus <- check_status_spLink$scientificName_status
spLink <- subset(spLink, spLink$scientificStatus != "indet")

#nrow(spLink)
#nrow(gbif)
#View(gbif)
#View(spLink)

#Merge the two tables
#Get onlye species names, lat, long and ID. Rename columns of the two datasets to be the same. Add a column that provides info about data source.

splink_table <- data.frame(spLink$record_id,spLink$scientificName, spLink$decimalLatitude, spLink$decimalLongitude, spLink$scientificStatus, spLink$order, spLink$country, spLink$stateProvince, spLink$locality)

splink_table$DataSource <- "SpeciesLink"
names(splink_table)[names(splink_table) == "spLink.record_id"] <- "ID"
names(splink_table)[names(splink_table) == "spLink.scientificName"] <- "species"
names(splink_table)[names(splink_table) == "spLink.decimalLatitude"] <- "decimalLatitude"
names(splink_table)[names(splink_table) == "spLink.decimalLongitude"] <- "decimalLongitude"
names(splink_table)[names(splink_table) == "spLink.scientificStatus"] <- "scientificStatus"
names(splink_table)[names(splink_table) == "spLink.order"] <- "Order"
names(splink_table)[names(splink_table) == "spLink.country"] <- "Country"
names(splink_table)[names(splink_table) == "spLink.stateProvince"] <- "stateProvince"
names(splink_table)[names(splink_table) == "spLink.locality"] <- "locality"

gbif_table <- data.frame(gbif$key, gbif$scientificName, gbif$decimalLatitude, gbif$decimalLongitude, gbif$scientificStatus, gbif$order, gbif$country, gbif$stateProvince, gbif$locality)

gbif_table$DataSource <- "GBIF"
names(gbif_table)[names(gbif_table) == "gbif.key"] <- "ID"
names(gbif_table)[names(gbif_table) == "gbif.scientificName"] <- "species"
names(gbif_table)[names(gbif_table) == "gbif.decimalLatitude"] <- "decimalLatitude"
names(gbif_table)[names(gbif_table) == "gbif.decimalLongitude"] <- "decimalLongitude"
names(gbif_table)[names(gbif_table) == "gbif.scientificStatus"] <- "scientificStatus"
names(gbif_table)[names(gbif_table) == "gbif.order"] <- "Order"
names(gbif_table)[names(gbif_table) == "gbif.country"] <- "Country"
names(gbif_table)[names(gbif_table) == "gbif.stateProvince"] <- "stateProvince"
names(gbif_table)[names(gbif_table) == "gbif.locality"] <- "locality"

#merge the two datasets to clean them together
merge <- bind_rows(splink_table, gbif_table)
#View(merge)
#file_path3 <- file.path("./results", paste0("merged_",scientificName[a], ".csv"))
#write.csv(merge, file_path3, row.names = FALSE)

# output w/ only potential correct coordinates

# Cleaning data -------------------------------------

#Clean NAs
merge_coord <- merge[!is.na(merge$decimalLatitude) & !is.na(merge$decimalLongitude),]
#Set rownames to NULL so it does not get errors in geo_clean function
rownames(merge_coord) <- NULL

#In case geo_clean finds specific rows with errors, use this to clean it. Then continue manually.
#merge_coord <- merge_coord[-c(row1, row2, row1...), ]

#nrow(merge_coord)
#View(merge_coord)

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


#############################################
########### Important: TO DO
##Check if species has more than 20 occurences in total after cleaning. If not, than it can't be used in ENM and should not be save (Stockwell & Peterson, 2002).
#############################################

### If is has more than 20 occurences, than keep it and sabe it to a csv file.
message("writing final output file")
write.csv(geo.clean2,
          paste0("./results/clean_data/clean_data_",scientificName[a],".csv"),
          row.names = FALSE)
}
}
}
