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
Sptable <- read.csv("./Aves/Sp_Aves_13_07_2020.csv")
# Create a vector with the names
scientificName <- as.vector(Sptable$species)
#Create a different folder inside results for clean tables (keep it organized)
#dir.create("./results/clean_data2")

#start the data gathering & cleaning
for (a in 1:length(scientificName)) {
  ## get records from SpeciesLink and save ir to the "results" directory (the creation of the results directory is part of the rspeciesLink function)
  file_path <- file.path("./Aves/Clean_data", paste0("clean_data2_",scientificName[a], ".csv"))

  #file_path <- file.path("D:/RedeCLIMA/model_inputs/Biodiversidade/Data_cleaning/results/clean_data", paste0("clean_data_",scientificName[a], ".csv"))

  table <- read.csv(file_path)
  table <- table[!is.na(table$decimalLatitude) & !is.na(table$decimalLongitude),]

        #Note: at this moment having a specific ID code for each observation is essential. The raw data already provides an ID in the column `gbifID`.

        geo.clean <- clean_coordinates(x = table,
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
        message("writing final output")
        write.csv(geo.clean2,
                  paste0("./results/clean_data/clean_data2_",scientificName[a],".csv"),
                  row.names = FALSE)
}
