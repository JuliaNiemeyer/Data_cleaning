


#devtools::install_github("saramortara/rocc", force = TRUE)
library(rocc)


# Read the spreadsheet with the names of all your species (should be located in the "data" folder).
Sptable <- read.csv("./data/Peixes.csv")
# Create a vector with the names
scientificName <- as.vector(Sptable$Species)
#Create a different folder inside results for clean tables (keep it organized)
#dir.create("./results/clean_data2")

#start the data gathering & cleaning
for (a in 1:length(scientificName)) {
  ## get records from SpeciesLink and save ir to the "results" directory (the creation of the results directory is part of the rspeciesLink function)
  #file_path <- file.path("D:/RedeCLIMA/model_inputs/Biodiversidade/Repteis/clean_data", paste0("clean_data_",scientificName[a], ".csv"))

  file_path <- file.path("D:/RedeCLIMA/model_inputs/Biodiversidade/Peixes/clean_data", paste0("clean_data_",scientificName[a], ".csv"))

  table <- read.csv(file_path)

  ##Check status - function by Sara Mortara
  check_status_gbif <- check_string(scientificName = table$species)
  #head(check_status_gbif)
  table$scientificStatus <- check_status_gbif$scientificName_status

  table$species <- check_status_gbif$scientificName_new
  ##Here the script needs to replace name only IF NAME IS VALID (e.g. if "indet" > discart ) ## TO DO // need to see if other type of status is not valid!
  table <- subset(table, table$scientificStatus != "indet")

  #Get onlye species names, lat, long and ID. Rename columns of the two datasets to be the same. Add a column that provides info about data source.
  final_table <- data.frame(table$ID, table$species, table$decimalLatitude, table$decimalLongitude, table$scientificStatus, table$DataSource)


  message("writing final output")
  write.csv(final_table,
            paste0("./results/clean_data/clean_data_",scientificName[a],".csv"),
            row.names = FALSE)
}
