######################

#title: "Basic workflow for getting occurences from SpeciesLink and biodiversity data cleaning using R"
#author: "Julia Niemeyer"
#date: April 2020

######################

##Start by loading packages. If you don't have them you should install them frist

#install.packages("Taxonstand")
#install.packages("CoordinateCleaner")
#install.packages("maps")

library(Taxonstand)
library(CoordinateCleaner)
library(maps)

## Then run rspeciesLink function in SpeciesLink_function.R
##TEM COMO COLOCAR UMA CHAMADA PRA OUTRO SCRIPT AQUI DENTRO PRA RODAR AUTOMATICO?

######## WORK ON IT

## PS. A BOA É JUNTAR COM A TABELA DO GBIF ANTES.

# Read the spreadsheet with the names of all your species (should be located in the "data" folder).
Sptable <- read.csv("./data/Species_ex.csv")
scientificName <- as.vector(Sptable$Species)

for (a in 1:length(scientificName)) {
## get records from SpeciesLink and save ir to the "results" directory (the creation of the results directory is part of the rspeciesLink function)
spLink <- rspeciesLink(filename = paste0("raw_data_",scientificName[a]), scientificName = scientificName[a], Coordinates = "Yes")

#read the above table as csv to work on it
file_path <- file.path("./results", paste0("raw_data_",scientificName[a], ".csv"))
table <- read.csv(file_path)

#nrow(table)
#View(table)

#Clean NAs
splink.coord <- table[!is.na(table$decimalLatitude) & !is.na(table$decimalLongitude),]
#nrow(splink.coord)
#View(splink.coord)


#Now we will use the the function `clean_coordinates()` from the `CoordinateCleaner` package to clean the species records. This function checks for common errors in coordinates such as institutional coordinates, sea coordinates, outliers, zeros, centroids, etc. This function does not accept not available information (here addressed as "NA") so we will first select only data that have a numerical value for both latitude and longitude.

#Note: at this moment having a specific ID code for each observation is essential. The raw data already provides an ID in the column `gbifID`.

# output w/ only potential correct coordinates

# Cleaning data -------------------------------------
##TA UMA MERDA ISSO AQUI CONSERTAR!!

#Split the table in x tables: in which x is the the number of scpecies in our table.
#splitData <- split(splink.coord, splink.coord$scientificName)
#geo.clean <- splitData
#unique <- unique(splink.coord$scientificName)
#data.name <- splitData

#Do a loop to clean each splited table separately

#####PERGUNTA: tirar pontos por "locality" == zoologico etc?
#sp_name <- paste0("splitData$", splink.coord$scientificName[a], "`")

#View(splitData)
geo.clean <- clean_coordinates(x = splink.coord,
                               lon = "decimalLongitude",
                               lat = "decimalLatitude",
                               species = "scientificName",
                               value = "clean")
#nrow(geo.clean)
#View(geo.clean)
duplicata <- duplicated(geo.clean[, c('decimalLongitude', 'decimalLatitude')])
#which(duplicata)
#sum(duplicata)
geo.clean2 <- geo.clean[!duplicata, ]
# newData <- lapply(splitData[a], remove.duplicates, zero = 1, remove.second = TRUE)

# Now combine clean splited tables into one single table
#geo.clean <- do.call("rbind", splitData)


#plotting in maps to see points of occurence (if you want to keep track of what's going on)
#par(mfrow = c(1, 2))
#plot(decimalLatitude ~ decimalLongitude, data = table)
#map(, , , add = TRUE)
#plot(decimalLatitude ~ decimalLongitude, data = geo.clean2)
#map(, , , add = TRUE)
#par(mfrow = c(1, 1))
#nrow(geo.clean2)

write.csv(geo.clean2,
          paste0("./results/clean_data_",table$scientificName[a],".csv"),
          row.names = FALSE)
}
