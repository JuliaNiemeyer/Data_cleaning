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


######## WORK ON IT
#inserir todas as espécies em forma de lista:
#passo 1. ler a tabela ue eu criei e transformar a lista de espécies em lista
# Passo 2. botar como scienficname que vai entrar na função
#scientificName = c('..........')

# Read the spreadsheet with the names of all your species (should be located in the "data" folder).
Sptable <- read.csv("./data/Species_ex.csv")
scientificName <- as.vector(Sptable$Species)

## get records from SpeciesLink and save ir to the "results" directory (the creation of the results directory is part of the rspeciesLink function)
spLink <- rspeciesLink(filename = "raw_data", scientificName = scientificName, Coordinates = "Yes")

#read the above table as csv to work on it
table <- read.csv("./results/raw_data.csv")

#nrow(table)
#View(table)

#Clean NAs
splink.coord <- table[!is.na(table$decimalLatitude) & !is.na(table$decimalLongitude),]
#nrow(splink.coord)
#View(splink.coord)


#Now we will use the the function `clean_coordinates()` from the `CoordinateCleaner` package to clean the species records. This function checks for common errors in coordinates such as institutional coordinates, sea coordinates, outliers, zeros, centroids, etc. This function does not accept not available information (here addressed as "NA") so we will first select only data that have a numerical value for both latitude and longitude.

#Note: at this moment having a specific ID code for each observation is essential. The raw data already provides an ID in the column `gbifID`.

# output w/ only potential correct coordinates

#####PERGUNTA: será que isso aqui deve ser feito também por espécie separada?
geo.clean <- clean_coordinates(x = splink.coord,
                               lon = "decimalLongitude",
                               lat = "decimalLatitude",
                               species = "scientificName",
                               value = "clean")

#nrow(geo.clean)
#View(geo.clean)

# Cleaning by removing the duplicates -------------------------------------

#Split the table in x tables: in which x is the the number of scpecies in our table.
splitData <- split(geo.clean, geo.clean$scientificName)

#Do a loop to clean each splited table separately
for (a in 1:length(splitData)) {
  duplicata <- duplicated(splitData[[a]][, c('decimalLongitude', 'decimalLatitude')])
  #which(duplicata)
  #sum(duplicata)
  splitData[[a]] <- splitData[[a]][!duplicata, ]
 # newData <- lapply(splitData[a], remove.duplicates, zero = 1, remove.second = TRUE)
}

# Now combine clean splited tables into one single table
geo.clean2 <- do.call("rbind", splitData)


#plotting in maps to see points of occurence (if you want to keep track of what's going on)
#par(mfrow = c(1, 2))
#plot(decimalLatitude ~ decimalLongitude, data = table)
#map(, , , add = TRUE)
#plot(decimalLatitude ~ decimalLongitude, data = geo.clean2)
#map(, , , add = TRUE)
#par(mfrow = c(1, 1))

#Creates an output drive and save the final csv file
dir.create("./results/clean_data")
write.csv(geo.clean2,
          "./results/clean_data/clean_data.csv",
          row.names = FALSE)

