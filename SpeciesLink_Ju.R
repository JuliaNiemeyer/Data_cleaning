getwd()
#mudar para uma das minhas sp.(mamifero) *Blastocerus dichotomus*
#2. mudar para uma das minhas sp.(planta) *Blanchetia heterotricha*

######## WORK ON IT
#inserir todas as espécies em forma de lista:
#passo 1. ler a tabela ue eu criei e transformar a lista de espécies em lista
# Passo 2. botar como scienficname que vai entrar na função
#scientificName = c('..........')

######Decidir qual vai ser o nome do diretório
#dir = "SpeciesLink"

## get records from species link and save ir to the "results" directory (it's part of the function)
spLink <- rspeciesLink (filename = "raw_data", scientificName =  c("Blastocerus dichotomus", "Blanchetia heterotricha"), Coordinates = "Yes")

#read the above table as csv to work on it
table <- read.csv("./results/raw_data.csv")

#nrow(table)
#View(table)

#species.names <- unique(table.spLink$data.scientificName)

#Clean NAs
splink.coord <- table[!is.na(table$decimalLatitude) & !is.na(table$decimalLongitude),]
#nrow(splink.coord)
#View(splink.coord)


#Now we will use the the function `clean_coordinates()` from the `CoordinateCleaner` package to clean the species records. This function checks for common errors in coordinates such as institutional coordinates, sea coordinates, outliers, zeros, centroids, etc. This function does not accept not available information (here addressed as "NA") so we will first select only data that have a numerical value for both latitude and longitude.

#Note: at this moment having a specific ID code for each observation is essential. The raw data already provides an ID in the column `gbifID`.

# output w/ only potential correct coordinates
geo.clean <- clean_coordinates(x = splink.coord,
                               lon = "decimalLongitude",
                               lat = "decimalLatitude",
                               species = "scientificName",
                               value = "clean")

#nrow(geo.clean)
#View(geo.clean)

# Cleaning by removing the duplicates -------------------------------------

duplicata <- duplicated(geo.clean[, c('decimalLongitude', 'decimalLatitude')])
#which(duplicata)
#sum(duplicata)
geo.clean2 <- geo.clean[!duplicata, ]

#head(splink.coord2)
#nrow(splink.coord2)

#plotting
#par(mfrow = c(1, 2))
#plot(decimalLatitude ~ decimalLongitude, data = occs)
#map(, , , add = TRUE)
#plot(decimalLatitude ~ decimalLongitude, data = geo.clean)
#map(, , , add = TRUE)
#par(mfrow = c(1, 1))

dir.create("./results/clean_data")
write.csv(geo.clean2,
          "./results/clean_data/clean_data.csv",
          row.names = FALSE)

