getwd()
#mudar para uma das minhas sp.(mamifero) *Blastocerus dichotomus*
#2. mudar para uma das minhas sp.(planta) *Blanchetia heterotricha*
spLink <- rspeciesLink (filename = "raw_data", scientificName =  c("Blastocerus dichotomus", "Blanchetia heterotricha"), Coordinates = "Yes" )

table.spLink <- as.data.frame(spLink)
#View(table.spLink)

#Column names get a "data." in front of them
#colnames(table.spLink)

#species.names <- unique(table.spLink$data.scientificName)

#Clean NAs
splink.coord <- table.spLink[!is.na(table.spLink$data.decimalLatitude) & !is.na(table.spLink$data.decimalLongitude),]
#nrow(splink.coord)
#View(splink.coord)


# output w/ only potential correct coordinates
geo.clean <- clean_coordinates(x = splink.coord,
                               lon = "data.decimalLongitude",
                               lat = "data.decimalLatitude",
                               species = "data.scientificName",
                               value = "clean")

dir.create("./results/clean_data")
write.csv(occs,
          "./results/data/raw_data.csv",
          row.names = FALSE)

