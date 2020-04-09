getwd()
#mudar para uma das minhas sp.(mamifero) *Blastocerus dichotomus*
#2. mudar para uma das minhas sp.(planta) *Blanchetia heterotricha*
spLink <- rspeciesLink(filename = "ex01", scientificName =  c("Blastocerus dichotomus", "Blanchetia heterotricha"), Coordinates = "Yes" )
table.spLink <- as.data.frame(spLink)
#View(table.spLink)

#Column names get a "data." in front of them
#colnames(table.spLink)

#species.names <- unique(table.spLink$data.scientificName)

#Clean NAs
splink.coord <- table.spLink[!is.na(table.spLink$data.decimalLatitude) & !is.na(table.spLink$data.decimalLongitude),]
#nrow(splink.coord)
#View(splink.coord)

#Clean lat and long 0s
splink.coord2 <- splink.coord[splink.coord$data.decimalLatitude != 0 & splink.coord$data.decimalLongitude != 0,]
#nrow(splink.coord2)
#View(splink.coord2)


occs.new.geo <- clean_coordinates(x = splink.coord2,
                                  lon = "data.decimalLongitude",
                                  lat = "data.decimalLatitude",
                                  species = "data.scientificName",
                                  )
