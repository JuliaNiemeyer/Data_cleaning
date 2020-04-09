getwd()
#mudar para uma das minhas sp.(mamifero) *Blastocerus dichotomus*
#2. mudar para uma das minhas sp.(planta) *Blanchetia heterotricha*
spLink <- rspeciesLink (filename = "raw_data", scientificName =  c("Blastocerus dichotomus", "Blanchetia heterotricha"), Coordinates = "Yes" )
nrow(spLink)
#table.spLink <- as.data.frame(spLink)
#View(table.spLink)

#Column names get a "data." in front of them
#colnames(table.spLink)

#species.names <- unique(table.spLink$data.scientificName)

#Clean NAs
splink.coord <- spLink[!is.na(spLink$data.decimalLatitude) & !is.na(spLink$data.decimalLongitude),]
#nrow(splink.coord)
#View(splink.coord)


# output w/ only potential correct coordinates
geo.clean <- clean_coordinates(x = splink.coord,
                               lon = "decimalLongitude",
                               lat = "decimalLatitude",
                               species = "scientificName",
                               value = "clean")


#plotting
par(mfrow = c(1, 2))
plot(decimalLatitude ~ decimalLongitude, data = occs)
map(, , , add = TRUE)
plot(decimalLatitude ~ decimalLongitude, data = geo.clean)
map(, , , add = TRUE)
par(mfrow = c(1, 1))


dir.create("./results/clean_data")
write.csv(occs,
          "./results/data/raw_data.csv",
          row.names = FALSE)

