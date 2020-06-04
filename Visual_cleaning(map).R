##get packgs
library(sp)
library(mapview)
library(raster)
library(rgeos)
library(rgdal)

## Visualisation

#insert a table with the name of all species if you want to work with many species. If not, provide the name of a single species in the next command line.
Sptable <- read.csv("./data/Lagartos_Silva2017.csv")

# Create a vector with all species names in you table
scientificName <- as.vector(Sptable$Species)

#Or provide the name of the single species you want to work with (as found in the table)
scientificName <- "Tropidurus semitaeniatus"

##insert a as the row number of the species you are currently working with. If you provide the name of only one species, you can leave it as a=1
a = 1

##read the table with species name, lat, lon. At this points you can just read your table directly or create a path to read multiple species based on row number in your species table.
file_path <- file.path("D:/RedeCLIMA/model_inputs/Biodiversidade/Repteis/clean_data", paste0("clean_data_",scientificName[a], ".csv"))

#file_path <- file.path("./results/clean_data", paste0("clean_data_",scientificName[a], ".csv"))

table <- read.csv(file_path)
View(table)


#################TAXONOMIC CLEANING (wrong subspecies/synonyms)

##check all the synonyms you have and select only the valid ones [you can skip this step]
print(table$species[1])

#exclude wrong names if there are any
table <- table[-c(which(table$species == 'Gymnodactylus geckoides')),]
# or select only one valid name
table <- table[c(which(table$species == 'Gymnodactylus geckoides')),]
View(table)


#Create spatial points
poc <- SpatialPointsDataFrame(data.frame(table$decimalLongitude,table$decimalLatitude), table)
#plot them on iteractive map
proj4string(poc) <- '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'
mapview(poc)

# if points and names are ok, save the new table
write.csv(table,
          paste0("./results/clean_data/clean_data_",scientificName[a],".csv"),
          row.names = FALSE)


#################CLEANING BASED ON SPATIAL REFERENCE

##add shapefile to perform spatial cleaning based on geographic distribution in the literature
biomes <- readOGR(dsn = './Maps', layer = 'bioma_1milhao_uf2015_250mil_IBGE_albers_v3_revisao_carta250mil')
##put it in the same projection as your spatial points
biomes <- spTransform(biomes, proj4string(poc))

#plot both shapefile and spatial points on mapviewer
biomes_map <- mapview(biomes)
poc_map <- mapview(poc)
biomes_map + poc_map

#select the specific polygon area you want
Caatinga <- biomes[which(biomes@data$CD_LEGENDA == 'CAATINGA'),]
plot(Caatinga)

#select the points inside the polygon area you want (based on shapefile data)
sel_poc <- poc[biomes[which(biomes@data$CD_LEGENDA == 'CAATINGA'),],]
plot(sel_poc, add = T)
sel_poc_map <- mapview(sel_poc)

#plot in mapviewer to check
sel_poc_map + biomes_map

#Save as table
write.csv(sel_poc,
          paste0("./results/clean_data/clean_data2_",scientificName[a],".csv"),
          row.names = FALSE)

#################CLEANING BASED ON POINTS ID
#On mapviewer you can click on each point to know its data information

#erase bad points by row number
#table2 <- table[-14,]

#erase bad points by ID number (the first one)
table2 <- table[-c(which(table$ID == 30)),]
View(table2)

#if there are more points, erase here chaging only the ID number (you need to change this number for each points at a time - this is annoying)
table2 <- table2[-c(which(table2$ID == 256)),]
#View(table2)


#create spatial points from new table and plot them on iteractive map to check
poc2 <- SpatialPointsDataFrame(data.frame(table2$decimalLongitude,table2$decimalLatitude), table2)
proj4string(poc2) <- proj4string(poc)
poc2_map <- mapview(poc2)
poc2_map + biomes_map

#Save the new table
write.csv(table2,
          paste0("./results/clean_data/clean_data_",scientificName[a],".csv"),
          row.names = FALSE)
