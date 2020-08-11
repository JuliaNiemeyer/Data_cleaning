##get packgs
library(sp)
library(mapview)
library(raster)
library(rgeos)
library(rgdal)

## Visualisation

#insert a table with the name of all species if you want to work with many species. If not, provide the name of a single species in the next command line.
Sptable <- read.csv("./data/Mamif_Silva2017.csv")

# Create a vector with all species names in you table
scientificName <- as.vector(Sptable$Species)

#Or provide the name of the single species you want to work with (as found in the table)
##insert a as the row number [a] of the species you are currently working with. If you provide the name of only one species, you can leave it as a=1
a = 1
scientificName <- "Amburana cearensis"


##read the table with species name, lat, lon. At this points you can just read your table directly or create a path to read multiple species based on row number in your species table.
#file_path <- file.path("D:/RedeCLIMA/model_inputs/Biodiversidade/Peixes/clean_data", paste0("/clean_data_",scientificName[a], ".csv"))

file_path <- file.path("./Peixes/clean_data", paste0("clean_data_",scientificName[a], ".csv"))

table <- read.csv(file_path)
View(table)

#Create spatial points
poc <- SpatialPointsDataFrame(data.frame(table$decimalLongitude,table$decimalLatitude), table)
#plot them on iteractive map
proj4string(poc) <- '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'
mapview(poc)
poc_map <- mapview(poc)


#################TAXONOMIC CLEANING (wrong subspecies/synonyms)

##check all the synonyms you have and select only the valid ones [you can skip this step]
print(unique(table$species))

#exclude wrong names if there are any
table <- table[-c(which(table$species == 'Opuntia argentina')),]
# or select only one valid name
table <- table[c(which(table$species == 'Amburana cearensis')),]
#for more than one name
table <- table[c(which(table$species == 'Cordia glazioviana' | table$species == 'Auxemma glazioviana')),]





# if points and names are ok, save the new table
write.csv(table,
          paste0("./Peixes/clean_data/clean_data_",scientificName[a],".csv"),
          row.names = FALSE)


#################CLEANING BASED ON SPATIAL REFERENCE

#Bacia do Sao Francisco
Hydro <- readOGR(dsn = './data_cleaning/Maps', layer = 'SNIRH_RegioesHidrograficas')
Hydro <- spTransform(Hydro, proj4string(poc))
Hydro_map <- mapview(Hydro)
Hydro_map + poc_map

sel_poc <- poc[Hydro[which(Hydro@data$RHI_NM == 'PARNAÃBA' | Hydro@data$RHI_NM == 'SÃƒO FRANCISCO'),],]

sel_poc_map <- mapview(sel_poc)


BSF <- readOGR(dsn = './data_cleaning/Maps', layer = 'Watershed_buf')
BSF <- spTransform(BSF, proj4string(poc))
BSF_map <- mapview(BSF)

BSF_map + poc_map

sel_poc <- poc[BSF,]
sel_poc_map <- mapview(sel_poc)

sel_poc_map + BSF_map

##add shapefile to perform spatial cleaning based on geographic distribution in the literature
biomes <- readOGR(dsn = './data_cleaning/Maps', layer = 'bioma_1milhao_uf2015_250mil_IBGE_albers_v3_revisao_carta250mil')

##put it in the same projection as your spatial points
biomes <- spTransform(biomes, proj4string(poc))

#plot both shapefile and spatial points on mapviewer
biomes_map <- mapview(biomes)
biomes_map + poc_map


#select the specific polygon area you want
Caatinga <- biomes[which(biomes@data$CD_LEGENDA == 'CAATINGA'),]
plot(Caatinga)

Cerrado <- biomes[which(biomes@data$CD_LEGENDA == 'CERRADO'),]

AF <- biomes[which(biomes@data$CD_LEGENDA == 'MATA ATLÃ‚NTICA'),]

#select the points inside the polygon area you want (based on shapefile data)
sel_poc <- poc[biomes[which(biomes@data$CD_LEGENDA == 'CAATINGA'),],]
##or
sel_poc <- poc[biomes[which(biomes@data$CD_LEGENDA == 'CAATINGA' | biomes@data$CD_LEGENDA == 'MATA ATLÃ‚NTICA' | biomes@data$CD_LEGENDA == 'PAMPA'),],]

sel_poc <- poc[biomes[which(biomes@data$CD_LEGENDA == 'MATA ATLÃ‚NTICA'),],]

sel_poc_map <- mapview(sel_poc)

#plot in mapviewer to check
sel_poc_map + biomes_map

sel_poc <- as.data.frame(sel_poc)
sel_poc <- data.frame(sel_poc$ID, sel_poc$species, sel_poc$decimalLatitude, sel_poc$decimalLongitude, sel_poc$DataSource)

names(sel_poc)[names(sel_poc) == "sel_poc.ID"] <- "ID"
names(sel_poc)[names(sel_poc) == "sel_poc.species"] <- "species"
names(sel_poc)[names(sel_poc) == "sel_poc.decimalLatitude"] <- "decimalLatitude"
names(sel_poc)[names(sel_poc) == "sel_poc.decimalLongitude"] <- "decimalLongitude"
names(sel_poc)[names(sel_poc) == "sel_poc.DataSource"] <- "DataSource"

#Save as table
write.csv(sel_poc,
          paste0("./Peixes/clean_data/clean_data_",scientificName[a],".csv"),
          row.names = FALSE)

#with other shapefile (Brasil in this case)
brasil <- readOGR(dsn = './data_cleaning/Maps', layer = 'BRUFE250GC_SIR')
brasil <- spTransform(brasil, proj4string(poc))
brasil_map <- mapview(brasil)
brasil_map + poc_map


sel_UF_poc <- poc[brasil,]

## select brazilian state
sel_UF_poc <- poc[brasil[which(brasil@data$NM_REGIAO == "NORDESTE"),],]

sel_UF_poc <- poc[brasil[-which(brasil@data$NM_ESTADO == "MATO GROSSO"),],]

sel_UF_poc <- sel_UF_poc[sel_UF_poc[-which(sel_UF_poc@data$ID == "2596111216"),],]


sel_poc_map_2 <- mapview(sel_UF_poc)
brasil_map + sel_UF_poc

sel_UF_poc <- as.data.frame(sel_UF_poc)
sel_UF_poc <- data.frame(sel_UF_poc$ID, sel_UF_poc$species, sel_UF_poc$decimalLatitude, sel_UF_poc$decimalLongitude, sel_UF_poc$DataSource)

names(sel_UF_poc)[names(sel_UF_poc) == "sel_UF_poc.ID"] <- "ID"
names(sel_UF_poc)[names(sel_UF_poc) == "sel_UF_poc.species"] <- "species"
names(sel_UF_poc)[names(sel_UF_poc) == "sel_UF_poc.decimalLatitude"] <- "decimalLatitude"
names(sel_UF_poc)[names(sel_UF_poc) == "sel_UF_poc.decimalLongitude"] <- "decimalLongitude"
names(sel_UF_poc)[names(sel_UF_poc) == "sel_UF_poc.DataSource"] <- "DataSource"

#Save as table
write.csv(sel_UF_poc,
          paste0("./Peixes/clean_data/clean_data_",scientificName[a],".csv"),
          row.names = FALSE)


world <- readOGR(dsn = './data_cleaning/Maps', layer = 'TM_WORLD_BORDERS_SIMPL-0.3')
world <- spTransform(world, proj4string(poc))
Americas <- world[which(world@data$REGION == '19'),]
Americas_map <- mapview(Americas)
Americas_map + poc_map

sel_country_poc <- poc[Americas[which(Americas@data$SUBREGION == '5'),],]

sel_country_poc <- poc[Americas[which(Americas@data$NAME == 'Brazil' | Americas@data$NAME == 'Argentina' | Americas@data$NAME == 'Paraguay' | Americas@data$NAME == 'Bolivia'),],]

sel_country_poc <- poc[Americas[which(Americas@data$NAME == 'Brazil' | Americas@data$NAME == 'Paraguay' | Americas@data$NAME == 'Bolivia'),],]



sel_country_map <- mapview(sel_country_poc)
sel_country_map + Americas_map
sel_country_map + biomes_map

#plot in mapviewer to check
sel_country_map + biomes_map
sel_country_poc <- sel_country_poc[biomes[which(biomes@data$CD_LEGENDA != 'AMAZÃ”NIA'),],]

#Save the new table
write.csv(sel_country_poc,
          paste0("./Peixes/clean_data/clean_data",scientificName[a],".csv"),
          row.names = FALSE)
#################CLEANING BASED ON POINTS ID
#On mapviewer you can click on each point to know its data information

#erase bad points by row number
#table2 <- table[-14,]
sel_country_poc2 <- sel_country_poc[-c(which(sel_country_poc@data$ID == 197237750 | sel_country_poc@data$ID == 13)),]
mapview(sel_country_poc2)
sel_poc

#Save the new table
write.csv(sel_country_poc2,
          paste0("./Peixes/clean_data/clean_data",scientificName[a],".csv"),
          row.names = FALSE)


#erase bad points by ID number (the first one)
table2 <- table[-c(which(table$ID == 10 | table$ID == 9)),]
View(table2)

#if there are more points, erase here chaging only the ID number (you need to change this number for each points at a time - this is annoying)
table2 <- table[-c(which(table$ID == 9604 | table$ID == 9603)),]

#View(table2)
table2 <- table2[-c(which(table2$ID == 42)),]


#create spatial points from new table and plot them on iteractive map to check
poc2 <- SpatialPointsDataFrame(data.frame(table2$decimalLongitude,table2$decimalLatitude), table2)
proj4string(poc2) <- proj4string(poc)
poc2_map <- mapview(poc2)
mapview(poc2)

poc2_map + biomes_map
poc2_map + Americas


#Save the new table
write.csv(table2,
          paste0("./Peixes/clean_data/clean_data_",scientificName[a],".csv"),
          row.names = FALSE)
