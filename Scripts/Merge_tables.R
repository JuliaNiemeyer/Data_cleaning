setwd('D:/RedeCLIMA/model_inputs/Biodiversidade')


## Get pckgs
library(plyr)
library(dplyr)


#Create a different folder inside results for clean tables (keep it organized)
dir.create("./Peixes/clean_data/Final_data")

#read table with species names
Sptable <- read.csv("./Peixes/Sp_Peixes_13_07_2020.csv")

#Tests #scientificName <- c('Alouatta caraya', 'Artibeus cinereus', 'Cabassous tatouay')

#create a vector with species names
scientificName <- as.vector(Sptable$species)

### ATENÇÃO: TEM QUE ADIDIOCNAR OUTRAS REFS: Vale2015 / Delgado2018 / ICMBio / Hasui2018
hasui <- read.csv("./ATLANTIC_BIRDS_modifiedJu.csv", sep = ";")
Delgado <- read.csv("./raw_data_Delgado2018_Natalus macrourus.csv")
ICMBio <- read.csv("./Aves_ICMBio_FINAL_Cadu2.csv", sep = ";")
Vale <- read.csv("./raw_data_Vale2015_Leopardus tigrinus.csv", sep = ";")

#start the data gathering & cross-ref with reference tables to get all info
for (a in 1:length(scientificName)) {

  message("starting the analysis for ", paste0(scientificName[a]))
  ## Read original tables

  file_path3 <- file.path("./Peixes/Clean_data", paste0("clean_data_",scientificName[a], ".csv"))

  table <- read.csv(file_path3)

  #table <- read.csv(file_path3, sep = ";")
  #write.csv(table, file_path3)

  ##Inserir a classe aqui pq não tem nas tabelas
  table$ScientificName <- 'Fish'

  #Create new columns
  table[,"Order"] <- NA
  table[,"Country"] <- NA
  table[,"stateProvince"] <- NA
  table[,"locality"] <- NA

  #Checking if the output is empty (no occurrence data available in SpeciesLink for this species. Will work with GBIF only)
  for (x in 1:nrow(table)) {

    if (table$DataSource[x] == "GBIF") {
      ###Get info from Gbif table only

      file_path2 <- file.path("./Peixes", paste0("raw_data_GBIF_",scientificName[a], ".csv"))
      gbif <- read.csv(file_path2)

      select_gbif <- gbif[which(gbif$key == table$ID[x]),]

      ##Get info as pass to the table
      table$Order[x] <- as.character(select_gbif$order)
      table$Country[x] <- as.character(select_gbif$country)
      table$stateProvince[x] <- as.character(select_gbif$stateProvince)
      table$locality[x] <- as.character(select_gbif$locality)

    }
    if (table$DataSource[x] == "SpeciesLink") {
      ##Select the correct line based on ID
      file_path <- file.path("./Peixes", paste0("raw_data_SpeciesLink_",scientificName[a], ".csv"))
      r <- read.csv(file_path)

      select <- r[which(r$record_id == table$ID[x]),]

      ##Get info as pass to the table
      table$Order[x] <- as.character(select$order)
      table$Country[x] <- as.character(select$country)
      table$stateProvince[x] <- as.character(select$stateProvince)
      table$locality[x] <- as.character(select$locality)

    }
    if (table$DataSource[x] == "Vale et al 2015") {
      select <- Vale[which(as.character(Vale$ID) == as.character(table$ID[x])),]

      ##Get info as pass to the table
      table$Order[x] <- as.character(select$Order)
      table$Country[x] <- as.character(select$country)
      table$stateProvince[x] <- as.character(select$stateProvince)
      table$locality[x] <- as.character(select$locality)

    }
    if (table$DataSource[x] == "Delgado2018") {
      select <- Delgado[which(as.character(Delgado$ID) == as.character(table$ID[x])),]

      ##Get info as pass to the table
      table$Order[x] <- as.character(select$Order)
      table$Country[x] <- as.character(select$Country)
      table$stateProvince[x] <- as.character(select$stateProvince)
      table$locality[x] <- as.character(select$locality)

    }
    if (table$DataSource[x] == "ICMBio") {
      select <- ICMBio[which(as.character(ICMBio$ID.Final) == as.character(table$ID[x])),]

      ##Get info as pass to the table
      table$Country[x] <- as.character(select$PAÍS)
      table$stateProvince[x] <- as.character(select$Município)
      table$locality[x] <- as.character(select$Localidade)
      if (x > 1) {
        table$Order[x] <- as.character(table$Order[x - 1])
      }

    }
    if (table$DataSource[x] == "Hasui2018") {
      select <- hasui[which(as.character(hasui$Record_id) == as.character(table$ID[x])),]

      ##Get info as pass to the table
      table$Order[x] <- as.character(select$Order)
      table$Country[x] <- as.character(select$Country)
      table$stateProvince[x] <- as.character(select$Municipality)
      table$locality[x] <- as.character(select$site)

    }


    name <- gsub(" ", "_", scientificName[a], fixed = TRUE)

    write.csv(table,
              paste0("./Peixes/clean_data/Final_data/Final_data_",name,".csv"),
              row.names = FALSE)

  }

}


##--------------------------------------------------------------------------

##Now read all modified clean tables of all species and merge them all together

filenames <- list.files(path = "./Peixes/clean_data/Final_data", pattern = "Final_data", full.names = TRUE)
import.list <- ldply(filenames, read.csv)


#If there's the cientificnamestatus column, erase it. Sometimes it can have a first column X, erase it also if that's the case.
import.list <- import.list[,-c(1,6)]
#or
import.list <- import.list[,-5]

#select only the first 10 columns or as many as you would like to use in your final data (if any others were created by error they will be discarted)
import.list2 <- import.list[,1:10]

#Wite final table
write.csv(import.list2, "./Peixes/clean_data/Final_data/Final_data_Peixes.csv")


###-------------- NEED TO COMBINE MULTIPLE GBIF OR SPECIES LINK TABLES BECAUSE SPECIES CHANGED THEIR NAMES?

#SPLInk
##THIS ONE WORKS WHEN COLUMS ARE DIFFERENT (USUALLY HAPPENS WITH SPLINK)
#r1 is the first splink table (species new name f. ex.)
#r2 is the second splink table (previous species name, f. ex.)

##One problem is that splink has the same ID from 1 to nrows for all tables. So you will have to change ID numbers in both one of the splink tables, and in their respective in clean_table. I usually multiply them by 1000 to be certain it won't be equal to any other ID in the clean_table.

r2 <- read.csv('./Peixes/raw_data_SpeciesLink_Leporinus elongatus.csv')
r2$record_id <- r2$record_id*1000
write.csv(r2, "./Peixes/raw_data_SpeciesLink_Leporinus elongatus.csv")


table[c(20:59),]$ID <- table[c(20:59),]$ID*1000

write.csv(table, file_path3)



#the new data frame will be the second splink, the one with lessc colums.Check before with ncol.
#ncol(r)
#ncol(r2)

df <- r2[colnames(r2) %in% colnames(r)]
#then bind them together
all_splink <- rbind.fill(r, df)
#save ir
write.csv(all_splink, file_path)


#GBIF: usually all tables in gbif have the same columns and column order. you will just have to bind them together.

#read the new table (previous species name, f.ex.)
gbif_l <- read.csv('./Peixes/raw_data_GBIF_Leporinus elongatus.csv')
#View(gbif)
#View(gbif_l)

#bind with the other table (new species name)
all_gbif <- rbind.fill(gbif, gbif_l)
#View(all_gbif)

#save
write.csv(all_gbif, file_path2)




