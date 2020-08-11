library(magrittr)
library(raster)
library(ncdf4)
library(rgeos)
library(dismo)
library(jsonlite)
library("readxl")
library("xlsx")
library(rgdal)
library(sp)
library(reshape)
library(maptools)
library(gdistance)
require('geosphere')
library(devtools)
library(Mapinguari)
library(tidyverse)
library( spThin )
library(ggmap)
library(ggplot2)



# Colecting data ----------------------------------------------------------

tina_soli_raw <- gbif("Auliscomys", "boliviensis", geo=T, removeZeros=T)
#colnames(tina_soli_raw) #checar o nome das colunas, ai na proxima linha eu escolho quais colunas eu quero
tina_soli_4col <- tina_soli_raw[,c("species","lon","lat","country")]
unique(tina_soli_4col$country)
#tina_soli_count <- tina_soli_4col[tina_soli_4col$country != c("Madagascar"),]
#tina_soli_count <- tina_soli_count[tina_soli_count$country != c("Japan"),]
tina_soli_count=tina_soli_4col
head(tina_soli_count)
names(tina_soli_count) <-c("Species", "Lon", "Lat")
#plot(tina_soli_raw$lon ~tina_soli_raw$lat)

#sp1 <- read.csv("C:/Users/Luara/Dropbox/Doutorado/Occurency_records/table_sp_ref/Mammals/Choloepus_hoffmanni_plusGaston.csv")
#PhrynocephalusSppTb1 <- rbind(tina_soli_count[,-c(4,5)],sp1)
PhrynocephalusSppTb1 <- tina_soli_count[,-c(4,5)]
PhrynocephalusSppTb1$`Species` <- sub(pattern = " ", replacement = "_", x = PhrynocephalusSppTb1$`Species`)
head(PhrynocephalusSppTb1)

# Cleaning by removing zeros and NA ------------------------------------------


tina_soli_my_gbif = PhrynocephalusSppTb1
tina_soli_sNA <- subset(tina_soli_my_gbif, Lat !=0 & Lon !=0)
#tina_soli_sNA <- tina_soli_my_gbif[((tina_soli_my_gbif$Lat != 0) & (tina_soli_my_gbif$Lon != 0)),]
#test2 <- test[complete.cases(test), ]
subset(tina_soli_sNA, Lon==0)
subset(tina_soli_sNA, Lat==0)
which(is.na(tina_soli_sNA$Lat))
which(is.na(tina_soli_sNA$Lon))
#t2 <-t[!is.na(t$Temperature.Cº.), ]
#tina_soli_sNA <- tina_soli_sNA[-c(1),]
head(tina_soli_sNA)
dim(tina_soli_sNA)
PhrynocephalusSppTb1 = tina_soli_sNA


# Cleaning by biovariable layers ------------------------------------------------

tina_soli2 = PhrynocephalusSppTb
#tina_soli2 = PhrynocephalusSppTb1

#variable_world <- raster("F:/microclim/50_shade_soil_air/TA1cm_soil_50_1.tif")

# extent (lon0,lon1,lat0,lat1) - in the square esq baixo os zeros, esq alto lat 1, dir baixo lat1)
#e <- extent(c(-70,-30,-40,10)) # M.A.
e <- extent(-115,-35,-40,45)
variable <- crop(t_raster, e)
#proj4string(variable)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
#variable<-projectRaster(variable,crs=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

coordinates(tina_soli2) <- ~Lon+Lat
proj4string(tina_soli2)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
tina_soliprj<-spTransform(tina_soli2,CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
plot(variable, axes=T)
plot(tina_soliprj, add=T, col= "red")

varcrop = crop(variable, tina_soliprj)
plot(varcrop)
plot(tina_soliprj, add=T, col= "red")
#proj4string(varcrop)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
#varcrop <-projectRaster(varcrop,crs=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

tina_soliextract <- raster::extract(variable, tina_soliprj, method = "bilinear")
tina_soliprjext<- data.frame(tina_soliprj,tina_soliextract)
which(tina_soliprjext$tina_soliextract ==0)
which(is.na(tina_soliprjext$tina_soliextract))
PhrynocephalusSpp <- tina_soliprjext[!is.na(tina_soliprjext$tina_soliextract),]
which(is.na(PhrynocephalusSpp$tina_soliextract))
head(PhrynocephalusSpp)
PhrynocephalusSppTb <- PhrynocephalusSpp[,c(1:11)]
#PhrynocephalusSppTb <- PhrynocephalusSpp[,c(1:3)]
head(PhrynocephalusSppTb)
dim(PhrynocephalusSppTb)


# Cleaning by worldclim rasters -------------------------------------------

variable_worldclim <- raster("C:/Users/Luara/Documents/worldclim/global_grids_30_seconds/tmax_01.gri")

# extent (lon0,lon1,lat0,lat1) - in the square esq baixo os zeros, esq alto lat 1, dir baixo lat1)
#e <- extent(c(-70,-30,-40,10)) # M.A.
#e <- extent(c(-180,-10,-70,80)) #All America
#variable <- crop(variable_worldclim, e)
#proj4string(variable)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
#variable<-projectRaster(variable,crs=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

#tina_soli2 = PhrynocephalusSppTb
# coordinates(tina_soli2) <- ~Lon+Lat
# proj4string(tina_soli2)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
# tina_soliprj<-spTransform(tina_soli2,CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
# plot(variable, axes=T)
# plot(tina_soliprj, add=T, col= "red")

varcrop = crop(variable_worldclim, tina_soliprj)
#plot(varcrop)
#plot(tina_soliprj, add=T, col= "red")
#proj4string(varcrop)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
#varcrop <-projectRaster(varcrop,crs=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

tina_soliextract <- raster::extract(variable, tina_soliprj, method = "bilinear")
tina_soliprjext<- data.frame(tina_soliprj,tina_soliextract)
which(tina_soliprjext$tina_soliextract ==0)
which(is.na(tina_soliprjext$tina_soliextract))
PhrynocephalusSpp<- tina_soliprjext[!is.na(tina_soliprjext$tina_soliextract),]
which(is.na(PhrynocephalusSpp$tina_soliextract))
#PhrynocephalusSpp <- PhrynocephalusSpp[-c(1,1339,1340),]

PhrynocephalusSppTb <- PhrynocephalusSpp[,c(1:11)]
#PhrynocephalusSppTb <- PhrynocephalusSpp[,c(1:3)]
head(PhrynocephalusSppTb)
dim(PhrynocephalusSppTb)


# Cleaning by removing the duplicates -------------------------------------

dups2 <- duplicated(PhrynocephalusSppTb[, c('Lon', 'Lat')])
which(dups2)
sum(dups2)
tina_soli_sdupl <- PhrynocephalusSppTb[!dups2, ]
head(tina_soli_sdupl)
dim(tina_soli_sdupl)


# Cleaning by 1km using spThin --------------------------------------------

library( spThin )
setwd("C:/Users/Luara/Dropbox/Doutorado/Occurency_records/table_sp_ref/Mammals/")

thinned_dataset_full <-
  thin( loc.data = tina_soli_sdupl, 
        lat.col = "Lat", long.col = "Lon", 
        spec.col = "Species", 
        thin.par = 1, reps = 1, 
        locs.thinned.list.return = TRUE, 
        write.files = TRUE, 
        max.files = 1, 
        out.dir = ".", out.base = "Auliscomys_boliviensis", 
        write.log.file = F)

sp <- read.csv("C:/Users/Luara/Dropbox/Doutorado/Occurency_records/table_sp_ref/Mammals/Auliscomys_boliviensis_thin1.csv",sep=",")
dim(sp)
plot(varcrop, axes=T)
coordinates(sp) <- ~Lon+Lat
plot(sp, add=T, col= "red")

#So para ver se cai no recorte que quero
# crop <- raster("E:/worldclim/RCP85_70_MPI_Eurasia/m1_af.crop.grd")
# plot(crop, axes=T)
# plot(sp,add=T,col= "red")

#plot(variable_worldclim, xlim=c(-105,-30), ylim=c(-55,20), axes=TRUE) #South America

PhrynocephalusSppTb <- read.csv("C:/Users/Luara/Dropbox/Doutorado/Occurency_records/table_sp_ref/Mammals/Auliscomys_boliviensis_thin1.csv",sep=",")
head(PhrynocephalusSppTb)


#Apagar algum ponto que vc ache necessario
test= PhrynocephalusSppTb
t<-test[!(test$Lon < -110),]
#PhrynocephalusSppTb <- PhrynocephalusSppTb[-c(33098,31383,14379,14388),]
#PhrynocephalusSppTb2 <- PhrynocephalusSppTb[!PhrynocephalusSppTb$Lon > 45,]
  
PhrynocephalusSppTb = t
coordinates(t) <- ~Lon+Lat
plot(t, col= "red")

#dir.create(paste0("C:/Users/Luara/Dropbox/Doutorado/Occurency_records/table_sp_ref/Mammals/TNZ_tables", "/", "Rodentia"))
write.csv(PhrynocephalusSppTb,("C:/Users/Luara/Dropbox/Doutorado/Occurency_records/table_sp_ref/Mammals/TNZ_tables/Rodentia/Auliscomys_boliviensisTb.csv"),row.names = F)


