
#library("tidyverse")
#library("stringr")
#library(data.table)

#rm(list=ls())

setwd("D:/RedeCLIMA/model_inputs/Biodiversidade")

#df <- read.csv("trial.csv", header = T)

df <- read.csv("raw_data_Delgado2018_Natalus macrourus_Copy.csv", header = T, sep = ';')
head(df)

## FIRST RUN dg2dec FUNCTION

# Converting coordinates from DMS or DdM formats to decimal
# DMS = "degrees minutes seconds"; DdM = "degrees decimal minutes"
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dg2dec <- function(varb, Dg=NA, Min=NA, Sec=NA, SW.Hemisphere="S|W") {
  # Dg=decimal, Min=minutes and Sec=seconds;
  # NOTE 1 - if the format is "degrees decimal minutes - DdM" (e.g. 40° 26.767′ N) and not
  # "degrees minutes seconds - DMS" (e.g. 40° 26′ 46″ N), then call the function only with
  # Dg and Min arguments, like dg2dec(varb, Dg="°", Min="′N").
  # Same applies when there is no seconds symbol (e.g. 45°12'7.38).
  # Note that one should not use blank spaces in Dg, Min or Sec arguments (will return NA).
  # For more details on formats see:
  # https://en.wikipedia.org/wiki/Geographic_coordinate_conversion#Coordinate_format_conversion

  # Use paste0("[", Dg, Min, Sec, "]") to build regex [] pattern
  # therefore, strsplit() will split string "varb" by what symbols you give to Dg, Min, Sec
  DMS <- sapply(strsplit(varb, paste0('[', Dg, Min, Sec, ']')), as.numeric)

  # DMS is a matrix; first row contains degrees; second - minutes; third - seconds.
  # If the format is "degrees decimal minutes" (e.g. 40° 26.767′ N) and not
  # "degrees minutes seconds" (e.g. 40° 26′ 46″ N), then the matrix has only two valid rows:
  # first row contains degrees; the second - minutes;
  # therefore, compute conversion for seconds only if there are more than 2 rows in DMS
  # and Sec is different from NA (if there are seconds in the DMS format)
  decdg <- abs(DMS[1, ]) + DMS[2, ]/60 + ifelse(dim(DMS)[1] > 2  & !is.na(Sec), DMS[3, ]/3600, 0)

  # all cordinates from Southern or Western Hemispheres become negative in their decimal format
  SW <- grepl(pattern = SW.Hemisphere, x = varb, ignore.case = TRUE)
  return(ifelse(SW, -1, 1) * decdg)
}

# References:
# http://stackoverflow.com/questions/14404596/converting-geo-coordinates-from-degree-to-decimal
# https://en.wikipedia.org/wiki/Geographic_coordinate_conversion#Coordinate_format_conversion
# https://en.wikipedia.org/wiki/Decimal_degrees
# tested conversion with http://www.pgc.umn.edu/tools/conversion



for (i in 1:nrow(df)) {

#if (is.numeric(df$Latitude[i]) == TRUE){
  if (grepl("-[0-9]*.",df$Latitude[i]) == T) {
  #df.dt[i, lat_dec := df.dt$Latitude[i]]
  #df.dt[i, long_dec := df.dt$Longitude[i]]
  df$lat_dec[i] <- as.character(df$Latitude[i])
  df$long_dec[i] <- as.character(df$Longitude[i])

} else {

  if (grepl("[0-9]*''S|[0-9]*''N",df$Latitude[i]) == T) {

    ###acertar essa parte aqui. funçao funciona mas nao aplica o valor à linha
    # create new column "DMS_dec" by applying dg2dec() function on column "DMS"

    #df.dt[i, lat_dec := dg2dec(as.character(df.dt$Latitude[i]), Dg="°", Min="'", Sec="''S|N")]

    df$decimalLatitude[i] <- dg2dec(as.character(df$Latitude[i]), Dg = "°", Min = "'", Sec = "''S|N")

    #df.dt[i, long_dec := dg2dec(as.character(df.dt$Longitude[i]), Dg="°", Min="'", Sec="''E|W")]
    df$decimalLongitude[i] <- dg2dec(as.character(df$Longitude[i]), Dg = "°", Min = "'", Sec = "''E|W")

  }else {

    #if (is.na(Sec) == TRUE) {
    #df.dt[i, lat_dec := dg2dec(as.character(df.dt$Latitude[i]), Dg="°", Min="'")]

    #df.dt[i, long_dec := dg2dec(as.character(df.dt$Longitude[i]), Dg="°", Min="'")]

    df$decimalLatitude[i] <- dg2dec(as.character(df$Latitude[i]), Dg = "°", Min = "'S|N")
    df$decimalLongitude[i] <- dg2dec(as.character(df$Longitude[i]), Dg = "°", Min = "'E|W")

}}
}

write.csv(df, "./raw_data_Delgado2018_Natalus macrourus_Copy.csv", row.names = F)

# case of "degrees minutes seconds" (DMS) for latitude
# with character for Northern (N) and Southern (S) Hemispheres
# use regular expression metacharacter | to indicate logical operation "OR"
#dg2dec(varb=df$Latitude, Dg="°", Min="'", Sec="''S|N")

#dg2dec(varb=df$Longitude, Dg="°", Min="'", Sec="''E|W")
