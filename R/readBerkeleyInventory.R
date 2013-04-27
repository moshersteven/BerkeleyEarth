readBerkeleyInventory<-function(Directory,filename="site_summary.txt",spatial=TRUE){
  require(sp)
  require(raster)
  X <- read.delim(file.path(Directory,filename, fsep =.Platform$file.sep), 
                  comment.char = "%", 
                  quote ="",
                  na.strings = c("-999.000","-9999.000","-9.99999","-9999","-999"),
                  header = FALSE, 
                  
                  stringsAsFactors= FALSE)
  
  if(filename=="site_summary.txt"){ 
  colnames(X)<- c("Id","Lat","Lon","Altitude")
  }
  
  if(filename=="site_detail.txt"){
     colnames(X)<- c("Id","Name", "Lat","Lon" ,"Altitude" ,"LatUnc","LonUnc","AltUnc",
                  "Country","State","County","Tzone","WMO","COOP","WBAN","ICOA",
                  "Relo","SugRelo","Sources","Hash")
      X$Tzone[X$Tzone == -99]          <- NA 
      X$ICOA[X$ICOA == "      "] <- NA
      X$State[X$State == "            "]  <-NA
      X$Country[X$Country == "[Missing]                               "] <- NA
      X$County[X$County == "                    "] <- NA
      X$COOP[X$COOP == -9999] <- NA
      X$Lat[X$Lat == -999] <-NA
      X$Lon[X$Lon == -999] <-NA
      X$Country <- trim(X$Country)
  }
  
   
  
  if(filename=="site_complete_detail.txt"){
     colnames(X)<- c("Id","ReportNo","Name", "Country","StartDate","EndDate","Source", 
                     "Lat","Lon" ,"Altitude" ,"AltElevation","LatUnc","LonUnc","AltUnc",
                     "ReloFlag","State","County","Tzone","WMO","COOP","WBAN","ICOA",
                      "USAF","NCDC","Instrument","OtherId","ArchiveKey","Hash")
  
   
   
      X$Tzone[X$Tzone == -99]              <- NA 
      X$ICOA[X$ICOA == "      "]           <- NA
      X$State[X$State == "            "]  <-NA
      X$Country[X$Country == "[Missing]                               "] <- NA
      X$County[X$County == "                    "] <- NA
      X$COOP[X$COOP == -9999]                      <- NA
      X$AltElevation[X$AltElevation == -999]       <- NA
      X$WMO[X$WMO == -9999]                        <- NA 
      X$WBAN[X$WBAN == -9999]          <- NA
      X$USAF[X$USAF == -9999]          <- NA
      X$Altitude[X$Altitude == -999] <- NA
      X$AltUnc[X$AltUnc == -9.99999] <- NA
     X$Lat[X$Lat == -999] <-NA
     X$Lon[X$Lon == -999] <-NA
     X$Country <- trim(X$Country)
  }
  
  if(spatial){
    X   <- X[!is.na(X$Lon),]
    X   <- X[!is.na(X$Lat),]
    warning("Stations with missing Lat/Lon removed")
    WGS <-  "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    coordinates(X) <- c("Lon","Lat")
    proj4string(X) <- CRS(WGS)
  }
  return(X)
  
  
}
 