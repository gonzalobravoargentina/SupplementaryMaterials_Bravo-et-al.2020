#Prepare data to be uploaded into OBIS

#we got density and cover matrix and we wanto upload both types of data 
#read cover and density data
Cover.data<- read.csv("Percent_Cover_Data.csv")
Density.data <- read.csv("Density_Data.csv")
Density.data[,-(1:20)] <- Density.data[,-(1:20)]/0.0625 #m2
#add "C" to cover and "D" to density 
library(tibble)
Cover.data <- add_column(Cover.data, CD = "C", .after = "Annotation.area")
Density.data <- add_column(Density.data, CD = "D", .after = "Annotation.area")

#upper case to character columns 
library(tidyverse)
Cover.data %>% mutate_if(is.character, str_to_upper) -> Cover.data
Density.data %>% mutate_if(is.character, str_to_upper) -> Density.data


#Add quadrat number by reef and surface orientation (n=20 per orientation)
Cover.data=Cover.data %>% group_by(reef.name, reef.area) %>% 
  mutate(quadratID = paste("Q", sprintf("%03d",1:n()) , sep=""))
Density.data=Density.data %>% group_by(reef.name, reef.area) %>% 
  mutate(quadratID = paste("Q", sprintf("%03d",1:n()) , sep=""))

#move columns(https://stackoverflow.com/questions/3369959/moving-columns-within-a-data-frame-without-retyping)
move.col <- function(df, move_this, next_to_this, before = FALSE) {
  if (before==FALSE)
    df[,c(match(setdiff(names(df)[1:which(names(df)==next_to_this)],move_this),names(df)),
          match(move_this,names(df)),
          match(setdiff(names(df)[which(names(df)==next_to_this):ncol(df)],c(next_to_this,move_this)),names(df)))]
  else
    df[,c(match(setdiff(names(df)[1:(which(names(df)==next_to_this))],c(next_to_this,move_this)),names(df)),
          match(move_this,names(df)),
          match(setdiff(names(df)[(which(names(df)==next_to_this)):ncol(df)],move_this),names(df)))]
}

Cover.data=move.col(Cover.data , "quadratID", "Name")
Density.data=move.col(Density.data , "quadratID", "Name")


#Read taxa list match with WORMS
WORMS_match <- read.csv("WORMS_check_matched.csv")

#Merge density and cover matrix
Cover_Density<- merge(Cover.data,Density.data, all= TRUE) 

#Covert into log format matrix 
library(reshape)
Cover_Density_long = melt(Cover_Density, id.vars = 1:22, measure.vars = 23:ncol(Cover_Density), variable_name = "scientificName", value.name ="value", na.rm = TRUE)

#take out abund/cobertura 0 o NA
library(dplyr)
Cover_Density_long = Cover_Density_long %>% filter(value > 0,!is.na(value))

#separeted columns with abundance and cover data
Cover_Density_long$abundance <- ifelse(Cover_Density_long$CD=="D", Cover_Density_long$value, NA)
Cover_Density_long$cover <- ifelse(Cover_Density_long$CD=="C", Cover_Density_long$value, NA)

#Replace "." with space on the scientific names
Cover_Density_long$scientificName <- chartr(".", " ", Cover_Density_long$scientificName)

#ADD WORMS info
Cover_Density_long = left_join(Cover_Density_long, WORMS_match, by=c("scientificName"="Original_Name"))

#ADD country 
Cover_Density_long <- add_column(Cover_Density_long, country = "ARGENTINA", .after = "Date")
#ADD countryCode
Cover_Density_long <- add_column(Cover_Density_long, countryCode = "AR", .after = "country")
#stateProvince
Cover_Density_long <- add_column(Cover_Density_long, stateProvince = "CHUBUT", .after = "countryCode")
#ADD locality
Cover_Density_long <- add_column(Cover_Density_long, locality = "PTOPIRAMIDES", .after = "region")
#ADD institutionCode
Cover_Density_long <- add_column(Cover_Density_long, institutionCode="CENPAT-CONICET", .after = "reef.name")
#ADDbasisOfRecord
Cover_Density_long <- add_column(Cover_Density_long, basisOfRecord="HumanObservation", .after = "institutionCode")
#ADD individualCount
Cover_Density_long <- add_column(Cover_Density_long, individualCount=Cover_Density_long$abundance*0.0625, .after = "abundance")



#Format date
Cover_Density_long$Date <- as.Date(Cover_Density_long$Date,"%d/%m/%Y")

#ADD year column
Cover_Density_long <- add_column(Cover_Density_long, Year = format(as.Date(Cover_Density_long$Date, format="%d/%m/%Y"),"%Y"), .after = "Date")

#Change names of reefs:
Cover_Density_long$reef.name[Cover_Density_long$reef.name == "SHALLOW_WEST"] <- "SHALLOW1"
Cover_Density_long$reef.name[Cover_Density_long$reef.name == "SHALLOW_EAST"] <- "SHALLOW2"
Cover_Density_long$reef.name[Cover_Density_long$reef.name == "MIDDLE_WEST"] <- "MEDIUM1"
Cover_Density_long$reef.name[Cover_Density_long$reef.name == "MIDDLE_EAST(WE)"] <- "MEDIUM2"
Cover_Density_long$reef.name[Cover_Density_long$reef.name == "MIDDLE_EAST(SN)"] <- "MEDIUM3"
Cover_Density_long$reef.name[Cover_Density_long$reef.name == "DEEP_WEST"] <- "DEEP1"
Cover_Density_long$reef.name[Cover_Density_long$reef.name == "DEEP_EAST"] <- "DEEP2"

#Name = photoquadrat
names(Cover_Density_long)[names(Cover_Density_long) == "Name"] <- "PhotoID"
#recordedBy = photographer
names(Cover_Density_long)[names(Cover_Density_long) == "Photographer"] <- "recordedBy"

#scientificNameID = photographer
names(Cover_Density_long)[names(Cover_Density_long) == "LSID"] <- "scientificNameID"

#individualCount=abundance
#names(Cover_Density_long)[names(Cover_Density_long) == "abundance"] <- "individualCount"

#organismQuantity=cover
#names(Cover_Density_long)[names(Cover_Density_long) == "cover"] <- "organismQuantity"

#Create eventID (Country_Locality_year_site_reef_reefsurfaceorientation_quadrat)
Cover_Density_long <- add_column(Cover_Density_long, eventID = paste(Cover_Density_long$country,Cover_Density_long$locality,Cover_Density_long$Year,Cover_Density_long$site,Cover_Density_long$reef.name,Cover_Density_long$reef.area,Cover_Density_long$quadratID,sep ="_"), .before = "PhotoID")


#Create occurrenceID
Cover_Density_long = Cover_Density_long %>% group_by(eventID) %>% 
  mutate(occurrenceID = paste(eventID, sprintf("%03d",1:n()) , sep="_"))

Cover_Density_long=move.col(Cover_Density_long , "occurrenceID", "eventID")

#Create parentEventID
Cover_Density_long <- add_column(Cover_Density_long, parentEventID = paste(Cover_Density_long$country,Cover_Density_long$locality,Cover_Density_long$Year,Cover_Density_long$site,Cover_Density_long$reef.name,Cover_Density_long$reef.area,sep ="_"), .before = "occurrenceID")



## occurrence----------------------------------------------------------------------------------
rocky.reefs.all = Cover_Density_long %>% select(eventID, occurrenceID, locality=locality, site=site,rockyreef=reef.name, surfaceorientation=reef.area, Quadrat=quadratID,Photo=PhotoID,scientificName=ScientificName_accepted, AphiaID=AphiaID_accepted,group=Group,CD, individualCount=abundance,organismQuantity=cover)


CENPAT-CONICET
## eMoF----------------------------------------------------------------------------------
### Measurement or facts file
## we will do that first for abundance then for cover and them bind both DF 
## abundance
rocky.abun = subset(rocky.reefs.all, CD=="D")
MoF.abund = data.frame(occurrenceID = rocky.abun$occurrenceID, 
                       eventID = rocky.abun$eventID,
                       measurementType = rep("surface area", nrow(rocky.abun)),
                       measurementTypeID = rep("http://vocab.nerc.ac.uk/collection/P01/current/AREABEDS/",nrow(rocky.abun)),
                       measurementValue = as.numeric(rocky.abun$abundance),
                       measurementUnit = rep("m2", nrow(rocky.abun)),
                       measurementUnitID = rep("http://vocab.nerc.ac.uk/collection/P06/current/UMSQ/", nrow(rocky.abun)))

## cover
rocky.cover = subset(rocky.reefs.all, CD=="C")
MoF.cover = data.frame(occurrenceID = rocky.cover$occurrenceID, 
                       eventID = rocky.cover$eventID,
                       measurementType = rep("cover", nrow(rocky.cover)),
                       measurementTypeID = rep("http://vocab.nerc.ac.uk/collection/P01/current/SDBIOL10/",nrow(rocky.cover)),
                       measurementValue = as.numeric(rocky.cover$cover), 
                       measurementUnit = rep("percentage", nrow(rocky.cover)),
                       measurementUnitID = rep("http://vocab.nerc.ac.uk/collection/P06/current/UPCT/", nrow(rocky.cover))) 

rocky.MoF = bind_rows(MoF.abund, MoF.cover)


#eventDF---------------------------------------------------------------------------------

##parentEventID
eventDF = data.frame(eventID = Cover_Density_long$eventID,
                     parentEventID = Cover_Density_long$parentEventID, 
                     eventDate = Cover_Density_long$Date, 
                     country = Cover_Density_long$country,
                     locality = Cover_Density_long$locality,
                     site = Cover_Density_long$site,
                     reef = Cover_Density_long$reef.name,
                     Quadrat = Cover_Density_long$quadratID,  
                     decimalLongitude = Cover_Density_long$Longitude, 
                     decimalLatitude = Cover_Density_long$Latitude,
                     minimumDepthInMeters =  NA,
                     maximumDepthInMeters = NA,
                     coordinateUncertaintyInMeters = NA,
                     geodeticDatum = "WGS84",photo=Cover_Density_long$PhotoID)

#select only the rows with unique photoquadrats 
eventDF <- distinct(eventDF,photo, .keep_all = TRUE)


#1-7 m (n= 2 reefs), 8-15 m (n= 3 reefs) and 16-25 m 
#add rows of reefs in eventID with their others columns  
eventDF <- eventDF %>% add_row(eventID=unique(paste(Cover_Density_long$country,Cover_Density_long$locality,Cover_Density_long$Year,Cover_Density_long$site,Cover_Density_long$reef.name, sep="_")),parentEventID=unique(paste(Cover_Density_long$country,Cover_Density_long$locality,Cover_Density_long$Year,Cover_Density_long$site, sep="_")),country=unique(Cover_Density_long$country),locality=unique(Cover_Density_long$locality),site=unique(Cover_Density_long$site),reef=c("MEDIUM2","SHALLOW1","DEEP1","DEEP2","MEDIUM1","MEDIUM3","SHALLOW1"),minimumDepthInMeters=c(8,1,16,16,8,8,1),maximumDepthInMeters=c(15,7,25,25,15,15,7),.before = 1)

#add row of site 
eventDF <- eventDF %>% add_row(eventID=paste(unique(Cover_Density_long$country), paste(unique(Cover_Density_long$locality), unique(Cover_Density_long$Year), sep="_"), unique(Cover_Density_long$site), sep="_"),.before = 1)

#add eventID country_locality_year_site_reef_reefarea
eventDF <- eventDF %>% add_row(eventID=unique(paste(Cover_Density_long$country,Cover_Density_long$locality,Cover_Density_long$Year,Cover_Density_long$site,Cover_Density_long$reef.name,Cover_Density_long$reef.area, sep="_")),.before = 1)


#add manually in excel=
#eventID                                                   parentID
#ARGENTINA_PUERTOPIRAMIDES-2019_PARDELAS_DEEP1_HORIZONTAL ARGENTINA_PUERTOPIRAMIDES-2019_PARDELAS_DEEP1


## grabo las tablas
fileRoot = paste("ARGENTINA", "PTOPIRAMIDES", "2019", sep="_")
write_csv(Cover_Density_long, path = paste0(fileRoot, "_occurrence2.csv"),na="") ## antes de subir a OBIS hay que eliminar los sustratos
write_csv(eventDF, path = paste0(fileRoot, "_event.csv"))
write_csv(rocky.MoF, path = paste0(fileRoot, "_eMoF.csv"))



#TAXONOMIC COVERAGE

#remove those categories that dont have AphiaID (eg bare substrate)
sppList <- filter(rocky.reefs.all,!is.na(AphiaID))
sppList <-  unique(sppList$scientificName)
## remove NAs
sppList = sppList[!is.na(sppList)]

## Write the names to a text file
writeLines(con="sppList.txt", sppList)
print(sppList)


