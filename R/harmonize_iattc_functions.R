#' @name iattc_functions
#' @aliases iattc_functions 
#' @title Functions to harmonize the structure of the IATTC datasets
#' @description Set of functions to harmonize the structure of the IATTC datasets
#' @export create_additional_columns_IATTC_CE FUN_catches_IATTC_CE_LLTunaBillfishShark FUN_catches_IATTC_CE_Flag_or_SetType FUN_catches_IATTC_CE_Flag_or_SetType_Shark FUN_efforts_IATTC_CE_LLTunaBillfish FUN_efforts_IATTC_CE_allbutLLTunaBillfish FUN_cas_IATTC IATTC_CE_catches_pivotDSD_to_harmonizedDSD IATTC_CE_efforts_pivotDSD_to_harmonizedDSD
#' @author Paul Taconet, IRD \email{paul.taconet@ird.fr}
#'
#' @family harmonize data structure
#'
# IATTC CE file normalization. The aim is to have a file with the following format: lat, lon, squaresize, codesquaresize, species, catchunit, catch, effortunit, effort

#function to create geographical, catchunits and gear columns
create_additional_columns_IATTC_CE<-function(input_IATTC_CE_file,ColLat,ColLon,SquareSizeDegrees,GearCode){
  
  
  # create geographical columns
  
  colnames(input_IATTC_CE_file)[which(colnames(input_IATTC_CE_file) == ColLat)] <- "Lat"
  colnames(input_IATTC_CE_file)[which(colnames(input_IATTC_CE_file) == ColLon)] <- "Lon"
  input_IATTC_CE_file$SquareSize<-SquareSizeDegrees
  if (SquareSizeDegrees==5){
    input_IATTC_CE_file$CodeSquareSize<-6
  }
  if (SquareSizeDegrees==1){
    input_IATTC_CE_file$CodeSquareSize<-5
  }
  
  
  # create column gear
  input_IATTC_CE_file$Gear<-GearCode
  
  return(input_IATTC_CE_file)
}



# Longline catch data are submitted to the IATTC as numbers of individuals, weight of catch, or
#  both numbers and weight for the same catch. To eliminate duplication of data within a file when
#  the same catch is reported in both numbers and weight, catch and effort data are presented in 2
#  separate files. Data reported only in numbers or only in weight are reported identically in both
# files. However, when the same data are reported in both number and weight, the Number
#  (PublicLLTunaBillfishNum.csv ) file contains number data in the number columns, and the weight
#  data are excluded; likewise, the Weight file (PublicLLTunaBillfishMt.csv ) contains weight data in
#  the weight columns, and the number data are excluded. In this way, the same catch is never
#  duplicated within a data file as both number and weight, and either number or weight data can
#  be used for analysis. No attempt has been made to convert numbers to weight or weight to
#  numbers.


FUN_catches_IATTC_CE_LLTunaBillfishShark<-function(Path_to_IATTC_CE_LLTunaBillfishShark){  
  
  IATTC_CE <- read.table(Path_to_IATTC_CE_LLTunaBillfishShark, sep=",", header=TRUE, stringsAsFactors=FALSE,strip.white=TRUE)
  
  IATTC_CE<-data.table(IATTC_CE)
  IATTC_CE<-melt(IATTC_CE, id.vars=c("Year","Month","Flag","LatC5","LonC5","Hooks")) 
  IATTC_CE<-as.data.frame(IATTC_CE)
  
  IATTC_CE <- IATTC_CE  %>% 
    filter( ! value %in% 0 ) %>%
    filter( ! is.na(value)) 
  
  IATTC_CE$variable<-as.character(IATTC_CE$variable)
  
  
  # create catchunits column
  IATTC_CE$CatchUnits<-"init" 
  index.mt <-grep("mt",IATTC_CE$variable)
  index.n <-grep("n",IATTC_CE$variable)
  
  IATTC_CE[index.mt,"CatchUnits"]<- "MT"
  IATTC_CE[index.n,"CatchUnits"]<- "NO"
  
  IATTC_CE$variable<-gsub("n","",IATTC_CE$variable)
  IATTC_CE$variable<-gsub("mt","",IATTC_CE$variable)
  
  
  IATTC_CE <- create_additional_columns_IATTC_CE(IATTC_CE,"LatC5","LonC5",5,"LL")
  IATTC_CE$SetType<-"ALL"
  
  
  IATTC_CE<-unique(IATTC_CE)
  
  return(IATTC_CE)
  
}



FUN_catches_IATTC_CE_Flag_or_SetType<-function(Path_to_IATTC_CE,aggregation_dimension,GearCode){  #aggregation_dimension="Flag" or "SetType"
  
  IATTC_CE <- read.table(Path_to_IATTC_CE, sep=",", header=TRUE, stringsAsFactors=FALSE,strip.white=TRUE)
  
  IATTC_CE<-data.table(IATTC_CE)
  IATTC_CE<-melt(IATTC_CE, id.vars=c("Year","Month",aggregation_dimension,"LatC1","LonC1","NumSets")) 
  IATTC_CE<-as.data.frame(IATTC_CE)
  
  IATTC_CE <- IATTC_CE  %>% 
    filter( ! value %in% 0 ) %>%
    filter( ! is.na(value)) 
  
  IATTC_CE$variable<-as.character(IATTC_CE$variable)
  
  #Create additional columns
  IATTC_CE <- create_additional_columns_IATTC_CE(IATTC_CE,"LatC1","LonC1",1,GearCode)
  IATTC_CE$CatchUnits<-"MT"
  
  if (aggregation_dimension=="Flag"){
    IATTC_CE$SetType<-"ALL"
  }
  if (aggregation_dimension=="SetType"){
    IATTC_CE$Flag<-"ALL"
  }
  
  return(IATTC_CE)
}


FUN_catches_IATTC_CE_Flag_or_SetType_Shark<-function(Path_to_IATTC_CE,aggregation_dimension,GearCode){
  
  IATTC_CE<-FUN_catches_IATTC_CE_Flag_or_SetType(Path_to_IATTC_CE,aggregation_dimension,GearCode)
  
  
  # create catchunits column
  IATTC_CE$CatchUnits<-"init" 
  index.mt <-grep("mt",IATTC_CE$variable)
  index.n <-grep("n",IATTC_CE$variable)
  
  IATTC_CE[index.mt,"CatchUnits"]<- "MT"
  IATTC_CE[index.n,"CatchUnits"]<- "NO"
  
  IATTC_CE$variable<-gsub("n","",IATTC_CE$variable)
  IATTC_CE$variable<-gsub("mt","",IATTC_CE$variable)
  
  IATTC_CE<-unique(IATTC_CE)
  
  return(IATTC_CE)
}



FUN_efforts_IATTC_CE_LLTunaBillfish<-function(Path_to_IATTC_CE,ColEffortUnits,aggregation_dimension,GearCode){
  
  IATTC_CE <- read.table(Path_to_IATTC_CE, sep=",", header=TRUE, stringsAsFactors=FALSE,strip.white=TRUE)
  
  IATTC_CE<-IATTC_CE[, 1:6]
  
  IATTC_CE <- create_additional_columns_IATTC_CE(IATTC_CE,"LatC5","LonC5",5,GearCode)
  
  # create effortunits column
  IATTC_CE[,"EffortUnits"]<- colnames(IATTC_CE[which(colnames(IATTC_CE) == ColEffortUnits)])
  colnames(IATTC_CE)[which(colnames(IATTC_CE) == ColEffortUnits)] <- "Effort"
  
  
  if (aggregation_dimension=="Flag"){
    IATTC_CE$SetType<-"ALL"
  }
  if (aggregation_dimension=="SetType"){
    IATTC_CE$Flag<-"ALL"
  }
  
  IATTC_CE <- IATTC_CE  %>% 
    filter( ! Effort %in% 0 ) %>%
    filter( ! is.na(Effort)) 
  
  IATTC_CE<-unique(IATTC_CE)
  
  return(IATTC_CE) 
}


FUN_efforts_IATTC_CE_allbutLLTunaBillfish<-function(Path_to_IATTC_CE,ColEffortUnits,aggregation_dimension,GearCode){
  
  IATTC_CE <- read.table(Path_to_IATTC_CE, sep=",", header=TRUE, stringsAsFactors=FALSE,strip.white=TRUE)
  
  IATTC_CE<-IATTC_CE[, 1:6]
  
  IATTC_CE <- create_additional_columns_IATTC_CE(IATTC_CE,"LatC1","LonC1",1,GearCode)
  
  # create effortunits column
  IATTC_CE[,"EffortUnits"]<- colnames(IATTC_CE[which(colnames(IATTC_CE) == ColEffortUnits)])
  colnames(IATTC_CE)[which(colnames(IATTC_CE) == ColEffortUnits)] <- "Effort"
  
  
  if (aggregation_dimension=="Flag"){
    IATTC_CE$SetType<-"ALL"
  }
  if (aggregation_dimension=="SetType"){
    IATTC_CE$Flag<-"ALL"
  }
  
  IATTC_CE <- IATTC_CE  %>% 
    filter( ! Effort %in% 0 ) %>%
    filter( ! is.na(Effort)) 
  
  IATTC_CE<-unique(IATTC_CE)
  
  return(IATTC_CE) 
}




FUN_cas_IATTC<-function(path_to_df_total_catches,path_to_df_frequency,path_to_df_fishery_code){
  
  df_total_catches<-read.csv(path_to_df_total_catches,sep="\t", header=TRUE,stringsAsFactors = F)
  df_frequency<-read.csv(path_to_df_frequency,sep="\t", header=TRUE,stringsAsFactors = F)
  df_fishery_code<-read.csv(path_to_df_fishery_code,sep="\t", header=TRUE,stringsAsFactors = F)
  
  
  
  # reformat df_total_catches
  df_total_catches <- melt(df_total_catches, id=c("Year","season"))
  df_total_catches$variable<-as.character(df_total_catches$variable)
  colnames(df_total_catches)<-c("Year","Season","Flt.Svy","TotalCatches")
  df_total_catches$Flt.Svy<-gsub("F","",df_total_catches$Flt.Svy)
  df_total_catches$Flt.Svy<-as.numeric(df_total_catches$Flt.Svy)
  
  # reformat df_frequency
  df_frequency$Part<-NULL
  df_frequency$Nsamp<-NULL
  
  df_frequency <- melt(df_frequency, id=c("Yr","Seas","Flt.Svy","Gender"))
  colnames(df_frequency)<-c("Year","Season","Flt.Svy","Gender","freq_class","value")
  
  df_frequency$value<-as.character(df_frequency$value)
  df_frequency$value<-gsub(",",".",df_frequency$value)
  df_frequency$value<-as.double(df_frequency$value)
  df_frequency <- df_frequency  %>% filter( ! value %in% 0 )
  
  df_frequency$freq_class<-as.character(df_frequency$freq_class)
  df_frequency$freq_class<-gsub("X","",df_frequency$freq_class)
  df_frequency$freq_class<-as.numeric(df_frequency$freq_class)
  
  # reformat df_total_catches
  df_fishery_code$FisheryNum<-NULL
  df_fishery_code$FisheryAlpha<-NULL  
  df_fishery_code$Years<-NULL
  
  # merge df_frequency and df_total_catches
  cas<-merge(df_frequency,df_total_catches,by=c("Year","Season","Flt.Svy"),all.x=TRUE,all.y=FALSE)
  cas$cas<-cas$value*cas$TotalCatches
  
  ## get info (year, season, gear)
  cas<-merge(cas,df_fishery_code,all.x=TRUE,all.y=FALSE,by="Flt.Svy")
  #for the time being we keep only the PS values as I did not understand much on the LL data...
  cas <- cas  %>% filter( ! Gear %in% "LL" )
  
  return(cas)
  
}



# Catch: intermediate data sample:
# Year Month Flag  Lat    Lon variable value CatchUnits SquareSize CodeSquareSize Gear SetType
# 1992     7  USA 27.5 -137.5      BSH     4         NO          5              6   LL     ALL
# 1993     4  USA 27.5 -137.5      BSH    75         NO          5              6   LL     ALL
# 1993     4  USA 32.5 -137.5      BSH    15         NO          5              6   LL     ALL
# 1993     5  USA 27.5 -137.5      BSH    24         NO          5              6   LL     ALL
# 1994     3  USA 27.5 -137.5      BSH    14         NO          5              6   LL     ALL
# 1994     3  USA 32.5 -137.5      BSH     4         NO          5              6   LL     ALL


# Cath: final data sample:
# Flag Gear time_start   time_end AreaName School Species CatchType CatchUnits Catch
#  USA   LL 1992-07-01 1992-08-01  6425135    ALL     BSH       ALL         NO     4
#  USA   LL 1993-04-01 1993-05-01  6425135    ALL     BSH       ALL         NO    75
#  USA   LL 1993-04-01 1993-05-01  6430135    ALL     BSH       ALL         NO    15
#  USA   LL 1993-05-01 1993-06-01  6425135    ALL     BSH       ALL         NO    24
#  USA   LL 1994-03-01 1994-04-01  6425135    ALL     BSH       ALL         NO    14
#  USA   LL 1994-03-01 1994-04-01  6430135    ALL     BSH       ALL         NO     4


IATTC_CE_catches_pivotDSD_to_harmonizedDSD<-function(IATTC_CE_catches_df_pivotDSD,colToKeep_captures){
  
  ##Catches
  
  #RFMO
  IATTC_CE_catches_df_pivotDSD$RFMO<-"IATTC"
  
  #Ocean
  IATTC_CE_catches_df_pivotDSD$Ocean<-"PAC_E"
  
  #Flag
  index.flag.other <- which( IATTC_CE_catches_df_pivotDSD[,"Flag"] == "Other" )
  IATTC_CE_catches_df_pivotDSD[index.flag.other,"Flag"]<- "OTR"
  
  #Gear
  #OK
  
  #Year and period
  IATTC_CE_catches_df_pivotDSD<-harmo_time_2(IATTC_CE_catches_df_pivotDSD,"Year","Month")

  # Area (AreaType,AreaCWPgrid,AreaName)
  IATTC_CE_catches_df_pivotDSD<-harmo_spatial_4(IATTC_CE_catches_df_pivotDSD,"Lat","Lon","SquareSize","CodeSquareSize")
  
  
  #School
  #index.ls <- which( IATTC_CE_catches_df_pivotDSD[,"SetType"] == "OBJ" )
  #IATTC_CE_catches_df_pivotDSD[index.ls,"SetType"]<- "LS"
  #index.fs <- which( IATTC_CE_catches_df_pivotDSD[,"SetType"] == "NOA" )
  #IATTC_CE_catches_df_pivotDSD[index.fs,"SetType"]<- "FS"
  #index.del <- which( IATTC_CE_catches_df_pivotDSD[,"SetType"] == "DEL" )
  #IATTC_CE_catches_df_pivotDSD[index.del,"SetType"]<- "DEL"
  
  colnames(IATTC_CE_catches_df_pivotDSD)[which(colnames(IATTC_CE_catches_df_pivotDSD) == "SetType")] <- "School"
  
  #Species
  colnames(IATTC_CE_catches_df_pivotDSD)[which(colnames(IATTC_CE_catches_df_pivotDSD) == "variable")] <- "Species"
  
  #CatchType
  IATTC_CE_catches_df_pivotDSD$CatchType<-"ALL"
  
  #CatchUnits
  #ok             
  
  #Catch
  IATTC_CE_catches_df_pivotDSD$Catch<-IATTC_CE_catches_df_pivotDSD$value
  
  
  catches <-IATTC_CE_catches_df_pivotDSD[colToKeep_captures]
  
  rm(IATTC_CE_catches_df_pivotDSD)
  
  
  #remove whitespaces on columns that should not have withespace
  catches[,c("AreaName","Flag")]<-as.data.frame(apply(catches[,c("AreaName","Flag")],2,function(x){gsub(" *$","",x)}),stringsAsFactors=FALSE)
  
  # remove 0 and NA values 
  catches <- catches  %>% 
    filter( ! Catch %in% 0 ) %>%
    filter( ! is.na(Catch)) 
  
  catches <- catches %>% 
    group_by(Flag,Gear,time_start,time_end,AreaName,School,Species,CatchType,CatchUnits) %>% 
    summarise(Catch = sum(Catch))
  catches<-as.data.frame(catches)
  
  return(catches)
}




# Effort: intermediate data sample:
# Year Month Flag   Lat    Lon Effort SquareSize CodeSquareSize Gear EffortUnits SetType
# 1979     1  KOR -22.5 -127.5  62901          5              6   LL       Hooks     ALL
# 1979     1  KOR -22.5 -122.5  75482          5              6   LL       Hooks     ALL
# 1979     1  KOR -17.5 -132.5  41705          5              6   LL       Hooks     ALL
# 1979     1  KOR -17.5 -127.5  23322          5              6   LL       Hooks     ALL
# 1979     1  KOR -17.5 -122.5  42136          5              6   LL       Hooks     ALL
# 1979     1  KOR -12.5 -147.5  26128          5              6   LL       Hooks     ALL


# Effort: final data sample:
# Flag Gear time_start   time_end AreaName School EffortUnits Effort
#  BLZ   LL 2009-01-01 2009-02-01  6400100    ALL       Hooks 122400
#  BLZ   LL 2009-01-01 2009-02-01  6400110    ALL       Hooks  22900
#  BLZ   LL 2009-01-01 2009-02-01  6405100    ALL       Hooks  45000
#  BLZ   LL 2009-01-01 2009-02-01  6405105    ALL       Hooks  24300
#  BLZ   LL 2009-01-01 2009-02-01  6405110    ALL       Hooks  37050
#  BLZ   LL 2009-01-01 2009-02-01  6405115    ALL       Hooks  39000


IATTC_CE_efforts_pivotDSD_to_harmonizedDSD<-function(IATTC_CE_efforts_df_pivotDSD,colToKeep_efforts){

#RFMO
IATTC_CE_efforts_df_pivotDSD$RFMO<-"IATTC"

#Ocean
IATTC_CE_efforts_df_pivotDSD$Ocean<-"PAC_E"

#Flag
index.flag.other <- which( IATTC_CE_efforts_df_pivotDSD[,"Flag"] == "Other" )
IATTC_CE_efforts_df_pivotDSD[index.flag.other,"Flag"]<- "OTR"

#Gear
#OK

#Year and period
IATTC_CE_efforts_df_pivotDSD<-harmo_time_2(IATTC_CE_efforts_df_pivotDSD,"Year","Month")

# Area (AreaType,AreaCWPgrid,AreaName)
IATTC_CE_efforts_df_pivotDSD<-harmo_spatial_4(IATTC_CE_efforts_df_pivotDSD,"Lat","Lon","SquareSize","CodeSquareSize")


#School
#index.ls <- which( IATTC_CE_efforts_df_pivotDSD[,"SetType"] == "OBJ" )
#IATTC_CE_efforts_df_pivotDSD[index.ls,"SetType"]<- "LS"
#index.fs <- which( IATTC_CE_efforts_df_pivotDSD[,"SetType"] == "NOA" )
#IATTC_CE_efforts_df_pivotDSD[index.fs,"SetType"]<- "FS"
#index.del <- which( IATTC_CE_efforts_df_pivotDSD[,"SetType"] == "DEL" )
#IATTC_CE_efforts_df_pivotDSD[index.del,"SetType"]<- "DEL"

colnames(IATTC_CE_efforts_df_pivotDSD)[which(colnames(IATTC_CE_efforts_df_pivotDSD) == "SetType")] <- "School"


#EffortUnits
#OK

#Effort
#OK


efforts<-IATTC_CE_efforts_df_pivotDSD[colToKeep_efforts]

rm(IATTC_CE_efforts_df_pivotDSD)


#remove whitespaces on columns that should not have withespace
efforts[,c("AreaName","Flag")]<-as.data.frame(apply(efforts[,c("AreaName","Flag")],2,function(x){gsub(" *$","",x)}),stringsAsFactors=FALSE)

# remove 0 and NA values 
efforts <- efforts  %>% 
  filter( ! Effort %in% 0 ) %>%
  filter( ! is.na(Effort)) 

efforts <- efforts %>% 
  group_by(Flag,Gear,time_start,time_end,AreaName,School,EffortUnits) %>% 
  summarise(Effort = sum(Effort))  
efforts<-as.data.frame(efforts)

return(efforts)
}


