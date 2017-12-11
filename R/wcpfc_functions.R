#' @name wcpfc_functions
#' @aliases wcpfc_functions 
#' @title Functions to harmonize the structure of the WCPFC datasets
#' @description Set of functions to harmonize the structure of the WCPFC datasets
#' @export FUN_catches_WCPFC_CE_allButPurseSeine FUN_catches_WCPFC_CE_Purse_Seine FUN_catches_WCPFC_CE_Purse_Seine_2016 FUN_efforts_WCPFC_CE_Purse_Seine_2016 FUN_efforts_WCPFC_CE FUN_cas_WCPFC WCPFC_CE_catches_pivotDSD_to_harmonizedDSD WCPFC_CE_efforts_pivotDSD_to_harmonizedDSD 
#' @author Paul Taconet, IRD \email{paul.taconet@ird.fr}
#'
#' @family harmonize data structure
#' 
# normalize WCPFC CE
# fonction to 1) normalize and 2) Create column to know if catch is expressed in number or in weigth
# Works for Drifnet, Longline and Pole and line but not Purse Seine (since purse seine has a school type detail). function for purse seine is under.
FUN_catches_WCPFC_CE_allButPurseSeine <-function(DF_Path) {
  DF<-read.dbf(DF_Path,as.is=TRUE)
  
  DF<-melt(DF, id=c(colnames(DF[1:5]))) 

  DF<- DF %>% 
    filter( ! value %in% 0 ) %>%
    filter( ! is.na(value)) 
  DF$variable<-as.character(DF$variable)
  colnames(DF)[which(colnames(DF) == "variable")] <- "Species"
  
  DF$CatchUnits<-substr(DF$Species, nchar(DF$Species), nchar(DF$Species))
  
  DF$Species<-sub('_C', '', DF$Species)
  DF$Species<-sub('_N', '', DF$Species)
  
  DF$School<-"OTH"
  
  DF$EffortUnits<-colnames(DF[5])    
  colnames(DF)[5]<-"Effort"
  
  return(DF)
}


FUN_catches_WCPFC_CE_Purse_Seine <-function(DF_Path) {
  #we consider that the catches are all given in tons
  DF<-read.dbf(DF_Path,as.is=TRUE)
  
  DF<-melt(DF, id=c(colnames(DF[1:5]))) 
  
  DF<- DF %>% 
    filter( ! value %in% 0 ) %>%
    filter( ! is.na(value)) 
  DF$variable<-as.character(DF$variable)
  colnames(DF)[which(colnames(DF) == "variable")] <- "Species"
  
  DF$School<-substr(DF$Species, nchar(DF$Species)-2, nchar(DF$Species))
  
  DF$Species<-sub('_C_UNA', '', DF$Species)
  DF$Species<-sub('_C_ASS', '', DF$Species)
  DF$Species<-sub('_C_OTH', '', DF$Species)
  
  DF$CatchUnits<-"MT"
  
  DF$EffortUnits<-colnames(DF[5])    
  colnames(DF)[5]<-"Effort"
  
  return(DF)
}


FUN_catches_WCPFC_CE_Purse_Seine_2016 <-function(DF_Path) {
  #we consider that the catches are all given in tons
  DF<-read.dbf(DF_Path,as.is=TRUE)
  
  DF<-melt(DF, id=c(colnames(DF[1:10]))) 
  
  DF<- DF %>% 
    filter( ! value %in% 0 ) %>%
    filter( ! is.na(value)) 
  DF$variable<-as.character(DF$variable)
  colnames(DF)[which(colnames(DF) == "variable")] <- "Species"
  
  DF$School<-substr(DF$Species, 7, nchar(DF$Species))
  
  DF$Species<-sub('_C_UNA', '', DF$Species)
  DF$Species<-sub('_C_LOG', '', DF$Species)
  DF$Species<-sub('_C_DFAD', '', DF$Species)
  DF$Species<-sub('_C_AFAD', '', DF$Species)
  DF$Species<-sub('_C_OTH', '', DF$Species)
  
  DF$CatchUnits<-"MT"
  
  DF$EffortUnits<-colnames(DF[5])    
  colnames(DF)[5]<-"Effort"
  
  return(DF)
}


FUN_efforts_WCPFC_CE_Purse_Seine_2016 <-function(DF_Path) {
  #there are 2 efforts: one in number of days and one in 
  DF<-read.dbf(DF_Path,as.is=TRUE)
  
  # effort unit in days
    DF_effortunit_days<-DF[,1:5]
  DF_effortunit_days$EffortUnits<-"DAYS"
  colnames(DF_effortunit_days)[5]<-"Effort"
  DF_effortunit_days$School<-"ALL"
  
  DF_effortunit_days<- DF_effortunit_days %>% 
    filter( ! Effort %in% 0 ) %>%
    filter( ! is.na(Effort)) 

  

  # effort unit in sets
  DF_effortunit_sets<-DF[,1:10]
  DF_effortunit_sets<-DF_effortunit_sets[,-(5)]
  DF_effortunit_sets<-melt(DF_effortunit_sets, id=c(colnames(DF_effortunit_sets[1:4]))) 
  DF_effortunit_sets$EffortUnits<-"SETS"
  colnames(DF_effortunit_sets)[5]<-"School" 
  DF_effortunit_sets$School<-as.character(DF_effortunit_sets$School)
  
  DF_effortunit_sets$School<-substr(DF_effortunit_sets$School, 6, nchar(DF_effortunit_sets$School))
  colnames(DF_effortunit_sets)[6]<-"Effort"
  
  DF_effortunit_sets<- DF_effortunit_sets %>% 
    filter( ! Effort %in% 0 ) %>%
    filter( ! is.na(Effort)) 
  
  
DF<-rbind(DF_effortunit_days,DF_effortunit_sets)
  
  return(DF)
}



FUN_efforts_WCPFC_CE <-function(DF_Path) {  
  
  DF<-read.dbf(DF_Path,as.is=TRUE)
  
  DF <- DF[c(1:5)] 
  DF$EffortUnits<-colnames(DF[5])    
  colnames(DF)[5]<-"Effort"
  DF$School<-"ALL" 
  
  return(DF)
}


FUN_cas_WCPFC <-function(DF_Path) {  
  
  
  WCPFC_CAS <- read.csv(DF_Path,header=TRUE, stringsAsFactors=FALSE,strip.white=TRUE,quote = "")
  
  WCPFC_CAS<-data.table(WCPFC_CAS)
  WCPFC_CAS <- melt(WCPFC_CAS, id=c("Year","len")) 
  WCPFC_CAS<-as.data.frame(WCPFC_CAS)
  
  WCPFC_CAS$variable<-as.character(WCPFC_CAS$variable)
  
  WCPFC_CAS <- WCPFC_CAS  %>% 
    filter( ! value %in% 0 ) %>%
    filter( ! is.na(value)) 
  
  colnames(WCPFC_CAS)[which(colnames(WCPFC_CAS)=="value")]<-"Catch"
  colnames(WCPFC_CAS)[which(colnames(WCPFC_CAS)=="variable")]<-"Gear"
  colnames(WCPFC_CAS)[which(colnames(WCPFC_CAS)=="len")]<-"SizeMin"
  
  #School
  #For the school
  indexFS<-grep("_UNA",WCPFC_CAS$Gear)
  indexLS<-grep("_ASS",WCPFC_CAS$Gear)
  indexUNCL<- setdiff (rownames(WCPFC_CAS),c(indexFS,indexLS))
  
  WCPFC_CAS[indexFS,"School"]<- "FS"
  WCPFC_CAS[indexLS,"School"]<- "LS"
  WCPFC_CAS[indexUNCL,"School"]<- "ALL"
  
  return(WCPFC_CAS)
}



WCPFC_CE_catches_pivotDSD_to_harmonizedDSD<-function(catches_pivot_WCPFC,colToKeep_captures){
  
  
  
  #RFMO
  catches_pivot_WCPFC$RFMO<-"WCPFC"
  
  #Ocean
  catches_pivot_WCPFC$Ocean<-"PAC_W"
  
  #Flag
  catches_pivot_WCPFC$Flag<-"ALL"
  
  #Gear
  #OK
  
  #Year and period
  catches_pivot_WCPFC<-harmo_time_2(catches_pivot_WCPFC,"YY","MM")

  # Area (AreaType,AreaCWPgrid,AreaName)
  catches_pivot_WCPFC<-harmo_spatial_3(catches_pivot_WCPFC,"LAT5","LON5",5,6)
  
  #School
  #indice.school.oth<- which(catches_pivot_WCPFC$School=="OTH")
  #indice.school.una<- which(catches_pivot_WCPFC$School=="UNA")
  #indice.school.ass<- which(catches_pivot_WCPFC$School=="ASS")
  
  #catches_pivot_WCPFC$School[indice.school.oth]<-"OTH"
  #catches_pivot_WCPFC$School[indice.school.una]<-"FS"
  #catches_pivot_WCPFC$School[indice.school.ass]<-"LS"
  
  #Species
  #OK
  
  #CatchType
  catches_pivot_WCPFC$CatchType<-"ALL"
  
  
  #Catch
  catches_pivot_WCPFC$Catch<-catches_pivot_WCPFC$value

  catches <-catches_pivot_WCPFC[colToKeep_captures]
  
  rm(catches_pivot_WCPFC)
  
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
  


WCPFC_CE_efforts_pivotDSD_to_harmonizedDSD<-function(efforts_pivot_WCPFC,colToKeep_efforts){
  
#RFMO
efforts_pivot_WCPFC$RFMO<-"WCPFC"

#efforts_pivot_WCPFC
efforts_pivot_WCPFC$Ocean<-"PAC_W"

#Flag
efforts_pivot_WCPFC$Flag<-"ALL"

#Gear
#OK

#Year and period
efforts_pivot_WCPFC<-harmo_time_2(efforts_pivot_WCPFC,"YY","MM")

# Area (AreaType,AreaCWPgrid,AreaName)
efforts_pivot_WCPFC<-harmo_spatial_3(efforts_pivot_WCPFC,"LAT5","LON5",5,6)

#School
#OK

#EffortUnits
#OK

efforts<-efforts_pivot_WCPFC[colToKeep_efforts]

rm(efforts_pivot_WCPFC)



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