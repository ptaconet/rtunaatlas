#' @name iccat_functions
#' @aliases iccat_functions 
#' @title Functions to harmonize the structure of the ICCAT datasets
#' @description Set of functions to harmonize the structure of the ICCAT datasets
#' @export FUN_catches_ICCAT_CE FUN_efforts_ICCAT_CE_without_schooltype FUN_efforts_ICCAT_CE_with_schooltype FUN_efforts_ICCAT_CE_keep_all_efforts FUN_cas_ICCAT FUN_catchtypeinfo_iccat ICCAT_CE_catches_pivotDSD_to_harmonizedDSD ICCAT_CE_effort_pivotDSD_to_harmonizedDSD
#' @author Paul Taconet, IRD \email{paul.taconet@ird.fr}
#'
#' @family harmonize data structure
#' 
# Normalize ICCAT Catch and effort file
FUN_catches_ICCAT_CE<-function(RFMO_CE,RFMO_CE_species_colnames){


  for (i  in 1:dim(RFMO_CE)[2]){
    index <- which(is.na(RFMO_CE[,i]))
    
    if (length(index)>0){
      RFMO_CE[index,i]<- 0
    }
  }
  
  RFMO_CE<-data.table(RFMO_CE)
  # @juldebar => use dpplyr gather instead (to avoid crashes) ?
  # melt / gather species columns in key value pairs and remove 0 and NA values 
  RFMO_CE<-melt(RFMO_CE, id.vars=setdiff(colnames(RFMO_CE),RFMO_CE_species_colnames))   %>% filter( ! value %in% 0 ) %>% filter( ! is.na(value)) %>% mutate(variable = as.character(variable)) 
  
  #@juldebar => the line below was crashing my R sesssion : I switched it with lines above to use as data frame conversion at the end
  RFMO_CE<-as.data.frame(RFMO_CE)
  # class(RFMO_CE)
  # nrow(RFMO_CE)
  # head(RFMO_CE)
  
  return(RFMO_CE)
  
}



FUN_efforts_ICCAT_CE_without_schooltype<-function(RFMO_CE,RFMO_CE_species_colnames){
  
  # if data in csv: efforts_pivot_ICCAT <- read.table(Path_to_CE_dataset, sep=",", header=TRUE, stringsAsFactors=FALSE,strip.white=TRUE)
  
  efforts_pivot_ICCAT<-RFMO_CE[ , -which(names(RFMO_CE) %in% RFMO_CE_species_colnames)]
  efforts_pivot_ICCAT<-efforts_pivot_ICCAT[ , -which(names(efforts_pivot_ICCAT) %in% c("DSetType","CatchUnit"))]
  
  
  efforts_pivot_ICCAT <- efforts_pivot_ICCAT  %>% 
    filter( ! Eff1 %in% 0 ) 
  
  # only keep unique records. 
  efforts_pivot_ICCAT<-  unique(efforts_pivot_ICCAT)
  
  return(efforts_pivot_ICCAT)
  
}

#This function is very close to the function FUN_efforts_IOTC_CE

FUN_efforts_ICCAT_CE_with_schooltype<-function(RFMO_CE,last_column_not_catch_value){
  
  # if data in csv: efforts_pivot_ICCAT <- read.table(Path_to_CE_dataset, sep=",", header=TRUE, stringsAsFactors=FALSE,strip.white=TRUE)
  
  FScolumns<-colnames(RFMO_CE)[grep("fs",colnames(RFMO_CE))]
  LScolumns<-colnames(RFMO_CE)[grep("fd",colnames(RFMO_CE))]
  
  RFMO_CE$sumFS<- rowSums(RFMO_CE[, FScolumns],na.rm = TRUE)
  RFMO_CE$sumLS<- rowSums(RFMO_CE[, LScolumns],na.rm = TRUE)
  
  indexFS<-which(RFMO_CE$sumFS>0 & RFMO_CE$sumLS==0  )
  indexLS<-which(RFMO_CE$sumFS==0 & RFMO_CE$sumLS!=0  )
  indexUNCL<-setdiff(rownames(RFMO_CE),c(indexFS,indexLS))
  
  RFMO_CE<-RFMO_CE[,-(last_column_not_catch_value:ncol(RFMO_CE))] 
  
  
  RFMO_CE$School<-"ALL"
  RFMO_CE[indexFS,"School"]<- "fs"
  RFMO_CE[indexLS,"School"]<- "fd"
  RFMO_CE[indexUNCL,"School"]<- "ALL"
  
  return(RFMO_CE)
  
}



FUN_efforts_ICCAT_CE_keep_all_efforts<-function(CE_dataset_after_FUN_efforts_ICCAT_CE,vector_colnames_efforts,vector_colnames_effortunits){

#colnames_efforts<-c("Eff1","Eff2","Eff3","Eff4","Eff5")
#colnames_effortunits<-c("Eff1Type","Eff2Type","Eff3Type","Eff4Type","Eff5Type")

colnames_colsToKeep<-setdiff(colnames(CE_dataset_after_FUN_efforts_ICCAT_CE),c(vector_colnames_efforts,vector_colnames_effortunits))


efforts_pivot_ICCAT2<-NULL

for (i in 1:length(vector_colnames_efforts)){
  
  eff_dataset<-CE_dataset_after_FUN_efforts_ICCAT_CE[colnames_colsToKeep]
  
  efforts_pivot_ICCAT_temp<-cbind.data.frame(eff_dataset,CE_dataset_after_FUN_efforts_ICCAT_CE[,vector_colnames_efforts[i]],CE_dataset_after_FUN_efforts_ICCAT_CE[,vector_colnames_effortunits[i]],stringsAsFactors = FALSE)
  
  colnames(efforts_pivot_ICCAT_temp)<-c(colnames_colsToKeep,"Effort","EffortUnits")
  
  efforts_pivot_ICCAT2<-rbind.data.frame(efforts_pivot_ICCAT2,efforts_pivot_ICCAT_temp)
  
}
  
efforts_pivot_ICCAT2$Effort<-gsub(",",".",efforts_pivot_ICCAT2$Effort)
efforts_pivot_ICCAT2$Effort<-as.numeric(efforts_pivot_ICCAT2$Effort)

return(efforts_pivot_ICCAT2)

}


## Normalize ICCAT CAS
FUN_cas_ICCAT<-function(Path_to_cas_dataset,vector_cas_classes,dataset_name){
  
  if(dataset_name=="casBET7514_details_v2"){
    encod="UTF-8"
  }
  if(dataset_name=="casSKJ6913_v1"){
    encod="latin1"
  }
  if(dataset_name=="casYFT7010_ffmt_v2"){
    encod="latin1"
  }
  
  cas_harm_ICCAT <- read.csv(Path_to_cas_dataset,header=TRUE, stringsAsFactors=FALSE,strip.white=TRUE,quote = "",encoding = encod)
 
  cas_harm_ICCAT<-data.table(cas_harm_ICCAT)
  cas_harm_ICCAT<-melt(cas_harm_ICCAT, id.vars=setdiff(colnames(cas_harm_ICCAT),vector_cas_classes)) 
  cas_harm_ICCAT<-as.data.frame(cas_harm_ICCAT)
  
  cas_harm_ICCAT <- cas_harm_ICCAT  %>% 
    filter( ! value %in% 0 ) %>%
    filter( ! is.na(value)) 
  
  cas_harm_ICCAT$variable<-gsub("X", "", cas_harm_ICCAT$variable)
  cas_harm_ICCAT$variable<-as.numeric(cas_harm_ICCAT$variable)
  
  return(cas_harm_ICCAT)
}

FUN_catchtypeinfo_iccat<-function(dataset,CatchTypeInfoAvailable,CatchTypeColName){
  
  if (CatchTypeInfoAvailable=="FALSE"){
    dataset$CatchType="ALL"
  }
  
  if (CatchTypeInfoAvailable=="TRUE"){
    colnames(dataset)[which(colnames(dataset) == CatchTypeColName)] <- "CatchType"
    # In some datasets, the discards dead are coded as D or DD. In the DB, it's DD. So if the dataset is coded with a D, we change it to DD.
    index.discard <- which( dataset[,"CatchType"] == "D" )
    dataset[index.discard,"CatchType"]<- "DD"
  }
  
  return(dataset)
}


#FUN_merge_flag_codelist<-function(Path_to_ICCAT_flag_NCandCAS_CE_mapping_codelist,df_tomerge,flag_colname_df_tomerge){
  
  
 # ICCAT_flag_NCandCAS_CE_mapping_codelist<-read.csv(Path_to_ICCAT_flag_NCandCAS_CE_mapping_codelist,stringsAsFactors = F,encoding="latin1")
  
 # df_tomerge<-data.table(df_tomerge)
#  df_tomerge<-merge(df_tomerge,ICCAT_flag_NCandCAS_CE_mapping_codelist,by.x=flag_colname_df_tomerge,by.y="flag_code_in_NcandCas",all.x=T)
#  df_tomerge<-as.data.frame(df_tomerge)
  
#  df_tomerge$Flag<-NULL
#  colnames(df_tomerge)[which(colnames(df_tomerge) == "flag_code_in_CE_and_in_db")] <- "Flag"
  
#  return(df_tomerge)
  
#}



ICCAT_CE_catches_pivotDSD_to_harmonizedDSD<-function(catches_pivot_ICCAT,colToKeep_captures){

#RFMO
catches_pivot_ICCAT$RFMO<-"ICCAT"

#Ocean
catches_pivot_ICCAT$Ocean<-"ATL"

#Flag
#OK

#Gear
catches_pivot_ICCAT$Gear<-catches_pivot_ICCAT$GearCode

#Year and period
# @juldebar => this function takes forever
cat("Harmonization of time dimension values \n")
catches_pivot_ICCAT<-harmo_time_1(catches_pivot_ICCAT, "YearC", "TimePeriodID")

# Area (AreaType,AreaCWPgrid,AreaName)
# @juldebar => this function takes forever
cat("Harmonization of spatial dimension values \n")
catches_pivot_ICCAT<-harmo_spatial_1(catches_pivot_ICCAT,"Lon","Lat","QuadID","SquareTypeCode",NULL)

#School
#OK

#Species
catches_pivot_ICCAT$Species<-catches_pivot_ICCAT$variable

#CatchType
#In iccat an unknown catchtype is flagged as 'C' (i.e. "catches")
catches_pivot_ICCAT$CatchType<-"C"



#Catch
catches_pivot_ICCAT$Catch<-catches_pivot_ICCAT$value
# Catches in weight are given in kg. We divide by 1000 to have them in tons

index.kg <- which( catches_pivot_ICCAT[,"CatchUnits"] == "MT" | catches_pivot_ICCAT[,"CatchUnits"] == "MTNO")
catches_pivot_ICCAT[index.kg,"Catch"]<- catches_pivot_ICCAT[index.kg,"Catch"]/1000


catches <-catches_pivot_ICCAT[colToKeep_captures]

rm(catches_pivot_ICCAT)


#remove whitespaces on columns that should not have withespace
# @juldebar => change "Flag" label to "FishingFleet"
catches[,c("AreaName","FishingFleet")]<-as.data.frame(apply(catches[,c("AreaName","FishingFleet")],2,function(x){gsub(" *$","",x)}),stringsAsFactors=FALSE)

# remove 0 and NA values 
catches <- catches  %>% 
  filter( ! Catch %in% 0 ) %>%
  filter( ! is.na(Catch)) 

# @juldebar => change "Flag" label to "FishingFleet"
catches <- catches %>% 
  group_by(FishingFleet,Gear,time_start,time_end,AreaName,School,Species,CatchType,CatchUnits) %>% 
  summarise(Catch = sum(Catch))
catches<-as.data.frame(catches)


return(catches)
}




ICCAT_CE_effort_pivotDSD_to_harmonizedDSD<-function(efforts_pivot_ICCAT,colToKeep_efforts){
  
  efforts_pivot_ICCAT <- efforts_pivot_ICCAT  %>% 
    filter( ! Effort %in% 0 ) %>%
    filter( ! is.na(Effort)) %>%
    filter( ! EffortUnits %in% "NULL") 
  
  
  #RFMO
  efforts_pivot_ICCAT$RFMO<-"ICCAT"
  
  #Ocean
  efforts_pivot_ICCAT$Ocean<-"ATL"
  
  #Flag
  #OK
  
  #Gear
  efforts_pivot_ICCAT$Gear<-efforts_pivot_ICCAT$GearCode
  
  #Year and period
  efforts_pivot_ICCAT<-harmo_time_1(efforts_pivot_ICCAT, "YearC", "TimePeriodID")

  # Area (AreaType,AreaCWPgrid,AreaName)
  efforts_pivot_ICCAT<-harmo_spatial_1(efforts_pivot_ICCAT,"Lon","Lat","QuadID","SquareTypeCode",NULL)
  
  #School
  #OK
  
  #EffortUnits
  #OK
  
  #Effort
  #OK
  
  
  efforts<-efforts_pivot_ICCAT[colToKeep_efforts]
  
  rm(efforts_pivot_ICCAT)
  
  
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






