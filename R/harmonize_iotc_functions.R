#' @name iotc_functions
#' @aliases iotc_functions 
#' @title Functions to harmonize the structure of the IOTC datasets
#' @description Set of functions to harmonize the structure of the IOTC datasets
#' @export FUN_catches_IOTC_CE FUN_efforts_IOTC_CE FUN_cas_IOTC IOTC_CE_catches_pivotDSD_to_harmonizedDSD IOTC_CE_effort_pivotDSD_to_harmonizedDSD
#' @author Paul Taconet, IRD \email{paul.taconet@ird.fr}
#'
#' @family harmonize data structure

# Path_to_CE_dataset: Path to the IOTC CE dataset
# last_column_not_catch_value : Position of the last column that is not a catch value column in the CE file
FUN_catches_IOTC_CE<-function(Path_to_CE_dataset,last_column_not_catch_value,CE_dataset_nature){
  
  # read files
  IOTC_CE <- read.table(Path_to_CE_dataset, sep=",", header=TRUE, stringsAsFactors=FALSE,strip.white=TRUE)  
  
  #remove spaces in the columns fleet and iGrid
  IOTC_CE[,c("Fleet","iGrid")]<-as.data.frame(apply(IOTC_CE[,c("Fleet","iGrid")],2,function(x){gsub(" *$","",x)}),stringsAsFactors=FALSE)
  
  # convert NAs to 0s
  for (i  in 1:dim(IOTC_CE)[2]){
    index <- which(is.na(IOTC_CE[,i]))
    
    if (length(index)>0){
      IOTC_CE[index,i]<- 0
    }
  }
  
  # Normalize 
  IOTC_CE<-data.table(IOTC_CE)
  IOTC_CE<-melt(IOTC_CE, id.vars=colnames(IOTC_CE)[1:last_column_not_catch_value])
  IOTC_CE<-as.data.frame(IOTC_CE)
  
  # remove values=0
  IOTC_CE <- IOTC_CE  %>% 
    filter( ! value %in% 0 ) %>%
    filter( ! is.na(value)) 
  
  # remove all that is before (after?) the point
  IOTC_CE$Species<-sub('\\..*', '', IOTC_CE$variable)
  
  if (CE_dataset_nature=="Surface"){
    IOTC_CE$School<-sub('.*\\.', '', IOTC_CE$variable)
    #indiceSchoolUNCL <- which(IOTC_CE$School=="UNCL")
    #IOTC_CE[indiceSchoolUNCL,"School"]="ALL"
  } else {
  
    IOTC_CE$CatchUnits<-sub('.*\\.', '', IOTC_CE$variable)
    
    IOTC_CE$School<-"ALL"
  }
  
  # Check data that exist both in number and weight
  
  number_of_units_by_strata<- group_by_(IOTC_CE,.dots=setdiff(colnames(IOTC_CE),c("value","CatchUnits","variable"))) %>%
    summarise(count = n())
  
  strata_in_number_and_weight<-number_of_units_by_strata[number_of_units_by_strata$count>1,]
  
  IOTC_CE<-left_join (IOTC_CE,strata_in_number_and_weight,by=setdiff(colnames(strata_in_number_and_weight),"count"))
  
  index.catchinweightandnumber <- which(IOTC_CE[,"count"]==2 & IOTC_CE[,"CatchUnits"]=="NO")
  IOTC_CE[index.catchinweightandnumber,"CatchUnits"]="NOMT"
  
  index.catchinweightandnumber <- which(IOTC_CE[,"count"]==2 & IOTC_CE[,"CatchUnits"]=="MT")
  IOTC_CE[index.catchinweightandnumber,"CatchUnits"]="MTNO"
  
  
  return(IOTC_CE)
}



# Combine and normalize IOTC CE for efforts
# Note: This function gives a school type to an effort. If a record in the raw data is linked to only one type of catch (free school or log school), then the effort is linked to this type of school. Else, the effort school in unknown.

FUN_efforts_IOTC_CE<-function(Path_to_CE_dataset,last_column_not_catch_value){  
  
  efforts_pivot_IOTC <- read.table(Path_to_CE_dataset, sep=",", header=TRUE, stringsAsFactors=FALSE,strip.white=TRUE)  
  
  FScolumns<-colnames(efforts_pivot_IOTC)[grep("-FS",colnames(efforts_pivot_IOTC))]
  LScolumns<-colnames(efforts_pivot_IOTC)[grep("-LS",colnames(efforts_pivot_IOTC))]
  UNCLcolumns<-colnames(efforts_pivot_IOTC)[grep("-UNCL",colnames(efforts_pivot_IOTC))]
  
  
  efforts_pivot_IOTC$sumFS<- rowSums(efforts_pivot_IOTC[, FScolumns],na.rm = TRUE)
  efforts_pivot_IOTC$sumLS<- rowSums(efforts_pivot_IOTC[, LScolumns],na.rm = TRUE)
  efforts_pivot_IOTC$sumUNCL<- rowSums(efforts_pivot_IOTC[, UNCLcolumns],na.rm = TRUE)
  
  indexFS<-which(efforts_pivot_IOTC$sumFS>0 & efforts_pivot_IOTC$sumLS==0 & efforts_pivot_IOTC$sumUNCL==0 )
  indexLS<-which(efforts_pivot_IOTC$sumFS==0 & efforts_pivot_IOTC$sumLS!=0 & efforts_pivot_IOTC$sumUNCL==0 )
  indexUNCL<-setdiff(rownames(efforts_pivot_IOTC),c(indexFS,indexLS))
  
  efforts_pivot_IOTC<-efforts_pivot_IOTC[,-(last_column_not_catch_value:ncol(efforts_pivot_IOTC))] 
  
  efforts_pivot_IOTC$SchoolEffort<-"IND"
  efforts_pivot_IOTC[indexFS,"SchoolEffort"]<- "FS"
  efforts_pivot_IOTC[indexLS,"SchoolEffort"]<- "LS"
  efforts_pivot_IOTC[indexUNCL,"SchoolEffort"]<- "UNCL"
  
  efforts_pivot_IOTC <- efforts_pivot_IOTC  %>% 
    filter( ! Effort %in% 0 )   
  
  return (efforts_pivot_IOTC)
  
}


FUN_cas_IOTC<-function(Path_to_cas_dataset,vector_cas_classes,FirstClassLow_column,SizeInterval_column){
  
  
  cas_harm_IOTC <- read.csv(Path_to_cas_dataset,header=TRUE, stringsAsFactors=FALSE,strip.white=TRUE,quote = "")
  
  cas_harm_IOTC<-data.table(cas_harm_IOTC)
  cas_harm_IOTC<-melt(cas_harm_IOTC, id=setdiff(colnames(cas_harm_IOTC),vector_cas_classes)) 
  cas_harm_IOTC<-as.data.frame(cas_harm_IOTC)
  
  cas_harm_IOTC <- cas_harm_IOTC  %>% 
    filter( ! value %in% 0 ) %>%
    filter( ! is.na(value)) 
  
  cas_harm_IOTC$variable<-gsub("T", "", cas_harm_IOTC$variable)
  cas_harm_IOTC$variable<-as.numeric(cas_harm_IOTC$variable)
  cas_harm_IOTC$value<-as.numeric(cas_harm_IOTC$value)
  
  colnames(cas_harm_IOTC)[which(colnames(cas_harm_IOTC)=="variable")]<-"Class"
  colnames(cas_harm_IOTC)[which(colnames(cas_harm_IOTC)=="value")]<-"Catch"
  
  cas_harm_IOTC$SizeMin<-cas_harm_IOTC[,FirstClassLow_column]+(cas_harm_IOTC[,SizeInterval_column]*(cas_harm_IOTC$Class-1))
  

  
  return(cas_harm_IOTC)
  
}



IOTC_CE_catches_pivotDSD_to_harmonizedDSD<-function(catches_pivot_IOTC,colToKeep_captures){

#RFMO
catches_pivot_IOTC$RFMO<-"IOTC"

#Ocean
catches_pivot_IOTC$Ocean<-"IND"

#Flag
catches_pivot_IOTC$Flag<-catches_pivot_IOTC$Fleet

#Gear
catches_pivot_IOTC$Gear<-catches_pivot_IOTC$Gear

#Year and period
catches_pivot_IOTC<-harmo_time_3(catches_pivot_IOTC, "Year", "MonthStart", "MonthEnd")

#AreaCWPgrid
catches_pivot_IOTC$AreaCWPgrid<-catches_pivot_IOTC$Grid

#AreaName
catches_pivot_IOTC$AreaName<-catches_pivot_IOTC$iGrid

#AreaType
catches_pivot_IOTC<-harmo_spatial_2(catches_pivot_IOTC,"AreaName")

#School
catches_pivot_IOTC$School<-catches_pivot_IOTC$School

#Species
catches_pivot_IOTC$Species<-catches_pivot_IOTC$Species

#CatchUnits
catches_pivot_IOTC$CatchUnits<-catches_pivot_IOTC$CatchUnits

#CatchType
catches_pivot_IOTC$CatchType<-"ALL"

#Catch
catches_pivot_IOTC$Catch<-catches_pivot_IOTC$value


catches <-catches_pivot_IOTC[colToKeep_captures]

rm(catches_pivot_IOTC)


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



IOTC_CE_effort_pivotDSD_to_harmonizedDSD<-function(efforts_pivot_IOTC,colToKeep_efforts){
# remove 0 and NA values 
efforts_pivot_IOTC <- efforts_pivot_IOTC  %>% 
  filter( ! Effort %in% 0 )    %>%
  filter( ! is.na(Effort) )   


#RFMO
efforts_pivot_IOTC$RFMO<-"IOTC"

#Ocean
efforts_pivot_IOTC$Ocean<-"IND"

#Flag
efforts_pivot_IOTC$Flag<-efforts_pivot_IOTC$Fleet

#Gear
efforts_pivot_IOTC$Gear<-efforts_pivot_IOTC$Gear

#Year and period
efforts_pivot_IOTC<-harmo_time_3(efforts_pivot_IOTC, "Year", "MonthStart", "MonthEnd")

#AreaCWPgrid
efforts_pivot_IOTC$AreaCWPgrid<-efforts_pivot_IOTC$Grid

#AreaName
efforts_pivot_IOTC$AreaName<-efforts_pivot_IOTC$iGrid

#AreaType
efforts_pivot_IOTC<-harmo_spatial_2(efforts_pivot_IOTC,"AreaName")

#School
efforts_pivot_IOTC$School<-efforts_pivot_IOTC$SchoolEffort

#EffortUnits
efforts_pivot_IOTC$EffortUnits<-efforts_pivot_IOTC$EffortUnits

#Effort
efforts_pivot_IOTC$Effort<-efforts_pivot_IOTC$Effort



efforts<-efforts_pivot_IOTC[colToKeep_efforts]

rm(efforts_pivot_IOTC)


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









