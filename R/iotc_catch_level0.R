#' @name iotc_catch_level0
#' @aliases iotc_catch_level0
#' @title Extract source georeferenced catch datasets of IOTC from Sardara World database
#' @description This function extracts the source georeferenced catch datasets stored in the Sardara World database coming from the Indian Ocean Tuna Commission (IOTC). The output dataset provides the catch of tuna, tuna-like and by-catch in the area managed under the jurisdiction of the IOTC. Catches are stratified by month, species, gear, vessel flag reporting country, fishing mode (i.e. type of school used), area (usualy 1° or 5° square) and unit of catch (weight or number). Data are expressed using IOTC's coding system.
#' @export
#'
#' @usage iotc_catch_level0(year_tunaatlas)
#'                 
#' @param year_tunaatlas numeric. The year of the datasets to extract. Starts in 2017
#'  
#' @details More information (metadata) regarding the dataset ouput of this function can be found at \href{URLOF DATASET IN CATALOGUE TO ADD}{this URL} on the \href{http://geonetwork2-tunaatlas.d4science.org/geonetwork/srv/fre/catalog.search#/search}{Tuna Atlas catalogue}
#' 
#' @family extract data
#' 
#' 
#' @examples
#' 
#' # Retrieve IOTC georeferenced catch data from 2017
#' iotc_catch<-iotc_catch_level0(2017)
#' head(iotc_catch)
#' 
#' @author Paul Taconet, \email{paul.taconet@@ird.fr}
#' @import RPostgreSQL   

iotc_catch_level0<-function(year_tunaatlas){
  
  # Select iotc raw datasets release on the year year_tunaatlas
  
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, dbname="sardara_world", user="invsardara", password="fle087", host="db-tuna.d4science.org")
  
  # retrieves 3 lines. IOTC level0 is only the combination of the 3 IOTC catch-and-effort datasets: indian_ocean_catch_ll_tunaatlasIOTC_level0 , indian_ocean_catch_tunaatlasIOTC_level0__coastal , indian_ocean_catch_tunaatlasIOTC_level0__surface
  
  datasets_permanent_identifiers="'indian_ocean_catch_ll_tunaatlasIOTC_level0','indian_ocean_catch_tunaatlasIOTC_level0__coastal','indian_ocean_catch_tunaatlasIOTC_level0__surface'"
  
  metadata_datasets<-dbGetQuery(con,paste0("SELECT * from metadata.metadata where dataset_permanent_identifier IN (",datasets_permanent_identifiers,") and dataset_name LIKE '%_",year_tunaatlas,"_%'"))
  
  # columns for catch
  columns_to_keep<-c("source_authority","species","gear","flag","schooltype","time_start","time_end","geographic_identifier","catchtype","catchunit","value")
  
  # Retrieve IOTC georef. catches 
  iotc_level0<-NULL
  for (i in 1:nrow(metadata_datasets)){
    cat(paste0("\nretrieving data from dataset ",metadata_datasets$dataset_name[i]))
    iotc_level0_thisdf<-extract_dataset(con,metadata_datasets[i])
    
    # keep only wanted columns
    iotc_level0_thisdf <- iotc_level0_thisdf[(names(iotc_level0_thisdf) %in% columns_to_keep)]
    
    # add missing columns and fill them with "UNK" values
    for (j in 1:length(columns_to_keep)){
      if (!(columns_to_keep[j]) %in% names(iotc_level0_thisdf)){
        cat(paste0("\ndimension ",columns_to_keep[j]," is missing in the dataset. Adding this dimension to the dataset and filling values of this dimension with UNK (unknown)"))
        iotc_level0_thisdf[,columns_to_keep[j]]<-"UNK"
      }
    }
    
    iotc_level0<-rbind(iotc_level0,iotc_level0_thisdf)
  }
  
  dbDisconnect(con)
  
  iotc_level0$source_authority<-"IOTC"
  
  return(iotc_level0)
  
}