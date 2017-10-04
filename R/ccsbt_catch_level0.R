#' @name ccsbt_catch_level0
#' @aliases ccsbt_catch_level0
#' @title Extract source georeferenced catch datasets of ccsbt from Sardara World database
#' @description This function extracts the source georeferenced catch datasets stored in the Sardara World database coming from the Commission for the Conservation of Southern Bluefin Tuna (CCSBT). The output dataset provides the catch of tuna and tuna-like species in the area managed under the jurisdiction of the CCSBT Catches are stratified by month, species, gear, vessel flag reporting country, area (1° or 5° square) and unit of catch (weight or number).
#' @export
#'
#' @usage ccsbt_catch_level0(year_tunaatlas)
#'                 
#' @param year_tunaatlas numeric. The year of the datasets to extract. Starts in 2017
#'  
#' @details 
#' The output dataset lists catch of tuna and tuna-like species in the Southern hemisphere oceans. Catches are stratified by month, species, gear, vessel flag reporting country, area (1° or 5° square) and unit of catch (weight or number). The dataset is computed using public domain datasets released by the CCSBT.
#' This function merges the primary catch-and-effort datasets released by CCSBT
#' Output dataset is expressed with CCSBT's coding system.
#' 
#' @family tRFMOs datasets extraction
#' @family datasets extraction
#' 
#' @examples
#' 
#' # Retrieve ccsbt georeferenced catch data from 2017
#' ccsbt_catch<-ccsbt_catch_level0(2017)
#' head(ccsbt_catch)
#' 
#' @author Paul Taconet, \email{paul.taconet@@ird.fr}
#'  

ccsbt_catch_level0<-function(year_tunaatlas){
  
  # Select ccsbt raw datasets release on the year year_tunaatlas
  
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, dbname="sardara_world", user="invsardara", password="fle087", host="db-tuna.d4science.org")
  
  # retrieves 3 lines. IOTC level0 is only the combination of the 3 IOTC catch-and-effort datasets: indian_ocean_catch_ll_tunaatlasdf_level0 , indian_ocean_catch_tunaatlasdf_level0__coastal , indian_ocean_catch_tunaatlasdf_level0__surface
  datasets_permanent_identifiers="'southern_hemisphere_oceans_catch_1deg_1m_tunaatlasCCSBT_level0__surface','southern_hemisphere_oceans_catch_5deg_1m_ll_tunaatlasCCSBT_level0'"
  
  metadata_datasets<-dbGetQuery(con,paste0("SELECT * from metadata.metadata where dataset_permanent_identifier IN (",datasets_permanent_identifiers,") and dataset_name LIKE '%_",year_tunaatlas,"_%'"))
  
  # columns for catch
  columns_to_keep<-c("source_authority","species","gear","flag","schooltype","time_start","time_end","geographic_identifier","catchtype","catchunit","value")
  
  # Retrieve CCSBT georef. catches 
  df_level0<-extract_and_merge_multiple_datasets(con,metadata_datasets,columns_to_keep)
  
  dbDisconnect(con)
  
  df_level0$source_authority<-"CCSBT"
  
  return(df_level0)
  
}