#' @name iotc_effort_level0
#' @aliases iotc_effort_level0
#' @title Extract source georeferenced efforts datasets of IOTC from the Tuna atlas database
#' @description This function extracts the source georeferenced effort datasets stored in the Tuna atlas database coming from the Indian Ocean Tuna Commission (IOTC). The output dataset provides efforts of tuna, tuna-like and shark species in the Indian ocean. Efforts are stratified by month, gear, vessel flag reporting country, fishing mode (i.e. type of school used), area (usualy 1° or 5° square) and unit of effort. Data are expressed using IOTC's coding system.
#' @export
#'
#' @usage iotc_effort_level0(year_tunaatlas)
#'                 
#' @param year_tunaatlas numeric. The year of the datasets to extract. Starts in 2017
#'  
#' @details 
#' 
#'
#' @family process data
#' 
#' 
#' @examples
#' 
#' # Retrieve IOTC georeferenced efforts data from 2017
#' iotc_effort<-iotc_effort_level0(2017)
#' head(iotc_effort)
#' 
#' @author Paul Taconet, \email{paul.taconet@@ird.fr}
#' @import RPostgreSQL   

iotc_effort_level0<-function(year_tunaatlas){
  
  # Select iotc raw datasets release on the year year_release
  con <- db_connection_tunaatlas_world()
  
  # retrieves 3 lines. IOTC level0 is only the combination of the 3 IOTC catch-and-effort datasets: indian_ocean_effort_ll_tunaatlasdf_level0 , indian_ocean_effort_tunaatlasdf_level0__coastal , indian_ocean_effort_tunaatlasdf_level0__surface
  
  datasets_permanent_identifiers="'indian_ocean_effort_ll_tunaatlasIOTC_level0','indian_ocean_effort_tunaatlasIOTC_level0__coastal','indian_ocean_effort_tunaatlasIOTC_level0__surface'"
  
  metadata_datasets<-dbGetQuery(con,paste0("SELECT * from metadata.metadata where persistent_identifier IN (",datasets_permanent_identifiers,") and identifier LIKE '%_",year_tunaatlas,"_level0%'"))
  
  # columns for efforts
  columns_to_keep<-c("source_authority","gear","flag","schooltype","time_start","time_end","geographic_identifier","unit","value")
  
  # Retrieve IOTC georef. catches 
  df_level0<-extract_and_merge_multiple_datasets(con,metadata_datasets,columns_to_keep)
  
  dbDisconnect(con)
  
  return(df_level0)
  
}