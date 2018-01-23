#' @name ccsbt_effort_level0
#' @aliases ccsbt_effort_level0
#' @title Extract source georeferenced efforts datasets of CCSBT from the Tuna atlas database
#' @description This function extracts the source georeferenced effort datasets stored in the Tuna atlas database coming from the Commission for the Conservation of Southern Bluefin Tuna (CCSBT). The output dataset provides efforts targetting tuna, tuna-like and shark species in the area managed under the jurisdiction of the CCSBT. Efforts are stratified by year, month, fishing gear, fishing country, unit of effort, area (5° squares). Data are expressed using CCSBT's coding system.
#' @export
#'
#' @usage ccsbt_effort_level0(year_tunaatlas)
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
#' # Retrieve ccsbt georeferenced efforts data from 2017
#' ccsbt_effort<-ccsbt_effort_level0(2017)
#' head(ccsbt_effort)
#' 
#' @author Paul Taconet, \email{paul.taconet@@ird.fr}
#' @import RPostgreSQL   

ccsbt_effort_level0<-function(year_tunaatlas){
  
  # Select ccsbt raw datasets release on the year year_release
  con <- db_connection_tunaatlas_world()
  
  # retrieves 3 lines. ccsbt level0 is only the combination of the 3 ccsbt catch-and-effort datasets: indian_ocean_effort_ll_tunaatlasdf_level0 , indian_ocean_effort_tunaatlasdf_level0__coastal , indian_ocean_effort_tunaatlasdf_level0__surface
  
  datasets_permanent_identifiers="'southern_hemisphere_oceans_effort_1deg_1m_tunaatlasCCSBT_level0__surface','southern_hemisphere_oceans_effort_5deg_1m_ll_tunaatlasCCSBT_level0'"
  
  metadata_datasets<-dbGetQuery(con,paste0("SELECT * from metadata.metadata where persistent_identifier IN (",datasets_permanent_identifiers,") and identifier LIKE '%_",year_tunaatlas,"_level0%'"))
  
  # columns for efforts
  columns_to_keep<-c("source_authority","gear","flag","schooltype","time_start","time_end","geographic_identifier","unit","value")
  
  # Retrieve ccsbt georef. catches 
  df_level0<-extract_and_merge_multiple_datasets(con,metadata_datasets,columns_to_keep)
  
  dbDisconnect(con)
  
  return(df_level0)
  
}