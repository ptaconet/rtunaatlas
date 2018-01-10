#' @name wcpfc_catch_level0
#' @aliases wcpfc_catch_level0
#' @title Extract source georeferenced catch datasets of WCPFC from Sardara World database
#' @description This function extracts the source georeferenced catch datasets stored in the Sardara World database coming from the Western and Central Pacific Ocean Fisheries Commission (WCPFC). The output dataset provides the catch of tuna and tuna-like species in the area managed under the jurisdiction of the WCPFC. Catches are stratified by month, species, gear, fishing mode (i.e. type of school used), area (5° square) and unit of catch (weight or number).
#' @export
#'
#' @usage wcpfc_catch_level0(year_tunaatlas)
#'                 
#' @param year_tunaatlas numeric. The year of the datasets to extract. Starts in 2017
#'  
#' @details 
#' The output dataset lists catch of tuna and tuna-like species in the Western Pacific ocean. Catches are stratified by month, species, gear, fishing mode (i.e. type of school used), area (5° square) and unit of catch (weight or number). The dataset is computed using public domain datasets released by the Western and Central Pacific Fisheries Commission (WCPFC).
#' This function merges the primary catch-and-effort datasets released by WCPFC.
#' Output dataset is expressed with WCPFC's coding system.
#' 
#' @family process data
#' 
#' @examples
#' 
#' # Retrieve WCPFC georeferenced catch data from 2017
#' wcpfc_catch<-wcpfc_catch_level0(2017)
#' head(wcpfc_catch)
#' 
#' @author Paul Taconet, \email{paul.taconet@@ird.fr}
#'  

wcpfc_catch_level0<-function(year_tunaatlas){
  
  # Select wcpfc raw datasets release on the year year_tunaatlas
  con <- db_connection_tunaatlas_world()
  
  # retrieves 3 lines. IOTC level0 is only the combination of the 3 IOTC catch-and-effort datasets: indian_ocean_catch_ll_tunaatlasdf_level0 , indian_ocean_catch_tunaatlasdf_level0__coastal , indian_ocean_catch_tunaatlasdf_level0__surface
  
  datasets_permanent_identifiers="'west_pacific_ocean_catch_5deg_1m_ll_tunaatlasWCPFC_level0__1950to1970','west_pacific_ocean_catch_5deg_1m_ll_tunaatlasWCPFC_level0__1990to2000','west_pacific_ocean_catch_5deg_1m_tunaatlasWCPFC_level0__driftnet','west_pacific_ocean_catch_5deg_1m_ll_tunaatlasWCPFC_level0__2000','west_pacific_ocean_catch_5deg_1m_bb_tunaatlasWCPFC_level0','west_pacific_ocean_catch_5deg_1m_ps_tunaatlasWCPFC_level0','west_pacific_ocean_catch_5deg_1m_ll_tunaatlasWCPFC_level0__1970to1980','west_pacific_ocean_catch_5deg_1m_ll_tunaatlasWCPFC_level0__1980to1990'"
  
  metadata_datasets<-dbGetQuery(con,paste0("SELECT * from metadata.metadata where persistent_identifier IN (",datasets_permanent_identifiers,") and identifier LIKE '%_",year_tunaatlas,"_%'"))
  
  # columns for catch
  columns_to_keep<-c("source_authority","species","gear","flag","schooltype","time_start","time_end","geographic_identifier","catchtype","unit","value")
  
  # Retrieve WCPFC georef. catches 
  df_level0<-extract_and_merge_multiple_datasets(con,metadata_datasets,columns_to_keep)
  
  dbDisconnect(con)
  
  df_level0$source_authority<-"WCPFC"
  
  return(df_level0)
  
}