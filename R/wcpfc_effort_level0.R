#' @name wcpfc_effort_level0
#' @aliases wcpfc_effort_level0
#' @title Extract source georeferenced efforts datasets of WCPFC from the Tuna atlas database
#' @description This function extracts the source georeferenced effort datasets stored in the Tuna atlas database coming from the Western and Central Pacific Ocean Fisheries Commission (WCPFC). The output dataset provides efforts of tuna, tuna-like and shark species in the Western pacific ocean. Efforts are stratified by year, month, fishing mode, unit of effort, area (5° squares). Data are expressed using WCPFC's coding system.
#' @export
#'
#' @usage wcpfc_effort_level0(year_tunaatlas)
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
#' # Retrieve wcpfc georeferenced efforts data from 2017
#' wcpfc_effort<-wcpfc_effort_level0(2017)
#' head(wcpfc_effort)
#' 
#' @author Paul Taconet, \email{paul.taconet@@ird.fr}
#' @import RPostgreSQL   

wcpfc_effort_level0<-function(year_tunaatlas){
  
  # Select wcpfc raw datasets release on the year year_release
  con <- db_connection_tunaatlas_world()
  
  # retrieves 3 lines. wcpfc level0 is only the combination of the 3 wcpfc catch-and-effort datasets: indian_ocean_effort_ll_tunaatlasdf_level0 , indian_ocean_effort_tunaatlasdf_level0__coastal , indian_ocean_effort_tunaatlasdf_level0__surface
  
  datasets_permanent_identifiers="'west_pacific_ocean_effort_5deg_1m_ll_tunaatlasWCPFC_level0__1950to1970','west_pacific_ocean_effort_5deg_1m_ll_tunaatlasWCPFC_level0__1990to2000','west_pacific_ocean_effort_5deg_1m_tunaatlasWCPFC_level0__driftnet','west_pacific_ocean_effort_5deg_1m_ll_tunaatlasWCPFC_level0__2000','west_pacific_ocean_effort_5deg_1m_bb_tunaatlasWCPFC_level0','west_pacific_ocean_effort_5deg_1m_ps_tunaatlasWCPFC_level0','west_pacific_ocean_effort_5deg_1m_ll_tunaatlasWCPFC_level0__1970to1980','west_pacific_ocean_effort_5deg_1m_ll_tunaatlasWCPFC_level0__1980to1990'"

  metadata_datasets<-dbGetQuery(con,paste0("SELECT * from metadata.metadata where persistent_identifier IN (",datasets_permanent_identifiers,") and identifier LIKE '%_",year_tunaatlas,"_level0%'"))
  
  # columns for efforts
  columns_to_keep<-c("source_authority","gear","flag","schooltype","time_start","time_end","geographic_identifier","unit","value")
  
  # Retrieve wcpfc georef. catches 
  df_level0<-extract_and_merge_multiple_datasets(con,metadata_datasets,columns_to_keep)
  
  dbDisconnect(con)
  
  return(df_level0)
  
}