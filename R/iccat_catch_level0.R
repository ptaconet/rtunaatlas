#' @name iccat_catch_level0
#' @aliases iccat_catch_level0
#' @title Extract source georeferenced catch datasets of ICCAT from Sardara World database
#' @description This function extracts the source georeferenced catch datasets stored in the Sardara World database coming from the International Commission for the Conservation of Atlantic Tuna (ICCAT). The output dataset provides the catch of tuna, tuna-like and by-catch in the area managed under the jurisdiction of the ICCAT Catches are stratified by month, species, gear, vessel flag reporting country, fishing mode (i.e. type of school used), area (usualy 1° or 5° square) and unit of catch (weight or number).
#' @export
#'
#' @usage iccat_catch_level0(year_tunaatlas,include_type_of_school)
#'                 
#' @param year_tunaatlas numeric. The year of the datasets to extract. Starts in 2017
#' @param include_type_of_school boolean. Set TRUE if you want the output dataset with school type stratification. FALSE will provide the stratification without the type of school. See details for more information.

#' @details 
#' The output dataset lists catch of tuna, tuna-like and shark species in the Atlantic ocean. Catches are stratified by month, species, gear, vessel flag reporting country, fishing mode (i.e. type of school used) - if parameter \code{include_type_of_school} is set to TRUE, area (1° or 5° square) and unit of catch (weight or number). This dataset is computed using public domain datasets released by the International Commission for the Conservation of Atlantic Tuna (ICCAT).
#' 
#' Details on the use of the parameter \code{include_type_of_school}: ICCAT delivers two catch-and-efforts datasets for purse seiners: one that gives the detail of the type of school (Fad|Free school) for purse seine fisheries and that starts in 1994 (called Task II catch|effort by operation mode Fad|Free school) and one that does not give the information of the type of school and that covers all the time period (from 1950) (called Task II catch|effort). These data are redundant (i.e. the data from the dataset Task II catch|effort by operation mode are also available in the dataset Task II catch|effort) but in the latter, the information on the type of school is not available.
#' \itemize{
#' \item{ If the parameter \code{include_type_of_school} is set to TRUE, both datasets will be combined to produce a dataset that covers the whole time period, with fishing mode information (Fad | free school).}
#' \item{ If the parameter \code{include_type_of_school} is set to FALSE, only the dataset without the type of school information will be used. The output dataset will hence not have the stratification by type of school. }
#' }
#' 
#' Output dataset is expressed with ICCAT's coding system.
#' 
#' @family process data
#' 
#' @examples
#' 
#' # Retrieve ICCAT georeferenced catch data from 2017, with information on fishing mode
#' iccat_catch<-iccat_catch_level0(2017,include_type_of_school=TRUE)
#' head(iccat_catch)
#' 
#' @author Paul Taconet, \email{paul.taconet@@ird.fr}
#' 


iccat_catch_level0<-function(year_tunaatlas,include_type_of_school){
  
  # Select iccat raw datasets release on the year year_tunaatlas
  
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, dbname="sardara_world", user="invsardara", password="fle087", host="db-tuna.d4science.org")

  datasets_permanent_identifiers="'atlantic_ocean_catch_tunaatlasICCAT_level0__noSchool'"
  
  metadata_datasets_WithoutSchooltypeInfo<-dbGetQuery(con,paste0("SELECT * from metadata.metadata where dataset_permanent_identifier IN (",datasets_permanent_identifiers,") and dataset_name LIKE '%_",year_tunaatlas,"_%'"))
  
  # columns for catch
  columns_to_keep<-c("source_authority","species","gear","flag","schooltype","time_start","time_end","geographic_identifier","catchtype","catchunit","value")
  
  iccat_ce_WithoutSchooltypeInfo<-extract_and_merge_multiple_datasets(con,metadata_datasets_WithoutSchooltypeInfo,columns_to_keep)
  
  if (include_type_of_school==TRUE){
    # Retrieve ICCAT dataset with schooltype information (task2 by operation mode) (https://goo.gl/f2jz5R). We do not use the template (template_query_catches) because flag code list used in iccat task2 by operation mode dataset is different from flag code list used in ICCAT task2; however we have to use the same flag code list for data raising. In other words, we express all ICCAT datasets following ICCAT task2 flag code list.
   datasets_permanent_identifiers="'atlantic_ocean_catch_1deg_1m_ps_tunaatlasICCAT_level0__bySchool'"
   metadata_datasets_WithSchooltypeInfo<-dbGetQuery(con,paste0("SELECT * from metadata.metadata where dataset_permanent_identifier IN (",datasets_permanent_identifiers,") and dataset_name LIKE '%_",year_tunaatlas,"_%'"))
    
    # We need to map flag code list, because flag code list used in iccat task2 by operation mode dataset is different from flag code list used in ICCAT task2; however we have to use the same flag code list for data raising. In other words, we express all ICCAT datasets following ICCAT task2 flag code list.
    
    iccat_ce_WithSchooltypeInfo<-extract_and_merge_multiple_datasets(con,metadata_datasets_WithSchooltypeInfo,columns_to_keep)

    strata_in_withoutschooltype_and_not_in_withshooltype<-anti_join (iccat_ce_WithoutSchooltypeInfo,iccat_ce_WithSchooltypeInfo,by=setdiff(columns_to_keep,c("value","schooltype")))
    
    # Join datasets: Dataset with the type of school + dataset without the type of school from which we have removed the strata that are also available in the dataset with the type of school.
    df_level0<-rbind(strata_in_withoutschooltype_and_not_in_withshooltype,iccat_ce_WithSchooltypeInfo)
    
  } else {
    df_level0<-iccat_ce_WithoutSchooltypeInfo
  }
  
  dbDisconnect(con)
  
  df_level0$source_authority<-"ICCAT"
  
  return(df_level0)
  
}
