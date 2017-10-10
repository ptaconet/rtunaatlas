#' @name iattc_catch_level0
#' @aliases iattc_catch_level0
#' @title Extract source georeferenced catch datasets of IATTC from Sardara World database
#' @description This function extracts the source georeferenced catch datasets stored in the Sardara World database coming from the Inter American Tropical Tuna Commission (IATTC). The output dataset provides the catches of tuna, tuna-like and shark species in the East Pacific ocean. Catch are stratified by month, species, gear, vessel flag reporting country, fishing mode (i.e. type of school used), area (1° or 5° square) and unit of catch (weight or number). This dataset was computed using public domain datasets released by the IATTC.
#' @export
#'
#' @usage iattc_catch_level0(year_tunaatlas)
#'                 
#' @param year_tunaatlas numeric. The year of the datasets to extract. Starts in 2017
#' @param raise_flags_to_typeofschool boolean. Raise dataset with flag stratification to dataset with schooltype stratification? See section Details for more information.
#' @param dimension_to_use_if_no_raising_flags_to_typeofschool NULL or string. Not nullable if \code{raise_flags_to_typeofschool} is set to FALSE. if not NULL, either set to "flag" or "schooltype"
#'  
#' @details 
#' The output dataset lists catch of tuna, tuna-like and shark species in the Eastern Atlantic ocean. Catch are stratified by month, species, gear, vessel flag reporting country, fishing mode (i.e. type of school used), area (1° or 5° square) and unit of catch (weight or number). This dataset is computed using public domain datasets released by the Indian Ocean Tuna Commission (iattc).
#' Primary longline and pole-and-line datasets are simply merged.
#' 
#' Details on the use of the parameter \code{raise_flags_to_typeofschool}: For confidentiality policies, information on flag and school type for the geo-referenced catches is available in separate files for Purse seine datasets.
#' \itemize{
#' \item{ If the parameter \code{raise_flags_to_typeofschool} is set to TRUE, for each stratum, the catch from the flag-detailed dataset will be raised to the catch from the school type-detailed dataset to get an estimation of the catches by flag and school type in each stratum.}
#' \item{ If the parameter \code{raise_flags_to_typeofschool} is set to FALSE, one single dataset will be used and in this case, the parameter \code{dimension_to_use_if_no_raising_flags_to_typeofschool} must be filled in: }
#'  \itemize{
#' \item{ If the parameter \code{dimension_to_use_if_no_raising_flags_to_typeofschool} is set to "flag", the data with flag information will be used.}
#' \item{ If the parameter \code{dimension_to_use_if_no_raising_flags_to_typeofschool} is set to "schooltype", the data with schooltype information will be used. }
#' }
#' }
#' 
#' Output dataset is expressed with IATTC's coding system.
#' 
#' @family process data
#' 
#' @examples
#' 
#' # Retrieve IATTC georeferenced catch data from 2017, with dataset with flag dimension raised to dataset with schoolytpe dimension
#' iattc_catch<-iattc_catch_level0(2017,raise_flags_to_typeofschool=TRUE)
#' head(iattc_catch)
#' 
#' @author Paul Taconet, \email{paul.taconet@@ird.fr}
#'   

iattc_catch_level0<-function(year_tunaatlas,raise_flags_to_typeofschool,dimension_to_use_if_no_raising_flags_to_typeofschool=NULL){
  
  # columns for catch
  columns_to_keep<-c("source_authority","species","gear","flag","schooltype","time_start","time_end","geographic_identifier","catchtype","catchunit","value")
  
  # Select iattc raw datasets release on the year year_tunaatlas
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, dbname="sardara_world", user="invsardara", password="fle087", host="db-tuna.d4science.org")
  
  # The data that are not Purse Seine do not suffer any correction for level 0. They are taken as distributed by IATTC.
  datasets_permanent_identifiers_notPS="'east_pacific_ocean_catch_1deg_1m_bb_tunaatlasIATTC_level0__tuna_byFlag','east_pacific_ocean_catch_5deg_1m_ll_tunaatlasIATTC_level0__tuna_billfish','east_pacific_ocean_catch_5deg_1m_ll_tunaatlasIATTC_level0__shark'"
  metadata_datasets_notPS<-dbGetQuery(con,paste0("SELECT * from metadata.metadata where dataset_permanent_identifier IN (",datasets_permanent_identifiers_notPS,") and dataset_name LIKE '%_",year_tunaatlas,"_%'"))
  
  # Datasets that are stratified by schooltype but not by flag
  datasets_permanent_identifiers_PSSetType="'east_pacific_ocean_catch_1deg_1m_ps_tunaatlasIATTC_level0__billfish_bySchool','east_pacific_ocean_catch_1deg_1m_ps_tunaatlasIATTC_level0__shark_bySchool','east_pacific_ocean_catch_1deg_1m_ps_tunaatlasIATTC_level0__tuna_bySchool'"
  metadata_datasets_PSSetType<-dbGetQuery(con,paste0("SELECT * from metadata.metadata where dataset_permanent_identifier IN (",datasets_permanent_identifiers_PSSetType,") and dataset_name LIKE '%_",year_tunaatlas,"_%'"))
  
  # Datasets that are stratified by flag but not by schooltype
  datasets_permanent_identifiers_PSFlag="'east_pacific_ocean_catch_1deg_1m_ps_tunaatlasIATTC_level0__billfish_byFlag','east_pacific_ocean_catch_1deg_1m_ps_tunaatlasIATTC_level0__shark_byFlag','east_pacific_ocean_catch_1deg_1m_ps_tunaatlasIATTC_level0__tuna_byFlag'"
  metadata_datasets_PSFlag<-dbGetQuery(con,paste0("SELECT * from metadata.metadata where dataset_permanent_identifier IN (",datasets_permanent_identifiers_PSFlag,") and dataset_name LIKE '%_",year_tunaatlas,"_%'"))
  
  # Retrieve IATTC georef. catches not Purse Seine
  df_iattc_notps<-extract_and_merge_multiple_datasets(con,metadata_datasets_notPS,columns_to_keep)

  # Retrieve IATTC georef. catches Purse Seine by Schooltype
  df_iattc_PSSetType<-extract_and_merge_multiple_datasets(con,metadata_datasets_PSSetType,columns_to_keep)

  # Retrieve IATTC georef. catches Purse Seine by Flag
  df_iattc_PSFlag<-extract_and_merge_multiple_datasets(con,metadata_datasets_PSFlag,columns_to_keep)
  
  # If user decided to raise flags to type of school:  The Purse Seine data are raised (schooltype raised to flag)
  if (raise_flags_to_typeofschool==TRUE){
    cat(paste0("\nraising flags to schooltype"))
    
    iattc_flag_raised_to_schooltype<-raise_datasets_by_dimension(df1=df_iattc_PSFlag,
                                                              df2=df_iattc_PSSetType,
                                                              dimension_missing_df1="schooltype",
                                                              dimension_missing_df2="flag")
    
    df_level0<-rbind(df_iattc_notps,iattc_flag_raised_to_schooltype)
    
  } else {  # If user decides to not raise flags to type of school, he chooses to use either the data with stratification by flag or the data with stratification by schooltype
    
    cat(paste0("\nkeeping dataset with information on ",dimension_to_use_if_no_raising_flags_to_typeofschool))
    
    if (dimension_to_use_if_no_raising_flags_to_typeofschool=='flag'){
      df_level0<-rbind(df_iattc_notps,df_iattc_PSFlag)
    } else if (dimension_to_use_if_no_raising_flags_to_typeofschool=='schooltype'){
      df_level0<-rbind(df_iattc_notps,df_iattc_PSSetType)
    }
    
  }
  
  dbDisconnect(con)
  
  df_level0$source_authority<-"IATTC"
  
  return(df_level0)
  
}