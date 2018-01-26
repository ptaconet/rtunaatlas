#' @name get_rfmos_datasets_level0
#' @aliases get_rfmos_datasets_level0
#' @title Extract primary gridded tuna RFMOs datasets from the Tuna atlas database
#' @description This function extracts the primary gridded time series coming from the tuna Regional fisheries management organizations and stored in the Tuna atlas database. Data include geo-spatial gridded catch and efforts.
#' @export
#'
#' @usage get_rfmos_datasets_level0(rfmo,variable,year_tunaatlas,iattc_raise_flags_to_schooltype,iattc_dimension_to_use_if_no_raising_flags_to_schooltype,iccat_include_type_of_school)
#'  
#' @param rfmo string. Acronym of the RFMO. Accepted values: "IOTC", "ICCAT", "IATTC", "WCPFC", "CCSBT". See additional parameters to set if \code{rfmo} is set to "IATTC" or "ICCAT"
#' @param variable string. Variable to extract. Accepted values: "catch", "effort"
#' @param year_tunaatlas numeric. The year of the datasets to extract. Starts in 2017
#' @param iattc_raise_flags_to_schooltype boolean. Use only if \code{rfmo}=="IATTC". Raise dataset with flag stratification to dataset with schooltype stratification? See section Details for more information.
#' @param iattc_dimension_to_use_if_no_raising_flags_to_schooltype NULL or string. Use only if \code{rfmo}=="IATTC". Not nullable if \code{raise_flags_to_schooltype} is set to FALSE. if not NULL, either set to "flag" or "schooltype"
#' @param iccat_include_type_of_school boolean. Use only if \code{rfmo}=="ICCAT". Set TRUE if you want the output dataset with school type stratification. FALSE will provide the stratification without the type of school. See details for more information.
#' 
#' @details 
#' The output dataset lists catch or effort of tuna, tuna-like and shark species in the area of competence of the RFMO specified. Catches or efforts are usually stratified by year, month, species (for catches only), fishing gear, vessel flag reporting country, fishing mode (i.e. type of school used), area (1° or 5° square) and unit. Some of these dimensions can be missing depending on the confidentialy policies of each RFMO. The output dataset is computed using public domain datasets released by the RFMOs.
#' 
#' Details on the use of the parameter \code{iattc_raise_flags_to_schooltype}: For confidentiality policies, information on flag and school type for the geo-referenced catches is available in separate files for IATTC Purse seine datasets.
#' \itemize{
#' \item{ If the parameter \code{iattc_raise_flags_to_schooltype} is set to TRUE, for each stratum, the catch/effort from the flag-detailed dataset will be raised to the catch/effort from the school type-detailed dataset to get an estimation of the catches by flag and school type in each stratum.}
#' \item{ If the parameter \code{iattc_raise_flags_to_schooltype} is set to FALSE, one single dataset will be used and in this case, the parameter \code{dimension_to_use_if_no_raising_flags_to_schooltype} must be filled in: }
#'  \itemize{
#' \item{ If the parameter \code{iattc_dimension_to_use_if_no_raising_flags_to_schooltype} is set to "flag", the data with flag information will be used.}
#' \item{ If the parameter \code{iattc_dimension_to_use_if_no_raising_flags_to_schooltype} is set to "schooltype", the data with schooltype information will be used. }
#' }
#' }
#' 
#' #' Details on the use of the parameter \code{iccat_include_type_of_school}: ICCAT delivers two catch-and-efforts datasets for purse seiners: one that gives the detail of the type of school (Fad|Free school) for purse seine fisheries and that starts in 1994 (called Task II catch|effort by operation mode Fad|Free school) and one that does not give the information of the type of school and that covers all the time period (from 1950) (called Task II catch|effort). These data are redundant (i.e. the data from the dataset Task II catch|effort by operation mode are also available in the dataset Task II catch|effort) but in the latter, the information on the type of school is not available.
#' \itemize{
#' \item{ If the parameter \code{iccat_include_type_of_school} is set to TRUE, both datasets will be combined to produce a dataset that covers the whole time period, with fishing mode information (Fad | free school).}
#' \item{ If the parameter \code{iccat_include_type_of_school} is set to FALSE, only the dataset without the type of school information will be used. The output dataset will hence not have the stratification by type of school. }
#' }
#' 
#' The output dataset is expressed with the RFMO's coding system.
#' 
#' @family process data
#' 
#' @examples
#' 
#' # Retrieve IATTC georeferenced catch data from 2017, with dataset with flag dimension raised to dataset with schoolytpe dimension
#' iattc_catch<-get_rfmos_datasets_level0("iattc","catch",2017,raise_flags_to_schooltype=TRUE)
#' head(iattc_catch)
#' 
#' @author Paul Taconet, \email{paul.taconet@@ird.fr}
#'   



get_rfmos_datasets_level0<-function(rfmo,
                                variable,
                                year_tunaatlas,
                                iattc_raise_flags_to_schooltype=TRUE,
                                iattc_dimension_to_use_if_no_raising_flags_to_schooltype=NULL,
                                iccat_include_type_of_school=TRUE){
  
  con <- db_connection_tunaatlas_world()
  
  # Select datasets release on the year year_tunaatlas
  if (variable=="catch"){
    columns_to_keep<-c("source_authority","species","gear","flag","schooltype","time_start","time_end","geographic_identifier","catchtype","unit","value")
  } else if (variable=="effort"){
    columns_to_keep<-c("source_authority","gear","flag","schooltype","time_start","time_end","geographic_identifier","unit","value")
  }
  
  
  if (rfmo=="IOTC"){
    # retrieves 3 lines. IOTC level0 is only the combination of the 3 IOTC catch-and-effort datasets: indian_ocean_catch_ll_tunaatlasdf_level0 , indian_ocean_catch_tunaatlasdf_level0__coastal , indian_ocean_catch_tunaatlasdf_level0__surface
    datasets_permanent_identifiers=paste0("'indian_ocean_",variable,"_ll_tunaatlasIOTC_level0','indian_ocean_",variable,"_tunaatlasIOTC_level0__coastal','indian_ocean_",variable,"_tunaatlasIOTC_level0__surface'")
  } else if (rfmo=="WCPFC"){
    datasets_permanent_identifiers=paste0("'west_pacific_ocean_",variable,"_5deg_1m_ll_tunaatlasWCPFC_level0__1950to1970','west_pacific_ocean_",variable,"_5deg_1m_ll_tunaatlasWCPFC_level0__1990to2000','west_pacific_ocean_",variable,"_5deg_1m_tunaatlasWCPFC_level0__driftnet','west_pacific_ocean_",variable,"_5deg_1m_ll_tunaatlasWCPFC_level0__2000','west_pacific_ocean_",variable,"_5deg_1m_bb_tunaatlasWCPFC_level0','west_pacific_ocean_",variable,"_5deg_1m_ps_tunaatlasWCPFC_level0','west_pacific_ocean_",variable,"_5deg_1m_ll_tunaatlasWCPFC_level0__1970to1980','west_pacific_ocean_",variable,"_5deg_1m_ll_tunaatlasWCPFC_level0__1980to1990'")
  } else if (rfmo=="CCSBT"){
    # retrieves 2 lines. CCSBT level0 is only the combination of the 2 CCSBT catch-and-effort datasets: southern_hemisphere_oceans_catch_1deg_1m_tunaatlasCCSBT_level0__surface , southern_hemisphere_oceans_catch_5deg_1m_ll_tunaatlasCCSBT_level0
    datasets_permanent_identifiers=paste0("'southern_hemisphere_oceans_",variable,"_1deg_1m_tunaatlasCCSBT_level0__surface','southern_hemisphere_oceans_",variable,"_5deg_1m_ll_tunaatlasCCSBT_level0'")
  } else if (rfmo=="IATTC"){
    # The data that are not Purse Seine do not suffer any correction for level 0. They are taken as distributed by IATTC.
    datasets_permanent_identifiers=paste0("'east_pacific_ocean_",variable,"_1deg_1m_bb_tunaatlasIATTC_level0__tuna_byFlag','east_pacific_ocean_",variable,"_5deg_1m_ll_tunaatlasIATTC_level0__tuna_billfish','east_pacific_ocean_",variable,"_5deg_1m_ll_tunaatlasIATTC_level0__shark'")
    # Datasets that are stratified by schooltype but not by flag
    datasets_permanent_identifiers_PSSetType=paste0("'east_pacific_ocean_",variable,"_1deg_1m_ps_tunaatlasIATTC_level0__billfish_bySchool','east_pacific_ocean_",variable,"_1deg_1m_ps_tunaatlasIATTC_level0__shark_bySchool','east_pacific_ocean_",variable,"_1deg_1m_ps_tunaatlasIATTC_level0__tuna_bySchool'")
    # Datasets that are stratified by flag but not by schooltype
    datasets_permanent_identifiers_PSFlag=paste0("'east_pacific_ocean_",variable,"_1deg_1m_ps_tunaatlasIATTC_level0__billfish_byFlag','east_pacific_ocean_",variable,"_1deg_1m_ps_tunaatlasIATTC_level0__shark_byFlag','east_pacific_ocean_",variable,"_1deg_1m_ps_tunaatlasIATTC_level0__tuna_byFlag'")
  } else if (rfmo=="ICCAT"){
    datasets_permanent_identifiers=paste0("'atlantic_ocean_",variable,"_tunaatlasICCAT_level0__noSchool'")
  }
  
  metadata_datasets<-dbGetQuery(con,paste0("SELECT * from metadata.metadata where persistent_identifier IN (",datasets_permanent_identifiers,") and identifier LIKE '%__",year_tunaatlas,"%'"))
  
  df_level0<-extract_and_merge_multiple_datasets(con,metadata_datasets,columns_to_keep)
  
  # Deal with special case of ICCAT PS
  if (rfmo=="ICCAT" && iccat_include_type_of_school==TRUE){ # We include in the dataset the data including the information on type of school
    # Retrieve ICCAT dataset with schooltype information (task2 by operation mode) (https://goo.gl/f2jz5R). We do not use the template (template_query_effortes) because flag code list used in iccat task2 by operation mode dataset is different from flag code list used in ICCAT task2; however we have to use the same flag code list for data raising. In other words, we express all ICCAT datasets following ICCAT task2 flag code list.
    datasets_permanent_identifiers=paste0("'atlantic_ocean_",variable,"_1deg_1m_ps_tunaatlasICCAT_level0__bySchool'")
    metadata_datasets_WithSchooltypeInfo<-dbGetQuery(con,paste0("SELECT * from metadata.metadata where persistent_identifier IN (",datasets_permanent_identifiers,") and identifier LIKE '%__",year_tunaatlas,"%'"))
    
    iccat_ce_WithSchooltypeInfo<-extract_and_merge_multiple_datasets(con,metadata_datasets_WithSchooltypeInfo,columns_to_keep)
    
    # We need to map flag code list, because flag code list used in iccat task2 by operation mode dataset is different from flag code list used in ICCAT task2; however we have to use the same flag code list for data raising. In other words, we express all ICCAT datasets following ICCAT task2 flag code list.
    flag_mapping_flag_iccat_from_ncandcas_to_flag_iccat<-rtunaatlas::extract_dataset(con,list_metadata_datasets(con,dataset_name="codelist_mapping_flag_iccat_from_ncandcas_flag_iccat"))
    iccat_ce_WithSchooltypeInfo<-map_codelist(iccat_ce_WithSchooltypeInfo,flag_mapping_flag_iccat_from_ncandcas_to_flag_iccat,"flag")[[1]]
    
    strata_in_withoutschooltype_and_not_in_withshooltype<-anti_join (df_level0,iccat_ce_WithSchooltypeInfo,by=setdiff(columns_to_keep,c("value","schooltype")))
    
    # Join datasets: Dataset with the type of school + dataset without the type of school from which we have removed the strata that are also available in the dataset with the type of school.
    df_level0<-rbind(strata_in_withoutschooltype_and_not_in_withshooltype,iccat_ce_WithSchooltypeInfo)
    
  }
  
  # Deal with special case of IATTC PS
  if (rfmo=="IATTC"){
    
    # Retrieve IATTC georef. dataset Purse Seine by Schooltype
    metadata_datasets_PSSetType<-dbGetQuery(con,paste0("SELECT * from metadata.metadata where persistent_identifier IN (",datasets_permanent_identifiers_PSSetType,") and identifier LIKE '%__",year_tunaatlas,"%'"))
    df_iattc_PSSetType<-extract_and_merge_multiple_datasets(con,metadata_datasets_PSSetType,columns_to_keep)
    
    # Retrieve IATTC georef. dataset Purse Seine by Flag
    metadata_datasets_PSFlag<-dbGetQuery(con,paste0("SELECT * from metadata.metadata where persistent_identifier IN (",datasets_permanent_identifiers_PSFlag,") and identifier LIKE '%__",year_tunaatlas,"%'"))
    df_iattc_PSFlag<-extract_and_merge_multiple_datasets(con,metadata_datasets_PSFlag,columns_to_keep)
    
    if (iattc_raise_flags_to_schooltype==TRUE){
      iattc_flag_raised_to_schooltype<-raise_datasets_by_dimension(df1=df_iattc_PSFlag,
                                                                   df2=df_iattc_PSSetType,
                                                                   dimension_missing_df1="schooltype",
                                                                   dimension_missing_df2="flag")
      
      df_level0<-rbind(df_level0,iattc_flag_raised_to_schooltype$df)
      
    } else {  # If user decides to not raise flags to type of school, he chooses to use either the data with stratification by flag or the data with stratification by schooltype
      cat(paste0("\nkeeping dataset with information on ",iattc_dimension_to_use_if_no_raising_flags_to_schooltype))
    if (iattc_dimension_to_use_if_no_raising_flags_to_schooltype=='flag'){
      df_level0<-rbind(df_level0,df_iattc_PSFlag)
    } else if (iattc_dimension_to_use_if_no_raising_flags_to_schooltype=='schooltype'){
      df_level0<-rbind(df_level0,df_iattc_PSSetType)
    }
  }
}
  

  dbDisconnect(con)
  
  return(df_level0)
  
}