#' @name get_rfmos_datasets_level0
#' @aliases get_rfmos_datasets_level0
#' @title Extract primary gridded tuna RFMOs datasets from the Tuna atlas database
#' @description This function extracts the primary gridded time series coming from the tuna Regional fisheries management organizations and stored in the Tuna atlas database. Data include geo-spatial gridded catch and efforts.
#' @export
#'
#' @usage get_rfmos_datasets_level0(rfmo,variable,year_tunaatlas,iattc_ps_raise_flags_to_schooltype,iattc_ps_dimension_to_use_if_no_raising_flags_to_schooltype,iattc_ps_catch_billfish_shark_raise_to_effort,iattc_ps_effort_to_extract,iccat_ps_include_type_of_school)
#'  
#' @param rfmo string. Acronym of the RFMO. Accepted values: "IOTC", "ICCAT", "IATTC", "WCPFC", "CCSBT". See additional parameters to set if \code{rfmo} is set to "IATTC" or "ICCAT"
#' @param variable string. Variable to extract. Values accepted: "catch", "effort"
#' @param year_tunaatlas numeric. The year of the datasets to extract (i.e. year the datasets were released). Starts in 2017
#' @param iattc_ps_raise_flags_to_schooltype boolean. Use only if \code{rfmo}=="IATTC". Raise dataset with flag stratification to dataset with schooltype stratification? See section Details for more information.
#' @param iattc_ps_dimension_to_use_if_no_raising_flags_to_schooltype NULL or string. Use only if \code{rfmo}=="IATTC". Not nullable if \code{raise_flags_to_schooltype} is set to FALSE. if not NULL, either set to "flag" or "schooltype"
#' @param iattc_ps_catch_billfish_shark_raise_to_effort boolean. Use only if \code{rfmo}=="IATTC" and \code{variable}=="catch". IATTC Purse Seine datasets are available in separate files for tuna, billfish and sharks. TRUE: raise billfish (resp. shark) catch to ratio effort tuna / effort billfish (resp. shark). FALSE: keep billfish (resp. shark) catch as they are provided in the billfish (resp. shark) catch datasets.
#' @param iattc_ps_effort_to_extract NULL or string. Use only if \code{rfmo}=="IATTC" and \code{variable}=="effort". IATTC Purse Seine datasets are available in separate files for tunas, billfishes and sharks. Which effort data should be kept between these 3 files? {"tuna","billfish","shark"}. See section Details for more information.
#' @param iccat_ps_include_type_of_school boolean. Use only if \code{rfmo}=="ICCAT". Set TRUE if you want the output dataset with school type stratification. FALSE will provide the stratification without the type of school. See section Details for more information.
#' 
#' @details 
#' The output dataset lists catch or effort of tuna, tuna-like and shark species in the area of competence of the RFMO specified. Catches or efforts are usually stratified by year, month, species (for catches only), fishing gear, vessel flag reporting country, fishing mode (i.e. type of school used), area (1° or 5° square) and unit. Some of these dimensions can be missing depending on the confidentialy policies of each RFMO. The output dataset is computed using public domain datasets released by the RFMOs.
#' 
#' Details on the use of the parameter \code{iattc_ps_raise_flags_to_schooltype}: For confidentiality policies, information on flag and school type for the geo-referenced catches is available in separate files for IATTC Purse seine datasets.
#' \itemize{
#' \item{ If the parameter \code{iattc_ps_raise_flags_to_schooltype} is set to \code{TRUE}, for each stratum, the catch/effort from the flag-detailed dataset will be raised to the catch/effort from the school type-detailed dataset to get an estimation of the catches by flag and school type in each stratum.}
#' \item{ If the parameter \code{iattc_ps_raise_flags_to_schooltype} is set to \code{FALSE}, one single dataset will be used and in this case, the parameter \code{dimension_to_use_if_no_raising_flags_to_schooltype} must be set: }
#'  \itemize{
#' \item{ If the parameter \code{iattc_ps_dimension_to_use_if_no_raising_flags_to_schooltype} is set to \code{flag}, only the data with flag information will be used.}
#' \item{ If the parameter \code{iattc_ps_dimension_to_use_if_no_raising_flags_to_schooltype} is set to \code{schooltype}, only the data with schooltype information will be used. }
#' }
#' }
#' 
#' Details on the use of the parameter \code{iattc_ps_catch_billfish_shark_raise_to_effort}:  In addition to the separation flag / schooltype (see above), IATTC Purse seine catch-and-effort are available in 3 separate files according to the group of species: tuna, billfishes, sharks. This is due to the fact that PS data is collected from 2 sources, observer and fishing vessel logbooks. Observer records are used when available, and for unobserved trips logbooks are used. Both sources collect tuna data, but only observers collect shark and billfish data. So as an example a strata may have observer effort and the number of sets from the observed trips would be counted for tuna, shark and billfish. But there may have also been logbook data for unobserved sets in the same strata, so the tuna catch and number of sets for a cell would be added. This would make a higher total number of sets for tuna catch than shark or billfish. So efforts in the billfish and shark datasets might represent only a proportion of the total effort allocated in some strata since it is the observed effort, i.e. for which there was an observer onboard. As a result, catch in the billfish and shark datasets might represent only a proportion of the total catch allocated in a some strata.
#' \itemize{
#' \item{ \code{TRUE}: Raise billfish (resp. shark) catch to the ratio  effort tuna / effort billfish (resp. shark).}
#' \item{ \code{FALSE}: Keep billfish (resp. shark) catch as they are provided in the billfish (resp. shark) catch datasets.}
#' }
#' 
#' Details on the use of the parameter \code{iattc_ps_effort_to_extract}: For the same reason as above, this parameter enables to select the effort dataset that the user wants to use:
#' \itemize{
#' \item{ \code{tuna}: Keep the effort from the tuna dataset. Likely to be the best approximation of effort. }
#' \item{ \code{billfish}: Keep the effort from the billfish dataset.}
#' \item{ \code{shark}: Keep the effort from the shark dataset. }
#' }
#' 
#' Details on the use of the parameter \code{iccat_ps_include_type_of_school}: ICCAT delivers two catch-and-efforts datasets for purse seiners: one that gives the detail of the type of school (Fad|Free school) for purse seine fisheries and that starts in 1994 (called Task II catch|effort by operation mode Fad|Free school) and one that does not give the information of the type of school and that covers all the time period (from 1950) (called Task II catch|effort). These data are redundant (i.e. the data from the dataset Task II catch|effort by operation mode are also available in the dataset Task II catch|effort) but in the latter, the information on the type of school is not available.
#' \itemize{
#' \item{ If the parameter \code{iccat_ps_include_type_of_school} is set to \code{TRUE}, both datasets will be combined to produce a dataset that covers the whole time period, with fishing mode information (Fad | free school).}
#' \item{ If the parameter \code{iccat_ps_include_type_of_school} is set to \code{FALSE}, only the dataset without the type of school information will be used. The output dataset will hence not have the stratification by type of school. }
#' }
#' 
#' The output dataset is expressed with the RFMO coding system.
#' 
#' @family process data
#' 
#' @examples
#' 
#' # Retrieve IATTC georeferenced catch data from 2017, with dataset with flag dimension raised to dataset with schoolytpe dimension
#' iattc_catch<-get_rfmos_datasets_level0("iattc","catch",2017,iattc_ps_raise_flags_to_schooltype=TRUE)
#' head(iattc_catch)
#' 
#' @author Paul Taconet, \email{paul.taconet@@ird.fr}
#'   



get_rfmos_datasets_level0<-function(rfmo,
                                variable,
                                year_tunaatlas,
                                iattc_ps_raise_flags_to_schooltype=TRUE,
                                iattc_ps_dimension_to_use_if_no_raising_flags_to_schooltype=NULL,
                                iattc_ps_catch_billfish_shark_raise_to_effort=FALSE,
                                iattc_ps_effort_to_extract="tuna",
                                iccat_ps_include_type_of_school=TRUE){
  
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
    # The PS data are dealt separately (after in the function)
  } else if (rfmo=="ICCAT"){
    datasets_permanent_identifiers=paste0("'atlantic_ocean_",variable,"_tunaatlasICCAT_level0__noSchool'")
  }
  
  metadata_datasets<-dbGetQuery(con,paste0("SELECT * from metadata.metadata where persistent_identifier IN (",datasets_permanent_identifiers,") and identifier LIKE '%__",year_tunaatlas,"%'"))
  
  df_level0<-extract_and_merge_multiple_datasets(con,metadata_datasets,columns_to_keep)
  
  # Deal with special case of ICCAT PS
  if (rfmo=="ICCAT" && iccat_ps_include_type_of_school==TRUE){ # We include in the dataset the data including the information on type of school
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
    
    ## IATTC PS catch-and-effort are stratified as following:
    # - 1 dataset for tunas, stratified by type of school (but not flag)
    # - 1 dataset for tunas, stratified by flag (but not type of school)
    # - 1 dataset for billfishes, stratified by type of school (but not flag)
    # - 1 dataset for billfishes, stratified by flag (but not type of school)
    # - 1 dataset for sharks, stratified by type of school (but not flag)
    # - 1 dataset for sharks, stratified by flag (but not type of school)
    ## So in total there are 6 datasets. 
    
    # Commentaire Emmanuel Chassot: L’effort est exprimé ici en nombre de calées. Cela signifie dans le cas de l’EPO que les efforts donnés dans certains jeux de données peuvent correspondre à une partie de l’effort total alloué à une strate puisqu’il s’agit de l’effort observé, cà-d. pour lequel il y avait un observateur à bord du senneur. De mon point de vue, (1) L’effort unique et homogène serait celui des thons tropicaux et (2) pour uniformiser le jeu de captures par strate, il faut calculer un ratio de captures de requins par calée (observée) et de porte-épées par calée (observée) et de les multiplier ensuite par l’effort reporté pour les thons tropicaux puisqu’on considère que c’est l’effort de la pêcherie (qui cible les thons). Le raising factor est effort thons / effort billfish et effort thons / effort sharks.

    # Get metadata of Catch datasets (for tuna, billfish and shark, and stratified by flag and then by type of school)
    metadata_dataset_PSSetType_tuna_catch<-dbGetQuery(con,paste0("SELECT * from metadata.metadata where persistent_identifier='east_pacific_ocean_catch_1deg_1m_ps_tunaatlasIATTC_level0__tuna_bySchool' and identifier LIKE '%__",year_tunaatlas,"%'"))
    metadata_dataset_PSFlag_tuna_catch<-dbGetQuery(con,paste0("SELECT * from metadata.metadata where persistent_identifier='east_pacific_ocean_catch_1deg_1m_ps_tunaatlasIATTC_level0__tuna_byFlag' and identifier LIKE '%__",year_tunaatlas,"%'"))
    
    metadata_dataset_PSSetType_billfish_catch<-dbGetQuery(con,paste0("SELECT * from metadata.metadata where persistent_identifier='east_pacific_ocean_catch_1deg_1m_ps_tunaatlasIATTC_level0__billfish_bySchool' and identifier LIKE '%__",year_tunaatlas,"%'"))
    metadata_dataset_PSFlag_billfish_catch<-dbGetQuery(con,paste0("SELECT * from metadata.metadata where persistent_identifier='east_pacific_ocean_catch_1deg_1m_ps_tunaatlasIATTC_level0__billfish_byFlag' and identifier LIKE '%__",year_tunaatlas,"%'"))
    
    metadata_dataset_PSSetType_shark_catch<-dbGetQuery(con,paste0("SELECT * from metadata.metadata where persistent_identifier='east_pacific_ocean_catch_1deg_1m_ps_tunaatlasIATTC_level0__shark_bySchool' and identifier LIKE '%__",year_tunaatlas,"%'"))
    metadata_dataset_PSFlag_shark_catch<-dbGetQuery(con,paste0("SELECT * from metadata.metadata where persistent_identifier='east_pacific_ocean_catch_1deg_1m_ps_tunaatlasIATTC_level0__shark_byFlag' and identifier LIKE '%__",year_tunaatlas,"%'"))
    
    # Get metadata of Effort datasets (for tuna, billfish and shark, and stratified by flag and then by type of school)
    metadata_dataset_PSSetType_tuna_effort<-dbGetQuery(con,paste0("SELECT * from metadata.metadata where persistent_identifier='east_pacific_ocean_effort_1deg_1m_ps_tunaatlasIATTC_level0__tuna_bySchool' and identifier LIKE '%__",year_tunaatlas,"%'"))
    metadata_dataset_PSFlag_tuna_effort<-dbGetQuery(con,paste0("SELECT * from metadata.metadata where persistent_identifier='east_pacific_ocean_effort_1deg_1m_ps_tunaatlasIATTC_level0__tuna_byFlag' and identifier LIKE '%__",year_tunaatlas,"%'"))
    
    metadata_dataset_PSSetType_billfish_effort<-dbGetQuery(con,paste0("SELECT * from metadata.metadata where persistent_identifier='east_pacific_ocean_effort_1deg_1m_ps_tunaatlasIATTC_level0__billfish_bySchool' and identifier LIKE '%__",year_tunaatlas,"%'"))
    metadata_dataset_PSFlag_billfish_effort<-dbGetQuery(con,paste0("SELECT * from metadata.metadata where persistent_identifier='east_pacific_ocean_effort_1deg_1m_ps_tunaatlasIATTC_level0__billfish_byFlag' and identifier LIKE '%__",year_tunaatlas,"%'"))
    
    metadata_dataset_PSSetType_shark_effort<-dbGetQuery(con,paste0("SELECT * from metadata.metadata where persistent_identifier='east_pacific_ocean_effort_1deg_1m_ps_tunaatlasIATTC_level0__shark_bySchool' and identifier LIKE '%__",year_tunaatlas,"%'"))
    metadata_dataset_PSFlag_shark_effort<-dbGetQuery(con,paste0("SELECT * from metadata.metadata where persistent_identifier='east_pacific_ocean_effort_1deg_1m_ps_tunaatlasIATTC_level0__shark_byFlag' and identifier LIKE '%__",year_tunaatlas,"%'"))
    
    columns_to_keep_effort=c("source_authority","gear","flag","schooltype","time_start","time_end","geographic_identifier","unit","value")
    
    # For the effort data, we keep only effort from one of the files (tuna or billfishes or shark). This is driven by the parameter "iattc_ps_effort_to_extract"
    if (variable=="effort"){
      if(iattc_ps_effort_to_extract=="tuna"){
        metadata_dataset_effort_flag<-metadata_dataset_PSFlag_tuna_effort
        metadata_dataset_effort_settype<-metadata_dataset_PSSetType_tuna_effort
      } else if(iattc_ps_effort_to_extract=="billfish"){
        metadata_dataset_effort_flag<-metadata_dataset_PSFlag_billfish_effort
        metadata_dataset_effort_settype<-metadata_dataset_PSSetType_billfish_effort
      } else if(iattc_ps_effort_to_extract=="shark"){
        metadata_dataset_effort_flag<-metadata_dataset_PSFlag_shark_effort
        metadata_dataset_effort_settype<-metadata_dataset_PSSetType_shark_effort
      }
      
      df_iattc_effort_PSSetType<-extract_and_merge_multiple_datasets(con,metadata_dataset_effort_settype,columns_to_keep=columns_to_keep_effort)
      df_iattc_effort_PSFlag<-extract_and_merge_multiple_datasets(con,metadata_dataset_effort_flag,columns_to_keep=columns_to_keep_effort)
      
      if (iattc_ps_raise_flags_to_schooltype==TRUE){
        #Get Tuna effort by raising flags to schooltype
        df<-raise_datasets_by_dimension(df1=df_iattc_effort_PSFlag,
                                        df2=df_iattc_effort_PSSetType,
                                        dimension_missing_df1="schooltype",
                                        dimension_missing_df2="flag")$df
        
      } else {  # If the user decides not to raise flags to type of school, he chooses to use either the data with stratification by flag or the data with stratification by schooltype
        if (iattc_ps_dimension_to_use_if_no_raising_flags_to_schooltype=='flag'){
          df<-df_iattc_effort_PSFlag
        } else if (iattc_ps_dimension_to_use_if_no_raising_flags_to_schooltype=='schooltype'){
          df<-df_iattc_effort_PSSetType
        }
      
      df_level0<-rbind(df_level0,df)
      
    }
      } else { # if variable=="catch"
      
      # Function to extract the datasets of catch (for billfish and tuna) and raise them to the ratio effort tuna / effort billfish (or effort shark)
      function_raise_catch_to_effort<-function(metadata_dataset_tuna_effort,
                                               metadata_dataset_billfish_or_shark_catch,
                                               metadata_dataset_billfish_or_shark_effort,
                                               raising_dimensions){
        
        billfish_or_shark_catch<-extract_and_merge_multiple_datasets(con,metadata_dataset_billfish_or_shark_catch,columns_to_keep)
        billfish_or_shark_effort<-extract_and_merge_multiple_datasets(con,metadata_dataset_billfish_or_shark_effort,columns_to_keep=columns_to_keep_effort)
        tuna_effort<-extract_and_merge_multiple_datasets(con,metadata_dataset_tuna_effort,columns_to_keep=columns_to_keep_effort)
        
        
          # Get RF for effort (rf=effort tuna / effort billfish   or    effort tuna / effort shark)
          df_rf<-raise_get_rf(
            df_input=billfish_or_shark_effort,
            df_input_total=tuna_effort,
            x_raising_dimensions=c(raising_dimensions,"unit") )
          
          df_rf$unit<-NULL
          
          # Raise the data
          catch_raised<-raise_incomplete_dataset_to_total_dataset(
            df_input_incomplete=billfish_or_shark_catch,
            df_input_total=billfish_or_shark_catch,
            df_rf=df_rf,
            x_raising_dimensions=raising_dimensions,
            threshold_rf=NULL)
          
          return(catch_raised$df)

      }
      
      # Extract tuna catch
      df_catch_tuna_flag<-extract_and_merge_multiple_datasets(con,metadata_dataset_PSFlag_tuna_catch,columns_to_keep=columns_to_keep)
      df_catch_tuna_settype<-extract_and_merge_multiple_datasets(con,metadata_dataset_PSSetType_tuna_catch,columns_to_keep=columns_to_keep)
      
      # Extract billfish and shark catch.
      # If the user decides to raise shark/billfish catch to ratio effort tuna / effort shark/billfish:
      if (iattc_ps_catch_billfish_shark_raise_to_effort==TRUE){
        df_catch_billfish_flag<-function_raise_catch_to_effort(metadata_dataset_PSFlag_tuna_effort,metadata_dataset_PSFlag_billfish_catch,metadata_dataset_PSFlag_billfish_effort,c("gear","flag","time_start","time_end","geographic_identifier"))
        df_catch_billfish_settype<-function_raise_catch_to_effort(metadata_dataset_PSSetType_tuna_effort,metadata_dataset_PSSetType_billfish_catch,metadata_dataset_PSSetType_billfish_effort,c("gear","schooltype","time_start","time_end","geographic_identifier"))
        
        df_catch_shark_flag<-function_raise_catch_to_effort(metadata_dataset_PSFlag_tuna_effort,metadata_dataset_PSFlag_shark_catch,metadata_dataset_PSFlag_shark_effort,c("gear","flag","time_start","time_end","geographic_identifier"))
        df_catch_shark_settype<-function_raise_catch_to_effort(metadata_dataset_PSSetType_tuna_effort,metadata_dataset_PSSetType_shark_catch,metadata_dataset_PSSetType_shark_effort,c("gear","schooltype","time_start","time_end","geographic_identifier"))
        
         } else { # Else do not raise (i.e. for billfish/shark, keep catch only from billfish / shark)
          df_catch_billfish_flag<-extract_and_merge_multiple_datasets(con,metadata_dataset_PSFlag_billfish_catch,columns_to_keep=columns_to_keep)
          df_catch_billfish_settype<-extract_and_merge_multiple_datasets(con,metadata_dataset_PSSetType_billfish_catch,columns_to_keep=columns_to_keep)
           
          df_catch_shark_flag<-extract_and_merge_multiple_datasets(con,metadata_dataset_PSFlag_shark_catch,columns_to_keep=columns_to_keep)
          df_catch_shark_settype<-extract_and_merge_multiple_datasets(con,metadata_dataset_PSSetType_shark_catch,columns_to_keep=columns_to_keep)
          
         }
      
        if (iattc_ps_raise_flags_to_schooltype==TRUE){
          
        df_catch_billfish<-raise_datasets_by_dimension(df1=df_catch_billfish_flag,
                                        df2=df_catch_billfish_settype,
                                        dimension_missing_df1="schooltype",
                                        dimension_missing_df2="flag")$df
        
        df_catch_shark<-raise_datasets_by_dimension(df1=df_catch_shark_flag,
                                                       df2=df_catch_shark_settype,
                                                       dimension_missing_df1="schooltype",
                                                       dimension_missing_df2="flag")$df
        
        df_catch_tuna<-raise_datasets_by_dimension(df1=df_catch_tuna_flag,
                                                    df2=df_catch_tuna_settype,
                                                    dimension_missing_df1="schooltype",
                                                    dimension_missing_df2="flag")$df
        
        
      } else {  # If user decides to not raise flags to type of school, he chooses to use either the data with stratification by flag or the data with stratification by schooltype
        if (iattc_ps_dimension_to_use_if_no_raising_flags_to_schooltype=='flag'){
          df_catch_billfish<-df_catch_billfish_flag
          df_catch_shark<-df_catch_shark_flag
          df_catch_tuna<-df_catch_tuna_flag
          } else if (iattc_ps_dimension_to_use_if_no_raising_flags_to_schooltype=='schooltype'){
          df_catch_billfish<-df_catch_billfish_settype
          df_catch_shark<-df_catch_shark_settype
          df_catch_tuna<-df_catch_tuna_settype
          }
      }
      
      df_level0<-rbind(df_level0,df_catch_billfish,df_catch_shark,df_catch_tuna)
  
    }
  }
  

  dbDisconnect(con)
  
  return(df_level0)
  
}