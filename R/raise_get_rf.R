#' @name raise_get_rf
#' @aliases raise_get_rf
#' @title Get raising factors from two datasets
#' @description This function outputs a data.frame of raising factors, provided two datasets, one for which the information information is usually very stratified but the associated measure represents only part of the reality, and one for wich the the information is usually less stratified (i.e. more aggregated) but the measure represents the reality.
#' See section "Details" for a use case.
#' 
#' 
#'
#'
#' 
#' @export
#' 
#' @param df_input data.frame to raise (e.g. representing partial catches or efforts)
#' @param df_input_total data.frame with total information (e.g. total catches)
#' @param x_raising_dimensions vector of dimensions to use for the raising.
#'
#' @return a data.frame of raising factors. 
#'
#'
#' @details
#' 
#' Catch-and-effort data are data aggregated over spatio-temporal strata that are collected by the CPCs or the tRFMOs
#' in some cases. Generally, catch-and-effort data are defined over one month time period and 1° or 5° size square
#' spatial resolution. Following ICCAT, catch and fishing effort statistics are defined as “the complete species (tuna,
#' tuna like species and sharks) catch composition (in weight <kg> or/and in number of fish) obtained by a given
#' amount of effort (absolute value) in a given stratification or detail level (stratum). T2CE are basically data obtained
#' from sampling a portion of the individual fishing operations of a given fishery in a specified period of time.”
#' (ICCAT Task 2). Hence, geo-referenced catch data and associated effort can represent only part of the total catches.
#' This function raises a dataset of georeferenced catches or efforts to a dataset of total catches.

#' 
#' 
#' In the output raising factor dataset, the column "sum_value_df_input" gives the sum of the catch/effort in the partial dataset for the considered statum, and the column "sum_value_df_input_total" gives the sum of the catch/effort in the total dataset for the considered statum. The meaning of the raising factors (column rf) is the following:
#' \itemize{
#'  \item{"rf>1": }{for the stratum, the sum of the catch/effort in the partial dataset is inferior to the sum of the catch in the total dataset}
#'  \item{"rf<1": }{for the stratum, the sum of the catch/effort in the partial dataset is greater to the sum of the catch in the total dataset}
#'  \item{"rf=1": }{for the stratum, the sum of the catch/effort in the partial dataset is equal to the sum of the catch in the total dataset}
#'  \item{"rf=NA": }{for the stratum, values of catch/effort exist in the partial dataset, but there is no existing value for the same statum in the total dataset; or reversely}
#' }
#'
#' You can raise by year by providing "year" as one of the elements of the parameter \code{x_raising_dimensions}. 
#'
#' @family create your own tuna atlas
#' 
#' @examples
#' 
#' # Connect to Sardara DB
#' con <- db_connection_sardara_world()
#'
#' # Extract IOTC georeferenced catch time series of catches from Sardara DB
#' ind_catch_tunaatlasird_level1<-extract_dataset(con,list_metadata_datasets(con,dataset_name="indian_ocean_catch_1952_11_01_2016_01_01_tunaatlasIRD_2017_level1"))
#' head(ind_catch_tunaatlasird_level1)
#'
#' # Extract IOTC total (nominal) catch time series from Sardara DB
#' ind_nominal_catch_tunaatlasiotc_level0<-extract_dataset(con,list_metadata_datasets(con,dataset_name="indian_ocean_nominal_catch_1950_01_01_2015_01_01_tunaatlasIOTC_2017_level0"))
#' head(ind_nominal_catch_tunaatlasiotc_level0)
#'
#' # Get raising factors by stratum defined by the following dimensions: {gear, flag, species, year, source_authority, unit}
#' 
#' iotc_rf<-raise_get_rf(
#' df_input=ind_catch_tunaatlasird_level1,
#' df_input_total=ind_nominal_catch_tunaatlasiotc_level0,
#' x_raising_dimensions=c("gear","flag","species","year","source_authority","unit") )
#' 
#' head(iotc_rf)
#' 
#' dbDisconnect(con)
#' 
#' @import dplyr     

raise_get_rf<-function(
  df_input, # data frame with partial information available
  df_input_total, # data frame with total information available
  x_raising_dimensions # Columns to consider for the raising. e.g. c("gear","flag","species","year","source_authority","catchunit")
  
) {
  
  if ("year" %in% x_raising_dimensions){
    df_input$year<-as.numeric(substr(df_input$time_start, 0, 4))
    df_input_total$year<-as.numeric(substr(df_input_total$time_start, 0, 4))
  }
  
  # georefcatches_in_stratum_flagknown
  DFPartialInfo_ByEachRaisingDimension<-group_by_(df_input,.dots=x_raising_dimensions) %>%
    summarise(value = sum(value))
  
  # totalcatches_in_stratum_flagknown
  DFTotalInfo_ByEachRaisingDimension<-group_by_(df_input_total,.dots=x_raising_dimensions) %>%
    summarise(value = sum(value))
  
  # rf is the sum of total catches in the strata divided by the sum of partial catches in the strata
  DFPartialInfo_rf<-merge(DFPartialInfo_ByEachRaisingDimension,
                          DFTotalInfo_ByEachRaisingDimension,
                          by=x_raising_dimensions,
                          all=TRUE)
  
  colnames(DFPartialInfo_rf)[which(colnames(DFPartialInfo_rf)=="value.x")]<-"sum_value_df_input"
  colnames(DFPartialInfo_rf)[which(colnames(DFPartialInfo_rf)=="value.y")]<-"sum_value_df_input_total"
  
  DFPartialInfo_rf$rf<-DFPartialInfo_rf$sum_value_df_input_total/DFPartialInfo_rf$sum_value_df_input
  
  return(DFPartialInfo_rf)
  
}

