#' @name raise_by_dimension
#' @aliases raise_by_dimension
#' @title Create a dataset with most complete information using two datasets with partial information
#' @description This function creates a dataset with most complete information using two datasets with partial information , by crossing the informations available in both datasets.
#'  It takes as input two datasets, each with partial information, but the full information is available if we consider both datasets. Each dataset used as input misses one dimension, the missing dimension in the dataset n°1 being available in the dataset n°2 and reversely. The functions uses both datasets to create a new dataset with the full information (i.e. with values filled for both dimensions on the dataset).
#'  The data frames of fact must be properly structured. For structures of data frames, see details and here: \url{http://}. 
#' @export
#'
#' @usage raise_by_dimension(df1, df2, dimension_missing_df1, dimension_missing_df2)
#'                 
#' @param df1 a data.frame of fact with one dimension missing (i.e. for this dimension all values are set to UNK), the latter being available in df2.
#' @param df2 the same data.frame of fact with another dimension missing, the latter being available in df1.
#' @param dimension_missing_df1 string. The name of the dimension missing in df1.
#' @param dimension_missing_df2 string. The name of the dimension missing in df2.
#' 
#' @return a list with one object:
#' \itemize{
#'  \item{"df": }{a data frame, where df1 and df2 have been crossed to get a dataset with both dimensions filled).
#' }
#' 
#' @details 
#' 
#' In this function, we make the hypothesis that ....? comment décrire ca???? -> cf mail Manu "Formes"
#' Example:
#' \itemize{
#' \item{\code{df1} :}  {is a dataset where the "flag" dimension is available but the "schooltype" dimension is missing (i.e. in this dataset, all the values of the column "schooltype" are set to UNK) }
#' \item{\code{df2} :} {is a dataset where the "schooltype" dimension is available but the "flag" dimension is missing (i.e. in this dataset, all the values of the column "flag" are set to UNK) }
#' }
#' 
#' The concept of raising the data is the following: in a given stratum S, a fishing country (flag) F has catched a percentage RF % of the total catches made in this stratum (this information is extracted from the dataset with flag detail, i.e. df1. RF is called the raising factor). In the same stratum S, there has been in total Y tons of catches realized on log school, made by all the fishing countries (this information extracted from the dataset with schooltype detail, i.e. df2). Raising the data means saying that the fishing country F has catched RF * T tons on log school in the stratum S. RF * T is the value raised.
#' 
#' In the output dataset, both flag and school type dimensions are available for each stratum. 
#' 
#' We make the hypothesis that the proportion of catches by schooltype is equal for all the fishing countries.
#' 
#' @family create your own tuna atlas
#' 
#' 
#' @examples
#'
#' ## Retrieve some IATTC georeferenced times series of catch 
#' # Connect to Sardara DB
#' con <- db_connection_sardara_world()
#' 
#'
#' # IATTC dataset stratified by schooltype (and not flag)
#' dataset_iattc_ce_PSSetType<-extract_dataset(con,list_metadata_datasets(con,dataset_name="east_pacific_ocean_catch_1958_12_01_2016_01_01_1deg_1m_ps_tunaatlasIATTC_2017_level0__tuna_bySchool"))
#' head(dataset_iattc_ce_PSSetType)
#' unique(dataset_iattc_ce_PSSetType$flag) # Note that the column "flag" is all set with "UNK"
#' unique(dataset_iattc_ce_PSSetType$schooltype) # Note that the column "schooltype" is detailed
#'
#' # Same IATTC datasets, but stratified by flag (and not schooltype)
#' dataset_iattc_ce_PSFlag<-extract_dataset(con,list_metadata_datasets(con,dataset_name="east_pacific_ocean_catch_1958_12_01_2016_01_01_1deg_1m_ps_tunaatlasIATTC_2017_level0__tuna_byFlag"))
#' head(dataset_iattc_ce_PSFlag)
#' unique(dataset_iattc_ce_PSFlag$schooltype) # Note that the column "schooltype" is all set with "UNK"
#' unique(dataset_iattc_ce_PSFlag$flag) # Note that the column "flag" is detailed
#' 
#' ## Raise both datasets. In the output dataset, both flag and school type information are available for each stratum. 
#'  
#' dataset_iattc_flag_raised_to_schooltype<-raise_by_dimension(
#' df1=dataset_iattc_ce_PSFlag,
#' df2=dataset_iattc_ce_PSSetType,
#' dimension_missing_df1="schooltype",
#' dimension_missing_df2="flag")
#'
#' head(dataset_iattc_flag_raised_to_schooltype$df)
#' unique(dataset_iattc_flag_raised_to_schooltype$df$schooltype)
#' unique(dataset_iattc_flag_raised_to_schooltype$df$flag) # Note that both columns "flag" and "schooltype" are detailed
#'
#' dbDisconnect(con)
#' 
#' @author Paul Taconet, \email{paul.taconet@@ird.fr}
#' 

raise_by_dimension<-function(df1, # dataset 1
                             df2, # dataset 2
                             dimension_missing_df1, # e.g. "schooltype"
                             dimension_missing_df2 # e.g. "flag"
){
  
  colnames_input_dataset<-unique(c(colnames(df1),c(colnames(df2))))
  
  RaisingDataset_RaisingDimensionsColNames<-setdiff(colnames_input_dataset,c(dimension_missing_df1,"value"))
  RaisedDataset_RaisedDimensionsColNames<-setdiff(colnames_input_dataset,c(dimension_missing_df2,"value"))
  
  RaisedDataset_RaisedDimension<-dimension_missing_df1
  RaisingDataset_RaisingDimension<-dimension_missing_df2
  
  # 1) Calculation of the raising factor
  
  # group by df1 and keep stratification by raising dimension
  # example: get the catches in each stratum by each flag
  RaisingDataset_ByEachRaisingDimension<-group_by_(df1,.dots=RaisingDataset_RaisingDimensionsColNames) %>%
    summarise(value = sum(value))
  
  #group by df1, including the raising dimension
  # example: get the catches in each stratum, all flags included
  RaisingDataset_AllRaisingDimension<-group_by_(df1,.dots=setdiff(RaisingDataset_RaisingDimensionsColNames,RaisingDataset_RaisingDimension)) %>%
    summarise(value = sum(value))
  
  
  # example: Percentage of catches made in each stratum by each flag (in a given stratum S, a flag F has catched a percentage RF % of the total catches in this stratum)
  Percentage_made_in_each_stratum_byeachRaisingDimension<-merge(RaisingDataset_ByEachRaisingDimension,
                                                                RaisingDataset_AllRaisingDimension,
                                                                by=setdiff(RaisingDataset_RaisingDimensionsColNames,RaisingDataset_RaisingDimension),
                                                                all.x=TRUE)
  
  
  Percentage_made_in_each_stratum_byeachRaisingDimension$rf<-Percentage_made_in_each_stratum_byeachRaisingDimension$value.x / Percentage_made_in_each_stratum_byeachRaisingDimension$value.y
  
  
  # example: In the same stratum S, there has been in total Y tons of catches realized on log school - i.e. made by all the flags (this information extracted from the files with schooltype detail. Column v_catch). Raising the data means saying that flag F has catched RF * T tons on log school in the stratum S, i.e. value_raised = RF * T 
  
  RaisedDF<-merge(Percentage_made_in_each_stratum_byeachRaisingDimension,
                  df2,
                  by.x=setdiff(RaisingDataset_RaisingDimensionsColNames,RaisingDataset_RaisingDimension),
                  by.y=setdiff(RaisedDataset_RaisedDimensionsColNames,RaisedDataset_RaisedDimension),
                  all.y=TRUE)
  
  
  RaisedDF$raised_value<-RaisedDF$rf*RaisedDF$value
  
  RaisedDF = RaisedDF[!is.na(RaisedDF$raised_value),]
  
  colnames(RaisedDF)[which(colnames(RaisedDF) == paste0(RaisingDataset_RaisingDimension,".x"))] <- RaisingDataset_RaisingDimension
  
  RaisedDF$value<-NULL
  colnames(RaisedDF)[which(colnames(RaisedDF) == "raised_value")] <- "value"
  RaisedDF<-RaisedDF[colnames_input_dataset]
  
  return(list(df=RaisedDF))
  
}

