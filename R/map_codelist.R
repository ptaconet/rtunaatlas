#' @name map_codelist
#' @aliases map_codelist
#' @title Maps and replaces one dimension of a fact dataset using a dataset of mappings between code lists
#' @description This function maps one dimension (i.e. column with codes) of a fact dataset to a target code list using a dataset of mappings between code lists. In other words, it makes the correspondance between two code lists for a given dimension, and replaces for this dimension in the fact dataset the old codes by the new codes available in the dataset of mappings between code lists.
#' @export
#'
#'
#' @usage map_codelist(df_input, df_mapping, dimension_to_map,keep_src_code)
#'                 
#' @param df_input a data.frame of fact
#' @param df_mapping a data.frame of code list mapping
#' @param dimension_to_map the name (string) of the dimension to map.
#' @param keep_src_code boolean keep source coding system column? TRUE will conserve in the output dataset both source and target coding systems columns, FALSE will conserve only target coding system (i.e. mapped). Default is FALSE
#' 
#' @return a list with two objects:
#' \itemize{
#'  \item{"df": }{The input data.frame of fact, where the dimension_to_map has been mapped using the df_mapping}
#'  \item{"stats": }{A data.frame with some information regarding the data not mapped. It provides, for each unit of measure available in the input dataset, the sum and percentage of the data that could not be map because no correspondance are available in the dataset of mappings between code lists}
#' }
#' 
#' @details The data frames of fact and code list mapping must be properly structured. The data.frame of mapping must have the 2 following columns:
#' \itemize{
#' \item{"src_code": }{The source codes for the dimension to map (i.e. the codes used in the df_input fot the considered dimension)}
#' \item{"trg_code": }{The target codes for the dimension to map}
#' }
#' 
#' Some codes might not be mapped, because no correspondance exists between the source code(s) and the target code(s). In the output dataset of the function map_codelist, these unmapped codes are set to "UNK".  
#' If \code{keep_src_code} is set to FALSE, the source coding system column will be dropped and the target coding system column will be named out dimension_to_map. 
#' If \code{keep_src_code} is set to TRUE, the source coding system column will be kept. In that case, the source coding system column will conserve its original name (dimension_to_map), and the target coding system column will be named "dimension_to_map"_mapping (e.g. gear_mapping)
#' 
#' @family process data
#' 
#' 
#' @examples
#' 
#' # Connect to Tuna atlas database
#' con<-db_connection_tunaatlas_world()
#' 
#'   # Reads IOTC nominal catch dataset (2017 release)
#'   iotc_nominal_catch<-extract_dataset(con,list_metadata_datasets(con,identifier="indian_ocean_nominal_catch_1950_01_01_2015_01_01_tunaatlasIOTC_2017_level0"))
#'   head(iotc_nominal_catch)
#'   
#'   # Read a mapping between code lists (in this case, mapping between codes for fishing gears used by the tuna RFMOs and the International Standard Statistical Classification of Fishing Gear)
#'   df_mapping<-extract_dataset(con,list_metadata_datasets(con,identifier="codelist_mapping_gear_iotc_isscfg_revision_1")) 
#'   head(df_mapping)
#'  
#'   # Map code lists. Output is a list with two elements (see section "return"). Default conserves only target coding system in the output dataset. Set keep_src_code=TRUE to conserve both source and target coding systems in the output dataset.
#'   df_mapped<-map_codelist(iotc_nominal_catch,df_mapping,"gear",FALSE)
#'   
#'   # Get the dataframe mapped: dimension "gear" mapped to ISSCFG. The column "gear" has its values changed compared to the ones before the execution of the function. The codes have been mapped following the dimensions "gear" and "source_authority", since the dataset of mappings between code lists had both dimensions.
#'   df_mapped_df<-df_mapped$df
#'   head(df_mapped_df) 
#'   
#'   # Get information regarding the data that were not mapped.
#'   df_mapped$stats
#'  
#' @author Paul Taconet, \email{paul.taconet@@ird.fr}
#'      



map_codelist<-function(df_input,df_mapping,dimension_to_map,keep_src_code=FALSE){
  
  cat(paste0("\n mapping dimension ",dimension_to_map," with code list mapping"))
  
  column_names_df_input<-colnames(df_input)
  
 colnames(df_mapping)[colnames(df_mapping) == "src_code"] <- dimension_to_map
 
  df_input<-left_join(df_input,df_mapping)
  
  if (keep_src_code==FALSE){
  df_input[,dimension_to_map]<-df_input$trg_code
  df_input <- df_input[column_names_df_input]
  } else {
  colnames(df_input)[colnames(df_input) == dimension_to_map] <-paste0(dimension_to_map,"_src_code")
  colnames(df_input)[colnames(df_input) == "trg_code"] <- dimension_to_map
  df_input <- df_input[c(column_names_df_input,paste0(dimension_to_map,"_src_code"))]
  }
  
  #group by the new dimension
  if (keep_src_code==FALSE){
    df_input<-df_input %>% group_by_(.dots = setdiff(column_names_df_input,"value")) %>% summarise(value=sum(value))
  }
  
  df_input<-data.frame(df_input)
  
  # statistics on the percentage of data that are not mapped
  stats_data_not_mapped <- df_input %>% 
    mutate(sum_mapped_unmapped = ifelse(is.na(df_input[,dimension_to_map]), "sum_value_not_mapped", "sum_value_mapped")) %>% 
    group_by(sum_mapped_unmapped,unit) %>% 
    summarise(sum_value_by_dimension = sum(value))
  
  # Replace NA by "UNK"
  df_input[,dimension_to_map][which(is.na(df_input[,dimension_to_map]))]="UNK"
  
  stats_data_not_mapped<-dcast(setDT(stats_data_not_mapped),unit~sum_mapped_unmapped, sum)
  if (!("sum_value_not_mapped" %in% colnames(stats_data_not_mapped))) {
    stats_data_not_mapped$sum_value_not_mapped=0
  } 
  stats_data_not_mapped[is.na(stats_data_not_mapped)] <- 0
  stats_data_not_mapped$percentage_not_mapped<-stats_data_not_mapped$sum_value_not_mapped/stats_data_not_mapped$sum_value_mapped*100
  
  return(list(df=df_input,stats=stats_data_not_mapped))
}