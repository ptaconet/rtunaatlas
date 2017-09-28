#' @name map_codelist
#' @aliases map_codelist
#' @title Maps and replaces one dimension of a fact dataset using a dataset of mappings between code lists
#' @description This function maps one dimension (i.e. column with codes) of a fact dataset to a target code list using a dataset of mappings between code lists. In other words, it makes the correspondance between two code lists for a given dimension, and replaces for this dimension in the fact dataset the old codes by the new codes available in the dataset of mappings between code lists.
#' @export
#'
#'
#' @usage map_codelist(df_input, df_mapping, dimension_to_map)
#'                 
#' @param df_input a data.frame of fact
#' @param df_mapping a data.frame of code list mapping
#' @param dimension_to_map the name (string) of the dimension to map.
#' 
#' @return a list with two objects:
#' \itemize{
#'  \item{"df": }{The input data.frame of fact, where the dimension_to_map has been mapped using the df_mapping}
#'  \item{"stats": }{A data.frame with some information regarding the data not mapped. It provides, for each unit of measure available in the input dataset, the sum and percentage of the data that could not be map because no correspondance are available in the dataset of mappings between code lists}
#' }
#' 
#' @details The data frames of fact and code list mapping must be properly structured. For structures of data frames, see here: \url{http://}. Note that the dataset of code list mapping can have several dimensions that are used for the mapping. However, one single dimension will be mapped - this dimension is the one specified in the dimension_to_map parameter. Some codes might not be mapped, because no correspondance exists between the source code(s) and the target code(s). In the output dataset of the function map_codelist, these unmapped codes are set to NA.  
#' 
#' @family create your own tuna atlas
#' 
#' 
#' @examples
#'   # Open a data.frame of fact
#'   df_input<-read.csv("inst/extdata/fact_table_example.csv",stringsAsFactors = F)
#'   head(df_input)
#'   
#'   # Open a data.frame of mapping between code lists (in this case, mapping between codes for fishing gears used by the tuna RFMOs and the International Standard Statistical Classification of Fishing Gear)
#'   df_mapping<-read.csv("inst/extdata/gear_mapping_to_standard.csv",stringsAsFactors = F,colClasses = "character")
#'   head(df_mapping)
#'  
#'   # Map code lists. Output is a list with two elements (see section "return")
#'   list_df_mapped<-map_codelist(df_input,df_mapping,"gear")
#'   
#'   # Get the dataframe mapped: dimension "gear" mapped to ISSCFG. The column "gear" has its values changed compared to the ones before the execution of the function. The codes have been mapped following the dimensions "gear" and "source_authority", since the dataset of mappings between code lists had both dimensions.
#'   df_mapped<-list_df_mapped$df
#'   head(df_mapped) 
#'   
#'   # Get information regarding the data that were not mapped.
#'   list_df_mapped$stats
#'  
#' @author Paul Taconet, \email{paul.taconet@@ird.fr}
#' @import data.table dplyr     



map_codelist<-function(df_input,df_mapping,dimension_to_map){
  
  df_input<-dplyr::left_join(df_input,df_mapping)
  df_input[,dimension_to_map]<-df_input$codetarget
  df_input$codetarget<-NULL
  
  # statistics on the percentage of data that are not mapped
  stats_data_not_mapped <- df_input %>% 
    dplyr::mutate(sum_mapped_unmapped = ifelse(is.na(df_input[,dimension_to_map]), "sum_value_not_mapped", "sum_value_mapped")) %>% 
    dplyr::group_by(sum_mapped_unmapped,unit) %>% 
    dplyr::summarise(sum_value_by_dimension = sum(value))
  
  stats_data_not_mapped<-data.table::dcast(stats_data_not_mapped,unit~sum_mapped_unmapped, sum)
  if (!("sum_value_not_mapped" %in% colnames(stats_data_not_mapped))) {
    stats_data_not_mapped$sum_value_not_mapped=0
  } 
  stats_data_not_mapped[is.na(stats_data_not_mapped)] <- 0
  stats_data_not_mapped$percentage_not_mapped<-stats_data_not_mapped$sum_value_not_mapped/stats_data_not_mapped$sum_value_mapped*100
  
  return(list(df=df_input,stats=stats_data_not_mapped))
}