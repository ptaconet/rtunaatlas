#' @name list_dataset_available_dimensions
#' @aliases list_dataset_available_dimensions
#' @title List dimensions available in a raw dataset
#' @description This function list the dimensions available in a dataset. A dimension is set as available if at least 1 value does exist for this dimension.
#' @export 
#'
#' @usage 
#' list_dataset_available_dimensions(con,dataset_name)
#'    
#' @param con a wrapper of rpostgresql connection (connection to a database)
#' @param dataset_name string. The name of a dataset of type "raw_dataset" in the database (column "dataset_name" of the table metadata.metadata)
#' 
#' @examples
#' 
#' # List the available dimensions in the dataset "indian_ocean_effort_1970_01_01_2015_08_01_tunaatlasIOTC_2017_level0_coastal":
#' list_dataset_available_dimensions(db_connection_sardara_world(),"indian_ocean_effort_1970_01_01_2015_08_01_tunaatlasIOTC_2017_level0_coastal")
#' 
#' @author Paul Taconet, \email{paul.taconet@@ird.fr}
#'


list_dataset_available_dimensions<-function(con,dataset_name){
  
  db_dimensions_parameters<-read.csv("inst/extdata/db_dimensions_parameters.csv",stringsAsFactors = F)
  
  metadata_row<-dbGetQuery(con,paste0("SELECT * from metadata.metadata where dataset_name='",dataset_name,"'"))
  
  # get type of variable of the dataset
  table_name=metadata_row$table_name
  id_metadata=metadata_row$id_metadata
  table_type=metadata_row$table_type
  
  if (!(table_type=="raw_dataset")) { stop("the dataset provided is not a raw_dataset. You must provide a dataset of type raw_dataset") }
  
  
  variable<-gsub("fact_tables.","",table_name)
  # get columns of this dataset
  columns_fact_tables<-dbGetQuery(con,paste0("select column_name from information_schema.columns where table_name='",variable,"'"))
  
  # get corresponding labels for these columns (the ones that are used in the views of the schemas "tunaatlas....")
  available_dimensions_in_dataset<-db_dimensions_parameters[which(db_dimensions_parameters$db_fact_table_colname %in% columns_fact_tables$column_name),]

  # get only columns that are not empty (i.e. not filled with only with NULL values)
  available_dimensions_in_dataset_not_null<-NULL
    
  for (i in 1:nrow(available_dimensions_in_dataset)){
    
  unique_values_dimension<-dbGetQuery(con,paste0("SELECT DISTINCT ", available_dimensions_in_dataset$db_fact_table_colname[i] ," FROM ",table_name," WHERE id_metadata=",id_metadata))
  
  # the condition hereunder filters the columns that are not null (i.e. do not have only UNKNOWN values)
  if(!(nrow(unique_values_dimension==1) & unique_values_dimension[1,1]==0)) {
    available_dimensions_in_dataset_not_null<-c(available_dimensions_in_dataset_not_null, available_dimensions_in_dataset$dimension[i])
  } 
  
  }
  
return(available_dimensions_in_dataset_not_null)  
 
}