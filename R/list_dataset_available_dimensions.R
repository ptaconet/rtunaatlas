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
#' # List the available source IOTC datasets:
#' metadata_iotc_datasets<-list_datasets(db_connection_sardara_world,c("IOTC"))$dataset_name
#' 
#' metadata_iotc_datasets
#' 
#' # List the available dimensions in the first dataset :
#' list_dataset_available_dimensions(con,metadata_iotc_datasets[1])
#' 
#' @author Paul Taconet, \email{paul.taconet@@ird.fr}
#'


list_dataset_available_dimensions<-function(con,dataset_name){
  
  db_dimensions_parameters<-read.csv("inst/extdata/db_dimensions_parameters.csv",stringsAsFactors = F)
  
  # get type of variable of the dataset
  table_name=dbGetQuery(con,paste0("SELECT table_name from metadata.metadata where dataset_name='",dataset_name,"'"))$table_name
  id_metadata=dbGetQuery(con,paste0("SELECT id_metadata from metadata.metadata where dataset_name='",dataset_name,"'"))$id_metadata
  
  variable<-gsub("fact_tables.","",table_name)
  # get columns of this dataset
  columns_fact_tables<-dbGetQuery(con,paste0("select column_name from information_schema.columns where table_name='",variable,"'"))
  
  # get corresponding labels for these columns (the ones that are used in the views of the schemas "tunaatlas....")
  available_dimensions_in_dataset<-db_dimensions_parameters[which(db_dimensions_parameters$db_fact_table_colname %in% columns_fact_tables$column_name),]

  # get only columns that are not empty (i.e. not filled with only with NULL values)
  available_dimensions_in_dataset_not_null<-NULL
    
  for (i in 1:nrow(available_columns_in_dataset)){
    
  unique_values_dimension<-dbGetQuery(con,paste0("SELECT DISTINCT ", available_dimensions_in_dataset$db_fact_table_colname[i] ," FROM ",table_name," WHERE id_metadata=",id_metadata))
  
  # the condition hereunder filters the columns that are not null (i.e. do not have only UNKNOWN values)
  if(!(nrow(unique_values_dimension==1) & unique_values_dimension[1,1]==0)) {
    available_dimensions_in_dataset_not_null<-c(available_dimensions_in_dataset_not_null, available_dimensions_in_dataset$dimension[i])
  } 
  
  }
  
return(available_dimensions_in_dataset_not_null)  
 
}