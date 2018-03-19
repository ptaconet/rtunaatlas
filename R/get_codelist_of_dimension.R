#' @name get_codelist_of_dimension
#' @aliases get_codelist_of_dimension
#' @title Get the metadata of a code list used in a raw_dataset
#' @description This function extracts the metadata of the code list used in one dimension of a raw_dataset
#' @export
#'
#' @param con a wrapper of rpostgresql connection (connection to a database)
#' @param dataset_metadata data.frame of type "metadata" (one row extracted from the table metadata.metadata).
#' @param dimension_name string. One of the dimensions available in the dataset
#'  
#' @return a data.frame of the metadata of the code list used for the dimension set in the parameter dimension_name
#'
#'@usage get_codelist_of_dimension(con,dataset_metadata,dimension_name)
#'
#'@family discover data
#'
#'@examples
#'
#'
#' # Connect to Tuna atlas database
#' con<-db_connection_tunaatlas_world()
#' 
#'# Retrieve code list of dimension "gear" of dataset "indian_ocean_catch_ll_1952_11_01_2016_01_01_tunaatlasIOTC_2017_level0"
#'
#'
#' dataset_metadata<-list_metadata_datasets(con,dataset_name="global_catch_1950_01_01_2016_01_01_tunaatlasird_level0__2017")
#'
#' # Get the dimensions available in the dataset
#' #list_dataset_available_dimensions(con,dataset_metadata)
#' 
#' # Get the code list used in the dataset for the dimension "gear"
#' get_codelist_of_dimension(con,dataset_metadata,"gear")
#'
#'

get_codelist_of_dimension<-function(con,dataset_metadata,dimension_name){
  
  
  db_dimensions_parameters<-read.csv(system.file("extdata", "db_dimensions_parameters.csv",package="rtunaatlas"),stringsAsFactors = F)
  
  db_dimensions_parameters<- db_dimensions_parameters %>% filter(dimension==dimension_name)
  
  dataset_name_codelist_dimension_query<-paste0("select distinct(",db_dimensions_parameters$db_tablesource_colname,") from ",dataset_metadata$database_table_name," tab join ",db_dimensions_parameters$db_tablename," tab_link on tab_link.",db_dimensions_parameters$db_pkattribute_colname,"=tab.",db_dimensions_parameters$db_pkattribute_colname," where tab.",db_dimensions_parameters$db_pkattribute_colname,"<>0 and tab.id_metadata=",dataset_metadata$id_metadata)
  
  dataset_name_codelist_dimension<-dbGetQuery(con,dataset_name_codelist_dimension_query)
  
  #retrieve metadata row of the code list
  
  metadata_codelist_dimension=dbGetQuery(con,paste0("SELECT * FROM metadata.metadata where identifier='",dataset_name_codelist_dimension,"'"))
  
  return(metadata_codelist_dimension)
  
}