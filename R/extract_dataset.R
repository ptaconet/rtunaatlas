#' @name extract_dataset
#' @aliases extract_datasets
#' @title Extract a dataset available in Sardara database
#' @description This function outputs a dataset available in the Sardara database.
#' @export
#'
#'
#' @usage 
#' extract_dataset(con,dataset_name)
#'    
#' @param con a wrapper of rpostgresql connection (connection to a database)
#' @param dataset_metadata data.frame of type "metadata" (one row extracted from the table metadata.metadata).
#'
#' @return a data.frame of the data available in the database and set as \code{dataset_name}
#'
#' @details 
#' 
#' The meaning of the columns of a raw dataset is provided in the \href{https://docs.google.com/spreadsheets/d/1BUppXu-Z_YX8cJaNISfk9KrKrKJ6y0Dd2XCfIvjBRQc/edit#gid=747135938}{data dictionary of raw datasets}.  ## to fill in this description when it is finalized
#' 
#' @family extract data
#' 
#' 
#' @examples
#' 
#' con=db_connection_sardara_world()
#' 
#' # Extract a raw dataset
#' raw_dataset<-extract_dataset(con,list_metadata_datasets(con,dataset_name="global_catch_5deg_1m_1950_01_01_2016_01_01_tunaatlasIRD_level1"))
#'
#' # Extract a code list
#' code_list<-extract_dataset(con,list_metadata_datasets(con,dataset_name="species_asfis"))
#' 
#' # Extract a mapping between code lists )
#' code_list_mapping<-extract_dataset(con,list_metadata_datasets(con,dataset_name="codelist_mapping_species_iotc_speciesgroup_tunaatlas"))
#'
#' 
#' @author Paul Taconet, \email{paul.taconet@@ird.fr}
#'


extract_dataset<-function(con,dataset_metadata){
  
  # retrieve query to execute using the function getSQLSardaraQueries
  query<-getSQLSardaraQueries(con,dataset_metadata)$query_CSV
  
  df<-dbGetQuery(con,query)

  return(df)
  
}