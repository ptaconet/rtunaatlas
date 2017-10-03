#' @name extract_dataset
#' @aliases extract_datasets
#' @title Extract a dataset available in Sardara database
#' @description This function outputs a dataset available in the Sardara database.
#' @export extract_dataset
#'
#'
#' @usage 
#' extract_dataset(con,dataset_name)
#'    
#' @param con a wrapper of rpostgresql connection (connection to a database)
#' @param dataset_name string. The name of a dataset in the database (column "dataset_name" of the table metadata.metadata)
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
#' # Extract a raw dataset
#' raw_dataset<-extract_dataset(con,"global_catch_5deg_1m_1950_01_01_2016_01_01_tunaatlasIRD_level1")
#'
#' # Extract a code list
#' code_list<-extract_dataset(con,"species_asfis")
#' 
#' # Extract a mapping between code lists
#' code_list_mapping<-extract_dataset(con,"codelist_mapping_species_iotc_speciesgroup_tunaatlas")
#'
#' 
#' @author Paul Taconet, \email{paul.taconet@@ird.fr}
#'


extract_dataset<-function(con,dataset_name){
  
  # get metadata row for the provided dataset_name
  metadata_row<-dbGetQuery(con, paste0("SELECT * FROM metadata.metadata where dataset_name='",dataset_name,"'"))
  
  if (length(metadata_row)==0){ stop("the dataset provided does not exist in the database") }
  
  # retrieve query to execute using the function getSQLSardaraQueries
  query<-getSQLSardaraQueries(con,dataset_name)$query_CSV
  
  df<-dbGetQuery(con,query)

  return(df)
  
}