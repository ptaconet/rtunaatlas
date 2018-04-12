#' @name list_dataset_available_dimensions
#' @aliases list_dataset_available_dimensions
#' @title List dimensions available in a raw dataset
#' @description This function returns the dimensions available in a dataset. A dimension is set as available in a dataset if at least 1 value does exist for this dimension.
#' @export 
#'
#' @usage 
#' list_dataset_available_dimensions(con,dataset_metadata)
#'    
#' @param con a wrapper of rpostgresql connection (connection to a database)
#' @param dataset_metadata data.frame of type "metadata" (one row extracted from the table metadata.metadata). Column table_type must be "raw_dataset"
#' 
#' @details 
#' 
#' Example to understand the meaning of this function: Let us take the following dataset:
#' 
#'  \tabular{rrrrrrr}{
#' flag	\tab time_start	\tab time_end	  \tab geographic_identifier	\tab gear	\tab species	\tab value\cr
#' 	\tab 1992-02-01	\tab 1992-03-01	\tab 235140	\tab LL	\tab YFT	\tab 0.05 \cr
#' 	\tab 1992-06-01	\tab 1992-07-01	\tab 230125	\tab LL	\tab YFT	\tab 0.42\cr
#' 	\tab 1992-07-01	\tab 1992-08-01	\tab 235140	\tab LL	\tab YFT	\tab 1.05\cr
#' 	\tab 1992-07-01	\tab 1992-08-01	\tab 240140	\tab LL	\tab YFT	\tab 0.15\cr
#' 	\tab 1992-08-01	\tab 1992-09-01	\tab 240140	\tab LL	\tab YFT	\tab 0.61\cr
#' 	\tab 1992-11-01	\tab 1992-12-01	\tab 230120	\tab LL	\tab YFT	\tab 0.15\cr
#' 	\tab 1992-11-01	\tab 1992-12-01	\tab 230125	\tab LL	\tab YFT	\tab 0.15\cr
#' 	\tab 1992-11-01	\tab 1992-12-01	\tab 235115	\tab LL	\tab YFT	\tab 1.65\cr
#' 	\tab 1992-11-01	\tab 1992-12-01	\tab 235135	\tab LL	\tab YFT	\tab 5\cr
#' 	\tab 1992-12-01	\tab 1993-01-01	\tab 235110	\tab LL	\tab YFT	\tab 0.08\cr
#' 	\tab 1992-12-01	\tab 1993-01-01	\tab 240140	\tab LL	\tab YFT	\tab 0.12
#'  }
#' 
#' In the above table, the dimension "flag" as not got any values. Hence, for this table the dimension "flag" is considered not to be available. The dimensions that will be returned by the function \code{list_dataset_available_dimensions} applied on this dataset are: time, area, gear, species 
#' 
#' @return a vector with the name of the dimensions available in the dataset. 
#' 
#' @family discover data
#' 
#' @examples
#' 
#' # Connect to Tuna atlas database
#' con<-db_connection_tunaatlas_world()
#' 
#' # List the available dimensions in the dataset "global_catch_5deg_1m_1950_01_01_2016_01_01_tunaatlasIRD_level1":
#' dataset_metadata(con,identifier="global_catch_5deg_1m_1950_01_01_2016_01_01_tunaatlasIRD_level1"))
#' list_dataset_available_dimensions(db_connection_tunaatlas_world(),dataset_metadata)
#' 
#' @author Paul Taconet, \email{paul.taconet@@ird.fr}
#'


list_dataset_available_dimensions<-function(con,dataset_metadata){
  
  if(nrow(dataset_metadata)==0){stop("There is no dataset that corresponds to your query")}
  
  db_dimensions_parameters<-read.csv(system.file("extdata", "db_dimensions_parameters.csv",package="rtunaatlas"),stringsAsFactors = F)
  
  # get type of variable of the dataset
  table_name=dataset_metadata$database_table_name
  id_metadata=dataset_metadata$id_metadata
  dataset_type=dataset_metadata$dataset_type
  
  if (!(dataset_type=="raw_dataset")) { stop("the dataset provided is not a raw_dataset. You must provide a dataset of type raw_dataset") }
  
  
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
  if (nrow(unique_values_dimension)==1 & unique_values_dimension[1,1]==0) { } else { 
      available_dimensions_in_dataset_not_null<-c(available_dimensions_in_dataset_not_null, available_dimensions_in_dataset$dimension[i]) }
  
  } 
  
  
return(available_dimensions_in_dataset_not_null)  
 
}