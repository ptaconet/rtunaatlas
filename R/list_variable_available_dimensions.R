#' @name list_variable_available_dimensions
#' @aliases list_variable_available_dimensions
#' @title List dimensions available for a given variable
#' @description This function returns all the dimensions possibly available for a given variable stored on Sardara DB. NB: the term "variable" is sometimes called "fact".
#' @export 
#'
#' @usage 
#' list_variable_available_dimensions(con,variable)
#'    
#' @param con a wrapper of rpostgresql connection (connection to a database)
#' @param variable string. The name of a variable (e.g. "catch", or "effort", or "catch_at_size")
#' 
#' @details 
#' This function should not be confused with the function \link{list_dataset_available_dimensions}. The difference between both functions is: 
#' \itemize{
#'  \item{list_variable_available_dimensions: }{Returns all the dimensions possibly available for a given variable.}
#'  \item{list_dataset_available_dimensions: }{Returns the dimensions specifically available for a given dataset.}
#' }
#' 
#' A variable has a list of dimensions associated. A dataset belongs to a variable (e.g. dataset of catch, of effort) but might not contain values for all the dimensions associated to its variable.
#' 
#' For instance: the variable "catch" is associated to the dimensions: \it{flag}, \it{gear}, \it{species}, \it{schooltype}, \it{species}, \it{catchtype}, \it{unit}, \it{time}, \it{area} 
#' However, the dimensions available in the dataset "indian_ocean_catch_ll_1952_11_01_2016_01_01_tunaatlasIOTC_2017_level0" are the followings: \it{flag}, \it{gear}, \it{species}, \it{species", \it{catchtype}, \it{unit}, \it{time}, \it{area}. The dimension \it{schooltype} is not available in this dataset (in this case, because longliners by definition do not fish on schools).
#' 
#' While the latter lists the dimensions available in a given dataset
#' 
#' @family discover data
#' 
#' @examples
#' 
#' #list dimensions available for variable "catch"
#' list_variable_available_dimensions(db_connection_sardara_world(),"catch")
#' 
#' @author Paul Taconet, \email{paul.taconet@@ird.fr}
#'


list_variable_available_dimensions<-function(con,variable){
  
  # All the dimensions possibly available in Sardara
  db_dimensions_parameters<-read.csv(system.file("extdata", "db_dimensions_parameters.csv",package="rtunaatlas"),stringsAsFactors = F,strip.white=TRUE)

  ## List of available columns for the variable
  dimensions<-dbGetQuery(con,paste0("SELECT column_name as dimension_name from information_schema.columns where table_schema||'.'||table_name='fact_tables.",variable,"'"))
  dimensions <- as.vector(dimensions$dimension_name)
  
  dimensions_available_in_dataset<-db_dimensions_parameters$dimension[which(db_dimensions_parameters$db_fact_table_colname %in% dimensions)]
  
  return(dimensions_available_in_dataset)
  
}