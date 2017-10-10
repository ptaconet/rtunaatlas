#' @name get_dsd_of_dataset
#' @aliases get_dsd_of_dataset
#' @title Get the data structure definition of a raw_dataset
#' @description This function creates the data structure definition of a raw_dataset stored in the database.
#' 
#' 
#' 
#' 
#' 
#' @family discover data
#' 
#' 

get_dsd_of_dataset<-function(con,dataset_metadata){  # A FINIR
  
  #get template of DSD
  
  
  # list dimensions available in the dataset
  dimensions_available<-list_dataset_available_dimensions(con,dataset_metadata)
  
  # for each dimension, retrieve the metadata of code list associated and extract useful information for the dsd
  
  for (i in 1:length(dimensions_available)){
    
    metadata_codelist_dimension<-get_codelist_of_dimension(con,dimensions_available[i])
    
  
    
  }
  
  
  return(dsd)
}