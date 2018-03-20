#' @name list_metadata_datasets
#' @aliases list_metadata_datasets list_metadata_raw_datasets list_metadata_codelists list_metadata_codelists_mapping
#' @title List metadata of the datasets available in Sardara database
#' @description This function outputs the metadata of the data available in the Sardara database.
#' @export list_metadata_datasets list_metadata_raw_datasets list_metadata_codelists list_metadata_codelists_mapping
#'
#'
#' @usage 
#' list_metadata_datasets(con,identifier=NULL,source_authority=NULL)
#' list_metadata_raw_datasets(con,identifier=NULL,source_authority=NULL,variable=NULL,spatial_resolution=NULL,level_of_correction=NULL)
#' list_metadata_codelists(con,identifier=NULL,source_authority=NULL,dimension=NULL)
#' list_metadata_codelists_mapping(con,identifier=NULL,source_authority=NULL,dimension=NULL)
#'    
#' @param con a wrapper of rpostgresql connection (connection to a database)
#' @param identifier NULL or string. If not NULL, extracts the metadata of the identifier stated. In this case, all the other parameters will be ignored
#' @param source_authority NULL or vector of strings. If not NULL, filter available datasets by the source authority in charge of producing the source statistics collated and harmonized. E.g. c("IOTC","ICCAT") will provide the metadata only for the data produced by IOTC and ICCAT.
#' @param dimension NULL or vector of strings. For codelists and mappings only. If not NULL, filter available code lists / mappings by dimensions. E.g. c("gear","species") will provide the metadata only for code lists/mappings between code lists related to fishing gears and species.
#' @param variable NULL or vector of strings. For datasets only. If not NULL, filter available code lists / mappings by variable. Three variables are available in the Tuna atlas database: catch, effort, catch_at_size
#' @param spatial_resolution NULL or real. For datasets only. If not NULL, filter available datasets that are defined on that spatial resolution (in degrees).
#' @param level_of_correction NULL or integer. For datasets only. If not NULL, filter available datasets that are have that level of correction provided.
#'
#' @return a data.frame of metadata for the data available in Sardara database. The meaning of the columns of the output data.frame is provided in the \href{https://docs.google.com/spreadsheets/d/1HAgGQzd7GgPXLYgrHECZyqGy4Vsf4IOlppSbmhWEuaE/edit#gid=0}{data dictionary of the metadata table of Sardara database}. ## to fill in this description when it is finalized
#'
#' @details 
#' Three types of data are available in Sardara database:
#' \itemize{
#'  \item{"raw_dataset": }{Datasets. Three types of datasets are available:}
#'   \itemize{
#'   \item{"catch": }{Quantity of of fish in number or biomass harvested in a given stratum}
#'   \item{"effort": }{Quantity of effort (expressed in a given unit such as number of sets, number of fishing hours, number of hooks, etc.) exerted in a given stratum}
#'   \item{"catch-at-size": }{Quantity of fish at a given size (or size class) harvested in a given stratum}
#'   }
#'  \item{"codelist": }{Reference tables with at least codes, and usualy labels associated to these codes.}
#'  \item{"mapping": }{Tables that establishes mappings / correspondences between codes from various reference tables.}
#' }
#'  
#' The meaning of the columns of the raw dataset is provided in the \href{https://docs.google.com/spreadsheets/d/1BUppXu-Z_YX8cJaNISfk9KrKrKJ6y0Dd2XCfIvjBRQc/edit#gid=747135938}{data dictionary of raw datasets}.  ## to fill in this description when it is finalized
#' 
#' \itemize{
#' \item{\code{list_metadata_datasets}} {lists the metadata of all the types of datasets (raw_dataset, codelists, mappings)}
#' \item{\code{list_metadata_raw_datasets}} {lists only the metadata of raw_datasets}
#' \item{\code{list_metadata_codelists}} {lists only the metadata of the code lists}
#' \item{\code{list_metadata_mappings}} {lists only the metadata of the mappings between code lists}
#' }
#' 
#' @family discover data
#' 
#' 
#' @examples
#' 
#' # List the available source IOTC datasets:
#' metadata_iotc_datasets<-list_metadata_datasets(db_connection_sardara_world(),source_authority=c("IOTC"))
#' 
#' # List the available code lists for WCPFC and IATTC
#' metadata_iccat_code_lists<-list_metadata_codelists(db_connection_sardara_world(),source_authority=c("WCPFC","IATTC"))
#' 
#' # List the available raw datasets of catch and of effort that are defined on 5° grid resolution
#' metadata_raw_dataset_catch_5deg<-list_metadata_raw_datasets(db_connection_sardara_world(),variable=c("catch","effort"),spatial_resolution=5)
#'
#' 
#' @author Paul Taconet, \email{paul.taconet@@ird.fr}
#'
  
  list_metadata_datasets<-function(con,identifier=NULL,source_authority=NULL){
    
    where_clause<-NULL
    
    if(!is.null(identifier)){
      where_clause<-paste0(where_clause," and identifier = '",identifier,"'")
    }
    
    if(!is.null(source_authority)){
      source_authority<-paste(source_authority, collapse = "','")
      where_clause<-paste0(where_clause," and source IN ('",source_authority,"')")
    }
    
    metadata_datasets<-dbGetQuery(con,paste("SELECT * from metadata.metadata where lineage is not null ",where_clause," order by source,identifier",sep=))
    
    if (nrow(metadata_datasets)==0) {cat(paste0("There is no dataset that corresponds to your query"))}
    
    return(metadata_datasets) 
  }

list_metadata_codelists<-function(con,identifier=NULL,source_authority=NULL,dimension=NULL){
  
  where_clause<-NULL
  
  if(!is.null(identifier)){
    where_clause<-paste0(where_clause," and identifier = '",identifier,"'")
  }
  
  if(!is.null(source_authority)){
    source_authority<-paste(source_authority, collapse = "','")
    where_clause<-paste0(where_clause," and source IN ('",source_authority,"')")
  }
  
  if(!is.null(dimension)){
    dimension<-paste(dimension, collapse = "','")
    where_clause<-paste0(where_clause," and split_part(table_name, '.', 1) IN ('",dimension,"')")
  }
  
  metadata_datasets<-dbGetQuery(con,paste("SELECT * from metadata.metadata where dataset_type='codelist' ",where_clause," order by source,identifier",sep=))
  
  if (nrow(metadata_datasets)==0) {cat(paste0("There is no dataset that corresponds to your query"))}
  
 return(metadata_datasets) 
}




list_metadata_codelists_mapping<-function(con,identifier=NULL,source_authority=NULL,dimension=NULL){
  
  where_clause<-NULL
  
  if(!is.null(identifier)){
    where_clause<-paste0(where_clause," and identifier = '",identifier,"'")
  }
  
  if(!is.null(source_authority)){
    source_authority<-paste(source_authority, collapse = "','")
    where_clause<-paste0(where_clause," and source IN ('",source_authority,"')")
  }
  
  if(!is.null(dimension)){
    dimension<-paste(dimension, collapse = "','")
    where_clause<-paste0(where_clause," and split_part(table_name, '.', 1) IN ('",dimension,"')")
  }
  
  metadata_datasets<-dbGetQuery(con,paste("SELECT * from metadata.metadata where dataset_type='mapping' ",where_clause," order by source,identifier",sep=))
  
  if (nrow(metadata_datasets)==0) {cat(paste0("There is no dataset that corresponds to your query"))}
  
  return(metadata_datasets) 
}



list_metadata_raw_datasets<-function(con,identifier=NULL,source_authority=NULL,variable=NULL,spatial_resolution=NULL,level_of_correction=NULL,ocean=NULL){
  
  where_clause<-NULL
  
  if(!is.null(identifier)){
    where_clause<-paste0(where_clause," and identifier = '",identifier,"'")
  }
  
  if(!is.null(source_authority)){
    source_authority<-paste(source_authority, collapse = "','")
    where_clause<-paste0(where_clause," and source IN ('",source_authority,"')")
  }
  
  if(!is.null(ocean)){
    source_authority<-paste(source_authority, collapse = "','")
    where_clause<-paste0(where_clause," and identifier LIKE '%",ocean,"%'")
  }
  
  if(!is.null(variable)){
    variable<-paste(variable, collapse = "','")
    where_clause<-paste0(where_clause," and split_part(table_name, '.', 2) IN ('",variable,"')")
  }
  
  if(!is.null(spatial_resolution)){
    where_clause<-paste0(where_clause," and identifier LIKE '%_",spatial_resolution,"deg_%'")
  }
  
  if(!is.null(level_of_correction)){
    where_clause<-paste0(where_clause," and identifier LIKE '%_level",level_of_correction,"%'")
  }
  
  metadata_datasets<-dbGetQuery(con,paste("SELECT * from metadata.metadata where dataset_type='raw_dataset' ",where_clause," order by source,identifier",sep=))
  
  if (nrow(metadata_datasets)==0) {cat(paste0("There is no dataset that corresponds to your query"))}
  
  return(metadata_datasets) 
}
