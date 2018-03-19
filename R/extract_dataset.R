#' @name extract_dataset
#' @aliases extract_datasets
#' @title Extract a dataset available in Sardara database
#' @description This function outputs a dataset available in the Tuna atlas database.
#' @export extract_dataset extract_and_merge_multiple_datasets
#'
#'
#' @usage 
#' extract_dataset(con,metadata_dataset,labels=FALSE)
#'
#' @param con a wrapper of rpostgresql connection (connection to a database)
#' @param metadata_dataset data.frame of type "metadata" (one row extracted from the table metadata.metadata).
#' @param labels boolean extract the dataset with codes and labels? TRUE = codes + labels . FALSE = only codes. Default is FALSE
#'
#' @return a data.frame of the data available in the database and set as \code{dataset_name}
#'
#' @details 
#' 
#' The meaning of the columns of a raw dataset is provided in the \href{https://docs.google.com/spreadsheets/d/1BUppXu-Z_YX8cJaNISfk9KrKrKJ6y0Dd2XCfIvjBRQc/edit#gid=747135938}{data dictionary of raw datasets}.  ## to fill in this description when it is finalized
#' 
#' @family access data
#' 
#' 
#' @examples
#' 
#' # Connect to Tuna atlas database
#' con<-db_connection_tunaatlas_world()
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


extract_dataset<-function(con,metadata_dataset,labels=FALSE){
  
  if(nrow(metadata_dataset)==0){stop("There is no dataset that corresponds to your query")}
  
  if (labels==FALSE){
  # retrieve query to execute using the function getSQLSardaraQueries
  query<-getSQLSardaraQueries(con,metadata_dataset)$query_CSV
  } else {
  query<-getSQLSardaraQueries(con,metadata_dataset)$query_CSV_with_labels
  }
  
  df<-dbGetQuery(con,query)

  return(df)
  
}


# columns_to_keep : vector of columns to keep. If columns of the source datasets are missing, a column with this dimension will be added to the dataset and filled with value = UNK (unknown)
# metadata_datasets data.frame of type "metadata" (muliple rows extracted from the table metadata.metadata).

extract_and_merge_multiple_datasets<-function(con,metadata_datasets,columns_to_keep,labels=FALSE){
  
  if(nrow(metadata_datasets)==0){stop("There is no dataset that corresponds to your query")}
  
  df<-NULL
  
  for (i in 1:nrow(metadata_datasets)){
    cat(paste0("\nretrieving dataset ",metadata_datasets$identifier[i]))
    df_thisdf<-extract_dataset(con,metadata_datasets[i,],labels)
    
    # keep only wanted columns
    df_thisdf <- df_thisdf[(names(df_thisdf) %in% columns_to_keep)]
    
    # add missing columns and fill them with "UNK" values
    for (j in 1:length(columns_to_keep)){
      if (!(columns_to_keep[j]) %in% names(df_thisdf)){
        cat(paste0("\ndimension ",columns_to_keep[j]," is missing in the dataset. Adding this dimension to the dataset and filling it with values UNK (unknown)"))
        df_thisdf[,columns_to_keep[j]]<-"UNK"
      }
    }
    
    df<-rbind(df,df_thisdf)
  }
  
  return(df)
  
}