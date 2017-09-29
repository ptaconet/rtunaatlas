#' @name list_datasets
#' @aliases list_datasets list_raw_datasets list_codelists list_codelists_mapping
#' @title List metadata of the datasets available in Sardara database
#' @description This function outputs the metadata of the data available in the Sardara database.
#' @export
#'
#'
#' @usage 
#' list_datasets(source_authority=NULL,db="sardara_world")
#' list_raw_datasets(source_authority=NULL,variable=NULL,spatial_resolution=NULL,level_of_correction=NULL,db="sardara_world")
#' list_codelists<-function(source_authority=NULL,dimension=NULL,db="sardara_world")
#' list_mappings<-function(source_authority=NULL,dimension=NULL,db="sardara_world")
#'    
#' @param source_authority NULL or vector of strings. If not NULL, filter available datasets by the source authority in charge of producing the source statistics collated and harmonized. E.g. c("IOTC","ICCAT") will provide the metadata only for the data produced by IOTC and ICCAT.
#' @param dimension NULL or vector of strings. For codelists and mappings only. If not NULL, filter available code lists / mappings by dimensions. E.g. c("gear","species") will provide the metadata only for code lists/mappings between code lists related to fishing gears and species.
#' @param variable NULL or vector of strings. For datasets only. If not NULL, filter available code lists / mappings by variable. Three variables are available in Sardara: catch, effort, catch_at_size
#' @param spatial_resolution NULL or real. For datasets only. If not NULL, filter available datasets that are defined on that spatial resolution (in degrees).
#' @param level_of_correction NULL or integer. For datasets only. If not NULL, filter available datasets that are have that level of correction provided.
#' @param db the name of the database to connect to. Defaut connects to Sardara database hosted on the BlueBridge project servers.
#'
#' @return a data.frame of metadata for the data available in Sardara database. For the meaning of the columns of the output data.frame, see \href{https://docs.google.com/spreadsheets/d/1HAgGQzd7GgPXLYgrHECZyqGy4Vsf4IOlppSbmhWEuaE/edit#gid=0}{data dictionary of the metadata table of Sardara database}. ## to fill in this description when it is finalized
#'
#' @details 
#' Three types of data are available in Sardara database:
#' \itemize{
#'  \item{"raw_dataset": }{Datasets. Three types of datasets are available:
#'   \itemize{
#'   \item{"catch": }{Quantity of of fish in number or biomass harvested in a given stratum}
#'   \item{"effort": }{Quantity of effort (expressed in a given unit such as number of sets, number of fishing hours, number of hooks, etc.) exerted in a given stratum}
#'   \item{"catch-at-size": }{Quantity of fish at a given size (or size class) harvested in a given stratum}
#'   }
#'   }
#'  \item{"codelist": }{Reference tables with at least codes, and usualy labels associated to these codes.}
#'  \item{"mapping": }{Tables that establishes mappings / correspondences between codes from various reference tables.}
#' }
#' . 
#' For the meaning of the columns of the raw dataset, see \href{https://docs.google.com/spreadsheets/d/1BUppXu-Z_YX8cJaNISfk9KrKrKJ6y0Dd2XCfIvjBRQc/edit#gid=747135938}{data dictionary of raw datasets}.  ## to fill in this description when it is finalized
#' 
#' \code{list_datasets} lists the metadata of all the types of datasets (raw_dataset, codelists, mappings)
#' \code{list_raw_datasets} lists only the metadata of raw_datasets
#' \code{list_codelists} lists only the metadata of the code lists
#' \code{list_mappings} lists only the metadata of the mappings between code lists
#' 
#' @family list available data
#' 
#' 
#' @examples
#' 
#' # List the available source IOTC datasets:
#' metadata_iotc_datasets<-list_datasets(c("IOTC"))
#' 
#' # List the available code lists for WCPFC and CCSBT
#' metadata_iccat_code_lists<-list_codelists(c("WCPFC","CCSBT"))
#' 
#' # List the available raw datasets of catch and of effort that are defined on 5° grid resolution
#' metadata_raw_dataset_catch_5deg<-list_raw_datasets(variable=c("catch","effort"),spatial_resolution=5)
#'
#' 
#' @author Paul Taconet, \email{paul.taconet@@ird.fr}
#' @import RPostgreSQL   


list_codelists<-function(source_authority=NULL,dimension=NULL,db="sardara_world"){
  
  con<-db_connexion(db)
  
  where_clause<-NULL
  
  if(!is.null(source_authority)){
    source_authority<-paste(source_authority, collapse = "','")
    where_clause<-paste0(where_clause," and dataset_origin_institution IN ('",source_authority,"')")
  }
  
  if(!is.null(dimension)){
    dimension<-paste(dimension, collapse = "','")
    where_clause<-paste0(where_clause," and split_part(table_name, '.', 1) IN ('",dimension,"')")
  }
  
  metadata_datasets<-dbGetQuery(con,paste("SELECT * from metadata.metadata where dataset_lineage is not null and table_type='codelist' ",where_clause," order by dataset_origin_institution,table_name",sep=))
  
  dbDisconnect(con)
  
 return(metadata_datasets) 
}




list_codelists_mapping<-function(source_authority=NULL,dimension=NULL,db="sardara_world"){
  
  con<-db_connexion(db)
  
  where_clause<-NULL
  
  if(!is.null(source_authority)){
    source_authority<-paste(source_authority, collapse = "','")
    where_clause<-paste0(where_clause," and dataset_origin_institution IN ('",source_authority,"')")
  }
  
  if(!is.null(dimension)){
    dimension<-paste(dimension, collapse = "','")
    where_clause<-paste0(where_clause," and split_part(table_name, '.', 1) IN ('",dimension,"')")
  }
  
  
  metadata_datasets<-dbGetQuery(con,paste("SELECT * from metadata.metadata where dataset_lineage is not null and table_type='mapping' ",where_clause," order by dataset_origin_institution,table_name",sep=))
  
  dbDisconnect(con)
  
  return(metadata_datasets) 
}



list_datasets<-function(source_authority=NULL,variable=NULL,spatial_resolution=NULL,level_of_correction=NULL,db="sardara_world"){
  
  con<-db_connexion(db)
  
  where_clause<-NULL
  
  if(!is.null(source_authority)){
    source_authority<-paste(source_authority, collapse = "','")
    where_clause<-paste0(where_clause," and dataset_origin_institution IN ('",source_authority,"')")
  }
  
  if(!is.null(variable)){
    variable<-paste(variable, collapse = "','")
    where_clause<-paste0(where_clause," and split_part(table_name, '.', 2) IN ('",variable,"')")
  }
  
  if(!is.null(spatial_resolution)){
    where_clause<-paste0(where_clause," and dataset_name LIKE '%_",spatial_resolution,"deg_%'")
  }
  
  if(!is.null(level_of_correction)){
    where_clause<-paste0(where_clause," and dataset_name LIKE '%_level",level_of_correction,"%'")
  }
  
  metadata_datasets<-dbGetQuery(con,paste("SELECT * from metadata.metadata where dataset_lineage is not null and table_type='raw_dataset' ",where_clause," order by dataset_origin_institution,table_name",sep=))
  
  dbDisconnect(con)
  
  return(metadata_datasets) 
}



extract_codelist<-function(dataset_name,db="sardara_world"){
  
  con<-db_connexion(db)
  
  # get metadata row for the provided dataset_name
  metadata_row<-dbGetQuery(con, paste0("SELECT * FROM metadata.metadata where dataset_name='",dataset_name,"'"))
  
  if (length(metadata_row)==0){ stop("the dataset provided does not exist in the database") }
  
  static_metadata_table_name=metadata_row$table_name
  
  # 1) extraction des noms de colonnes code et label du code list source. Ces infos sont contenues dans la table metadata.codelists_codes_labels_column_names. ces colonnes seront nommÃƒÆ’Ã‚Â©es 'code' et 'label' dans le codelist extrait (WFS et csv)
  code_label_column_name<-dbGetQuery(con,paste0("SELECT code_column,english_label_column FROM metadata.codelists_codes_labels_column_names WHERE table_name='",static_metadata_table_name,"'"))  
  # 2) S'il n'y a pas de label, on remplit la colonne 'label' avec des 'NULL'  
  if (is.na(code_label_column_name$english_label_column[1])){
    code_label_column_name$english_label_column[1]="NULL"
  }
  # 3)  on va chercher les autres colonnes du code list
  all_column_names<-dbGetQuery(con,paste0("SELECT column_name from information_schema.columns where table_schema||'.'||table_name='",static_metadata_table_name,"'"))
  all_column_names_vector <- as.vector(all_column_names$column_name)
  other_columns_column_names<-setdiff(all_column_names_vector, c(code_label_column_name$code_column[1],code_label_column_name$english_label_column[1]))
  if (length(other_columns_column_names)>0){
    other_columns_column_names<-paste0(", ",paste(as.character(other_columns_column_names),collapse=", ",sep="")," ")
  } else {
    other_columns_column_names<-NULL
  }
  # 4) s'il s'agit d'un code list de type spatial, on prend en plus la gÃƒÆ’Ã‚Â©omÃƒÆ’Ã‚Â©trie. Ce morceau de requete SQL est ÃƒÆ’  changer pour le CSV car on ne prendra pas l'attribut "the_geom" mais plutot un WKT.
  if (substr(static_metadata_table_name,1,4)=='area'){
    # Get geometry column name, type and srid
    table_geometry_information<-dbGetQuery(con,paste0("select * from geometry_columns where f_table_schema='area' and f_table_name='",substring(static_metadata_table_name, 6),"'"))
    query<-paste("SELECT ",code_label_column_name$code_column[1]," as code,",code_label_column_name$english_label_column[1]," as label, st_astext(",table_geometry_information$f_geometry_column,") as geom_wkt ",other_columns_column_names,"  FROM ",static_metadata_table_name,sep="")
    
  } else {
    
    # 5) s'il s'agit d'un code list non spatial (classique), on prend le code list 
    query <- paste("SELECT ",code_label_column_name$code_column[1]," as code,",code_label_column_name$english_label_column[1]," as label ",other_columns_column_names,"  FROM ",static_metadata_table_name,sep="")
  }
  
  codelist<-dbGetQuery(con,query)
  
  dbDisconnect(con)
  
  return(codelist)
}
  