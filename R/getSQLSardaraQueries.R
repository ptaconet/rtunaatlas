#' @name getSQLSardaraQueries
#' @aliases getSQLSardaraQueries
#' @title Get a list of queries to execute to plug services to the datasets available in Sardara
#' @description Get a list of queries to execute to plug services to the datasets available in Sardara
#' @export 
#'
#' @usage 
#' getSQLSardaraQueries(con,dataset_name)
#'    
#' @param con a wrapper of rpostgresql connection (connection to a database)
#' @param dataset_name string. The name of a dataset in the database (column "dataset_name" of the table metadata.metadata)
#' 
#' @return a list with SQL queries and informations on the dataset. Depending on the type of data (raw_dataset, codelist, mapping), the output elements of the list might vary. The elements of the list are:
#' \itemize{
#'  \item{\code{query_CSV} :}{ Query to retrieve a data.frame }
#'  \item{\code{query_NetCDF} :}{ }
#'  \item{\code{query_wfs_wms} :}{ }
#'  \item{\code{query_wfs_wms_aggregated_layer} :}{ }
#'  \item{\code{query_dynamic_list_keywords_species} :}{ }
#'  \item{\code{query_dynamic_list_keywords_fishing_gears} :}{ }
#'  \item{\code{query_dynamic_list_keywords_Fleets} :}{ }
#'  \item{\code{query_dynamic_metadata_count_features} :}{ }
#'  \item{\code{query_dynamic_metadata_spatial_Extent} :}{ }
#'  \item{\code{query_dynamic_metadata_temporal_Extent} :}{ }
#'  \item{\code{query_dynamic_metadata_get_SRID} :}{ }
#'  \item{\code{lineage} :}{ }
#'  \item{\code{query_dynamic_list_keywords_institutions} :}{ }
#'   }
#' 
#' @examples
#' 
#' queries<-getSQLSardaraQueries(db_connection_sardara_world(),"global_catch_5deg_1m_1950_01_01_2016_01_01_tunaatlasIRD_level1")
#' 
#' # retrieve data.frame of global_catch_5deg_1m_1950_01_01_2016_01_01_tunaatlasIRD_level1
#' 
#' global_catch_5deg_1m_1950_01_01_2016_01_01_tunaatlasIRD_level1<-dbGetQuery(con,queries$query_CSV)
#' 
#' @author Paul Taconet, \email{paul.taconet@@ird.fr}
#'




getSQLSardaraQueries <- function(con, dataset_name){
  
  dataset<-dbGetQuery(con, paste0("SELECT * FROM metadata.metadata where dataset_name='",dataset_name,"'"))
  
  #similar static definitions same as in write_tuna_atlas_metadata
  #to facilitate later editing of metadata fieldname changes, but to simplify the parameters of the function...too many parameters is crazy and unreadable
  static_metadata_table_name<- dataset$table_name # TO BE DONE => REMOVE IT / NOT USED FOR NOW
  static_metadata_id <- dataset$id_metadata
  static_metadata_permanent_id <- dataset$dataset_permanent_identifier
  static_metadata_date_of_what <- dataset$date
  static_metadata_URL_of_what <- dataset$url_report_process # TO BE DONE => REMOVE IT / NOT USED FOR NOW
  
  static_metadata_dataset_origin_institution <- dataset$dataset_origin_institution
  static_metadata_view_type <- dataset$table_type
  
  static_metadata_type_operation <- dataset$type_operation # TO BE DONE => REMOVE THE COLUMN / NOT USED FOR NOW
  static_metadata_url_download_page <- dataset$url_download_page # TO BE DONE => REMOVE THE COLUMN AND PUT ITS CONTENT IN A NEW ONE FOR ALL URLs / NOT USED FOR NOW
  static_metadata_table_description <- dataset$table_description
  static_metadata_table_purpose <- dataset$table_short_description
  static_metadata_url_original_file <- dataset$url_original_file # TO BE DONE => REMOVE THE COLUMN AND PUT ITS CONTENT IN A NEW ONE FOR ALL URLs / NOT USED FOR NOW
  static_metadata_dataset_description_report_url <- dataset$dataset_description_report_url # TO BE DONE => REMOVE THE COLUMN AND PUT ITS CONTENT IN A NEW ONE FOR ALL URLs / NOT USED FOR NOW
  static_metadata_dataset_description_report_original_url <- dataset$dataset_description_report_original_url # TO BE DONE => REMOVE THE COLUMN AND PUT ITS CONTENT IN A NEW ONE FOR ALL URLs / NOT USED FOR NOW
  static_metadata_dataset_name <- dataset$dataset_name
  static_metadata_dataset_release_date <- dataset$dataset_release_date  # TO BE DONE => REMOVE THE COLUMN / NOT USED FOR NOW
  static_metadata_dataset_available_dimensions <- dataset$dataset_available_dimensions
  static_metadata_table_sql_query <- dataset$table_sql_query
  static_metadata_table_dataset_title <- dataset$dataset_title
  static_metadata_table_view_name <- dataset$view_name
  
  #logger.info("Setting SQL queries according to dataset type (codelist / mapping / raw_dataset)")
  SQL <- list()
  
  SQL$dynamic_metadata_count_features=NULL
  SQL$SRID=NULL
  SQL$dynamic_metadata_spatial_Extent=NULL
  SQL$dynamic_metadata_temporal_Extent=NULL
  SQL$query_wfs_wms_aggregated_layer=NULL
  SQL$query_materialized_view_sardara=NULL

  #lineage
  SQL$lineage <- paste("SELECT md_mp.*, md.table_name, md.view_name, md.table_type, md.dataset_name, md.dataset_title FROM metadata.metadata_mapping md_mp, metadata.metadata md WHERE md_mp.metadata_mapping_id_from IN ('",static_metadata_id,"')  AND  metadata_mapping_id_from=md.id_metadata;;",sep="")

  #dynamic keyword list
  #institution
  SQL$query_dynamic_list_keywords_institutions <-paste("
  WITH id_metadata_genealogy AS (
  SELECT ",static_metadata_id," UNION SELECT metadata_mapping_id_to
  FROM metadata.metadata_mapping
  WHERE metadata_mapping_id_from=",static_metadata_id,"
  )
  SELECT DISTINCT dataset_origin_institution as keyword, 'ORIGIN_INSTITUTIONS' as thesaurus 
  FROM metadata.metadata  
  WHERE id_metadata IN 
  (SELECT * FROM id_metadata_genealogy)
  UNION 
  SELECT DISTINCT name_en_origin_institution as keyword, 'ORIGIN_INSTITUTIONS' as thesaurus 
  FROM metadata.metadata 
  JOIN metadata.origin_institution
  ON origin_institution.code_origin_institution=metadata.dataset_origin_institution
  WHERE id_metadata IN 
  (SELECT * FROM id_metadata_genealogy)
   order by keyword ;",sep="")

  ##logger.info(SQL$query_dynamic_list_keywords_institutions)
  dynamic_metadata_Keywords_institutions <- dbGetQuery(con, SQL$query_dynamic_list_keywords_institutions)

  #sql queries depending on dataset_type
  if (static_metadata_view_type=='codelist'){
  
    ##logger.info("Setting SQL queries specific to CODELIST")
    
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
      SQL$query_wfs_wms<-paste("SELECT ",code_label_column_name$code_column[1]," as code,",code_label_column_name$english_label_column[1]," as label, ",table_geometry_information$f_geometry_column," as the_geom ",other_columns_column_names,"  FROM ",static_metadata_table_name,sep="")
      SQL$query_CSV<-paste("SELECT ",code_label_column_name$code_column[1]," as code,",code_label_column_name$english_label_column[1]," as label, st_astext(",table_geometry_information$f_geometry_column,") as geom_wkt ",other_columns_column_names,"  FROM ",static_metadata_table_name,sep="")
      
      
    } else {
      
      # 5) s'il s'agit d'un code list non spatial (classique), on prend le code list 
      SQL$query_CSV <- paste("SELECT ",code_label_column_name$code_column[1]," as code,",code_label_column_name$english_label_column[1]," as label ",other_columns_column_names,"  FROM ",static_metadata_table_name,sep="")
      SQL$query_wfs_wms <- SQL$query_CSV
      # TO BE DONE => ADD SQL QUERIES FOR KEYWORDS ...
      
    }
  
  }else if (static_metadata_view_type=='mapping'){
  
    #logger.info("Setting SQL queries specific to MAPPING")
  
    dimension<-sub("\\..*", "", static_metadata_table_name)
  
    SQL$query_CSV<-paste0("SELECT sub1.codesource as src_code,
                        sub2.codetarget as trg_code,
                        sub1.db_tablesource as src_codingsystem,
                        sub2.db_tabletarget as trg_codingsystem
                        FROM
                        ( SELECT ",dimension,".id_",dimension," AS db_idsource,
                        ",dimension,".codesource_",dimension," AS codesource,
                        ",dimension,".tablesource_",dimension," AS db_tablesource
                        FROM ",dimension,".",dimension,"
                        JOIN metadata.metadata ON metadata.id_metadata = ",dimension,".id_metadata
                        WHERE ",dimension,".tablesource_",dimension," = (SELECT distinct src.tablesource_",dimension,"
                        FROM ",static_metadata_table_name,"
                        JOIN ",dimension,".",dimension," src ON src.id_",dimension,"=",dimension,"_mapping.",dimension,"_mapping_id_from
                        JOIN ",dimension,".",dimension," trg ON trg.id_",dimension,"=",dimension,"_mapping.",dimension,"_mapping_id_to
                        WHERE ",dimension,"_mapping.id_metadata=",static_metadata_id,")::text) sub1
                        LEFT JOIN ( SELECT ",dimension,"_mapping.",dimension,"_mapping_id_from,
                        ",dimension,"_mapping.",dimension,"_mapping_id_to AS db_idtarget,
                        ",dimension,".codesource_",dimension," AS codetarget,
                        ",dimension,".tablesource_",dimension," AS db_tabletarget
                        FROM ",static_metadata_table_name,"
                        JOIN ",dimension,".",dimension," ON ",dimension,".id_",dimension," = ",dimension,"_mapping.",dimension,"_mapping_id_to
                        JOIN metadata.metadata ON metadata.id_metadata = ",dimension,".id_metadata
                        WHERE ",dimension,".tablesource_",dimension," = (SELECT distinct trg.tablesource_",dimension,"
                        FROM ",static_metadata_table_name,"
                        JOIN ",dimension,".",dimension," src ON src.id_",dimension,"=",dimension,"_mapping.",dimension,"_mapping_id_from
                        JOIN ",dimension,".",dimension," trg ON trg.id_",dimension,"=",dimension,"_mapping.",dimension,"_mapping_id_to
                        WHERE ",dimension,"_mapping.id_metadata=",static_metadata_id,")::text) sub2 ON sub1.db_idsource = sub2.",dimension,"_mapping_id_from
                        ORDER BY sub2.db_tabletarget,sub1.codesource")
  
  
    SQL$query_wfs_wms <- SQL$query_CSV
  
  }else if (static_metadata_view_type=='raw_dataset'){
  
    ##logger.info("Setting SQL queries specific to RAW_DATASET")
    
    db_dimensions_parameters<-read.csv("inst/extdata/db_dimensions_parameters.csv",stringsAsFactors = F)
    
    # Get the dimensions available in the raw_dataset
  
    dataset_available_dimensions<-list_dataset_available_dimensions(con,static_metadata_dataset_name)
    
    columns_csv_wms_wfs<-db_dimensions_parameters$sql_column_label[which(db_dimensions_parameters$dimension %in% dataset_available_dimensions)]
    
    select_query_csv_wms_wfs<-paste(columns_csv_wms_wfs,collapse=", ",sep="") 

    # create WHERE clause for queries wms/wfs
    columns_wms_wfs_where_clause<-setdiff(columns_csv_wms_wfs, c("time_start","time_end"))
    where_query_wms_wfs<-NULL
    for (i in 1:length(columns_wms_wfs_where_clause)){
      where_query_wms_wfs<-paste(where_query_wms_wfs,paste0(columns_wms_wfs_where_clause[i]," IN regexp_split_to_table(regexp_replace('%",columns_wms_wfs_where_clause[i],"%',' ', '+', 'g'),E'\\\\+') )"),sep=" AND ")
    }
    # add time dimensions
    if (any(columns_csv_wms_wfs=="time_start")){
    where_query_wms_wfs<-paste0(where_query_wms_wfs," AND time_start>='%time_start%' AND time_end<='%time_end%' ")
    }
    # remove first "AND" at the beginning of the where clause
    where_query_wms_wfs<-substring(where_query_wms_wfs, 5)
    
    # add types of data to columns_csv_wms_wfs (for further use as parameter of the function publish_wms_wfs in the script write_data_access_ogc_wms_wfs to determine the regexp)
    columns_wms_wfs_where_clause<-data.frame(columns_wms_wfs_where_clause)
    colnames(columns_wms_wfs_where_clause)<-"attname"
    columns_wms_wfs_where_clause<-merge(column_names_and_types_dataset,columns_wms_wfs_where_clause)
    
    
    # write the queries
    if (grepl("nominal_catch",static_metadata_dataset_name)){
      #logger.info("This dataset is a raw_dataset STORING NOMINAL CATCH !  ######################")
      geo_attributes<-",area.label,st_astext(ST_Envelope(geom)) as the_geom"
      geo_attributes_NetCDF<-",area.label,st_astext(ST_Envelope(geom)) as the_geom"
    }else{
      #logger.info("This dataset is a raw_dataset ######################")   
      geo_attributes<-",st_astext(geom) as the_geom,st_x(ST_Centroid(geom)) as longitude,st_y(ST_Centroid(geom)) as latitude"
      geo_attributes_NetCDF<-",st_astext(geom) as the_geom"
    }
  
    #logger.info("Writing SQL Queries to get dynamic metadata elements from the values stored in the database")
    #logger.info("Writing SQL Queries for CSV, NetCDF, WMS, WFS")
    
    SQL$query_CSV<-paste("SELECT ",select_query_csv_wms_wfs,geo_attributes,",value FROM ",static_metadata_table_view_name," LEFT JOIN area.areas_with_geom area USING (id_area) order by ",select_query_csv_wms_wfs, sep="")
    
    SQL$query_NetCDF <- paste ("SELECT ",select_query_csv_wms_wfs,geo_attributes_NetCDF,",value FROM ",static_metadata_table_view_name," LEFT JOIN area.areas_with_geom area USING (id_area) where catchunit IN ('MT','MTNO');",sep=" ")

    SQL$query_wfs_wms <- paste("SELECT ",select_query_csv_wms_wfs,",tab_geom.geom as the_geom FROM ",static_metadata_table_view_name," LEFT OUTER JOIN area.areas_with_geom tab_geom USING (id_area) WHERE ",where_query_wms_wfs,sep="")
    SQL$query_wfs_wms_aggregated_layer <- paste("SELECT value,tab_geom.codesource_area as geographic_identifier,tab_geom.geom as the_geom from ( SELECT CASE '%aggregation_method%' WHEN 'sum' THEN sum(value) WHEN 'avg' THEN sum(value)/(select DATE_PART('year', '%time_end%'::date) - DATE_PART('year', '%time_start%'::date) ) END as value,id_area FROM ",static_metadata_table_view_name," WHERE ",where_query_wms_wfs,"  group by id_area) tab   LEFT OUTER JOIN area.areas_with_geom tab_geom USING (id_area) ",sep="")
    
    
    #logger.info("Writing SQL Queries for KEYWORDS")
    
    if (any(dataset_available_dimensions == "species")){
    SQL$query_dynamic_list_keywords_species <- paste("SELECT DISTINCT asfis_scientific_name AS keyword, 'ASFIS' AS thesaurus FROM ",static_metadata_table_name," JOIN species.species_labels USING (id_species) WHERE id_metadata= ",static_metadata_id,";",sep="")
    #logger.info(paste("SQL Query to get dynamic keywords for species: \n", SQL$query_dynamic_list_keywords_species," \n", sep=" "))
    #logger.info("Running SQL Queries to get dynamic metadata keywords for species  ######################")
    #SQL$dynamic_metadata_Keywords_species <- dbGetQuery(con, SQL$query_dynamic_list_keywords_species)
    } #else { SQL$dynamic_metadata_Keywords_species = NULL }
    
    if (any(dataset_available_dimensions == "gear")){
    SQL$query_dynamic_list_keywords_fishing_gears<-paste("SELECT DISTINCT isscfg_gear_categories AS keyword, 'ISSCFG' AS thesaurus FROM ",static_metadata_table_name," JOIN gear.gear_labels USING (id_gear) WHERE id_metadata= ",static_metadata_id,";",sep="")
    #logger.info(paste("SQL Query to get dynamic keywords for gear: \n", SQL$query_dynamic_list_keywords_fishing_gears," \n", sep=" "))
    #logger.info("Running SQL Queries to get dynamic metadata keywords for gear  ######################")
    #SQL$dynamic_metadata_Keywords_fishing_gears <- dbGetQuery(con, SQL$query_dynamic_list_keywords_fishing_gears)
    } #else { SQL$dynamic_metadata_Keywords_fishing_gears = NULL }
    
    if (any(dataset_available_dimensions == "flag")){
    SQL$query_dynamic_list_keywords_Fleets <-paste("SELECT DISTINCT cwp_name AS keyword, 'FAO_COUNTRY_OR_AREA_CODE' AS thesaurus FROM ",static_metadata_table_name," JOIN flag.flag_labels USING (id_flag) WHERE id_metadata= ",static_metadata_id,";",sep="")
    #logger.info(paste("SQL Query to get dynamic keywords for flag: \n", SQL$query_dynamic_list_keywords_Fleets," \n", sep=" "))
    #logger.info("Running SQL Queries to get dynamic metadata keywords for flag  ######################")
    #SQL$dynamic_metadata_Keywords_fishing_fleet <- dbGetQuery(con, SQL$query_dynamic_list_keywords_Fleets)
    } #else { SQL$dynamic_metadata_Keywords_fishing_fleet = NULL }
    
    if (any(dataset_available_dimensions == "schooltype")){
    SQL$query_dynamic_list_keywords_schooltype <-paste("SELECT DISTINCT schooltype_tunaatlas_label AS keyword, 'SCHOOLTYPE' AS thesaurus FROM ",static_metadata_table_name," JOIN schooltype.schooltype_labels USING (id_schooltype) WHERE id_metadata= ",static_metadata_id,";",sep="")
    #logger.info(paste("SQL Query to get dynamic keywords for schooltype: \n", SQL$query_dynamic_list_keywords_schooltype," \n", sep=" "))
    #logger.info("Running SQL Queries to get dynamic metadata keywords for schooltype  ######################")
    #SQL$dynamic_metadata_Keywords_schooltype <- dbGetQuery(con, SQL$query_dynamic_list_keywords_schooltype)
    } #else { SQL$dynamic_metadata_Keywords_schooltype = NULL }
    
    if (any(dataset_available_dimensions == "unit") & static_metadata_table_name=='fact_tables.effort'){
       SQL$query_dynamic_list_keywords_effortunit <-paste("SELECT DISTINCT effortunit_rfmo_english_label AS keyword, 'FISHING_EFFORT_UNIT' AS thesaurus FROM ",static_metadata_table_name," JOIN effortunit.effortunit_labels USING (id_effortunit) WHERE id_metadata= ",static_metadata_id,";",sep="")
      #logger.info(paste("SQL Query to get dynamic keywords for effortunit: \n", SQL$query_dynamic_list_keywords_effortunit," \n", sep=" "))
      #logger.info("Running SQL Queries to get dynamic metadata keywords for effortunit  ######################")
       #SQL$dynamic_metadata_Keywords_effortunit <- dbGetQuery(con, SQL$query_dynamic_list_keywords_effortunit)
     } #else { SQL$dynamic_metadata_Keywords_effortunit = NULL }
    
    #logger.info("Writing SQL Queries for SPATIAL AND TEMPORAL COVERAGES, SRID, FEATURES COUNT  ######################")
     SQL$query_dynamic_metadata_count_features <-  paste("SELECT count(*) FROM ",static_metadata_table_name," c WHERE c.id_metadata = ",static_metadata_id,";", sep="")
     SQL$query_dynamic_metadata_spatial_Extent <- paste("SELECT ST_XMin(ST_SetSRID(ST_Extent(geom),4326)) as xmin, ST_YMin(ST_SetSRID(ST_Extent(geom),4326)) AS ymin, ST_XMax(ST_SetSRID(ST_Extent(geom),4326)) AS xmax, ST_Ymax(ST_SetSRID(ST_Extent(geom),4326)) AS ymax FROM ",static_metadata_table_name," c LEFT JOIN area.areas_with_geom USING (id_area) WHERE c.id_metadata = ",static_metadata_id,";", sep="")
     SQL$query_dynamic_metadata_temporal_Extent <- paste("SELECT MIN(time.time_start) AS start_date, MAX(time.time_end) AS end_date FROM ",static_metadata_table_name," c LEFT JOIN time.time USING (id_time) WHERE c.id_metadata = ",static_metadata_id,";", sep="")
     SQL$query_dynamic_metadata_get_SRID <-  paste("SELECT ST_SRID(geom) AS SRID FROM ",static_metadata_table_name," c LEFT JOIN area.areas_with_geom USING (id_area)  WHERE c.id_metadata = ",static_metadata_id," LIMIT 1;", sep="")
    
    #logger.info("Running SQL Queries for SPATIAL AND TEMPORAL COVERAGES, SRID, FEATURES COUNT  ######################")
     #SQL$dynamic_metadata_count_features <- dbGetQuery(con, SQL$query_dynamic_metadata_count_features)
     #SQL$dynamic_metadata_spatial_Extent <- dbGetQuery(con, SQL$query_dynamic_metadata_spatial_Extent)
     #SQL$SRID <- dbGetQuery(con, SQL$query_dynamic_metadata_get_SRID )
     #SQL$dynamic_metadata_temporal_Extent <- dbGetQuery(con, SQL$query_dynamic_metadata_temporal_Extent)
    }
  
  return(SQL)
  
}