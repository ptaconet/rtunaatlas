#' @name load_datasets_in_db
#' @aliases load_raw_dataset_in_db load_df_input_in_db load_mapping_in_db
#' @title Load a dataset in Sardara database
#' @description These functions enable to load a dataset in Sardara database. The datasets can be of type "raw_dataset", "df_input" or "mapping"
#' @export load_raw_dataset_in_db load_codelist_in_db load_mapping_in_db
#'
#' @usage 
#' load_raw_dataset_in_db(con,df_to_load,df_metadata,df_codelists_input)
#' load_codelist_in_db(con,df_to_load,df_metadata)
#' load_mapping_in_db(con,df_to_load,df_metadata)
#' 
#'     
#' @param con a wrapper of rpostgresql connection (connection to a database) with WRITE permission
#' @param df_to_load data.frame to load. See section "Details" for the structure of the dataset
#' @param df_metadata data.frame of metadata of to the df_to_load. See section "Details" for the structure of the dataset
#' @param df_codelists_input data.frame of the code lists used in case of loading of a raw_dataset. See section "Details" for the structure of the dataset
#' 
#' 
#' @details 
#'
#' The input data.frames (parameters \code{df_to_load}) must comply with defined structures. 
#' 
#' For raw datasets: A REDIGER
#' 
#' For code lists: 
#' \itemize{
#' \item{1 column named 'code':}  containing the unique and not null codes for the code list
#' \item{1 column named 'label':}  associated label. 
#' \item{Any additional column is accepted}. 
#' \item{For spatial code lists, they must have a column named "geom_wkt" with the geometry in WKT format. If geometries are missing, the colum must be filled-in with value NA}
#' }
#' 
#' For code lists mappings: 
#' \itemize{
#' \item{1 column named 'src_code':} {code of the code list source (code to map)}
#' \item{1 column named 'trg_code':} {code of the code list target}
#' \item{1 column named 'src_codingsystem':} {name (dataset_name) of the coding system in Sardara from where the code source is extracted. Only 1 value authorized. The code list must be in the database.}
#' \item{1 column named 'trg_codingsystem':} {name (dataset_name) of the coding system in Sardara from where the code target is extracted. The code list must be in the database.}
#' }
#' 
#' In addition, The code lists used in the mapping must be available in the DB.
#' 
#' For \code{df_codelists_input}: a template is available under inst/extdata/template_df_code_list.csv
#' 
#' For \code{df_metadata}: a template of the metadata data.frame to use is available under inst/extdata/template_metadata.csv
#' 
#' @family load data to database
#' 
#' @examples
#' 
#' @import dplyr
#' @import data.table
#' 
#' @author Paul Taconet, \email{paul.taconet@@ird.fr}
#'



# script to upload data into the sardara database. Data must have been formatted to the right structure before use of this script. These are the expected structures in function of the variables (facts):
# Efforts <- c("flag","gear","time_start","time_end","area","schooltype","effortunit","v_effort")
# Catches <- c("flag","gear","time_start","time_end","area","schooltype","species","catchtype","catchunit","v_catch")
# Catch-at-size <- c("flag","gear","time_start","time_end","area","schooltype","species","catchtype","catchunit","sex","size_step","size_min","v_catch")
# In the datasets, unknown values must be set to either "ALL" or "UNK"


# This is the full load script. The user has to provide the metadata of the dataset that is imported + the name of the code lists tables (available in Sardara) for each dimension. The code lists must be available in Sardara. If one/several code lists are not available in Sardara, they HAVE to be imported previously in Sardara with the dedicated script. 

# This script uses 1 external dataset: db_dimensions_parameters.csv


load_raw_dataset_in_db<- function(
  con, # connection to the DB
  df_to_load, # data.frame of of the dataset to upload
  df_metadata, # data.frame of metadata. format as : CsvMetadata
  df_codelists_input # data.frame of the code lists to use for each dimensions
){
  
  db_dimensions_parameters<-read.csv(system.file("extdata", "db_dimensions_parameters.csv",package="rtunaatlas"),stringsAsFactors = F,strip.white=TRUE)
  
  variable_name<-gsub("fact_tables.","",df_metadata$database_table_name)
  
  dimensions<-list_variable_available_dimensions(con,variable_name)
  
  # Set df_inputs to use
  df_codelists_input<-df_codelists_input[which(df_codelists_input$dimension %in% dimensions),]
  
  # convert columns that are not character to character and ensure that the column "value" is a numeric
  cols<-setdiff(colnames(df_to_load),"value")
  for (i in 1:length(cols)){
    if (typeof(df_to_load[,cols[i]])!="character"){
      df_to_load[,cols[i]]<-as.character(df_to_load[,cols[i]])
    }
  }
  df_to_load$value<-as.numeric(df_to_load$value)
  
  #### First we deal with all the dimensions that are "real" code lists: area,catchtype,catchunit,effortunit,flag,gear,schooltype,species,sex,ocean
  # Dimensions time and sizeclass are not "real" code lists. They are dealt in a second step
  
  db_dimensions_parameters<-db_dimensions_parameters[which(db_dimensions_parameters$dimension %in% dimensions),]
  
  #Keep only dimensions that are code list (i.e. dimensions time and sizeclass will be dealt separately,as non-code list dimension)
  db_df_inputlike_dimensions_parameters<-db_dimensions_parameters[ which(db_dimensions_parameters$codelist_table==TRUE), ]
  
  area_df_inputtouse<-df_codelists_input[which(df_codelists_input$dimension=="area"),]$code_list_identifier
  # if the area code list to use is wkt, remove from the dimensions that are code lists. It will be treated later
  if (area_df_inputtouse == "area_wkt"){
    db_df_inputlike_dimensions_parameters<-db_df_inputlike_dimensions_parameters[-which(db_df_inputlike_dimensions_parameters$dimension=="area"),]
  }
  
  
  # Create columns of the dimensions that do not exist. These dimensions will be set to NULL in Sardara.
  for (i in 1:nrow(db_df_inputlike_dimensions_parameters)){
    if(!(db_df_inputlike_dimensions_parameters$csv_formatted_dimension_colname[i] %in% colnames(df_to_load))){
      df_to_load[db_df_inputlike_dimensions_parameters$csv_formatted_dimension_colname[i]]<-"ALL"
    }
    # We make sure that all the columns (except the value column) are characters
    #if (sapply(df_to_load[db_df_inputlike_dimensions_parameters$csv_formatted_dimension_colname[i]], class)[1] != "character"){
    #df_to_load[db_df_inputlike_dimensions_parameters$csv_formatted_dimension_colname[i]]<-as.character(df_to_load[db_df_inputlike_dimensions_parameters$csv_formatted_dimension_colname[i]])
    #}
  }
  
  
  
  
  # One by one, merge the dimensions
  
  #Initialize TablesToUpdateInDB and missingCodesInTableInDB. These vectors will be used to advise the user on the codes that are missing inside the DB
  TablesToUpdateInDB<-c()
  missingCodesInTableInDB<-c()
  
  for (i in 1:nrow(db_df_inputlike_dimensions_parameters)){
    
    if (length(unique(df_to_load[,db_df_inputlike_dimensions_parameters$csv_formatted_dimension_colname[i]]))==1 & df_to_load[1,db_df_inputlike_dimensions_parameters$csv_formatted_dimension_colname[i]]=="ALL"){
      df_to_load[,db_df_inputlike_dimensions_parameters$db_pkattribute_colname[i]]=0
      } else {
    #Retrieve the name of the code list to use
    index<-which(df_codelists_input$dimension==db_df_inputlike_dimensions_parameters$dimension[i])
    db_df_inputstouse<-df_codelists_input$code_list_identifier[index]
    
    # Merge the dimension 
    df_to_load<-FUNMergeDimensions_CodeListLike(
      con,
      db_df_inputlike_dimensions_parameters$db_tablename[i],
      db_df_inputlike_dimensions_parameters$db_pkattribute_colname[i],
      db_df_inputlike_dimensions_parameters$db_codesource_colname[i],
      db_df_inputlike_dimensions_parameters$db_tablesource_colname[i],
      df_to_load,
      db_df_inputlike_dimensions_parameters$csv_formatted_dimension_colname[i],
      db_df_inputstouse
    )
    
    # For the considered dimension, the code lists might have been updated. Checks if some codes are present in the dataset to upload but absent in the corresponding dimension table of the DB. 
    index.na<-which(is.na(df_to_load[,db_df_inputlike_dimensions_parameters$db_pkattribute_colname[i]]))
    
    
    if (length(index.na)>0){
      missingCodesInDB<-unique(df_to_load[index.na,db_df_inputlike_dimensions_parameters$csv_formatted_dimension_colname[i]])
    } else {
      missingCodesInDB<-NULL
    }
    
    
    if (!is.null(missingCodesInDB)){
      for (k in 1:length(missingCodesInDB)){
        TablesToUpdateInDB<-c(TablesToUpdateInDB,as.character(db_df_inputstouse))
      }
      missingCodesInTableInDB<-c(missingCodesInTableInDB,missingCodesInDB)
      
      #warning(paste("Some code(s) exist in the dataset to upload but do not exist in the corresponding code list in the  do not exist in the table ",db_df_inputstouse," of the database. You should update the table ",db_df_inputstouse," of Sardara with these code before uploading the dataset in Sardara.",sep=""))
    }
  }
    #remove from the dataset to upload the column that has just been merged
    varsToDelete <- names(df_to_load) %in% db_df_inputlike_dimensions_parameters$csv_formatted_dimension_colname[i]
    df_to_load <- df_to_load[!varsToDelete]
    
  }
  
  
  # These are the codes that are missing in the tables within the DB. The tables of the DB have to be filled with these values before the dataset is uploaded in the DB.
  missingCodes_dataframe<- data.frame(TablesToUpdateInDB,missingCodesInTableInDB)
  missingCodes_dataframe<-missingCodes_dataframe[! (missingCodes_dataframe$missingCodesInTableInDB %in% c("ALL","UNK")),]
  if (nrow(missingCodes_dataframe)>0){
  missingCodes_dataframe<-missingCodes_dataframe[!is.na(missingCodes_dataframe$missingCodesInTableInDB),]
  }
  
  if (nrow(missingCodes_dataframe)>0){
    print(missingCodes_dataframe)
    stop("Some code(s) exist in the dataset to upload but do not exist in the corresponding code list. You should update the tables of Sardara with these code(s) (and the mapping if relevant) before uploading the dataset in Sardara.")
  } else {
    
    #replace NA by 'NA'. These values will be set to NULL in the DB. These values are the ones that were set to "ALL" in the dataset to upload
    #df_to_load<-replace(df_to_load, is.na(df_to_load), "NA") 
    
    ## Now deal with non-code list like dimensions (time,sizeclass). For these dimensions, the column names on the dataset to upload MUST BE the sames as the DBs ones. i.e. for time, the dataset to upload must have a "time_start" and a "time_end" column , and the DB table "time" must also have the same "time_start" and "time_end" columns
    
    #Keep only the dimensions that are non-code list
    db_nondf_inputlike_dimensions_parameters<-db_dimensions_parameters[ which(db_dimensions_parameters$codelist_table==FALSE), ]
    
    # One by one, retrieve the numeric codes
    for (dim in 1:nrow(db_nondf_inputlike_dimensions_parameters)){
      
      if (db_nondf_inputlike_dimensions_parameters$dimension[dim]=="sizeclass"){
        df_to_load$size_min<-as.numeric(df_to_load$size_min)
        df_to_load$size_step<-as.numeric(df_to_load$size_step)
      }
      
      # Merge to get back the ID from the DB
      df_to_load <- FUNMergeDimensions_NonCodeListLike(
        con,
        df_to_load,
        db_nondf_inputlike_dimensions_parameters$db_pkattribute_colname[dim],
        db_nondf_inputlike_dimensions_parameters$csv_formatted_dimension_colname[dim],
        db_nondf_inputlike_dimensions_parameters$db_tablename[dim]
      )
      
      #check if some codes are missing after the matching. If so, upload the new records in Sardara
      index.na<-which(is.na(df_to_load[, db_nondf_inputlike_dimensions_parameters$db_pkattribute_colname[dim]]))
      
      if (length(index.na)>0){
        rs<-FUNuploadNewRecordsToDB(con,
                                    df_to_load,
                                    db_nondf_inputlike_dimensions_parameters$db_tablename[dim],
                                    db_nondf_inputlike_dimensions_parameters$db_pkattribute_colname[dim],
                                    db_nondf_inputlike_dimensions_parameters$csv_formatted_dimension_colname[dim])
        
        #if codes were missing in db, re-run the function that does the merging
        
        df_to_load[,db_nondf_inputlike_dimensions_parameters$db_pkattribute_colname[dim]]<-NULL
        
        df_to_load <- FUNMergeDimensions_NonCodeListLike(
          con,
          df_to_load,
          db_nondf_inputlike_dimensions_parameters$db_pkattribute_colname[dim],
          db_nondf_inputlike_dimensions_parameters$csv_formatted_dimension_colname[dim],
          db_nondf_inputlike_dimensions_parameters$db_tablename[dim]
        )
      }
      
      #remove from the dataset to upload the column that has just been merged
      #colnames_to_merge_vectorformat<-strsplit(db_nondf_inputlike_dimensions_parameters$csv_formatted_dimension_colname[dim],",")[[1]]
      
      #varsToDelete <- names(df_to_load) %in% colnames_to_merge_vectorformat
      #df_to_load <- df_to_load[!varsToDelete]
      
    }
    
    
    ### Deal with area in wkt format
    if (area_df_inputtouse=="area_wkt"){
      
      df_to_load<-FUNMergeDimensions_CodeListLike(
        con,
        "area.area",
        "id_area",
        "codesource_area",
        "tablesource_area",
        df_to_load,
        "geographic_identifier",
        "area_wkt"
      )
      
      #check if some codes are missing after the matching
      index.na<-which(is.na(df_to_load[, "id_area"]))
      
      if (length(index.na)>0){
        
        sql<-paste("SELECT code,code as code_wkt_merge from area.area_wkt")
        df_inputFromDB<-dbGetQuery(con, sql)
        
        df_to_load<-data.table(df_to_load)
        df_to_load<-merge(df_to_load,df_inputFromDB,by.x="geographic_identifier",by.y="code",all.x=TRUE)
        df_to_load<-as.data.frame(df_to_load)
        
        # upload the new wkt to the db
        CodesToLoad<-unique(df_to_load[index.na,"geographic_identifier"])
        
        sql4 <- paste0("COPY  area.area_wkt (code) FROM STDIN NULL 'NA' ")
        postgresqlpqExec(con, sql4)
        postgresqlCopyInDataframe(con, data.frame(CodesToLoad))
        rs <- postgresqlgetResult(con)
        
        #if codes were missing in db, re-run the function that does the merging
        
        df_to_load[,"id_area"]<-NULL
        
        df_to_load<-FUNMergeDimensions_CodeListLike(
          con,
          "area.area",
          "id_area",
          "codesource_area",
          "tablesource_area",
          df_to_load,
          "geographic_identifier",
          "area_wkt"
        )
      }
      
      ## Refresh materialized view area.area_labels to take into account the new WKT codes just inserted
      dbSendQuery(con,"REFRESH MATERIALIZED VIEW area.area_labels")
      
    }
    
    cat("Data merged with code lists of database\n")
    
    
    # Load metadata
    rs<-FUNUploadDatasetToTableInDB(con,df_metadata,"metadata.metadata")
    cat("Metadata loaded\n")
    
    # Retrieve the PK of the metadata for the line just inserted
    sql<- "SELECT max(id_metadata) FROM metadata.metadata"
    PK_metadata <- dbGetQuery(con, sql)
    PK_metadata<-as.integer(PK_metadata$max[1])
    
    df_to_load$id_metadata<-PK_metadata
    
    # For the dataset to upload, keep only rows that are in the database table (i.e. remove all other columns)  (is it really necessary? TO CHECK)
    # This first line is to change the metric column name
    # colnames(df_to_load)[which(colnames(df_to_load) == metric_colname_dftoupload)] <- metric_colname_db
    
    query_get_colnames<-paste("select column_name from information_schema.columns where table_name='",variable_name,"' and table_schema='fact_tables' and column_default is null",sep="")
    column_names<-dbGetQuery(con, query_get_colnames)
    column_names_to_keep<-column_names$column_name
    
    df_to_load<-df_to_load[column_names_to_keep]
    
    # Replace NA by O. In the db, 0 = unknown
    df_to_load<-replace(df_to_load, is.na(df_to_load), 0)
    
    cat("Loading the dataset in the database...\n")
    
    # Upload file to database
    
    rs<-FUNUploadDatasetToTableInDB(con,df_to_load,df_metadata$database_table_name)
    
    cat("Data loaded in the DB\n")
    
    
    ## Update some metadata elements
     # first drop materialized view
    if(!is.null(df_metadata$database_view_name)){
    dbSendQuery(con,paste0("DROP MATERIALIZED VIEW IF EXISTS ",df_metadata$database_view_name,";"))
    }
    
    # sql_query_dataset_extraction
    df_metadata$id_metadata<-PK_metadata
    sql_query_dataset_extraction<-getSQLSardaraQueries(con,df_metadata)
    dbSendQuery(con,paste0("UPDATE metadata.metadata SET sql_query_dataset_extraction='",gsub("'","''",sql_query_dataset_extraction$query_CSV_with_labels),"' WHERE identifier='",df_metadata$identifier,"'"))
    
    # spatial coverage
    sp_extent_sql<-paste0("SELECT st_astext(ST_Extent(geom)) FROM ",df_metadata$database_table_name," c LEFT JOIN area.area_labels USING (id_area) WHERE id_metadata='",PK_metadata,"'")
    sp_extent<-dbGetQuery(con,sp_extent_sql)$st_astext
    dbSendQuery(con,paste0("UPDATE metadata.metadata SET spatial_coverage='",sp_extent,"' WHERE identifier='",df_metadata$identifier,"'"))
    
    
    # metadata_mapping
    # 1) dataset-codelists (le dataset x utilise les codelist x,y,z) 
    for (i in 1:nrow(df_codelists_input)){
      id_metadata_code_list<-dbGetQuery(con,paste0("SELECT id_metadata from metadata.metadata where identifier='",df_codelists_input$code_list_identifier[i],"'"))
      if(nrow(id_metadata_code_list)){
      dbSendQuery(con,paste0("INSERT INTO metadata.metadata_mapping(metadata_mapping_id_from,metadata_mapping_id_to) VALUES (",PK_metadata,",",id_metadata_code_list,")"))
    }
  }
    # 2) dataset-mappings (le dataset x utilise les mappings x,y,z) -> PAS ENCORE GERE
    # 3) dataset-dataset  (le dataset x utilise les dataset x,y,z) -> PAS ENCORE GERE
    
    
    # Create the materialized view if set in the metadata
    if(!is.na(df_metadata$database_view_name)){
      cat(paste0("Creating materialized view ",df_metadata$database_view_name," (with codes and labels)\n"))
      # Check if schema exists
      list_of_schemas<-dbGetQuery(con,"select schema_name from information_schema.schemata")$schema_name
      # Get schema name where to store the materialized view
      schema_name<-sub('\\..*', '', df_metadata$database_view_name)
      # Create the schema if it does not exist
      if (!(schema_name %in% list_of_schemas)){
        dbSendQuery(con,paste0("CREATE SCHEMA ",schema_name,"; GRANT USAGE ON SCHEMA ",schema_name," TO tunaatlas_inv;ALTER DEFAULT PRIVILEGES IN SCHEMA ",schema_name," GRANT SELECT ON TABLES TO tunaatlas_inv;"))
      }
      # Create the materialized view without the labels (to get the labels, replace sql_query_dataset_extraction$query_CSV by sql_query_dataset_extraction$query_CSV_with_labels)
      dbSendQuery(con,paste0("DROP MATERIALIZED VIEW IF EXISTS ",df_metadata$database_view_name,";
                             CREATE MATERIALIZED VIEW ",df_metadata$database_view_name," AS ",sql_query_dataset_extraction$query_CSV_with_labels,";
                             COMMENT ON MATERIALIZED VIEW ",df_metadata$database_view_name," IS '",df_metadata$title,"'"))
    }
    
    }
  
}





## Code to add a new code list in the Sardara DB
load_codelist_in_db<-function(con,df_to_load,df_metadata){
  
  ## change all columns to "text" format. in the db, the columns will all be set to "text"
  df_to_load<-df_to_load %>% mutate_all(as.character)
  df_metadata<-df_metadata %>% mutate_all(as.character)
  
  # Input code list must be in CSV format, with the first line giving the column names. separators should be commas
  
  codelist_dataset_name<-df_metadata$identifier
  dimension_name<-sub('\\..*', '', df_metadata$database_table_name)
  
  table_name<-df_metadata$database_table_name
  
  colnames(df_to_load)<-tolower(colnames(df_to_load))
  
  # if there are points in the columns of the input code list, replace them with underscores
  colnames(df_to_load)<-gsub('.', '_', colnames(df_to_load),fixed=TRUE)
  
  
  ### Check if information given by the operator is OK
  
  # Check if the column 'code' exists, and is unique not null
  if ( !(any(names(df_to_load)=="code")) ) {
    stop("There is no column 'code' in the code list. Please set at least a column 'code', and possibly a column 'label'")
  }
  if (length(df_to_load$code)!=nrow(df_to_load) | length(which(is.na(df_to_load$code)))>0  | length(which(is.null(df_to_load$code)))>0 ){
    stop("The codes contain NULL values or is are not unique. Codes must be unique and cannot contain NULL values")
  }
  
  # Check if there is no blank in codelist_dataset_name and df_inputPKattributeName
  countSpaces <- function(s) { sapply(gregexpr(" ", s), function(p) { sum(p>=0) } ) }
  if (countSpaces(codelist_dataset_name)>0 | countSpaces("code")>0){
    stop("The name of the code list and the codes in the column \"code\" cannot have blanks. Please check those parameters.")
  } 
  
  
  # Check if the name of the table does not already exist in database
  sql<-paste("SELECT '",dimension_name,"'||'.'||table_name FROM information_schema.tables WHERE table_schema='",dimension_name,"' UNION SELECT oid::regclass::text FROM pg_class WHERE relkind = 'm';",sep="")
  DB_Dimension_TableNames<-dbGetQuery(con, sql)
  colnames(DB_Dimension_TableNames)<-"table_name"
  if (paste0(dimension_name,".",codelist_dataset_name) %in% DB_Dimension_TableNames$table_name){
    stop("The name of the code list already exists in the database. Please set another name.")
  }
  
  # Check if there is the right number of df_inputColumnsDataTypes
  #if (length(df_inputColumnsDataTypes)!=ncol(df_to_load)){
  # stop("The number of data types you provided is different from the number of columns of the code list. Please set another number of data types (df_inputColumnsDataTypes) re-run the script")
  #}
  
  # Check if one of the columns of the df_to_load has not the name of a SQL Key word. If yes, add a "_" after the column name
  sql<-"select * from pg_get_keywords() where catdesc='reserved'"
  ReservedWords<-dbGetQuery(con, sql)
  ColNamesAsReservedWords<-tolower(colnames(df_to_load)) %in% ReservedWords$word
  index.ColNamesAsReservedWords <- which(ColNamesAsReservedWords=="TRUE")
  if(length(index.ColNamesAsReservedWords)){
    colnames(df_to_load)[index.ColNamesAsReservedWords]<-paste(colnames(df_input)[index.ColNamesAsReservedWords],"_",sep="")
  }
  
  
  
  ### Add metadata in metadata tables
  
  # Load metadata
  rs<-FUNUploadDatasetToTableInDB(con,df_metadata,"metadata.metadata")
  cat("Metadata loaded\n")
  
  # Retrieve the PK of the metadata for the line just inserted
  sql<- "SELECT max(id_metadata) FROM metadata.metadata"
  PK_metadata <- dbGetQuery(con, sql)
  PK_metadata<-as.integer(PK_metadata$max[1])
  
  
  ### Add code list table in the DB, with constraints (data types and primary key) and triggers
  #First create table ...
  sql<- paste("CREATE TABLE ",table_name,"()",sep="")
  dbSendQuery(con, sql)
  
  
  for (i in 1:ncol(df_to_load)){
    # columns are all set to "text" type.
    #sql<- paste("ALTER TABLE ",dimension_name,".",codelist_dataset_name," ADD COLUMN ",tolower(colnames(df_input)[i])," ",df_inputColumnsDataTypes[i],sep="")
    sql<- paste("ALTER TABLE ",table_name," ADD COLUMN ",tolower(colnames(df_to_load)[i])," text",sep="")
    dbSendQuery(con, sql)
  }
  
  sql<- paste("ALTER TABLE ",table_name," ADD CONSTRAINT ",codelist_dataset_name,"_pkey PRIMARY KEY (code)",sep="")
  dbSendQuery(con, sql)
  
  cat("code list created in db")
  
  ## Add codes and labels in the table metadata.df_inputs_codes_labels_column_names
  #df_inputPKattributeName="code"
  #if ( !(any(names(df_to_load)=="label")) ) {
  #  df_inputLabelattributeName="code" } else {df_inputLabelattributeName="label"}
  
  #df_inputLabelattributeName="label"
  #sql<-paste0("INSERT INTO metadata.codelists_codes_labels_column_names(database_table_name,code_column,label_column) VALUES ('",table_name,"','",df_inputPKattributeName,"','",df_inputLabelattributeName,"')")
  #dbSendQuery(con, sql)
  
  # Create triggers to automatically fill and update the link dimension table
  
  sql_trigg_fill_link_dimension_table<-paste("CREATE OR REPLACE FUNCTION ",dimension_name,".func_add_new_record_in_link_table_",codelist_dataset_name,"() RETURNS trigger AS $BODY$ BEGIN INSERT INTO ",dimension_name,".",dimension_name," ( codesource_",dimension_name,",tablesource_",dimension_name,") VALUES (NEW.code,'",codelist_dataset_name,"') ; RETURN NEW; END; $BODY$ LANGUAGE 'plpgsql' VOLATILE;",sep="")
  dbSendQuery(con, sql_trigg_fill_link_dimension_table)
  
  sql_trigg_fill_link_dimension_table<-paste("CREATE TRIGGER trig_add_new_record_in_link_table_",codelist_dataset_name," BEFORE INSERT ON ",table_name," FOR EACH ROW EXECUTE PROCEDURE ",dimension_name,".func_add_new_record_in_link_table_",codelist_dataset_name,"();",sep="")
  dbSendQuery(con, sql_trigg_fill_link_dimension_table)
  
  
  # ... Then fill table
  #dbWriteTable(con, c(dimension_name, codelist_dataset_name), value = df_input,row.names=FALSE,append=TRUE)
  rs<-FUNUploadDatasetToTableInDB(con,df_to_load,table_name)
  
  # Finally fill the dimension link table with the metadata
  sql<-paste("UPDATE ",dimension_name,".",dimension_name," SET id_metadata=",PK_metadata," WHERE tablesource_",dimension_name,"='",codelist_dataset_name,"'",sep="")
  dbSendQuery(con, sql)
  
  dbSendQuery(con,paste0("COMMENT ON TABLE ",table_name," IS '",df_metadata$title,"'"))
  
  test_if_code_list_is_inserted<-dbGetQuery(con,paste0("SELECT * FROM ",table_name, " LIMIT 10"))
  if(nrow(test_if_code_list_is_inserted)>0){
    cat(paste0("\nThe code list was successfully loaded. It is in the table ",table_name," of the database\n"))
  }
  
  
  # Add a column geometry if geospatial code list. For now: must be a polygon or multipolygon with SRID=4326
  if (dimension_name=="area"){
    cat(paste0("\nAdding geometry..."))
    # Add the column
    sql<-paste("ALTER TABLE ",table_name," ADD COLUMN geom GEOMETRY(MultiPolygon,4326);",sep="")
    dbSendQuery(con, sql)
    # Calculate the column    
    sql<-paste("UPDATE ",table_name," SET geom=ST_Multi(ST_GeomFromText(geom_wkt,4326));",sep="")
    dbSendQuery(con, sql)
    # Remove the column geom_wkt
    sql<-paste("ALTER TABLE ",table_name," DROP COLUMN geom_wkt;",sep="")
    dbSendQuery(con, sql)
  }
  
  ### Updates the view that gives the labels, with the new code list just inserted 
  cat("Updating materialized view of labels...\n")
  
  table_name_without_schema<-gsub(".*\\.","",table_name)
  
  name_view_labels<-paste0(dimension_name,"_labels")
  
  colname_view_id<-paste0("id_",dimension_name)
  colname_view_codesource<-paste0("codesource_",dimension_name)
  colname_view_tablesource<-paste0("tablesource_",dimension_name)
  colname_view_label<-"source_label"
  
  query_create_view_label<-dbGetQuery(con,paste0("select pg_get_viewdef('",dimension_name,".",name_view_labels,"', true)"))
  
  ### Get name of the columns of the view
  columns_names_types_view_label<-dbGetQuery(con,paste0("SELECT
                                                        a.attname as column,
                                                        pg_catalog.format_type(a.atttypid, a.atttypmod) as datatype
                                                        FROM
                                                        pg_catalog.pg_attribute a
                                                        WHERE
                                                        a.attnum > 0
                                                        AND NOT a.attisdropped
                                                        AND a.attrelid = (
                                                        SELECT c.oid
                                                        FROM pg_catalog.pg_class c
                                                        LEFT JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace
                                                        WHERE c.relname = '",name_view_labels,"' 
                                                        )"))
  
  # these are the columns that will not be updated (they will be set with NULL values. User will have then to fill the right columns... to improve in next version)
  columns_names_types_view_label<-columns_names_types_view_label %>% filter (!(column %in% c(colname_view_id,colname_view_codesource,colname_view_tablesource,colname_view_label)))
  
  if ("geom" %in% columns_names_types_view_label$column){
    columns_names_types_view_label = columns_names_types_view_label[columns_names_types_view_label$column != "geom",]
  }
  
  query_null_columns<-NULL
  for (i in 1:nrow(columns_names_types_view_label)){
    query_null_columns_this_column<-paste0("NULL::",columns_names_types_view_label$datatype[i]," as ",columns_names_types_view_label$column[i])
    query_null_columns<-paste(query_null_columns,query_null_columns_this_column,sep=",")
  }
  if (dimension_name=="area"){
    query_null_columns<-paste0(query_null_columns,",geom")
  }
  
  sql_query_for_view_label_new_codelist<-paste0(" UNION SELECT ",colname_view_id,",",colname_view_codesource,",",colname_view_tablesource,",label as source_label")
  sql_query_for_view_label_new_codelist<-paste0(sql_query_for_view_label_new_codelist,query_null_columns," FROM ",table_name," tab JOIN ",dimension_name,".",dimension_name," ON ",dimension_name,".codesource_",dimension_name,"=tab.code::text WHERE ",dimension_name,".tablesource_",dimension_name,"='",table_name_without_schema,"'::text ORDER BY 3,2;")
  
  query_create_view_label<-gsub("ORDER BY 3, 2","",query_create_view_label)
  query_create_view_label<-gsub(";","",query_create_view_label)
  query_create_view_label<-paste("CREATE OR REPLACE VIEW ",dimension_name,".",name_view_labels," AS ",query_create_view_label,sql_query_for_view_label_new_codelist,sep="")
  
  # View of labels for geometry is a bit special since there is the geom. CODE BELOW TO IMPROVE!!!!
  if (dimension_name=="area"){
    pattern="CREATE OR REPLACE VIEW area.area_labels AS  WITH vue AS (.*?) SELECT vue.id_area"
    query_create_view_label<-regmatches(query_create_view_label,regexec(pattern,query_create_view_label))[[1]][1]
    query_create_view_label<-gsub(")\n SELECT vue.id_area","",query_create_view_label)
    query_create_view_label<-paste0(query_create_view_label,sql_query_for_view_label_new_codelist," )
                                    SELECT vue.id_area,
                                    vue.codesource_area,
                                    vue.tablesource_area,
                                    vue.source_label,
                                    vue.source_french_label,
                                    vue.source_spanish_label,
                                    st_setsrid(vue.geom, 4326) AS geom
                                    FROM vue")
    query_create_view_label<-gsub(";","",query_create_view_label)
    query_create_view_label<-gsub("CREATE OR REPLACE VIEW","DROP MATERIALIZED VIEW area.area_labels; CREATE MATERIALIZED VIEW",query_create_view_label)
    }
  
  #finally send the query to recreate the view for the labels with the new code list inserted
  dbSendQuery(con,query_create_view_label)
  cat("Materialized view of labels updated\n")
  
  
  ## fill-in metadata 'sql_query_dataset_extraction'
  df_metadata$id_metadata<-PK_metadata
  sql_query_dataset_extraction<-getSQLSardaraQueries(con,df_metadata)
  dbSendQuery(con,paste0("UPDATE metadata.metadata SET sql_query_dataset_extraction='",sql_query_dataset_extraction$query_CSV,"' WHERE identifier='",df_metadata$identifier,"'"))
  }



# Load a mapping table

load_mapping_in_db<-function(con,df_to_load,df_metadata){
  
  # Check errors: TO DO  (are tables existing? etc.)
  
  # get table_name corresponding to dataset_name of src_codingsystem and trg_codingsystem
  src_codingsystem_table_name<-dbGetQuery(con,paste0("SELECT id_metadata,database_table_name FROM metadata.metadata where identifier='",unique(df_to_load$src_codingsystem),"'"))
  trg_codingsystem_table_name<-dbGetQuery(con,paste0("SELECT id_metadata,database_table_name FROM metadata.metadata where identifier='",unique(df_to_load$trg_codingsystem),"'"))
  
  DBDimensionName=gsub("\\..*","",src_codingsystem_table_name$database_table_name)
  
  src_codingsystem_table_name$database_table_name<-gsub(".*\\.","",src_codingsystem_table_name$database_table_name)
  trg_codingsystem_table_name$database_table_name<-gsub(".*\\.","",trg_codingsystem_table_name$database_table_name)
  
  # Get the PK of the two tables (DBToTableName and DBFromTableName)
  sql<- paste("SELECT id_",DBDimensionName,",codesource_",DBDimensionName," FROM ",DBDimensionName,".",DBDimensionName," WHERE tablesource_",DBDimensionName,"='",src_codingsystem_table_name$database_table_name,"'",sep="")   
  FromTable<-dbGetQuery(con, sql)
  
  sql<- paste("SELECT id_",DBDimensionName,",codesource_",DBDimensionName," FROM ",DBDimensionName,".",DBDimensionName," WHERE tablesource_",DBDimensionName,"='",trg_codingsystem_table_name$database_table_name,"'",sep="")   
  ToTable<-dbGetQuery(con, sql)
  
  # Make mapping
  
  MapFromTableWithMappingTable<-merge(FromTable,df_to_load,by.y="src_code",by.x=paste("codesource_",DBDimensionName,sep=""),all.y=T,all.x=F)
  
  MapFinal<-merge(MapFromTableWithMappingTable,ToTable,by.y=paste("codesource_",DBDimensionName,sep=""),by.x="trg_code",all.x=T,all.y=F)
  
  
  MapFinal <- MapFinal[c(paste0("id_",DBDimensionName,".x"),paste0("id_",DBDimensionName,".y"))]
  MapFinal$mapping_relation_type<-"NA"
  colnames(MapFinal)<-c(paste0(DBDimensionName,"_mapping_id_from"),paste0(DBDimensionName,"_mapping_id_to"),paste0(DBDimensionName,"_mapping_relation_type"))
  
  
  # Load metadata
  rs<-FUNUploadDatasetToTableInDB(con,df_metadata,"metadata.metadata")
  cat("Metadata loaded\n")
  
  # Retrieve the PK of the metadata for the line just inserted
  sql<- "SELECT max(id_metadata) FROM metadata.metadata"
  PK_metadata <- dbGetQuery(con, sql)
  PK_metadata<-as.integer(PK_metadata$max[1])
  
  MapFinal$id_metadata<-PK_metadata
  
  # Insert mapping into mapping table
  
  dbWriteTable(con, c(DBDimensionName, paste(DBDimensionName,"_mapping",sep="")), value = MapFinal,row.names=FALSE,append=TRUE)
  
  
  ## Update some metadata elements
  
  # metadata_mapping
  
  sql<-paste0("INSERT INTO metadata.metadata_mapping(metadata_mapping_id_from,metadata_mapping_id_to) VALUES 
       (",PK_metadata,",",src_codingsystem_table_name$id_metadata,"),
       (",PK_metadata,",",trg_codingsystem_table_name$id_metadata,")")
  dbSendQuery(con,sql)
  
  
  # sql_query_dataset_extraction
  df_metadata$id_metadata<-PK_metadata
  sql_query_dataset_extraction<-getSQLSardaraQueries(con,df_metadata)
  dbSendQuery(con,paste0("UPDATE metadata.metadata SET sql_query_dataset_extraction='",sql_query_dataset_extraction$query_CSV,"' WHERE identifier='",df_metadata$identifier,"'"))
  
  
}




