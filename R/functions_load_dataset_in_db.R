#' @name functions_load_dataset_in_db
#' @aliases functions_load_dataset_in_db
#' @title Functions used to load a dataset in a database with Sardara model
#' @description A set of functions used to load a dataset in the database
#' @export FUNMergeDimensions_CodeListLike FUNMergeDimensions_NonCodeListLike FUNuploadNewRecordsToDB FUNUploadDatasetToTableInDB
#'
#' @family load data
#' 
#' @author Paul Taconet, \email{paul.taconet@@ird.fr}
#'



# This function merges the codes of a dimension of a dataset to load with the ones of the code list in the database
# It works for the dimensions that are "real" code lists: area,catchtype,catchunit,effortunit,flag,gear,schooltype,species,sex 
FUNMergeDimensions_CodeListLike<-function(DBconnection,  # connection to DB
                            DBTableName,  # Name of the dimension table in DB, i.e. "flag.flag"
                            DB_PK_AttributeName,   # Name of the primary key attribute in the dimension table in DB, i.e. "id_flag"
                            DB_CodeRFMO_AttributeName, # Name of attribute to merge in the dimension table in DB, i.e. "code_flag"
                            DB_TableRFMO_AttributeName, # Name of the tables attribute (attribute that contains the name of the tables for which there can be data) in the dimension table in DB, i.e. "tablesource_flag"
                            inputDataset,  # Input harmonized dataset
                            HarmonizedDF_CodeRFMO_AttributeName, # Name of attribute to merge in the harmonized dataset, i.e. "Flag"
                            DB_Tables_ToLookInto # List of tables that can contain the codes, i.e. "flag_iotc"
){
  
  # Retrieve the dimension code list from the DB

    sql<- paste("SELECT ",DB_PK_AttributeName,",",DB_CodeRFMO_AttributeName," FROM ",DBTableName," WHERE ",DB_TableRFMO_AttributeName," IN (",paste("'",gsub(",","','", DB_Tables_ToLookInto),"'",sep=""),")",sep="")
    CodeListFromDB <- dbGetQuery(DBconnection, sql)
  
  # merge code list from db with inputDataset
  
inputDataset<-data.table(inputDataset)
inputDatasetMergedWithDBCodeList<-merge(inputDataset,CodeListFromDB,by.x=HarmonizedDF_CodeRFMO_AttributeName,by.y=DB_CodeRFMO_AttributeName,all.x=TRUE)
inputDatasetMergedWithDBCodeList<-as.data.frame(inputDatasetMergedWithDBCodeList)
  
  return(inputDatasetMergedWithDBCodeList)
}


# This function merges the codes of a dimension of a dataset to load with the ones of the code list in the database (integers)
# It works for the dimensions time and sizeclass

FUNMergeDimensions_NonCodeListLike<-function(DBconnection,   # connection to DB
                                             inputDataset,   # Input harmonized dataset
                                             DB_PK_AttributeName,     # Name of the primary key attribute in the dimension table in DB, i.e. "id_time"
                                             dimension_colnames_to_retrieve,   # Name of the columns that will be used to match inputDataset and DBTableName
                                             DBTableName   # Name of the dimension table in DB, i.e. "time.time"
){
  
  if(DBTableName=="time.time"){
    sql<-"select id_time,to_char(time_start,'YYYY-MM-DD HH:MI:SS') as time_start,to_char(time_end,'YYYY-MM-DD HH:MI:SS') as time_end from time.time"
    
    # time in the DB are in format "YYYY-MM-DD HH:MM:SS" (timestamp without time zone). The data that we import must be with the same time format. When we do to_char function on a time without time stamp in postgresql which has HH:MI:SS = 00:00:00 , the output is 12:00:00 . Hence we fill the InputDataset with these value
    SetTimeStampFormat<-function(df,timeColumnName){
      index.time.withoutHours<-which(nchar(df[,timeColumnName])==10)
      if (length(index.time.withoutHours)>0){
        df[index.time.withoutHours,timeColumnName]<-paste(df[,timeColumnName]," 12:00:00",sep="")
      }
      return(df)
    }
    inputDataset<-SetTimeStampFormat(inputDataset,"time_start")
    inputDataset<-SetTimeStampFormat(inputDataset,"time_end")
  } else {
    sql<-paste("SELECT ",DB_PK_AttributeName,",",dimension_colnames_to_retrieve," from ",DBTableName,sep="")
  }
  
  CodeListFromDB<-dbGetQuery(DBconnection, sql)
  
  colnames_to_merge_vectorformat<-strsplit(dimension_colnames_to_retrieve,",")[[1]]
  
  # Merge inputDataset codes with Sardara codes
  inputDataset<-data.table(inputDataset)
  inputDatasetMergedWithDBCodeList<-left_join(inputDataset,CodeListFromDB,by=colnames_to_merge_vectorformat,all.x=TRUE)
  inputDatasetMergedWithDBCodeList<-as.data.frame(inputDatasetMergedWithDBCodeList)
  
  return(inputDatasetMergedWithDBCodeList)
}




# For a given dimension, some values might exist in the dataset to upload but not in the corresponding table of the database. The function FUNuploadNewRecordsToDB uploads these records in the proper tables of the DB.
# Example: 
# The dataset to upload has a record for time that is:
# time_start = 2020-01-01    time_end=2022-01-01
# This record does not exist in the table "time.time" of the DB. The function FUNuploadNewRecordsToDB will upload this record on the DB so as to get an ID.
FUNuploadNewRecordsToDB<-function(DBconnection,     # connection to DB
                                  inputDatasetMergedWithDBCodeList,   # Dataset to upload, merged with the table in the DB for the dimension to consider (DBTableName). For the considered dimension, the records that are present in the dataset to upload but not present in DBTableName will be marked as NA. 
                                  DBTableName,   # Name of the table in the DB for the dimension to consider. e.g. "time.time"
                                  DB_PK_AttributeName,     # Name of the primary key attribute in the table in the DB. e.g. "id_time"
                                  dimension_colnames_to_retrieve     # Name of the column(s) to fill with new values within the DB, separated by commas if multiple columns. e.g. "time_start,time_end"
                                  ){
  
  # Check
  index.na<-which(is.na(inputDatasetMergedWithDBCodeList[,DB_PK_AttributeName]))
  
  if (length(index.na)>0){
  column_names<-strsplit(dimension_colnames_to_retrieve,",")[[1]]
  CodesToLoad<-unique(inputDatasetMergedWithDBCodeList[index.na,column_names])
   if(DBTableName=="time.time"){
     CodesToLoad$time_start<-gsub("12:", "00:", CodesToLoad$time_start)
     CodesToLoad$time_end<-gsub("12:", "00:", CodesToLoad$time_end)
   }
  sql4 <- paste0("COPY  ", DBTableName, "(",dimension_colnames_to_retrieve,") FROM STDIN NULL 'NA' ")
  postgresqlpqExec(DBconnection, sql4)
  postgresqlCopyInDataframe(DBconnection, CodesToLoad)
  rs <- postgresqlgetResult(DBconnection)
  
  }
  return(rs)
}


#function to upload a new dataset into database.
FUNUploadDatasetToTableInDB<-function(DBconnection,    # connection to DB
                          InputDataset,    # Dataset to upload in the DB  (the columns of the dataset must have the same names as the ones of the database (TableNameInDB))
                          TableNameInDB){  # Name of the table where the dataset will be uploaded, e.g. "fact_tables.total_cathes"
  

  InputDataset[is.na(InputDataset)] <- "NA" 
  
  sql4 <- paste0("COPY  ", TableNameInDB, "(",
                 paste0(names(InputDataset), collapse=", "), ") FROM STDIN NULL 'NA' ")
  postgresqlpqExec(DBconnection, sql4)
  postgresqlCopyInDataframe(DBconnection, InputDataset)
  rs <- postgresqlgetResult(DBconnection)
  
  return(rs)
}

