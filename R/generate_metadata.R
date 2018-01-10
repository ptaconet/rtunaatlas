#' @name generate_metadata
#' @aliases generate_metadata
#' @title Generate the dataset of metadata
#' @description Generate a dataset of metadata used as input of the functions to load a dataset in the database from a template of metadata dataframe
#' @export generate_metadata
#'
#'
#' @usage 
#' generate_metadata(metadata_file,dataset_type)
#'    
#' @param metadata_file data.frame of metadata (input)
#' @param dataset_type string type of dataset (raw_dataset, codelist, mapping)
#'
#' @return a data.frame of metadata with the structure to use as input of the functions to load a dataset
#'
#' @details 
#'
#' @family load data
#' 
#' 
#' @examples
#' 
#' 
#' @author Paul Taconet, \email{paul.taconet@@ird.fr}
#'

##### Generate the dataset of metadata, that will be loaded in the DB
generate_metadata<-function(metadata_file,dataset_type){

df_metadata<-NULL

for (i in 1:ncol(metadata_file)){
  if (identical(metadata_file[,colnames(metadata_file)[i]],"")){
    metadata_file[,colnames(metadata_file)[i]]<-NA
  }
}

if (dataset_type=="raw_dataset"){
### dataset_time_start and dataset_time_end.
df_metadata$dataset_time_start<-metadata_file$dataset_time_start
df_metadata$dataset_time_end<-metadata_file$dataset_time_end

### identifier
df_metadata$identifier<-gsub("tunaatlas",paste(df_metadata$dataset_time_start,df_metadata$dataset_time_end,"tunaatlas",sep="_"),metadata_file$persistent_identifier)
#df_metadata$identifier<-gsub("level0",paste(metadata_file$year_tuna_atlas,"level0",sep="_"),df_metadata$identifier)
df_metadata$identifier<-gsub("level",paste0(metadata_file$year_tuna_atlas,"_level"),df_metadata$identifier)
df_metadata$identifier<-gsub("-","_",df_metadata$identifier)

### title
df_metadata$title<-gsub("%date_start%",substr(df_metadata$dataset_time_start,1,4),metadata_file$title)
df_metadata$title<-gsub("%date_end%",substr(df_metadata$dataset_time_end,1,4),df_metadata$title)

### description
df_metadata$description<-gsub("%date_start%",substr(df_metadata$dataset_time_start,1,4),metadata_file$description)
df_metadata$description<-gsub("%date_end%",substr(df_metadata$dataset_time_end,1,4),df_metadata$description)


### temporal_coverage
df_metadata$temporal_coverage<-paste0("start=",df_metadata$dataset_time_start,";end=",df_metadata$dataset_time_end,";")



} else {
  df_metadata$identifier<-metadata_file$persistent_identifier
  df_metadata$title<-metadata_file$title
  df_metadata$description<-metadata_file$description
}

### Subject
df_metadata$subject=metadata_file$subject

### persistent_identifier
df_metadata$persistent_identifier<-metadata_file$persistent_identifier

### contacts_and_roles

## Function to generate contacts_and_roles when there are multiple roles
create_contacts_and_roles<-function(contacts_metadata_file,role_name){
  cont<-strsplit(contacts_metadata_file,split=";")[[1]] 
  contact<-NULL
  for (i in 1:length(cont)){
    contact<-paste0(contact,role_name,"=",cont[i],";")
  }
  return(contact)
}

## Get contacts and roles (all except for processor which will be dealt separately)
columns_contacts_and_roles<-colnames(metadata_file)[grep("contact_",colnames(metadata_file))]
columns_contacts_and_roles<-setdiff(columns_contacts_and_roles,"contact_processor")
roles<-gsub("contact_","",columns_contacts_and_roles)

contacts_and_roles<-NULL
for (j in 1:length(columns_contacts_and_roles)){
  if (!is.na(metadata_file[,columns_contacts_and_roles[j]])){
  contact_j<-create_contacts_and_roles(metadata_file[,columns_contacts_and_roles[j]],roles[j])
  contacts_and_roles<-paste0(contacts_and_roles,contact_j)
  }
}

## Get contact and roles for processor
# Get number of steps
position_max_number_step<-max(gregexpr(pattern ="step\\d",metadata_file$lineage)[[1]])
number_of_steps<-substr(metadata_file$lineage, position_max_number_step+4, position_max_number_step+4)
number_of_steps<-as.numeric(number_of_steps)
# Create contacts_and_roles for processors
contact_processor<-NULL
for (i in 1:number_of_steps){
  contact_processor<-paste0(contact_processor,"processor_step",i,"=",metadata_file$contact_processor,";")
}


df_metadata$contacts_and_roles<-paste0(contacts_and_roles,contact_processor)

### subject
df_metadata$subject<-metadata_file$subject

### date
columns_dates<-colnames(metadata_file)[grep("date_",colnames(metadata_file))]
roles<-gsub("date_","",columns_dates)

date<-NULL

if (length(columns_dates)>0){
for (j in 1:length(columns_dates)){
  if (!is.na(metadata_file[,columns_dates[j]])){
  date<-paste0(date,roles[j],"=",metadata_file[,columns_dates[j]],";")
  }
}
df_metadata$date<-date
}

### format
df_metadata$format<-metadata_file$format

### language
df_metadata$language<-metadata_file$language


### relation
columns_relation<-colnames(metadata_file)[grep("relation_",colnames(metadata_file))]
roles<-gsub("relation_","",columns_relation)

relation<-NULL
if (length(columns_dates)>0){
for (j in 1:length(columns_relation)){
  if (!is.na(metadata_file[,columns_relation[j]])){
  relation<-paste0(relation,roles[j],"=",metadata_file[,columns_relation[j]],";")
  }
}
df_metadata$relation<-relation
}
### spatial_coverage
# DONE AFTER THE UPLOAD OF THE DATASET (in the function load_dataset_in_db)


### rights
df_metadata$rights<-metadata_file$rights

### source
df_metadata$source<-metadata_file$source

### lineage

df_metadata$lineage<-metadata_file$lineage

words_to_replace<-gsub("[\\%\\%]", "", regmatches(df_metadata$lineage, gregexpr("\\%.*?\\%", df_metadata$lineage))[[1]])
if (length(words_to_replace>0)){
for (i in 1:length(words_to_replace)){
  if (words_to_replace[i] %in% colnames(metadata_file)){
  df_metadata$lineage<-gsub(paste0("%",words_to_replace[i],"%"),metadata_file[,words_to_replace],df_metadata$lineage)
  }
 }
}

#df_metadata$lineage<-gsub("%date_download%",metadata_file$date_download,df_metadata$lineage)
#df_metadata$lineage<-gsub("%relation_source_dataset%",metadata_file$relation_source_dataset,df_metadata$lineage)
#df_metadata$lineage<-gsub("%relation_source_download%",metadata_file$relation_source_download,df_metadata$lineage)

### supplemental_information
df_metadata$supplemental_information<-metadata_file$supplemental_information

### dataset_type
df_metadata$dataset_type<-dataset_type

### sql_query_dataset_extraction

# DONE AFTER THE UPLOAD OF THE DATASET (in the function load_dataset_in_db)

### database_table_name
df_metadata$database_table_name<-metadata_file$database_table_name

### database_view_name
df_metadata$database_view_name<-metadata_file$database_view_name


df_metadata<-data.frame(df_metadata,stringsAsFactors = FALSE)

return(df_metadata)

}
