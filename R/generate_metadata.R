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
#' @param dataset the dataset associated
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
generate_metadata<-function(metadata_file,dataset,metadata=NULL){
  
# Get available column names for metadata
metadata_columns_input<-colnames(metadata_file)
con<-rtunaatlas::db_connection_tunaatlas_world()  
metadata_columns_output<-dbGetQuery(con,"SELECT column_name FROM information_schema.columns WHERE table_schema = 'metadata' AND table_name = 'metadata'")$column_name

#for (i in 1:ncol(metadata_file)){
#  if (identical(metadata_file[,colnames(metadata_file)[i]],"")){
#    metadata_file[,colnames(metadata_file)[i]]<-NA
#  }
#}




if (metadata_file$dataset_type=="raw_dataset"){
  ### dataset_time_start and dataset_time_end
  metadata_file$dataset_time_start<-as.character(min(as.Date(dataset$time_start)))
  metadata_file$dataset_time_end<-as.character(max(as.Date(dataset$time_end)))
  
  metadata_file$date_start<-substr(df_metadata$dataset_time_start,1,4)
  metadata_file$date_end<-substr(df_metadata$dataset_time_end,1,4)
  
  ### temporal_coverage
  metadata_file$temporal_coverage<-paste0("start=",metadata_file$dataset_time_start,";end=",metadata_file$dataset_time_end,";")
  
  ### title
  #df_metadata$title<-gsub("%date_start%",substr(df_metadata$dataset_time_start,1,4),metadata_file$title)
  #df_metadata$title<-gsub("%date_end%",substr(df_metadata$dataset_time_end,1,4),df_metadata$title)
  
  ### description
  #df_metadata$description<-gsub("%date_start%",substr(df_metadata$dataset_time_start,1,4),metadata_file$description)
  #df_metadata$description<-gsub("%date_end%",substr(df_metadata$dataset_time_end,1,4),df_metadata$description)
  
}


# Complete metadata with any other parameter that might have been generated through the R script of dataset generation. It will be pasted to the appropriate column
for (i in 1:length(metadata_columns_input)){
  content_metadata<-metadata[[metadata_columns_input[i]]][[1]]
  if (!(is.null(content_metadata))){
    metadata_file[,metadata_columns_input[i]]<-gsub("@@automatically generated@@","",metadata_file[,metadata_columns_input[i]])
    metadata_file[,metadata_columns_input[i]]<-paste(metadata_file[,metadata_columns_input[i]],content_metadata,sep="\n")
  }
}

### Replace holes with metadata elements from other columns of the file

for (i in 1:length(metadata_columns_input)){
  this_metadata_element<-metadata_file[metadata_columns_input[i]]
  words_to_replace<-gsub("[\\%\\%]", "", regmatches(this_metadata_element, gregexpr("\\%.*?\\%", this_metadata_element))[[1]])
  if (length(words_to_replace>0)){
    for (i in 1:length(words_to_replace)){
      if (words_to_replace[i] %in% colnames(metadata_file)){
        metadata_file[metadata_columns_input[i]]<-gsub(paste0("%",words_to_replace[i],"%"),this_metadata_element,metadata_file[metadata_columns_input[i]])
      }
    }
  }
  
}

### generate_metadata_from_splitted_columns (date, relation, contacts, etc.)
# 1) create a new column whose name is set as parameter: column_name_final_metadata_file
# 2) get column names of the source metadata file associated to the final metadata file elements
# 3) for each column:
# - get role name
# write in column column_name_final_metadata_file : role_name=name in the column. if there are multiple elements in the column (separated by ";"), paste to have role_name=wxcx;role_name="dfds ; etc.

generate_metadata_from_splitted_columns<-function(metadata_file,column_name_source_metadata_file,column_name_final_metadata_file){
  
  column_names<-colnames(metadata_file)[grep(paste0(column_name_source_metadata_file,"_"),colnames(metadata_file))]
  if (length(column_names)>0){
    metadata_file[column_name_final_metadata_file]<-""
      for (i in 1:length(column_names)){
      role_name<-gsub(paste0(column_name_source_metadata_file,"_"),"",column_names[i])
      metadata_element<-metadata_file[1,column_names[i]]
      if (!(metadata_element %in% c("",NA))){
      cont<-strsplit(metadata_element,split=";")[[1]] 
      if (length(cont)>1){
      for (j in 1:length(cont)){
        metadata_file[column_name_final_metadata_file]<-paste0(metadata_file[column_name_final_metadata_file],role_name,"=",cont[i],";")
      }
      } else {
        metadata_file[column_name_final_metadata_file]<-paste0(metadata_file[column_name_final_metadata_file],role_name,"=",metadata_element,";")
      }
      }
    }
 }
  return(metadata_file)
}


## date
metadata_file<-generate_metadata_from_splitted_columns(metadata_file,"date","date")

## relation
metadata_file<-generate_metadata_from_columns(metadata_file,"relation","relation")

## contacts_and_roles (all except processor in a first stage, and then processor which depends on the number of lineage steps)
names(metadata_file)[names(metadata_file) == "contact_processor"] <- "contact2_processor"
metadata_file<-generate_metadata_from_splitted_columns(metadata_file,"contact","contacts_and_roles")
# Get contact and roles for processor
position_max_number_step<-max(gregexpr(pattern ="step\\d",metadata_file$lineage)[[1]])
number_of_steps<-substr(metadata_file$lineage, position_max_number_step+4, position_max_number_step+4)
number_of_steps<-as.numeric(number_of_steps)
# Create contacts_and_roles for processors
for (i in 1:number_of_steps){
  metadata_file$contacts_and_roles<-paste0(metadata_file$contacts_and_roles,"processor_step",i,"=",metadata_file$contact2_processor,";")
}


### subject
# Get list of dimensions or of column names associated to the dataset
# Check one by one if the columns have different values
dimensions<-NULL
for (col in 1:ncol(dataset)){
  dimension_name<-colnames(dataset[col])
  if (!(dimension_name %in% c("value","time_start","time_end"))){
    unique_values<-unique(dataset[dimension_name])
    if (nrow(unique_values)>1){
      dimensions<-paste(dimensions,dimension_name,sep=", ")
    }
  }
}

if ("time_start" %in% colnames(dataset)){
  dimensions<-paste(dimensions,"time",sep=", ")
}

dimensions<-substring(dimensions, 2)
if (metadata_file$dataset_type=="raw_dataset"){
  subject_element<-"DIMENSIONS"
} else if (metadata_file$dataset_type=="codelist"){
  subject_element<-"COLUMNS"
} else if (metadata_file$dataset_type=="mapping"){
  subject_element<-NULL
  dimensions<-NULL
}
metadata_file$subject<-paste0(subject_element," = ",dimensions," ; ",metadata_file$subject)



 #else {
  #df_metadata$title<-metadata_file$title
  #df_metadata$description<-metadata_file$description
  #df_metadata$subject<-metadata_file$subject
#}


### persistent_identifier
#df_metadata$persistent_identifier<-metadata_file$persistent_identifier

### identifier
#df_metadata$identifier<-metadata_file$identifier

### contacts_and_roles

## Function to generate contacts_and_roles when there are multiple roles
#create_contacts_and_roles<-function(contacts_metadata_file,role_name){
#  cont<-strsplit(contacts_metadata_file,split=";")[[1]] 
#  contact<-NULL
#  for (i in 1:length(cont)){
#    contact<-paste0(contact,role_name,"=",cont[i],";")
#  }
#  return(contact)
#}

## Get contacts and roles (all except for processor which will be dealt separately)
#columns_contacts_and_roles<-colnames(metadata_file)[grep("contact_",colnames(metadata_file))]
#columns_contacts_and_roles<-setdiff(columns_contacts_and_roles,"contact_processor")
#roles<-gsub("contact_","",columns_contacts_and_roles)

#contacts_and_roles<-NULL
#for (j in 1:length(columns_contacts_and_roles)){
#  if (!is.na(metadata_file[,columns_contacts_and_roles[j]])){
#  contact_j<-create_contacts_and_roles(metadata_file[,columns_contacts_and_roles[j]],roles[j])
#  contacts_and_roles<-paste0(contacts_and_roles,contact_j)
#  }
#}

## Get contact and roles for processor
# Get number of steps
#position_max_number_step<-max(gregexpr(pattern ="step\\d",metadata_file$lineage)[[1]])
#number_of_steps<-substr(metadata_file$lineage, position_max_number_step+4, position_max_number_step+4)
#number_of_steps<-as.numeric(number_of_steps)
# Create contacts_and_roles for processors
#contact_processor<-NULL
#for (i in 1:number_of_steps){
#  contact_processor<-paste0(contact_processor,"processor_step",i,"=",metadata_file$contact_processor,";")
#}


#df_metadata$contacts_and_roles<-paste0(contacts_and_roles,contact_processor)


### date
#columns_dates<-colnames(metadata_file)[grep("date_",colnames(metadata_file))]
#roles<-gsub("date_","",columns_dates)

#date<-NULL

#if (length(columns_dates)>0){
#for (j in 1:length(columns_dates)){
#  if (!is.na(metadata_file[,columns_dates[j]])){
#  date<-paste0(date,roles[j],"=",metadata_file[,columns_dates[j]],";")
#  }
#}
#df_metadata$date<-date
#}

### format
#df_metadata$format<-metadata_file$format

### language
#df_metadata$language<-metadata_file$language


### relation
#columns_relation<-colnames(metadata_file)[grep("relation_",colnames(metadata_file))]
#roles<-gsub("relation_","",columns_relation)

#relation<-NULL
#if (length(columns_dates)>0){
  #for (j in 1:length(columns_relation)){
  #if (!is.na(metadata_file[,columns_relation[j]])){
    #relation<-paste0(relation,roles[j],"=",metadata_file[,columns_relation[j]],";")
  #  }
  #}
  #df_metadata$relation<-relation
#}
### spatial_coverage
# DONE AFTER THE UPLOAD OF THE DATASET (in the function load_dataset_in_db)


### rights
#df_metadata$rights<-metadata_file$rights

### source
#df_metadata$source<-metadata_file$source

### lineage

#df_metadata$lineage<-metadata_file$lineage

#words_to_replace<-gsub("[\\%\\%]", "", regmatches(df_metadata$lineage, gregexpr("\\%.*?\\%", df_metadata$lineage))[[1]])
#if (length(words_to_replace>0)){
#for (i in 1:length(words_to_replace)){
#  if (words_to_replace[i] %in% colnames(metadata_file)){
#  df_metadata$lineage<-gsub(paste0("%",words_to_replace[i],"%"),metadata_file[,words_to_replace[i]],df_metadata$lineage)
##  }
# }
#}

#df_metadata$lineage<-gsub("%date_download%",metadata_file$date_download,df_metadata$lineage)
#df_metadata$lineage<-gsub("%relation_source_dataset%",metadata_file$relation_source_dataset,df_metadata$lineage)
#df_metadata$lineage<-gsub("%relation_source_download%",metadata_file$relation_source_download,df_metadata$lineage)

### supplemental_information
#df_metadata$supplemental_information<-metadata_file$supplemental_information

### dataset_type
#df_metadata$dataset_type<-metadata_file$dataset_type

### sql_query_dataset_extraction

# DONE AFTER THE UPLOAD OF THE DATASET (in the function load_dataset_in_db)

### database_table_name
#df_metadata$database_table_name<-metadata_file$database_table_name

### database_view_name
#df_metadata$database_view_name<-metadata_file$database_view_name

## Keep only pertinent columns
metadata_file<-metadata_file[metadata_columns_output]

metadata_file<-data.frame(metadata_file,stringsAsFactors = FALSE)

return(df_metadata)

}
