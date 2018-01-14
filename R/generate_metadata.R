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
con<-rtunaatlas::db_connection_tunaatlas_world()  
metadata_columns_output<-dbGetQuery(con,"SELECT column_name FROM information_schema.columns WHERE table_schema = 'metadata' AND table_name = 'metadata'")$column_name

if (metadata_file$dataset_type=="raw_dataset"){
  
  dataset_time_start<-as.character(min(as.Date(dataset$time_start)))
  dataset_time_end<-as.character(max(as.Date(dataset$time_end)))
  ### dataset_time_start and dataset_time_end
  metadata_file$year_start<-substr(dataset_time_start,1,4)
  metadata_file$year_end<-substr(dataset_time_end,1,4)
  
  ### temporal_coverage
  metadata_file$temporal_coverage<-paste0("start=",dataset_time_start,";end=",dataset_time_end,";")

}


# Complete metadata with any other parameter that might have been generated through the R script of dataset generation. It will be pasted to the appropriate column
metadata_columns_input<-colnames(metadata_file)
for (i in 1:length(metadata_columns_input)){
  content_metadata<-metadata[[metadata_columns_input[i]]][[1]]
  if (!(is.null(content_metadata))){
    metadata_file[,metadata_columns_input[i]]<-gsub("@@automatically generated@@","",metadata_file[,metadata_columns_input[i]])
    metadata_file[,metadata_columns_input[i]]<-paste(metadata_file[,metadata_columns_input[i]],content_metadata,sep="\n")
  }
}

### Replace holes with metadata elements from other columns of the file
for (i in 1:length(metadata_columns_input)){
  this_metadata_element<-metadata_file[metadata_columns_input[i]][[1]]
  words_to_replace<-gsub("[\\%\\%]", "", regmatches(this_metadata_element, gregexpr("\\%.*?\\%", this_metadata_element))[[1]])
  if (length(words_to_replace>0)){
    for (j in 1:length(words_to_replace)){
      if (words_to_replace[j] %in% colnames(metadata_file)){
        metadata_file[metadata_columns_input[i]]<-gsub(paste0("%",words_to_replace[j],"%"),metadata_file[words_to_replace[j]][[1]],metadata_file[metadata_columns_input[i]][[1]])
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
metadata_file<-generate_metadata_from_splitted_columns(metadata_file,"relation","relation")

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

## Keep only pertinent columns

columns_to_keep<-intersect(metadata_columns_output,colnames(metadata_file))
metadata_file<-metadata_file[columns_to_keep]

metadata_file<-data.frame(metadata_file,stringsAsFactors = FALSE)
metadata_file[metadata_file==""]  <- NA

return(metadata_file)

}
