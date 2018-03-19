#' @name spatial_curation_reallocate_data
#' @aliases spatial_curation_reallocate_data
#' @title Reallocate data using a spatial criterium
#' @description This function reallocates data
#' @export 
#' 
#' @param df_input data.frame of fact
#' @param dimension_reallocation dimension to reallocate
#' @param vector_to_reallocate vector of data to reallocate
#' @param reallocation_dimensions vector of dimensions to use for the reallocation 
#'
#' @return a list with 2 objects: 
#'  \itemize{
#'  \item{"df": }{\code{df_input} where data have been reallocated}
#'  \item{"stats": }{A data.frame with some information regarding the reallocation process:}
#'    \itemize{
#'     \item{"value": }{sum in the input dataset}
#'     \item{"value_reallocate": }{sum reallocated}
#'    }
#'  }
#'
#' @details Example of use of this function: reallocate the catch of the areas that are located in land over the areas that are located at sea with the same dimensions.
#'
#' @author Paul Taconet, IRD \email{paul.taconet@ird.fr}
#'
#' @family process data
#'
#' @examples
#' 
#' # Connect to Tuna atlas database
#' con<-db_connection_tunaatlas_world()
#' 
#' # Extract a dataset from Tuna atlas World database
#' dataset_metadata<-list_metadata_datasets(con,identifier="atlantic_ocean_catch_1950_01_01_2016_01_01_tunaatlasICCAT_2017_level0__noSchool")
#' df<-extract_dataset(con,dataset_metadata)
#' 
#' # Retrieve the spatial coding system used in the dataset
#' df_spatial_codingsystem <- get_codelist_of_dimension(con,dataset_metadata,"area")
#' 
#' # Get data that are fully located inland:
#' df_intersect_continents<-spatial_curation_intersect_areas(con,df_input=df,df_spatial_code_list_name=df_spatial_codingsystem$dataset_name,intersection_spatial_code_list_name="gshhs_world_coastlines")
#' areas_inland<-df_intersect_continents$df_input_areas_intersect_intersection_layer$geographic_identifier_source_layer[which(df_intersect_continents$df_input_areas_intersect_intersection_layer$proportion_source_area_intersection==1)]
#' # In the above, df_intersect_continents$df_input_areas_intersect_intersection_layer$proportion_source_area_intersection==1 means that we want to extract the codes of the areas that are located fully inland (100% of the surface is located inland). 
#' 
#' # Reallocate data that are located inland. Reallocate on strata with same month, gear, flag, species, unit :
#' dataset_reallocate_data_inland<-spatial_curation_function_reallocate_data(
#' df_input=df,
#' dimension_reallocation="geographic_identifier",
#' vector_to_reallocate=areas_inland,
#' reallocation_dimensions=c("month","gear","flag","species","unit"))
#' 
#' # New dataset with data that were located inland reallocated to areas at sea with same dimensions
#' head(dataset_reallocate_data_inland$df)
#' 
#' # To get some information on the reallocation process:
#' dataset_reallocate_data_inland$stats
#' 



# Function to reallocate data, given a vector of areas as input of data to reallocate (eg data on land or data with no geographical stratification)
spatial_curation_function_reallocate_data<-function(df_input,dimension_reallocation,vector_to_reallocate,reallocation_dimensions){
  
  # Convert to data.table for faster operations
  #df_input<-data.table(df_input)
  
  # Temporary set all NAs to "UNK/IND" 
  #df_input<-replace(df_input, is.na(df_input), "UNK/IND") 
  
  
  # Get the index of the lines that are inland (ie lines that will be reallocated or removed afterwards)
  index.dataToReallocate<-which(df_input[,dimension_reallocation] %in% vector_to_reallocate)
  
  if(length(index.dataToReallocate)>0){
    
    # get the dataset to reallocate 
    dataset_to_reallocate<-df_input[index.dataToReallocate,]
    dataset_to_reallocate <- dataset_to_reallocate %>% 
      group_by_(.dots=reallocation_dimensions) %>% 
      summarise(value = sum(value))
    
    # data that are reallocated
    data_reallocated<-semi_join(dataset_to_reallocate,df_input,by=reallocation_dimensions)
    perc_data_reallocated<-sum(data_reallocated$value)/sum(dataset_to_reallocate$value)*100
    
    # get the sum of catches to reallocate on the dataset and compare it to the sum of the catches of the whole dataset
    sum_fact_to_reallocate <- data_reallocated %>% 
      group_by(unit) %>% 
      summarise(value_reallocate = sum(value))
    
    sum_whole_dataset <- df_input %>% 
      group_by(unit) %>% 
      summarise(value = sum(value))
    
    stats_reallocated_data<-left_join(sum_whole_dataset,sum_fact_to_reallocate)
    #stats_reallocated_data$percentage_of_total_catches_reallocated<-stats_reallocated_data$value_reallocate/stats_reallocated_data$value*100
    
    
    # Remove data to reallocate from the dataset
    df_input<-df_input[-index.dataToReallocate,]
    
    ### find strata from data to reallocate that are available in the input dataset
    wxc<-left_join(df_input,dataset_to_reallocate,by=reallocation_dimensions)
    # get the number of strata on which to reallocate data (ie number of strata that are available in the input dataset for each strata to reallocate)
    wxc2<-wxc %>%
      filter(!is.na(value.y)) %>%
      group_by_(.dots=c(reallocation_dimensions,"value.y")) %>%
      summarise(number=n())
    wxc2$value_realloc<-wxc2$value.y/wxc2$number
    wxc2$value.y<-NULL
    wxc2$number<-NULL
    wxc2<-data.frame(wxc2)
    
    
    df_input<-left_join(df_input,wxc2,by=reallocation_dimensions)
    
    index.dataToReallocate<-which(!is.na(df_input$value_realloc))
    df_input$value[index.dataToReallocate]<-df_input$value[index.dataToReallocate]+df_input$value_realloc[index.dataToReallocate]
    
    df_input$value_realloc<-NULL
    
  } else {
    dataset_to_reallocate<-NULL
    stats_reallocated_data<-NULL
    perc_data_reallocated<-NULL
  }
  
  #df_input[df_input == "UNK/IND"] <- NA
  
  
  return(list(df=df_input,stats=stats_reallocated_data))
  
}
