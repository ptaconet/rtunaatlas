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
  
  #Total number of strata combinations
  # nb_strata_combinations_input <- nrow(df_input %>% group_by_(.dots=reallocation_dimensions) %>% summarise(value = sum(value)))
  nb_strata_combinations_input <- df_input %>% select(reallocation_dimensions)  %>% distinct
  # Temporary set all NAs to "UNK/IND" 
  #df_input<-replace(df_input, is.na(df_input), "UNK/IND") 
  total_before <- sum(df_input$value)
  
  # Get the index of the lines that are with wrong (inland) or without spatial information(ie lines that will be reallocated or removed afterwards)
  index.dataToReallocate<-which(df_input[,dimension_reallocation] %in% vector_to_reallocate)
  cat(paste0(length(index.dataToReallocate), " lines have to be removed and their catch value has to be reallocated in related lines (same strata) \n "))
  
  if(length(index.dataToReallocate)>0){
    
    # keep only lines of the dataset to reallocate 
    dataset_to_reallocate<-df_input[index.dataToReallocate,]
    # nrow(dataset_to_reallocate)
    
    # group lines to reallocate for the given list of dimensions
    dataset_to_reallocate <- dataset_to_reallocate %>% 
      group_by_(.dots=reallocation_dimensions) %>% 
      summarise(value = sum(value))
    # nrow(dataset_to_reallocate)
    
    # data that are reallocated
    data_reallocated<-semi_join(dataset_to_reallocate,df_input,by=reallocation_dimensions)
    perc_data_reallocated<-sum(data_reallocated$value)/sum(dataset_to_reallocate$value)*100
    
    # get the sum of catches to reallocate on the input dataset and compare it to the sum of the catches of the whole dataset
    sum_fact_to_reallocate <- data_reallocated %>% 
      group_by(unit) %>% 
      summarise(value_reallocate = sum(value))
    
    sum_whole_dataset <- df_input %>% 
      group_by(unit) %>% 
      summarise(value = sum(value))
    
    # basic stats
    stats_reallocated_data<-left_join(sum_whole_dataset,sum_fact_to_reallocate)
    #stats_reallocated_data$percentage_of_total_catches_reallocated<-stats_reallocated_data$value_reallocate/stats_reallocated_data$value*100
    
    # Remove data to reallocate from the dataset
    df_input<-df_input[-index.dataToReallocate,]
    # nb_strata_combinations_remaining_lines <- nrow(df_input %>% group_by_(.dots=reallocation_dimensions) %>% summarise(value = sum(value)))
    nb_strata_combinations_remaining_lines <- df_input %>% select(reallocation_dimensions)  %>% distinct
    
    # nb_strata_combinations_removed_lines <- nrow(df_input[index.dataToReallocate,] %>% group_by_(.dots=reallocation_dimensions) %>% summarise(value = sum(value)))
    nb_strata_combinations_removed_lines <- df_input[index.dataToReallocate,]  %>% select(reallocation_dimensions)  %>% distinct
    # nrow(nb_strata_combinations_removed_lines)
    # semi_join(nb_strata_combinations_remaining_lines,nb_strata_combinations_removed_lines)
    inner_join
    inner_join(nb_strata_combinations_removed_lines,nb_strata_combinations_remaining_lines)
    # left_join(nb_strata_combinations_removed_lines,nb_strata_combinations_remaining_lines)
    
    ### Find matching same strata between data to reallocate and the input dataset and store the value to reallocate in new column
    wxc<-left_join(df_input,dataset_to_reallocate,by=reallocation_dimensions)
    # nrow(df_input) - nrow(wxc)
    # head(wxc)
    # wxc  %>% filter(!is.na(value.y))
    
    # titi <- wxc   %>% filter(!is.na(value.y))  %>%    group_by_(.dots=c(reallocation_dimensions,"value.y")) %>% select(value.y)
    # sum(titi$value.y)
    # inter <- wxc   %>% filter(!is.na(value.y)) %>% select(reallocation_dimensions)  %>% distinct
    # nrow(inter)
    # 
    # get the number of lines for each strata on which to reallocate data (ie number of strata that are available in the input dataset for each strata to reallocate)
    wxc2 <- wxc %>%
      filter(!is.na(value.y)) %>%
      group_by_(.dots=c(reallocation_dimensions,"value.y")) %>%
      summarise(number=n())
    # summarise(number=n(),total_catch=value.y/n())
    # sum(wxc2$total_catch)
    #calculate how much of the catch will be reallocated in each line of the same strat
    # @juldebar => TO BE DONE : to do things properly we should use here a raising factor instead of adding the same amount to each line whatever its total
    wxc2$value_realloc<-wxc2$value.y/wxc2$number
    wxc2$value.y<-NULL
    wxc2$number<-NULL
    wxc2<-data.frame(wxc2)
    head(wxc2)
    
    # nb_strata_combinations_reallocated <- nrow(wxc2)
    # 
    # nb_strata_combinations_input
    # nb_strata_combinations_remaining_lines
    # nb_strata_combinations_removed_lines
    # nb_strata_combinations_reallocated
    # nrow(nb_strata_combinations_input)
    # nrow(nb_strata_combinations_remaining_lines)
    # nrow(nb_strata_combinations_removed_lines)
    # nrow(inter)
    # nb_strata_combinations_reallocated

    
    df_input<-left_join(df_input,wxc2,by=reallocation_dimensions)
    
    # @juldebar => same with dplyr 
    # df_input <- df_input  %>% mutate(value_realloc=replace(value_realloc, is.na(value_realloc), 0)) %>% mutate(value=replace(value,, value+value_realloc))  # %>% select (-c(value_realloc))
    
    index.dataToReallocate<-which(!is.na(df_input$value_realloc))
    df_input$value[index.dataToReallocate] <- df_input$value[index.dataToReallocate]+df_input$value_realloc[index.dataToReallocate]
    
    df_input$value_realloc<-NULL
    # total_before - sum(df_input$value)
    # sum_fact_to_reallocate$value_reallocate-sum(df_input$value)
    
  } else {
    dataset_to_reallocate<-NULL
    stats_reallocated_data<-NULL
    perc_data_reallocated<-NULL
  }
  
  #df_input[df_input == "UNK/IND"] <- NA
  total_after <- sum(df_input$value)
  difference <- total_before - total_after
  difference
  sum_fact_to_reallocate$value_reallocate-difference
  
  return(list(df=df_input,stats=stats_reallocated_data))
  
}
