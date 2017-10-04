#' @name convert_units
#' @aliases convert_units
#' @title Convert units of measure in a fact dataset
#' @description This function converts the units of measure in a fact dataset using a dataset of factors of conversions between units. The data frames of fact and of factors of conversions must be properly structured. For structures of data frames, see details and here: \url{http://}. 
#' @export
#'
#' @usage convert_units(df_input, df_conversion_factor, codelist_geoidentifiers_conversion_factors)
#'                 
#' @param df_input a data.frame of fact
#' @param df_conversion_factor a data.frame of factors of conversion between units
#' @param codelist_geoidentifiers_conversion_factors NULL or string. The name of the coding system for the spatial dimension used in df_conversion_factor, or NULL if the coding system for the spatial dimension is the same as the one used in df_input
#' 
#' @return a list with two objects:
#' \itemize{
#'  \item{"df": }{The input data.frame of fact, where the measures and related units have been converted when factors of conversion were available. Some data might not be converted at all because no conversion factor exists for the stratum: these data are kept in their source unit (i.e. they are not removed from the dataset).
#'  \item{"stats": }{A data.frame with some information regarding the conversion. ####It provides, for each unit of measure available in the input dataset, the sum and percentage of the data that could not be map because no correspondance are available in the dataset of mappings between code lists}
#' }
#' 
#' @details 
#' The data frame of factors of conversion between units (df_conversion_factor) must have the following structure:
#' - several columns of dimension, that stratify the factor of conversion (e.g. species, gear, etc.),
#' - one column named 'unit', that provides the code of the source unit of the conversion factor,
#' - one column named 'unit_target', that provides the code of the target unit of the conversion factor,
#' - one column named 'conversion_factor', that provides the numerical factor of conversion to convert the measure from the unit stated in the column 'unit' to the unit stated in the column 'unit_target'.
#' See an example of dataset of factors of conversion here: \url{https://goo.gl/KriwxV}.
#' 
#' Example: These are the first rows of a dataset of factors of conversions:
#' 
#'  \tabular{rrrrr}{
#'  source_authority   \tab species \tab    gear \tab  geographic_identifier \tab  time_start  \tab  time_end    \tab   unit \tab    unit_target  \tab   conversion_factor\cr
#'  IOTC  \tab          \tab    YFT  \tab      LL \tab    1      \tab                 1952-01-01 \tab   1953-01-01 \tab    NO   \tab    MT    \tab          0.048060001\cr
#'  IOTC  \tab          \tab    YFT   \tab     LL \tab    2       \tab                1952-01-01  \tab  1953-01-01   \tab  NO   \tab    MT    \tab          0.048680000\cr
#'  IOTC  \tab          \tab    YFT   \tab     LL \tab    3       \tab                1952-01-01  \tab  1953-01-01  \tab   NO   \tab    MT     \tab         0.058639999\cr
#'  IOTC  \tab          \tab    BET   \tab     LL \tab    0       \tab                1952-01-01  \tab  1953-01-01  \tab   NO   \tab    MT     \tab         0.044340000
#'  }
#'  
#'  The first row means that for the combination of dimensions: { source_authority=IOTC, species=YFT, gear=LL, geographical_identifier=1, starting date of validity of the factor of conversion (time_start)=1952-01-01, ending date of validity of the factor of conversion (time_start)=1953-01-01}, the factor of conversion to convert a measure from unit=NO to target_unit=MT is equal to 0.048060001 
#'  
#'  The codes used in the dimensions of the dataset of factors of conversion (df_conversion_factor) must be the same as the ones used in the dataset of fact with units to convert (df_input), except for the spatial dimension (geographic_identifier) - see here-after for more details.
#'  The only mandatory columns of the dataset of factors of conversion are "unit", "unit_target" and "conversion_factor". All the other columns are here to stratify the factors of conversion (by species, gear, time, space, etc.).
#'  Particularly, the columns "time_start", "time_end" and "geographic_identifier" allow to stratify spatialy and temporarily the factors of conversion. 
#'  The columns "time_start" and "time_end" provide respectively the starting date and the ending date of validity of the factor of conversion. 
#'  The column "geographic_identifier" provides the spatial stratification of the factor of conversion. If the coding system for spatial stratification used in df_conversion_factor is the same as the one used in df_input, then the parameter codelist_geoidentifiers_conversion_factors must be set to NULL. Else, the spatial coding system used in df_conversion_factor must be stored in the Sardara database, and the parameter codelist_geoidentifiers_conversion_factors must be set to the name of the spatial coding system (table) in Sardara DB.
#'  If df_conversion_factor mixes factors of conversion that have and do not have a spatial stratification, the rows that do not have spatial stratification must be set to geographic_identifier= 0.   
#' 
#'  Columns of time (time_start and time_end) must be character type (not Posix).
#' 
#' @family create your own tuna atlas
#' 
#' 
#' @examples
#' 
#' # Connect to Sardara World DB
#' con=db_connection_sardara_world()
#' 
#' # Retrieve IOTC georeferenced catch data from 2017
#' df_input<-iotc_catch_level0(2017)
#' 
#' # Open a dataset of factors of conversion (the one used to convert units of catch in the IRD Tuna Atlas)
#' conversion_factors_dataset="https://goo.gl/KriwxV"
#' df_conversion_factor=read.csv(conversion_factors_dataset,stringsAsFactors = F,colClasses="character")
#' head(df_conversion_factor)
#'
#' # Map gears of input dataset with standard gears, and then with sardara groups of gears, to align with codes of gears used in the conversion factor dataset
#' gear_rfmos_mapping_to_standard<-extract_dataset(con,list_metadata_datasets(con,dataset_name="codelist_mapping_gear_iotc_isscfg_revision_1")) 
#' df_mapped<-map_codelist(df_input,gear_rfmos_mapping_to_standard,"gear")$df
#' gear_standard_mapping_to_gear_sardara<-extract_dataset(con,list_metadata_datasets(con,dataset_name="codelist_mapping_gear_iotc_geargroup_tunaatlas")) 
#' df_input<-map_codelist(df_input,gear_standard_mapping_to_gear_sardara,"gear")$df
#' 
#' # Convert units MTNO to MT and remove NOMT (we do not keep the data that were expressed in number with corresponding value in weight)
#' df_input$unit[which(df_input$unit == "MTNO")]<-"MT"
#' df_input<-df_input[!(df_input$unit=="NOMT"),]
#'
#' # Convert units from numbers to weight using the dataset of factors of conversion. 
#' # The spatial coding system used in conversion_factor (column geographic_identifier) is not the same as the one used in df_input. Hence, we set in the parameter codelist_geoidentifiers_conversion_factors the name of the spatial coding system used in df_conversion factor ("areas_conversion_factors_numtoweigth_ird").
#' df_converted<-convert_units(con, df_input = df_input, df_conversion_factor = df_conversion_factor, codelist_geoidentifiers_conversion_factors = "areas_conversion_factors_numtoweigth_ird")
#'
#' # Get the dataframe with units converted: data that were expressed in number are converted to metric tons. Some data might not be converted at all because no conversion factor exists for the stratum: these data are kept in their original unit (in this case, number).
#' df_converted_df<-df_converted$df
#' head(df_converted_df)
#' 
#' # Get information regarding the conversion (data converted, data not converted because no factor of conversion existed, etc.)
#' df_converted$stats
#'
#' @author Paul Taconet, \email{paul.taconet@@ird.fr}
#' @import data.table dplyr RPostgreSQL   

convert_units<-function(con,df_input,df_conversion_factor,codelist_geoidentifiers_conversion_factors){
  
  cat(paste0("\n converting units and measures"))
  
  df_input<-data.table(df_input)
  
  units_source<-unique(df_conversion_factor$unit)
  units_target<-unique(df_conversion_factor$unit_target)
  df_conversion_factor$conversion_factor=as.numeric(df_conversion_factor$conversion_factor)
  
  if ("geographic_identifier" %in% colnames(df_conversion_factor)){
    ## deal with space: assign to each geographic_identifier of input dataset a geographic_identifier of conversion factor dataset
    colnames(df_conversion_factor)[which(colnames(df_conversion_factor)=="geographic_identifier")]<-"conv_factor_df_geo_id"
    
    # if the geographic areas of the conversion factors are the same as the ones used in the input dataset, there is nothing to do
    if (is.null(codelist_geoidentifiers_conversion_factors)){
      df_input$conv_factor_df_geo_id<-df_input$geographic_identifier
    } else {  # else we intersect input_dataset_geo_codelist_sardara and codelist_geoidentifiers_conversion_factors to assign to each geographic_identifier of input dataset a geographic_identifier of conversion factor dataset
      dataset_distinct_geographic_identifier<-unique(df_input$geographic_identifier)  
      dataset_distinct_geographic_identifier<-paste(unique(dataset_distinct_geographic_identifier), collapse = '\',\'')
      
      drv <- dbDriver("PostgreSQL")
      con <- dbConnect(drv, dbname="sardara_world", user="invsardara", password="fle087", host="db-tuna.d4science.org")
      
      correspondance_geo_identifiers_input_df_conv_fact_df<-dbGetQuery(con,paste("select
                                                                                 u1.codesource_area as geographic_identifier,
                                                                                 u2.codesource_area as conv_factor_df_geo_id
                                                                                 from
                                                                                 area.areas_with_geom u1,
                                                                                 area.areas_with_geom u2
                                                                                 where
                                                                                 u2.tablesource_area='",codelist_geoidentifiers_conversion_factors,"' and u1.codesource_area IN ('",dataset_distinct_geographic_identifier,"')
                                                                                 and ST_Contains(u2.geom, u1.geom)",sep=""))
      
      dbDisconnect(con)
      
      df_input<-merge(df_input,data.table(correspondance_geo_identifiers_input_df_conv_fact_df))
      
    }
  }
  
  if ("time_start" %in% colnames(df_conversion_factor)){
    ## deal with time: 
    # assign to each time of input dataset a time of conversion factor dataset
    colnames(df_conversion_factor)[which(colnames(df_conversion_factor)=="time_start")]<-"conv_factor_df_time_start"
    colnames(df_conversion_factor)[which(colnames(df_conversion_factor)=="time_end")]<-"conv_factor_df_time_end"
    
    combination_times_df_conversion_factor<-unique(df_conversion_factor[c("conv_factor_df_time_start", "conv_factor_df_time_end")])
    combination_times_input_dataset<-unique(data.frame(df_input)[c("time_start", "time_end")])
    
    # convert the dates to to POSIXlt type to be able to compare the dates
    combination_times_df_conversion_factor$conv_factor_df_time_start<-strptime(combination_times_df_conversion_factor$conv_factor_df_time_start, "%Y-%m-%d")
    combination_times_df_conversion_factor$conv_factor_df_time_end<-strptime(combination_times_df_conversion_factor$conv_factor_df_time_end, "%Y-%m-%d")
    combination_times_input_dataset$time_start<-strptime(combination_times_input_dataset$time_start, "%Y-%m-%d")
    combination_times_input_dataset$time_end<-strptime(combination_times_input_dataset$time_end, "%Y-%m-%d")
    
    
    
    combination_times_input_dataset$conv_factor_df_time_start<-NA
    combination_times_input_dataset$conv_factor_df_time_end<-NA
    
    for (i in 1:nrow(combination_times_input_dataset)){
      combination_times_input_dataset_this_row<-combination_times_df_conversion_factor[which(combination_times_df_conversion_factor$conv_factor_df_time_start <= combination_times_input_dataset$time_start[i] & combination_times_df_conversion_factor$conv_factor_df_time_end >= combination_times_input_dataset$time_end[i]), ]
      combination_times_input_dataset$conv_factor_df_time_start[i]<-as.character(combination_times_input_dataset_this_row$conv_factor_df_time_start[1])
      combination_times_input_dataset$conv_factor_df_time_end[i]<-as.character(combination_times_input_dataset_this_row$conv_factor_df_time_end[1])
    }
    
    combination_times_input_dataset$time_start <- as.character(combination_times_input_dataset$time_start)
    combination_times_input_dataset$time_end <- as.character(combination_times_input_dataset$time_end)
    
    df_input<-left_join(df_input,combination_times_input_dataset)
    
  }
  
  
  # assign conv_factor_df_geo_id=0 to the concerned data . 0 is for when there is no spatial stratification in the factors of conversion
  data_zone_0<-df_conversion_factor[which(df_conversion_factor$conv_factor_df_geo_id==0),]
  colnames(data_zone_0)[which(names(data_zone_0) == "conv_factor_df_geo_id")] <- "zone0"
  data_zone_0$conversion_factor<-NULL
  df_input<-left_join(df_input,data_zone_0)
  
  df_input$conv_factor_df_geo_id[which(!is.na(df_input$zone0))]<-"0"
  df_input<-df_input[, !(colnames(df_input) %in% c("zone0","unit_target"))]
  
  
  
  # finally merge dataset with factors of conversion
  
  df_input<-left_join(df_input,df_conversion_factor)
  
  
  sum_before_conversion<-df_input %>%
    #filter(unit %in% units_source) %>%
    group_by(unit) %>%
    summarise(sum_value_before_conversion = sum(value ))
  
  
  
  stats_before_conversion<-df_input %>%
    rename(unit_source = unit) %>%
    group_by(unit_source,unit_target) %>%
    summarize(sum_unit_source_before_conversion=sum(value)) %>%
    filter(!is.na(unit_target)) 
  # mutate(unit_target = ifelse(is.na(unit_target), unit_source,unit_target))
  
  ## make lasts operations
  
  # dataset_with_units_to_convert is the dataset with units to convert into target unit
  #dataset_with_units_to_convert<-df_input[ which(df_input$unit %in% unique(df_conversion_factor$unit)), ]
  #total_in_units_to_convert<-sum(dataset_with_units_to_convert$value)  ## to change to something like   total_in_units_to_convert <- df_input %>% group_by_(setdiff(columns_without_measure,"unit")) %>% summarise(value=sum(value))
  
  #dataset_to_keep_as_is<-df_input[ which(df_input$unit %in% unique(df_conversion_factor$unit_target)), ]
  #dataset_units_converted<-df_input[ which(df_input$unit_target %in% unique(df_conversion_factor$unit_target)), ]
  
  index.not_na.conv_factor<-which(!is.na(df_input$conversion_factor))
  #converted_in_unit_to_convert<-sum(df_input$value[index.not_na.conv_factor])   ## to change to something like   converted_in_unit_to_convert <- df_input %>% group_by_(setdiff(columns_without_measure,"unit")) %>% summarise(value=sum(value))
  df_input$value[index.not_na.conv_factor]<-df_input$value[index.not_na.conv_factor]*df_input$conversion_factor[index.not_na.conv_factor]
  
  stats_after_conversion<-df_input %>% 
    rename(unit_source = unit) %>%
    group_by(unit_source,unit_target) %>%
    summarize(sum_unit_target_after_conversion=sum(value)) %>%
    filter(!is.na(unit_target))
  # mutate(unit_target = ifelse(is.na(unit_target), unit_source,unit_target))
  
  # We change the unit of the data that have been converted
  df_input$unit[index.not_na.conv_factor]<-df_input$unit_target[index.not_na.conv_factor]
  
  # We remove useless columns
  df_input<-df_input[, !(colnames(df_input) %in% c("conv_factor_df_geo_id","conv_factor_df_time_start","conv_factor_df_time_end","unit_target","conversion_factor"))]
  
  #dataset_with_units_to_convert<-dataset_with_units_to_convert[, !(colnames(dataset_with_units_to_convert) %in% c("conv_factor_df_geo_id","conv_factor_df_time_start","conv_factor_df_time_end","unit_target","conversion_factor"))]
  
  sum_after_conversion<-df_input %>%
    #filter(unit %in% units_source) %>%
    group_by(unit) %>%
    summarise(sum_value_after_conversion = sum(value ))
  
  
  # create table of stats
  
  
  
  stats<-merge(stats_before_conversion,stats_after_conversion)
  
  
  
  
  
  stats %>% group_by(unit_source)%>%
    summarize(sum_unit_source_before_conversion=sum(sum_unit_source_before_conversion))
  
  stats %>% group_by(unit_target)%>%
    summarize(sum_unit_target_after_conversion=sum(sum_unit_target_after_conversion))
  
  
  
  
  #stats_after_conversion<-df_input %>% 
  #  mutate(sum_converted_notconverted = ifelse(is.na(df_input$conversion_factor), "sum_value_not_converted", "sum_value_converted")) %>% 
  #  group_by(unit_target,sum_converted_notconverted) %>%
  #  summarize(sum_unit_from_after_conversion=sum(value))
  
  
  
  # df_input is the dataset with the catch units converted into weight
  # dataset_to_keep_as_is is the dataset where no conversion has been done 
  # dataset_units_converted is the dataset only with the dataset converted into weight
  # ratio_converted_weight is the proportion, among the whole dataset in output, of the catches that have been converted in weight (ie originally expressed in number)
  # ratio_converted_number is the proportion of the catches that were originally expressed in number that have been converted into weight
  
  #ratio_converted_weight<-sum(df_input$value[index.not_na.conv_factor])/(sum(df_input$value[setdiff(rownames(df_input),index.not_na.conv_factor)])+sum(df_input$value[index.not_na.conv_factor]))*100
  #ratio_converted_number<-converted_in_unit_to_convert/total_in_units_to_convert*100
  
  #stats_converted_data<-data.frame(ratio_converted_weight,ratio_converted_number)
  #colnames(stats_converted_data)<-c("ratio_converted_weight","ratio_converted_number")
  
  # sum rows that have been converted
  #df_input <- df_input %>%
  #  group_by(rfmo,schooltype,species,time_start,time_end,area,gear,gear_group,flag,catchtype,catchunit) %>%
  #  summarise(value=sum(value))
  
  
  # Plot showing the data that have been converted and the data that are still in number#
  #function_plot_timeSeries_byCatchUnit<-function(catches_originally_expressed_in_weight,catches_converted_into_weight,unitFilter) {
  
  
  
  #if (!is.null(catches_converted_into_weight)){
  #catches_converted_into_weight$unit[which(catches_converted_into_weight$unit=="MT")]<-"MTfromNO"
  
  #inputDF<-rbind(catches_originally_expressed_in_weight,catches_converted_into_weight)
  #} else {
  #inputDF<- catches_originally_expressed_in_weight
  #}
  #inputDF$year<-as.numeric(substr(inputDF$time_start, 0, 4))
  
  #data_aggregated_by_unit <- inputDF %>% 
  #filter (unit %in% unitFilter) %>% 
  #group_by(unit,year) %>% 
  # summarise(value = sum(value))
  
  #plot<-ggplot(data=data_aggregated_by_unit, aes(x=year, y=value, fill=unit)) +
  #geom_bar(stat="identity") +
  #ggtitle("Catches by unit of catch") +
  # ylab("Catches")
  
  #return(plot)
  
  #}
  
  #plot_catches_by_catchunit<-function_plot_timeSeries_byCatchUnit(dataset_to_keep_as_is,dataset_with_units_to_convert,c("MT","MTfromNO"))
  #plot_catches_remaining_in_number<-function_plot_timeSeries_byCatchUnit(dataset_to_keep_as_is,dataset_with_units_to_convert,c("NO"))
  
  df_input<-data.frame(df_input)
  
  #  return(list(df_input,dataset_to_keep_as_is,dataset_with_units_to_convert,stats_converted_data,plot_catches_by_catchunit,plot_catches_remaining_in_number))
  
  return(list(df=df_input)) #,stats=stats_data_not_mapped))
  
}


