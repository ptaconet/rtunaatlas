#' @name raise_incomplete_dataset_to_total_dataset
#' @aliases raise_incomplete_dataset_to_total_dataset
#' @title Raise incomplete dataset to total dataset
#' @description Provided two datasets, one for which the information information is usually stratified is space and/or time but the associated measure represents only part of the reality ("incomplete" dataset), and one for wich the the information is usually less stratified (i.e. more aggregated) but the measure represents the reality ("total" dataset): 
#' this function raises the "incomplete" dataset to the "total" dataset, using raising factors. Raising factors are the proportion of data that of the "total" dataset that are available in the "incomplete" dataset (see section "Details" and function \link[rtunaatlas]{raise_get_rf} for additional information).
#' @export
#' 
#' @param df_input_incomplete data.frame "incomplete", to raise. Must have a set of dimensions (columns) + a value column
#' @param df_input_total data.frame "total". Must have a set of dimensions (columns) + a value column
#' @param df_rf data.frame of raising factors. Ouput of the function \link[rtunaatlas]{raise_get_rf}
#' @param x_raising_dimensions vector of dimensions (i.e. dimensions that compose the stratum) to use for the computation of the raising factors. The dimensions must be available in both input data.frames.
#' @param threshold_rf numeric from 0 to 100. If the raising factor for a stratum is not above this treshold, the data will be removed from the dataset.
#' 
#' @return a list with three objects:
#' \itemize{
#'  \item{"df": }{data.frame. Representing \code{df_input_incomplete} raised to \code{df_input_total}}
#'  \item{"stats": }{data.frame. Information regarding the raising process (see Details for additional information).}
#' }
#'
#'
#' @details
#'
#' It is possible to understand the concept of raising with the following example.
#' 
#' Let us take a first dataset \code{df_input_incomplete} representing the catches for the stratum defined by Flag=AUS, Year=1992, Gear=LL, Species=YFT , stratified by 5° quadrants and 1 month time resolution (typical catch-and-effort dataset):
#' 
#'  \tabular{rrrrrrr}{
#' flag	\tab time_start	\tab time_end	  \tab geographic_identifier	\tab gear	\tab species	\tab value\cr
#' AUS	\tab 1992-02-01	\tab 1992-03-01	\tab 235140	\tab LL	\tab YFT	\tab 0.05 \cr
#' AUS	\tab 1992-06-01	\tab 1992-07-01	\tab 230125	\tab LL	\tab YFT	\tab 0.42\cr
#' AUS	\tab 1992-07-01	\tab 1992-08-01	\tab 235140	\tab LL	\tab YFT	\tab 1.05\cr
#' AUS	\tab 1992-07-01	\tab 1992-08-01	\tab 240140	\tab LL	\tab YFT	\tab 0.15\cr
#' AUS	\tab 1992-08-01	\tab 1992-09-01	\tab 240140	\tab LL	\tab YFT	\tab 0.61\cr
#' AUS	\tab 1992-11-01	\tab 1992-12-01	\tab 230120	\tab LL	\tab YFT	\tab 0.15\cr
#' AUS	\tab 1992-11-01	\tab 1992-12-01	\tab 230125	\tab LL	\tab YFT	\tab 0.15\cr
#' AUS	\tab 1992-11-01	\tab 1992-12-01	\tab 235115	\tab LL	\tab YFT	\tab 1.65\cr
#' AUS	\tab 1992-11-01	\tab 1992-12-01	\tab 235135	\tab LL	\tab YFT	\tab 5\cr
#' AUS	\tab 1992-12-01	\tab 1993-01-01	\tab 235110	\tab LL	\tab YFT	\tab 0.08\cr
#' AUS	\tab 1992-12-01	\tab 1993-01-01	\tab 240140	\tab LL	\tab YFT	\tab 0.12
#'  }
#'
#' The second dataset \code{df_input_total} represents the catches for the same stratum (Flag=AUS, Year=1992, Gear=LL, Species=YFT), however the data is more aggregated: the tuna RFMO area of competence and 1 year time resolution (typical nominal catch dataset):
#'
#'  \tabular{rrrrrrr}{
#' flag	\tab time_start	\tab time_end	  \tab geographic_identifier	\tab gear	\tab species	\tab value\cr
#' AUS	\tab 1992-02-01	\tab 1993-01-01	\tab IOTC	\tab LL	\tab YFT	\tab 14
#' }
#' 
#' The information in both datasets is the same, however, in the dataset \code{df_input_incomplete}, only a sample of the catches has been reported. The catches reported in \code{df_input_total} are the real catches that happened in this stratum.
#' 
#' Raising the dataset \code{df_input_incomplete} to the dataset \code{df_input_total} with \code{x_raising_dimensions} = c("flag","year","gear","species") means:
#' \itemize{
#' \item{1. Calculating S2: }{Summing the data from \code{df_input_incomplete} by year (i.e. sum of the months) for each stratum (i.e. combination of {flag, year, gear, species)}). In the example, S2 = 9.43
#' At this stage, we get the following table:
#'  \tabular{rrrrrrrr}{
#' flag	\tab time_start	\tab time_end	  \tab geographic_identifier	\tab gear	\tab species	\tab value \tab sum_incomplete_catch \cr
#' AUS	\tab 1992-02-01	\tab 1992-03-01	\tab 235140	\tab LL	\tab YFT	\tab 0.05 \tab 9.43\cr
#' AUS	\tab 1992-06-01	\tab 1992-07-01	\tab 230125	\tab LL	\tab YFT	\tab 0.42 \tab 9.43 \cr
#' AUS	\tab 1992-07-01	\tab 1992-08-01	\tab 235140	\tab LL	\tab YFT	\tab 1.05 \tab 9.43 \cr
#' AUS	\tab 1992-07-01	\tab 1992-08-01	\tab 240140	\tab LL	\tab YFT	\tab 0.15 \tab 9.43 \cr
#' AUS	\tab 1992-08-01	\tab 1992-09-01	\tab 240140	\tab LL	\tab YFT	\tab 0.61 \tab 9.43 \cr
#' AUS	\tab 1992-11-01	\tab 1992-12-01	\tab 230120	\tab LL	\tab YFT	\tab 0.15 \tab 9.43 \cr
#' AUS	\tab 1992-11-01	\tab 1992-12-01	\tab 230125	\tab LL	\tab YFT	\tab 0.15 \tab 9.43 \cr
#' AUS	\tab 1992-11-01	\tab 1992-12-01	\tab 235115	\tab LL	\tab YFT	\tab 1.65 \tab 9.43 \cr
#' AUS	\tab 1992-11-01	\tab 1992-12-01	\tab 235135	\tab LL	\tab YFT	\tab 5 \tab 9.43 \cr
#' AUS	\tab 1992-12-01	\tab 1993-01-01	\tab 235110	\tab LL	\tab YFT	\tab 0.08 \tab 9.43 \cr
#' AUS	\tab 1992-12-01	\tab 1993-01-01	\tab 240140	\tab LL	\tab YFT	\tab 0.12 \tab 9.43 
#'  }
#'
#' }
#' \item{2.Calculating S1: }{Summing the data from \code{df_input_total} by year for each stratum (i.e. combination of {flag, year, gear, species)}). In the example, S1 = 14}
#' \item{3. Calculating the raising factor RF for each stratum: }{RF = S1/S2. This is done with the function \link[rtunaatlas]{raise_get_rf}. RF is the proportion of total catch that are available in the catch-and-effort. In the example, RF = 14/9.43 = 1.48. This means that  (1 / RF * 100 = ) 67 % of the total catches are available in the catch-and-effort file for this stratum.
#' At this stage, we get the following table:
#'  \tabular{rrrrrrrrrr}{
#' flag	\tab time_start	\tab time_end	  \tab geographic_identifier	\tab gear	\tab species	\tab value \tab sum_incomplete_catch \tab sum_total_catch \tab RF \cr
#' AUS	\tab 1992-02-01	\tab 1992-03-01	\tab 235140	\tab LL	\tab YFT	\tab 0.05 \tab 9.43 \tab 14 \tab 1.48 \cr
#' AUS	\tab 1992-06-01	\tab 1992-07-01	\tab 230125	\tab LL	\tab YFT	\tab 0.42 \tab 9.43 \tab 14 \tab 1.48\cr
#' AUS	\tab 1992-07-01	\tab 1992-08-01	\tab 235140	\tab LL	\tab YFT	\tab 1.05 \tab 9.43 \tab 14 \tab 1.48\cr
#' AUS	\tab 1992-07-01	\tab 1992-08-01	\tab 240140	\tab LL	\tab YFT	\tab 0.15 \tab 9.43 \tab 14 \tab 1.48\cr
#' AUS	\tab 1992-08-01	\tab 1992-09-01	\tab 240140	\tab LL	\tab YFT	\tab 0.61 \tab 9.43 \tab 14 \tab 1.48\cr
#' AUS	\tab 1992-11-01	\tab 1992-12-01	\tab 230120	\tab LL	\tab YFT	\tab 0.15 \tab 9.43 \tab 14 \tabb 1.48\cr
#' AUS	\tab 1992-11-01	\tab 1992-12-01	\tab 230125	\tab LL	\tab YFT	\tab 0.15 \tab 9.43 \tab 14 \tab 1.48\cr
#' AUS	\tab 1992-11-01	\tab 1992-12-01	\tab 235115	\tab LL	\tab YFT	\tab 1.65 \tab 9.43 \tab 14 \tab 1.48\cr
#' AUS	\tab 1992-11-01	\tab 1992-12-01	\tab 235135	\tab LL	\tab YFT	\tab 5 \tab 9.43 \tab 14 \tab 1.48\cr
#' AUS	\tab 1992-12-01	\tab 1993-01-01	\tab 235110	\tab LL	\tab YFT	\tab 0.08 \tab 9.43 \tab 14 \tab 1.48\cr
#' AUS	\tab 1992-12-01	\tab 1993-01-01	\tab 240140	\tab LL	\tab YFT	\tab 0.12 \tab 9.43 \tab 14 \tab 1.48
#'  }
#'
#' }
#' \item{4. Raising the \code{df_input_incomplete} dataset: }{Multiply each value from the catch-and-effort dataset by the RF associated.}
#' }
#' 
#' After the raising, the new dataset for catch-and-effort is the following:
#'  \tabular{rrrrrrr}{
#' flag	\tab time_start	\tab time_end	  \tab geographic_identifier	\tab gear	\tab species	\tab value\cr
#' AUS	\tab 1992-02-01	\tab 1992-03-01	\tab 235140	\tab LL	\tab YFT	\tab 0.074 \cr
#' AUS	\tab 1992-06-01	\tab 1992-07-01	\tab 230125	\tab LL	\tab YFT	\tab 0.6216\cr
#' AUS	\tab 1992-07-01	\tab 1992-08-01	\tab 235140	\tab LL	\tab YFT	\tab 1.55\cr
#' AUS	\tab 1992-07-01	\tab 1992-08-01	\tab 240140	\tab LL	\tab YFT	\tab 0.222\cr
#' AUS	\tab 1992-08-01	\tab 1992-09-01	\tab 240140	\tab LL	\tab YFT	\tab 0.9028\cr
#' AUS	\tab 1992-11-01	\tab 1992-12-01	\tab 230120	\tab LL	\tab YFT	\tab 0.222\cr
#' AUS	\tab 1992-11-01	\tab 1992-12-01	\tab 230125	\tab LL	\tab YFT	\tab 0.222\cr
#' AUS	\tab 1992-11-01	\tab 1992-12-01	\tab 235115	\tab LL	\tab YFT	\tab 2.442\cr
#' AUS	\tab 1992-11-01	\tab 1992-12-01	\tab 235135	\tab LL	\tab YFT	\tab 7.4\cr
#' AUS	\tab 1992-12-01	\tab 1993-01-01	\tab 235110	\tab LL	\tab YFT	\tab 0.1184\cr
#' AUS	\tab 1992-12-01	\tab 1993-01-01	\tab 240140	\tab LL	\tab YFT	\tab 0.1776
#'  }
#' 
#' The parameter \code{threshold_rf} allows to remove some data based on the raising factor. In the example, setting threshold_rf = 10 would remove all the data for which RF = 10 ; in other words, any data for which less that 10 per cent of the total catches are available in the catch-and-effort for the stratum set with \code{x_raising_dimensions}. Setting NULL will imply that not data are filtered.
#'
#' The object "stats" of the output list is a data.frame whose columns are the followings. In the list below "sum" is the sum of the data, i.e. the sum of the column 'value' of the dataset:
#'  \itemize{
#'  \item{sum_df_total: }{ sum extracted from \code{df_input_total} }
#' \item{sum_df_incomplete_before_raising: }{ sum extracted from \code{df_input_incomplete} before the raising}
#' \item{sum_df_incomplete_after_raising: }{ sum extracted from \code{df_input_incomplete} after the raising}
#' \item{sum_df_incomplete_do_not_exist_in_df_total: }{ sum extracted from \code{df_input_incomplete} that cannot be raised because the strata exists in \code{df_input_incomplete} but does not exist in \code{df_input_total} (ie sum of the strata exists in \code{df_input_incomplete} but not in \code{df_input_total})The raised value for these strata is equal to the raw value.}
#' \item{sum_df_total_do_not_exist_in_df_incomplete: }{ sum extracted from \code{df_input_total} that do not exist in \code{df_input_incomplete} (ie sum of the strata exists in \code{df_input_total} but not in \code{df_input_incomplete})}
#' \item{perc_df_incomplete_over_df_total_before_raising: }{ percentage of the data of \code{df_input_total} that were available in \code{df_input_incomplete} before the raising}
#' \item{perc_df_incomplete_over_df_total_after_raising: }{ percentage of the data of \code{df_input_total} that are available in \code{df_input_incomplete} after the raising}
#' \item{perc_df_incomplete_do_not_exist_in_df_total: }{ percentage of the data coming from \code{df_input_incomplete} for which there is no correspondance in \code{df_input_total} (i.e perc. of the strata that exist in \code{df_input_incomplete} but do not exist in \code{df_input_total})}
#' \item{perc_df_total_do_not_exist_in_df_incomplete: }{ percentage of the data coming from \code{df_input_total} for which there is no correspondance in \code{df_input_total} (i.e perc. of the strata that exist in \code{df_input_total} but do not exist in \code{df_input_incomplete})}
#' }
#' 
#' Usually, for catches: \code{x_raising_dimensions} = c("gear","flag","species","year","source_authority","unit")  and for efforts: c("gear","flag","year","source_authority","unit"). 
#' 
#' @examples
#' 
#' # Connect to Sardara DB
#' con <- db_connection_sardara_world()
#'
#' # Extract IOTC georeferenced catch time series of catches from Sardara DB
#' ind_catch_tunaatlasird_level1<-extract_dataset(con,list_metadata_datasets(con,dataset_name="indian_ocean_catch_1952_11_01_2016_01_01_tunaatlasIRD_level1"))
#' head(ind_catch_tunaatlasird_level1)
#'
#' # Extract IOTC total (nominal) catch time series from Sardara DB
#' ind_nominal_catch_tunaatlasiotc_level0<-extract_dataset(con,list_metadata_datasets(con,dataset_name="indian_ocean_nominal_catch_1950_01_01_2015_01_01_tunaatlasIOTC_2017_level0"))
#' head(ind_nominal_catch_tunaatlasiotc_level0)
#'
#' ## Raise georeferenced catch to total catch. Raise by {gear, flag, species, year, source_authority, unit}
#' 
#' # First calculate the dataset of raising factor
#' df_rf<-raise_get_rf(
#' df_input_incomplete=ind_catch_tunaatlasird_level1,
#' df_input_total=ind_nominal_catch_tunaatlasiotc_level0,
#' x_raising_dimensions=c("gear","flag","species","year","source_authority","unit")
#' )
#' 
#' # Then raise
#' ind_catch_tunaatlasird_level2<-raise_incomplete_dataset_to_total_dataset(
#' df_input_incomplete=ind_catch_tunaatlasird_level1,
#' df_input_total=ind_nominal_catch_tunaatlasiotc_level0,
#' df_rf=df_rf,
#' x_raising_dimensions=c("gear","flag","species","year","source_authority","unit"),
#' threshold_rf=NULL)
#' 
#' head(ind_catch_tunaatlasird_level2$df)
#' 
#' # get statistics on the raising process
#' head(ind_catch_tunaatlasird_level2$stats)
#' 
#' # get raising factor dataset
#' head(ind_catch_tunaatlasird_level2$df_rf)
#'
#' dbDisconnect(con)
#' 


raise_incomplete_dataset_to_total_dataset<-function(df_input_incomplete,
                                 df_input_total,
                                 df_rf,
                                 x_raising_dimensions,
                                 threshold_rf=NULL){
  
  
  # check if columns of x_raising_dimensions exist in the datasets
  if (length(setdiff(x_raising_dimensions,colnames(df_input_incomplete)))!=0 | length(setdiff(x_raising_dimensions,colnames(df_input_total)))!=0){stop("one of the dataframes as input does not have the dimensions set in the dimensions to consider for the raising")}
  
  colnames_input_dataset<-colnames(df_input_incomplete)
  
  sum_df_total <- df_input_total %>% 
    group_by(unit) %>%
    summarise(sum_df_total=sum(value))
  
  sum_df_incomplete_before_raising <- df_input_incomplete %>% 
    group_by(unit) %>%
    summarise(sum_df_incomplete_before_raising=sum(value))
  
  df_input_incomplete$year<-as.numeric(substr(df_input_incomplete$time_start, 0, 4))
  df_input_total$year<-as.numeric(substr(df_input_total$time_start, 0, 4))
  
  
  
  # rf>1 means that the catches in the georef dataset are inferior to the catches in the total dataset
  # rf<1 means that the catches in the georef dataset are greater to the catches in the total dataset
  
  
  # data that exist in total catches but do not exist in georef catches
  sum_df_total_do_not_exist_in_df_incomplete<- left_join(df_input_total,df_rf) %>% 
    filter(!is.na(sum_value_df_input_incomplete)) %>%
    group_by(unit) %>%
    summarize(sum_df_total_do_not_exist_in_df_incomplete=sum(value))
  
  
  ## Remove NAs and infinite from the raising factors. NAs are when data exist in partial dataset but do not exist in total dataset, or reversely
  index.na.rf<-which(is.na(df_rf$rf))
  df_rf <- df_rf[-index.na.rf, ]
  df_rf <- df_rf[!is.infinite(df_rf$rf),]
  
  # apply the raising factors
  df_input_incomplete<-left_join(df_input_incomplete,df_rf,by=x_raising_dimensions)
  
  sum_df_incomplete_do_not_exist_in_df_total<-df_input_incomplete %>%
    filter(is.na(sum_value_df_input_total)) %>%
    group_by(unit) %>%
    summarize(sum_df_incomplete_do_not_exist_in_df_total=sum(value))
  
  
  # When there is no raising factor for a given stratum, we keep the original value (ie value not raised)
  df_input_incomplete$value_raised<-df_input_incomplete[,"value"]
  
  # When there is a raising factor for a given stratum, we multiply the original value by the raising factor to get the raised value
  index.rfNotNa<-which(!is.na(df_input_incomplete[,"rf"]))
  
  df_input_incomplete$value_raised[index.rfNotNa]<-df_input_incomplete[index.rfNotNa,"value"]*df_input_incomplete[index.rfNotNa,"rf"]
  
  
  if (!is.null(threshold_rf)){
    ## Remove the catches for which the rf threshold is above the threshold set by the user
    # For instance, if RF=10, 10 % of the total catches are available in the CE file for this stratum. The data is therefore considered as bad quality and is removed
    index.threshold<-which(df_input_incomplete$rf>threshold_rf)
    df_input_incomplete <- df_input_incomplete[-index.threshold, ]
  }
  
  
  # df_georef_sup_total = strata for which sum of georef catches > sum of total catch (wich is anormal)
  #df_georef_sup_total<-df_input_incomplete[which(df_input_incomplete$rf < 0.95),]
  
  # df_georef_inf_total = strata for which sum of georef catches < sum of total catch (wich is normal)
  #df_georef_inf_total<-df_input_incomplete[which(df_input_incomplete$rf > 1.1),]
  
  # Keep the good columns for the dataset raised to return
  dataset_to_return<-df_input_incomplete
  
  dataset_to_return$value<-NULL
  colnames(dataset_to_return)[which(names(dataset_to_return) == "value_raised")] <- "value"
  dataset_to_return<-dataset_to_return[colnames_input_dataset]
  
  sum_df_incomplete_after_raising <- dataset_to_return %>% 
    group_by(unit) %>%
    summarise(sum_df_incomplete_after_raising=sum(value))
  
  
  stats <- sum_df_total %>%
    full_join(sum_df_incomplete_before_raising) %>%
    full_join(sum_df_incomplete_after_raising) %>%
    full_join(sum_df_incomplete_do_not_exist_in_df_total) %>%
    full_join(sum_df_total_do_not_exist_in_df_incomplete)
  
  stats[is.na(stats)] <- 0
  
  stats$perc_df_total_do_not_exist_in_df_incomplete <- stats$sum_df_total_do_not_exist_in_df_incomplete/stats$sum_df_total*100
  stats$perc_df_incomplete_do_not_exist_in_df_total <- stats$sum_df_incomplete_do_not_exist_in_df_total/stats$sum_df_total*100
  stats$perc_df_incomplete_over_df_total_after_raising <- stats$sum_df_incomplete_after_raising/stats$sum_df_total*100
  stats$perc_df_incomplete_over_df_total_before_raising <- stats$sum_df_incomplete_before_raising/stats$sum_df_total*100
  
  
  return(list(df=dataset_to_return,stats=stats))
  
}