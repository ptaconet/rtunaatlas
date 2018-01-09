#' @name spatial_curation_upgrade_resolution
#' @aliases spatial_curation_upgrade_resolution
#' @title Aggregate gridded data
#' @description This function aggregates the data of a dataset on a grid with resolution equal to \code{resolution}. Data with resolutions inferior to \code{resolution} will be aggregated on the corresponding \code{resolution} quadrant. Data with resolutions superior to \code{resolution} will not be disaggregated. To disaggregate data with resolutions superior to \code{resolution}, use the function \code{spatial_curation_downgrade_resolution} 
#' @export 
#' 
#' @param con a wrapper of rpostgresql connection (connection to a database)
#' @param df_input data.frame of fact. The data frame must contain at least one column "geographical_identifier" with CWP grid codification or IOTC irregular areas from catch-and-effort datasets.
#' @param resolution integer. Resolution to reach (in degrees). Currently, works only with 5. 
#'
#' @return a list with 2 objects: 
#'  \itemize{
#'  \item{"df": }{\code{df_input} where data have been aggregated}
#'  \item{"stats": }{A data.frame with some information regarding the process}
#'  }
#'
#' @author Paul Taconet, IRD \email{paul.taconet@ird.fr}
#'
#' @family process data
#'
#' @examples
#' 
#' 
#' # Extract a df_input from Sardara World database
#' con=db_connection_sardara_world()
#' dataset_metadata<-list_metadata_datasets(con,dataset_name="atlantic_ocean_catch_1950_01_01_2016_01_01_tunaatlasICCAT_2017_level0__noSchool")
#' df<-extract_dataset(con,dataset_metadata)
#' 
#' # Aggregate data with resolutions inferior to 5° on the corresponding 5° resolution grid:
#' df_aggregated_5_deg<-spatial_curation_upgrade_resolution(con,df_input=df,resolution=5)
#' 
#' head(df_aggregated_5_deg$df)
#' # In this dataset, data that were defined on a grid inferior to 5° were aggregated on the corresponding 5° resolution, other data remained at their source resolution
#' # if you want the data superior to 5° to be disaggregated on the 5° grid, use : 
#' # spatial_curation_downgrade_resolution(con,df_input=df_aggregated_5_deg,resolution=5)
#' 


spatial_curation_upgrade_resolution<-function(con,df_input,resolution){
  
  #colnames(df_input)[which(grepl("unit",colnames(df_input)))]<-"unit"
  columns_df_input<-colnames(df_input)
  
  # get id_area of Sardara corresponding to codesource_area
  df_input_distinct_area<-unique(df_input$geographic_identifier)  
  
  df_input_distinct_area<-paste(unique(df_input_distinct_area), collapse = '\',\'')
  
  # get distinct of areas in 5° in the df_input
  cwp_grid_data_with_resolution_to_upgrade<-dbGetQuery(con,paste0("SELECT codesource_area as geographic_identifier,left(cwp_grid.code,7) as code FROM area.area_labels 
                                                                  JOIN area.cwp_grid
                                                                  USING (geom)
                                                                  WHERE codesource_area IN ('",df_input_distinct_area,"')
                                                                  AND tablesource_area='areas_tuna_rfmos_task2'
                                                                  and spatial_resolution='",resolution,"'"))
  
  # df_input that is already 5deg resolution, with the cwp code associated
  df_input_to_leave_as_so<-inner_join(df_input,cwp_grid_data_with_resolution_to_upgrade,by="geographic_identifier")
  df_input_to_leave_as_so$geographic_identifier<-df_input_to_leave_as_so$code
  df_input_to_leave_as_so$code<-NULL
  
  
  # get distinct of areas not in 5° in the df_input (either > or < to 5°)
  
  area_changeresolution<-setdiff(unique(df_input$geographic_identifier),cwp_grid_data_with_resolution_to_upgrade$geographic_identifier)
  area_changeresolution<-paste(unique(area_changeresolution), collapse = '\',\'')
  
  # get areas to project data that are inferior to 5°
  
  areas_to_project_data_to_aggregate<-
    dbGetQuery(con,paste0( 
      "SELECT
      left(a2.code,7) as input_geographic_identifier,
      left(a1.code,7) as geographic_identifier_project
      from
      area.cwp_grid a1,
      area.cwp_grid a2
      where
      a2.code IN ('",area_changeresolution,"') and
      a1.size_grid = '6' and a2.size_grid = '5' and 
      ST_Within(a2.geom, a1.geom)
      UNION
      SELECT
      a2.code_area as input_geographic_identifier,
      left(a1.code,7) as geographic_identifier_project
      from
      area.cwp_grid a1,
      area.irregular_areas_task2_iotc a2
      where
      a2.code_area IN ('",area_changeresolution,"') and
      a1.size_grid='6' and 
      ST_Within(a2.geom, a1.geom)"))
  
  if (nrow(areas_to_project_data_to_aggregate)>0){
    
    # get df_input to aggregate
    df_input_to_aggregate<-inner_join(df_input,areas_to_project_data_to_aggregate,by = c("geographic_identifier"="input_geographic_identifier"))
    
    # aggregate data (for data that have resolutions inferior to 5°)
    df_input_to_aggregate<-df_input_to_aggregate %>%
      #group_by(rfmo,schooltype,species,time_start,time_end,gear,flag,catchtype,catchunit,gear_group,area_project) %>%  
      group_by_(.dots=setdiff(c(columns_df_input,"geographic_identifier_project"),c("geographic_identifier","value"))) %>%
      summarise(value=sum(value))
    
    df_input_to_aggregate$geographic_identifier<-df_input_to_aggregate$geographic_identifier_project
    df_input_to_aggregate$geographic_identifier_project<-NULL
  } else {df_input_to_aggregate=NULL}
  
  # get df_input that is defined on resolution superior to 5° ( = neither defined on 5° nor on resolutions inferior to 5°)
  areas_to_project_data_to_disaggregate<-dbGetQuery(con,paste0( 
    "SELECT
    left(a2.code,7) as input_geographic_identifier,
    left(a1.code,7) as geographic_identifier_project
    from
    area.cwp_grid a1,
    area.cwp_grid a2
    where
    a2.code IN ( '",area_changeresolution,"') and
    a1.size_grid='6' and a2.size_grid IN ('1','2','7','8','9') and 
    ST_Within(a1.geom, a2.geom)
    UNION
    SELECT
    a2.code_area as input_geographic_identifier,
    left(a1.code,7) as geographic_identifier_project
    from
    area.cwp_grid a1,
    area.irregular_areas_task2_iotc a2
    where
    a2.code_area IN ( '",area_changeresolution,"') and
    a1.size_grid='6' and 
    ST_Within(a1.geom, a2.geom)"))

if (nrow(areas_to_project_data_to_disaggregate)>0){
  
areas_sup_to_resolution_to_aggregate<-unique(areas_to_project_data_to_disaggregate$input_geographic_identifier)
df_input_areas_sup_to_resolution_to_aggregate<-df_input %>% filter (geographic_identifier %in% areas_sup_to_resolution_to_aggregate)
} else {df_input_areas_sup_to_resolution_to_aggregate=NULL}

# merge data that was already in 5°, data that has been upgraded and data superior to 5°

df_input_final_aggregated_on_resolution_to_aggregate<-rbind(data.frame(df_input_to_leave_as_so),data.frame(df_input_to_aggregate),data.frame(df_input_areas_sup_to_resolution_to_aggregate))

if (!is.null(df_input_to_aggregate)){
# some stats on the data that are reallocated
sum_fact_to_reallocate <- df_input_to_aggregate %>% 
  group_by(unit) %>% 
  summarise(value_reallocate = sum(value))

sum_whole_df_input <- df_input %>% 
  group_by(unit) %>% 
  summarise(value = sum(value))

stats_reallocated_data<-left_join(sum_whole_df_input,sum_fact_to_reallocate)
stats_reallocated_data$percentage_reallocated<-stats_reallocated_data$value_reallocate/stats_reallocated_data$value*100

} else {stats_reallocated_data=NULL}

return(list(df=df_input_final_aggregated_on_resolution_to_aggregate,stats=stats_reallocated_data))
}
