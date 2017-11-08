#' @name spatial_curation_downgrade_resolution
#' @aliases spatial_curation_downgrade_resolution
#' @title Disaggregate gridded data
#' @description This function disaggregates the data of a df_input on a grid with resolution equal to \code{resolution}. Data with resolutions superior to \code{resolution} will be disaggregated on the corresponding \code{resolution} quadrant by dividing the catch equally on the overlappings \code{resolution} quadrants. Data with resolutions inferior to \code{resolution} will not be aggregated. To aggregate data with resolutions inferior to \code{resolution}, use the function \code{spatial_curation_upgrade_resolution} 
#' 
#' @param con a wrapper of rpostgresql connection (connection to a database)
#' @param df_input data.frame of fact. The data frame must contain at least one column "geographical_identifier" with CWP grid codification.
#' @param resolution integer. Resolution to reach (in degrees). Currently, works with 1 and 5. 
#'
#' @return a list with 2 objects: 
#'  \itemize{
#'  \item{"df": }{\code{df_input} where data have been disaggregated}
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
#' # Disaggregate data with resolutions superior to 5° on the corresponding 5° resolution grid, by dividing the catch equally on the overlappings \code{resolution} quadrants:
#' df_disaggregated_5_deg<-spatial_curation_downgrade_resolution(con,df_input=df,resolution=5)
#' 
#' head(df_disaggregated_5_deg$df)
#' 
#' # Some stats (percentage of the data that have been disaggregated)
#' df_disaggregated_5_deg$stats
#' 
#' # In this df_input, data that were defined on a grid superior to 5° were disaggregated on the corresponding 5° resolution, other data remained at their source resolution (e.g. data at 1° resolution will not be aggregated on the corresponding 5° grid)
#' # if you want the data inferior to 5° to be aggregated on the 5° grid, use : 
#' # spatial_curation_downgrade_resolution(con,df_input=df_disaggregated_5_deg,resolution=5)
#' 
#' @export

spatial_curation_downgrade_resolution<-function(con,df_input,resolution){
  
  #colnames(df_input)[which(grepl("unit",colnames(df_input)))]<-"unit"
  columns_dataset_input<-colnames(df_input)
  
  # get id_area of Sardara corresponding to codesource_area
  dataset_distinct_area<-unique(df_input$geographic_identifier)  
  
  dataset_distinct_area<-paste(unique(dataset_distinct_area), collapse = '\',\'')
  
  # get distinct of areas in 5°/1° in the df_input
  cwp_grid_data_with_resolution_to_downgrade<-dbGetQuery(con,paste0("SELECT codesource_area as geographic_identifier,left(cwp_grid.gridcode_cwp,7) as gridcode_cwp FROM area.areas_with_geom 
                                                                    JOIN area.cwp_grid
                                                                    USING (geom)
                                                                    WHERE codesource_area IN ('",dataset_distinct_area,"')
                                                                    and spatial_resolution=",resolution))
  
  # df_input that is already 5deg resolution, with the cwp code associated
  dataset_to_leave_as_so<-inner_join(df_input,cwp_grid_data_with_resolution_to_downgrade,by="geographic_identifier")
  dataset_to_leave_as_so$geographic_identifier<-dataset_to_leave_as_so$gridcode_cwp
  dataset_to_leave_as_so$gridcode_cwp<-NULL
  
  
  # get distinct of areas not in 5°/1° in the df_input (either > or < to 5°/1°)
  
  area_changeresolution<-setdiff(unique(df_input$geographic_identifier),cwp_grid_data_with_resolution_to_downgrade$geographic_identifier)
  area_changeresolution<-paste(unique(area_changeresolution), collapse = '\',\'')
  
  # get areas to project data that are superior to 5°/1°
  
  if (resolution==1){
    a1.size_grid=5
    a2.size_grid="(1,2,6,7,8,9)"
  } else if (resolution==5){
    a1.size_grid=6
    a2.size_grid="(1,2,7,8,9)"
  }
  
  areas_to_project_data_to_disaggregate<-dbGetQuery(con,paste0( 
    "SELECT
    left(a2.gridcode_cwp,7) as input_geographic_identifier,
    left(a1.gridcode_cwp,7) as geographic_identifier_project
    from
    area.cwp_grid a1,
    area.cwp_grid a2
    where
    a2.gridcode_cwp IN ( '",area_changeresolution,"') and
    a1.size_grid=",a1.size_grid," and a2.size_grid IN ",a2.size_grid," and 
    ST_Within(a1.geom, a2.geom)
    UNION
    SELECT
    a2.code_area as input_geographic_identifier,
    left(a1.gridcode_cwp,7) as geographic_identifier_project
    from
    area.cwp_grid a1,
    area.irregular_areas_task2_iotc a2
    where
    a2.code_area IN ( '",area_changeresolution,"') and
    a1.size_grid=",a1.size_grid," and 
    ST_Within(a1.geom, a2.geom)"))
  
  
  if (nrow(areas_to_project_data_to_disaggregate)>0){
    # get df_input to disaggregate
    dataset_to_disaggregate<-inner_join(df_input,areas_to_project_data_to_disaggregate,by = c("geographic_identifier"="input_geographic_identifier"))
    # get the number of strata on which to reallocate data
    wxc2<-dataset_to_disaggregate %>%
      group_by_(.dots=columns_dataset_input) %>%
      summarise(number=n())
    
    dataset_to_disaggregate<-left_join(dataset_to_disaggregate,wxc2,by=setdiff(columns_dataset_input,"geographic_identifier"))
    
    dataset_to_disaggregate$value_realloc<-dataset_to_disaggregate$value/dataset_to_disaggregate$number
    dataset_to_disaggregate$value<-dataset_to_disaggregate$value_realloc
    dataset_to_disaggregate$geographic_identifier<-dataset_to_disaggregate$geographic_identifier_project
    dataset_to_disaggregate<-dataset_to_disaggregate[,columns_dataset_input]
    
    dataset_to_disaggregate<-data.frame(dataset_to_disaggregate)
  } else {dataset_to_disaggregate=NULL}
  
  
  
  # get df_input that is defined on resolution inferior to 5° ( = neither defined on 5° nor on resolutions superior to 5°)
  
  if (resolution==5){
    
    areas_to_project_data_to_aggregate<-
      dbGetQuery(con,paste0( 
        "SELECT
        left(a2.gridcode_cwp,7) as input_geographic_identifier,
        left(a1.gridcode_cwp,7) as geographic_identifier_project
        from
        area.cwp_grid a1,
        area.cwp_grid a2
        where
        a2.gridcode_cwp IN ('",area_changeresolution,"') and
        a1.size_grid = 6 and a2.size_grid = 5 and 
        ST_Within(a2.geom, a1.geom)
        UNION
        SELECT
        a2.code_area as input_geographic_identifier,
        left(a1.gridcode_cwp,7) as geographic_identifier_project
        from
        area.cwp_grid a1,
        area.irregular_areas_task2_iotc a2
        where
        a2.code_area IN ('",area_changeresolution,"') and
        a1.size_grid=6 and 
        ST_Within(a2.geom, a1.geom)"))

  
  if (nrow(areas_to_project_data_to_aggregate)>0){
    
    areas_inf_to_resolution_to_disaggregate<-unique(areas_to_project_data_to_aggregate$input_geographic_identifier)
    dataset_areas_inf_to_resolution_to_disaggregate<-df_input %>% filter (geographic_identifier %in% areas_inf_to_resolution_to_disaggregate)
  } else {dataset_areas_inf_to_resolution_to_disaggregate=NULL}
  
  } else if (resolution==1) { dataset_areas_inf_to_resolution_to_disaggregate=NULL  }
  
  # merge data that was already in 5°/1°, data that has been downgraded to 5°/1° and data that has resolution inferior to 5°/1°
  dataset_final_disaggregated_on_resolution_to_disaggregate<-rbind(data.frame(dataset_to_leave_as_so),data.frame(dataset_to_disaggregate),data.frame(dataset_areas_inf_to_resolution_to_disaggregate))
  
  
  if (!is.null(dataset_to_disaggregate)){
    # some stats on the data that are reallocated
    sum_fact_to_reallocate <- dataset_to_disaggregate %>% 
      group_by(unit) %>% 
      summarise(value_reallocate = sum(value))
    
    
    sum_whole_dataset <- df_input %>% 
      group_by(unit) %>% 
      summarise(value = sum(value))
    
    stats_reallocated_data<-left_join(sum_whole_dataset,sum_fact_to_reallocate)
    stats_reallocated_data$percentage_reallocated<-stats_reallocated_data$value_reallocate/stats_reallocated_data$value*100
  } else {stats_reallocated_data=NULL}
  
  
  return(list(df=dataset_final_disaggregated_on_resolution_to_disaggregate,stats=stats_reallocated_data))
  
}
  