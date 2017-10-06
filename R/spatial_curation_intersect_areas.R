#' @name spatial_curation_intersect_areas
#' @aliases spatial_curation_intersect_areas
#' @title Perform spatial intersection with a geospatial layer
#' @description This function performs the spatial intersection between a georeferenced dataset (\it{input layer}) and a georeferenced code list stored in Sardara database (\it{intersection layer}). It provides the features of the \it{intersection layer} that intersect the features of the \it{input layer}, along with the proportion of the area of the \it{input layer} feature being intersected by the \it{intersection layer} feature. 
#' @export
#'
#'
#' @usage 
#' spatial_curation_intersect_areas(con,df_input,df_spatial_code_list_name,intersection_spatial_code_list_name)
#'    
#' @param con a wrapper of rpostgresql connection (connection to a database)
#' @param df_input data.frame of fact. The dataframe must have at least a column 'geographic_identifier'. The code list used for the column 'geographic_identifier' must be one of the table of the schema 'area' of the database.
#' @param df_spatial_code_list_name string . Name of the code list used for the column 'geographic_identifier' of \code{df_input} (type polygon)
#' @param intersection_spatial_code_list_name string .  Name of the intersection layer to use (type polygon). Corresponds to the name of the table in the database.
#' 
#' @return a list with 2 objects: 
#' \itemize{
#'  \item{"df": }{\code{df_input} with additional columns:
#'  \itemize{
#'  \item{"geographic_identifier_intersection_layer": }{ Code of the feature from the intersection layer (\code{intersection_spatial_code_list_name}) that intersects the geographical identifier of \code{df_input}}
#'  \item{"proportion_intersection": }{ Proportion of the area of the \code{df_input} feature being intersected by the \it{intersection layer} feature. }
#'  }}
#'  \item{"df_input_areas_intersect_intersection_layer": }{\code{a data.frame summarizing the results of the spatial intersection. The columns are: }
#'  \itemize{
#'  \item{"geographic_identifier_source_layer": }{ Code of the feature (geographic identifier) from \code{df_input}}
#'  \item{"geographic_identifier_intersection_layer": }{ Code of the feature from (\code{intersection_spatial_code_list_name}) that intersects the feature of \code{df_input}}
#'  \item{"codelist_source_layer": }{ Name of the code list used for the column 'geographic_identifier' of \code{df_input}}
#'  \item{"codelist_intersection_layer": }{ Name of the intersection layer code list (\code{intersection_spatial_code_list_name}) }
#'  \item{"proportion_intersection": }{ Proportion of the area of the \it{input layer} feature being intersected by the \it{intersection layer} feature. }
#'  }}
#'
#' }
#' 
#'
#' @details 
#' 
#' User has to be aware that the intersection might return several rows for 1 given row of \code{df_input}. This happens when one spatial feature of the \it{input layer} intersects several features of the \it{intersection layer}. Hence, the values of the ouput dataset might not be summed directly.
#' Example:
#' 
#' 1 row of \code{df_input}:
#' \tabular{rrrrrrr}{
#' flag	\tab time_start	\tab time_end	  \tab geographic_identifier	\tab gear	\tab species	\tab value\cr
#' AUS	\tab 1992-02-01	\tab 1992-03-01	\tab 235140	\tab LL	\tab YFT	\tab 0.05 \cr
#' }
#' 
#' When executing the function \code{function spatial_curation_intersect_areas} taking as intersection layer "fao_fishing_areas", the output dataset will return 2 rows:
#' \tabular{rrrrrrrrr}{
#' flag	\tab time_start	\tab time_end	  \tab geographic_identifier	\tab gear	\tab species	\tab value \tab  geographic_identifier_intersection_layer \tab proportion_intersection \cr 
#' AUS	\tab 1992-02-01	\tab 1992-03-01	\tab 235140	\tab LL	\tab YFT	\tab 3.2 \tab F51 \tab 0.5 \cr 
#' AUS	\tab 1992-02-01	\tab 1992-03-01	\tab 235140	\tab LL	\tab YFT	\tab 3.2 \tab F57 \tab 0.5 \cr 
#' }
#' 
#' This table means that the geograpical identifier "235140" intersects two areas of the intersection layer: "F51" with 50% of the area of 235140 being intersected by F51, and "F57" with 50% of the area of 235140 being intersected by F57. However, the value of catch is 3.2 in both cases - i.e. it has not been multiplied by the proportion of area intersected.
#' 
#' @family create your own tuna atlas
#' 
#' 
#' @examples
#' 
#' con=db_connection_sardara_world()
#' 
#' # Extract a dataset
#' df <- extract_dataset(con,list_metadata_datasets(con,dataset_name="indian_ocean_effort_1970_01_01_2015_08_01_tunaatlasIOTC_2017_level0_coastal"))
#'
#' # Get spatial coding systemd used in the dataset
#' df_spatial_codingsystem <- TODO
#' 
#' # Get geospatial layers available in the database
#' spatial_code_lists_available<-list_metadata_codelists(con,dimension="area")
#' 
#' # Intersect df with continents (to check which data are located on land areas)
#' df_intersect_continents<-spatial_curation_intersect_areas(con,df_input=df,df_spatial_code_list_name=df_spatial_codingsystem,intersection_spatial_code_list_name="gshhs_world_coastlines")
#' 
#' head(df_intersect_continents$df)
#' head(df_intersect_continents$df_input_areas_intersect_intersection_layer)
#' 
#' # Intersect df with EEZ (to know what percentage of df is located on EEZs)
#' df_intersect_eez<-spatial_curation_intersect_areas(con,df_input=df,df_spatial_code_list_name,intersection_spatial_code_list_name="vliz_world_eez_v8_2014")
#' 
#' head(df_intersect_eez$df)
#' head(df_intersect_eez$df_input_areas_intersect_intersection_layer)
#' 
#' @author Paul Taconet, \email{paul.taconet@@ird.fr}
#'


spatial_curation_intersect_areas<-function(con, df_input, df_spatial_code_list_name, intersection_spatial_code_list_name){ 
  
  cat(paste0("Ignore warning message 'unrecognized PostgreSQL field type unknown'"))
  
  inputAreas_forQuery<-paste(unique(df_input$geographic_identifier), collapse = '\',\'')
  
  names_codes_labels_table_inputAreas<- dbGetQuery(con,paste0("SELECT code_column,english_label_column FROM metadata.codelists_codes_labels_column_names WHERE table_name='area.",df_spatial_code_list_name,"'"))
  names_codes_labels_table_intersectionArea<- dbGetQuery(con,paste0("SELECT code_column,english_label_column FROM metadata.codelists_codes_labels_column_names WHERE table_name='area.",intersection_spatial_code_list_name,"'"))
  
  name_geom_table_inputAreas<- dbGetQuery(con,paste0("SELECT f_geometry_column FROM geometry_columns WHERE f_table_name='",df_spatial_code_list_name,"'"))
  name_geom_table_intersectionArea<- dbGetQuery(con,paste0("SELECT f_geometry_column FROM geometry_columns WHERE f_table_name='",intersection_spatial_code_list_name,"'"))
  
  
  query_data_inland<-paste("WITH 
                           source_layer AS (
                           SELECT ",names_codes_labels_table_inputAreas$code," as code, ",names_codes_labels_table_inputAreas$english_label_column," AS label, ",name_geom_table_inputAreas$f_geometry_column," as geom FROM area.",df_spatial_code_list_name," WHERE ",names_codes_labels_table_inputAreas$code," IN ('",inputAreas_forQuery,"')
                           ),intersection_layer
                           AS (
                           SELECT ",names_codes_labels_table_intersectionArea$code," as code, ",names_codes_labels_table_intersectionArea$english_label_column," AS label, ",name_geom_table_intersectionArea$f_geometry_column," as geom FROM area.",intersection_spatial_code_list_name,"
                           )
                           SELECT 
                           source_layer.code as geographic_identifier_source_layer,
                           intersection_layer.code as geographic_identifier_intersection_layer,
                           '",df_spatial_code_list_name,"' as codelist_source_layer,
                           '",intersection_spatial_code_list_name,"' as codelist_intersection_layer,
                           ST_Area(ST_Intersection(source_layer.geom, intersection_layer.geom))/ST_Area(source_layer.geom) as proportion_source_area_intersection
                           FROM 
                           source_layer,intersection_layer
                           WHERE
                           ST_Intersects(source_layer.geom, intersection_layer.geom)"
                           ,sep="")
  
    areas_intersected<-dbGetQuery(con,query_data_inland)
  
    df_input<-left_join(df_input,areas_intersected,by= c("geographic_identifier" = "geographic_identifier_source_layer"))
    df_input$codelist_source_layer<-NULL
    df_input$codelist_intersection_layer<-NULL
    
    df_input$proportion_source_area_intersection[which(is.na(df_input$proportion_source_area_intersection))]<-0
    
    
    areas_not_intersected<-setdiff(df_input$geographic_identifier,unique(areas_intersected$geographic_identifier_source_layer))
    areas_not_intersected<-data.frame(areas_not_intersected)
    colnames(areas_not_intersected)<-"geographic_identifier_source_layer"
    areas_not_intersected$geographic_identifier_source_layer<-as.character(areas_not_intersected$geographic_identifier_source_layer)
    
    areas_not_intersected$geographic_identifier_intersection_layer<-NA
    areas_not_intersected$codelist_intersection_layer<-intersection_spatial_code_list_name
    areas_not_intersected$proportion_source_area_intersection<-0
    areas_not_intersected$codelist_source_layer<-df_spatial_code_list_name
    
    df_input_areas_intersect_intersection_layer<-rbind(areas_intersected,areas_not_intersected)
    df_input_areas_intersect_intersection_layer$geographic_identifier_intersection_layer[which(df_input_areas_intersect_intersection_layer$proportion_intersection==0)]<-NA
  
    
    
  return(list(df=df_input,df_input_areas_intersect_intersection_layer=df_input_areas_intersect_intersection_layer))
}