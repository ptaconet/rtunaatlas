#' @name rasterize_geo_timeseries
#' @aliases rasterize_geo_timeseries
#' @title
#' @description
#'
#' @usage
#'  
#' @param firstdate
#' @param finaldate
#' @param resolution
#' @param unit
#'
#' @return
#' 
#' @details 
#' 
#' 
#' @family process data
#' 
#' @examples
#' 
#' 
#' @author Chloé Dalleau, \email{dalleau.chloe@@hotmail.fr}
#' 


rasterize_geo_timeseries <- function(df_input,
                                         grid_spatial_resolution,
                                         intersection_layer=NULL,
                                         data_crs="+init=epsg:4326 +proj=longlat +datum=WGS84" ,
                                         temporal_resolution,
                                         temporal_resolution_unit,
                                         first_date=NULL,
                                         final_date=NULL,
                                         aggregate_data=TRUE,
                                         spatial_association_method="equaldistribution",
                                         buffer=1)
  {
  
}
  
  
  
  
  