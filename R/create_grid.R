#' @name create_grid
#' @aliases create_grid
#' @title Creates a continious grid 
#' @description Create a continious (regular) geospatial grid
#' @export
#'
#' @usage create_grid(latmin,latmax,lonmin,lonmax,resolution,crs,centred)
#'  
#' @param latmin real smallest latitude for the spatial grid in degree (range: -90 to 90)
#' @param latmax real biggest latitude wanted for the spatial grid in degree (range: -90 to 90)
#' @param lonmin real smallest longitude for the spatial grid in degree (range: -180 to 180)
#' @param lonmax real biggest longitude wanted for the spatial grid in degree (range: -180 to 180)
#' @param resolution real spatial resolution of the grid in degree
#' @param crs character string a character string of projection arguments; the arguments must be entered exactly as in the PROJ.4 documentation.
#' @param centred boolean TRUE for a grid centred on zero. default is TRUE
#'
#' @return A SpatialPolygon object (package : sp) based on input parameters and composed by square polygons.
#' 
#' @details 
#' 
#' The default CRS is EPSG 4326: "+init=epsg:4326 +proj=longlat +datum=WGS84"
#' 
#' @family process data
#' 
#' @examples
#' 
#' grid <- create_grid(latmin=-90,latmax=90,lonmin=-180,lonmax=80,resolution=1,centred=T)
#' 
#' @import sp
#' 
#' @author Chloé Dalleau, \email{dalleau.chloe@@hotmail.fr}; modified by Paul Taconet, \email{paul.taconet@@ird.fr}
#'   

  create_grid <- function(latmin=-90,latmax=90,lonmin=-180,lonmax=180,resolution,crs="+init=epsg:4326 +proj=longlat +datum=WGS84",centred=T){
  
  ### Creates polygons grid
  ## if lat and lon are equal to [-90,90] and [-180,180], it create error in spTransform (infinite value) in the code part : "Select polygones which are covered by trajectories"
    if (latmin== -90){latmin=latmin+resolution}
    if (latmax== 90){latmax=latmax-resolution}
    if (lonmin== -180){lonmin=lonmin+resolution}
    if (lonmax== 180){lonmax=lonmax-resolution}
    ## treatment
    cellsdimlat <-  ceiling(abs(latmax-latmin)/resolution)
    cellsdimlon <-  ceiling(abs((lonmax-lonmin)/resolution))
    ## create a centered grid 
    if (centred==T){
      smallest_lon <- ceiling(lonmin/resolution)*resolution + resolution/2
      smallest_lat <- ceiling(latmin/resolution)*resolution + resolution/2
    } else {
      smallest_lon <- lonmin + resolution/2
      smallest_lat <- latmin + resolution/2
    }
    grid = GridTopology(cellcentre.offset=c(smallest_lon,smallest_lat), cellsize=c(resolution,resolution), cells.dim=c(cellsdimlon,cellsdimlat))
    sp_zone <- as.SpatialPolygons.GridTopology(grid, proj4string = CRS(crs))
    
    return(sp_zone)
  }
