#' @name rasterize_geo_timeseries
#' @aliases rasterize_geo_timeseries
#' @title Aggregate a geospatial time series on polygons
#' @description
#' @export
#'
#' @usage
#'  
#' @param df_input 
#' @param intersection_layer sf polygon
#' @param calendar 
#' @param data_crs
#' @param aggregate_data
#' @param spatial_association_method
#' @param buffer real only in case df_input is a trajectory
#' 
#' @return
#' 
#' @details 
#' 
#' df_input must have columns: date, lat, lon
#' if df_input is a trajectory, must have 2 additional columns: id_object and id_trajectory
#' 
#' calendar must have columns: time_start and time_end
#' 
#' intersection_layer: type sp SpatialPolygonsDataFrame
#' 
#' The input data that do not intersect any polygon of the intersection_layer are marked as 'no_geo_intersection' in the column 'geographic_identifier' of the output dataset
#' 
#' @family process data
#' 
#' @examples
#' 
#' # Create a continous calendar of 1 month time step
#' calendar<-create_calendar("2000-01-01","2020-01-01",1,"month")
#' # Create a ĝrid of 1° resolution
#' grid <- create_grid(latmin=-90,latmax=90,lonmin=-180,lonmax=80,resolution=1,centred=T)
#' 
#' # Rasterize
#' 
#' 
#' @import data.table dplyr sf stringr
#' 
#' @author Chloé Dalleau, \email{dalleau.chloe@@hotmail.fr}; modified by Paul Taconet, \email{paul.taconet@@ird.fr}
#' 


rasterize_geo_timeseries <- function(df_input,
                                     intersection_layer,
                                     calendar,
                                     data_crs="+init=epsg:4326 +proj=longlat +datum=WGS84" ,
                                     aggregate_data=TRUE,
                                     spatial_association_method="equaldistribution",
                                     buffer=10){
  
  ######################## Initialisation
  ### Convert data in a table (processing faster)
  dataset_table<- as.data.table(df_input)
  calendar <- as.data.table(calendar)
  ### delete duplicate data
  dataset_table <- unique(dataset_table)
  # sort data by date
  dataset_table <- setorder(dataset_table, date)
  
  
  # Determine data type (point or line (i.e. trajectory))
  if ("id_trajectory" %in% colnames(df_input)){
    type="line"
    data_crs_proj <- "+init=epsg:3395"
    if (!("geographic_identifier" %in% colnames(intersection_layer))){ # if intersection layer is a grid
      intersection_layer$geom_wkt=st_as_text(intersection_layer$geometry)
    }
    intersection_layer <- intersection_layer %>% st_transform(3395)
  } else {
    type="point"
  }
  
  ### aggregation parameters
  list_dimensions_output = setdiff(colnames(df_input),c("value","date","lat","lon"))
  #var_aggregated_value = "value"
  #fact_name = agg_parameters$fact_name           ### A voir la réelle utilité pour fads et effort. Pourrait être effectué en dehors de la fonction?
  #if (fact_name =="fad"){number_days = agg_parameters$calculation_of_number_days}
  
  ### Join dataset and calendar
  ## initialisation
  dataset_faketime_s_e <- data.table(dataset_table, time_start=as.Date(dataset_table$date), time_end=as.Date(dataset_table$date))
  setkey(calendar, time_start, time_end) # is used in the function : foverlaps
  ## select the index calendar for each "time" of data
  index <- foverlaps(x=dataset_faketime_s_e,y=calendar, by.x = c("time_start","time_end"),by.y = c("time_start","time_end"), type="within", nomatch=0, which = T)
  ### join data and calendar
  dataset_calendar <- data.table(dataset_table[index$xid,], calendar[index$yid,])
  ### list of used period in calendar
  data_calendar <-  calendar[unique(index$yid),]
  
  
  # get intersection layer type
  
  if ("geographic_identifier" %in% colnames(intersection_layer)){
    intersection_layer$geom_wkt<-NA
    intersection_layer_type="irregular_polygon"
  } else {
    if (type=="point"){
    intersection_layer$geom_wkt<-st_as_text(intersection_layer$geometry)
     }
    intersection_layer$geographic_identifier<-intersection_layer$geom_wkt
    intersection_layer_type="grid"
  }
  
  
  if (type=="point"){
    
    ######################## Create spatial point composed of the unique combination of coordinates
    sf_points<-unique(dataset_calendar[,c("lon","lat")])
    
    sf_points = st_as_sf(sf_points, coords = c("lon", "lat"), crs = 4326, agr = "constant")
    
    
    points_on_poly<-st_join(sf_points,intersection_layer, join = st_intersects,left = TRUE)
    
    ## For the points that are NA, we add 0.01° to lon and lat if the points are in a grid
    
    points_on_poly_na<-points_on_poly %>% filter(is.na(geom_wkt))
    
    if (intersection_layer_type=="grid" & nrow(points_on_poly_na)>0){
      
      points_on_poly_not_na<-points_on_poly %>% filter(!(is.na(geom_wkt)))
      points_on_poly_not_na$lon=st_coordinates(points_on_poly_not_na)[,1]
      points_on_poly_not_na$lat=st_coordinates(points_on_poly_not_na)[,2]
      points_on_poly_not_na$lon_new=points_on_poly_not_na$lon
      points_on_poly_not_na$lat_new=points_on_poly_not_na$lat
      points_on_poly_not_na<-unique(points_on_poly_not_na[c("lon","lat","lon_new","lat_new")])
      
      points_on_poly_na$lon=st_coordinates(points_on_poly_na)[,1]
      points_on_poly_na$lat=st_coordinates(points_on_poly_na)[,2]
      points_on_poly_na$lon_new=points_on_poly_na$lon+0.01
      points_on_poly_na$lat_new=points_on_poly_na$lat+0.01
      points_on_poly_na<-unique(points_on_poly_na[c("lon","lat","lon_new","lat_new")])
      
      point_on_poly_new_coord<-rbind(points_on_poly_not_na,points_on_poly_na)
      
      dataset_calendar<-left_join(dataset_calendar,point_on_poly_new_coord)
      dataset_calendar$lat<-dataset_calendar$lat_new
      dataset_calendar$lon<-dataset_calendar$lon_new
      
      dataset_calendar$lat_new<-NULL
      dataset_calendar$lon_new<-NULL
      dataset_calendar$geometry<-NULL
      
      sf_points<-unique(dataset_calendar[,c("lon","lat")])
      
      sf_points = st_as_sf(sf_points, coords = c("lon", "lat"), crs = 4326, agr = "constant")
      
      points_on_poly<-st_join(sf_points,intersection_layer, join = st_intersects,left = TRUE)
      
    }
    
    points_on_poly$lon=st_coordinates(points_on_poly)[,1]
    points_on_poly$lat=st_coordinates(points_on_poly)[,2]
    
    points_on_poly$geometry=NULL
    
    
    # Get duplicated data (i.e. data in various polygons). On équi répartit les valeurs dans les différents carré
    
    points_on_poly_number_of_points <- points_on_poly %>% group_by(lat,lon) %>% summarise(n=n())
    
    points_on_poly <- left_join(points_on_poly,points_on_poly_number_of_points)
    
    dataset_calendar<-left_join(dataset_calendar,points_on_poly)
    
    
    if (intersection_layer_type=="grid"){
      dataset_calendar <- dataset_calendar %>% filter (!(is.na(geographic_identifier)))
    }
    
    
    dataset_calendar$value=dataset_calendar$value/dataset_calendar$n
    
    dataset_calendar<-dataset_calendar[,c(colnames(df_input),"time_start","time_end","geographic_identifier","geom_wkt")]
    
    dataset_calendar$geographic_identifier<-as.character(dataset_calendar$geographic_identifier)
    dataset_calendar$time_start<-as.character(dataset_calendar$time_start)
    dataset_calendar$time_end<-as.character(dataset_calendar$time_end)
    
    
    ## Special case for facts effort and FAD
    #if (fact_name=="effort"){
    ## calculation : boats effort
    # output_data_detail <- number_boat(output_data_detail)
    #} else if (fact_name=="fad"){
    ## calculation number of days and number of fad by space and by time
    # output_data_detail <- number_fad_and_days(output_data_detail, number_days)
    ## Data are already aggregated in the function number_fad_and_days
    #}
    
    
    ######################## Aggregation of data
    if (aggregate_data==T){ #& fact_name!="fad"){
      
      cat("\n Aggregation of data ... ")
      # list of dimensions
      list_dimensions_output_modify <- c(list_dimensions_output,"time_start","time_end","geographic_identifier","geom_wkt")
      output_data_agg <- dataset_calendar %>% select_(.dots=c(list_dimensions_output_modify,"value")) %>% group_by_(.dots=list_dimensions_output_modify) %>% dplyr::summarise_all (funs(n(),sum,mean,sd,min,max)) %>% setNames( c(list_dimensions_output_modify, "n_value","sum_value","mean_value","sd_value","min_value","max_value")) 
      output_data <- data.frame(output_data_agg) 
      cat("\n Aggregation of data OK ")
      
    } else {
      ## store data without aggregation but associate by time and by space
      output_data <- data.frame(dataset_calendar)
    }
    
    
    ######## LINES 
  } else if (type=="line"){
    
    buffer=as.numeric(buffer)
    
    # Select data with more than one point
    traj2 <- dataset_calendar %>% group_by_(.dots=c(list_dimensions_output,"time_start","time_end")) %>% summarize(n=n())
    
    dataset_calendar <- left_join(dataset_calendar,traj2) %>% filter (n>1) %>% select(-n)
    
    cat(paste0("\n",Sys.time(),": Creating sf points from lon and lat coordinates ... "))
    traj_sf<-st_as_sf(dataset_calendar, coords = c("lon", "lat"), crs = 4326, agr = "constant")
    
    traj_sf <- traj_sf %>% arrange_(.dots=c("id_object","id_trajectory","date",setdiff(list_dimensions_output,c("id_object","id_trajectory"))))
    
    cat(paste0("\n",Sys.time(),": Creating trajectories as sf linestrings ... "))
    lines <- traj_sf  %>% group_by_(.dots=c(list_dimensions_output,"time_start","time_end")) %>% summarize(a = 1,do_union=FALSE) %>% st_cast("LINESTRING") %>% select(-a) %>% st_transform(3395) %>% arrange_(.dots=c("id_object","id_trajectory",setdiff(list_dimensions_output,c("id_object","id_trajectory")),"time_start"))
    
    cat(paste0("\n",Sys.time(),": Creating the buffer around the trajectories ... "))
    lines_buffer <- lines  %>% st_buffer(dist = buffer*1000, nQuadSegs = 30)
    
    cat(paste0("\n",Sys.time(),": Creating the spatial intersection with the intersection layer for the calculation of the distances intersected... "))
    int_dist = st_intersection(lines, intersection_layer)
    
    cat(paste0("\n",Sys.time(),": Creating the spatial intersection with the intersection layer for the calculation of the surfaces intersected... "))
    int_surf = st_intersection(lines_buffer, intersection_layer)
    
    cat(paste0("\n",Sys.time(),": Measuring length and surface of each line segment... "))
    int_dist$distance_value = st_length(int_dist) 
    int_surf$surface_value = st_area(int_surf)
    
    int_dist=as.data.frame(int_dist)
    int_surf=as.data.frame(int_surf) 
    
    int_dist$distance_value=round(as.numeric(int_dist$distance_value)/1000,3)
    int_surf$surface_value=round(as.numeric(int_surf$surface_value)/1000000,3)
    
    int_dist$geometry=NULL
    int_surf$geometry=NULL
    
    
    output_data_detail<-left_join(int_dist,int_surf)
    
    ### Calulate normalize data
    output_data_detail$ndistance_value <- (output_data_detail$distance_value-min(output_data_detail$distance_value))/(max(output_data_detail$distance_value)-min(output_data_detail$distance_value))
    output_data_detail$nsurface_value <- (output_data_detail$surface_value-min(output_data_detail$surface_value))/(max(output_data_detail$surface_value)-min(output_data_detail$surface_value))

    
    output_data_detail$geographic_identifier<-as.character(output_data_detail$geographic_identifier)
    output_data_detail$time_start<-as.character(output_data_detail$time_start)
    output_data_detail$time_end<-as.character(output_data_detail$time_end)
    
    ######################## Aggregation of data
    if (aggregate_data==T){
      
      cat("\n Aggregating data ... ")
      
      ######################## Aggregation of data
      
      ### list of dimensions for the output
      #dimensions <-c("geom_wkt", if(is.null(spatial_zone)==FALSE){"geographic_identifier_label"}, "time_start", "time_end", list_dim_output)
      list_dimensions_output_modify <- c("geographic_identifier","geom_wkt","time_start","time_end",setdiff(list_dimensions_output,c("id_object","id_trajectory")))
      #list_dimensions_output_modify <- c(list_dimensions_output,"time_start","time_end","geographic_identifier","geom_wkt")
      
      ### Aggregation (data.table package)
      output_data_agg <- output_data_detail %>% group_by_(.dots=list_dimensions_output_modify) %>% summarise(distance_value=round(sum(distance_value),3),surface_value=round(sum(surface_value),3), number_of_trajectories_value=n())# summarise(distance=round(sum(distance),3),surface=round(sum(surface),3) )
      output_data_agg <- data.table(output_data_agg)
      ### Calculate normalize data
      ndistance <- (output_data_agg[,"distance_value"]-min(output_data_agg[,"distance_value"]))/(max(output_data_agg[,"distance_value"])-min(output_data_agg[,"distance_value"]))
      colnames(ndistance)<-"ndistance_value"
      nsurface<- (output_data_agg[,"surface_value"]-min(output_data_agg[,"surface_value"]))/(max(output_data_agg[,"surface_value"])-min(output_data_agg[,"surface_value"]))
      colnames(nsurface)<-"nsurface_value"
      ## Bind all data
      
      output_data <- data.table(output_data_agg[,list_dimensions_output_modify, with=FALSE], output_data_agg[,"distance_value"],
                                ndistance, output_data_agg[,"surface_value"],
                                nsurface, output_data_agg[,"number_of_trajectories_value"] 
      )
      
      output_data<-data.frame(output_data)
      cat("ok \n")
      
    } else {
      
    }
    
  }
  
  return(output_data)
  
}
