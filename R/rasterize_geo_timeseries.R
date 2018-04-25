#' @name rasterize_geo_timeseries
#' @aliases rasterize_geo_timeseries
#' @title Aggregate a geospatial time series on polygons
#' @description
#' @export
#'
#' @usage
#'  
#' @param df_input
#' @param intersection_layer sp polygon
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
#' @import data.table dplyr sp rgeos rgdal stringr
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
  calendar <- data.table(calendar)
  ### delete duplicate data
  dataset_table <- unique(dataset_table)
  # sort data by date
  dataset_table <- setorder(dataset_table, date)
  
  
  # Determine data type (point or line (i.e. trajectory))
  if ("id_trajectory" %in% colnames(df_input)){
    type="line"
    data_crs_proj <- "+init=epsg:3395"
    intersection_layer <- spTransform(intersection_layer , CRS(data_crs_proj) )
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
  
  if ("data" %in% slotNames(intersection_layer)){
    intersection_layer_type="irregular"
  } else {
    intersection_layer_type=NULL
  }
  
  ### Initialisation of final data
  output_data_detail <- NULL
  compteur=0
  
  
  if (type=="point"){
    
    ######################## Create spatial point composed of the unique combination of coordinates
    sp_points<-unique(dataset_calendar[,c("lon","lat")])
    
    n <- 100
    nr <- nrow(sp_points)
    sp_points<-split(sp_points, rep(1:n, each=floor(nr/n), length.out=nr))
    
    for (i in 1:length(sp_points)){
      sp_points[[i]]$splt<-i
    }
    sp_points<-data.frame(Reduce(rbind, sp_points))
    
    unique_id = unique(sp_points$splt)
    
    sp_points_on_intersect_layer<-NULL
    
    for (id_subdata in unique_id){
      
      sp_points_sub<-subset(sp_points,sp_points$splt==id_subdata)
      sp_points_sub<-SpatialPoints(sp_points_sub,proj4string = CRS(data_crs))
      
      ######################## Select the polygons that are covered by the points
      join_poly <- overGeomGeom(intersection_layer,gEnvelope(sp_points_sub))
      id_polygons <- which(!is.na(join_poly))
      polygons <- intersection_layer[id_polygons,]
      
      if (length(polygons)>0){
        
        # make intersection
        intersection <- gIntersection(sp_points_sub,polygons,byid=T, drop_lower_td = T)
        
        intersection<-data.frame(intersection)
        intersection$id_geom<-row.names(intersection)
        intersection$id_geom <- str_replace(intersection$id_geom, " " , fixed("."))
        intersection$id_point <- str_split_fixed(intersection$id_geom, fixed("."),2)[,1]
        intersection$id_geom <- str_split_fixed(intersection$id_geom, fixed("."),2)[,2]
        colnames(intersection) <- c("lon","lat","id_geom","id_point")
        
        
        # get wkt of intersection layer
        id_poly <- sapply(polygons@polygons,slot, "ID")
        if(!is.null(intersection_layer_type)){ # case irregular polygons
          if (spatial_association_method=="cwp"){
            stop("CWP reallocation method can't be used on non-gridded areas. Please select random or equaldistribtion methods.")
          } else {
            wkt <- writeWKT(spTransform(gEnvelope(polygons, byid=TRUE, id = NULL),CRS(data_crs)), byid = T)
            poly <- data.table(id_poly,as.character(polygons@data$geographic_identifier),wkt)
          }
        } else { # case grid
          sp_transform_poly <- spTransform(polygons,CRS(data_crs))
          wkt <- writeWKT(sp_transform_poly, byid = T)
          poly <- data.table(id_poly,wkt,wkt)
          if (spatial_association_method=="cwp"){ 
            dist_0_centr_poly <- data.table(spDistsN1(gCentroid(sp_transform_poly, byid=TRUE),c(0,0),longlat=T))
            coord_centroid_geom <- data.table(gCentroid(sp_transform_poly, byid=TRUE)@coords)
            poly <- data.table(id_poly,wkt,wkt,dist_0_centr_poly,coord_centroid_geom)
          }
        }
        
        names(poly) <- c("id_geom","geographic_identifier", "geom_wkt",
                         if(spatial_association_method=="cwp"){c("dist_0_centr_poly","lon_cent_geom", "lat_cent_geom")})
        
        output_data_detail_id_with_duplicated <- merge(intersection,poly, by="id_geom")
        
        output_data_detail_id_with_duplicated$id_point <- as.numeric(output_data_detail_id_with_duplicated$id_point)
        
        ### Extract the duplicated geolocalisation (points on polygon boundary)
        table_number_rep <- table(as.factor(output_data_detail_id_with_duplicated$id_point))
        ## Extract id
        # id_duplicated <- as.numeric(which(table_number_rep>1))
        id_duplicated <- names(which(table_number_rep>1))
        # id_unique <- as.numeric(which(table_number_rep==1))
        id_unique <- names(which(table_number_rep==1))
        
        ## Extract data
        duplicated_data <- output_data_detail_id_with_duplicated[which(output_data_detail_id_with_duplicated$id_point %in% id_duplicated),]
        output_data_detail_id <- output_data_detail_id_with_duplicated[which(output_data_detail_id_with_duplicated$id_point %in% id_unique),]
        
        if(spatial_association_method=="equaldistribution" & nrow(output_data_detail_id)>0){
          output_data_detail_id$size<-1
        }
        
        ### Select the traitement for points on polygon boundary
        select_data_duplicated<-NULL
        if (length(id_duplicated)>0){
          for ( id in id_duplicated) {
            subset_duplicated_data <- subset(duplicated_data, duplicated_data$id_point == id)
            switch (spatial_association_method,
                    "random" = {
                      id_select=data.table(rand=runif(dim(subset_duplicated_data)[1], min = 0, max = 1), keep.rownames = T)
                      select_data <- subset_duplicated_data[which(id_select$rand==max(id_select)),] 
                    },
                    "equaldistribution" = {
                      size <- dim(subset_duplicated_data)[1]
                      select_data <- subset_duplicated_data
                      select_data$size <- size ## value is calculated afterwards
                    }, 
                    "cwp" = {
                      if (is.null(intersection_layer_type)){
                        ## reste à tester lat ==0 et/ou lon ==0
                        lat <- subset_duplicated_data$lat[1]
                        lon <- subset_duplicated_data$lon[1]
                        if (lat==0 ){
                          select_data <- subset_duplicated_data[which(abs(subset_duplicated_data$lon_cent_geom)==max(abs(subset_duplicated_data$lon_cent_geom)) & subset_duplicated_data$lat_cent_geom>0 ),]
                        } else if (lon==0 ){
                          select_data <- subset_duplicated_data[which(abs(subset_duplicated_data$lat_cent_geom)==max(abs(subset_duplicated_data$lat_cent_geom)) & subset_duplicated_data$lon_cent_geom>0) ,]
                        } else if (lon==0 & lat ==0){
                          select_data <- subset_duplicated_data[which(subset_duplicated_data$lat_cent_geom>0 & subset_duplicated_data$lon_cent_geom>0),]
                        } else {
                          select_data <- subset_duplicated_data[which(subset_duplicated_data$dist_0_centr_poly==max(subset_duplicated_data$dist_0_centr_poly)),]
                        }
                      } else {
                        stop("CWP can't be used on irregular spatial zone. Please to select random or equaldistribtion methods.")
                      }
                    }
            )
            
            select_data_duplicated<-bind_rows(select_data_duplicated,select_data)
          }
        } 
        ## Store the data selected by the method
        output_data_detail_id <- bind_rows(output_data_detail_id,select_data_duplicated)
      }
      else { }
      
      compteur = compteur +1
      cat(paste0("\n", compteur, " % at ", Sys.time(), " ... "))
      
      sp_points_on_intersect_layer<-bind_rows(sp_points_on_intersect_layer,output_data_detail_id)
      
    }
    
    dataset_calendar<-left_join(dataset_calendar,sp_points_on_intersect_layer,by=c("lat","lon"))
    ## hereunder "no_geo_intersection" are the data that do not spatially intersect any polygon of intersection_layer
    dataset_calendar <- dataset_calendar %>% mutate(geographic_identifier = if_else(is.na(geographic_identifier), "no_geo_intersection", geographic_identifier))
    dataset_calendar <- dataset_calendar %>% mutate(geom_wkt = if_else(is.na(geom_wkt), "no_geo_intersection", geom_wkt))
    
    if (spatial_association_method=="equaldistribution"){
      dataset_calendar <- dataset_calendar %>% mutate(size = if_else(is.na(size), 1, size))
      dataset_calendar$value<-dataset_calendar$value/dataset_calendar$size
    }
    
    output_data_detail<-dataset_calendar[,c(colnames(df_input),"time_start","time_end","geographic_identifier","geom_wkt")]
    
    output_data_detail$geographic_identifier<-as.character(output_data_detail$geographic_identifier)
    output_data_detail$time_start<-as.character(output_data_detail$time_start)
    output_data_detail$time_end<-as.character(output_data_detail$time_end)
    
    
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
      output_data_agg <- output_data_detail %>% select_(.dots=c(list_dimensions_output_modify,"value")) %>% group_by_(.dots=list_dimensions_output_modify) %>% dplyr::summarise_all (funs(n(),sum,mean,sd,min,max)) %>% setNames( c(list_dimensions_output_modify, "n_value","sum_value","mean_value","sd_value","min_value","max_value")) 
      output_data <- data.frame(output_data_agg) 
      cat("\n Aggregation of data OK ")
      
    } else {
      ## store data without aggregation but associate by time and by space
      output_data <- data.frame(output_data_detail)
    }
    
    
    ######## LINES 
  } else if (type=="line"){
    
    buffer=as.numeric(buffer)
    
    unique_id <- unique(dataset_calendar$id_object)
    
    for (id_subdata in unique_id){
      
      ######################## Split data in a list according to dimensions
      ### Extract data for the part id_part
      dataset <- subset(dataset_calendar,dataset_calendar$id_object==id_subdata)
      
      ### If the object have more than 1 geolocalisation, the trajectories are calculated
      if (dim(dataset)[1]>1){
        ### Split data
        # split_data_by_time <- split(dataset, dataset[,list_dim, with=FALSE], drop= T) 
        split_data_by_time <- split(dataset, dataset[,c(list_dimensions_output,"time_start","time_end"),with=FALSE], drop= T) 
        
        ### Select data with more than 1 point
        list_data_by_time <- split_data_by_time[which(sapply(split_data_by_time,dim)[1,]>1)]
        
        if (length(list_data_by_time)>0){
          
          ######################## Create lines trajectories
          ### Create the geometry "line" for each object : Line-class will be used in Lines-class
          trajectories <- list()
          coord <- lapply(list_data_by_time, "[", j=c("lon","lat"), with=F)
          #coord <- lapply(list_data_by_time, "[", c("lon","lat"))
          list_line <- lapply(coord, Line)
          ### create the geometry "lines" for each object: Lines-class will be used in SpatialLines
          ### the loup is obligatory to create the IDs
          for (i in 1:length(list_line)){
            trajectories[[i]] <- Lines(list_line[i], ID=names(list_line[i])) 
          }
          ### Convert Lines to SpatialLines
          sp_trajectories <- SpatialLines(trajectories, proj4string = CRS(data_crs))
          ### release space memory
          rm(trajectories)
          sp_trajectories <- spTransform(sp_trajectories , CRS( data_crs_proj ) )
          ### Creat a buffer arround the lines for surface calculation
          ### ERROR memory space : alternative solution is a loop
          buffer_sp_traj <- gBuffer(sp_trajectories, capStyle = "ROUND", joinStyle = "ROUND", byid = T, width = buffer*1000)
          #    cat(paste0("\n buffer step (",length(sp_trajectories),") :"))
          #   count=0
          #  for (i in 1:length(sp_trajectories)){
          #   traj <- gSimplify(sp_trajectories[i],tol=0.001)
          #  if (gIsValid(traj)==T){
          #   buffer <- gBuffer(sp_trajectories[i], capStyle = "ROUND", joinStyle = "ROUND", width = buffer*1000)
          #  buffer <- gBuffer(buffer, width = 0)
          # count=count+1
          #         if (count==1){
          #            buffer_sp_traj <- buffer
          #       } else {
          #          buffer_sp_traj <- union(buffer_sp_traj,buffer)
          #     }
          #     ### release space memory
          #   #rm(buffer)
          #  cat(paste0(count," "))
          #     }
          #  }
          
          
          # ### test : plot trajectoire
          # # id = unique_idobject[4]
          # plot(sp_trajectories, axes=T)
          # sp_points <- SpatialPointsDataFrame(dataset[,c("lon","lat")], dataset, proj4string = CRS(data_crs))
          # points(sp_points, col="red")
          # row <- which(sp_points@data["lat"]>(20))
          # sp_points@data[(row-10):(row+10),]
          # dataset_sql <- data.table(dataset_sql)
          # dataset_sql[which(id_object==19 & id_traj==14 & lat<20),]
          
          
          ######################## Select polygones which are covered by trajectories
          ### select polygons covered by trajectories
          join_poly <- overGeomGeom(intersection_layer,buffer_sp_traj)
          id_polygons <- which(!is.na(join_poly))
          polygons <- intersection_layer[id_polygons,]
          
          
          
          # if polygons=ZEE, the intersection between intersection_layer and the trajectories can be NULL
          if (length(polygons)>0){
            ######################## Clip trajectories and buffers by poly
            ### Intersection between trajectories and polygons
            intersect_traj <- gIntersection(sp_trajectories,polygons,byid=T, drop_lower_td = T)
            ### Intersection between buffer and polygons
            intersect_buffer <- gIntersection(buffer_sp_traj,polygons, byid = T,  drop_lower_td = T)
            
            
            # NOTE : id of a spatial polygon
            # intersect_buffer[i]@polygons[[1]]@ID
            
            ######################## Calculate variables : distance and buffer surface
            ### Calculate the distance by objects and by polygons
            ## Note : keep if, if geolocatisations are fixed gIntersection can create NULL data
            if(!is.null(intersect_traj)){
              distance <- as.data.table(round(gLength(intersect_traj,byid=T)/1000,3), keep.rownames=T)
              colnames(distance) <- c("id","distance_value")
            }
            ### Calculate the surface exploited by objects and by polygons
            surface <- as.data.table(round(gArea(intersect_buffer,byid=T)/1000000,3), keep.rownames=T)
            colnames(surface) <- c("id","surface_value")
            
            
            ######################## Merge all the calculated variables
            var_cal <- merge(distance,surface, by="id",all= T)
            var_cal[is.na(var_cal)] <- 0
            
            
            ######################## Create output data without data aggregation 
            ### Initialisation
            ### extract dimensions data
            dim_data <- str_replace(var_cal$id, " " , fixed("."))
            dim_data <- str_split_fixed(dim_data, fixed("."),length(c(list_dimensions_output,"time_start","time_end"))+1)
            dim_data <- data.table(dim_data)
            colnames(dim_data) <- c(list_dimensions_output,"time_start","time_end","id_geom")
            
            ### extract WKT or label from polygons
            id_poly <- sapply(polygons@polygons,slot, "ID")
            if(is.null(intersection_layer_type)){
              wkt <- writeWKT(spTransform(polygons,CRS(data_crs)), byid = T)
              poly <- data.table(id_poly,wkt,wkt)
            } else {
              wkt <- writeWKT(spTransform(gEnvelope(polygons, byid=TRUE, id = NULL),CRS(data_crs)), byid = T)
              poly <- data.table(id_poly,polygons@data$geographic_identifier,wkt)
            }
            names(poly) <- c("id_geom","geographic_identifier", "geom_wkt")            
            
            ### Bind data and calculate variables
            output_data <-  as.data.table(cbind(dim_data,var_cal[,-"id"]))
            
            ### Merge data and polygons
            output_data <- merge(poly,output_data, by="id_geom")
            
            
            compteur = compteur +1
            cat(paste0("\n",compteur," object(s) trajectories created over "))
            
            ### Store data for the loop
            output_data_detail <- data.table(rbind(output_data_detail,output_data))
            
            ### release space memory
            rm(polygons)
            rm(output_data)
            
          }
          
        }
        
      }
      
    }
    
    
    
    ### Calulate normalize data
    output_data_detail$ndistance_value <- sapply(output_data_detail[,"distance_value"], function(x) (x-min(x))/(max(x)-min(x)))
    output_data_detail$nsurface_value <- sapply(output_data_detail[,"surface_value"], function(x) (x-min(x))/(max(x)-min(x)))
    
    
    ######################## Aggregation of data
    if (aggregate_data==T){
      
      cat("\n Aggregation of data in progress ... ")
      
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
      ### Store data without agregation
      output_data <- output_data_detail[,-"id_geom"]
    }
    
  }
  
  return(output_data)
  
}