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
  
  ### aggragation parameters
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
    
    # ## error :
    # Error in RGEOSBinPredFunc(spgeom1, spgeom2, byid, func) :
    # rgeos_binpredfunc_prepared: maximum returned dense matrix size exceeded
    # separate data in some parts
    
    ## create a "fake" column to separate the dataset into 100 parts
    n <- 100
    nr <- nrow(dataset_calendar)
    dataset_calendar<-split(dataset_calendar, rep(1:ceiling(nr/n), each=n, length.out=nr))
    
    for (i in 1:length(dataset_calendar)){
      dataset_calendar[[i]]$splt<-i
    }
    dataset_calendar<-data.frame(Reduce(rbind, dataset_calendar))
    
    unique_id = unique(dataset_calendar$splt)
    
    for (id_subdata in unique_id){
      
      dataset <- subset(dataset_calendar,dataset_calendar$splt==id_subdata)
      
      ######################## Create spatial point
      sp_points <- SpatialPointsDataFrame(dataset[,c("lon","lat")], dataset,proj4string = CRS(data_crs))
      
      ######################## Select polygones which are covered by geolocalisation
      ### select polygons covered by geolocalisation
      join_poly <- overGeomGeom(intersection_layer,gEnvelope(sp_points))
      id_polygons <- which(!is.na(join_poly))
      polygons <- intersection_layer[id_polygons,]
      
      # if polygons=EEZ, intersection between data and polygons can be NULL
      if (length(polygons)>0){
        ######################## Intersection between points and polygons
        intersection <- gIntersection(sp_points,polygons,byid=T, drop_lower_td = T)
        
        # NOTE : id of a spatial polygon
        # intersect_buffer[i]@polygons[[1]]@ID
        
        ## if polygons=ZEE, data extent can intersect polygons but not real geolocation data
        if(!is.null(intersection)){
          ######################## Create data.table without data aggregation 
          ### Initialisation
          ### extract dimensions data ID, other dimensions and variable
          sp_dim_data <- str_replace(rownames(intersection@coords), " " , fixed("."))
          sp_dim_data <- str_split_fixed(sp_dim_data, fixed("."),2)
          sp_dim_data <- data.table(sp_dim_data)
          colnames(sp_dim_data) <- c("id_point","id_geom")
          
          all_dim_data <- data.table(id_point=rownames(sp_points@data),sp_points@data)#, sp_points@coords)
          
          ### Merge spatial data ID and dimensions
          sp_data <- merge(sp_dim_data,all_dim_data, by="id_point")
          
          ### extract WKT or label from polygons
          id_poly <- sapply(polygons@polygons,slot, "ID")
          if(!is.null(intersection_layer_type)){ # case irregular polygons
            wkt <- writeWKT(spTransform(gEnvelope(polygons, byid=TRUE, id = NULL),CRS(data_crs)), byid = T)
            poly <- data.table(id_poly,polygons@data$geographic_identifier,wkt)
          } else { # case grid
            sp_transform_poly <- spTransform(polygons,CRS(data_crs))
            wkt <- writeWKT(sp_transform_poly, byid = T)
            if (spatial_association_method=="cwp"){ 
              dist_0_centr_poly <- data.table(spDistsN1(gCentroid(sp_transform_poly, byid=TRUE),c(0,0),longlat=T))
              coord_centroid_geom <- data.table(gCentroid(sp_transform_poly, byid=TRUE)@coords)
              wkt <- cbind(wkt,dist_0_centr_poly,coord_centroid_geom)
            }
            poly <- data.table(id_poly,wkt,wkt)
          }
          
          names(poly) <- c("id_geom","geographic_identifier", "geom_wkt",
                           if(spatial_association_method=="cwp"){c("dist_0_centr_poly","lon_cent_geom", "lat_cent_geom")})
          
          ### Merge spatial data, dimensions and variable
          output_data_detail_id_with_duplicated <- merge(sp_data,poly, by="id_geom")
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
          
          ## Store non duplicate data
          output_data_detail <- bind_rows(output_data_detail,output_data_detail_id )
          
          ### Select the traitement for points on polygon boundary
          for ( id in id_duplicated) {
            subset_duplicated_data <- subset(duplicated_data, duplicated_data$id_point == id)
            switch (spatial_association_method,
                    "random" = {
                      id_select=data.table(rand=runif(dim(subset_duplicated_data)[1], min = 0, max = 1), keep.rownames = T)
                      select_data <- subset_duplicated_data[id_select$rand==max(id_select),] 
                    },
                    "equaldistribution" = {
                      size <- dim(subset_duplicated_data)[1]
                      select_data <- subset_duplicated_data
                      select_data$value <- select_data[,"value", with=F]/size
                    }, 
                    "cwp" = {
                      if (is.null(intersection_layer_type)){
                        ## reste à tester lat ==0 et/ou lon ==0
                        lat <- subset_duplicated_data$lat[1]
                        lon <- subset_duplicated_data$lon[1]
                        if (lat==0 ){
                          select_data <- subset_duplicated_data[abs(subset_duplicated_data$lon_cent_geom)==max(abs(subset_duplicated_data$lon_cent_geom)) & subset_duplicated_data$lat_cent_geom>0 ,]
                        } else if (lon==0 ){
                          select_data <- subset_duplicated_data[abs(subset_duplicated_data$lat_cent_geom)==max(abs(subset_duplicated_data$lat_cent_geom)) & subset_duplicated_data$lon_cent_geom>0 ,]
                        } else if (lon==0 & lat ==0){
                          select_data <- subset_duplicated_data[subset_duplicated_data$lat_cent_geom>0 & subset_duplicated_data$lon_cent_geom>0,]
                        } else {
                          select_data <- subset_duplicated_data[subset_duplicated_data$dist_0_centr_poly==max(subset_duplicated_data$dist_0_centr_poly)]
                        }
                      } else {
                        stop("CWP can't be used on irregular spatial zone. Please to select random or equaldistribtion methods.")
                      }
                    }
            )
            
            ## Store the data selected by the method
            output_data_detail <- bind_rows(output_data_detail,select_data )
          }
          
          compteur = compteur +1
          cat(paste0("\n", compteur, " % at ", Sys.time(), " ... "))
          
          ### bbox
          if (compteur>1){
            bbox <- gUnion(bbox,polygons)
            bbox <- gEnvelope(bbox)
          } else {
            bbox <- gEnvelope(polygons)
          }
          
        } else {
          compteur = compteur +1
          cat("No intersection between the subset ",id_subdata," and spatial zone")
        }
        
      } else {
        compteur = compteur +1
        cat("No intersection between the subset ",id_subdata," and spatial zone")
      }
      
    }
    
    cat("\n Spatial treatment OK ")
    
    
    ## Select the wanted dimenions
    list_remove_dim <- c(if(spatial_association_method=="cwp"){c("dist_0_centr_poly","lon_cent_geom","lat_cent_geom")},c("id_point","id_geom"))
    if (dim(output_data_detail)[1]<1){
      warnings("The intersection between the data and the intersection layer is empty")
    } else {
      output_data_detail <- output_data_detail[,-list_remove_dim, with=F]
    }
    
    output_data_detail$splt<-NULL
    
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
