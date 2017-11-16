#' @name pie_map
#' @aliases pie_map
#' @title Create a pie map 
#' @description This function ouputs a pie map from the dataset provided as input
#' @export
#' 
#' @param con a wrapper of rpostgresql connection (connection to a database)
#' @param df_input data.frame to map
#' @param dimension_group_by string. Name of the dimension that will be the classes in the pies 
#' @param df_spatial_code_list_name string. Name of the spatial coding system used in df_input
#' @param number_of_classes integer. Number of classes to visualize in the pies.
#' 
#'
#' @details
#'
#' 
#'
#' @author Paul Taconet, \email{paul.taconet@@ird.fr}
#' @family visualize data
#' @examples
#' 
#' # Connect to Sardara DB
#' con <- db_connection_sardara_world()
#'
#' # Extract IOTC (Indian Ocean) georeferenced catch time series of catches from Sardara DB, in 5° resolution
#' ind_catch_tunaatlasird_level2<-extract_dataset(con,list_metadata_datasets(con,dataset_name="indian_ocean_catch_5deg_1m_1952_11_01_2016_01_01_tunaatlasIRD_level2"))
#' head(ind_catch_tunaatlasird_level2)
#'
#' # filter the data to keep only catches on log schools in 2014:
#' ind_catch_tunaatlasird_level2 <- ind_catch_tunaatlasird_level2 %>% filter (year==2014) %>% filter (schooltype=="LS")
#' 
#' # Map the catches made on log schools in 2014 by species:
#' pie_map(con,
#'       df_input=ind_catch_tunaatlasird_level2,
#'       dimension_group_by="species",
#'       df_spatial_code_list_name="areas_tuna_rfmos_task2",
#'       number_of_classes=4 
#'      )
#'      
#' dbDisconnect(con)
#' @importFrom maps map.axes
#' @importFrom maps map
#' @importFrom plotrix floating.pie
#' @importFrom sp SpatialPointsDataFrame
#' @importFrom sp coordinates
#' @importFrom sp CRS
#' @importFrom rgeos readWKT




pie_map<-function(con,
                 df_input, # data frame with standard DSD. 
                 dimension_group_by, # String. Column name to use to aggregate. NULL if no aggregation column
                 df_spatial_code_list_name, # Name of the spatial coding system used in the input data frame. The spatial coding system must be available in the database
                 area_filter_wkt=NULL, # WKT of the area filter
                 number_of_classes #number of classes
                 ){

if(nrow(df_input)==0){stop("There is no data in your dataset")}
  
  # If multiple units, stop the function  
if("unit" %in% colnames(df_input)){
  if (length(unique(df_input$unit))>1){
    stop("There is more than one unit in your dataset. Please filter the data to keep only one unit before plotting them.")
  }
}
  
  
  inputAreas_forQuery<-paste(unique(df_input$geographic_identifier), collapse = '\',\'')
  
  db_table_name_inputAreas<-dbGetQuery(con,paste0("SELECT table_name from metadata.metadata where dataset_name='",df_spatial_code_list_name,"'"))$table_name
  names_codes_labels_table_inputAreas<- dbGetQuery(con,paste0("SELECT code_column,english_label_column FROM metadata.codelists_codes_labels_column_names WHERE table_name='",db_table_name_inputAreas,"'"))
  colname_geom<- dbGetQuery(con,paste0("SELECT f_geometry_column FROM geometry_columns WHERE 'area.'||f_table_name='",db_table_name_inputAreas,"'"))$f_geometry_column

  areas_lat_lon_wkt<-paste("SELECT ",names_codes_labels_table_inputAreas$code," as geographic_identifier, ST_x(ST_Centroid(",colname_geom,")) as lng, ST_y(ST_Centroid(",colname_geom,")) as lat, st_astext(",colname_geom,") FROM area.",df_spatial_code_list_name," WHERE ",names_codes_labels_table_inputAreas$code," IN ('",inputAreas_forQuery,"')",sep="")
  
  areas_lat_lon_wkt<-dbGetQuery(con,areas_lat_lon_wkt)
  areas_wkt<-areas_lat_lon_wkt$st_astext
  areas_lat_lon_wkt$st_astext<-NULL
  
  df_input<-merge(df_input,areas_lat_lon_wkt)
  
  ColnameLatCentroid="lat"
  ColnameLonCentroid="lng"
  
  if (!is.null(dimension_group_by)){
    df_input<-df_input %>%
  group_by_(dimension_group_by,ColnameLatCentroid,ColnameLonCentroid) %>% 
  summarise(value = sum(value))
  } else {
    df_input<-df_input %>%
  group_by_(ColnameLatCentroid,ColnameLonCentroid) %>% 
  summarise(value = sum(value))
  }

  df_input<-data.frame(df_input)
  
if (!is.null(dimension_group_by)){   # If there is a variable of aggregation (e.g. gear, species, etc.)
  
if (number_of_classes>length(unique(df_input[,dimension_group_by]))) { number_of_classes=length(unique(df_input[,dimension_group_by])) }
df_input<-fun_aggregate_keep_n_classes(df_input,number_of_classes,dimension_group_by)

number_of_variables<-length(unique(df_input[,dimension_group_by]))
name_of_variables<-unique(df_input[,dimension_group_by])

df_input <- reshape(df_input,timevar=dimension_group_by,direction="wide",idvar=c(ColnameLonCentroid,ColnameLatCentroid))

colnames(df_input) <-sub('.*\\.', '', colnames(df_input))
df_input[is.na(df_input)] <- 0

### Assign very small values to avoid 0 for floating.pie
df_input[df_input==0] <- 1e-8

if (number_of_variables==1){
  df_input$TOTAL=df_input[,name_of_variables]
} else {
  df_input$TOTAL <- apply(df_input[,name_of_variables],1,sum,na.rm=TRUE)
}

} else {
  number_of_variables<-1
  name_of_variables<-"variable"
  df_input$variable<-df_input[,Colnamevalue]
  df_input$TOTAL <- df_input[,Colnamevalue]
}

  
  
### Change the projection
#df_map.spdf = spTransform(df_map.spdf, CRS=CRS("+proj=rectangular"))

## dynamic map with Plotly
#source("1_map_dynamic.R",echo=TRUE,local=TRUE)
##

## Spatial dataframe
df_input.spdf <- SpatialPointsDataFrame(coords = df_input[,c(ColnameLonCentroid,ColnameLatCentroid)], data=df_input,proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))


### Define reference circle
#catch.ref <- 100
#radius.ref <- sqrt(catch.ref)
#df_map.spdf$radius <- sqrt(df_map.spdf$TOTAL)			#Square root of total catch
#df_map.spdf$relativeradius <- df_map.spdf$radius/radius.ref

# Area of the bounding box of the data
if (is.null(area_filter_wkt)){
bbox_data<-bbox(df_input.spdf)
} else {
bbox_data<-bbox(readWKT(area_filter_wkt, p4s=CRS("+proj=longlat"), id=as.character(1)))
}

 area_data<-sqrt((bbox_data[1,1]-bbox_data[1,2])^2+(bbox_data[2,1]-bbox_data[2,2])^2)

# Coordinates for the plot. 


x_coord_for_extension<-min(abs(bbox_data[1,1]),abs(bbox_data[1,2]))
y_coord_for_extension<-min(abs(bbox_data[2,1]),abs(bbox_data[2,2]))

fun_calc_coord_plot_bounding_box<-function(coord,percentage_extension,coord_for_extension,type){  # type={min,max}
  
  if (type=="min"){
    coord_extension<-coord-percentage_extension*coord_for_extension
  } 
  if (type=="max"){
    coord_extension<-coord+percentage_extension*coord_for_extension
  } 
  
  return(coord_extension)
  
}

xmin_plot<-fun_calc_coord_plot_bounding_box(bbox_data[1,1],0.1,x_coord_for_extension,"min")
xmax_plot<-fun_calc_coord_plot_bounding_box(bbox_data[1,2],0.1,x_coord_for_extension,"max")
ymin_plot<-fun_calc_coord_plot_bounding_box(bbox_data[2,1],0.15,y_coord_for_extension,"min")
ymax_plot<-fun_calc_coord_plot_bounding_box(bbox_data[2,2],0.1,y_coord_for_extension,"max")

#area_data<-sqrt((xmin_plot-xmax_plot)^2+(ymin_plot-ymax_plot)^2)

size_grid<-unique(substr(ind_catch_tunaatlasird_level1$geographic_identifier,1,1))

if (length(size_grid)==1){
if (size_grid=="6"){ # If the whole dataset is a 5° grid
    max_radius<-area_data^(1/5)/1
    min_radius<-area_data^(1/1000)/4
  }
  if (size_grid=="5"){ # If the whole dataset is a 1° grid
    max_radius<-area_data^(1/5)/2/2
    min_radius<-area_data^(1/1000)/4/3
  }
} else {  #hereunder is for the grids for which the sizes are not known
  max_radius<-area_data^(1/5)/2
  min_radius<-area_data^(1/1000)/4
  }

max_catch<-max(df_input.spdf$TOTAL)
min_catch<-min(df_input.spdf$TOTAL)


#define radius in function of the catch
if (nrow(df_input.spdf)>1){ # do this when there is more that 1 row in the dataset, if not we have max_catch=min_catch
#we can choose one of the functions in function of the distribution of the catches:
# if most of the catches are small, then we use the sqrt function to bring the small catches out
# if most of the catches are big, then we use the ^2 function to bring the big catches out
if (median(df_input.spdf$TOTAL)<mean(df_input.spdf$TOTAL)){
  fun_to_apply<-function(val){ val<-sqrt(val) ; return(val) }
}
if (median(df_input.spdf$TOTAL)>=mean(df_input.spdf$TOTAL)){
  fun_to_apply<-function(val){ val<-(val)^2 ; return(val) }
}

# function sqrt -> the differences between the small values of catches are more visible than the differences between the big values of catches
# y=a*sqrt(x)+b
#fun_to_apply<-function(val){ val<-sqrt(val) ; return(val) }

# function ^2 -> the differences between the big values of catches are stronger than the differences between the small values of catches
# y=a*(x)^2+b
#fun_to_apply<-function(val){ val<-(val)^2 ; return(val) }

# function linear -> proportionality
# y=a*(x)+b
#fun_to_apply<-function(val){ val<-val ; return(val) }


a=(min_radius-max_radius)/(fun_to_apply(min_catch)-fun_to_apply(max_catch))
b=max_radius-a*fun_to_apply(max_catch)
df_input.spdf$radius=a*fun_to_apply(df_input.spdf$TOTAL)+b
} else if (min_catch==max_catch){
  df_input.spdf$radius=max_radius
}

# to have a look at the profile of the radius in function of the circle: 
#plot(df_map.spdf$TOTAL,df_map.spdf$radius)




############ Other tests of functions...  ########################

# # test 1
# #df_map.spdf$radius<-df_map.spdf$TOTAL*max_radius/max(df_map.spdf$TOTAL)
# 
# #test 2
# #coeff_k<-1/3*max(df_map.spdf$TOTAL)
# #df_map.spdf$radius<-min_radius+(max_radius-min_radius)*(1-exp(-(df_map.spdf$TOTAL/coeff_k)))
# 
# #test 3
# #df_map.spdf$radius<-min_radius+(max_radius-min_radius)*(atan(2*df_map.spdf$TOTAL-max(df_map.spdf$TOTAL)/max(df_map.spdf$TOTAL)))
# 
# #initialize radius
# df_map.spdf$radius<-min_radius
# 
# 
# val_catch_inflexion_point<-mean(df_map.spdf$TOTAL) #max(df_map.spdf$TOTAL)/2
# 
# index.sup.val_catch_inflexion_point<-which(df_map.spdf$TOTAL>=val_catch_inflexion_point)
# index.inf.val_catch_inflexion_point<-which(df_map.spdf$TOTAL<val_catch_inflexion_point)
# 
# val_rayon_inflexion_point<-val_catch_inflexion_point*max_radius/max(df_map.spdf$TOTAL)
# 
# df_map.spdf$radius[index.inf.val_catch_inflexion_point]<-min_radius+(df_map.spdf$TOTAL[index.inf.val_catch_inflexion_point]/val_catch_inflexion_point)^2*(val_rayon_inflexion_point-min_radius)
# 
# df_map.spdf$radius[index.sup.val_catch_inflexion_point]<-df_map.spdf$TOTAL[index.sup.val_catch_inflexion_point]*max_radius/max(df_map.spdf$TOTAL)
#   
# 
# 
# 
# index_value_at_98_percent<-round(0.98*nrow(df_map.spdf))
# catch_value_at_98_percent<-df_map.spdf$TOTAL[order(df_map.spdf$TOTAL)][index_value_at_98_percent]
# 
# if (catch_value_at_98_percent*2.5 > max(df_map.spdf$TOTAL)){
# 
#   
#   val_rayon_inflexion_point<-val_catch_inflexion_point*max_radius/catch_value_at_98_percent
#   df_map.spdf$radius[index.inf.val_catch_inflexion_point]<-min_radius+(df_map.spdf$TOTAL[index.inf.val_catch_inflexion_point]/val_catch_inflexion_point)^2*(val_rayon_inflexion_point-min_radius)
#   
#   df_map.spdf$radius[index.sup.val_catch_inflexion_point]<-df_map.spdf$TOTAL[index.sup.val_catch_inflexion_point]*max_radius/max(df_map.spdf$TOTAL)
#   
#   
# index.sup98.val_catch_inflexion_point<-which(df_map.spdf$TOTAL>=catch_value_at_98_percent)
# radius_catch98<-df_map.spdf$radius[which(df_map.spdf$TOTAL==catch_value_at_98_percent)]
# k=0.5
# df_map.spdf$radius[index.sup98.val_catch_inflexion_point]<-radius_catch98+(max_radius-radius_catch98)/(pi/2)*atan((df_map.spdf$TOTAL[index.sup98.val_catch_inflexion_point]-catch_value_at_98_percent)/(k*max(df_map.spdf$TOTAL)))
#   
#   
# }

######################################################

### Define the colors

col_plot<-NULL
for (j in 1:number_of_variables){
  col_temp<-rainbow(number_of_variables)[j]
  col_plot<-c(col_plot,col_temp)  
}


#map("world", fill=TRUE, col="gray81", bg="white", xlim=c(xmin_plot,xmax_plot),ylim=c(ymin_plot,ymax_plot), mar=c(5,3,5,3))
map("world", fill=TRUE, col="gray81", bg="white", xlim=c(xmin_plot,xmax_plot),ylim=c(ymin_plot,ymax_plot))

#moll projection and centered in the pacific:
#map("world", fill=TRUE, col="gray", bg="white",  mar=c(8,2,5,2),projection="moll",orientation=c(90,180,0),wrap=TRUE)

#a<-map("world", fill=TRUE, col="gray", bg="white", xlim=c(xmin_plot,xmax_plot),ylim=c(ymin_plot,ymax_plot), mar=c(0,0,0,0))
#map.grid(a,nx=5,ny=5,pretty=TRUE)

# add a graticule
axis.y<-seq(par()$yaxp[1],par()$yaxp[2],by=(par()$yaxp[2]-par()$yaxp[1])/par()$yaxp[3])
axis.x<-seq(par()$xaxp[1],par()$xaxp[2],by=(par()$xaxp[2]-par()$xaxp[1])/par()$xaxp[3])


#if the default coordinates for the graticule are less that 5, we change the graticule to 5 quadrants
#This will have to be adapted when plotting data on 1 grids
function_multipleNombre<-function (val,mul,type) {
  val_abs_val<-abs(val)
  a = val_abs_val/mul
  b = trunc(a)
  
  if(val<0 & type=="min") { fact=0.1 } else { fact=0.9 }
  
  if((a-b)<=fact){
    r = b*mul
  } else {
    r = (b+1)*mul
  }
  if (r>0){
  r = max(r,mul)
  }
  if (val<0){ r=-r }
  return (r)
}

if ((par()$yaxp[2]-par()$yaxp[1])/par()$yaxp[3]<5){
  min_coord<-par()$yaxp[1]
  max_coord<-par()$yaxp[2]

  min_coord_multiple5<-function_multipleNombre(min_coord,5,"min")
  max_coord_multiple5<-function_multipleNombre(max_coord,5,"max")
  
  axis.y<-seq(min_coord_multiple5,max_coord_multiple5,by=5)
}
if ((par()$xaxp[2]-par()$xaxp[1])/par()$xaxp[3]<5){
  min_coord<-par()$xaxp[1]
  max_coord<-par()$xaxp[2]
  
  min_coord_multiple5<-function_multipleNombre(min_coord,5,"min")
  max_coord_multiple5<-function_multipleNombre(max_coord,5,"max")
  
  axis.x<-seq(min_coord_multiple5,max_coord_multiple5,by=5)
}


#plot the graticule
for (i in 1:length(axis.x)){
  abline(v=axis.x[i],lty=3)
}
for (i in 1:length(axis.y)){
  abline(h=axis.y[i],lty=3)
}



#add the axes
map.axes(cex.axis=0.8,side=1,at=axis.x,labels=paste(axis.x,"°",sep=""))
map.axes(cex.axis=0.8,side=2,at=axis.y,labels=paste(axis.y,"°",sep=""))


#plot centered on Pacific:
#map('world2', fill=FALSE,col="black",bg="white",wrap=TRUE,xlim=c(xmin_plot,xmax_plot),ylim=c(ymin_plot,ymax_plot))


### Map of total catch
#symbols(coordinates(df_map.spdf)[,1],coordinates(df_map.spdf)[,2],circles=df_map.spdf$radius,fg=NA, bg="darkgrey",add=TRUE,inches=.07)

### Add legend
#symbols(6000000,6000000,circles=1,inches=0.07,add=TRUE,fg="darkgrey",bg="darkgrey")
#text(8000000,6000000,paste(catch.ref*3," t",sep = ""), cex=0.9,col="white")

#Plot EEZ only if the area is <= 3000

#if (plot_eez==TRUE & !is.null(area_filter_wkt) & gArea(area_filter_wkt)<=3000){
## Get the EEZ from the marineregions webiste
#dsn<-paste("WFS:http://geo.vliz.be/geoserver/MarineRegions/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=MarineRegions:eez&maxFeatures=200&BBOX=",xmin_plot,",",ymin_plot,",",xmax_plot,",",ymax_plot,sep="")
#eez_marineregion_layer<-readOGR(dsn,"MarineRegions:eez")
#col_eez<-rgb(red=0, green=0, blue=102,alpha=70,maxColorValue=255)
#plot(eez_marineregion_layer,col=col_eez,add=TRUE,lwd=0.6,border="bisque4")
#}

# Plot data countour.


DistinctWKTdf <- subset(areas_wkt, !is.na(areas_wkt))
for (i in 1:length(DistinctWKTdf)){
  pol_data_countour <- readWKT(DistinctWKTdf[i], p4s=CRS("+proj=longlat"), id=as.character(1))
  plot(pol_data_countour,add=TRUE,lwd=0.3,border="gray79")
}



# Plot AOI
if (!is.null(area_filter_wkt)){
plot(readWKT(area_filter_wkt, p4s=CRS("+proj=longlat"), id=as.character(1)),add=TRUE,lwd=3)
}


  ### Map of catches by variable_class

### Add the catch 
for (i in 1:nrow(df_input.spdf)) { 
  #create vector x
  
  x_plot<-NULL
  for (j in 1:number_of_variables){
  x_temp<-as.data.frame(df_input.spdf[name_of_variables[j]])[i,1]
  x_plot<-c(x_plot,x_temp)
  }
  
  #If we use the world2 map (map centered on the Pacific), we have to change the negative longitude from [-180,180] to [0,360]
  #if (((coordinates(df_map.spdf)[i,])[1])<0){
  # xpos=(coordinates(df_map.spdf)[i,])[1]+360
  #} else {
    xpos=(coordinates(df_input.spdf)[i,])[1]
  #}
  
  #floating.pie(xpos=xpos,ypos=(coordinates(df_map.spdf[i,]))[2],x=x_plot,radius=df_map.spdf$relativeradius[i]*adj,edges=50,col=col_plot)
    floating.pie(xpos=xpos,ypos=(coordinates(df_input.spdf[i,]))[2],x=x_plot,radius=df_input.spdf$radius[i],edges=50,col=col_plot)
}

if (nrow(df_input.spdf)>1){
  #catch_for_legend<-ceiling(max(df_map.spdf$TOTAL))*2
  catch_for_legend<-ceiling(max(df_input.spdf$TOTAL))
fact<-floor( log10( catch_for_legend ) )
catch_for_legend<-catch_for_legend%/%10^fact*10^fact
radius_for_legend<-(a*fun_to_apply(catch_for_legend)+b)
} else {
  catch_for_legend<-max_catch
  radius_for_legend<-max_radius
}
angles <- floating.pie(xmax_plot-radius_for_legend,ymax_plot-radius_for_legend, 1, radius=radius_for_legend, edge=25, col = "darkgrey")


legend("bottomright",name_of_variables, bg="white",box.col="black",cex=1,col=col_plot, bty="o", fill=col_plot,horiz=FALSE,ncol=2,title="Classes :")

text(xmax_plot-radius_for_legend,ymax_plot-radius_for_legend,paste(catch_for_legend,"\n ",sep = ""), cex=0.9,col="black",font=2)

}



