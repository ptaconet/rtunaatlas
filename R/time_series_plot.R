#' @name time_series_plot
#' @aliases time_series_plot
#' @title Create a times series plot
#' @description This function ouputs a pie map from the dataset provided as input
#' @export
#' 
#' @param df_input data.frame to map
#' @param time_resolution string time resolution to visualize on the plot. 
#' @param dimension_group_by string. Name of the dimension that will be the classes in the chart or NULL if no aggregation dimension.
#' @param number_of_classes integer. Number of classes to visualize on the chart.
#' 
#'
#' @details
#'
#' All values in \code{df_input} must be expressed with the same unit (since the function aggregates the data).
#' 
#' time_resolution takes one of the following values: "month", "quarter", "semester", "year", "decade" 
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
#' time_series_plot(
#'       df_input=ind_catch_tunaatlasird_level2,
#'       time_resolution="month",
#'       dimension_group_by="species",
#'       number_of_classes=4 
#'      )
#'      
#' dbDisconnect(con)
#' 
#' @importFrom lubridate quarter
#' @importFrom lubridate semester
#' @importFrom plotrix stackpoly
#' @importFrom plotrix ablineclip


time_series_plot<-function(df_input, # data frame with standard DSD. Can have a column 'proportion_source_area_intersection' with proportional (output of function rtunaatlas::spatial_curation_intersect_areas)
                         time_resolution, # month,quarter,semester,year,decade
                         dimension_group_by=NULL, # String. Column name to use to aggregate. NULL if no aggregation column
                         number_of_classes=2 #number of classes
){
  
  if(nrow(df_input)==0){stop("There is no data in your dataset")}

  if ("proportion_source_area_intersection" %in% colnames(df_input)){
  
  compute_proportion=TRUE  
    
  # Dataset Hypothesis minimizing the data
  df_input_min<-df_input[which(df_input$proportion_source_area_intersection==1),]

  # Dataset Hypothesis maximizing the data
  df_input_max<-df_input[which(df_input$proportion_source_area_intersection>0.001),]
  # Remove duplicated rows (when a source area intersects multiple target areas)
  df_input_max<-data.table(df_input_max)
  df_input_max<-unique(df_input_max,by=setdiff(colnames(df_input_max),c("geographic_identifier_intersection_layer", "proportion_source_area_intersection")))
  
  # Dataset Hypothesis proportional.
  df_input<-df_input[which(!is.na(df_input$geographic_identifier_intersection_layer)),]
  df_input$value<-df_input$value*df_input$proportion_source_area_intersection
  
  } else {
    compute_proportion=FALSE  
  }
  
  
  ColnameTimeStart="time_start"
  ColnameTimeEnd="time_end"
  
  if (!is.null(dimension_group_by)){
    df_input<-df_input %>%
      group_by_(dimension_group_by,ColnameTimeStart,ColnameTimeEnd) %>% 
      summarise(value = sum(value))
  } else {
    df_input<-df_input %>%
      group_by_(ColnameTimeStart,ColnameTimeEnd) %>% 
      summarise(value = sum(value))
  }
  
  df_input<-data.frame(df_input)
  
  if (!is.null(dimension_group_by)){
  #Then we aggregate to keep only the number_of_classes most important classes. The others are grouped under "other"
  df_input<-fun_aggregate_keep_n_classes(df_input,number_of_classes,dimension_group_by)
  }

  df_input<-fun_create_time_column(df_input,time_resolution)
  
  TimeStart<-min(df_input$time)
  TimeEnd<-max(df_input$time)
  df_input<-fun_fill_wholes_in_catch(df_input,TimeStart,TimeEnd,time_resolution,dimension_group_by)
  
  if (is.null(dimension_group_by)){
    dimension_group_by<-"no_group"
    df_input[,dimension_group_by]<-""
  }
  df_input <-aggregate(df_input$value, by=list(df_input[,dimension_group_by],df_input$time),FUN=sum)
colnames(df_input)<-c("variable_class","time","value")

df_input <- df_input[order(df_input$time, df_input$variable_class),]
df_input<-as.data.frame(tapply(df_input$value, list(time=df_input$time, variable_class=df_input$variable_class), mean))

df_input[is.na(df_input)]<-0


# Set some graphical parameters
#if (length(wholeTimeSeries)<20){
#xat<-seq(from = as.Date(TimeStart, "%Y-%m-%d"), to = as.Date(TimeEnd,"%Y-%m-%d"), by = TimeResolution) 
#} else {
#  xat<-seq(from = as.Date(TimeStart, "%Y-%m-%d"), to = as.Date(TimeEnd,"%Y-%m-%d"), by = TimeResolution) 
#}

if (time_resolution %in% c("year","decade")){
xaxlab<-substr(rownames(df_input),0,4)
}

if (time_resolution %in% c("month","quarter","semester")){
xaxlab<-rownames(df_input)
}

# Plot 

if (ncol(df_input)==1){
  x=as.Date(rownames(df_input))
  y=df_input[,1]
  stack_value=FALSE
} else {
  x=df_input
  y=NULL
  stack_value=TRUE
}



if (compute_proportion==TRUE){
# if the intersection method is a proportion, we compute the min catches (i.e. exclude) and max catches (i.e. include) to have a index of confidence

# prepare mincatch and maxcatch in case of proportion
  if(nrow(df_input_min)>0){
    df_input_min<-df_input_min %>%
      group_by_(ColnameTimeStart,ColnameTimeEnd) %>% 
      summarise(value = sum(value))
    df_input_min<-fun_create_time_column(df_input_min,time_resolution)
    df_input_min<-data.frame(df_input_min)
    df_input_min<-fun_fill_wholes_in_catch(df_input_min,TimeStart,TimeEnd,time_resolution,NULL)
    df_input_min<-aggregate(df_input_min$value, by=list(df_input_min$time), FUN=sum)
    df_input_min[is.na(df_input_min)]<-0
    colnames(df_input_min)<-c("time","value")
  } else {
    df_input_min<-data.frame(time=as.Date(rownames(df_input)),value=0)
}
  df_input_max<-df_input_max %>%
    group_by_(ColnameTimeStart,ColnameTimeEnd) %>% 
    summarise(value = sum(value))
  df_input_max<-fun_create_time_column(df_input_max,time_resolution)
  df_input_max<-data.frame(df_input_max)
  df_input_max<-fun_fill_wholes_in_catch(df_input_max,TimeStart,TimeEnd,time_resolution,NULL)
  df_input_max<-aggregate(df_input_max$value, by=list(df_input_max$time), FUN=sum)
  df_input_max[is.na(df_input_max)]<-0
  colnames(df_input_max)<-c("time","value")

  ymax=max(df_input_max$value)
} else { ymax=max(apply(df_input,1,sum))+0.1*max(apply(df_input,1,sum)) }

plotrix::stackpoly(x=x,y=y, stack=stack_value, xaxlab=xaxlab,ylim=c(0,ymax),col=grey.colors(ncol(df_input)),
                   xlab="Time", border="black",lwd=0.3,cex.main = 1.5)


if (compute_proportion==TRUE){
if (ncol(df_input)==1){
  
  for (i in par("usr")[1]:(par("usr")[1]+nrow(df_input)-2)){
    
    y.i<-i-par("usr")[1]+1
    
    yb=df_input_max$value[y.i+1]
    ya=df_input_max$value[y.i]
    xb=as.numeric(as.Date(rownames(df_input)[y.i+1]))
    xa=as.numeric(as.Date(rownames(df_input)[y.i]))
    
    draw_line(yb,ya,xb,xa)
    
    yb=df_input_min$value[y.i+1]
    ya=df_input_min$value[y.i]
    draw_line(yb,ya,xb,xa)
    
  }
  
} else {
  
  for (i in par("usr")[1]:(par("usr")[1]+nrow(df_input)-2)){
    
    yb=df_input_max$value[i+1]
    ya=df_input_max$value[i]
    xb=i+1
    xa=i
    
    draw_line(yb,ya,xb,xa)
    
    yb=df_input_min$value[i+1]
    ya=df_input_min$value[i]
    draw_line(yb,ya,xb,xa)
    
  }
  
}

# A mention on the red dotted lines

#text(x = par("usr")[2]-0.3*par("usr")[2], y = par("usr")[4]-0.1*par("usr")[4], "The red dotted lines indicate the minimum and maximum catches",cex = 1, col = "black",adj=0)

}


if (!is.null(dimension_group_by)){
legend("topleft",legend=rev(colnames(df_input)),bty="n",fill=rev(grey.colors(ncol(df_input))),cex=1)
} else {}


}