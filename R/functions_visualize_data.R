#' @name functions_visualize_data
#' @aliases functions_visualize_data
#' @title Functions used to load a dataset in a database with Sardara model
#' @description A set of functions used to vizualise datasets
#' @export fun_aggregate_keep_n_classes
#'
#' @family visualize data
#' 
#' @author Paul Taconet, \email{paul.taconet@@ird.fr}
#'



fun_aggregate_keep_n_classes<-function(df_plot,number_of_classes,ColnameVariable){
  
  if (number_of_classes>length(unique(df_plot[,ColnameVariable]))) { number_of_classes=length(unique(df_plot[,ColnameVariable])) }
  
  aggdata <-aggregate(df_plot$value, by=list(df_plot[,ColnameVariable]),FUN=sum, na.rm=TRUE)
  aggdata <- aggdata[order(-aggdata$x),]
  n_more_catches_variables<-aggdata$Group.1[1:number_of_classes-1]
  
  index.other<-which(!(df_plot[,ColnameVariable] %in% n_more_catches_variables))
  if(length(index.other)>0){

    df_plot[index.other,ColnameVariable]<-"Others"
    
    df_plot_aggregate_var<-as.list(df_plot[,-which(names(df_plot) == "value")])

    df_plot <-aggregate(df_plot$value, by=df_plot_aggregate_var,FUN=sum, na.rm=TRUE)
    colnames(df_plot)[which(colnames(df_plot)=="x")] <- "value"
  }
  
  return(df_plot)
}



# draw min and max catches

draw_line<-function(yb,ya,xb,xa){
  
  pente=(yb-ya)/(xb-xa)
  intercept=yb-pente*xb
  ligne<-ablineclip(a=intercept,b=pente,x1=xa,x2=xb,lty = 3,col="red")
  return(ligne)
}

#generate series with null catches when there are wholes in the catches, and then fill the data frame with this empty dates and 0 for catch values
fun_fill_wholes_in_catch<-function(df_input,TimeStart,TimeEnd,time_resolution,colNameVariable){
  
  if (time_resolution=="semester"){
    time_resolution_time_series<-"6 month"
    time_resolution_get_TimeEnd<-time_resolution_time_series
  }
  if (time_resolution=="decade"){
    time_resolution_time_series<-"10 year"
    time_resolution_get_TimeEnd<-time_resolution_time_series
  }
  if (time_resolution %in% c("month","quarter","year")){
    time_resolution_time_series<-time_resolution
    time_resolution_get_TimeEnd<-paste("1 ",time_resolution_time_series,sep="")
  }
  
  TimeEnd_for_timeseries<-as.Date(TimeEnd,"%Y-%m-%d") + diff(seq(from=as.Date(TimeEnd,"%Y-%m-%d"), length=2, by=paste("-",time_resolution_get_TimeEnd,sep="")))
  
  wholeTimeSeries <- seq(from = as.Date(TimeStart, "%Y-%m-%d"), to = TimeEnd_for_timeseries, by = time_resolution_time_series) 
  
  #wholeTimeSeries <- seq(from = as.Date(TimeStart, "%Y-%m-%d"), to = as.Date(TimeEnd,"%Y-%m-%d"), by = time_resolution_time_series) 
  EmptyDates<-as.Date(setdiff(wholeTimeSeries,unique(df_input$time)),"1970-01-01")
  
  if (!is.null(colNameVariable)){
  variable_classes<-unique(df_input[,colNameVariable])
  } else { variable_classes=NULL }
  
  if (length(EmptyDates)>0) {
    
    for (j in 1:length(EmptyDates)){
      if (!is.null(variable_classes)){
      for (i in 1:length(variable_classes)){
        df_input<-rbind(df_input,c(variable_classes[i],0,as.character(EmptyDates[j])))
      }
      } else {
        df_input<-rbind(df_input,c(0,as.character(EmptyDates[j])))
      }
    }
  }
  
  df_input$value<-as.numeric(df_input$value)
  return(df_input)
}


fun_create_time_column<-function(df_input,time_resolution){
  #Input: a column time_start and a column time_end
  #We aggregate the time
  
  df_input$time_date_format<-as.Date(df_input$time_start)
  df_input$year<-as.numeric(format(df_input$time_date_format,'%Y'))
  
  if (time_resolution %in% c("month","quarter","semester")){
    
    if (time_resolution=="month"){
      df_input$time_res<-as.numeric(format(df_input$time_date_format,'%m'))
    }
    if (time_resolution %in% c("quarter","semester")){
      if (time_resolution=="semester"){
        vector_date_from=c(1,2)
        vector_date_to=c(1,7)
        df_input$time_res<-lubridate::semester(df_input$time_date_format)
      }
      if (time_resolution=="quarter"){
        vector_date_from=c(4,3,2,1)
        vector_date_to=c(10,7,4,1)
        df_input$time_res<-lubridate::quarter(df_input$time_date_format)
      }
      
      fun_convert_time_quarter_semester<-function(vector_date_from,vector_date_to,df_input){
        for (i in 1:length(vector_date_from)){  
          df_input$time_res<-replace(df_input$time_res, which(df_input$time_res==vector_date_from[i]), vector_date_to[i])
        }
        return(df_input)
      }
      
      df_input<-fun_convert_time_quarter_semester(vector_date_from,vector_date_to,df_input)
      
    }
  
    df_input$time<-as.Date(paste(df_input$year,"-",df_input$time_res,"-01",sep=""))
    df_input$time_res<-NULL
  }
  
    if (time_resolution %in% c("year","decade")){
    df_input$time<-as.Date(paste(df_input$year,"-01-01",sep=""))
  }
  df_input$year<-NULL
  df_input$time_date_format<-NULL
  df_input$time_start<-NULL
  df_input$time_end<-NULL
  
  return(df_input)
}


