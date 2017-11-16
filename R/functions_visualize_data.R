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
fun_fill_wholes_in_catch<-function(df,TimeStart,TimeEnd,TimeResolution,colNameVariable,colNameTime){
  
  if (TimeResolution=="semester"){
    TimeResolution_time_series<-"6 month"
    TimeResolution_get_TimeEnd<-TimeResolution_time_series
  }
  if (TimeResolution=="decade"){
    TimeResolution_time_series<-"10 year"
    TimeResolution_get_TimeEnd<-TimeResolution_time_series
  }
  if (TimeResolution %in% c("month","quarter","year")){
    TimeResolution_time_series<-TimeResolution
    TimeResolution_get_TimeEnd<-paste("1 ",TimeResolution_time_series,sep="")
  }
  
  TimeEnd_for_timeseries<-as.Date(TimeEnd,"%Y-%m-%d") + diff(seq(from=as.Date(TimeEnd,"%Y-%m-%d"), length=2, by=paste("-",TimeResolution_get_TimeEnd,sep="")))
  
  wholeTimeSeries <- seq(from = as.Date(TimeStart, "%Y-%m-%d"), to = TimeEnd_for_timeseries, by = TimeResolution_time_series) 
  
  #wholeTimeSeries <- seq(from = as.Date(TimeStart, "%Y-%m-%d"), to = as.Date(TimeEnd,"%Y-%m-%d"), by = TimeResolution_time_series) 
  EmptyDates<-as.Date(setdiff(wholeTimeSeries,unique(df[,colNameTime])),"1970-01-01")
  
  variable_classes<-unique(df[,colNameVariable])
  
  if (length(EmptyDates)>0) {
    
    for (j in 1:length(EmptyDates)){
      for (i in 1:length(variable_classes)){
        df<-rbind(df,c(variable_classes[i],0,as.character(EmptyDates[j])))
      }
    }
  }
  
  df$value<-as.numeric(df$value)
  return(df)
}


fun_create_time_column<-function(df,TimeResolution,TimeStartColumn,TimeEndColumn,outputTimeColumnName){ #year, time
  #Input: a column time_start and a column time_end
  #We aggregate the time
  #Algo: 
  
  
  #if (TimeResolution %in% c("month","quarter","semester")){
    
  #  if (TimeResolution %in% c("quarter","semester")){
  #    if (TimeResolution=="semester"){
  #      vector_date_from=c(1,2)
  #      vector_date_to=c(1,7)
  #    }
  #    if (TimeResolution=="quarter"){
  #      vector_date_from=c(4,3,2,1)
  #      vector_date_to=c(10,7,4,1)
  #    }
      
  #    fun_convert_time_quarter_semester<-function(vector_date_from,vector_date_to,df){
  #      for (i in 1:length(vector_date_from)){  
  #        df[,TimeResolution]<-replace(df[,TimeResolution], which(df[,TimeResolution]==vector_date_from[i]), vector_date_to[i])
  #      }
  #      return(df)
  #    }
      
  #    df<-fun_convert_time_quarter_semester(vector_date_from,vector_date_to,df)
      
  #  }
    
  #  df[,outputTimeColumnName]<-as.Date(paste(df[,YearColName],"-",df[,TimeResolution],"-01",sep=""))
  #  df[,TimeResolution]<-NULL
  #}
  
  #  if (TimeResolution %in% c("year","decade")){
  #  df[,outputTimeColumnName]<-as.Date(paste(df[,YearColName],"-01-01",sep=""))
  #}
  #df$year<-NULL
  
#  return(df)
}


