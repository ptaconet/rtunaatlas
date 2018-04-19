#' @name create_calendar
#' @aliases create_calendar
#' @title Creates a continious calendar 
#' @description Creates a continious calendar according to input parameters : first and final date of the calendar, temporal resolution and temporal resolution unit
#' @export
#'
#' @usage create_calendar(firstdate,finaldate,resolution,unit)
#'  
#' @param firstdate character string first date of the calendar, format : YYYY-MM-DD
#' @param finaldate character string final date of the calendar, format : YYYY-MM-DD
#' @param resolution real temporal resolution of the calendar in day, month or year (see: temporal_reso_unit). Note: for 1/2 month put temporal_reso=1/2 and temporal_reso_unit="month" , type = integer;
#' @param unit character string temporal resolution unit. accepted value : "day" "month" "year"
#'
#' @return A dataframe with two columns : "time_start" (starting date of the period) and "time_end" (ending date of the period).
#' 
#' @details 
#' 
#' Values for unit can be: "day","month","year". 
#' 
#' The algorithm can create a mi-month calendar : first period between 1st and 15th of a month, second period between 16th and the end of the month. For this set resolution=0.5 and unit="month"
#' 
#' @family process data
#' 
#' @examples
#' 
#' calendar<-create_calendar("1900-01-01","2020-01-01",15,"day")
#' calendar<-create_calendar("1900-01-01","2020-01-01",0.5,"month")
#' 
#' @import lubridate
#' 
#' @author Chloé Dalleau, \email{dalleau.chloe@@hotmail.fr}; modified by Paul Taconet, \email{paul.taconet@@ird.fr}
#'   

create_calendar <- function(firstdate,finaldate,resolution,unit){

  ### Initialisation
  firstdate <- as_date(firstdate, tz = "UTC")
  finaldate <- as_date(finaldate, tz = "UTC")
  ### Create calendar based on input parameters
  if (resolution==1/2 && unit=="month") {
    ### Case : months are cut in two periods : first one from 1 to 15th, second one from 16 to month end
    ### initialisation
    temp_step <- 1
    day(firstdate) <- 01
    month(finaldate)<- month(finaldate)+1
    finaldate <- rollback(finaldate)
    mi_month <- firstdate
    day(mi_month) <- 15
    ### create the first date of each month
    pre_calendar_1 <- seq(firstdate, finaldate, paste(temp_step, unit))
    ### create the 15th of each month
    pre_calendar_2 <- seq(mi_month, finaldate, paste(temp_step, unit))
    ### create the calendar end
    end_calendar <- c(pre_calendar_1[length(pre_calendar_1)] + months(temp_step)-days(1))
    ### merge in a calendar, time_start = period beginnig, time_end = period end
    calendar <- data.frame(time_start=sort(c(pre_calendar_1,pre_calendar_2+1)),time_end=sort(c(pre_calendar_1[2:length(pre_calendar_1)]-1,pre_calendar_2, end_calendar)))
  } else {
    ### Other case
    ### create the period of time
    pre_calendar <- seq(firstdate, finaldate, paste(resolution, unit))
    ### create the calendar end
    switch(unit,
           "day" = {end_calendar <- pre_calendar[length(pre_calendar)] + days(resolution)-days(1)}, 
           "month" = { end_calendar <- pre_calendar[length(pre_calendar)] + months(resolution)-days(1)},
           "year" = {end_calendar <- pre_calendar[length(pre_calendar)] + years(resolution)-days(1)}
    )
    ### merge in a calendar, time_start = period beginnig, time_end = period end
    calendar <- data.frame(time_start=pre_calendar,time_end=c(pre_calendar[2:length(pre_calendar)]-1, end_calendar))
  }
  
  return(calendar)
}
