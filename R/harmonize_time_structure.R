#' @name harmonize_time_structure
#' @aliases harmonize_time_structure 
#' @title Set of functions to harmonize the time dimension structure of the source tRFMOs datasets
#' @description Set of functions to harmonize the structure of the datasets for the time dimension
#' @export harmo_time_1 harmo_time_2 harmo_time_3 format_time_db_format
#'
#'
#' @usage 
#' harmo_time_1(df_input,colname_year,colname_timeperiod)
#' harmo_time_2(df_input,colname_year,colname_month)
#' harmo_time_3(df_input,colname_year,colname_monthstart,colname_monthstop)
#'
#' 
#' @param df_input data.frame with the time structure to harmonize
#' @param colname_year string. Name of the column of years.
#' @param colname_timeperiod string. Name of the column of period of time. 
#' @param colname_month string. Name of the column of month.
#' @param colname_monthstart string. Name of the column of starting month of validity of the data.
#' @param colname_monthstop string. Name of the column of ending month of validity of the data
#' 
#' 
#' @return a data.frame in which time dimension is structured with two columns:
#' \itemize{
#' \item{time_start}: {providing the starting date of validity of the data (included)}
#' \item{time_end}: {providing ending date of validity of the data (excluded)}
#'  }
#'
#'
#' @details 
#' \itemize{
#' \item{colname_year}: {Integer: YYYY}
#' \item{colname_timeperiod}: {Integer. Periods are defined as following:}
#' \itemize{
#' \item{1 to 12}: {1 month time period. The number gives the month number in the year (e.g. 1=January, 2=February, etc.);}
#' \item{13 to 16}: {1 quarter time period. 13=January to March, 14=April to June, 15=July to September, 16=October to December;}
#' \item{17}: {One year time period;}
#' \item{18 and 19}: {1 semester time period. 18=January to June, 19=July to December}
#' }
#' \item{colname_month}: {Integer: MM}
#' \item{colname_monthstart}: {Integer: MM}
#' \item{colname_monthstop}: {Integer: MM}
#' }
#' 
#' In 2017, the time structure of the source tRFMOs datasets are converted with the following functions:
#' \itemize{
#' \item{harmo_time_1}: {}
#' \item{harmo_time_2}: {}
#' \item{harmo_time_3}: {}
#' }
#' 
#' The functions harmo_time_1, harmo_time_2, harmo_time_3, harmo_time_4 work as following: 
#' 
#' 1) Fist they convert the source time structure to the following target structure:
#' \itemize{
#' \item{One column "Year" giving the year,}
#' \item{One coumn "MonthStart" giving the first month of the period,}
#' \item{One column "Period" giving the period. The period is defined as hereunder:}
#'  \itemize{
#' \item{1 = one month}
#' \item{3 = one trimester}
#' \item{6 = one semester}
#' \item{12 = one year}
#' }}
#' 
#' 2) Then they convert the latter structure to the structure with the time_start and time_end columns (this step is done using the function \code{format_time_db_format})
#'
#' @family harmonize data structure
#' 
#' @import lubridate
#' 
#' @author Paul Taconet, \email{paul.taconet@@ird.fr}


#Format df_input time to have the time format of the DB, which is one column time_start and one time_end
format_time_db_format<-function(df_input){
  
  df_input$time_start<-as.Date(paste(df_input[,"Year"],"-",df_input[,"MonthStart"],"-01",sep=""))
  # "months" is a function from the lubridate library
  df_input$time_end<-df_input$time_start+months(df_input[,"Period"])
  
  df_input$time_start<-as.character(df_input$time_start)  
  df_input$time_end<-as.character(df_input$time_end)
  
  #df_input$time_start<-as.POSIXlt(cut(df_input$time_start,breaks="day"),format="%Y-%m-%d",tz="UTC")
  #df_input$time_end<-as.POSIXlt(cut(df_input$time_end,breaks="day"),format="%Y-%m-%d",tz="UTC")
  
  #We transform to numeric type, because the package data.table that is used in the script "integration_sardara" does not accept the PosixCT type. We will, after, transform it back to PosixCT
  
  #df_input$time_start<-as.numeric(df_input$time_start)
  #df_input$time_end<-as.numeric(df_input$time_end)
  
  return(df_input)
}




harmo_time_1<-function(df_input, colname_year, colname_timeperiod){
  
  df_input[,"MonthStart"] <- NA
  
  index1to12 <- which( df_input[,colname_timeperiod]>=1 & df_input[,colname_timeperiod]<=12 )
  if (length(index1to12)>1){
    df_input[index1to12,"MonthStart"]<- df_input[index1to12,colname_timeperiod]
    df_input[index1to12,colname_timeperiod]<- 1
  }
  
  index17 <- which( df_input[,colname_timeperiod] == 17)
  if (length(index17)>1){
    df_input[index17,"MonthStart"]<- 1
    df_input[index17,colname_timeperiod]<- 12
  }
  index13 <- which( df_input[,colname_timeperiod] == 13)
  if (length(index13)>1){
    df_input[index13,"MonthStart"]<- 1
    df_input[index13,colname_timeperiod]<- 3
  }
  index14 <- which( df_input[,colname_timeperiod] == 14)
  if (length(index14)>1){
    df_input[index14,"MonthStart"]<- 4
    df_input[index14,colname_timeperiod]<- 3
  }
  index15 <- which( df_input[,colname_timeperiod] == 15)
  if (length(index15)>1){
    df_input[index15,"MonthStart"]<- 7
    df_input[index15,colname_timeperiod]<- 3
  }
  index16 <- which( df_input[,colname_timeperiod] == 16)
  if (length(index16)>1){
    df_input[index16,"MonthStart"]<- 10
    df_input[index16,colname_timeperiod]<- 3
  }
  index18 <- which( df_input[,colname_timeperiod] == 18)
  if (length(index18)>1){
    df_input[index18,"MonthStart"]<- 1
    df_input[index18,colname_timeperiod]<- 6
  }
  index19 <- which( df_input[,colname_timeperiod] == 19)
  if (length(index19)>1){
    df_input[index19,"MonthStart"]<- 7
    df_input[index19,colname_timeperiod]<- 6
  }
  
  colnames(df_input)[which(colnames(df_input) == colname_year)] <- "Year"
  colnames(df_input)[which(colnames(df_input) == colname_timeperiod)] <- "Period" 
  
  df_input<-format_time_db_format(df_input)
  
  return(df_input)
}

harmo_time_2<-function(df_input, colname_year, colname_month){
  
  colnames(df_input)[which(colnames(df_input) == colname_year)] <- "Year"
  colnames(df_input)[which(colnames(df_input) == colname_month)] <- "MonthStart"
  
  df_input$Period<-1
  #for the month that are not defined, we set the data to a year time period
  index_na<-which(is.na(df_input$MonthStart))
  if (length(index_na)>0){
    df_input$Period[index_na]=12
    df_input$MonthStart[index_na]=1
  }
  
  df_input<-format_time_db_format(df_input)
  
  return(df_input)
}


harmo_time_3<-function(df_input, colname_year, colname_monthstart, colname_monthstop){
  
  df_input$Period<-NA
  
  index1 <- which( df_input[,colname_monthstart] == df_input[,colname_monthstop])
  index17 <- which( df_input[,colname_monthstop] - df_input[,colname_monthstart] == 11)
  index13 <- which( (df_input[,colname_monthstop] - df_input[,colname_monthstart] == 2) & ( df_input[,colname_monthstart] == 1))
  index14 <- which( (df_input[,colname_monthstop] - df_input[,colname_monthstart] == 2) & ( df_input[,colname_monthstart] == 4))
  index15 <- which( (df_input[,colname_monthstop] - df_input[,colname_monthstart] == 2) & ( df_input[,colname_monthstart] == 7))
  index16 <- which( (df_input[,colname_monthstop] - df_input[,colname_monthstart] == 2) & ( df_input[,colname_monthstart] == 10))
  index18 <- which( (df_input[,colname_monthstop] - df_input[,colname_monthstart] == 5) & ( df_input[,colname_monthstart] == 1))
  index19 <- which( (df_input[,colname_monthstop] - df_input[,colname_monthstart] == 5) & ( df_input[,colname_monthstart] == 7))
  
  
  if (length(index1)>1){
    df_input[index1,"Period"]<- df_input[index1,colname_monthstart]
  }
  
  if (length(index13)>1){
    df_input[index13,"Period"]<- 13
  }
  
  if (length(index14)>1){  
    df_input[index14,"Period"]<- 14
  }
  
  if (length(index15)>1){
    df_input[index15,"Period"]<- 15
  }
  
  if (length(index16)>1){
    df_input[index16,"Period"]<- 16
  }
  if (length(index17)>1){
    df_input[index17,"Period"]<- 17
  }
  if (length(index18)>1){
    df_input[index18,"Period"]<- 18
  }
  
  if (length(index19)>1){  
    df_input[index19,"Period"]<- 19
  }
  
  colnames(df_input)[which(colnames(df_input) == colname_year)] <- "Year"
  
  df_input<-harmo_time_1(df_input,"Year","Period")
  
  df_input<-format_time_db_format(df_input)
  
  return(df_input)
  
}



#--------------------------------------------------#
# Format 4. To transforme data provided with a Season Column that represent a year and a month at the same time knowing that there are 3 months separating the values of Season_ColName
#Input :
#df_input = data.frame
#Season_ColName = Name of the column of season. 
#YearStart = First year 

#Concerned TRFMO(s): IATTC CAS 
#--------------------------------------------------#

#Harmo_Time_4<-function(df_input, Season_ColName,StartYear){
  
#  df_input$Period<-3
#df_input$Year<-trunc(StartYear+trunc(df_input[,Season_ColName])/4)
#  index.multiple4<-which(df_input[,Season_ColName]%%4==0)
# df_input$Year[index.multiple4]<-df_input$Year[index.multiple4]-1
  
  #  df_input$rest<-df_input[,Season_ColName]/4
  #df_input$rest<-(df_input$rest*100)%%100
  
  #index.0<-which(df_input$rest==0)
  #index.25<-which(df_input$rest==25)
  #index.50<-which(df_input$rest==50)
  #index.75<-which(df_input$rest==75)
  
  #df_input$MonthStart<-1
  #df_input$MonthStart[index.0]<-9
  #df_input$MonthStart[index.25]<-1
  #df_input$MonthStart[index.50]<-3
  #df_input$MonthStart[index.75]<-6
  
  #df_input$rest<-NULL
  
  #  return(df_input)
  #}

