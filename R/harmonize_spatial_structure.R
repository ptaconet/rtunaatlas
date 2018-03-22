#' @name harmonize_spatial_structure
#' @aliases harmonize_spatial_structure 
#' @title Set of functions to harmonize the spatial dimension structure of the source tRFMOs datasets
#' @description Set of functions to harmonize the structure of the datasets for the spatial dimension.
#' @export harmo_spatial_1 harmo_spatial_2 harmo_spatial_3 harmo_spatial_4 harmo_spatial_5
#'
#' @usage 
#' harmo_spatial_1(df_input,colname_longitude,colname_latitude,colname_quadrant,colname_squaresize,colname_samplingareacode)
#' harmo_spatial_2(df_input,colname_year,colname_month)
#' harmo_spatial_3(df_input,colname_year,colname_monthstart,colname_monthstop)
#'
#' @param df_input data.frame with the spatial structure to harmonize
#' @param colname_longitude string. Name of the columns of longitudes (see Details) 
#' @param colname_latitude string. Name of the columns of latitudes (see Details) 
#' @param colname_quadrant string. Name of the columns for quadrants (see Details) 
#' @param colname_squaresize string. Name of the columns of size of squares (see Details)
#' @param colname_samplingareacode string. Name of the columns of sampling areas (see Details)
#' 


## Functions for CWP Grid harmonization

# harmo_spatial_1: for ICCAT . CAUTION: this function will not work for an ocean other than the Atlantic.
# The columns Lat and Lon in input give the coordinates of the point closest to (0,0). The column ColQuadrant gives the number of the CWP globe quadrant (1 to 4). colname_squaresize gives the CWP code of the square size.
# TO DO: EXTEND THIS FUNCTION SO THAT IT WORKS FOR ANY LAT, LON, COLQUADRANT, colname_squaresize)


# df_input = Input data frame
# colname_longitude = Name of the column longitude (string)
# colname_latitude = Name of the column latitude (string)
# colname_quadrant = Name of the column 


harmo_spatial_1<-function(df_input, colname_longitude, colname_latitude, colname_quadrant, colname_squaresize,colname_samplingareacode){
  
  
  df_input$SquareSizeCode<-NULL
  
  indice.5<- which(df_input[,colname_squaresize]=="1x1")
  indice.6<- which(df_input[,colname_squaresize]=="5x5")
  indice.7<- which(df_input[,colname_squaresize]=="10x10")
  indice.8<- which(df_input[,colname_squaresize]=="20x20")
  indice.9<- which(df_input[,colname_squaresize]=="30x30")
  #We add indices that do not exist in the CWP handbook. It is because in the ICCAT CE files there are square sizes that do not exist in the CWP handbook
  indice.2<- which(df_input[,colname_squaresize]=="10x20")
  indice.1<- which(df_input[,colname_squaresize]=="5x10")
  indice.none<- which(df_input[,colname_squaresize]=="none")
  indice.ICCAT <-which(df_input[,colname_squaresize]=="ICCAT")
  indice.LatLon <-which(df_input[,colname_squaresize]=="LatLon")
  
  
  df_input[indice.5,colname_squaresize]<-5
  df_input[indice.6,colname_squaresize]<-6
  df_input[indice.7,colname_squaresize]<-7
  df_input[indice.8,colname_squaresize]<-8
  df_input[indice.9,colname_squaresize]<-9
  df_input[indice.1,colname_squaresize]<-1
  df_input[indice.2,colname_squaresize]<-2
  df_input[indice.none,colname_squaresize]<-99
  df_input[indice.ICCAT,colname_squaresize]<-98
  df_input[indice.LatLon,colname_squaresize]<-98
  
  indice.col <- which((colnames(df_input) == colname_longitude) |(colnames(df_input) == colname_latitude)|(colnames(df_input) == colname_quadrant))
  
  indice.longi10 <- which(df_input[,colname_longitude]<10)
  indice.longi100 <- which((as.numeric(df_input[,colname_longitude])<100) & (as.numeric(df_input[,colname_longitude])>=10))
  
  if(length(indice.longi10)){
    df_input[indice.longi10,colname_longitude]<-paste("00", df_input[indice.longi10,colname_longitude], sep="")
  }
  if(length(indice.longi100)){
    df_input[indice.longi100,colname_longitude]<-paste("0", df_input[indice.longi100,colname_longitude], sep="")
  }
  
  indice.lati10 <- which(as.numeric(df_input[,colname_latitude])<10)
  if(length(indice.lati10)){
    df_input[indice.lati10,colname_latitude]<-paste("0", df_input[indice.lati10,colname_latitude], sep="")
  }
  
  df_input$AreaCWPgrid <- as.numeric(paste(df_input[,colname_squaresize],df_input[,colname_quadrant],df_input[,colname_latitude],df_input[,colname_longitude], sep=""))
  
  df_input$AreaName<-df_input[,"AreaCWPgrid"]
  
  df_input$AreaType<-df_input[,colname_squaresize]
  
  #df_input<-cbind(df_input,as.numeric(paste(df_input[,colname_squaresize],df_input[,colname_quadrant],df_input[,colname_latitude],df_input[,colname_longitude], sep="")))
  
  index.indet<-which(df_input[,colname_squaresize]=='99')
  if(length(index.indet)){
    df_input[index.indet,"AreaName"]<-"ALL"
    df_input[index.indet,"AreaCWPgrid"]<-NA
  }
  
  index.Get_SampAreaCode_instead_of_SquareCodeList<-which(df_input[,colname_squaresize]=='98')
  if(length(index.Get_SampAreaCode_instead_of_SquareCodeList)){
    df_input[index.Get_SampAreaCode_instead_of_SquareCodeList,"AreaName"]<-df_input[index.Get_SampAreaCode_instead_of_SquareCodeList,colname_samplingareacode]
    df_input[index.Get_SampAreaCode_instead_of_SquareCodeList,"AreaCWPgrid"]<-NA
  }
  
  return(df_input)
  
}



# harmo_spatial_2: for IOTC . Compute AreaType 

harmo_spatial_2<-function(df_input, ColIrrArea){
  
  df_input$AreaType<-as.numeric(substr(df_input[,ColIrrArea],0,1))
  
  indice.na <- which(is.na(df_input[,"AreaType"]))
  df_input[indice.na,"AreaType"]<-99
  
  
  # In IOTC datasets, the code 3 is equivalent to the CWP code 7; and the code 4 is equivalent to the CWP code 8. Therefore we transform the codes to be compliant with CWP standard.
  
  indice.3_to_7 <- which(df_input[,"AreaType"]==3)
  df_input[indice.3_to_7,"AreaType"]<-7
  df_input[indice.3_to_7,"AreaCWPgrid"]<-as.numeric(paste("7",substr(df_input[indice.3_to_7,"AreaCWPgrid"],2,7),sep=""))
  df_input[indice.3_to_7,"AreaName"]<-paste("7",substr(df_input[indice.3_to_7,"AreaCWPgrid"],2,7),sep="")
  
  indice.4_to_8 <- which(df_input[,"AreaType"]==4)
  df_input[indice.4_to_8,"AreaType"]<-8
  df_input[indice.4_to_8,"AreaCWPgrid"]<-as.numeric(paste("8",substr(df_input[indice.4_to_8,"AreaCWPgrid"],2,7),sep=""))
  df_input[indice.4_to_8,"AreaName"]<-paste("8",substr(df_input[indice.4_to_8,"AreaCWPgrid"],2,7),sep="")
  
  return(df_input)
}


# harmo_spatial_3: for WCPFC. Lat and Lon represent the latitude/longitude of the south-west corner of the square. CodeSquareSize = CWP code square size. SquareSize = length (in degrees) of a square (in lat or long)

harmo_spatial_3<-function(df_input, colname_latitude,colname_longitude,SquareSize,CodeSquareSize){
  
  # Calculate CWP quadrant in column "quadrant"
  df_input$Lon_W_or_E<-substr(df_input[,colname_longitude], nchar(df_input[,colname_longitude]), nchar(df_input[,colname_longitude]))
  df_input$Lat_N_or_S<-substr(df_input[,colname_latitude], nchar(df_input[,colname_latitude]), nchar(df_input[,colname_latitude]))
  
  df_input[,colname_longitude]<-sub('W', '', df_input[,colname_longitude])
  df_input[,colname_longitude]<-sub('E', '', df_input[,colname_longitude])
  df_input[,colname_latitude]<-sub('N', '', df_input[,colname_latitude])
  df_input[,colname_latitude]<-sub('S', '', df_input[,colname_latitude])
  
  indice.quad.1 <- which(df_input$Lon_W_or_E=="E" & df_input$Lat_N_or_S=="N")
  indice.quad.2 <- which(df_input$Lon_W_or_E=="E" & df_input$Lat_N_or_S=="S")
  indice.quad.3 <- which(df_input$Lon_W_or_E=="W" & df_input$Lat_N_or_S=="S")
  indice.quad.4 <- which(df_input$Lon_W_or_E=="W" & df_input$Lat_N_or_S=="N")
  
  df_input$quadrant<-9
  
  df_input[indice.quad.1,"quadrant"]<-1
  df_input[indice.quad.2,"quadrant"]<-2
  df_input[indice.quad.3,"quadrant"]<-3
  df_input[indice.quad.4,"quadrant"]<-4
  
  df_input[,colname_latitude]<-as.numeric(df_input[,colname_latitude])
  df_input[,colname_longitude]<-as.numeric(df_input[,colname_longitude])
  
  if(length(indice.quad.4)){
    df_input[indice.quad.4,colname_longitude]<-df_input[indice.quad.4,colname_longitude]-SquareSize
  }
  if(length(indice.quad.2)){
    df_input[indice.quad.2,colname_latitude]<-df_input[indice.quad.2,colname_latitude]-SquareSize
  }
  if(length(indice.quad.3)){
    df_input[indice.quad.3,colname_latitude]<-df_input[indice.quad.3,colname_latitude]-SquareSize
    df_input[indice.quad.3,colname_longitude]<-df_input[indice.quad.3,colname_longitude]-SquareSize
  }
  
  df_input$SquareSize<-CodeSquareSize
  
  df_input<-harmo_spatial_1(df_input,colname_longitude,colname_latitude,"quadrant","SquareSize")
  
  return(df_input)
}



# harmo_spatial_4: for IATTC. Lat and Lon represent the latitude/longitude of the center of the square

harmo_spatial_4<-function(df_input, colname_latitude,colname_longitude,colname_squaresize,ColCodeSquareSize){
  
  
  indice.quad.4 <- which(df_input[,colname_longitude]>=-180 & df_input[,colname_longitude]<=0 & df_input[,colname_latitude]>=0 & df_input[,colname_latitude]<=180)
  indice.quad.3 <- which(df_input[,colname_longitude]>=-180 & df_input[,colname_longitude]<=0 & df_input[,colname_latitude]>=-180 & df_input[,colname_latitude]<=0)
  indice.quad.2 <- which(df_input[,colname_longitude]>=0 & df_input[,colname_longitude]<=180 & df_input[,colname_latitude]>=-180 & df_input[,colname_latitude]<=0)
  indice.quad.1 <- which(df_input[,colname_longitude]>=0 & df_input[,colname_longitude]<=180 & df_input[,colname_latitude]>=0 & df_input[,colname_latitude]<=180)
  
  df_input$quadrant<-9
  
  df_input[indice.quad.1,"quadrant"]<-1
  df_input[indice.quad.2,"quadrant"]<-2
  df_input[indice.quad.3,"quadrant"]<-3
  df_input[indice.quad.4,"quadrant"]<-4
  
  
  if(length(indice.quad.4)){
    df_input[indice.quad.4,colname_longitude]<-df_input[indice.quad.4,colname_longitude]+df_input[indice.quad.4,colname_squaresize]/2
    df_input[indice.quad.4,colname_latitude]<-df_input[indice.quad.4,colname_latitude]-df_input[indice.quad.4,colname_squaresize]/2
  }
  if(length(indice.quad.2)){
    df_input[indice.quad.2,colname_longitude]<-df_input[indice.quad.2,colname_longitude]-df_input[indice.quad.2,colname_squaresize]/2
    df_input[indice.quad.2,colname_latitude]<-df_input[indice.quad.2,colname_latitude]+df_input[indice.quad.2,colname_squaresize]/2
  }
  if(length(indice.quad.3)){
    df_input[indice.quad.3,colname_longitude]<-df_input[indice.quad.3,colname_longitude]+df_input[indice.quad.3,colname_squaresize]/2
    df_input[indice.quad.3,colname_latitude]<-df_input[indice.quad.3,colname_latitude]+df_input[indice.quad.3,colname_squaresize]/2
  }
  if(length(indice.quad.1)){
    df_input[indice.quad.1,colname_longitude]<-df_input[indice.quad.1,colname_longitude]-df_input[indice.quad.1,colname_squaresize]/2
    df_input[indice.quad.1,colname_latitude]<-df_input[indice.quad.1,colname_latitude]-df_input[indice.quad.1,colname_squaresize]/2
  }
  
  df_input[,colname_longitude]<-abs(df_input[,colname_longitude])
  df_input[,colname_latitude]<-abs(df_input[,colname_latitude])
  
  
  df_input<-harmo_spatial_1(df_input,colname_longitude,colname_latitude,"quadrant",ColCodeSquareSize)
  
  return(df_input)
  
  
}




# harmo_spatial_5: for CCSBT. Lat and Lon represent the latitude/longitude of the north-west corner of the square. CodeSquareSize = CWP code square size. SquareSize = length (in degrees) of a square (in lat or long)

harmo_spatial_5<-function(df_input, colname_latitude,colname_longitude,SquareSize,CodeSquareSize){
  
  # Calculate CWP quadrant in column "quadrant"
  indice.quad.3 <- which(df_input[,colname_longitude]<=0)
  indice.quad.2 <- which(df_input[,colname_longitude]>0) 
  
  df_input$quadrant<-9
  df_input[indice.quad.2,"quadrant"]<-2
  df_input[indice.quad.3,"quadrant"]<-3
  
  df_input$SquareSize<-CodeSquareSize
  
  df_input[,colname_longitude]<-abs(df_input[,colname_longitude])
  df_input[,colname_latitude]<-abs(df_input[,colname_latitude])
  
  df_input<-harmo_spatial_1(df_input,colname_longitude,colname_latitude,"quadrant","SquareSize")
  
  return(df_input)
}














# function convert CWP to WKT (for vizualisation in Qgis)
cwp_tp_wkt<-function(tab,cwp_grid_column){
  #tab<-read.csv("/home/taconet/Bureau/IOTC-2015-WPB13-DATA-CELongline.csv",sep=",")
  tab$size_grid<-as.integer(substr(tab[,cwp_grid_column],1,1))
  tab$quadrant<-as.integer(substr(tab[,cwp_grid_column],2,2))
  
  
  conversions_aires<-data.frame(c(1,2,3,4,5,6),c(5,10,10,20,1,5),c(10,20,10,20,1,5))
  colnames(conversions_aires)<-c("code","latitude","longitude")
  
  tab$lon_min<-0
  tab$lon_max<-0
  tab$lat_min<-0
  tab$lat_max<-0
  
  
  
  # if (tab$quadrant==1 || tab$quadrant==2){
  
  
  index_quad1_2 <- which(tab$quadrant==1 | tab$quadrant==2)
  tab$lon_min[index_quad1_2]<-as.integer(substr(tab[,cwp_grid_column][index_quad1_2],5,7))
  
  index1 <- which(tab$quadrant==1 | tab$quadrant==4)
  tab$lat_min[index1]<- as.integer(substr(tab[,cwp_grid_column][index1],3,4))
  
  index2 <- which(tab$quadrant==2 | tab$quadrant==3)
  tab$lat_min[index2]<- - (as.integer(substr(tab[,cwp_grid_column][index2],3,4)))
  
  
  index3_1 <- which(tab$size_grid==1 & (tab$quadrant==1 | tab$quadrant==2))
  tab$lon_max[index3_1]<-tab$lon_min[index3_1]+10
  
  index3_2 <- which(tab$size_grid==2 & (tab$quadrant==1 | tab$quadrant==2))
  tab$lon_max[index3_2]<-tab$lon_min[index3_2]+20
  
  index3_3 <- which(tab$size_grid==3 & (tab$quadrant==1 | tab$quadrant==2))
  tab$lon_max[index3_3]<-tab$lon_min[index3_3]+10
  
  index3_4 <- which(tab$size_grid==4 & (tab$quadrant==1 | tab$quadrant==2))
  tab$lon_max[index3_4]<-tab$lon_min[index3_4]+20
  
  index3_5 <- which(tab$size_grid==5 & (tab$quadrant==1 | tab$quadrant==2))
  tab$lon_max[index3_5]<-tab$lon_min[index3_5]+1
  
  index3_6 <- which(tab$size_grid==6 & (tab$quadrant==1 | tab$quadrant==2))
  tab$lon_max[index3_6]<-tab$lon_min[index3_6]+5
  
  
  
  index4_1 <- which(tab$quadrant==1 & tab$size_grid==1)
  tab$lat_max[index4_1]<-tab$lat_min[index4_1]+5
  
  index4_2 <- which(tab$quadrant==1 & tab$size_grid==2)
  tab$lat_max[index4_2]<-tab$lat_min[index4_2]+10
  
  index4_3 <- which(tab$quadrant==1 & tab$size_grid==3)
  tab$lat_max[index4_3]<-tab$lat_min[index4_3]+10
  
  index4_4 <- which(tab$quadrant==1 & tab$size_grid==4)
  tab$lat_max[index4_4]<-tab$lat_min[index4_4]+20
  
  index4_5 <- which(tab$quadrant==1 & tab$size_grid==5)
  tab$lat_max[index4_5]<-tab$lat_min[index4_5]+1
  
  index4_6 <- which(tab$quadrant==1 & tab$size_grid==6)
  tab$lat_max[index4_6]<-tab$lat_min[index4_6]+5
  
  
  index5_1 <- which(tab$quadrant==2 & tab$size_grid==1)
  tab$lat_max[index5_1]<-tab$lat_min[index5_1]-5
  
  index5_2 <- which(tab$quadrant==2 & tab$size_grid==2)
  tab$lat_max[index5_2]<-tab$lat_min[index5_2]-10
  
  index5_3 <- which(tab$quadrant==2 & tab$size_grid==3)
  tab$lat_max[index5_3]<-tab$lat_min[index5_3]-10
  
  index5_4 <- which(tab$quadrant==2 & tab$size_grid==4)
  tab$lat_max[index5_4]<-tab$lat_min[index5_4]-20
  
  index5_5 <- which(tab$quadrant==2 & tab$size_grid==5)
  tab$lat_max[index5_5]<-tab$lat_min[index5_5]-1
  
  index5_6 <- which(tab$quadrant==2 & tab$size_grid==6)
  tab$lat_max[index5_6]<-tab$lat_min[index5_6]-5
  
  
  #  if (tab$quadrant==3 || tab$quadrant==4){
  index_quad3_4 <- which(tab$quadrant==3 | tab$quadrant==4)   
  tab$lon_max[index_quad3_4]<- - as.integer(substr(tab[,cwp_grid_column][index_quad3_4],5,7))
  
  
  
  index3_1 <- which(tab$size_grid==1 & (tab$quadrant==3 | tab$quadrant==4))
  tab$lon_min[index3_1]<-tab$lon_max[index3_1]-10
  
  index3_2 <- which(tab$size_grid==2 & (tab$quadrant==3 | tab$quadrant==4))
  tab$lon_min[index3_2]<-tab$lon_max[index3_2]-20
  
  index3_3 <- which(tab$size_grid==3 & (tab$quadrant==3 | tab$quadrant==4))
  tab$lon_min[index3_3]<-tab$lon_max[index3_3]-10
  
  index3_4 <- which(tab$size_grid==4 & (tab$quadrant==3 | tab$quadrant==4))
  tab$lon_min[index3_4]<-tab$lon_max[index3_4]-20
  
  index3_5 <- which(tab$size_grid==5 & (tab$quadrant==3 | tab$quadrant==4))
  tab$lon_min[index3_5]<-tab$lon_max[index3_5]-1
  
  index3_6 <- which(tab$size_grid==6 & (tab$quadrant==3 | tab$quadrant==4))
  tab$lon_min[index3_6]<-tab$lon_max[index3_6]-5
  
  
  
  index4_1 <- which(tab$quadrant==4 & tab$size_grid==1)
  tab$lat_max[index4_1]<-tab$lat_min[index4_1]+5
  
  index4_2 <- which(tab$quadrant==4 & tab$size_grid==2)
  tab$lat_max[index4_2]<-tab$lat_min[index4_2]+10
  
  index4_3 <- which(tab$quadrant==4 & tab$size_grid==3)
  tab$lat_max[index4_3]<-tab$lat_min[index4_3]+10
  
  index4_4 <- which(tab$quadrant==4 & tab$size_grid==4)
  tab$lat_max[index4_4]<-tab$lat_min[index4_4]+20
  
  index4_5 <- which(tab$quadrant==4 & tab$size_grid==5)
  tab$lat_max[index4_5]<-tab$lat_min[index4_5]+1
  
  index4_6 <- which(tab$quadrant==4 & tab$size_grid==6)
  tab$lat_max[index4_6]<-tab$lat_min[index4_6]+5
  
  
  index5_1 <- which(tab$quadrant==3 & tab$size_grid==1)
  tab$lat_max[index5_1]<-tab$lat_min[index5_1]-5
  
  index5_2 <- which(tab$quadrant==3 & tab$size_grid==2)
  tab$lat_max[index5_2]<-tab$lat_min[index5_2]-10
  
  index5_3 <- which(tab$quadrant==3 & tab$size_grid==3)
  tab$lat_max[index5_3]<-tab$lat_min[index5_3]-10
  
  index5_4 <- which(tab$quadrant==3 & tab$size_grid==4)
  tab$lat_max[index5_4]<-tab$lat_min[index5_4]-20
  
  index5_5 <- which(tab$quadrant==3 & tab$size_grid==5)
  tab$lat_max[index5_5]<-tab$lat_min[index5_5]-1
  
  index5_6 <- which(tab$quadrant==3 & tab$size_grid==6)
  tab$lat_max[index5_6]<-tab$lat_min[index5_6]-5
  
  
  tab$polygon_wkt<-paste("POLYGON ((",tab$lon_min," ",tab$lat_min," , ", tab$lon_min," ",tab$lat_max," , ", tab$lon_max," ",tab$lat_max," , " , tab$lon_max," ",tab$lat_min," , " , tab$lon_min," ",tab$lat_min," )) " , sep="")
  
  
  return(tab)
  
}
