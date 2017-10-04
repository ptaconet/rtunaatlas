#' @name db_connection_sardara_france
#' @export
#' @title Connects to Sardara France PostgreSQL database stored on the Observatory of Exploited Tropical Pelagic Ecosystems (Ob7) servers. 
#' @details A VPN connection is necessary to connect to this database. 
#' @return a wrapper of RPostgreSQL connection (object of type "con")
#' @import RPostgreSQL 


db_connection_sardara_world<-function(db){
  
  library("RPostgreSQL")
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, dbname="sardara_france_2015", user="invsardara", password="fle087", host="mdst-macroes.ird.fr")
  
  return(con)
}