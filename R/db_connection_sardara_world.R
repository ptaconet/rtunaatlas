#' @name db_connection_sardara_world
#' @export
#' @title Connects to Sardara World PostgreSQL database stored on the BlueBridge H2020 project servers.
#' @return a wrapper of RPostgreSQL connection (object of type "con")
#' @import RPostgreSQL 


db_connection_sardara_world<-function(db){

  library("RPostgreSQL")
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, dbname="sardara_world", user="invsardara", password="fle087", host="db-tuna.d4science.org")
  
  return(con)
}