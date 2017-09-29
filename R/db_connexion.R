#' @name db_connexion
#' @title Connects to a postgreSQL database and returns an object of type "con".
#' @description The databases connexions parameters must be set in this document: \url{https://raw.githubusercontent.com/ptaconet/rtunaatlas/master/inst/extdata/db_connexions_parameters.csv}
#' The column "connexion_name_package" is the name of the connexion used in the package rtunaatlas
#' @import RPostgreSQL 


db_connexion<-function(db){

  # read csv that specifies parameters for the connexions
  
  db_connexions_parameters<-read.csv("https://raw.githubusercontent.com/ptaconet/rtunaatlas/master/inst/extdata/db_connexions_parameters.csv",stringsAsFactors = F, colClasses = "character")

  db_connexions_parameters<-db_connexions_parameters[which(db_connexions_parameters$connexion_name_package==db),]
  
  dbname=db_connexions_parameters$server_dbname
  user=db_connexions_parameters$user
  password=db_connexions_parameters$password
  host=db_connexions_parameters$host
  
  library("RPostgreSQL")
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, dbname=dbname, user=user, password=password, host=host)
  
  
  return(con)
}