library("RPostgreSQL")

psqlQuery <- function(statement){
    drv <- dbDriver("PostgreSQL")
    source("/home/finapp/FinApp_dbcon_details.R", local = T)
    psqlcon <- dbConnect(drv, dbname = psqldbname,
                         host = "localhost", port = 5432,
                         user = psqldbuser, password = psqldbpw)
    data <- dbGetQuery(psqlcon, statement)
    data <- c(result=list(data),dbGetException(psqlcon)) # $result, $errorNum, $errorMsg
    RPostgreSQL::dbDisconnect(psqlcon)
    data
}

psqlInsert <- function(df, tablename){
    drv <- dbDriver("PostgreSQL")
    source("/home/finapp/FinApp_dbcon_details.R", local = T)
    psqlcon <- dbConnect(drv, dbname = psqldbname,
                         host = "localhost", port = 5432,
                         user = psqldbuser, password = psqldbpw)
    RPostgreSQL::dbWriteTable(psqlcon, c("app",tablename), value = df, append = T, row.names = F)
    output <- dbGetException(psqlcon) # $errorNum, $errorMsg
    RPostgreSQL::dbDisconnect(psqlcon)
    output
}