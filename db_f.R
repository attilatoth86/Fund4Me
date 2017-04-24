library("RPostgreSQL")

psqlQuery <- function(statement){
    drv <- dbDriver("PostgreSQL")
    source("/home/ati/Fund4Me/dbcon_details.R", local = T)
    psqlcon <- dbConnect(drv, dbname = psqldbname,
                         host = "localhost", port = 5432,
                         user = psqldbuser, password = psqldbpw)
    data <- dbGetQuery(psqlcon, statement)
    data <- c(result=list(data),dbGetException(psqlcon)) # $result, $errorNum, $errorMsg
    RPostgreSQL::dbDisconnect(psqlcon)
    data
}

psqlInsert_ld <- function(df, tablename){
    drv <- dbDriver("PostgreSQL")
    source("/home/ati/Fund4Me/dbcon_details.R", local = T)
    psqlcon <- dbConnect(drv, dbname = psqldbname,
                         host = "localhost", port = 5432,
                         user = psqldbuser, password = psqldbpw)
    RPostgreSQL::dbWriteTable(psqlcon, c("ld",tablename), value = df, append = T, row.names = F)
    output <- dbGetException(psqlcon) # $errorNum, $errorMsg
    RPostgreSQL::dbDisconnect(psqlcon)
    output
}