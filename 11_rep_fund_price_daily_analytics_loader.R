message("----------------------------------------")
message(paste("Job starts:",Sys.time()))

message("\n")
message("Importing necessary functions for database connection..")
source("/home/ati/Fund4Me/db_f.R")

message("\n")
message("Source Object setup..")
###############################################################################
sou_obj_desc <- "SO_REP_FUND_PRICE_DAILY_LOADER"
if(sou_obj_desc!=psqlQuery(sprintf("SELECT constant FROM dw.source_object WHERE constant='%s'",sou_obj_desc))$result[1,1]){
  stop("Invalid source object!")
} else {
  sou_obj_id <- psqlQuery(sprintf("SELECT id FROM dw.source_object WHERE constant='%s';",sou_obj_desc))$result[1,1]  
}
###############################################################################
message(paste0("Source Object id: ",sou_obj_id))
message(paste0("Source Object constant: ",sou_obj_desc))

message("\n")
truncateStatus <- psqlQuery("TRUNCATE TABLE rep.fund_price_daily_analytics;")
message(paste0("Truncate rep.fund_price_daily_analytics table........",truncateStatus$errorMsg))

message("\n")
queryString <- sprintf("INSERT INTO rep.fund_price_daily_analytics (source_object_id, fund_id, date, price, net_asset_value, paid_dividend, price_chg, return)
                          SELECT 
                          (SELECT id FROM dw.source_object WHERE constant='%s') source_object_id,
                          fund_id, 
                          date, 
                          price, 
                          net_asset_value, 
                          paid_dividend,
                          (price / first_value(price) OVER (PARTITION BY fund_id ORDER BY date)) - 1.0 AS price_chg,
                          (price / lag(price) OVER (PARTITION BY fund_id ORDER BY date)) - 1.0 AS return
                          FROM dw.fund_price;",sou_obj_desc)

execStatus <- psqlQuery(queryString)
message(paste0("Loader is being executed, data is being loaded into rep.fund_price_daily_analytics table........",execStatus$errorMsg))

message("\n")
message(paste("Job ends: ",Sys.time()))
message("----------------------------------------")