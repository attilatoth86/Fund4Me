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
queryString <- sprintf("INSERT INTO rep.fund_price_daily_analytics (source_object_id, fund_id, date, price, net_asset_value, paid_dividend, price_chg, return, return_rf, return_excess)
                          SELECT
                            (SELECT id FROM dw.source_object WHERE constant='%s') source_object_id,
                            core.fund_id,
                            core.date,
                            core.price,
                            core.net_asset_value,
                            core.paid_dividend,
                            core.price_chg,
                            core.return,
                            ((1.0 + yc.yield/100.0) ^ (1.0 / 252.0)) - 1.0,
                            core.return-(((1.0 + yc.yield/100.0) ^ (1.0 / 252.0)) - 1.0)
                          FROM
                            (
                              SELECT 
                                fp.fund_id, 
                                fp.date, 
                                fp.price, 
                                fp.net_asset_value, 
                                fp.paid_dividend,
                                (fp.price / first_value(fp.price) OVER (PARTITION BY fp.fund_id ORDER BY fp.date)) - 1.0 AS price_chg,
                                (fp.price / lag(fp.price) OVER (PARTITION BY fp.fund_id ORDER BY fp.date)) - 1.0 AS return,
                                ROUND(EXTRACT(epoch FROM f.rec_inv_term)/(24*60*60*7))*7 rec_inv_tenor,
                                f.currency_id
                              FROM dw.fund_price fp
                                  INNER JOIN dw.fund f ON fp.fund_id=f.id
                            ) core
                          LEFT OUTER JOIN dw.yield_curve yc ON core.currency_id=yc.currency_id 
                                                              AND core.date=yc.value_date 
                                                              AND core.rec_inv_tenor=yc.tenor 
                                                              AND yc.yield_curve_type_id=(SELECT id FROM dw.classification WHERE constant='C_YC_TYP_HUFZC')"
                       ,sou_obj_desc)

execStatus <- psqlQuery(queryString)
message(paste0("Loader is being executed, data is being loaded into rep.fund_price_daily_analytics table........",execStatus$errorMsg))

message("\n")
message(paste("Job ends: ",Sys.time()))
message("----------------------------------------")