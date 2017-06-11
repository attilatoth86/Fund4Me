message("----------------------------------------")
message(paste("Job starts:",Sys.time()))

message("\n")
message("Importing necessary functions for database connection..")
source("/srv/shiny-server/fund4me/db_f.R")

message("\n")
message("Source Object setup..")
###############################################################################
sou_obj_desc <- "SO_REP_FUNDSUM_LOADER"
if(sou_obj_desc!=psqlQuery(sprintf("SELECT constant FROM dw.source_object WHERE constant='%s'",sou_obj_desc))$result[1,1]){
  stop("Invalid source object!")
} else {
  sou_obj_id <- psqlQuery(sprintf("SELECT id FROM dw.source_object WHERE constant='%s';",sou_obj_desc))$result[1,1]  
}
###############################################################################
message(paste0("Source Object id: ",sou_obj_id))
message(paste0("Source Object constant: ",sou_obj_desc))

message("\n")
message("Calculating price dates..")
message("\n")
message("Creating control table..")
date_ctrl_tbl <- psqlQuery("SELECT f.id, f.short_name, 
                              dw.get_first_fund_price_date(f.id) date_start,
                              dw.get_last_fund_price_date (id) date_recent,
                              DATE_TRUNC('year', dw.get_last_fund_price_date(f.id))::DATE cal_date_ytd,
                              (dw.get_last_fund_price_date(f.id)-'1 month'::INTERVAL)::DATE cal_date_1m,
                              (dw.get_last_fund_price_date(f.id)-'3 months'::INTERVAL)::DATE cal_date_3m,
                              (dw.get_last_fund_price_date(f.id)-'6 months'::INTERVAL)::DATE cal_date_6m,
                              (dw.get_last_fund_price_date(f.id)-'1 year'::INTERVAL)::DATE cal_date_1yr,
                              (dw.get_last_fund_price_date(f.id)-'2 years'::INTERVAL)::DATE cal_date_2yr,
                              (dw.get_last_fund_price_date(f.id)-'3 years'::INTERVAL)::DATE cal_date_3yr,
                              (dw.get_last_fund_price_date(f.id)-'5 years'::INTERVAL)::DATE cal_date_5yr,
                              (dw.get_last_fund_price_date(f.id)-'10 years'::INTERVAL)::DATE cal_date_10yr,
                              '2007-10-09'::date cal_date_sftq_start,
                              '2009-03-09'::date cal_date_sftq_end
                              FROM dw.fund f")$result
print(date_ctrl_tbl)

for(i in date_ctrl_tbl$id){
  for(j in colnames(date_ctrl_tbl[,c("cal_date_ytd","cal_date_1m","cal_date_3m","cal_date_6m","cal_date_1yr","cal_date_2yr","cal_date_3yr","cal_date_5yr","cal_date_10yr","cal_date_sftq_start","cal_date_sftq_end")])){
    dt_output <- psqlQuery(sprintf("SELECT MAX(date) dt_output FROM dw.fund_price fp WHERE fp.fund_id=%i AND fp.date<='%s'",i,date_ctrl_tbl[date_ctrl_tbl$id==i,j]))$result 
    if(is.na(dt_output)==T){inp <- format(date_ctrl_tbl[date_ctrl_tbl$id==i,j],"%Y-%m-%d")} else{inp <- format(dt_output[1,1],"%Y-%m-%d")}
    date_ctrl_tbl[date_ctrl_tbl$id==i,gsub('cal_date','date',j)] <- inp
  }
}
message("\n")
message("Calculated dates..")
print(date_ctrl_tbl)

db_dump <- date_ctrl_tbl[,c("id", "date_start", "date_recent", "date_ytd","date_1m","date_3m","date_6m","date_1yr","date_2yr","date_3yr","date_5yr","date_10yr","date_sftq_start","date_sftq_end")]

message("\n")
truncateStatus_ld <- psqlQuery("TRUNCATE TABLE ld.ld_price_dates;")
message(paste0("Truncate ld.ld_price_dates table........",truncateStatus_ld$errorMsg))

insertStatus <- psqlInsert_ld(db_dump, "ld_price_dates")
message(paste0("Calculated dates are being loaded into ld.ld_price_dates table........",insertStatus$errorMsg))

message("\n")
truncateStatus <- psqlQuery("TRUNCATE TABLE rep.fund_summary;")
message(paste0("Truncate rep.fund_summary table........",truncateStatus$errorMsg))

message("\n")
queryString <- sprintf("INSERT INTO rep.fund_summary (fund_id, source_object_id, date_start, date_recent, date_ytd, date_1m, date_3m, date_6m, date_1yr, date_2yr, date_3yr, date_5yr, date_10yr, price_start, price_recent, price_ytd, price_1m, price_3m, price_6m, price_1yr, price_2yr, price_3yr, price_5yr, price_10yr, nav_recent, price_sftq_start, price_sftq_end)
                          SELECT 
                            core.fund_id, core.source_object_id, core.date_start, core.date_recent, core.date_ytd, core.date_1m, core.date_3m, core.date_6m, core.date_1yr, core.date_2yr, core.date_3yr, core.date_5yr, core.date_10yr,
                            fpstart.price price_start,
                            fprecent.price price_recent,
                            fpytd.price price_ytd,
                            fp1m.price price_1m,
                            fp3m.price price_3m,
                            fp6m.price price_6m,
                            fp1y.price price_1yr,
                            fp2y.price price_2yr,
                            fp3y.price price_3yr,
                            fp5y.price price_5yr,
                            fp10y.price price_10yr,
                            fprecent.net_asset_value nav_recent,
                            fpsftqstart.price price_sftq_start,
                            fpsftqend.price price_sftq_end
                          FROM
                            (
                            SELECT
                              f.id fund_id,
                              (SELECT id FROM dw.source_object WHERE constant='%s') source_object_id,
                              lpd.date_start,
                              lpd.date_recent,
                              lpd.date_ytd,
                              lpd.date_1m,
                              lpd.date_3m,
                              lpd.date_6m,
                              lpd.date_1yr,
                              lpd.date_2yr,
                              lpd.date_3yr,
                              lpd.date_5yr,
                              lpd.date_10yr,
                              lpd.date_sftq_start,
                              lpd.date_sftq_end
                            FROM
                              dw.fund f
                              LEFT OUTER JOIN ld.ld_price_dates lpd ON f.id=lpd.fund_id
                            ) core
                            LEFT OUTER JOIN dw.fund_price fpstart ON core.fund_id=fpstart.fund_id AND core.date_start=fpstart.date
                            LEFT OUTER JOIN dw.fund_price fprecent ON core.fund_id=fprecent.fund_id AND core.date_recent=fprecent.date
                            LEFT OUTER JOIN dw.fund_price fpytd ON core.fund_id=fpytd.fund_id AND core.date_ytd=fpytd.date
                            LEFT OUTER JOIN dw.fund_price fp1m ON core.fund_id=fp1m.fund_id AND core.date_1m=fp1m.date
                            LEFT OUTER JOIN dw.fund_price fp3m ON core.fund_id=fp3m.fund_id AND core.date_3m=fp3m.date
                            LEFT OUTER JOIN dw.fund_price fp6m ON core.fund_id=fp6m.fund_id AND core.date_6m=fp6m.date
                            LEFT OUTER JOIN dw.fund_price fp1y ON core.fund_id=fp1y.fund_id AND core.date_1yr=fp1y.date
                            LEFT OUTER JOIN dw.fund_price fp2y ON core.fund_id=fp2y.fund_id AND core.date_2yr=fp2y.date
                            LEFT OUTER JOIN dw.fund_price fp3y ON core.fund_id=fp3y.fund_id AND core.date_3yr=fp3y.date
                            LEFT OUTER JOIN dw.fund_price fp5y ON core.fund_id=fp5y.fund_id AND core.date_5yr=fp5y.date
                            LEFT OUTER JOIN dw.fund_price fp10y ON core.fund_id=fp10y.fund_id AND core.date_10yr=fp10y.date
                            LEFT OUTER JOIN dw.fund_price fpsftqstart ON core.fund_id=fpsftqstart.fund_id AND core.date_sftq_start=fpsftqstart.date
                            LEFT OUTER JOIN dw.fund_price fpsftqend ON core.fund_id=fpsftqend.fund_id AND core.date_sftq_end=fpsftqend.date
                          ;",sou_obj_desc)

execStatus <- psqlQuery(queryString)
message(paste0("Loader is being executed, data is being loaded into rep.fund_summary table........",execStatus$errorMsg))

message("\n")
message(paste("Job ends: ",Sys.time()))
message("----------------------------------------")