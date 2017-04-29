message("----------------------------------------")
message(paste("Job starts:",Sys.time()))

message("\n")
message("Importing necessary functions for database connection..")
source("/home/ati/Fund4Me/db_f.R")

message("\n")
message("Source Object setup..")
###############################################################################
sou_obj_desc <- "SO_REP_FUNDSUM_LOADER"
sou_obj_id <- psqlQuery(sprintf("SELECT id FROM dw.source_object WHERE constant='%s';",sou_obj_desc))$result[1,1]
###############################################################################
message(paste0("Source Object id: ",sou_obj_id))
message(paste0("Source Object constant: ",sou_obj_desc))

message("\n")
truncateStatus <- psqlQuery("TRUNCATE TABLE rep.fund_summary;")
message(paste0("Truncate rep.fund_summary table........",truncateStatus$errorMsg))

message("\n")
queryString <- "INSERT INTO rep.fund_summary (fund_id, source_object_id, date_start, date_recent, date_ytd, date_1m, date_3m, date_6m, date_1yr, date_2yr, date_3yr, date_5yr, price_start, price_recent, price_ytd, price_1m, price_3m, price_6m, price_1yr, price_2yr, price_3yr, price_5yr)
                SELECT
                  f.id fund_id,
                  (SELECT id FROM dw.source_object WHERE constant='SO_REP_FUNDSUM_LOADER') source_object_id,
                  dw.get_first_fund_price_date(f.id) date_start,
                  dw.get_last_fund_price_date(f.id) date_recent,
                  dw.get_prev_working_day(DATE_TRUNC('year', dw.get_last_fund_price_date(f.id))::DATE,'HU') date_ytd,
                  dw.get_prev_working_day((dw.get_last_fund_price_date(f.id)-'1 month'::INTERVAL)::DATE,'HU') date_1m,
                  dw.get_prev_working_day((dw.get_last_fund_price_date(f.id)-'3 months'::INTERVAL)::DATE,'HU') date_3m,
                  dw.get_prev_working_day((dw.get_last_fund_price_date(f.id)-'6 months'::INTERVAL)::DATE,'HU') date_6m,
                  dw.get_prev_working_day((dw.get_last_fund_price_date(f.id)-'1 year'::INTERVAL)::DATE,'HU') date_1yr,
                  dw.get_prev_working_day((dw.get_last_fund_price_date(f.id)-'2 years'::INTERVAL)::DATE,'HU') date_2yr,
                  dw.get_prev_working_day((dw.get_last_fund_price_date(f.id)-'3 years'::INTERVAL)::DATE,'HU') date_3yr,
                  dw.get_prev_working_day((dw.get_last_fund_price_date(f.id)-'5 years'::INTERVAL)::DATE,'HU') date_5yr,
                  --dw.get_prev_working_day((dw.get_last_fund_price_date(f.id)-'10 years'::INTERVAL)::DATE,'HU') date_10yr,
                  fpstart.price price_start,
                  fprecent.price price_recent,
                  fpytd.price price_ytd,
                  fp1m.price price_1m,
                  fp3m.price price_3m,
                  fp6m.price price_6m,
                  fp1y.price price_1yr,
                  fp2y.price price_2yr,
                  fp3y.price price_3yr,
                  fp5y.price price_5yr--,
                  --NULL price_10yr
                FROM
                  dw.fund f
                  LEFT OUTER JOIN dw.fund_price fpstart ON f.id=fpstart.fund_id AND dw.get_first_fund_price_date(f.id)=fpstart.date
                  LEFT OUTER JOIN dw.fund_price fprecent ON f.id=fprecent.fund_id AND dw.get_last_fund_price_date(f.id)=fprecent.date
                  LEFT OUTER JOIN dw.fund_price fpytd ON f.id=fpytd.fund_id AND dw.get_prev_working_day(DATE_TRUNC('year', dw.get_last_fund_price_date(f.id))::DATE,'HU')=fpytd.date
                  LEFT OUTER JOIN dw.fund_price fp1m ON f.id=fp1m.fund_id AND dw.get_prev_working_day((dw.get_last_fund_price_date(f.id)-'1 month'::INTERVAL)::DATE,'HU')=fp1m.date
                  LEFT OUTER JOIN dw.fund_price fp3m ON f.id=fp3m.fund_id AND dw.get_prev_working_day((dw.get_last_fund_price_date(f.id)-'3 months'::INTERVAL)::DATE,'HU')=fp3m.date
                  LEFT OUTER JOIN dw.fund_price fp6m ON f.id=fp6m.fund_id AND dw.get_prev_working_day((dw.get_last_fund_price_date(f.id)-'6 months'::INTERVAL)::DATE,'HU')=fp6m.date
                  LEFT OUTER JOIN dw.fund_price fp1y ON f.id=fp1y.fund_id AND dw.get_prev_working_day((dw.get_last_fund_price_date(f.id)-'1 year'::INTERVAL)::DATE,'HU')=fp1y.date
                  LEFT OUTER JOIN dw.fund_price fp2y ON f.id=fp2y.fund_id AND dw.get_prev_working_day((dw.get_last_fund_price_date(f.id)-'2 years'::INTERVAL)::DATE,'HU')=fp2y.date
                  LEFT OUTER JOIN dw.fund_price fp3y ON f.id=fp3y.fund_id AND dw.get_prev_working_day((dw.get_last_fund_price_date(f.id)-'3 years'::INTERVAL)::DATE,'HU')=fp3y.date
                  LEFT OUTER JOIN dw.fund_price fp5y ON f.id=fp5y.fund_id AND dw.get_prev_working_day((dw.get_last_fund_price_date(f.id)-'5 years'::INTERVAL)::DATE,'HU')=fp5y.date
                ;"

execStatus <- psqlQuery(queryString)
message(paste0("Loader is being executed, data is being loaded into rep.fund_summary table........",execStatus$errorMsg))

message("\n")
message(paste("Job ends: ",Sys.time()))
message("----------------------------------------")