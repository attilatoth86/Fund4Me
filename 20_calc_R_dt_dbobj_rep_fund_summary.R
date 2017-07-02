message("----------------------------------------")
message(paste("Job starts:",Sys.time()))

message("\n")

# init --------------------------------------------------------------------

message("Importing necessary packages & functions..")
message("\n")

library(PerformanceAnalytics) # https://www.rdocumentation.org/packages/PerformanceAnalytics/
library(plyr)
library(dplyr)
source("/srv/shiny-server/fund4me/db_f.R")

message("\n")
message("Source Object setup..")
###############################################################################
sou_obj_desc <- "SO_REP_FUNDSUMEXT_LOADER"
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
truncateStatus <- psqlQuery("TRUNCATE TABLE ld.ld_fund_summary_pre;")
message(paste0("Truncate rep.fund_summary table........",truncateStatus$errorMsg))

message("\n")
queryString <- sprintf("INSERT INTO ld.ld_fund_summary_pre (fund_id, source_object_id, date_start, date_recent, date_ytd, date_1m, date_3m, date_6m, date_1yr, date_2yr, date_3yr, date_5yr, date_10yr, price_start, price_recent, price_ytd, price_1m, price_3m, price_6m, price_1yr, price_2yr, price_3yr, price_5yr, price_10yr, nav_recent, price_sftq_start, price_sftq_end, date_sftq_start, date_sftq_end)
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
                       fpsftqend.price price_sftq_end,
                       core.date_sftq_start,
                       core.date_sftq_end
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
message(paste0("Loader is being executed, data is being loaded into ld.ld_fund_summary_pre table........",execStatus$errorMsg))

message("\n")

# init data import --------------------------------------------------------

message("Importing data from ld.ld_fund_summary_pre..")
message("\n")

dt_dbobj_rep_fund_summary <- psqlQuery("SELECT fs.fund_id, fs.source_object_id, fs.date_start, 
                                          fs.date_recent, fs.date_ytd, fs.date_1m, fs.date_3m, 
                                          fs.date_6m, fs.date_1yr, fs.date_2yr, fs.date_3yr, 
                                          fs.date_5yr, fs.date_10yr, fs.price_start, fs.price_recent,
                                          fs.price_ytd, fs.price_1m, fs.price_3m, fs.price_6m, fs.price_1yr,
                                          fs.price_2yr, fs.price_3yr, fs.price_5yr, fs.price_10yr,
                                          fs.nav_recent, fs.price_sftq_start, fs.price_sftq_end,
                                          fs.date_sftq_start, fs.date_sftq_end,
                                          f.name, f.short_name, f.isin, f.rec_inv_term,
                                          o.name asset_manager_name, 
                                          c.iso_code currency, 
                                          c_fundcat.description fund_category
                                          FROM ld.ld_fund_summary_pre fs,
                                               dw.fund f,
                                               dw.organization o,
                                               dw.currency c,
                                               dw.classification c_fundcat
                                          WHERE fs.fund_id=f.id 
                                          AND f.fund_manager_id=o.id 
                                          AND f.currency_id=c.id
                                          AND f.fund_category_id=c_fundcat.id
                                         ")$result

message("Importing data from rep.fund_price_daily_analytics..")
message("\n")

dt_dbobj_rep_fund_price_daily_analytics <- psqlQuery("SELECT * FROM rep.fund_price_daily_analytics")$result

# calculate cumulative return ---------------------------------------------

message("Calculating cumulative returns..")
message("\n")

dt_dbobj_rep_fund_summary <- dt_dbobj_rep_fund_summary %>% mutate(return_ytd=round((price_recent/price_ytd-1),digits=4),
                                                                  return_1m=round((price_recent/price_1m-1),digits=4),
                                                                  return_3m=round((price_recent/price_3m-1),digits=4),
                                                                  return_6m=round((price_recent/price_6m-1),digits=4),
                                                                  return_1yr=round((price_recent/price_1yr-1),digits=4),
                                                                  return_2yr=round((price_recent/price_2yr-1),digits=4),
                                                                  return_3yr=round((price_recent/price_3yr-1),digits=4),
                                                                  return_sftq=round((price_sftq_end/price_sftq_start-1),digits=4)
                                                                  )

# calculate annualized return ---------------------------------------------

message("Calculating annualized returns..")
message("\n")

dt_dbobj_rep_fund_summary <- dt_dbobj_rep_fund_summary %>% mutate(return_ann_2yr=round(((price_recent/price_2yr)^(365/(as.numeric(date_recent-date_2yr)+1))-1),digits=4),
                                                                  return_ann_3yr=round(((price_recent/price_3yr)^(365/(as.numeric(date_recent-date_3yr)+1))-1),digits=4),
                                                                  return_ann_5yr=round(((price_recent/price_5yr)^(365/(as.numeric(date_recent-date_5yr)+1))-1),digits=4),
                                                                  return_ann_10yr=round(((price_recent/price_10yr)^(365/(as.numeric(date_recent-date_10yr)+1))-1),digits=4),
                                                                  return_ann_si=round(((price_recent/price_start)^(365/(as.numeric(date_recent-date_start)+1))-1),digits=4)
                                                                  )

# calculate annualized volatility -----------------------------------------

message("Calculating annualized volatility..")
message("\n")

dt_calc_volatility <- data.frame(integer(),double(),double(),double(),double(),double())
for(i_vol_fundid in dt_dbobj_rep_fund_summary$fund_id){
  val_calc_volatility <- vector()
  for(i_vol_date in dt_dbobj_rep_fund_summary[dt_dbobj_rep_fund_summary$fund_id==i_vol_fundid,c("date_1yr","date_2yr","date_3yr","date_5yr","date_10yr")][which(!is.na(dt_dbobj_rep_fund_summary[dt_dbobj_rep_fund_summary$fund_id==i_vol_fundid,c("price_1yr","price_2yr","price_3yr","price_5yr","price_10yr")]))]){
    dt_proc <- dt_dbobj_rep_fund_price_daily_analytics[dt_dbobj_rep_fund_price_daily_analytics$fund_id==i_vol_fundid & dt_dbobj_rep_fund_price_daily_analytics$date>=i_vol_date & is.na(dt_dbobj_rep_fund_price_daily_analytics$return)==F,"return"]
    val_calc_volatility <- c(val_calc_volatility,StdDev.annualized(dt_proc,scale=252))
  }
  dt_calc_volatility <- rbind(dt_calc_volatility,
                              data.frame(fund_id=i_vol_fundid,
                                         volatility_1yr=val_calc_volatility[1],
                                         volatility_2yr=val_calc_volatility[2],
                                         volatility_3yr=val_calc_volatility[3],
                                         volatility_5yr=val_calc_volatility[4],
                                         volatility_10yr=val_calc_volatility[5]
                              )
  )
}
dt_dbobj_rep_fund_summary <- dt_dbobj_rep_fund_summary %>% left_join(dt_calc_volatility, by="fund_id")

### SFTQ Volatility

message("Calculating SFTQ annualized volatility..")
message("\n")

dt_calc_sftq_volatility <- data.frame(integer(),double())
for(i_vol_fundid in dt_dbobj_rep_fund_summary[is.na(dt_dbobj_rep_fund_summary$price_sftq_start)==F,]$fund_id){
    dt_proc <- dt_dbobj_rep_fund_price_daily_analytics[dt_dbobj_rep_fund_price_daily_analytics$fund_id==i_vol_fundid & dt_dbobj_rep_fund_price_daily_analytics$date>=dt_dbobj_rep_fund_summary[dt_dbobj_rep_fund_summary$fund_id==i_vol_fundid,"date_sftq_start"] & dt_dbobj_rep_fund_price_daily_analytics$date<=dt_dbobj_rep_fund_summary[dt_dbobj_rep_fund_summary$fund_id==i_vol_fundid,"date_sftq_end"],"return"]
    val_calc_sftq_volatility <- StdDev.annualized(dt_proc,scale=252)

    dt_calc_sftq_volatility <- rbind(dt_calc_sftq_volatility,
                                      data.frame(fund_id=i_vol_fundid,
                                                 volatility_sftq=val_calc_sftq_volatility[1]
                                      )
                                     )
}

dt_dbobj_rep_fund_summary <- dt_dbobj_rep_fund_summary %>% left_join(dt_calc_sftq_volatility, by="fund_id")


# calculate drawndowns ----------------------------------------------------

message("Calculating drawdowns..")
message("\n")

dt_calc_drawdown <- data.frame(integer(),as.Date(character()),as.Date(character()),as.Date(character()),double(),integer(),integer(),integer())
for(i_dd_fundid in dt_dbobj_rep_fund_summary$fund_id){
  dt_proc <- dt_dbobj_rep_fund_price_daily_analytics[dt_dbobj_rep_fund_price_daily_analytics$fund_id==i_dd_fundid,c("date","return")]
  rownames(dt_proc) <- dt_proc$date
  dt_proc$date <- NULL
  dt_out <- data.frame(fund_id=i_dd_fundid,table.Drawdowns(dt_proc)[1,])
  dt_calc_drawdown <- rbind(dt_calc_drawdown,
                            data.frame(fund_id=dt_out[1,1],
                                       drawdown_from=dt_out[1,2],
                                       drawdown_trough=dt_out[1,3],
                                       drawdown_to=dt_out[1,4],
                                       drawdown_depth=dt_out[1,5],
                                       drawdown_length=dt_out[1,6],
                                       drawdown_totrough=dt_out[1,7],
                                       drawdown_recovery=dt_out[1,8])
  )
}

dt_dbobj_rep_fund_summary <- dt_dbobj_rep_fund_summary %>% left_join(dt_calc_drawdown, by="fund_id")

### SFTQ Drawdown

message("Calculating SFTQ drawdowns..")
message("\n")

dt_calc_sftq_drawdown <- data.frame(integer(),double(),integer())
for(i_dd_fundid in dt_dbobj_rep_fund_summary[is.na(dt_dbobj_rep_fund_summary$price_sftq_start)==F,]$fund_id){
  dt_proc <- dt_dbobj_rep_fund_price_daily_analytics[dt_dbobj_rep_fund_price_daily_analytics$fund_id==i_dd_fundid & dt_dbobj_rep_fund_price_daily_analytics$date>=dt_dbobj_rep_fund_summary[dt_dbobj_rep_fund_summary$fund_id==i_dd_fundid,"date_sftq_start"] & dt_dbobj_rep_fund_price_daily_analytics$date<=dt_dbobj_rep_fund_summary[dt_dbobj_rep_fund_summary$fund_id==i_dd_fundid,"date_sftq_end"],c("date","return")]
  rownames(dt_proc) <- dt_proc$date
  dt_proc$date <- NULL
  dt_out <- data.frame(fund_id=i_dd_fundid,table.Drawdowns(dt_proc)[1,])
  dt_calc_sftq_drawdown <- rbind(dt_calc_sftq_drawdown,
                            data.frame(fund_id=dt_out[1,1],
                                       drawdown_sftq_depth=dt_out[1,5],
                                       drawdown_sftq_length=dt_out[1,6])
                            )
}

dt_dbobj_rep_fund_summary <- dt_dbobj_rep_fund_summary %>% left_join(dt_calc_sftq_drawdown, by="fund_id")

# calculate Sharpe Ratio --------------------------------------------------

message("Calculating Sharpe Ratio..")
message("\n")

dt_calc_sharpe <- data.frame(integer(),double(),double(),double(),double())
for(i_sr_fundid in dt_dbobj_rep_fund_summary$fund_id){
  val_calc_sharpe <- vector()
  for(i_sr_date in dt_dbobj_rep_fund_summary[dt_dbobj_rep_fund_summary$fund_id==i_sr_fundid,c("date_1yr","date_2yr","date_3yr","date_5yr")][which(!is.na(dt_dbobj_rep_fund_summary[dt_dbobj_rep_fund_summary$fund_id==i_sr_fundid,c("price_1yr","price_2yr","price_3yr","price_5yr")]))]){
    dt_proc <- dt_dbobj_rep_fund_price_daily_analytics[dt_dbobj_rep_fund_price_daily_analytics$fund_id==i_sr_fundid & dt_dbobj_rep_fund_price_daily_analytics$date>=i_sr_date & is.na(dt_dbobj_rep_fund_price_daily_analytics$return)==F,]
    rownames(dt_proc) <- dt_proc$date
    dt_proc$date <- NULL
    val_calc_sharpe <- c(val_calc_sharpe,SharpeRatio.annualized(R=dt_proc["return"],Rf=dt_proc["return_rf"]))
  }
  dt_calc_sharpe <- rbind(dt_calc_sharpe,
                              data.frame(fund_id=i_sr_fundid,
                                         sharpe_ratio_1yr=val_calc_sharpe[1],
                                         sharpe_ratio_2yr=val_calc_sharpe[2],
                                         sharpe_ratio_3yr=val_calc_sharpe[3],
                                         sharpe_ratio_5yr=val_calc_sharpe[4]
                              )
  )
}

dt_dbobj_rep_fund_summary <- dt_dbobj_rep_fund_summary %>% left_join(dt_calc_sharpe, by="fund_id")

# file operations ---------------------------------------------------------

message("Archiving existing file..")
message(list.files(path = "/srv/shiny-server/fund4me/RDS_dt_dbobj_rep_fund_summary/", pattern = ".rds", recursive = F)[1])
message("\n")

if(is.na(list.files(path = "/srv/shiny-server/fund4me/RDS_dt_dbobj_rep_fund_summary/", pattern = ".rds", recursive = F)[1])==F){
  system(paste0("mv /srv/shiny-server/fund4me/RDS_dt_dbobj_rep_fund_summary/",
                list.files(path = "/srv/shiny-server/fund4me/RDS_dt_dbobj_rep_fund_summary/", pattern = ".rds", recursive = F)[1],
                " /srv/shiny-server/fund4me/RDS_dt_dbobj_rep_fund_summary/_arch/"))
}

message("Saving dataframe into file..")

saveRDS(dt_dbobj_rep_fund_summary, 
        sprintf("/srv/shiny-server/fund4me/RDS_dt_dbobj_rep_fund_summary/dt_db_obj_rep_fund_summary_%s.rds",format(Sys.time(),"%Y%m%d%H%M%S")))

message(list.files(path = "/srv/shiny-server/fund4me/RDS_dt_dbobj_rep_fund_summary/", pattern = ".rds", recursive = F)[1])
message("\n")

# database operations -----------------------------------------------------

df_to_load <- data.frame(fund_id=dt_dbobj_rep_fund_summary$fund_id,
                         source_object_id=sou_obj_id,
                         date_start=dt_dbobj_rep_fund_summary$date_start,
                         date_recent=dt_dbobj_rep_fund_summary$date_recent,
                         date_ytd=dt_dbobj_rep_fund_summary$date_ytd,
                         date_1m=dt_dbobj_rep_fund_summary$date_1m,
                         date_3m=dt_dbobj_rep_fund_summary$date_3m,
                         date_6m=dt_dbobj_rep_fund_summary$date_6m,
                         date_1yr=dt_dbobj_rep_fund_summary$date_1yr,
                         date_2yr=dt_dbobj_rep_fund_summary$date_2yr,
                         date_3yr=dt_dbobj_rep_fund_summary$date_3yr,
                         date_5yr=dt_dbobj_rep_fund_summary$date_5yr,
                         date_10yr=dt_dbobj_rep_fund_summary$date_10yr,
                         price_start=dt_dbobj_rep_fund_summary$price_start,
                         price_recent=dt_dbobj_rep_fund_summary$price_recent,
                         price_ytd=dt_dbobj_rep_fund_summary$price_ytd,
                         price_1m=dt_dbobj_rep_fund_summary$price_1m,
                         price_3m=dt_dbobj_rep_fund_summary$price_3m,
                         price_6m=dt_dbobj_rep_fund_summary$price_6m,
                         price_1yr=dt_dbobj_rep_fund_summary$price_1yr,
                         price_2yr=dt_dbobj_rep_fund_summary$price_2yr,
                         price_3yr=dt_dbobj_rep_fund_summary$price_3yr,
                         price_5yr=dt_dbobj_rep_fund_summary$price_5yr,
                         price_10yr=dt_dbobj_rep_fund_summary$price_10yr,
                         nav_recent=dt_dbobj_rep_fund_summary$nav_recent,
                         name=dt_dbobj_rep_fund_summary$name,
                         short_name=dt_dbobj_rep_fund_summary$short_name,
                         isin=dt_dbobj_rep_fund_summary$isin,
                         asset_manager_name=dt_dbobj_rep_fund_summary$asset_manager_name,
                         currency=dt_dbobj_rep_fund_summary$currency,
                         fund_category=dt_dbobj_rep_fund_summary$fund_category,
                         return_ytd=dt_dbobj_rep_fund_summary$return_ytd,
                         return_1m=dt_dbobj_rep_fund_summary$return_1m,
                         return_3m=dt_dbobj_rep_fund_summary$return_3m,
                         return_6m=dt_dbobj_rep_fund_summary$return_6m,
                         return_1yr=dt_dbobj_rep_fund_summary$return_1yr,
                         return_2yr=dt_dbobj_rep_fund_summary$return_2yr,
                         return_3yr=dt_dbobj_rep_fund_summary$return_3yr,
                         return_ann_2yr=dt_dbobj_rep_fund_summary$return_ann_2yr,
                         return_ann_3yr=dt_dbobj_rep_fund_summary$return_ann_3yr,
                         return_ann_5yr=dt_dbobj_rep_fund_summary$return_ann_5yr,
                         return_ann_10yr=dt_dbobj_rep_fund_summary$return_ann_10yr,
                         return_ann_si=dt_dbobj_rep_fund_summary$return_ann_si,
                         volatility_1yr=dt_dbobj_rep_fund_summary$volatility_1yr,
                         volatility_2yr=dt_dbobj_rep_fund_summary$volatility_2yr,
                         volatility_3yr=dt_dbobj_rep_fund_summary$volatility_3yr,
                         volatility_5yr=dt_dbobj_rep_fund_summary$volatility_5yr,
                         volatility_10yr=dt_dbobj_rep_fund_summary$volatility_10yr,
                         drawdown_from=dt_dbobj_rep_fund_summary$drawdown_from,
                         drawdown_trough=dt_dbobj_rep_fund_summary$drawdown_trough,
                         drawdown_to=dt_dbobj_rep_fund_summary$drawdown_to,
                         drawdown_depth=dt_dbobj_rep_fund_summary$drawdown_depth,
                         drawdown_length=dt_dbobj_rep_fund_summary$drawdown_length,
                         drawdown_totrough=dt_dbobj_rep_fund_summary$drawdown_totrough,
                         drawdown_recovery=dt_dbobj_rep_fund_summary$drawdown_recovery,
                         return_sftq=dt_dbobj_rep_fund_summary$return_sftq,
                         price_sftq_start=dt_dbobj_rep_fund_summary$price_sftq_start,
                         price_sftq_end=dt_dbobj_rep_fund_summary$price_sftq_end,
                         date_sftq_start=dt_dbobj_rep_fund_summary$date_sftq_start,
                         date_sftq_end=dt_dbobj_rep_fund_summary$date_sftq_end,
                         volatility_sftq=dt_dbobj_rep_fund_summary$volatility_sftq,
                         drawdown_sftq_depth=dt_dbobj_rep_fund_summary$drawdown_sftq_depth,
                         drawdown_sftq_length=dt_dbobj_rep_fund_summary$drawdown_sftq_length
                         )

truncateStatus <- psqlQuery("TRUNCATE TABLE ld.ld_fund_summary_ext;")
message(paste0("Truncate ld.ld_fund_summary_ext table........",truncateStatus$errorMsg))
insertLdStatus <- psqlInsert_ld(df_to_load,"ld_fund_summary_ext")
message(paste0("Insert into ld.ld_fund_summary_ext table........",insertLdStatus$errorMsg))
insertStatus <- psqlQuery("INSERT INTO rep.fund_summary_ext (fund_id,source_object_id,date_start,date_recent,date_ytd,date_1m,date_3m,date_6m,date_1yr,date_2yr,date_3yr,date_5yr,date_10yr,price_start,price_recent,price_ytd,price_1m,price_3m,price_6m,price_1yr,price_2yr,price_3yr,price_5yr,price_10yr,nav_recent,name,short_name,isin,asset_manager_name,currency,fund_category,return_ytd,return_1m,return_3m,return_6m,return_1yr,return_2yr,return_3yr,return_ann_2yr,return_ann_3yr,return_ann_5yr,return_ann_10yr,return_ann_si,volatility_1yr,volatility_2yr,volatility_3yr,volatility_5yr,volatility_10yr,drawdown_from,drawdown_trough,drawdown_to,drawdown_depth,drawdown_length,drawdown_totrough,drawdown_recovery,return_sftq,price_sftq_start,price_sftq_end,date_sftq_start,date_sftq_end,volatility_sftq,drawdown_sftq_depth,drawdown_sftq_length)
                          SELECT fund_id,source_object_id,date_start,date_recent,date_ytd,date_1m,date_3m,date_6m,date_1yr,date_2yr,date_3yr,date_5yr,date_10yr,price_start,price_recent,price_ytd,price_1m,price_3m,price_6m,price_1yr,price_2yr,price_3yr,price_5yr,price_10yr,nav_recent,name,short_name,isin,asset_manager_name,currency,fund_category,return_ytd,return_1m,return_3m,return_6m,return_1yr,return_2yr,return_3yr,return_ann_2yr,return_ann_3yr,return_ann_5yr,return_ann_10yr,return_ann_si,volatility_1yr,volatility_2yr,volatility_3yr,volatility_5yr,volatility_10yr,drawdown_from,drawdown_trough,drawdown_to,drawdown_depth,drawdown_length,drawdown_totrough,drawdown_recovery,return_sftq,price_sftq_start,price_sftq_end,date_sftq_start,date_sftq_end,volatility_sftq,drawdown_sftq_depth,drawdown_sftq_length FROM ld.ld_fund_summary_ext") 
message(paste0("Insert into rep.fund_summary_ext table........",insertStatus$errorMsg))

message(paste("Job ends: ",Sys.time()))
message("----------------------------------------")