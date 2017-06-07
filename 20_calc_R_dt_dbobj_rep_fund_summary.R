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

# init data import --------------------------------------------------------

message("Importing data from rep.fund_summary..")
message("\n")

dt_dbobj_rep_fund_summary <- psqlQuery("SELECT fs.*, 
                                          f.name, f.short_name, f.isin, 
                                          o.name asset_manager_name, 
                                          c.iso_code currency, 
                                          c_fundcat.description fund_category
                                          FROM rep.fund_summary fs,
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
                                                                  return_3yr=round((price_recent/price_3yr-1),digits=4)
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

df_to_load <- data.frame(fund_summary_id=dt_dbobj_rep_fund_summary$id,
                         fund_id=dt_dbobj_rep_fund_summary$fund_id,
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
                         drawdown_recovery=dt_dbobj_rep_fund_summary$drawdown_recovery)

truncateStatus <- psqlQuery("TRUNCATE TABLE ld.ld_fund_summary_ext;")
message(paste0("Truncate ld.ld_fund_summary_ext table........",truncateStatus$errorMsg))
insertLdStatus <- psqlInsert_ld(df_to_load,"ld_fund_summary_ext")
message(paste0("Insert into ld.ld_fund_summary_ext table........",insertLdStatus$errorMsg))
insertStatus <- psqlQuery("INSERT INTO rep.fund_summary_ext (fund_summary_id,fund_id,source_object_id,date_start,date_recent,date_ytd,date_1m,date_3m,date_6m,date_1yr,date_2yr,date_3yr,date_5yr,date_10yr,price_start,price_recent,price_ytd,price_1m,price_3m,price_6m,price_1yr,price_2yr,price_3yr,price_5yr,price_10yr,nav_recent,name,short_name,isin,asset_manager_name,currency,fund_category,return_ytd,return_1m,return_3m,return_6m,return_1yr,return_2yr,return_3yr,return_ann_2yr,return_ann_3yr,return_ann_5yr,return_ann_10yr,return_ann_si,volatility_1yr,volatility_2yr,volatility_3yr,volatility_5yr,volatility_10yr,drawdown_from,drawdown_trough,drawdown_to,drawdown_depth,drawdown_length,drawdown_totrough,drawdown_recovery)
                          SELECT fund_summary_id,fund_id,source_object_id,date_start,date_recent,date_ytd,date_1m,date_3m,date_6m,date_1yr,date_2yr,date_3yr,date_5yr,date_10yr,price_start,price_recent,price_ytd,price_1m,price_3m,price_6m,price_1yr,price_2yr,price_3yr,price_5yr,price_10yr,nav_recent,name,short_name,isin,asset_manager_name,currency,fund_category,return_ytd,return_1m,return_3m,return_6m,return_1yr,return_2yr,return_3yr,return_ann_2yr,return_ann_3yr,return_ann_5yr,return_ann_10yr,return_ann_si,volatility_1yr,volatility_2yr,volatility_3yr,volatility_5yr,volatility_10yr,drawdown_from,drawdown_trough,drawdown_to,drawdown_depth,drawdown_length,drawdown_totrough,drawdown_recovery FROM ld.ld_fund_summary_ext") 
message(paste0("Insert into rep.fund_summary_ext table........",insertStatus$errorMsg))

message(paste("Job ends: ",Sys.time()))
message("----------------------------------------")