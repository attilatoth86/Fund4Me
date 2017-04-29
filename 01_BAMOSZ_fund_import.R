message("----------------------------------------")
message(paste("Job starts:",Sys.time()))

message("\n")
message("Importing necessary packages..")
library(RCurl)
library(XML)

message("\n")
message("Importing necessary functions for database connection..")
source("/home/ati/Fund4Me/db_f.R")

message("\n")
message("Source Object setup..")
###############################################################################
sou_obj_desc <- "SO_BAMOSZ_FP_IMP"
sou_obj_id <- psqlQuery(sprintf("SELECT id FROM dw.source_object WHERE constant='%s';",sou_obj_desc))$result[1,1]
###############################################################################
message(paste0("Source Object id: ",sou_obj_id))
message(paste0("Source Object constant: ",sou_obj_desc))

message("\n")
message("Creating import control table..")
ctrl_df <- psqlQuery("SELECT id, short_name, isin FROM dw.fund")$result
print(ctrl_df)

message("\n")
message("Importing & processing..")
for(i_fundid in ctrl_df$id){
  message(paste0("..processing: ",ctrl_df[ctrl_df$id==i_fundid,"short_name"]))
  retrieve_url <- sprintf("http://www.bamosz.hu/en/alapoldal?isin=%s",ctrl_df[ctrl_df$id==i_fundid,"isin"])
  message(paste0("..reading in from URL: ",retrieve_url))
  web_import <- getURL(retrieve_url)
  df_import <- readHTMLTable(web_import, trim = T, header = F, as.data.frame = T, stringsAsFactors=F)[[7]][-1,c(1:4)]
  
  colnames(df_import) <- c("date","price","net_asset_value","paid_dividend")
  df_import$date <- as.Date(df_import$date,"%Y.%m.%d.")
  df_import$price <- as.numeric(df_import$price)
  df_import$net_asset_value <- as.numeric(gsub(",","",df_import$net_asset_value))
  df_import$paid_dividend <- as.numeric(df_import$paid_dividend)
  df_import$dw_fund_id <- i_fundid
  df_import$dw_fund_short_name <- ctrl_df[ctrl_df$id==i_fundid,"short_name"]
  df_import$source_object_id <- sou_obj_id
  df_import$source_object_desc <- sou_obj_desc
  
  message("Fetched data (first 30): ")
  print(head(df_import,30))
  
  db_cont <- psqlQuery(sprintf("SELECT date FROM dw.fund_price WHERE fund_id=%i",i_fundid))$result
  
  df_to_load <- df_import[!(df_import$date %in% db_cont$date),]
  
  message("Content to load in db: ")
  print(df_to_load)
  
  if(nrow(df_to_load)>0)
  {
    truncateStatus <- psqlQuery("TRUNCATE TABLE ld.ld_bamosz_fund_price;")
    message(paste0("Truncate ld.ld_bamosz_fund_price table........",truncateStatus$errorMsg))
    insertLdStatus <- psqlInsert_ld(df_to_load,"ld_bamosz_fund_price")
    message(paste0("Insert into ld.ld_bamosz_fund_price table........",insertLdStatus$errorMsg))
    insertStatus <- psqlQuery("INSERT INTO dw.fund_price (date, fund_id, price, net_asset_value, paid_dividend, source_object_id)
              SELECT date, dw_fund_id, price, net_asset_value, paid_dividend, source_object_id FROM ld.ld_bamosz_fund_price") 
    message(paste0("Insert into dw.fund_price table........",insertStatus$errorMsg))
  }
  message("\n")
}

message("\n")
message(paste("Job ends: ",Sys.time()))
message("----------------------------------------")
