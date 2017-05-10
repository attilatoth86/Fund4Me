
source("db_f.R")

### USER VARIABLES ############################################################
fund_short_name <- "AEGON Polish Bond Fund"
file_path <- "/home/ati/Fund4Me/data_source/AEGON Lengyel Kötvény.csv"
no_skip_lines <- 18 # default should be 18
###############################################################################

if(fund_short_name!=psqlQuery(sprintf("SELECT short_name FROM dw.fund WHERE short_name='%s'",fund_short_name))$result[1,1]){
  stop("Invalid fund short name!")
}

if(is.na(file.size(file_path)) | file.size(file_path)==0){
  stop("Invalid file path!")
}

### SOURCE OBJECT SETUP #######################################################
sou_obj_desc <- "SO_ADHOC_BAMOSZ_FP_CSV"
if(sou_obj_desc!=psqlQuery(sprintf("SELECT constant FROM dw.source_object WHERE constant='%s'",sou_obj_desc))$result[1,1]){
  stop("Invalid source object!")
} else {
  sou_obj_id <- psqlQuery(sprintf("SELECT id FROM dw.source_object WHERE constant='%s';",sou_obj_desc))$result[1,1]  
}
###############################################################################



data_import <- read.csv(file_path,sep = ";", skip = no_skip_lines, header = F)[,c(1:4)]

colnames(data_import) <- c("date","price","net_asset_value","paid_dividend")
data_import$date <- as.Date(data_import$date, "%Y/%m/%d")
data_import$price <- as.numeric(gsub(",",".",data_import$price))
data_import$net_asset_value <- as.numeric(data_import$net_asset_value)
data_import$paid_dividend <- as.numeric(data_import$paid_dividend)
data_import[is.na(data_import$paid_dividend),"paid_dividend"] <- 0
data_import$dw_fund_id <- psqlQuery(sprintf("SELECT id FROM dw.fund WHERE short_name='%s'",fund_short_name))$result[1,1]
data_import$dw_fund_short_name <- fund_short_name
data_import$source_object_id <- sou_obj_id
data_import$source_object_desc <- sou_obj_desc
data_import <- data_import[is.na(data_import$price)==F,]

psqlQuery("TRUNCATE ld.ld_bamosz_fund_price;")

psqlInsert_ld(data_import,"ld_bamosz_fund_price")

psqlQuery("INSERT INTO dw.fund_price (date, fund_id, price, net_asset_value, paid_dividend, source_object_id)
          SELECT date, dw_fund_id, price, net_asset_value, paid_dividend, source_object_id FROM ld.ld_bamosz_fund_price")

