message("----------------------------------------")
message(paste("Job starts:",Sys.time()))

message("\n")
message("Importing necessary packages..")
library(xlsx)

message("\n")
message("Importing functions..")
source("/srv/shiny-server/fund4me/db_f.R")

message("\n")
message("Other setups..")
options(scipen = 999, digits = 4, encoding = "iso-8859-2")

message("\n")
message("Source Object setup..")
###############################################################################
sou_obj_desc <- "SO_YC_IMPORT_HUF_BENCHM"
if(sou_obj_desc!=psqlQuery(sprintf("SELECT constant FROM dw.source_object WHERE constant='%s'",sou_obj_desc))$result[1,1]){
  stop("Invalid source object!")
} else {
  sou_obj_id <- psqlQuery(sprintf("SELECT id FROM dw.source_object WHERE constant='%s';",sou_obj_desc))$result[1,1]
}
###############################################################################
message(paste0("Source Object id: ",sou_obj_id))
message(paste0("Source Object constant: ",sou_obj_desc))

retrieve_url <- "http://www.akk.hu/hu/statisztika/hozamok-indexek-forgalmi-adatok/referenciahozamok?download=1"

message("\n")
message("Download file..")
message(paste("Source URL:",retrieve_url))

download.file(url=retrieve_url,destfile = "tmp.xlsx", method = "curl")

message("\n")
message("File processing..")

df_import <- read.xlsx("tmp.xlsx", sheetIndex = 1)
file.remove("tmp.xlsx")

df_import$tenor[df_import[,3]=="M3"] <- 90
df_import$tenor[df_import[,3]=="M6"] <- 180
df_import$tenor[df_import[,3]=="M12"] <- 365
df_import$tenor[df_import[,3]=="Y3"] <- 1095
df_import$tenor[df_import[,3]=="Y5"] <- 1825
df_import$tenor[df_import[,3]=="Y10"] <- 3650
df_import$tenor[df_import[,3]=="Y15"] <- 5475

ld_tbl_dump <- data.frame(
                          currency_id=psqlQuery("SELECT id FROM dw.currency WHERE iso_code='HUF'")$result[1,1],
                          value_date=df_import[,2],
                          tenor=df_import[,13],
                          yield=df_import[,7],
                          yield_curve_type_id=psqlQuery("SELECT id FROM dw.classification WHERE constant='C_YC_TYP_HUFBENCHMARK_RATES'")$result[1,1],
                          source_object_id=sou_obj_id
                        )

message("Downloaded data to be imported:")
print(ld_tbl_dump)

truncateStatus <- psqlQuery("TRUNCATE TABLE ld.ld_yield_curve")
message(paste("Truncate ld.ld_yield_curve..",truncateStatus$errorMsg))

ldInsertStatus <- psqlInsert_ld(ld_tbl_dump, "ld_yield_curve")
message(paste("Insert into ld.ld_yield_curve..",ldInsertStatus$errorMsg))

finalInsertStatus <- psqlQuery("INSERT INTO dw.yield_curve (currency_id, value_date, tenor, yield, yield_curve_type_id, source_object_id)
                                   SELECT 
                                     ldyc.currency_id::INTEGER,
                                     to_date(ldyc.value_date,'yyyy-mm-dd'),
                                     ldyc.tenor::INTEGER,
                                     ldyc.yield::DOUBLE PRECISION,
                                     ldyc.yield_curve_type_id::INTEGER,
                                     ldyc.source_object_id::INTEGER
                                   FROM ld.ld_yield_curve ldyc
                                   LEFT OUTER JOIN dw.yield_curve yc ON ldyc.currency_id::INTEGER=yc.currency_id 
                                     AND ldyc.tenor::INTEGER=yc.tenor 
                                     AND ldyc.yield_curve_type_id::INTEGER=yc.yield_curve_type_id
                                     AND to_date(ldyc.value_date,'yyyy-mm-dd')=yc.value_date
                                   WHERE yc.id IS NULL")

message(paste("Insert into dw.yield_curve..",finalInsertStatus$errorMsg))

message(paste("Job ends:",Sys.time()))
message("----------------------------------------")

