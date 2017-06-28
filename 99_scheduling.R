
##### External Sourcing
#########################

### Job 1
## desc: BAMOSZ fund price import job (daily)
system("Rscript /srv/shiny-server/fund4me/01_BAMOSZ_fund_import.R  >> /home/ati/Fund4Me/01_BAMOSZ_fund_import.log 2>&1")

### Job 2
## desc: AKK Zero Coupon Yield Curve import script
system("Rscript /srv/shiny-server/fund4me/02_AKK_HUF_ZCYieldCurve_import.R  >> /home/ati/Fund4Me/02_AKK_HUF_ZCYieldCurve_import.log 2>&1")

### Job 3
## desc: AKK Benchmark Yields import script
system("Rscript /srv/shiny-server/fund4me/03_AKK_HUF_BenchmarkYields_import.R  >> /home/ati/Fund4Me/03_AKK_HUF_BenchmarkYields_import.log 2>&1")

##### Calculations
#########################

### Job 4
## desc: Reporting Layer Loader (rep.fund_price_daily_analytics)                                                                                         
system("Rscript /srv/shiny-server/fund4me/10_rep_fund_price_daily_analytics_loader.R  >> /home/ati/Fund4Me/10_rep_fund_price_daily_analytics_loader.log 2>&1")

### Job 5
## desc: Reporting Layer Loader (rep.fund_summary)                                                                                                       
system("Rscript /srv/shiny-server/fund4me/11_rep_fund_summary_loader.R  >> /home/ati/Fund4Me/11_rep_fund_summary_loader.log 2>&1")

### Job 6
## desc: Fund Summary object creator (dt_db_obj_rep_fund_summary)                                                                                        
system("Rscript /srv/shiny-server/fund4me/20_calc_R_dt_dbobj_rep_fund_summary.R  >> /home/ati/Fund4Me/20_calc_R_dt_dbobj_rep_fund_summary.log 2>&1")


