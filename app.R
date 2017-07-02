
# Init load libraries -----------------------------------------------------

library(shiny) # http://shiny.rstudio.com
library(shinydashboard) # https://rstudio.github.io/shinydashboard/
library(plotly) # https://plot.ly
library(DT) # https://rstudio.github.io/DT/
library(plyr) # http://had.co.nz/plyr/
library(dplyr) # http://dplyr.tidyverse.org

# Init source external resources ------------------------------------------

source("db_f.R")
source("f_UI_builder.R")

# SERV init ---------------------------------------------------------------

server <- function(input, output, session) {

# SERV progress bar -------------------------------------------------------

  progress <- Progress$new(session, min=0, max=14)
  on.exit(progress$close())
  progress$set(message = 'Fund4Me is loading data in the background.',
               detail = 'You may use the application meanwhile..')

# SERV retrieve dt_dbobj_rep_fund_price_daily_analytics -------------------

  progress$set(value = 0) #######################################################################################
  dt_dbobj_rep_fund_price_daily_analytics <- psqlQuery("SELECT * FROM rep.fund_price_daily_analytics")$result

# SERV retrieve dt_dbobj_rep_fund_summary ---------------------------------

  progress$set(value = 1) #######################################################################################
  dt_dbobj_rep_fund_summary <- readRDS(paste0("/srv/shiny-server/fund4me/RDS_dt_dbobj_rep_fund_summary/",
                                              list.files(path = "/srv/shiny-server/fund4me/RDS_dt_dbobj_rep_fund_summary/", pattern = ".rds", recursive = F)[1]
                                              )
                                      )

# SERV prep DT cumulative return ------------------------------------------

  progress$set(value = 2) #######################################################################################
  disp_DT_cum_return <- dt_dbobj_rep_fund_summary %>% select("Fund Name"=short_name, "YTD"=return_ytd, "1M"=return_1m, "3M"=return_3m, "6M"=return_6m,
                                                             "1Y"=return_1yr, "2Y"=return_2yr, "3Y"=return_3yr)

# SERV prep DT annualized return ------------------------------------------

  progress$set(value = 3) #######################################################################################
  disp_DT_ann_return <- dt_dbobj_rep_fund_summary %>% select("Fund Name"=short_name, "2Y"=return_ann_2yr, "3Y"=return_ann_3yr, "5Y"=return_ann_5yr, "10Y"=return_ann_10yr)

# SERV prep DT annualized volatility --------------------------------------

  progress$set(value = 4) #######################################################################################
  disp_DT_volatility <- dt_dbobj_rep_fund_summary %>% select("Fund Name"=short_name, "1Y"=volatility_1yr, "2Y"=volatility_2yr, "3Y"=volatility_3yr, "5Y"=volatility_5yr, "10Y"=volatility_10yr)

# SERV prep DT drawdown ---------------------------------------------------

  progress$set(value = 5) #######################################################################################
  disp_DT_drawdown <- dt_dbobj_rep_fund_summary %>% select("Fund Name"=short_name, "Depth"=drawdown_depth, "Peak"=drawdown_from, "Trough"=drawdown_trough)
  disp_DT_drawdown$`Peak` <- as.character(disp_DT_drawdown$`Peak`)
  disp_DT_drawdown$`Trough` <- as.character(disp_DT_drawdown$`Trough`)

# SERV prep DT Sharpe Ratio ---------------------------------------------

  disp_DT_sharpe <- dt_dbobj_rep_fund_summary %>% select("Fund Name"=short_name, "1Y"=sharpe_ratio_1yr, "2Y"=sharpe_ratio_2yr, "3Y"=sharpe_ratio_3yr, "5Y"=sharpe_ratio_5yr)

# SERV prep Recession Proof ---------------------------------------------

  progress$set(value = 6) #######################################################################################
  
  sftq_fundid_filter <- dt_dbobj_rep_fund_summary[is.na(dt_dbobj_rep_fund_summary$return_sftq)==F,"fund_id"]
  
  disp_DT_sftq_perf <- dt_dbobj_rep_fund_summary %>% filter(fund_id %in% sftq_fundid_filter) %>% mutate(drawdown_sftq_length_rep=paste(drawdown_sftq_length,"days")) %>% arrange(desc(return_sftq)) %>% select("Fund Name"=short_name, "Return"=return_sftq, "Volatility"=volatility_sftq, "Max. Drawdown"=drawdown_sftq_depth, "Max. Drawdown Length"=drawdown_sftq_length_rep)
  
  plot_df_sftq_price <- dt_dbobj_rep_fund_price_daily_analytics %>%
    filter(fund_id %in% sftq_fundid_filter & 
           date>="2007-10-09" & date<="2009-03-09") %>%
    left_join(dt_dbobj_rep_fund_summary, c("fund_id" = "fund_id")) %>%
    select(fund_id, short_name, date, price) %>%
    left_join(
      dt_dbobj_rep_fund_price_daily_analytics %>% inner_join(
        dt_dbobj_rep_fund_price_daily_analytics %>%
          filter(fund_id %in% sftq_fundid_filter &date>="2007-10-09") %>%
          group_by(fund_id) %>%
          summarise(date=min(date))
        , by = c("fund_id","date")) %>% select(fund_id, start_price=price)
      ,by=c("fund_id")
    ) %>%
    mutate(price_chg_pct=price/start_price-1) %>% select(fund_id, short_name, date, price, price_chg_pct)
  
  plot_obj_price_sftq <- plot_ly() %>% layout(xaxis=list(title=""), yaxis=list(title="Price Change %"), hovermode = 'closest')
  for(i_sftq_fundid in sftq_fundid_filter) {  
    plot_obj_price_sftq <- add_trace(p = plot_obj_price_sftq,
                                    data=plot_df_sftq_price[plot_df_sftq_price$fund_id==i_sftq_fundid,], 
                                    x=~date, 
                                    y=~price_chg_pct*100,
                                    name=dt_dbobj_rep_fund_summary[dt_dbobj_rep_fund_summary$fund_id==i_sftq_fundid,"short_name"],
                                    visible={if(dt_dbobj_rep_fund_summary[dt_dbobj_rep_fund_summary$fund_id==i_sftq_fundid,"return_sftq"]>0) TRUE else "legendonly"},
                                    mode="lines",
                                    evaluate = TRUE)
  }

# SERV creating output$ objects - Return & Risk Summary --------------------

  progress$set(value = 7) #######################################################################################
  output$DT_cum_return <- DT::renderDataTable(DT::datatable(disp_DT_cum_return, extensions = "Responsive", options = list(language = list(info = "_START_ to _END_ of _TOTAL_", paginate = list(previous = "<<", `next` = ">>")), ordering=T, order = list(list(5, 'desc')), pageLength = 5, bLengthChange=F, searching=F, paging=T, scrollX = T), rownames=F) %>% formatPercentage(c("YTD","1M","3M","6M","1Y","2Y","3Y"),2))
  output$DT_ann_return <- DT::renderDataTable(DT::datatable(disp_DT_ann_return, extensions = "Responsive", options = list(language = list(info = "_START_ to _END_ of _TOTAL_", paginate = list(previous = "<<", `next` = ">>")), ordering=T, order = list(list(1, 'desc')), pageLength = 5, bLengthChange=F, searching=F, paging=T, scrollX = T), rownames=F) %>% formatPercentage(c("2Y","3Y","5Y","10Y"),2))
  output$DT_volatility <- DT::renderDataTable(DT::datatable(disp_DT_volatility, extensions = "Responsive", options = list(language = list(info = "_START_ to _END_ of _TOTAL_", paginate = list(previous = "<<", `next` = ">>")), ordering=T, order = list(list(4, 'asc')), pageLength = 5, bLengthChange=F, searching=F, paging=T, scrollX = T), rownames=F) %>% formatPercentage(c("1Y","2Y","3Y","5Y","10Y"),2))
  output$DT_drawdown <- DT::renderDataTable(DT::datatable(disp_DT_drawdown, extensions = "Responsive", options = list(language = list(info = "_START_ to _END_ of _TOTAL_", paginate = list(previous = "<<", `next` = ">>")), ordering=T, order = list(list(1, 'desc')), pageLength = 5, bLengthChange=F, searching=F, paging=T, scrollX = T, columnDefs = list(list(className = 'dt-center', targets = 1:3))), rownames=F) %>% formatPercentage(c("Depth"),2))
  output$DT_sharpe_ratio <- DT::renderDataTable(DT::datatable(disp_DT_sharpe, extensions = "Responsive", options = list(language = list(info = "_START_ to _END_ of _TOTAL_", paginate = list(previous = "<<", `next` = ">>")), ordering=T, order = list(list(4, 'desc')), pageLength = 5, bLengthChange=F, searching=F, paging=T, scrollX = T), rownames=F) %>% formatRound(c("1Y","2Y","3Y","5Y"),3))

# SERV creating output$ objects - LT price change plots --------------------

  progress$set(value = 8) #######################################################################################
  lapply(dt_dbobj_rep_fund_summary$fund_id, function(i_fundid){
    dt_proc <- dt_dbobj_rep_fund_price_daily_analytics[dt_dbobj_rep_fund_price_daily_analytics$fund_id==i_fundid,] %>% arrange(date)
    output[[paste0("PLT_price_chg_pct_fund_",i_fundid)]] <- plotly::renderPlotly(plot_ly() %>% add_trace(data=dt_proc, x=~date, y=~price, mode="lines") %>% layout(xaxis=list(title=""), yaxis=list(title="Unit Price")))
  })

# SERV creating output$ objects - Fund info boxes --------------------------

  progress$set(value = 9) #######################################################################################
  lapply(dt_dbobj_rep_fund_summary$fund_id, function(i_fundid){
    output[[paste0("UI_box_summary_fund_",i_fundid)]] <- renderUI({
      box(width = 6,
          id=paste0("fund_card_",i_fundid),
          collapsible = T, collapsed = F,
          status = "primary",
          title = dt_dbobj_rep_fund_summary[dt_dbobj_rep_fund_summary$fund_id==i_fundid,"name"],
          tags$table(
            tags$col(width="180"),
            tags$col(width="270"),
            tags$tr(
              tags$td(class="fund-card-upper-td",tags$b("ISIN")),
              tags$td(class="fund-card-upper-td",
                      dt_dbobj_rep_fund_summary[dt_dbobj_rep_fund_summary$fund_id==i_fundid,"isin"])
            ),
            tags$tr(
              tags$td(class="fund-card-upper-td",tags$b("Fund Category")),
              tags$td(class="fund-card-upper-td",
                      dt_dbobj_rep_fund_summary[dt_dbobj_rep_fund_summary$fund_id==i_fundid,"fund_category"])
            ),
            tags$tr(
              tags$td(class="fund-card-upper-td",tags$b("Asset Manager")),
              tags$td(class="fund-card-upper-td",
                      dt_dbobj_rep_fund_summary[dt_dbobj_rep_fund_summary$fund_id==i_fundid,"asset_manager_name"])
            ),
            tags$tr(
              tags$td(class="fund-card-upper-td",tags$b("Currency")),
              tags$td(class="fund-card-upper-td",
                      dt_dbobj_rep_fund_summary[dt_dbobj_rep_fund_summary$fund_id==i_fundid,"currency"])
            ),
            tags$tr(
              tags$td(class="fund-card-upper-td",tags$b("Start Date")),
              tags$td(class="fund-card-upper-td",
                      dt_dbobj_rep_fund_summary[dt_dbobj_rep_fund_summary$fund_id==i_fundid,"date_start"])
            ),
            tags$tr(
              tags$td(class="fund-card-upper-td",tags$b("Asset Under Management")),
              tags$td(class="fund-card-upper-td",
                      paste(format(dt_dbobj_rep_fund_summary[dt_dbobj_rep_fund_summary$fund_id==i_fundid,"nav_recent"],big.mark=" "),dt_dbobj_rep_fund_summary[dt_dbobj_rep_fund_summary$fund_id==i_fundid,"currency"]))
            ),
            tags$tr(
              tags$td(class="fund-card-upper-td",tags$b("Recommended Investment Term")),
              tags$td(class="fund-card-upper-td",
                      dt_dbobj_rep_fund_summary[dt_dbobj_rep_fund_summary$fund_id==i_fundid,"rec_inv_term"])
            )
          ),
          hr(),
          tags$table(
            tags$tr(
              tags$td(class="fund-card-metrics-td-col1",tags$b("Key Risk & Performance Indicators")),
              tags$td(class="fund-card-metrics-td",tags$b("1Y")),
              tags$td(class="fund-card-metrics-td",tags$b("3Y")),
              tags$td(class="fund-card-metrics-td",tags$b("5Y"))
            ),
            tags$tr(
              tags$td(class="fund-card-metrics-td-col1","Annualized Return"),
              tags$td(class="fund-card-metrics-td",sprintf("%.2f%%",100*dt_dbobj_rep_fund_summary[dt_dbobj_rep_fund_summary$fund_id==i_fundid,"return_1yr"])),
              tags$td(class="fund-card-metrics-td",sprintf("%.2f%%",100*dt_dbobj_rep_fund_summary[dt_dbobj_rep_fund_summary$fund_id==i_fundid,"return_ann_3yr"])),
              tags$td(class="fund-card-metrics-td",sprintf("%.2f%%",100*dt_dbobj_rep_fund_summary[dt_dbobj_rep_fund_summary$fund_id==i_fundid,"return_ann_5yr"]))
            ),
            tags$tr(
              tags$td(class="fund-card-metrics-td-col1","Annualized Volatility"),
              tags$td(class="fund-card-metrics-td",sprintf("%.2f%%",100*dt_dbobj_rep_fund_summary[dt_dbobj_rep_fund_summary$fund_id==i_fundid,"volatility_1yr"])),
              tags$td(class="fund-card-metrics-td",sprintf("%.2f%%",100*dt_dbobj_rep_fund_summary[dt_dbobj_rep_fund_summary$fund_id==i_fundid,"volatility_3yr"])),
              tags$td(class="fund-card-metrics-td",sprintf("%.2f%%",100*dt_dbobj_rep_fund_summary[dt_dbobj_rep_fund_summary$fund_id==i_fundid,"volatility_5yr"]))
            ),
            tags$tr(
              tags$td(class="fund-card-metrics-td-col1","Annualized Sharpe Ratio"),
              tags$td(class="fund-card-metrics-td",round(dt_dbobj_rep_fund_summary[dt_dbobj_rep_fund_summary$fund_id==i_fundid,"sharpe_ratio_1yr"],3)),
              tags$td(class="fund-card-metrics-td",round(dt_dbobj_rep_fund_summary[dt_dbobj_rep_fund_summary$fund_id==i_fundid,"sharpe_ratio_3yr"],3)),
              tags$td(class="fund-card-metrics-td",round(dt_dbobj_rep_fund_summary[dt_dbobj_rep_fund_summary$fund_id==i_fundid,"sharpe_ratio_5yr"],3))
            ),
            tags$tr(
              tags$td(class="fund-card-metrics-td-col1",""),
              tags$td(class="fund-card-metrics-td",""),
              tags$td(class="fund-card-metrics-td",""),
              tags$td(class="fund-card-metrics-td","")
            ),
            tags$tr(
              tags$td(class="fund-card-metrics-td-col1",""),
              tags$td(class="fund-card-metrics-td",tags$b("Peak")),
              tags$td(class="fund-card-metrics-td",tags$b("Trough")),
              tags$td(class="fund-card-metrics-td",tags$b("Decline"))
            ),
            tags$tr(
              tags$td(class="fund-card-metrics-td-col1","Most Severe Drawdown"),
              tags$td(class="fund-card-metrics-td",dt_dbobj_rep_fund_summary[dt_dbobj_rep_fund_summary$fund_id==i_fundid,"drawdown_from"]),
              tags$td(class="fund-card-metrics-td",dt_dbobj_rep_fund_summary[dt_dbobj_rep_fund_summary$fund_id==i_fundid,"drawdown_trough"]),
              tags$td(class="fund-card-metrics-td",sprintf("%.2f%%",100*dt_dbobj_rep_fund_summary[dt_dbobj_rep_fund_summary$fund_id==i_fundid,"drawdown_depth"]))
            )
          ),
          plotlyOutput(paste0("PLT_price_chg_pct_fund_",i_fundid))
      )
    })
  })

# SERV creating output$ objects - Equity Funds -----------------------------

  progress$set(value = 10) #######################################################################################
  output$UI_block_funds_overview_fund_details_Eq <- renderUI({
      fluidRow(
        lapply((filter(dt_dbobj_rep_fund_summary, fund_category =="Equity") %>% arrange(desc(nav_recent)))$fund_id, function(i_fundid) {
          uiOutput(paste0('UI_box_summary_fund_', i_fundid))
        })
      ) 
  })
  output$UI_block_funds_overview_fund_details_Eq_links <- renderUI({
    f_fund_quicklinks_box(c("Equity"),"in",dt_dbobj_rep_fund_summary)
  })

# SERV creating output$ objects - Absolute Return Funds --------------------
  
  progress$set(value = 11) #######################################################################################
  output$UI_block_funds_overview_fund_details_AbsRet <- renderUI({
    fluidRow(
      lapply((filter(dt_dbobj_rep_fund_summary,fund_category =="Absolute Return") %>% arrange(desc(nav_recent)))$fund_id, function(i_fundid) {
        uiOutput(paste0('UI_box_summary_fund_', i_fundid))
      })
    )
  })
  output$UI_block_funds_overview_fund_details_AbsRet_links <- renderUI({
    f_fund_quicklinks_box(c("Absolute Return"),"in",dt_dbobj_rep_fund_summary)
  })

# SERV creating output$ objects - Bond Funds -------------------------------
  
  progress$set(value = 12) #######################################################################################
  output$UI_block_funds_overview_fund_details_Bond <- renderUI({
    fluidRow(
      lapply((filter(dt_dbobj_rep_fund_summary,fund_category %in% c("Long Bond","Short Bond","Unlimited Duration Bond")) %>% arrange(desc(nav_recent)))$fund_id, function(i_fundid) {
        uiOutput(paste0('UI_box_summary_fund_', i_fundid))
      })
    )
  })
  output$UI_block_funds_overview_fund_details_Bond_links <- renderUI({
    f_fund_quicklinks_box(c("Long Bond","Short Bond","Unlimited Duration Bond"),"in",dt_dbobj_rep_fund_summary)
  })
  
# SERV creating output$ objects - Other Funds -------------------------------
  
  progress$set(value = 13) #######################################################################################
  output$UI_block_funds_overview_fund_details_Oth <- renderUI({
    fluidRow(
      lapply((filter(dt_dbobj_rep_fund_summary,!fund_category %in% c("Equity","Absolute Return","Long Bond","Short Bond","Unlimited Duration Bond")) %>% arrange(desc(nav_recent)))$fund_id, function(i_fundid) {
        uiOutput(paste0('UI_box_summary_fund_', i_fundid))
      })
    )
  })
  output$UI_block_funds_overview_fund_details_Oth_links <- renderUI({
    f_fund_quicklinks_box(c("Equity","Absolute Return","Long Bond","Short Bond","Unlimited Duration Bond"),"not in",dt_dbobj_rep_fund_summary)
  })

# SERV creating output$ objects - Selection - Recession Proof --------------

  progress$set(value = 14) #######################################################################################

  output$DT_sftq_perf <- DT::renderDataTable(DT::datatable(disp_DT_sftq_perf, extensions = "Responsive", options = list(language = list(info = "_START_ to _END_ of _TOTAL_", paginate = list(previous = "<<", `next` = ">>")), ordering=T, pageLength = 5, bLengthChange=F, searching=F, paging=T, scrollX = T, columnDefs = list(list(className = 'dt-center', targets = 1:4))), rownames=F) %>% formatPercentage(c("Return","Volatility","Max. Drawdown"),2))
  output$plot_sftq_price_chg <- plotly::renderPlotly(plot_obj_price_sftq)

  
} # SERV function end

# UI dashboardPage() start ------------------------------------------------

ui <- dashboardPage(

# UI dashboardHeader() start ----------------------------------------------

  dashboardHeader(title="Fund4Me", titleWidth = "250px"
                  ),

# UI dashboardSidebar() start ---------------------------------------------

  dashboardSidebar(width = "250px",
                   shinyjs::useShinyjs(),                                 
                   sidebarMenu(
                     menuItem("Home", tabName = "home", icon = icon("home")),
                     menuItem("Funds",tabName = NULL, icon = icon("line-chart"),
                              menuItem("Return & Risk Summary", tabName = "funds_returnrisksum", icon = icon("bars")),#, badgeLabel = "new", badgeColor = "green")
                              br(),
                              
                              menuItem("Equity Funds", tabName = "eq_funds_overview", icon = icon("angle-double-right")),# badgeLabel = "soon", badgeColor = "yellow"),
                              menuItem("Absolute Return Funds", tabName = "absret_funds_overview", icon = icon("angle-double-right")),# badgeLabel = "soon", badgeColor = "yellow"),
                              menuItem("Bond Funds", tabName = "bond_funds_overview", icon = icon("angle-double-right")),# badgeLabel = "soon", badgeColor = "yellow"),
                              menuItem("Other Funds", tabName = "oth_funds_overview", icon = icon("angle-double-right")),# badgeLabel = "soon", badgeColor = "yellow"),
                              br(),
                              
                              menuItem("Selections", icon = icon("folder-open"),
                                       menuItem("Recession-Proof Funds", tabName = "sel_recessionproof", icon = icon("angle-double-right"))
                                       ),
                              
                              br(),
                              menuItem("Fund Managers", icon = icon("users"),
                                       menuItem("AEGON Website", href = "https://www.aegonalapkezelo.hu/en/", icon = icon("globe")),
                                       menuItem("CONCORDE Website", href = "http://concordeam.com", icon = icon("globe"))
                                       )
                              )
                   ) # sidebarMenu() end
  ),

# UI dashboardBody() start ------------------------------------------------

  dashboardBody(
    # includeCSS("www/stylesheet_ext.css"),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "stylesheet_ext.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "www/stylesheet_ext.css")
    ),

# UI tabItems() start -----------------------------------------------------

    tabItems(

# UI tabItem home ---------------------------------------------------------

      tabItem(tabName = "home",
              h2("Fund4Me", tags$small("Invest smarter!")),
              fluidRow(
                column(width = 3,
                       br(),
                       box(status = "primary",
                           width = NULL,
                           br(),
                           img(class="profile-user-img img-responsive img-circle", src="logo1.png"),
                           br(),
                           h1(class="profile-username text-center","Fund4Me"),
                           p(class="text-muted text-center","An experimental data project", shiny::br(),"created by Attila Toth")
                        ),
                       div(class="callout callout-warning",
                           h4(icon("exclamation-triangle"), " Warning"),
                           "Every investment involves risk. It is highly recommended to consult a professional advisor about your personal circumstances before making investment decision."
                       )
                       ), # column() end
                column(width = 9,
                       br(),
                       tabBox(width = NULL,
                              tabPanel("Intro",
                                       h4("Hi."),
                                       br(),
                                       tags$b("Fund4Me"),"is a small-scale",tags$b("data project,"),"an attempt to build a full-stack web application that monitors performance of selected mutual funds in an automated fashion.",
                                       "It stands in an experimental phase and acts as a first public appearance of my personal endeavour with R and its surrounding ecosystem.",
                                       br(),br(),
                                       "The app is built with the purpose of",
                                       tags$ul(
                                              tags$li("exploring difficulties around setup and ownership of a",tags$b("cloud"),"based machine,"),
                                              tags$li("creating and managing my ",tags$b("own analytical stack,")),
                                              tags$li(tags$b("demonstrating skills"),"and techniques obtained during my journey with R ecosystem.")
                                              )
                                       ), # tabPanel() end
                              tabPanel("The App",
                                       h4("Purpose"),
                                       "The application in its current stage acts as an information gathering and presentation tool that collects publicly available fund price information, stores, processes, structures them, and eventually presents summarized results in a well interpretable way so that users may gain valuable insights about mutual funds' past performances.",
                                       br(),br(),
                                       h4("Funds"),
                                       "Fund price information is currently sourced from the",a("website",href="http://www.bamosz.hu/en/",target="_blank"),"of Association of Hungarian Investment Fund and Asset Management Companies (BAMOSZ).",
                                       "Current scope is limited to funds of two asset managers, AEGON Hungary Investment Fund Management and CONCORDE Investment Fund Management, available for purchase online by retail investors. These two asset managers have been selected for this data project due to their successful track record and professional recognition within Hungary. Additionally, AEGON and CONCORDE have been awarded 2-2 times in the last 4 years as 'Best Asset Manager or the Year'.",
                                       "Scope of the project is subject to extension any time in the future to involve further significant, trustworthy players of the Hungarian market.",
                                       "Fund4Me only processes HUF denominated funds, other currencies are out of scope for now.",
                                       br(),br(),
                                       tags$b("The author of this application does not maintain any form of business relationship with AEGON, CONCORDE or any further asset manager that may appear on this website and does not carry out any form of business activity for or on behalf of them."),
                                       br(),br(),
                                       h4("Operation in a nutshell"),
                                       tags$ul(
                                         tags$li("A scheduled job in the stack scans BAMOSZ's web portal on a daily basis searching for newly added fund prices."),
                                         tags$li("Some of the calculated metrics require general market information (e.g. yield curve, interest rates), too. Another set of scheduled jobs collects these pieces of information from",a("Government Debt Management Agency of Hungary",href="http://www.akk.hu",target="_blank"),"(AKK)."),
                                         tags$li("New fund prices and other collected market information are loaded into Fund4Me's database, wherein they get further processed and stored."),
                                         tags$li("Based on the most up-to-date information, the application generates visualizations, calculates key performance & risk metrics.")
                                         )
                                       ), # tabPanel() end
                              tabPanel("Technology",
                                        "Efforts invested into this application intended to create a so-called data project by exploring and exploiting opportunities provided by",tags$b("R ecosystem."),
                                        "However, in order to bring this product into life multiple components of various technologies must have been coordinated and harmonized.",
                                        "Given the characteristics and the purposes of the project, the implemented solution depends solely on",tags$b("open source"),"(free) tools and components.",
                                        "In order to promote the open source movement further and give back something to the community, the entire source code is available on",a("this",href="https://github.com/attilatoth86/Fund4Me",target="_blank"),"public repository.",
                                        br(),br(),
                                       fluidRow(
                                         column(width = 2, align="center",
                                                a(img(src="rstudioball_logo.png", width="75px"), href="https://www.rstudio.com/", target="_blank")),
                                         column(width = 2, align="center",
                                                a(img(src="shinyoct_logo.png", width="75px"), href="https://shiny.rstudio.com/", target="_blank")),
                                         column(width = 2, align="center",
                                                a(img(src="DO_logo.png", width="75px"), href="https://www.digitalocean.com/", target="_blank")),
                                         column(width = 2, align="center",
                                                a(img(src="ubuntu_logo.png", width="75px"), href="https://www.ubuntu.com/", target="_blank")),
                                         column(width = 2, align="center",
                                                a(img(src="postgresql_logo.png", width="75px"), href="https://www.postgresql.org/", target="_blank")),
                                         column(width = 2, align="center",
                                                a(img(src="github_logo.png", width="75px"), href="https://github.com/", target="_blank"))
                                       ),
                                       br(),
                                        h4("Highlights"),
                                        tags$ul(
                                          tags$li("Fund4Me heavily relies on",a("RStudio",href="https://www.rstudio.com/",target="_blank"),"(both desktop and server editions) as the main development environment."),
                                          tags$li("User interface has been created with",a("Shiny",href="https://shiny.rstudio.com/",target="_blank"),", a web application framework for R."),
                                          tags$li(a("Digital Ocean",href="https://www.digitalocean.com/",target="_blank"),", a cloud infrastucture provider, supplies virtual machines (in this particular case",a("Ubuntu",href="https://www.ubuntu.com/",target="_blank"),"14.04) to run and maintain the entire analytics stack."),
                                          tags$li(a("PostgreSQL",href="https://www.postgresql.org/",target="_blank")," serves as the backbone of the data storage capabilities."),
                                          tags$li(a("GitHub",href="https://github.com/",target="_blank")," does the version control and source code management for the project.")
                                        )
                                      ), # tabPanel() end 
                              tabPanel(title = "Terms of Use",
                                       "By accessing this web site, the pages contained on it, the products, services, information, tools and material contained or described herein (the \"Site\"), you acknowledge your agreement with and understanding of the following terms of use.",
                                       br(),br(),
                                       h4("Information Supplied"),
                                       "The content on this website was produced by Attila Toth (hereafter \"Author\") with the greatest of care and to the best of his knowledge and belief. However, Author provides no guarantee with regard to its content and completeness and does not accept any liability for losses which might arise from making use of the information contained herein. All details are provided for information purposes only. The information provided herein is not legally binding and it does not constitute an offer or invitation to enter into any type of financial transaction. The recipient is in particular recommended to check that the information provided is in line with his/her own circumstances with regard to any legal, regulatory, tax or other consequences, if necessary with the help of a professional advisor.",
                                       br(),br(),
                                       h4("Sales Restrictions and Fund Documents"),
                                       "The investment funds mentioned on this Site may only be purchased on the basis of the current sales prospectus and the most recent annual report (or monthly, quarterly report, if these are more recent). The key investor information documentation and the most recent reports are all available from",a("AEGON's",href="https://www.aegonalapkezelo.hu/",target="_blank"),"and",a("CONCORDE's",href="http://concordealapkezelo.hu/",target="_blank"),"website.",
                                       br(),br(),
                                       h4("Risk Considerations"),
                                       "Every investment involves risk, especially with regard to fluctuations in value and return.",
                                       br(),br(),
                                       "Investments in foreign currencies involve the additional risk that the foreign currency might lose value against the investor's reference currency.",
                                       br(),br(),
                                       "Some investment products include investments in Emerging Markets. Emerging Markets are located in countries that possess one or more of the following characteristics: A certain degree of political instability, relatively unpredictable financial markets and economic growth patterns, a financial market that is still at the development stage or a weak economy. Emerging markets investments usually result in higher risks such as political risks, economical risks, credit risks, exchange rate risks, market liquidity risks, legal risks, settlement risks, market risks, shareholder risk and creditor risk.",
                                       br(),br(),
                                       "Equities are subject to market forces and hence fluctuations in value which are not entirely predictable.",
                                       br(),br(),
                                       "Investment principal on bonds can be eroded depending on sale price or market price. In addition, there are bonds on which investment principal can be eroded due to changes in redemption amounts. Care is required when investing in such instruments.",
                                       br(),br(),
                                       "The key risks of real estate funds include limited liquidity in the real estate market, changing mortgage interest rates, subjective valuation of real estate, inherent risks with respect to the construction of buildings and environmental risks (e.g., land contamination).",
                                       br(),br(),
                                       "Historical performance indications and financial market scenarios are no guarantee for current or future performance. Performance indications do not consider commissions levied at subscription and/or redemption. Furthermore, no guarantee can be given that the performance of the benchmark will be reached or outperformed.",
                                       br(),br(),
                                       h4("Licence"),
                                       a(href="http://creativecommons.org/licenses/by-sa/4.0/",target="_blank",img(style="border-width:0",src="https://i.creativecommons.org/l/by-sa/4.0/80x15.png")),
                                       " This work is licensed under a",a(href="http://creativecommons.org/licenses/by-sa/4.0/","Creative Commons Attribution-ShareAlike 4.0 International License",target="_blank"),".",
                                       br()
                                      )
                        ) # tabBox() end
                       ) # column() end
              ) # fluidRow() end
      ),

# UI tabItem Equity funds overview -----------------------------------------

tabItem(tabName = "eq_funds_overview",
        h2("Equity Funds", tags$small("Overview")),
        fluidRow(
          column(width = 12,
                 box(width = NULL,
                     tags$table(
                       tags$col(width="75"),
                       tags$tr(
                         tags$td(h4(icon("info-circle"), "Info")),
                         tags$td("Equity (or stock) funds invest in stocks, also called equity securities. Fund assets are typically in stock, with some amount of cash, which is generally quite small. The objective of an equity fund is long-term growth through capital gains and dividends.")
                       )
                     )
                 )
          )
        ),
        uiOutput("UI_block_funds_overview_fund_details_Eq_links"),
        uiOutput("UI_block_funds_overview_fund_details_Eq")
),

# UI tabItem Absolute Return Funds overview --------------------------------

tabItem(tabName = "absret_funds_overview",
        h2("Absolute Return Funds", tags$small("Overview")),
        fluidRow(
          column(width = 12,
                 box(width = NULL,
                     tags$table(
                       tags$col(width="75"),
                       tags$tr(
                         tags$td(h4(icon("info-circle"), "Info")),
                         tags$td("Absolute return funds aim to deliver returns in any, both rising and falling, market conditions. Therefore they invest in a wide range of asset classes and employ various investment strategies. These strategies may often include usage of derivatives. Managers may also take short positions or invest in exotic securities.")
                       )
                     )
                 )
          )
        ),
        uiOutput("UI_block_funds_overview_fund_details_AbsRet_links"),
        uiOutput("UI_block_funds_overview_fund_details_AbsRet")
),

# UI tabItem Bond Funds overview -------------------------------------------

tabItem(tabName = "bond_funds_overview",
        h2("Bond Funds", tags$small("Overview")),
        fluidRow(
          column(width = 12,
                 box(width = NULL,
                     tags$table(
                       tags$col(width="75"),
                       tags$tr(
                         tags$td(h4(icon("info-circle"), "Info")),
                         tags$td("Bond (or debt) funds invest in bonds or other debt securities. They usually invest in various type of bonds, issued by government, government agencies, municipalities or corporations.")
                       )
                     )
                 )
          )
        ),
        uiOutput("UI_block_funds_overview_fund_details_Bond_links"),
        uiOutput("UI_block_funds_overview_fund_details_Bond")
        ),

# UI tabItem Other Funds overview ------------------------------------------

tabItem(tabName = "oth_funds_overview",
        h2("Other Funds", tags$small("Overview")),
        fluidRow(
          column(width = 12,
                 box(width = NULL,
                     tags$table(
                       tags$col(width="75"),
                       tags$tr(
                         tags$td(h4(icon("info-circle"), "Info")),
                         tags$td("Listed funds below represent various minor categories (e.g. Balanced, Deliberate, Dynamic, Guaranteed Funds, Other Money Market, etc.) of funds.")
                       )
                     )
                 )
          )
        ),
        uiOutput("UI_block_funds_overview_fund_details_Oth_links"),
        uiOutput("UI_block_funds_overview_fund_details_Oth")
        ),

# UI tabItem Selections - Recession Proof ----------------------------------

tabItem(tabName = "sel_recessionproof",
        h2("Fund Selections", tags$small("Recession-Proof Funds")),
        fluidRow(
          column(width = 12,
                 box(width = NULL,
                     tags$table(
                       tags$col(width="75"),
                       tags$tr(
                         tags$td(h4(icon("info-circle"), "Info")),
                         tags$td("The most recent period of a significant, worldwide, general economic decline took place during the late 2000s and early 2010s. It is considered to be triggered by the financial crisis in 2007-2008 which resulted in massive plunge and volatility on the markets globally.",
                                 "When making long-term investment decisions, it is recommended to consider certain performance indicators during such crisis.",
                                 "Below is a collection of funds that had already operated during the Great Recession.")
                         )
                       )
                     )
                 )
          ),
        fluidRow(
          column(width = 7,
                 box(status = "primary",
                     solidHeader = F,
                     width = NULL,
                     title = tags$b(div(icon("line-chart"), " Performance in Financial Crisis '07-'09")),
                     p("The financial crisis of 2007-2009 resulted in a major decline throughout the world's financial markets. This translated in the United States to a 17-month bear market between 9 October 2007 and 9 March 2009. Table below shows fund performances within this period."),
                     DT::dataTableOutput("DT_sftq_perf")
                     )
                 ),
          column(width = 5,
                 box(width = NULL,
                     solidHeader = F,
                     tags$table(
                       tags$col(width="125"),
                       tags$tr(class="sftq-desc-row",
                         tags$td(class="sftq-desc-td",tags$b("Reporting Period")),
                         tags$td(class="sftq-desc-td","9 October 2007 to 9 March 2009, time window the performance and risk measures are based on.")
                       ),
                       tags$tr(class="sftq-desc-row",
                         tags$td(class="sftq-desc-td",tags$b("Return")),
                         tags$td(class="sftq-desc-td","Simple cumulative return between the beginning and end of reporting period.")
                       ),
                       tags$tr(class="sftq-desc-row",
                         tags$td(class="sftq-desc-td",tags$b("Volatility")),
                         tags$td(class="sftq-desc-td","Annualized volatility, calculated on price changes within the reporting period.")
                       ),
                       tags$tr(#class="sftq-desc-row",
                         tags$td(class="sftq-desc-td",tags$b("Max. Drawdown")),
                         tags$td(class="sftq-desc-td","The largest plunge in fund prices during the Financial Crisis.")
                       )
                     )
                 )
                )
          ),
        fluidRow(
          column(width = 12,
                 box(status = "primary",
                     width = NULL,
                     plotlyOutput("plot_sftq_price_chg"),
                     "Note: by default, the best performers appear on the chart but you may control the displayed time series by clicking on fund names in the legend."
                     )
                 )
          )
        ),

# UI tabItem return & risk summary ----------------------------------------

      tabItem(tabName = "funds_returnrisksum",
              h2("Funds", tags$small("Return & Risk Summary")),
              fluidRow(
                column(width = 12,
                       box(width = NULL,
                           tags$table(
                             tags$col(width="100"),
                             tags$tr(
                               tags$td(h4(icon("info-circle"), "Info")),
                               tags$td("Below you may find summarized information about historical performances of mutual funds being processed.",
                                       "Each box represents a crucial performance metric (by fund along the most relevant time buckets) worth considering for investment decisions.",
                                       "A default sorting is applied on each table which you may interactively alter by clicking on column headers to reorder the particular table.")
                             )
                           ),
                           footer = tags$table(
                                               tags$col(width="100"),
                                               tags$tr(
                                                 tags$td(h4(class="text-red", icon("exclamation-triangle"), "Warning")),
                                                 tags$td(p(class="text-red","Historical performance indications are no guarantee for current or future performance. Performance indications do not consider commissions levied at subscription and/or redemption.",
                                                         "It is highly recommended to consult a professional advisor about your personal circumstances before making investment decision."))
                                               )
                                             )
                             
                       )
                )
              ),
              fluidRow(
                column(width = 7,
                       box(status = "primary",
                           solidHeader = F,
                           width = NULL,
                           title = tags$b(div(icon("line-chart"), " Cumulative Return")),
                           p("Cumulative return is the aggregate amount an investment in a fund has gained or lost over time, independent of the period of time involved."),
                           DT::dataTableOutput("DT_cum_return")
                        )
                       ),
                column(width = 5,
                       box(status = "primary",
                           solidHeader = F,
                           width = NULL,
                           title = tags$b(div(icon("line-chart"), " Annualized Return")),
                           p("Annualized return is the geometric average amount of money earned by a fund each year over a given time period. Calculated as a geometric average to show what an investor would earn over a period of time if the annual return was compounded."),
                           DT::dataTableOutput("DT_ann_return")
                        )
                       )
              ),
              fluidRow(
                column(width = 6,
                       box(status = "danger",
                           solidHeader = F,
                           width = NULL,
                           title = tags$b(div(icon("random"), " Annualized Volatility")),
                           p("Volatility indicates the uncertainty (risk) about the changes in a funds' value. A higher volatility means that funds' value can potentially be spread out over a larger range of values meaning an increased likelihood of dramatical price changes over a short time period in either direction. A lower volatility means a smaller chance of significant value fluctuation but changes in price at a steadier pace over a period of time."),
                           DT::dataTableOutput("DT_volatility"))
                ),
                column(width = 6,
                       box(status = "danger",
                           solidHeader = F,
                           width = NULL,
                           title = tags$b(div(icon("random"), " Most Severe Drawdown")),
                           p("Most severe drawdown is the largest peak-to-trough decline during the lifetime of a fund, quoted as the percentage between the peak and the subsequent trough."),
                           DT::dataTableOutput("DT_drawdown"))
                )
              ),
              fluidRow(
                column(width = 6,
                       box(status = "danger",
                           solidHeader = F,
                           width = NULL,
                           title = tags$b(div(icon("balance-scale"), " Annualized Sharpe Ratio")),
                           p("Sharpe Ratio is a measure for risk-adjusted return, it represents the average return earned in excess of the risk-free rate per unit of volatility. The greater Sharpe ratio indicates better risk-adjusted performance, and negative Sharpe ratio means that a risk-free asset would perform better than the security being analyzed."),
                           DT::dataTableOutput("DT_sharpe_ratio")
                        )
                       )
              )
      )

      ) # tabItems() end

    ) # dashboardBody() end

    ) # dashboardPage() end

# call shinyApp() ---------------------------------------------------------

shinyApp(ui = ui, server = server)
