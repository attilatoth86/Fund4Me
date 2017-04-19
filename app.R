
# Init load libraries -----------------------------------------------------

library(shiny) # http://shiny.rstudio.com
library(shinydashboard) # https://rstudio.github.io/shinydashboard/
library(plotly) # https://plot.ly
library(DT) # https://rstudio.github.io/DT/
library(PerformanceAnalytics) # https://www.rdocumentation.org/packages/PerformanceAnalytics/

# Init source external resources ------------------------------------------

source("f.R")

# SERV init ---------------------------------------------------------------

server <- function(input, output, session) {
  
  progress <- Progress$new(session, min=1, max=4)
  on.exit(progress$close())
  
  progress$set(message = 'Fund4Me is loading data in the background.',
               detail = 'You may use the application meanwhile..')
  
  chrt_dt <- psqlQuery("SELECT fund_id, description AS fund_name, value_date, change_pct FROM app.fund_return_calc_mvw
                       WHERE description like 'AEGON%'
                       ORDER BY fund_id, value_date
                       ")$result
  progress$set(value = 1)
  perf_dt <- psqlQuery("SELECT fund_id, 
                               'Cumulative performance (%)' \"Measure\", 
                               return_ytd*100 \"YTD\", 
                               return_ann_1y*100 \"1Y\", 
                               return_2y*100 \"2Y\", 
                               return_3y*100 \"3Y\", 
                               return_5y*100 \"5Y\" 
                        FROM app.fund_hist_tbl_vw
                       UNION ALL
                       SELECT fund_id, 
                              'Annualized performance (%)', 
                              NULL, 
                              return_ann_1y*100, 
                              return_ann_2y*100, 
                              return_ann_3y*100, 
                              return_ann_5y*100 
                       FROM app.fund_hist_tbl_vw t
                       ORDER BY fund_id, \"Measure\" DESC
                       ")$result
  progress$set(value = 2)
  ctrl_dt <- psqlQuery("SELECT * FROM app.fund_hist_tbl_vw")$result
  progress$set(value = 3)
  return_dt <- psqlQuery("SELECT * FROM app.fund_return_calc_mvw WHERE return IS NOT NULL ORDER BY fund_id, value_date")$result
  progress$set(value = 4)
  #data.frame(return_dt[return_dt$fund_id==16,"return",drop=F],row.names=return_dt[return_dt$fund_id==16,"value_date"])
  #i <- 18
  #r <- 4

  # Processing funds 1by1
  lapply(unique(chrt_dt$fund_id), function(i){
    
    # Relevant dates
    i_prc_dts <- ctrl_dt[ctrl_dt$fund_id==i,c("fund_id","dt_1y","dt_2y","dt_3y","dt_5y")]
    
    dt_proc <- chrt_dt[chrt_dt$fund_id==i,]
    output[[paste0("dyn_plot_out",i)]] <- renderPlotly(
        plot_ly() %>% add_trace(data=dt_proc, x=~value_date, y=~change_pct, mode="lines") %>%
          layout(xaxis=list(title=""), yaxis=list(title="Price Change %"))
    )
    
    i_add_metrics <- data.frame(NA,NA,NA,NA,NA)
    colnames(i_add_metrics) <- c("Measure","1Y","2Y","3Y","5Y")
    for(r in c(2:5)) {
        if(is.na(i_prc_dts[1,r])==F){
          return_dt_proc <- data.frame(return_dt[return_dt$fund_id==i,c("return","return_rf_wd"),drop=F],row.names=return_dt[return_dt$fund_id==i,"value_date"])
          return_dt_proc <- return_dt_proc[rownames(return_dt_proc)>=i_prc_dts[1,r],,drop=F]  
          i_add_metrics[1,r] <- round(StdDev.annualized(return_dt_proc[,"return",drop=F], scale = 252),digits=2)
          i_add_metrics[2,r] <- round(SharpeRatio.annualized(R = return_dt_proc[,"return",drop=F], Rf = return_dt_proc[,"return_rf_wd",drop=F], scale=252, geometric=T),digits=2)
          i_add_metrics[3,r] <- round(VaR(return_dt_proc[,"return",drop=F], p=.95, method="historical"),digits=2)
          i_add_metrics[4,r] <- round(maxDrawdown(return_dt_proc[,"return",drop=F], geometric=T, invert=F),digits=2)
        }
        i_add_metrics[1,1] <- "Annualized Volatility"
        i_add_metrics[2,1] <- "Annualized Sharpe Ratio"
        i_add_metrics[3,1] <- "VaR (p=95%)"
        i_add_metrics[4,1] <- "Maximum Drawdown"
    }
    
    perf_dt_proc <- perf_dt[perf_dt$fund_id==i,-c(1,3)]
    output[[paste0("dyn_table_out",i)]] <- DT::renderDataTable(rbind(perf_dt_proc,i_add_metrics),
                                                               options = list(dom = 't', ordering=F, searching=F, paging=F, scrollX = F),
                                                               rownames=F)
    
    output[[paste0("dyn_ui_components_out",i)]] <- renderUI({
      fluidRow(
        column(width = 6,
               box(title = unique(chrt_dt["fund_name"][chrt_dt$fund_id==i,]),
                   width = NULL,
                   solidHeader = T,
                   plotlyOutput(paste0("dyn_plot_out",i))
                )
               ),
        column(width = 6,
               box(title="Performance, Return & Risk",
                   status = "primary",
                   width = NULL,
                   dataTableOutput(paste0("dyn_table_out",i))),
                   "Historical performance indications are no guarantee for current or future performance. Performance indications do not consider commissions levied at subscription and/or redemption."
               )
      )
    })
  }) # end of Processing funds 1by1


  output$dyn_ui_block <- renderUI({
      lapply(unique(chrt_dt$fund_id), function(i) {
        box(
          solidHeader = T,
          width=12,
          uiOutput(paste0('dyn_ui_components_out', i))
        )
      })
  })

} # SERV function end

# UI dashboardPage() start ------------------------------------------------

ui <- dashboardPage(

# UI dashboardHeader() start ----------------------------------------------

  dashboardHeader(title="Fund4Me", titleWidth = "270px"),

# UI dashboardSidebar() start ---------------------------------------------

  dashboardSidebar(width = "270px",
                   shinyjs::useShinyjs(),                                 
                   sidebarMenu(
                     menuItem("Home", tabName = "home", icon = icon("home")),
                     menuItem("Funds",tabName = NULL, icon = icon("line-chart"),
                              menuItem("Overview", tabName = "funds_ov", icon = icon("angle-right"), badgeLabel = "disabled", badgeColor = "red"),
                              menuItem("Details", tabName = "fundprices", icon = icon("angle-right"))
                     )
                   ) # sidebarMenu() end
  ),

# UI dashboardBody() start ------------------------------------------------

  dashboardBody(

# UI tabItems() start -----------------------------------------------------

    tabItems(

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
                                              ),
                                       br(),
                                       div(class="alert alert-warning", #"alert alert-danger",
                                           role="alert",
                                           h4(icon("exclamation-triangle"), " Warning"),
                                           tags$b("Every investment involves risk. It is highly recommended to consult a professional advisor about your personal circumstances before making investment decision."),
                                           tags$b("Historical performance indications and financial market scenarios are no guarantee for current or future performance.")
                                        )
                                       ), # tabPanel() end
                              tabPanel("The App",
                                       h4("Purpose"),
                                       "The application in its current stage acts as an information gathering and presentation tool that collects publicly available fund price information, stores, processes, structures them, and eventually presents summarized results in a well interpretable way so that users may gain valuable insights about mutual funds' past performances.",
                                       br(),br(),
                                       h4("Funds"),
                                       "Fund price information is currently sourced (therefore available within the app) only from the Hungarian branch of",a("AEGON Asset Management.",href="https://www.aegonalapkezelo.hu/",target="_blank"),
                                       "AEGON has been selected for this data project due to its investor-friendly online presence, e.g. publishing daily fund price information online in a way that allows for automated data capturing.",
                                       "Fund4Me only processes AEGON's HUF denominated fund portfolio, other currencies are out of scope for now.",
                                       br(),br(),
                                       tags$b("The author of this application does not maintain any form of business relationship with AEGON Asset Management and does not carry out any form of business activity for or on behalf of them."),
                                       br(),br(),
                                       h4("Operation in a nutshell"),
                                       tags$ul(
                                         tags$li("A scheduled job in the stack scans AEGON's web portal on a daily basis searching for newly added fund prices."),
                                         tags$li("Some of the calculated metrics require general market information (e.g. yield curve, interest rates), too. Another set of scheduled jobs collects these pieces of information from",a("Government Debt Management Agency of Hungary",href="http://www.akk.hu",target="_blank"),"(AKK)."),
                                         tags$li("New fund prices and other collected market information are loaded into Fund4Me's database, wherein they get further processed and stored."),
                                         tags$li("Based on the most up-to-date information, the application generates visualizations, calculates key performance & risk metrics.")
                                         )
                                       ), # tabPanel() end
                              tabPanel("Technology",
                                       br(),
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
                                              "Efforts invested into this application intended to create a so-called data project by exploring and exploiting opportunities provided by",tags$b("R ecosystem."),
                                              "However, in order to bring this product into life multiple components of various technologies must have been coordinated and harmonized.",
                                              "Given the characteristics and the purposes of the project, the implemented solution depends solely on",tags$b("open source"),"(free) tools and components.",
                                              "In order to promote the open source movement further and give back something to the community, the entire source code is available on",a("this",href="https://github.com/attilatoth86/Fund4Me",target="_blank"),"public repository.",
                                              br(),br(),
                                              h4("Highlights"),
                                              tags$ul(
                                                tags$li("Fund4Me heavily relies on",a("RStudio",href="https://www.rstudio.com/",target="_blank"),"(both desktop and server editions) as the main development environment."),
                                                tags$li("User interface has been created with",a("Shiny",href="https://shiny.rstudio.com/",target="_blank"),", a web application framework for R."),
                                                tags$li(a("Digital Ocean",href="https://www.digitalocean.com/",target="_blank"),", a cloud infrastucture provider, supplies virtual machines (in this particular case",a("Ubuntu",href="https://www.ubuntu.com/",target="_blank"),"14.04) to run and maintain the entire analytics stack."),
                                                tags$li(a("PostgreSQL",href="https://www.postgresql.org/",target="_blank")," serves as the backbone of the data storage capabilities."),
                                                tags$li(a("GitHub",href="https://github.com/",target="_blank")," does the version control and source code management for the project.")
                                              )
                                           ), # tabPanel() end 
                              tabPanel("Terms of Use",
                                       "By accessing this web site, the pages contained on it, the products, services, information, tools and material contained or described herein (the \"Site\"), you acknowledge your agreement with and understanding of the following terms of use.",
                                       br(),br(),
                                       h4("Information supplied"),
                                       "The content on this website was produced by Attila Toth (hereafter \"Author\") with the greatest of care and to the best of his knowledge and belief. However, Author provides no guarantee with regard to its content and completeness and does not accept any liability for losses which might arise from making use of the information contained herein. All details are provided for information purposes only. The information provided herein is not legally binding and it does not constitute an offer or invitation to enter into any type of financial transaction. The recipient is in particular recommended to check that the information provided is in line with his/her own circumstances with regard to any legal, regulatory, tax or other consequences, if necessary with the help of a professional advisor.",
                                       br(),br(),
                                       h4("Sales restrictions and Fund Documents"),
                                       "The investment funds mentioned on this Site may only be purchased on the basis of the current sales prospectus and the most recent annual report (or monthly, quarterly report, if these are more recent). The key investor information documentation and the most recent reports are all available from",a("this",href="https://www.aegonalapkezelo.hu/",target="_blank"),"website.",
                                       br(),br(),
                                       h4("Risk Considerations"),
                                       "Every investment involves risk, especially with regard to fluctuations in value and return. Investments in foreign currencies involve the additional risk that the foreign currency might lose value against the investor's reference currency. Historical performance indications and financial market scenarios are no guarantee for current or future performance. Performance indications do not consider commissions levied at subscription and/or redemption. Furthermore, no guarantee can be given that the performance of the benchmark will be reached or outperformed. Some investment products include investments in Emerging Markets. Emerging Markets are located in countries that possess one or more of the following characteristics: A certain degree of political instability, relatively unpredictable financial markets and economic growth patterns, a financial market that is still at the development stage or a weak economy. Emerging markets investments usually result in higher risks such as political risks, economical risks, credit risks, exchange rate risks, market liquidity risks, legal risks, settlement risks, market risks, shareholder risk and creditor risk. Equities are subject to market forces and hence fluctuations in value which are not entirely predictable. Investment principal on bonds can be eroded depending on sale price or market price. In addition, there are bonds on which investment principal can be eroded due to changes in redemption amounts. Care is required when investing in such instruments. The key risks of real estate funds include limited liquidity in the real estate market, changing mortgage interest rates, subjective valuation of real estate, inherent risks with respect to the construction of buildings and environmental risks (e.g., land contamination). ",
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

      tabItem(tabName = "fundprices",
              h2("Funds", tags$small("Details")),
              fluidRow(uiOutput("dyn_ui_block"))
      )

      ) # tabItems() end

    ) # dashboardBody() end

    ) # dashboardPage() end

# call shinyApp() ---------------------------------------------------------

shinyApp(ui = ui, server = server)
