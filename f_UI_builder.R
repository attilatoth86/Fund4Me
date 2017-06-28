
f_fund_quicklinks_box <- function(filter_cond, in_not_in, df){
  switch (in_not_in,
    "in" = fluidRow(
          column(width = 12,
                 box(width = NULL,
                     solidHeader = T,
                     tags$table(
                       tags$col(width="75"),
                       tags$tr(
                         tags$td(tags$small("Quick Links")),
                         tags$td(
                           HTML(
                             paste(
                               lapply((filter(df, fund_category %in% filter_cond) %>% arrange(desc(nav_recent)))$fund_id, function(i_fundid) {
                                 sprintf("<a href=\"#fund_card_%i\">%s</a>",i_fundid,df[df$fund_id==i_fundid,"short_name"])
                               }),
                               collapse = " | ")
                           )
                         )
                       )
                     )
                 )
          )
        ),
    "not in" = fluidRow(
          column(width = 12,
                 box(width = NULL,
                     solidHeader = T,
                     tags$table(
                       tags$col(width="75"),
                       tags$tr(
                         tags$td(tags$small("Quick Links")),
                         tags$td(
                           HTML(
                             paste(
                               lapply((filter(df, !fund_category %in% filter_cond) %>% arrange(desc(nav_recent)))$fund_id, function(i_fundid) {
                                 sprintf("<a href=\"#fund_card_%i\">%s</a>",i_fundid,df[df$fund_id==i_fundid,"short_name"])
                               }),
                               collapse = " | ")
                           )
                         )
                       )
                     )
                 )
          )
        )
  )
}