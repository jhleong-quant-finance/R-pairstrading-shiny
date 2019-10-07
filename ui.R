

mainPanel_customize <- function(...) {
  div(class="span12",
      ...
  )
}

#dataRangeInput with input and label display in horizontal
dateRangeInput_h <- function (inputId, label, start = NULL, end = NULL, min = NULL, 
          max = NULL, format = "yyyy-mm-dd", startview = "month", weekstart = 0, 
          language = "en", separator = " to ") 
{
  if (inherits(start, "Date")) 
    start <- format(start, "%Y-%m-%d")
  if (inherits(end, "Date")) 
    end <- format(end, "%Y-%m-%d")
  if (inherits(min, "Date")) 
    min <- format(min, "%Y-%m-%d")
  if (inherits(max, "Date")) 
    max <- format(max, "%Y-%m-%d")
  tagList(singleton(tags$head(tags$script(src = "shared/datepicker/js/bootstrap-datepicker.min.js"), 
                              tags$link(rel = "stylesheet", type = "text/css", href = "shared/datepicker/css/datepicker.css"))), 
          tags$div(id = inputId, class = "shiny-date-range-input input-daterange", 
                   tags$div(class='row-fluid', 
                            tags$div(style="float:left; margin:0px 10px 0px 10px;",
                              tags$label(id = inputId, label)),
                            tags$div(style="float:left;",
                                     tags$input(class = "input-small", 
                                     type = "text", `data-date-language` = language, 
                                     `data-date-weekstart` = weekstart, 
                                     `data-date-format` = format, 
                                     `data-date-start-view` = startview, 
                                     `data-min-date` = min, 
                                     `data-max-date` = max, `data-initial-date` = start)), 
                            tags$div(style="float:left; margin:0px 10px 0px 10px;", HTML(separator)), 
                            tags$div(style="float:left;",tags$input(class = "input-small", 
                                     type = "text", `data-date-language` = language, 
                                     `data-date-weekstart` = weekstart, `data-date-format` = format, 
                                     `data-date-start-view` = startview, `data-min-date` = min, 
                                     `data-max-date` = max, `data-initial-date` = end)))))
}

# Define UI for dataset viewer application
shinyUI(bootstrapPage(
  # Show the caption, a summary of the dataset and an HTML table with
  # the requested number of observations
  #tags$link(rel="stylesheet", type="text/css", href="style.css"),
  tags$link(rel="stylesheet", type="text/css", href="css/font-awesome.css"),
  tags$link(rel="stylesheet", href="css/bootstrap.icon-large.min.css"),
  tags$link(rel="stylesheet", type="text/css", href="css/big.css"),
  tags$link(rel="stylesheet", type="text/css", href="css/css.css"),
  
  mainPanel_customize(
    tabsetPanel(
      tabPanel("Portfolio",
               actionButton("refresh_portf", "Refresh"),
               div(class="widget",
                   div(class="widget-header",
                       tags$i(class="icon-large icon-table"),
                       h3("Watch list table")
                   ),
                   div(class="widget-content",
                       tableOutput("pfTable")
                   )
               ),
               div(class="widget",
                   div(class="widget-header",
                       tags$i(class="icon-large icon-th"),
                       h3("Watch list real time chart")
                   ),
                   div(class="widget-content",
                       uiOutput("portf_plots")
                   )
               )
               ),
      tabPanel("Analysis",
               wellPanel(style="height:92px;", 
               div(class="row-fluid",
                   div(style="float:left; margin-right:20px;", 
               selectInput(inputId ="pairs_dataset",  label = "Choose a dataset:", 
                           choices = c("SP500, ETF100 pairs"=1, "SP500, SP500 pairs"=2, 
                                       "SP400, SP400 pairs"=3, "SP600, SP600 pairs"=4,
                                       "Same Sector pairs"=5,
                                       "Same Industry and Sector pairs"=6,                             
                                       "All stocks"=7))),
                   div(style="float:left; margin-right:20px;", 
               textInput("sym1", "Symbol 1:", "")),
                   div(style="float:left; margin-right:20px;", 
               textInput("sym2", "Symbol 2:", "")))),
               tabsetPanel(      
                 tabPanel("Potential Pairs", 
                          tags$div(class="span11", style="float:left; margin:10px 10px 0px 10px;",
                                   div(class="widget",
                                       div(class="widget-header",
                                           tags$i(class="icon-large icon-table"),
                                           h3("Pairs Stats.")
                                       ),
                                       div(class="widget-content",                                  
                                           htmlOutput("gvis_pot_pairs_ret")
                                       )
                                   )
                          )
                 ) ,
                 tabPanel("historical Performance",  
                          wellPanel(style="height:90px;", 
                            div(class="row-fluid",
                            div(style="float:left;", 
                                dateRangeInput("stock_date_rg", "Zoom into date range :",
                                           start = "2012-01-03", end = "2014-01-03",
                                           min = "2012-01-03", max = "2014-01-03")),
                                div(style="float:left; margin:20px 10px 0px 10px;", 
                                    actionButton("show_pairs_action", "Show Pairs")),
                                div(style="float:left; margin:20px 10px 0px 0px;", 
                                    actionButton("add_portf", "Add to Portfolio"))
                          )),
                          div(class="widget",
                              div(class="widget-header",
                                  tags$i(class="icon-large icon-stats "),
                                  #tags$i(class="fa fa-bar-chart-o fa-lg"),
                                  h3("Historical Performance")
                              ),
                              div(class="widget-content",
                                  plotOutput("perfPlot", height="100%")
                              )
                          ),
                          
                          div(class="widget widget-table",
                              div(class="widget-header",
                                  tags$i(class="icon-large icon-th"),
                                  h3("Pairs rolling windows stat")
                              ),
                              div(class="widget-content",
                                  tableOutput("rollingWindowTable")
                              )
                          )
                 ),
                 tabPanel("Pair Components",
                          div(class="widget",
                              div(class="widget-header",
                                  tags$i(class="icon-large icon-stats"),
                                  h3("Indv. stock charts")
                              ),
                              div(class="widget-content",
                                  plotOutput("stock1Plot"),
                                  plotOutput("stock2Plot")
                              )
                          ),
                          div(class="widget",
                              div(class="widget-header",
                                  tags$i(class="icon-large icon-table"),
                                  h3("Fundamental Data")
                              ),
                              div(class="widget-content",                                  
                                  htmlOutput("fundamentalTable")
                              )
                          )
                 ),                 
                 tabPanel("Trade Signal", 
                          div(class="widget",
                              div(class="widget-header",
                                  tags$i(class="icon-large icon-wifi-alt"),
                                  h3("Trade Signal")
                              ),
                              div(class="widget-content",
                                  htmlOutput("tradeSignalTbl3")
                              )
                          )
                 )
               )
      )
    )
  )
))
