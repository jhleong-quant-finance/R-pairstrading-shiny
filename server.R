##############################################################################################
#
#    pairs trading shiny webapp
#
#    version : 1.01 
#    modified on : 7 Oct 2019
#
#    1) add in demo url flag, demo account hide the most profitable pairs.
#    2) aligned the performance charts.
#    3) changed the signal presentation on the chart
#          
#              
##############################################################################################

#options(shiny.trace=TRUE)
#options(shiny.reactlog=TRUE) 
#export R_USER_DIR_CFG_FILE=/home/jhleong/dev/R/cfg/user_dir_cfg.R

Sys.setenv(R_USER_DIR_CFG_FILE = "/home/jhleong/dev/R/cfg/user_dir_cfg.R")
source(Sys.getenv('R_USER_DIR_CFG_FILE'));
source(cfg_file_pairs_trading_shiny);

library(shiny)
library(quantmod)
library(tseries)
library(PerformanceAnalytics)
library(ggthemes)
library(RColorBrewer);
library(googleVis)
library(sit)
library(plyr)
library(RSQLite);
source(paste0(cfg_path_pairs_trading_lib,'/libtrade.R'));
source(paste0(cfg_path_pairs_trading_shiny_src,'/chart_mod.R'));

# use local data for getSymbol()
if(cfg_use_local_symbol_lookup)
  set_local_symbol_lookup();

# configure
cfg_num_pairs_show = 1000;

#options(ar1_MTP_sim.num_sim=200);
options(ar1_MTP_sim.num_sim=10);


sp500_etf100_env <- new.env();
sp500_sp500_env <- new.env();
sp400_sp400_env <- new.env();
sp600_sp600_env <- new.env();
same_ind_env <- new.env();
same_sector_env <- new.env();
all_env <- new.env();

if(cfg_load_sp500_etf100)
    load(file=paste0(cfg_path_pairs_trading_data,'matched_pairs_sp500_etf100_rolling.rda'), envir=sp500_etf100_env);
if(cfg_load_sp500_sp500)
  load(file=paste0(cfg_path_pairs_trading_data,'matched_pairs_sp500_sp500_rolling.rda'), envir=sp500_sp500_env);
if(cfg_load_sp400_sp400)
  load(file=paste0(cfg_path_pairs_trading_data,'matched_pairs_sp400_sp400_rolling.rda'), envir=sp400_sp400_env);
if(cfg_load_sp600_sp600)
  load(file=paste0(cfg_path_pairs_trading_data,'matched_pairs_sp600_sp600_rolling.rda'), envir=sp600_sp600_env);
if(cfg_load_same_ind)
  load(file=paste0(cfg_path_pairs_trading_data,'matched_pairs_same_ind_rolling.rda'), envir=same_ind_env);
if(cfg_load_same_sector)
  load(file=paste0(cfg_path_pairs_trading_data,'matched_pairs_same_sector_rolling.rda'), envir=same_sector_env);
if(cfg_load_all)
  load(file=paste0(cfg_path_pairs_trading_data,'matched_pairs_all_rolling.rda'), envir=all_env);

format_pairs_table <- function(x)
{
  if(is.null(x))
    return(NULL);

  tbl <- x[, c(3, 4, 5, 6, 9, 10, 11, 12, 13)];
  tbl[, c(3:9)] <- round(tbl[, c(3:9)], digits=2);
  
  # min num of trade
  tbl <- tbl[tbl$sample_num_trade > 4,];
  
  # remove NA, Inf
  tbl <- tbl[tbl$sample_num_trade > 4,];
  
  tbl <- head(tbl[order(-tbl[,8]),], cfg_num_pairs_show);
  
  tbl_rowname <- rownames(tbl);
  tbl[,1] <- as.character(tbl[,1]);
  tbl[,2] <- as.character(tbl[,2]);
  rownames(tbl) <- tbl_rowname;
  colnames(tbl) <- c('Symbol1', 'Symbol2', 'Num Crossing', 'Hedge Ratio',
                     'Trade Duration', 'Trade Interval',
                     'Num Trade', 'Ann.Ret', 'Sharpe');
  
  return(tbl);
}

gen_demo_matched_pairs <- function(x){
  # x already sort desc
  # demo acc, hide the first 100 most profitable pairs.
   return(x[100:NROW(x),]);
}

sp500_etf100_env$matched_pairs_df <- format_pairs_table(sp500_etf100_env$matched_pairs_df);
sp500_sp500_env$matched_pairs_df <- format_pairs_table(sp500_sp500_env$matched_pairs_df);
sp400_sp400_env$matched_pairs_df <- format_pairs_table(sp400_sp400_env$matched_pairs_df);
sp600_sp600_env$matched_pairs_df <- format_pairs_table(sp600_sp600_env$matched_pairs_df);
same_ind_env$matched_pairs_df <- format_pairs_table(same_ind_env$matched_pairs_df);
same_sector_env$matched_pairs_df <- format_pairs_table(same_sector_env$matched_pairs_df);
all_env$matched_pairs_df <- format_pairs_table(all_env$matched_pairs_df);

# for demo acc
sp500_etf100_env$demo_matched_pairs_df <- gen_demo_matched_pairs(sp500_etf100_env$matched_pairs_df);
sp500_sp500_env$demo_matched_pairs_df <- gen_demo_matched_pairs(sp500_sp500_env$matched_pairs_df);
sp400_sp400_env$demo_matched_pairs_df <- gen_demo_matched_pairs(sp400_sp400_env$matched_pairs_df);
sp600_sp600_env$demo_matched_pairs_df <- gen_demo_matched_pairs(sp600_sp600_env$matched_pairs_df);
same_ind_env$demo_matched_pairs_df <- gen_demo_matched_pairs(same_ind_env$matched_pairs_df);
same_sector_env$demo_matched_pairs_df <- gen_demo_matched_pairs(same_sector_env$matched_pairs_df);
all_env$demo_matched_pairs_df <- gen_demo_matched_pairs(all_env$matched_pairs_df);

ColorSet <- wsj_pal('colors6')(5)

# SP500 benchmark
sym_sp500 <- getSymbols('SPY', from='1970-01-01', auto.assign=F);
sp500_ret <-  ROC(Cl(adjustOHLC(sym_sp500, use.Adjusted=T)),type='discrete');


# connection to SQLITE db
db_conn <- dbConnect(SQLite(), dbname=cfg_pairs_trading_sqlite_db);

# connection to mysql db
# con <- dbConnect(MySQL(),
#                  user=cfg_db_user, password=cfg_db_passwd,
#                  dbname=cfg_db_dbname, host=cfg_db_host);
#sys.on.exit(dbDisconnect(con));

# Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output, session) {
  source(paste0(cfg_path_pairs_trading_shiny_src, 'db.R'), local=T);
  
  # make R quit after leave browser
  session$onSessionEnded(function() {
    # disconnect SQLITE db
    dbDisconnect(db_conn);
    q();
  })

#*****************************************************************
# get param from URL
#******************************************************************
#session_user <- NULL;
#   demo_acc_flag <- reactive({
#     print(session$clientData$url_search);
#     query <- parseQueryString(session$clientData$url_search)
#         
#     if (!is.null(query$user)) {
#       print(paste('session user:', query$user,sep=''));
#       session_user <- query$user;
#     } else {
#       session_user <- 'guest';
#     }
#     
#     if (!is.null(query$demo_acc)) {
#       print(paste('demo account:', query$demo_acc,sep=''));
#       if(query$demo_acc == 'N') return(FALSE) else return(TRUE);
#     } else { retrun(TRUE); }
#   }) 
#   
  #demo_acc_flag <- NULL;
  session_user <- reactive({
    print(session$clientData$url_search);
    query <- parseQueryString(session$clientData$url_search)
    
    if (!is.null(query$user)) {
      print(paste('session user:', query$user,sep=''));
      return(query$user);
    } else {
      return('guest');
    }    
  }) 
  
  #get the demo account flag
  demo_acc_flag <- reactive({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query$demo_acc)) {
      print(paste('demo account:', query$demo_acc,sep=''));
      if(query$demo_acc == 'N') return(FALSE) else return(TRUE);
    } else {
      return(TRUE);
    }
  }) 
    
  get_dataset <- function(i){
  switch(i ,
         '1' = sp500_etf100_env, 
         '2' = sp500_sp500_env, 
         '3' = sp400_sp400_env,
         '4' = sp600_sp600_env,
         '5' = same_sector_env,
         '6' = same_ind_env,
         '7' = all_env);
  } 
  
  get_pairs_idx <- function(dataset_idx, sym1, sym2){
    p_env <- switch(dataset_idx ,
           '1' = sp500_etf100_env, 
           '2' = sp500_sp500_env, 
           '3' = sp400_sp400_env,
           '4' = sp600_sp600_env,
           '5' = same_sector_env,
           '6' = same_ind_env,
           '7' = all_env);
    p_df <- p_env$matched_pairs_df;
    
    idx <- rownames(p_df[(p_df[,1] == sym1 ) & 
         (p_df[,2] == sym2),]);
    
    return(as.integer(idx));
  } 
   
  env_id <- reactive({
    get_dataset(input$pairs_dataset);
  })
  
  gvis_pot_pairs_ret_jscode <- "
      var sel = chart.getSelection();
      var row = sel[0].row;
      var text = data.getValue(row,1);
      document.getElementById('sym1').value = data.getValue(row,0);
      document.getElementById('sym2').value = data.getValue(row,1);
      $('#sym1').trigger('change');
      $('#sym2').trigger('change');";
  
  output$gvis_pot_pairs_ret <- renderGvis({
    if(demo_acc_flag()){
      matched_pairs_df <- env_id()[['demo_matched_pairs_df']]; 
    } else {
      matched_pairs_df <- env_id()[['matched_pairs_df']]; }
    
    r <- gvisTable(matched_pairs_df,
                   options=list(page='enable',pageSize=15, showRowNumber=F,width='100%',
                                gvis.listener.jscode=gvis_pot_pairs_ret_jscode));

    r;
  })  
  
  #******************************************************************
  # Trade Performance
  #****************************************************************** 
  
  show_pairs_cfg <- reactive({
    # Don't do anything until after the first button push.
    if (input$show_pairs_action == 0)
      return(NULL)
    
    isolate({
      print(input$sym1);
      print(input$sym2); 
      pr <- env_id()[['matched_pairs_df']];      
      p <- pr[(pr[,1] == input$sym1 ) & 
                (pr[,2] == input$sym2),];
      idx <- rownames(p);
      print(idx);
      
      prices <- env_id()[['prices']][paste(input$stock_date_rg[1], 
                                           input$stock_date_rg[2],
                                           sep="::"), 
                                     c(input$sym1,input$sym2)];
      })
    
    return(isolate({
      list(sym1=input$sym1, sym2=input$sym2, 
           start_date=input$stock_date_rg[1], 
           end_date=input$stock_date_rg[2],
           idx=idx, prices=prices);
    }))
  })

  output$perfPlot <- renderPlot({
    
    if(!is.null(show_pairs_cfg() )){
      
      isolate({
        pairs_stat <- env_id()[['matched_pairs_list']][[as.integer(show_pairs_cfg()$idx)]];
      })
        
      daily_ret <- pairs_stat$daily_ret[paste(show_pairs_cfg()$start_date, 
                                   show_pairs_cfg()$end_date,sep="::")];
      sig <- pairs_stat$sig[paste(show_pairs_cfg()$start_date, 
                       show_pairs_cfg()$end_date,sep="::")];
      spread <- pairs_stat$spread[paste(show_pairs_cfg()$start_date, 
                             show_pairs_cfg()$end_date,sep="::")];
      opt_trade_lvl <- pairs_stat$opt_trade_lvl[paste(show_pairs_cfg()$start_date, 
                                        show_pairs_cfg()$end_date,sep="::")];
      
      d_ret <- merge(daily_ret, spread)[,1];
      prev_daily_ret <- which(index(d_ret) < index(daily_ret[1,]));
      d_ret[prev_daily_ret,] <- NA;
      
      charts.PerformanceSummary_mod2(d_ret, sig, opt_trade_lvl, spread );
    }
         
  },height=800)
  
  output$rollingWindowTable <- renderTable({
    if(!is.null(show_pairs_cfg() )){
      isolate({
        pairs_stat <- env_id()[['matched_pairs_list']][[as.integer(show_pairs_cfg()$idx)]];
      })
      
      rolling_df <- data.frame(RollingWindow=as.character(index(pairs_stat$rolling_stat)),
                                 pairs_stat$rolling_stat);
      rownames(rolling_df) <- NULL;
      return(rolling_df);
    }
  })
  
  output$fundamentalTable <- renderGvis({
    if(!is.null(show_pairs_cfg()$sym1)){
      try(temp <- getQuote(c(show_pairs_cfg()$sym1, show_pairs_cfg()$sym2), 
                           what = yahooQF(c("Symbol",
                                            "Name", 
                                            "Market Cap (RT)",
                                            "Ave. Daily Volume", 
                                            "Book Value", 
                                            "Earnings/Share",                                           
                                            "Market Capitalization", 
                                            "Price/Sales", "Price/Book",
                                            "P/E Ratio (RT)", "PEG Ratio", 
                                            "Price/EPS Estimate Current Year", 
                                            "Price/EPS Estimate Next Year",  
                                            "Short Ratio", 
                                            "1 yr Target Price", "Volume",
                                            "52-week Range", "Dividend Yield"))));
      rownames(temp) <- NULL;
      sym <- temp$Symbol;
      temp <- temp[,c(-1,-2)];
      tag <- colnames(temp);
      fundTable <- as.data.frame(t(temp));      
      fundTable <- data.frame(tag, fundTable);
      colnames(fundTable) <- c(' ',sym);

      
      gvisTable(fundTable,
                options=list(page='disable', sort='disable'))    
    }
  })
  
  output$stock1Plot <- renderPlot({
    isolate({
      prices <- env_id()[['prices']] })
    
    if(!is.null(show_pairs_cfg()$sym1)){
      stock1 <- prices[paste(show_pairs_cfg()$start_date, 
                             show_pairs_cfg()$end_date,sep="::"), show_pairs_cfg()$sym1];
      plot(stock1, main=show_pairs_cfg()$sym1);
    }
  })
  
  output$stock2Plot <- renderPlot({
    isolate({
      prices <- env_id()[['prices']] })
    
    if(!is.null(show_pairs_cfg()$sym2)){
      stock2 <- prices[paste(show_pairs_cfg()$start_date, 
                             show_pairs_cfg()$end_date,sep="::"), show_pairs_cfg()$sym2];
      plot(stock2, main=show_pairs_cfg()$sym2);
    }
  })
  

  #******************************************************************
  # Trade Signal Page
  #****************************************************************** 
  
  output$tradeSignalTbl3 <- renderGvis({
    
    if(!is.null(show_pairs_cfg())){
      #sig <- convertIndex(test_result()$sig, 'Date');
      prices <- show_pairs_cfg()$prices;
      
      isolate({
        pairs_stat <- env_id()[['matched_pairs_list']][[as.integer(show_pairs_cfg()$idx)]];
      })
      
      sig <- pairs_stat$sig[paste(show_pairs_cfg()$start_date, 
                                  show_pairs_cfg()$end_date,sep="::")];
      
      sig <- convertIndex(sig, 'Date');
      # the signal need to be lag, the sig1 is the tommorow sig.
      # for finding start trade, end trade
      sig1 <- sig;
      sig <- lag(sig);
      
      
      sig[is.na(sig)] <- 0;
      sig1[is.na(sig1)] <- 0;
      
      # find trades
      tstart = sig != sig1 & sig1 != 0
      tend = sig != 0 & sig != sig1
      
      # execution price logic
      tstarti = which(tstart);
      tendi = which(tend);
      
      print(sig);
      print(sig1);
      print(tstarti);
      print(index(sig[tstarti,]));
      print(index(sig[tendi,]));
      print(head(prices));
      
      if( length(tstarti) > 0 ) {
        if( length(tendi) < length(tstarti) ) tendi = c(tendi, nrow(sig))
        
        entry_date <- index(sig[tstarti,]);
        exit_date <- index(sig[tendi,]);
        
        sym1_trade_entry_price <- as.vector(prices[entry_date, 1]);
        sym2_trade_entry_price <- as.vector(prices[entry_date, 2]);
        sym1_trade_exit_price <- as.vector(prices[exit_date,1]);
        sym2_trade_exit_price <- as.vector(prices[exit_date,2]);
        
        trades_ret <- 0.5  * sign(sig[tstarti+1]) * 
          ( (sym1_trade_exit_price / sym1_trade_entry_price - 1) -
              (sym2_trade_exit_price / sym2_trade_entry_price - 1)
          );
        trades_ret <- as.vector(trades_ret);

        trades = data.frame(
                         as.character(entry_date),
                         as.character(exit_date), 
                         as.numeric(difftime(exit_date, entry_date, units='days')),
                         sym1_trade_entry_price, sym1_trade_exit_price,
                         sym2_trade_entry_price, sym2_trade_exit_price,
                         trades_ret);
        
        colnames(trades) <- c('entry.date','exit.date','Duration','Sym1 entry.price','Sym1 exit.price',
                              'Sym2 entry.price','Sym2 exit.price', 'Return %');
      }
    }
    
    t <- gvisTable(trades,
                   options=list(page='disable', sort='disable', 
                                width='100%', showRowNumber=T, allowHtml=T),
                   formats=list('Sym1 entry.price'='$#.##', 'Sym1 exit.price'='$#.##',
                                'Sym2 entry.price'='$#.##', 'Sym2 exit.price'='$#.##',
                                'Return %'='  #.#%'));
    
    t$html$chart[3] <- gsub("chart.draw\\(data,options\\);", "var formatter = new google.visualization.TableArrowFormat\\(\\); formatter.format\\(data, 7\\);chart.draw\\(data,options\\);", t$html$chart[3])
    t;
  })
})




