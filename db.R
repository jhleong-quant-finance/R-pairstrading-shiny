#******************************************************************
# Add to db
#****************************************************************** 
observe({
  # Don't do anything until after the first button push.
  if (input$add_portf == 0){
    print("add_pairs_portfolio == 0");
    return(NULL);
  }
  
  isolate({
    print("add_pairs_portfolio");
    if(length(input$sym1)>0 && length(input$sym2)>0){
      pr <- env_id()[['matched_pairs_df']];      
      p <- pr[(pr[,1] == input$sym1 ) & 
                (pr[,2] == input$sym2),];
      idx <- rownames(p);
      sql <- paste0("insert into portf ( user_id, dataset, sym1, sym2, idx, mod_time )",
                    "values ('", session_user(), "','",input$pairs_dataset,"',",
                    "'", input$sym1,"','", input$sym2, "',",idx,",",
                    "'",strftime(Sys.time(), format="%Y%m%d %H:%M:%S"),"')");
      print(sql);
      
      dbExecute(db_conn, sql);
    }
    
  })
  
})



#******************************************************************
# Portfolio Page
#****************************************************************** 

portf_df <- reactive({
  tmp <- input$refresh_portf;
  
  dbGetQuery(db_conn, paste0("select * from portf where user_id='", 
                             session_user(), "'"));
})


output$pfTable <- renderTable({
  portf_df();  
})


output$portf_plots <- renderUI({
  df <- portf_df();
  if(NROW(df)>0){

    plot_output_list <- lapply(1:NROW(df), function(i) {
      plotname <- paste0("plot", i);
      
      div(class="span6",
      plotOutput(plotname, height = 280, width = 900)
      )
    })
    
    # Convert the list to a tagList - this is necessary for the list of items
    # to display properly.
    do.call(tagList, plot_output_list)
  }
})

observe({
  if(NROW(portf_df()) > 0 ){
    for (i in 1:NROW(portf_df())) {
      # Need local so that each item gets its own number. Without it, the value
      # of i in the renderPlot() will be the same across all instances, because
      # of when the expression is evaluated.
      local({
        my_i <- i
        
        plotname <- paste("plot", my_i, sep="")
        df <- portf_df()[my_i,];
        
        
        #pairs_stat <- get_dataset(df$dataset)[['matched_pairs_list']][[as.integer(df$idx)]];
        idx <- get_pairs_idx(df$dataset, df$sym1, df$sym2);
        pairs_stat <- get_dataset(df$dataset)[['matched_pairs_list']][[idx]];
        
        len <- NROW(pairs_stat$spread);
        
        sig <- pairs_stat$sig[(len-66):len,];
        spread <- pairs_stat$spread[(len-66):len,];
        opt_trade_lvl <- pairs_stat$opt_trade_lvl[(len-66):len,];
        
        ##### extend the series with real time stock data, code from matching_algo.rolling
        # get stock price of the pairs
        # TODO: use cache or local stock data
        
        # specified the csv source and dir to force load local pricing. The set_local_symbol_lookup() somehow didn't work
        stock1 = NULL; stock2 = NULL;
        tryCatch({
        stock1 <<- getSymbols(pairs_stat$stock1_sym, from='2012-01-01', auto.assign=F, src='csv', dir='../data/finance/US/price');
        stock2 <<- getSymbols(pairs_stat$stock2_sym, from='2012-01-01', auto.assign=F, src='csv', dir='../data/finance/US/price');
        }, error=function(e){}, warning=function(war){})
        
        # without new historical pricing data, skip drawing the spread signal chart
        if(!is.null(stock1) & !is.null(stock2)){
          stock1 = Cl(adjustOHLC(stock1, use.Adjusted=T));
          stock2 = Cl(adjustOHLC(stock2, use.Adjusted=T));
          
          combined <- merge(stock1, stock2);
        
          # get last rolling window
          last_roll <- last(pairs_stat$rolling_stat);
          
          row = which((index(combined) > index(last_roll)));
          
          stock_1 <- combined[row,1];
          stock_2 <- combined[row,2];
          
          # the hedgeRatio, trade lvl from previous rolling window
          hedge_ratio_prev <- as.numeric(last_roll$hedge_ratio);
          opt_trade_lvl_prev <- as.numeric(last_roll$opt_trade_lvl);
          
          print(index(last_roll));
          print(hedge_ratio_prev);
          print(opt_trade_lvl_prev);
          print(last(pairs_stat$spread));
          
          spread_rolling <- stock_1 - hedge_ratio_prev * stock_2
          spread_rolling <- spread_rolling[!is.na(spread_rolling)];
          
          print(spread_rolling); 
          # build the same time series as spread_rolling
          opt_trade_lvl_rolling <- spread_rolling;
          opt_trade_lvl_rolling[] <- opt_trade_lvl_prev;
          
          spread <- rbind(spread, spread_rolling);
          opt_trade_lvl <- rbind(opt_trade_lvl, opt_trade_lvl_rolling);
          
          sig_rule1 <- gen_sig_trade_rule1(spread, opt_trade_lvl, -opt_trade_lvl);
          row_sig <- which((index(sig_rule1) > index(last(sig))));
          sig_rolling <- sig_rule1[row_sig,];
          sig <- rbind(sig, sig_rolling);
          
          ##### extend the series with real time stock data
          print(NROW(sig));
          output[[plotname]] <- renderPlot({
            charts.spread_signal(sig, opt_trade_lvl, spread, main=paste0(df$sym1," - ",df$sym2) );
          })
        }
      })
    }
  }
})
