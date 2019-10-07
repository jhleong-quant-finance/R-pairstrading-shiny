library(RColorBrewer);
  library(PerformanceAnalytics)

pred_cols <- brewer.pal(9, "Set1");

charts.spread_signal <-
  function (sig, opt_trade_lvl, spread, main){
    
    sprd_sig <- merge(spread, opt_trade_lvl, -opt_trade_lvl,  lag(sig));
    ylim <- max(abs(spread), abs(opt_trade_lvl), abs(sig), na.rm=T) * 1.1;
    print(ylim);
    chart.TimeSeries_mod(sprd_sig, 
                         type=c('l','l','l','h'), 
                         lwd=c(1,1,1,1), 
                         lty=c(1,5,5,1),
                         ylim=c(-ylim,ylim),
                         #colorset=c('black','red','red',cm.colors(10)[6]), main=main);
                         colorset=c('black',pred_cols[2],pred_cols[2],cm.colors(10)[6]), main=main);
  }

charts.PerformanceSummary_mod2 <-
function (R, sig, opt_trade_lvl, spread,  Rf = 0, main = NULL, geometric = TRUE, methods = "none", 
          width = 0, event.labels = NULL, ylog = FALSE, wealth.index = FALSE, 
          gap = 12, begin = c("first", "axis"), legend.loc = "topleft", 
          p = 0.95, ...) 
{
  begin = begin[1]
  x = checkData(R)
  colnames = colnames(x)
  ncols = ncol(x)
  length.column.one = length(x[, 1])
  start.row = 1
  start.index = 0
  while (is.na(x[start.row, 1])) {
    start.row = start.row + 1
  }
  x = x[start.row:length.column.one, ]
  if (ncols > 1) 
    legend.loc = legend.loc
  else legend.loc = NULL
  # if (is.null(main)) 
  #   main = paste(colnames[1], "Performance ex", sep = " ")
  if (ylog) 
    wealth.index = TRUE
  op <- par(no.readonly = TRUE)
  
  layout(matrix(c(1, 2)), heights = c(2, 3.5), widths = 1)
  #par(oma = c(2, 0, 4, 0), mar = c(1, 4, 4, 2), mfrow=c(3,1))
  par(mar = c(1, 4, 4, 2))
  
  plot_col <- cm.colors(10)[6];
  ylim <- max(abs(spread))
  
  sprd_sig <- merge(spread, opt_trade_lvl, -opt_trade_lvl,  lag(sig));
  plot_object <- chart.TimeSeries_mod(sprd_sig,
                                      type=c('l','l','l','h'),
                                      lty=c(1,5,5,1),
                                      colorset=c('black',pred_cols[2],pred_cols[2],cm.colors(10)[6]), main="Trade Signal", ...);
  
  par(mar = c(1, 4, 4, 2))
  plot_object <- chart.CumReturns(x, main = "Cumulative Return", 
                                  xaxis = FALSE, legend.loc = legend.loc, event.labels = event.labels, 
                                  ylog = ylog, wealth.index = wealth.index, begin = begin, 
                                  geometric = geometric, ylab = "Cumulative Return", ...)
  par(mar = c(1, 4, 0, 2))
  freq = periodicity(x)
  switch(freq$scale, seconds = {
    date.label = "Second"
  }, minute = {
    date.label = "Minute"
  }, hourly = {
    date.label = "Hourly"
  }, daily = {
    date.label = "Daily"
  }, weekly = {
    date.label = "Weekly"
  }, monthly = {
    date.label = "Monthly"
  }, quarterly = {
    date.label = "Quarterly"
  }, yearly = {
    date.label = "Annual"
  })
  plot_object <- chart.BarVaR(x, main = paste(date.label, "Return"), 
                              xaxis = FALSE, width = width, ylab = paste(date.label, 
                                                                         "Return"), methods = methods, event.labels = NULL, 
                              ylog = FALSE, gap = gap, p = p, add = TRUE, ...)
  par(mar = c(5, 4, 0, 2))
  plot_object <- chart.Drawdown(x, geometric = geometric, main = "Drawdown", 
                                ylab = "Drawdown", event.labels = NULL, ylog = FALSE, 
                                add = TRUE, ...)
  print(plot_object)
  title(main, outer = TRUE)
  par(op)
}

chart.TimeSeries_mod <-
  function (R, auto.grid = TRUE, xaxis = TRUE, yaxis = TRUE, yaxis.right = FALSE, 
            type = "l", lty = 1, lwd = 2, main = NULL, ylab = NULL, xlab = "Date", 
            date.format.in = "%Y-%m-%d", date.format = NULL, xlim = NULL, 
            ylim = NULL, element.color = "darkgray", event.lines = NULL, 
            event.labels = NULL, period.areas = NULL, event.color = "darkgray", 
            period.color = "aliceblue", colorset = (1:12), pch = (1:12), 
            legend.loc = NULL, ylog = FALSE, cex.axis = 0.8, cex.legend = 0.8, 
            cex.lab = 1, cex.labels = 0.8, cex.main = 1, major.ticks = "auto", 
            minor.ticks = TRUE, grid.color = "lightgray", grid.lty = "dotted", 
            xaxis.labels = NULL, ...) 
  {
    y = checkData(R)
    columns = ncol(y)
    rows = nrow(y)
    columnnames = colnames(y)
    if (is.null(date.format)) {
      freq = periodicity(y)
      yr_eq <- ifelse(format(index(first(y)), format = "%Y") == 
                        format(index(last(y)), format = "%Y"), TRUE, FALSE)
      switch(freq$scale, seconds = {
        date.format = "%H:%M"
      }, minute = {
        date.format = "%H:%M"
      }, hourly = {
        date.format = "%d %H"
      }, daily = {
        if (yr_eq) date.format = "%b %d" else date.format = "%Y-%m-%d"
      }, weekly = {
        if (yr_eq) date.format = "%b %d" else date.format = "%Y-%m-%d"
      }, monthly = {
        if (yr_eq) date.format = "%b" else date.format = "%b %y"
      }, quarterly = {
        if (yr_eq) date.format = "%b" else date.format = "%b %y"
      }, yearly = {
        date.format = "%Y"
      })
    }
    rownames = as.Date(xts:::time.xts(y))
    rownames = format(strptime(rownames, format = date.format.in), 
                      date.format)
    time.scale = periodicity(y)$scale
    ep = axTicksByTime(y, major.ticks, format.labels = date.format)
    logaxis = ""
    if (ylog) {
      logaxis = "y"
    }
    plot.new()
    if (is.null(xlim[1])) 
      xlim = c(1, rows)
    if (is.null(ylim[1])) {
      ylim = as.numeric(range(y, na.rm = TRUE))
    }
    plot.window(xlim, ylim, xaxs = "r", log = logaxis)
    if (is.null(ylab)) {
      if (ylog) 
        ylab = "ln(Value)"
      else ylab = "Value"
    }
    if (ylog) 
      dimensions = 10^par("usr")
    else dimensions = par("usr")
    if (!is.null(period.areas)) {
      period.dat = lapply(period.areas, function(x, y) c(first(index(y[x])), 
                                                         last(index(y[x]))), y = y)
      period.ind = NULL
      for (period in 1:length(period.dat)) {
        if (!is.na(period.dat[[period]][1])) {
          period.ind = list(grep(period.dat[[period]][1], 
                                 index(y)), grep(period.dat[[period]][2], index(y)))
          rect(period.ind[1], dimensions[3], period.ind[2], 
               dimensions[4], col = period.color, border = NA)
        }
      }
    }
    if (auto.grid) {
      abline(v = ep, col = grid.color, lty = grid.lty)
      grid(NA, NULL, col = grid.color)
    }
    abline(h = 0, col = element.color)
    if (!is.null(event.lines)) {
      event.ind = NULL
      for (event in 1:length(event.lines)) {
        event.ind = c(event.ind, grep(event.lines[event], 
                                      rownames))
      }
      number.event.labels = ((length(event.labels) - length(event.ind) + 
                                1):length(event.labels))
      abline(v = event.ind, col = event.color, lty = 2)
      if (!is.null(event.labels)) {
        text(x = event.ind, y = ylim[2], label = event.labels[number.event.labels], 
             offset = 0.2, pos = 2, cex = cex.labels, srt = 90, 
             col = event.color)
      }
    }
    
    #rescale the signal 
    #y[, 4] <- ifelse(y[,4] > 0, y[,4]*dimensions[4], -y[,4]*dimensions[3]);
    
    if (length(lwd) < columns) 
      lwd = rep(lwd, columns)
    if (length(lty) < columns) 
      lty = rep(lty, columns)
    if (length(pch) < columns) 
      pch = rep(pch, columns)
    for (column in (columns-1):1) {
      if(column == 1){

        sig <- y[, 4]
        sigal_col= ifelse( sig == -1, pred_cols[1], ifelse(sig == 1, pred_cols[3], pred_cols[9]));
        segments(1:(rows-1), y[1:(rows-1), column], x1=2:rows , y1=y[2:(rows), column], 
                 col = sigal_col, lwd = lwd[column], 
              pch = pch[column], lty = lty[column],
              ...)
      } else {
      
      lines(1:rows, y[, column], col = colorset[column], lwd = lwd[column], 
            pch = pch[column], lty = lty[column], type = type[column], 
            ...)
      }
    }
    if (xaxis) {
      if (minor.ticks) 
        axis(1, at = 1:NROW(y), labels = FALSE, col = "#BBBBBB")
      label.height = cex.axis * (0.5 + apply(t(names(ep)), 
                                             1, function(X) max(strheight(X, units = "in")/par("cin")[2])))
      if (is.null(xaxis.labels)) 
        xaxis.labels = names(ep)
      else ep = 1:length(xaxis.labels)
      axis(1, at = ep, labels = xaxis.labels, las = 1, lwd = 1, 
           mgp = c(3, label.height, 0), cex.axis = cex.axis)
      title(xlab = xlab, cex = cex.lab)
    }
    if (yaxis) 
      if (yaxis.right) 
        axis(4, cex.axis = cex.axis, col = element.color, 
             ylog = ylog)
    else axis(2, cex.axis = cex.axis, col = element.color, 
              ylog = ylog)
    box(col = element.color)
    if (!is.null(legend.loc)) {
      legend(legend.loc, inset = 0.02, text.col = colorset, 
             col = colorset, cex = cex.legend, border.col = element.color, 
             lty = lty, lwd = 2, bg = "white", legend = columnnames, 
             pch = pch)
    }
    if (is.null(main)) 
      main = columnnames[1]
    title(ylab = ylab, cex = cex.lab)
    title(main = main, cex = cex.main)
  }
