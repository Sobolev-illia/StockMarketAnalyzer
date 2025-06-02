# server.R
library(shiny)
library(quantmod)
library(dplyr)
library(TTR)
library(zoo)    
library(ggplot2)
library(plotly)
library(tidyr)  
library(reshape2) 
library(GGally)

PRIMARY_COLOUR   <- "#1f77b4"
SECONDARY_COLOUR <- "#ff7f0e"

server <- function(input, output, session) {
  # ---------------------------------------
  # Reactive: Fetch & combine all selected symbols
  # ---------------------------------------
  comparison_data <- reactive({
    req(input$stock_symbols, input$date_range)
    syms <- input$stock_symbols
    from <- input$date_range[1]
    to   <- input$date_range[2]
    
    df_list <- lapply(syms, function(sym) {
      xts_obj <- tryCatch({
        getSymbols(sym, src = "yahoo", auto.assign = FALSE, from = from, to = to)
      }, error = function(e) {
        return(NULL)
      })
      
      if (is.null(xts_obj) || NROW(xts_obj) == 0) {
        return(NULL)
      }
      
      tmp <- data.frame(
        Date     = index(xts_obj),
        coredata(xts_obj)
      )
      colnames(tmp) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
      tmp$Date   <- as.Date(tmp$Date)
      tmp$Symbol <- sym
      return(tmp)
    })
    
    combined <- bind_rows(df_list)
    req(nrow(combined) > 0)
    
    # Calculate daily return for each symbol
    combined <- combined %>%
      group_by(Symbol) %>%
      arrange(Date) %>%
      mutate(daily_return = Close / lag(Close) - 1) %>%
      ungroup()
    
    return(combined)
  })
  
  # ---------------------------------------
  # Reactive: Fetch data for a single symbol
  # ---------------------------------------
  stock_data <- reactive({
    req(input$stock_symbol, input$date_range)
    sym  <- input$stock_symbol
    from <- input$date_range[1]
    to   <- input$date_range[2]
    
    xts_obj <- tryCatch({
      getSymbols(sym, src = "yahoo", auto.assign = FALSE, from = from, to = to)
    }, error = function(e) {
      return(NULL)
    })
    
    if (is.null(xts_obj) || NROW(xts_obj) == 0) {
      showModal(modalDialog(
        title = "No Data Available",
        paste("No data for", sym, "between", from, "and", to),
        easyClose = TRUE
      ))
      return(NULL)
    }
    
    df <- data.frame(
      Date     = index(xts_obj),
      coredata(xts_obj)
    )
    colnames(df) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
    df$Date <- as.Date(df$Date)
    df
  })
  
  
  # ValueBox: Current Close Price
  output$vb_current_price <- renderValueBox({
    df <- stock_data()
    req(df)
    last_price <- tail(df$Close, 1)
    valueBox(
      subtitle = "Current Close Price",
      value    = paste0("$", format(round(last_price, 2), nsmall = 2)),
      icon     = icon("dollar-sign"),
      color    = "blue"
    )
  })
  
  # ValueBox: Daily % Change
  output$vb_daily_change <- renderValueBox({
    df <- stock_data()
    req(df)
    if (nrow(df) < 2) {
      pct_change <- NA
    } else {
      last_row <- tail(df, 1)
      prev_row <- tail(df, 2)[1, ]
      pct_change <- (last_row$Close / prev_row$Close - 1) * 100
    }
    change_str <- if (is.na(pct_change)) {
      "—"
    } else {
      sprintf("%+.2f%%", pct_change)
    }
    valueBox(
      subtitle = "Daily % Change",
      value    = change_str,
      icon     = icon(ifelse(is.na(pct_change), "minus", ifelse(pct_change >= 0, "arrow-up", "arrow-down"))),
      color    = ifelse(is.na(pct_change), "orange",
                        ifelse(pct_change >= 0, "green", "red"))
    )
  })
  
  # ValueBox: Volume
  output$vb_volume <- renderValueBox({
    df <- stock_data()
    req(df)
    vol     <- tail(df$Volume, 1)
    vol_str <- format(vol, big.mark = ",", scientific = FALSE)
    valueBox(
      subtitle = "Last Day Volume",
      value    = vol_str,
      icon     = icon("chart-bar"),
      color    = "blue"
    )
  })
  
  # ---------------------------------------
  # Stock Price Trend
  # ---------------------------------------
  output$stock_plot <- renderPlotly({
    df <- stock_data()
    req(df)
    
    p <- ggplot(df, aes(x = Date, y = Close, key = Date)) +
      geom_line(color = PRIMARY_COLOUR, size = 0.8) +
      labs(
        title = paste(input$stock_symbol, "Closing Price"),
        x     = NULL,
        y     = NULL
      ) +
      scale_y_continuous(
        labels = function(x) paste0(x, " USD")
      ) +
      theme_minimal(base_family = "Montserrat") +
      theme(
        plot.title = element_text(face = "regular", size = 12),
        axis.title  = element_text(size = 12),
        plot.margin = margin(t = 30, r = 10, b = 10, l = 10)
      )
    
    ggplotly(p, source = "stock_trend", tooltip = c("x", "y")) %>%
      layout(dragmode = "select") %>%
      config(displayModeBar = TRUE) %>%
      event_register("plotly_selected") %>%
      event_register("plotly_click")
  })
  
  
  # ---------------------------------------
  # Stock Comparison
  # ---------------------------------------
  output$comparison_plot <- renderPlotly({
    df <- comparison_data() %>%
      mutate(Symbol = factor(Symbol, levels = input$stock_symbols)) %>%
      arrange(Symbol, Date)
    req(df)
    
    colour_map <- c(
      "AAPL" = PRIMARY_COLOUR,
      "GOOG" = SECONDARY_COLOUR,
      "MSFT" = "#2ca02c",
      "AMZN" = "#d62728",
      "NVDA" = "#9467bd",
      "TSLA" = "#8c564b",
      "NFLX" = "#e377c2"
    )
    
    p2 <- ggplot(df, aes(x = Date, y = Close, color = Symbol, group = Symbol, key = Symbol)) +
      geom_line(size = 0.8) +
      scale_color_manual(values = colour_map) +
      labs(
        title = "Closing Price Comparison",
        x     = NULL,
        y     = NULL
      ) +
      scale_y_continuous(labels = function(x) paste0(x, " USD")) +
      theme_minimal(base_family = "Montserrat") +
      theme(
        plot.title   = element_text(face = "regular", size = 12),
        axis.title   = element_text(size = 12),
        plot.margin  = margin(t = 30, r = 10, b = 10, l = 10),
        legend.title = element_blank()
      )
    
    ggplotly(p2, source = "stock_comp", tooltip = c("x", "y", "colour")) %>%
      event_register("plotly_click") %>%
      event_register("plotly_selected") %>%
      layout(
        dragmode = "select",
        legend = list(
          orientation = "h",
          x = 0.5, xanchor = "center", y = -0.2
        )
      ) %>%
      config(displayModeBar = TRUE)
  })
  
  
  # ---------------------------------------
  # ROLLING VOLATILITY (20-DAY) Plot
  # ---------------------------------------
  output$comparison_vol_plot <- renderPlotly({
    df <- comparison_data() %>%
      mutate(Symbol = factor(Symbol, levels = input$stock_symbols)) %>%
      arrange(Symbol, Date)
    req(df)
    
    vol_df <- df %>%
      group_by(Symbol) %>%
      arrange(Date) %>%
      mutate(
        # use 20-day rolling std dev of daily_return
        vol20 = TTR::runSD(daily_return, n = 20)
      ) %>%
      ungroup()
    
    colour_map <- c(
      "AAPL" = PRIMARY_COLOUR,
      "GOOG" = SECONDARY_COLOUR,
      "MSFT" = "#2ca02c",
      "AMZN" = "#d62728",
      "NVDA" = "#9467bd",
      "TSLA" = "#8c564b",
      "NFLX" = "#e377c2"
    )
    
    p_vol <- ggplot(vol_df, aes(
      x = Date, y = vol20 * 100,
      color = Symbol, group = Symbol,
      key = Symbol
    )) +
      geom_line(size = 0.7) +
      scale_color_manual(values = colour_map) +
      labs(
        title = NULL,
        x     = NULL,
        y     = NULL
      ) +
      theme_minimal(base_family = "Montserrat") +
      theme(
        plot.title   = element_text(face = "regular", size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        plot.margin  = margin(t = 0, r = 10, b = 10, l = 10)
      )
    
    ggplotly(p_vol, source = "stock_comp_vol", tooltip = c("x", "y", "colour")) %>%
      event_register("plotly_click") %>%
      layout(
        legend = list(
          orientation = "h",
          x = 0.5, xanchor = "center", y = -0.2
        )
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  
  # ---------------------------------------
  # MAX DRAWDOWN (%) Plot
  # ---------------------------------------
  output$comparison_dd_plot <- renderPlotly({
    df <- comparison_data() %>%
      mutate(Symbol = factor(Symbol, levels = input$stock_symbols)) %>%
      arrange(Symbol, Date)
    req(df)
    
    dd_df <- df %>%
      group_by(Symbol) %>%
      arrange(Date) %>%
      mutate(
        running_max = cummax(Close),
        drawdown    = (Close - running_max) / running_max
      ) %>%
      ungroup()
    
    colour_map <- c(
      "AAPL" = PRIMARY_COLOUR,
      "GOOG" = SECONDARY_COLOUR,
      "MSFT" = "#2ca02c",
      "AMZN" = "#d62728",
      "NVDA" = "#9467bd",
      "TSLA" = "#8c564b",
      "NFLX" = "#e377c2"
    )
    
    p_dd <- ggplot(dd_df, aes(
      x = Date, y = drawdown * 100,
      color = Symbol, group = Symbol,
      key = Symbol
    )) +
      geom_line(size = 0.7) +
      scale_color_manual(values = colour_map) +
      labs(
        title = NULL,
        x     = NULL,
        y     = NULL
      ) +
      scale_y_continuous(labels = function(x) paste0(x, "%")) +
      theme_minimal(base_family = "Montserrat") +
      theme(
        plot.title   = element_text(face = "regular", size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        plot.margin  = margin(t = 0, r = 10, b = 10, l = 10)
      )
    
    ggplotly(p_dd, source = "stock_comp_dd", tooltip = c("x", "y", "colour")) %>%
      event_register("plotly_click") %>%
      layout(
        legend = list(
          orientation = "h",
          x = 0.5, xanchor = "center", y = -0.2
        )
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  
    
  
  # ---------------------------------------
  # Linked Highlighting by Symbol
  # ---------------------------------------
  observe({
    # Listen for clicks in any comparison plot with source = "stock_comp"
    d_price <- event_data("plotly_click", source = "stock_comp")
    d_vol   <- event_data("plotly_click", source = "stock_comp_vol")
    d_dd    <- event_data("plotly_click", source = "stock_comp_dd")
    
    # Determine which event fired (price, vol, or drawdown)
    d <- d_price
    if (is.null(d) && !is.null(d_vol)) d <- d_vol
    if (is.null(d) && !is.null(d_dd))  d <- d_dd
    
    # If no valid click event, reset all opacities to 1
    if (is.null(d)) {
      syms <- input$stock_symbols
      n    <- length(syms)
      if (n > 0) {
        full_opacities <- rep(1, n)
        trace_ids      <- as.list(0:(n-1))
        
        # Reset price plot
        plotlyProxy("comparison_plot", session) %>%
          plotlyProxyInvoke("restyle", list(opacity = full_opacities), trace_ids)
        # Reset volatility plot
        plotlyProxy("comparison_vol_plot", session) %>%
          plotlyProxyInvoke("restyle", list(opacity = full_opacities), trace_ids)
        # Reset drawdown plot
        plotlyProxy("comparison_dd_plot", session) %>%
          plotlyProxyInvoke("restyle", list(opacity = full_opacities), trace_ids)
      }
      return()
    }
    
    clicked_symbol <- d$key 
    syms           <- input$stock_symbols
    n              <- length(syms)
    if (n == 0) return()
    
    opacities  <- rep(0.2, n)
    selected_i <- which(syms == clicked_symbol)
    if (length(selected_i) == 1) {
      opacities[selected_i] <- 1
    } else {
      opacities <- rep(1, n)
    }
    
    trace_ids <- as.list(0:(n-1)) 
    
    # Apply opacities to each plot via plotlyProxy
    plotlyProxy("comparison_plot", session) %>%
      plotlyProxyInvoke("restyle", list(opacity = opacities), trace_ids)
    
    plotlyProxy("comparison_vol_plot", session) %>%
      plotlyProxyInvoke("restyle", list(opacity = opacities), trace_ids)
    
    plotlyProxy("comparison_dd_plot", session) %>%
      plotlyProxyInvoke("restyle", list(opacity = opacities), trace_ids)
  })
  
  # ---------------------------------------
  # Performance – 50-day SMA
  # ---------------------------------------
  output$performance_plot_sma <- renderPlotly({
    df <- stock_data()
    req(df)
    if (nrow(df) < 50) {
      showModal(modalDialog(
        title   = "Not Enough Data",
        "Need at least 50 data points to calculate a 50-day SMA.",
        easyClose = TRUE
      ))
      return(NULL)
    }
    df$sma50 <- TTR::SMA(df$Close, n = 50)
    
    p_sma <- ggplot(df, aes(x = Date)) +
      geom_line(aes(y = Close), color = PRIMARY_COLOUR, size = 0.6) +
      geom_line(aes(y = sma50),  color = SECONDARY_COLOUR, size = 0.8, linetype = "dash") +
      labs(
        title = paste(input$stock_symbol, "Closing Price & 50-Day SMA"),
        x     = NULL,
        y     = NULL
      ) +
      scale_y_continuous(labels = function(x) paste0(x, " USD")) +
      theme_minimal(base_family = "Montserrat") +
      theme(
        plot.title = element_text(face = "regular", size = 12),
        axis.title  = element_text(size = 12),
        plot.margin = margin(t = 30, r = 10, b = 10, l = 10)
      )
    
    ggplotly(p_sma, tooltip = c("x", "y")) %>%
      config(displayModeBar = FALSE)
  })
  
  # ---------------------------------------
  # Performance – 14-day RSI
  # ---------------------------------------
  output$performance_plot_rsi <- renderPlotly({
    df <- stock_data()
    req(df)
    if (nrow(df) < 15) {
      showModal(modalDialog(
        title   = "Not Enough Data",
        "Need at least 15 data points to calculate the 14-day RSI.",
        easyClose = TRUE
      ))
      return(NULL)
    }
    df$rsi14 <- TTR::RSI(df$Close, n = 14)
    
    p_rsi <- ggplot(df, aes(x = Date, y = rsi14)) +
      geom_line(color = PRIMARY_COLOUR, size = 0.8) +
      geom_hline(yintercept = 70, linetype = "dashed", color = "grey50") +
      geom_hline(yintercept = 30, linetype = "dashed", color = "grey50") +
      labs(
        title = paste(input$stock_symbol, "14-Day RSI"),
        x     = NULL,
        y     = NULL
      ) +
      scale_y_continuous(labels = function(x) paste0("RSI: ", x)) +
      theme_minimal(base_family = "Montserrat") +
      theme(
        plot.title = element_text(face = "regular", size = 12),
        axis.title  = element_text(size = 12),
        plot.margin = margin(t = 30, r = 10, b = 10, l = 10)
      )
    
    ggplotly(p_rsi, tooltip = c("x", "y")) %>%
      config(displayModeBar = FALSE)
  })
  
  # ---------------------------------------
  # Data Table (DT)
  # ---------------------------------------
  output$stock_data <- renderDT({
    df <- stock_data()
    req(df)
    df_display <- df %>% select(Date, Open, High, Low, Close, Volume)
    datatable(
      df_display,
      rownames   = FALSE,
      extensions = "Buttons",
      options    = list(
        pageLength = 10,
        dom        = 'Bfrtip',
        buttons    = c('copy','csv','excel','pdf','print'),
        columnDefs = list(list(targets = 0, searchable = FALSE))
      )
    )
  })
}