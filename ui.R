library(shiny)
library(shinydashboard)
library(plotly)      
library(DT)

# Define your “main” colours here
PRIMARY_COLOUR   <- "#1f77b4"   # a blue
SECONDARY_COLOUR <- "#ff7f0e"   # an orange

ui <- dashboardPage(
  dashboardHeader(
    title = tags$div(
      style = "font-family: 'Montserrat', sans-serif; font-weight: bold;",
      "Stock Market Analyzer"
    )
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Stock Prices",       tabName = "stock_prices", icon = icon("line-chart")),
      menuItem("Stock Comparison",   tabName = "comparison",   icon = icon("chart-bar")),
      menuItem("Performance Analysis", tabName = "performance", icon = icon("area-chart")),
      menuItem("Data Table",         tabName = "data_table",   icon = icon("table")),
      menuItem("Help",               tabName = "help",         icon = icon("question-circle"))
    )
  ),
  
  dashboardBody(
    # 1) Import Montserrat from Google Fonts & set default theme styles
    tags$head(
      tags$link(
        href = "https://fonts.googleapis.com/css2?family=Montserrat:wght@400;600&display=swap",
        rel  = "stylesheet"
      ),
      tags$style(HTML("
        body, label, h3, h4, .info-box-text, .info-box-number {
          font-family: 'Montserrat', sans-serif !important;
        }
        .box { border-top: 3px solid #1f77b4; }
        .box-header .box-title { font-weight: 600; }
        
        .small-box.bg-primary { background-color: #1f77b4 !important; color: white !important; }
        .small-box.bg-secondary { background-color: #ff7f0e !important; color: white !important; }
      "))
    ),
    
    tabItems(
      ## ——————————————————————————————
      ## STOCK PRICES TAB
      ## ——————————————————————————————
      tabItem(
        tabName = "stock_prices",
        fluidRow(
          valueBoxOutput("vb_current_price", width = 4),
          valueBoxOutput("vb_daily_change",  width = 4),
          valueBoxOutput("vb_volume",        width = 4)
        ),
        fluidRow(
          box(
            title = "SELECT STOCK & DATE RANGE",
            width = 12,
            background = "blue",
            solidHeader = TRUE,
            column(
              width = 4,
              selectInput(
                inputId  = "stock_symbol",
                label    = tags$span(style = "color:white; font-weight: 400;", "Choose Stock Symbol"),
                choices = c("AAPL","GOOG","MSFT","AMZN","NVDA","TSLA","NFLX"),
                selected = "AAPL"
              )
            ),
            column(
              width = 4,
              dateRangeInput(
                inputId = "date_range",
                label   = tags$span(style = "color:white; font-weight: 400;", "Date Range"),
                start   = Sys.Date() - 365,
                end     = Sys.Date()
              )
            )
          )
        ),
        fluidRow(
          box(
            title = "STOCK PRICE TREND",
            width = 12,
            background = "blue",
            solidHeader = TRUE,
            style = "padding:0;",
            plotlyOutput("stock_plot", height = "400px")
          )
        )
      ),
      
      
      ## ——————————————————————————————
      ## STOCK COMPARISON TAB 
      ## ——————————————————————————————
      tabItem(
        tabName = "comparison",
        
        fluidRow(
          column(
            width = 9,
            
            box(
              title       = "STOCK COMPARISON",
              width       = 12,
              background  = "blue",
              solidHeader = TRUE,
              style       = "padding:0;",
              plotlyOutput("comparison_plot", height = "400px")
            )
          ),
          
          column(
            width = 3,
            
            box(
              title       = "SELECT MULTIPLE STOCKS",
              width       = 12,
              background  = "blue",
              solidHeader = TRUE,
              selectInput(
                inputId   = "stock_symbols",
                label     = tags$span(style = "color:white;", "Choose Stocks"),
                choices   = c("AAPL","GOOG","MSFT","AMZN","NVDA","TSLA","NFLX"),
                selected  = c("AAPL", "GOOG"),
                multiple  = TRUE
              )
            )
          )
        ),  
        
        fluidRow(
          column(
            width = 6,
            box(
              title       = "ROLLING VOLATILITY (20-DAY)",
              width       = 12,
              background  = "blue",
              solidHeader = TRUE,
              style       = "padding:0;",
              plotlyOutput("comparison_vol_plot", height = "300px")
            )
          ),
          column(
            width = 6,
            box(
              title       = "MAX DRAWDOWN (%)",
              width       = 12,
              background  = "blue",
              solidHeader = TRUE,
              style       = "padding:0;",
              plotlyOutput("comparison_dd_plot", height = "300px")
            )
          )
        ) 
      ),  
      
      
      
      ## ——————————————————————————————
      ## PERFORMANCE ANALYSIS TAB
      ## ——————————————————————————————
      tabItem(
        tabName = "performance",
        
        fluidRow(
          # Left column: two stacked plot boxes (50‐day SMA & RSI)
          column(
            width = 9,
            box(
              title       = "50‐DAY SMA VS CLOSING PRICE",
              width       = 12,
              background  = "blue",
              solidHeader = TRUE,
              style       = "padding:0;",
              plotlyOutput("performance_plot_sma", height = "350px")
            ),
            box(
              title       = "RSI (14‐DAY)",
              width       = 12,
              background  = "blue",
              solidHeader = TRUE,
              style       = "padding:0;",
              plotlyOutput("performance_plot_rsi", height = "350px")
            )
          ),
          
          column(
            width = 3,
            box(
              title = "SELECT STOCK",
              width = 12,
              background = "blue",
              solidHeader = TRUE,
              column(
                width = 12,
                selectInput(
                  inputId  = "stock_symbol",
                  label = tags$span(style = "color:white; font-weight: 400;", "Choose Stock Symbol"),
                  choices  = c("AAPL","GOOG","MSFT","AMZN","NVDA","TSLA","NFLX"),
                  selected = "AAPL"
                )
              )
            ),
            box(
              title       = "SELECT DATE RANGE",
              width       = 12,
              background  = "blue",
              solidHeader = TRUE,
              column(
                width = 12,
                dateRangeInput(
                  inputId = "date_range",
                  label   = tags$span(style = "color:white; font-weight: 400;", "Date Range"),
                  start   = Sys.Date() - 365,
                  end     = Sys.Date()
                )
              )
            ),
          )
        )
      ),
      
      
      ## ——————————————————————————————
      ## DATA TABLE TAB
      ## ——————————————————————————————
      tabItem(
        tabName = "data_table",
        fluidRow(
          box(
            title = "RAW STOCK DATA TABLE",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            DTOutput("stock_data")
          )
        )
      ),
      
      
      ## ——————————————————————————————
      ## HELP TAB
      ## ——————————————————————————————
      tabItem(
        tabName = "help",
        h3("STOCK MARKET ANALYZER DASHBOARD"),
        p("Use this dashboard to analyze stock market trends, compare multiple symbols, and view performance indicators (SMA, RSI, etc.)."),
        tags$ul(
          tags$li("Select a stock and date range on the 'Stock Prices' tab to see an interactive time-series plot."),
          tags$li("Compare up to seven symbols on the 'Stock Comparison' tab. Clicking a point on any chart will highlight the same date across both charts."),
          tags$li("On the 'Performance Analysis' tab, view the 50-day Simple Moving Average (SMA) and the 14-day Relative Strength Index (RSI)."),
          tags$li("The 'Data Table' tab shows the raw OHLC + Volume data in a searchable, paginated table.")
        )
      )
    )
  )
)
