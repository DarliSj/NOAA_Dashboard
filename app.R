library(shiny)
library(bslib)
library(tidyverse)
library(DT)
library(plotly)
library(leaflet)
library(shinyjs)  # for tab/element toggling
library(shinycssloaders)  # for spinner loader
# DuckDB for data fetch
library(DBI)
library(duckdb) 

# Assume filtered_stations is loaded in the global environment
# filtered_stations must have columns: ID, LATITUDE, LONGITUDE, NAME, STATE

# Load DuckDB helper functions
source(knitr::purl("project.qmd", output = tempfile(), quiet = TRUE))

ui <- fluidPage(
  useShinyjs(),  # initialize shinyjs
  theme = bs_theme(bootswatch = "flatly"),
  tags$style(HTML("
    .datepicker {
      z-index: 9999 !important;
    }
  ")),
  titlePanel("Weather Records Explorer"),
  sidebarLayout(
    sidebarPanel(
      # --- New selector to switch between Station and State mode ---
      h4("1. Select Mode:"),
      radioButtons("selection_mode", NULL, choices = c("Station", "State"), selected = "Station", inline = TRUE),
      
      # Show both station map and state dropdown, control visibility dynamically
      conditionalPanel(
        condition = "input.selection_mode === 'Station'",
        leafletOutput("station_map", height = "300px")
      ),
      conditionalPanel(
        condition = "input.selection_mode === 'State'",
        selectInput("state_select", "Choose a state:", choices = sort(unique(filtered_stations$STATE)))
      ),
      
      # Only show this message in station mode
      conditionalPanel(
        condition = "input.selection_mode === 'Station'",
        verbatimTextOutput("selected_station_text")
      ),
      
      hr(),
      
      # --- Date range only (removed single-date selection) ---
      h4("2. Select Date Range:"),
      dateRangeInput(
        "range_dates", "Date Range",
        start = Sys.Date() - 30,
        end = as.Date("2025-04-20"),
        max = as.Date("2025-04-20")
      ),
      actionButton("go_range", "Get Range of Records")
    ),
    
    mainPanel(
      tabsetPanel(id = "view_tabs",
                  # --- Station View Tab ---
                  tabPanel(
                    title = "Station View",
                    value = "station_tab",
                    DT::dataTableOutput("range_records"),
                    uiOutput("download_station_ui"),
                    br(),
                    uiOutput("range_plot_ui"),
                    br(),
                    uiOutput("range_summary_ui"),
                    br(),
                    uiOutput("range_heatmap_ui")
                  ),
                  
                  # --- State View Tab ---
                  tabPanel(
                    title = "State View",
                    value = "state_tab",
                    DT::dataTableOutput("state_avg_table"),
                    uiOutput("download_state_ui"),
                    br(),
                    uiOutput("state_plot_ui"),
                    br(),
                    uiOutput("state_summary_ui"),
                    br(),
                    uiOutput("state_heatmap_ui")
                  )
      )
    )
  )
)

server <- function(input, output, session) {
  # --- Toggle visibility of station map and state dropdown ---
  observe({
    if (input$selection_mode == "Station") {
      show("station_map_container")
      hide("state_dropdown_container")
      updateTabsetPanel(session, "view_tabs", selected = "station_tab")
      shinyjs::enable(selector = "a[data-value='station_tab']")
      shinyjs::disable(selector = "a[data-value='state_tab']")
    } else {
      hide("station_map_container")
      show("state_dropdown_container")
      updateTabsetPanel(session, "view_tabs", selected = "state_tab")
      shinyjs::enable(selector = "a[data-value='state_tab']")
      shinyjs::disable(selector = "a[data-value='station_tab']")
    }
  })
  
  # --- Cache for state-level searches ---
  state_cache <- reactiveValues()
  
  # Render station map
  output$station_map <- renderLeaflet({
    leaflet(filtered_stations) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addCircleMarkers(
        layerId = ~ID,
        lng     = ~LONGITUDE,
        lat     = ~LATITUDE,
        label   = ~NAME,
        radius  = 6,
        fillOpacity = 0.7,
        stroke = FALSE
      )
  })
  
  # Track selected station via reactiveVal
  selected_station <- reactiveVal(NULL)
  observeEvent(input$station_map_marker_click, {
    clicked <- input$station_map_marker_click$id
    selected_station(clicked)
  })
  
  # Display selected station only (state is shown via dropdown)
  output$selected_station_text <- renderText({
    st <- selected_station()
    if (is.null(st)) {
      "No station selected"
    } else {
      paste0("Selected station: ", st)
    }
  })
  
  # --- Range records lookup for STATION ---
  range_rec <- eventReactive(input$go_range, {
    req(input$selection_mode == "Station", selected_station(), input$range_dates)
    df <- get_records_date_range(
      selected_station(),
      input$range_dates[1],
      input$range_dates[2]
    )
    if (!is.null(df)) {
      df$date <- as.Date(df$date, origin = "1970-01-01")
      df <- df %>% mutate(across(c(TMAX, TMIN, PRCP), ~ round(.x / 10, 2)))
    }
    df
  })
  
  output$range_records <- DT::renderDataTable({
    df <- range_rec()
    if (is.null(df)) {
      datatable(data.frame(Message = "No records found"), options = list(dom = 't'))
    } else {
      df <- df %>% rename(
        Date = date, 
        `Max Temperature (°C)` = TMAX,
        `Min Temperature (°C)` = TMIN,
        `Precipitation (mm)`   = PRCP
      )
      datatable(df, rownames = FALSE, filter = 'top', options = list(pageLength = 10))
    }
  })
  
  output$range_plot_ui <- renderUI({
    req(range_rec())
    tagList(
      h4("Time-Series Plot"),
      plotlyOutput("range_plot", height = "400px")
    )
  })
  
  output$range_plot <- renderPlotly({
    df <- range_rec()
    req(df)
    df_long <- df %>% pivot_longer(cols = c(TMAX, TMIN, PRCP), names_to = "Metric", values_to = "Value")
    p <- ggplot(df_long, aes(x = date, y = Value, color = Metric)) +
      geom_line() + labs(x = "Date", y = "Value", color = "Metric") + theme_minimal()
    ggplotly(p, tooltip = c("x","y","color")) %>%
      layout(xaxis = list(rangeslider = list(visible = TRUE)))
  })
  
  output$range_summary_ui <- renderUI({
    req(range_rec())
    tagList(
      h4("Summary Statistics"),
      tableOutput("range_summary")
    )
  })
  
  output$range_summary <- renderTable({
    df <- range_rec()
    req(df)
    tibble(
      Statistic = c("Mean","Min","Max","Median"),
      `Max Temperature (°C)` = c(mean(df$TMAX), min(df$TMAX), max(df$TMAX), median(df$TMAX)),
      `Min Temperature (°C)` = c(mean(df$TMIN), min(df$TMIN), max(df$TMIN), median(df$TMIN)),
      `Precipitation (mm)`   = c(mean(df$PRCP), min(df$PRCP), max(df$PRCP), median(df$PRCP))
    ) %>% mutate(across(-Statistic, ~ round(.x,2)))
  }, striped = TRUE, hover = TRUE, spacing = "s")
  
  output$range_heatmap_ui <- renderUI({
    req(range_rec())
    tagList(
      h4("Heatmap"),
      plotlyOutput("range_heatmap", height = "300px")
    )
  })
  
  output$range_heatmap <- renderPlotly({
    df <- range_rec()
    req(df)
    df_long <- df %>% pivot_longer(cols = c(TMAX,TMIN,PRCP), names_to = "Metric", values_to = "Value")
    plot_ly(data = df_long, x = ~date, y = ~Metric, z = ~Value, type = "heatmap") %>%
      layout(xaxis = list(title = "Date"), yaxis = list(title = "Metric"))
  })
  
  # --- Range records lookup for STATE (cached) ---
  state_range_data <- eventReactive(input$go_range, {
    req(input$selection_mode == "State", input$state_select, input$range_dates)
    
    key <- paste(input$state_select, input$range_dates[1], input$range_dates[2], sep = "_")
    if (!is.null(state_cache[[key]])) return(state_cache[[key]])
    
    state_station_ids <- filtered_stations %>%
      filter(STATE == input$state_select) %>%
      pull(ID)
    
    withProgress(message = "Fetching state data...", value = 0, {
      df <- purrr::map_dfr(
        seq_along(state_station_ids),
        function(i) {
          incProgress(1 / length(state_station_ids), detail = paste("Station", i))
          get_records_date_range(state_station_ids[[i]], input$range_dates[1], input$range_dates[2])
        }
      )
      if (!is.null(df)) df$date <- as.Date(df$date, origin = "1970-01-01")
      df$STATE <- input$state_select  # Add STATE column for identifier
      state_cache[[key]] <- df
      df
    })
  })
  
  # Daily state averages
  state_avg_by_day <- reactive({
    df <- state_range_data()
    req(nrow(df) > 0)
    df %>%
      group_by(date) %>%
      summarize(
        STATE = first(STATE),
        TMAX = mean(TMAX, na.rm = TRUE),
        TMIN = mean(TMIN, na.rm = TRUE),
        PRCP = mean(PRCP, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(across(c(TMAX, TMIN, PRCP), ~ round(.x / 10, 2)))
  })
  
  output$state_avg_table <- DT::renderDataTable({
    df <- state_avg_by_day() %>%
      rename(
        Date = date,
        State = STATE,
        `Max Temperature (°C)` = TMAX,
        `Min Temperature (°C)` = TMIN,
        `Precipitation (mm)`   = PRCP
      ) %>%
      select(State, Date, everything())
    datatable(df, rownames = FALSE, filter = 'top', options = list(pageLength = 10))
  })
  
  output$state_plot_ui <- renderUI({
    req(state_avg_by_day())
    tagList(
      h4("Time-Series Plot"),
      withSpinner(plotlyOutput("state_plot", height = "400px"))
    )
  })
  
  output$state_plot <- renderPlotly({
    df <- state_avg_by_day() %>%
      pivot_longer(cols = c(TMAX, TMIN, PRCP), names_to = "Metric", values_to = "Value")
    p <- ggplot(df, aes(x = date, y = Value, color = Metric)) +
      geom_line() + labs(x = "Date", y = "Value", color = "Metric") + theme_minimal()
    ggplotly(p, tooltip = c("x", "y", "color")) %>%
      layout(xaxis = list(rangeslider = list(visible = TRUE)))
  })
  
  output$state_summary_ui <- renderUI({
    req(state_avg_by_day())
    tagList(
      h4("Summary Statistics"),
      tableOutput("state_summary")
    )
  })
  
  output$state_summary <- renderTable({
    df <- state_avg_by_day()
    req(nrow(df) > 0)
    tibble(
      Statistic = c("Mean","Min","Max","Median"),
      `Max Temperature (°C)` = c(mean(df$TMAX), min(df$TMAX), max(df$TMAX), median(df$TMAX)),
      `Min Temperature (°C)` = c(mean(df$TMIN), min(df$TMIN), max(df$TMIN), median(df$TMIN)),
      `Precipitation (mm)`   = c(mean(df$PRCP), min(df$PRCP), max(df$PRCP), median(df$PRCP))
    ) %>% mutate(across(-Statistic, ~ round(.x,2)))
  }, striped = TRUE, hover = TRUE, spacing = "s")
  
  output$state_heatmap_ui <- renderUI({
    req(state_avg_by_day())
    tagList(
      h4("Heatmap"),
      withSpinner(plotlyOutput("state_heatmap", height = "300px"))
    )
  })
  
  output$state_heatmap <- renderPlotly({
    df <- state_avg_by_day() %>%
      pivot_longer(cols = c(TMAX, TMIN, PRCP), names_to = "Metric", values_to = "Value")
    plot_ly(df, x = ~date, y = ~Metric, z = ~Value, type = "heatmap") %>%
      layout(xaxis = list(title = "Date"), yaxis = list(title = "Metric"))
  })
  
  # Show button only if station results are available
  output$download_station_ui <- renderUI({
    req(range_rec())
    tags$div(
      style = "margin-bottom: 15px;",
      downloadButton("download_station", "Download CSV", class = "btn btn-sm btn-secondary")
    )
  })
  
  # Show button only if state results are available
  output$download_state_ui <- renderUI({
    req(state_avg_by_day())
    tags$div(
      style = "margin-bottom: 15px;",
      downloadButton("download_state", "Download CSV", class = "btn btn-sm btn-secondary")
    )
  })
  
  # Station CSV
  output$download_station <- downloadHandler(
    filename = function() {
      paste0("station_weather_", selected_station(), ".csv")
    },
    content = function(file) {
      df <- range_rec()
      req(df)
      df <- df %>% rename(
        Date = date,
        `Max Temperature (°C)` = TMAX,
        `Min Temperature (°C)` = TMIN,
        `Precipitation (mm)`   = PRCP
      )
      readr::write_csv(df, file)
    }
  )
  
  # State CSV
  output$download_state <- downloadHandler(
    filename = function() {
      paste0("state_weather_", input$state_select, ".csv")
    },
    content = function(file) {
      df <- state_avg_by_day()
      req(df)
      df <- df %>% rename(
        Date = date,
        State = STATE,
        `Max Temperature (°C)` = TMAX,
        `Min Temperature (°C)` = TMIN,
        `Precipitation (mm)`   = PRCP
      ) %>%
        select(State, Date, everything())
      readr::write_csv(df, file)
    }
  )
  
}

shinyApp(ui, server)