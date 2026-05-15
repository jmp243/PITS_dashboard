# last updated 2025-01-07
# Jung Mee Park / jmp243

# Load required packages
library(shiny)
library(bslib)
library(tidyverse)
library(plotly)
library(crosstalk)
library(DT)
library(RColorBrewer)
library(readr)
library(lubridate)
library(dplyr)
library(tidyr)
library(htmlwidgets)
library(stringr)
library(ggplot2)
library(kableExtra)
library(ggpubr)
library(bs4Dash)

ggplot2::theme_set(ggplot2::theme_minimal())

# =============================================================================
# Load data
# =============================================================================

# PITS_public <- read_csv("processed_data/PITS_public2.csv") %>% select(-1)
# MyPC_public  <- read_csv("processed_data/MyPC_public.csv")  %>% select(-1)
PITS_public <- read_csv("processed_data/PITS_public_share.csv") %>% select(-1)
MyPC_public  <- read_csv("processed_data/MyPC_public_share.csv")  %>% select(-1)

# =============================================================================
# Clean PITS
# =============================================================================

PITS_public <- PITS_public %>%
  mutate(
    # Convert string "NA" to real NA
    `Suspension Status` = na_if(`Suspension Status`, "NA"),
    `Suspension Type`   = na_if(`Suspension Type`,   "NA"),
    Susp_Start_date     = na_if(as.character(Susp_Start_date), "NA"),
    Susp_End_date       = na_if(as.character(Susp_End_date),   "NA"),
    Susp_Start_date     = as.Date(Susp_Start_date),
    Susp_End_date       = as.Date(Susp_End_date),
    Date2               = as.Date(Date2),

    Suspension_length = case_when(
      is.na(Susp_End_date) | is.na(Susp_Start_date) ~ NA_real_,
      Susp_End_date < Susp_Start_date               ~ NA_real_,
      TRUE ~ as.numeric(Susp_End_date - Susp_Start_date)
    ),

    # Drive susp_binary off Suspension Type — avoids "NONE" miscount
    susp_binary = ifelse(`Suspension Type` == "Suspended customer", 1L, 0L),
    susp_binary = replace_na(susp_binary, 0L),

    hour_label = fct_reorder(as.factor(hour_label), hour_label)
  )

MyPC_public <- MyPC_public %>%
  mutate(
    booking_date = as.Date(booking_date),
    booking_from = as.Date(booking_from),
    hour_label   = fct_reorder(as.factor(hour_label), hour_label)
  )

# =============================================================================
# Pre-compute ratio data (computer users vs incidents)
# =============================================================================

PITS_incident_counts <- PITS_public %>%
  group_by(Location_masked, date = as.Date(Date2)) %>%
  summarise(incident_count = n_distinct(`Incident ID`), .groups = "drop")

MyPC_counts <- MyPC_public %>%
  group_by(Location_masked, date = booking_date) %>%
  summarise(user_count = n(), .groups = "drop")

ratio_data <- MyPC_counts %>%
  full_join(PITS_incident_counts, by = c("Location_masked", "date")) %>%
  mutate(
    user_count     = replace_na(user_count,     0),
    incident_count = replace_na(incident_count, 0)
  )

# =============================================================================
# Pre-compute suspension ratio data (suspensions per incident + avg length)
# =============================================================================

# One row per unique suspended incident — prevents duration double-counting
PITS_susp_deduped <- PITS_public %>%
  filter(susp_binary == 1,
         !is.na(`Incident ID`),
         !is.na(Suspension_length)) %>%
  distinct(`Incident ID`, Location_masked, Date2, Suspension_length)

# Suspensions per location per day
PITS_susp_counts <- PITS_susp_deduped %>%
  group_by(Location_masked, date = as.Date(Date2)) %>%
  summarise(
    suspension_count   = n_distinct(`Incident ID`),
    avg_susp_length    = mean(Suspension_length),
    median_susp_length = median(Suspension_length),
    .groups = "drop"
  )

susp_ratio_data <- PITS_incident_counts %>%
  left_join(PITS_susp_counts, by = c("Location_masked", "date")) %>%
  mutate(
    suspension_count   = replace_na(suspension_count,   0),
    avg_susp_length    = replace_na(avg_susp_length,    0),
    median_susp_length = replace_na(median_susp_length, 0),
    susp_ratio         = ifelse(incident_count > 0,
                                suspension_count / incident_count, 0)
  )

# =============================================================================
# App constants
# =============================================================================

locations     <- sort(unique(PITS_public$Location_masked))
all_locations <- c("All" = "All", locations)

# =============================================================================
# UI
# =============================================================================

ui <- dashboardPage(
  title = "PITS Dashboard",
  dark  = NULL,

  header = dashboardHeader(
    status = "grey",
    title  = dashboardBrand(title = "PITS Dashboard", color = "olive")
  ),

  sidebar = dashboardSidebar(
    minified = FALSE,
    width    = "350px",

    sidebarMenu(
      id = "tabs",
      menuItem("Computer use",  tabName = "computer",    icon = icon("computer")),
      menuItem("Incidents",     tabName = "incidents",   icon = icon("exclamation-triangle")),
      menuItem("Suspensions",   tabName = "suspensions", icon = icon("ban"))
    ),

    dateRangeInput(
      inputId = "selected_date",
      label   = "Selected Date Range",
      start   = min(PITS_public$Date2),
      end     = as.Date("2025-12-23"),
      min     = min(PITS_public$Date2),
      max     = as.Date("2025-12-23"),
      width   = "100%"
    ),

    selectInput(
      inputId  = "Location",
      label    = "Location",
      choices  = all_locations,
      multiple = TRUE,
      selected = "All",
      width    = "100%"
    )
  ),

  body = dashboardBody(
    tabItems(

      # -----------------------------------------------------------------------
      # Tab 1: Computer usage
      # -----------------------------------------------------------------------
      tabItem(
        tabName = "computer",
        fluidRow(
          box(title = "Computer Usage by Hour",
              width = 4,
              plotlyOutput("plot_MyPC")),
          box(title = "Infractions per 1,000 Computer Users",
              width = 8,
              p("Ratio of total incidents to computer sessions over the selected period."),
              DTOutput("summary_ratio_table"))
        ),
        fluidRow(
          box(title = "Computer Usage Over Time",
              width = 12,
              p("Total computer sessions per month over the selected period."),
              plotlyOutput("plot_computer_over_time", height = "350px"))
        )
      ),

      # -----------------------------------------------------------------------
      # Tab 2: Incidents
      # -----------------------------------------------------------------------
      tabItem(
        tabName = "incidents",
        fluidRow(
          box(title = "Incidents by Type, Location, and Hour",
              width = 8,
              plotlyOutput("plot_PITS_by_type_location_time")),
          box(title = "Types of Incidents",
              width = 4,
              DTOutput("table_data"))
        ),
        fluidRow(
          box(title = "Total Incidents Over Time",
              width = 12,
              p("Total number of incidents per month over the selected period."),
              plotlyOutput("plot_incidents_over_time", height = "350px"))
        )
      ),

      # -----------------------------------------------------------------------
      # Tab 3: Suspensions
      # -----------------------------------------------------------------------
      tabItem(
        tabName = "suspensions",
        fluidRow(
          # Summary value boxes
          valueBoxOutput("vbox_total_suspensions", width = 3),
          valueBoxOutput("vbox_susp_ratio",        width = 3),
          valueBoxOutput("vbox_avg_length",         width = 3),
          valueBoxOutput("vbox_median_length",      width = 3)
        ),
        fluidRow(
          box(title = "Suspension Length Distribution by Location",
              width = 7,
              plotlyOutput("Violin_PITS_Susp_length", height = "420px")),
          box(title = "Suspension Summary by Location",
              width = 5,
              p("Avg and median suspension length (days) per location."),
              DTOutput("susp_summary_table"))
        ),
        fluidRow(
          box(title = "Suspension Rate Over Time",
              width = 12,
              p("Proportion of incidents that resulted in a suspension, by month."),
              plotlyOutput("plot_susp_ratio_time", height = "350px"))
        )
      )
    )
  )
)

# =============================================================================
# Server
# =============================================================================

server <- function(input, output, session) {

  # ---------------------------------------------------------------------------
  # Reactive filtered frames
  # ---------------------------------------------------------------------------

  # PITS filtered by date + location
  data <- reactive({
    df <- PITS_public %>%
      filter(between(Date2, input$selected_date[1], input$selected_date[2]))
    if (!"All" %in% input$Location)
      df <- df %>% filter(Location_masked %in% input$Location)
    df
  })

  # MyPC filtered
  data2 <- reactive({
    df <- MyPC_public %>%
      filter(between(booking_date, input$selected_date[1], input$selected_date[2]))
    if (!"All" %in% input$Location)
      df <- df %>% filter(Location_masked %in% input$Location)
    df
  })

  # Deduplicated suspended incidents within filter
  susp_data <- reactive({
    df <- data() %>%
      filter(susp_binary == 1, !is.na(`Incident ID`), !is.na(Suspension_length)) %>%
      distinct(`Incident ID`, Location_masked, Date2, Suspension_length)
    df
  })

  # ratio_data filtered
  ratio_filtered <- reactive({
    df <- ratio_data %>%
      filter(between(date, input$selected_date[1], input$selected_date[2]))
    if (!"All" %in% input$Location)
      df <- df %>% filter(Location_masked %in% input$Location)
    df
  })

  # susp_ratio_data filtered
  susp_ratio_filtered <- reactive({
    df <- susp_ratio_data %>%
      filter(between(date, input$selected_date[1], input$selected_date[2]))
    if (!"All" %in% input$Location)
      df <- df %>% filter(Location_masked %in% input$Location)
    df
  })

  # ---------------------------------------------------------------------------
  # Tab 1: Computer use
  # ---------------------------------------------------------------------------

  output$summary_ratio_table <- renderDT({
    tbl <- ratio_filtered() %>%
      group_by(Location_masked) %>%
      summarise(
        Total_Users               = sum(user_count,     na.rm = TRUE),
        Total_Incidents           = sum(incident_count, na.rm = TRUE),
        Avg_Daily_Users           = round(mean(user_count,     na.rm = TRUE), 1),
        Avg_Daily_Incidents       = round(mean(incident_count, na.rm = TRUE), 2),
        Incidents_Per_1000_Users  = round(
          ifelse(sum(user_count) > 0,
                 sum(incident_count) / sum(user_count) * 1000, 0), 2),
        .groups = "drop"
      ) %>%
      rename(Location = Location_masked)

    datatable(tbl,
              extensions = "Buttons", rownames = FALSE,
              options = list(pageLength = 10, scrollX = TRUE,
                             dom = "Bfrtip",
                             buttons = c("copy", "csv", "excel"))) %>%
      formatStyle(
        "Incidents_Per_1000_Users",
        background = styleColorBar(c(0, max(tbl$Incidents_Per_1000_Users, 1)), "lightblue"),
        backgroundSize = "100% 90%", backgroundRepeat = "no-repeat",
        backgroundPosition = "center"
      )
  })

  output$plot_MyPC <- renderPlotly({
    hu <- data2() %>%
      group_by(Location_masked, hour_label, booking_date) %>%
      summarise(count = n(), .groups = "drop") %>%
      mutate(hour_label = sprintf("%02d:00", as.numeric(substr(hour_label, 1, 2)))) %>%
      arrange(as.numeric(substr(hour_label, 1, 2)))

    n_loc    <- length(unique(hu$Location_masked))
    pal      <- RColorBrewer::brewer.pal(min(n_loc, 8), "Set3")
    if (n_loc > 8) pal <- colorRampPalette(pal)(n_loc)

    plot_ly(hu,
            x = ~factor(hour_label, levels = sprintf("%02d:00", 0:23)),
            y = ~count, color = ~Location_masked,
            type = "bar", colors = pal,
            hovertemplate = ~paste("Hour:", hour_label,
                                   "<br>Location:", Location_masked,
                                   "<br>Count:", count,
                                   "<br>Date:", format(booking_date, "%Y-%m-%d"),
                                   "<extra></extra>")) %>%
      layout(barmode = "stack",
             title  = "Computer Usage by Hour of Day",
             xaxis  = list(title = "Hour of Day", tickangle = 45),
             yaxis  = list(title = "Number of Sessions"))
  })

  output$plot_computer_over_time <- renderPlotly({
    monthly_pc <- data2() %>%
      mutate(month = floor_date(booking_date, "month")) %>%
      group_by(month) %>%
      summarise(sessions = n(), .groups = "drop")

    if (nrow(monthly_pc) == 0)
      return(plotly_empty() %>% layout(title = "No data in selection"))

    plot_ly(monthly_pc, x = ~month, y = ~sessions,
            type = "scatter", mode = "lines+markers",
            line   = list(color = "#52A1A5", width = 2.5),
            marker = list(color = "#52A1A5", size = 7),
            hovertemplate = ~paste("Month:", format(month, "%b %Y"),
                                   "<br>Sessions:", sessions,
                                   "<extra></extra>")) %>%
      layout(xaxis = list(title = "Month"),
             yaxis = list(title = "Total Computer Sessions"))
  })

  # ---------------------------------------------------------------------------
  # Tab 2: Incidents
  # ---------------------------------------------------------------------------

  output$plot_PITS_by_type_location_time <- renderPlotly({
    ic <- data() %>%
      filter(!is.na(parsed_categories)) %>%
      distinct(`Incident ID`, parsed_categories, Location_masked, Date2, hour_label) %>%
      group_by(Location_masked, Date2, hour_label, Incident_Type = parsed_categories) %>%
      summarise(Count = n(), .groups = "drop") %>%
      arrange(as.numeric(substr(hour_label, 1, 2)))

    plot_ly(ic, x = ~hour_label, y = ~Count, color = ~Incident_Type,
            type = "bar",
            hovertemplate = ~paste("Hour:", hour_label,
                                   "<br>Location:", Location_masked,
                                   "<br>Type:", Incident_Type,
                                   "<br>Date:", Date2,
                                   "<extra></extra>")) %>%
      layout(barmode = "stack",
             title  = "Incidents by Location, Hour, and Type",
             xaxis  = list(title = "Hour of Day", tickangle = 45),
             yaxis  = list(title = "Number of Incidents"))
  })

  output$table_data <- renderDT({
    tbl <- data() %>%
      group_by(parsed_categories) %>%
      summarise(Count = n(), .groups = "drop") %>%
      mutate(Percentage = paste0(round(Count / sum(Count) * 100, 1), "%")) %>%
      rename(Categories = parsed_categories) %>%
      arrange(desc(Count))

    datatable(tbl, extensions = "Buttons", rownames = FALSE,
              options = list(pageLength = 8, scrollX = TRUE,
                             dom = "Bfrtip",
                             buttons = c("copy", "csv", "excel"))) %>%
      formatStyle("Count",
                  background = styleColorBar(c(0, max(tbl$Count)), "lightblue"),
                  backgroundSize = "100% 90%", backgroundRepeat = "no-repeat",
                  backgroundPosition = "center")
  })

  output$plot_incidents_over_time <- renderPlotly({
    monthly_inc <- data() %>%
      mutate(month = floor_date(as.Date(Date2), "month")) %>%
      group_by(month) %>%
      summarise(incidents = n_distinct(`Incident ID`), .groups = "drop")

    if (nrow(monthly_inc) == 0)
      return(plotly_empty() %>% layout(title = "No data in selection"))

    plot_ly(monthly_inc, x = ~month, y = ~incidents,
            type = "scatter", mode = "lines+markers",
            line   = list(color = "#8965CD", width = 2.5),
            marker = list(color = "#8965CD", size = 7),
            hovertemplate = ~paste("Month:", format(month, "%b %Y"),
                                   "<br>Incidents:", incidents,
                                   "<extra></extra>")) %>%
      layout(xaxis = list(title = "Month"),
             yaxis = list(title = "Total Incidents"))
  })

  # ---------------------------------------------------------------------------
  # Tab 3: Suspensions — value boxes
  # ---------------------------------------------------------------------------

  output$vbox_total_suspensions <- renderValueBox({
    valueBox(
      value    = nrow(susp_data()),
      subtitle = "Total Suspended Incidents",
      icon     = icon("ban"),
      color    = "purple"
    )
  })

  output$vbox_susp_ratio <- renderValueBox({
    total_inc  <- n_distinct(data()$`Incident ID`)
    total_susp <- nrow(susp_data())
    ratio_pct  <- ifelse(total_inc > 0,
                         round(total_susp / total_inc * 100, 1), 0)
    valueBox(
      value    = paste0(ratio_pct, "%"),
      subtitle = "Suspensions / Total Incidents",
      icon     = icon("percent"),
      color    = "olive"
    )
  })

  output$vbox_avg_length <- renderValueBox({
    avg <- ifelse(nrow(susp_data()) > 0,
                  round(mean(susp_data()$Suspension_length, na.rm = TRUE), 1), 0)
    valueBox(
      value    = paste0(avg, " days"),
      subtitle = "Mean Suspension Length",
      icon     = icon("calendar"),
      color    = "lime"
    )
  })

  output$vbox_median_length <- renderValueBox({
    med <- ifelse(nrow(susp_data()) > 0,
                  median(susp_data()$Suspension_length, na.rm = TRUE), 0)
    valueBox(
      value    = paste0(med, " days"),
      subtitle = "Median Suspension Length",
      icon     = icon("calendar-check"),
      color    = "info"
    )
  })

  # ---------------------------------------------------------------------------
  # Tab 3: Suspensions — violin
  # ---------------------------------------------------------------------------

  output$Violin_PITS_Susp_length <- renderPlotly({
    sd <- susp_data()
    if (nrow(sd) == 0)
      return(plotly_empty(type = "violin") %>%
               layout(title = "No suspended incidents in selection"))

    plot_ly(data = sd, x = ~Location_masked, y = ~Suspension_length,
            type = "violin",
            box      = list(visible = TRUE),
            meanline = list(visible = TRUE),
            points   = "all", jitter = 0.2) %>%
      layout(title  = "Suspension Length Distribution by Location",
             xaxis  = list(title = "Location"),
             yaxis  = list(title = "Suspension Length (Days)"))
  })

  # ---------------------------------------------------------------------------
  # Tab 3: Suspensions — summary table
  # ---------------------------------------------------------------------------

  output$susp_summary_table <- renderDT({
    tbl <- susp_ratio_filtered() %>%
      group_by(Location_masked) %>%
      summarise(
        Total_Incidents   = sum(incident_count,    na.rm = TRUE),
        Total_Suspensions = sum(suspension_count,  na.rm = TRUE),
        Susp_Rate_Pct     = round(
          ifelse(sum(incident_count) > 0,
                 sum(suspension_count) / sum(incident_count) * 100, 0), 1),
        Avg_Susp_Days     = round(
          ifelse(sum(suspension_count) > 0,
                 weighted.mean(avg_susp_length, w = pmax(suspension_count, 1), na.rm = TRUE),
                 0), 1),
        Median_Susp_Days  = round(
          median(rep(median_susp_length, times = pmax(suspension_count, 1)), na.rm = TRUE), 1),
        .groups = "drop"
      ) %>%
      rename(Location = Location_masked)

    datatable(tbl, extensions = "Buttons", rownames = FALSE,
              options = list(pageLength = 12, scrollX = TRUE,
                             dom = "Bfrtip",
                             buttons = c("copy", "csv", "excel"))) %>%
      formatStyle("Susp_Rate_Pct",
                  background = styleColorBar(c(0, max(tbl$Susp_Rate_Pct, 1)), "lightsalmon"),
                  backgroundSize = "100% 90%", backgroundRepeat = "no-repeat",
                  backgroundPosition = "center")
  })

  # ---------------------------------------------------------------------------
  # Tab 3: Suspensions — ratio over time
  # ---------------------------------------------------------------------------

  output$plot_susp_ratio_time <- renderPlotly({
    monthly <- susp_ratio_filtered() %>%
      mutate(month = floor_date(date, "month")) %>%
      group_by(month) %>%
      summarise(
        incidents    = sum(incident_count,   na.rm = TRUE),
        suspensions  = sum(suspension_count, na.rm = TRUE),
        susp_rate    = ifelse(incidents > 0, suspensions / incidents * 100, 0),
        .groups = "drop"
      )

    if (nrow(monthly) == 0)
      return(plotly_empty() %>% layout(title = "No data in selection"))

    plot_ly(monthly, x = ~month, y = ~susp_rate,
            type = "scatter", mode = "lines+markers",
            line   = list(color = "#52A1A5", width = 2.5),
            marker = list(color = "#52A1A5", size = 7),
            hovertemplate = ~paste("Month:", format(month, "%b %Y"),
                                   "<br>Suspensions:", suspensions,
                                   "<br>Incidents:", incidents,
                                   "<br>Rate:", round(susp_rate, 1), "%",
                                   "<extra></extra>")) %>%
      layout(xaxis = list(title = "Month"),
             yaxis = list(title = "Suspension Rate (%)"))
  })

}

# Run app
shinyApp(ui, server)
