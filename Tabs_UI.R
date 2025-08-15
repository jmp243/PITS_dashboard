# last updated
# 2025-8-15
# Jung Mee Park
# jmp243

# Load required packages
library(shiny)
library(bslib)
library(thematic)
library(tidyverse)
library(plotly)
library(crosstalk)
library(DT)
library(RColorBrewer)
library(readr)
library(lubridate)
library(usethis) 
library(dplyr)
library(tidyr)
library(htmlwidgets)
library(stringr)
library(ggplot2)
library(kableExtra)
library(ggpubr)
library(bslib)
library(bs4Dash)
library(fresh)

# https://www.youtube.com/watch?v=mvB1FvAfaH0

# Apply theme to all visualizations
thematic_shiny()
ggplot2::theme_set(ggplot2::theme_minimal())

# load in data 
PITS_public <- read_csv("processed_data/PITS_public2.csv") %>% select(-1)
PITS_public <- PITS_public %>% 
  mutate(hour_label = fct_reorder(as.factor(hour_label), hour_label)) 

MyPC_public <- read_csv("processed_data/MyPC_public.csv") %>% select(-1) 
MyPC_public <- MyPC_public %>% 
  mutate(hour_label = fct_reorder(as.factor(hour_label), hour_label)) 

hourly_usage <- MyPC_public %>%
  group_by(Location_masked, hour_label, booking_date) %>%
  summarize(count = n(), .groups = "drop") %>% 
  arrange(as.numeric(substr(hour_label, 1, 2)))
# mutate(hour_label = fct_reorder(as.factor(hour_label), hour_label)) 
# 

levels(hourly_usage$hour_label)

#############################
# streamlined ratio process #
#############################
MyPC_counts <- MyPC_public %>%
  # MyPC_counts <- read_csv("data/processed_data/MyPC_public.csv") %>%
  select(-1) %>%  # Remove first column
  rename(date = booking_from) %>%
  group_by(Location_masked, date) %>%
  summarize(user_count = n(), .groups = "drop")

# Process PITS data
PITS_counts <- PITS_public %>%
  mutate(date = as.Date(Date2)) %>%
  group_by(Location_masked, date) %>%
  summarize(infraction_count = n(), .groups = "drop")

# Aggregate MyPC data by location and date
MyPC_counts <- MyPC_public %>%
  rename(date = booking_date) %>%
  group_by(Location_masked, date) %>%
  summarize(user_count = n(), .groups = "drop")

# Step 2: Join the datasets
ratio_data <- MyPC_counts %>%
  full_join(PITS_counts, by = c("Location_masked", "date")) %>%
  mutate(
    infraction_count = ifelse(is.na(infraction_count), 0, infraction_count),
    user_count = ifelse(is.na(user_count), 0, user_count)
  ) 

# App colours
theme <- create_theme(
  bs4dash_color(
    lime = "#52A1A5",
    olive = "#4A9094",
    purple = "#8965CD"
  ),
  bs4dash_status(
    primary = "#E1EDED",
    info = "#E4E4E4"
  )
)

# all locations
locations <- sort(unique(PITS_public$Location_masked))  # Sort locations alphabetically
all_locations <- c("All" = "All", locations)

ui <- dashboardPage(
  title = "PITS Dashboard",
  freshTheme = theme,
  
  header = dashboardHeader(
    status = "lime",
    title = dashboardBrand(
      title = "PITS Dashboard",
      color = "olive"
    )
  ),
  
  sidebar = dashboardSidebar(
    minified = FALSE,
    width = "350px",
    
    # Navigation menu
    sidebarMenu(
      id = "tabs",
      menuItem("Computer use", tabName = "computer", icon = icon("computer")),
      menuItem("Incidents", tabName = "incidents", icon = icon("exclamation-triangle")),
      menuItem("Suspensions", tabName = "suspensions", icon = icon("ban"))
    ),
    
    # Filters below menu
    dateRangeInput(
      inputId = "selected_date",
      label = "Selected Date Range",
      start = min(PITS_public$Date2),
      end = max(PITS_public$Date2),
      width = "100%"
    ),
    
    selectInput(
      inputId = "Location",
      label = "Location",
      choices = all_locations,
      multiple = TRUE,
      selected = "All", 
      width = "100%"
    )
  ),
  
  body = dashboardBody(
    tabItems(
      
      # TAB 1: Computer usage
      tabItem(
        tabName = "computer",
        fluidRow(
          box(
            title = "MyPC",
            width = 4,
            plotlyOutput("plot_MyPC")
          ),
          box(
            title = "Ratio Table",
            width = 8,
            p("Infractions per 1000 computer users."),
            DTOutput("summary_ratio_table")
          )
        )
      ),
      
      # TAB 2: Incidents
      tabItem(
        tabName = "incidents",
        fluidRow(
          box(
            title = "Incidents by Type, Location, and Time",
            width = 8,
            plotlyOutput("plot_PITS_by_type_location_time")
          ),
          box(
            title = "Types of Incidents",
            width = 4,
            DTOutput("table_data")
          )
        )
      ),
      
      # TAB 3: Suspensions
      tabItem(
        tabName = "suspensions",
        fluidRow(
          box(
            title = "Violin Plot of Suspension Lengths",
            width = 12,
            plotlyOutput("Violin_PITS_Susp_length")
          )
        )
      )
    )
  )
)


server <- function(input, output, session) {
  
  # ------------------------
  # 1. Reactive data filters
  # ------------------------
  data <- reactive({
    filtered_data <- PITS_public %>% 
      filter(between(Date2, input$selected_date[1], input$selected_date[2]))
    
    if (!"All" %in% input$Location) {
      filtered_data <- filtered_data %>% 
        filter(Location_masked %in% input$Location)
    }
    filtered_data
  })
  
  data2 <- reactive({
    filtered_data <- MyPC_public %>% 
      filter(between(booking_from, input$selected_date[1], input$selected_date[2]))
    
    if (!"All" %in% input$Location) {
      filtered_data <- filtered_data %>% 
        filter(Location_masked %in% input$Location)
    }
    filtered_data
  })
  
  summary_table_data <- reactive({
    filtered_data <- ratio_data %>%
      filter(between(date, input$selected_date[1], input$selected_date[2]))
    
    if (!"All" %in% input$Location) {
      filtered_data <- filtered_data %>% 
        filter(Location_masked %in% input$Location)
    }
    
    filtered_data %>%
      group_by(Location_masked) %>%
      summarize(
        Total_Users = sum(user_count, na.rm = TRUE),
        Total_Infractions = sum(infraction_count, na.rm = TRUE),
        Average_Daily_Users = mean(user_count, na.rm = TRUE),
        Average_Daily_Infractions = mean(infraction_count, na.rm = TRUE),
        Infractions_Per_1000_Users = ifelse(
          sum(user_count, na.rm = TRUE) > 0,
          (sum(infraction_count, na.rm = TRUE) / sum(user_count, na.rm = TRUE)) * 1000,
          0
        ),
        .groups = "drop"
      ) %>%
      mutate(
        Average_Daily_Users = round(Average_Daily_Users, 2),
        Average_Daily_Infractions = round(Average_Daily_Infractions, 2),
        Infractions_Per_1000_Users = round(Infractions_Per_1000_Users, 2)
      )
  })
  
  # ------------------------
  # 2. Outputs
  # ------------------------
  
  # Summary table
  output$summary_ratio_table <- renderDT({
    summary_table_data() %>% 
      rename("Location" = Location_masked) %>%
      datatable(
        options = list(
          pageLength = 8,
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf')
        ),
        extensions = 'Buttons',
        rownames = FALSE
      ) %>%
      formatStyle(
        "Infractions_Per_1000_Users",
        background = styleColorBar(
          c(0, max(summary_table_data()$Infractions_Per_1000_Users)), 'lightblue'
        ),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })
  
  # MyPC plot
  output$plot_MyPC <- renderPlotly({
    hourly_usage <- data2() %>%
      group_by(Location_masked, hour_label, booking_date) %>%
      summarize(count = n(), .groups = "drop") %>%
      mutate(hour_label = sprintf("%02d:00", as.numeric(substr(hour_label, 1, 2)))) %>%
      arrange(as.numeric(substr(hour_label, 1, 2)))
    
    n_locations <- length(unique(hourly_usage$Location_masked))
    my_palette <- RColorBrewer::brewer.pal(n = min(n_locations, 8), name = "Set3")
    if (n_locations > 8) my_palette <- colorRampPalette(my_palette)(n_locations)
    
    plot_ly(
      hourly_usage, 
      x = ~factor(hour_label, levels = sprintf("%02d:00", 0:23)),
      y = ~count, 
      color = ~Location_masked, 
      type = "bar",
      colors = my_palette,
      hovertemplate = ~paste(
        "Hour: ", hour_label,
        "<br>Location: ", Location_masked,
        "<br>Count: ", count,
        "<br>Date: ", format(booking_date, "%Y-%m-%d"),
        "<extra></extra>"
      )
    ) %>%
      layout(
        title = "Computer Usage by Hour of Day",
        xaxis = list(title = "Hour of Day", tickangle = 45),
        yaxis = list(title = "Number of Sessions"),
        barmode = "stack"
      )
  })
  
  # Violin plot
  output$Violin_PITS_Susp_length <- renderPlotly({
    suspended_raw <- data() %>%
      filter(susp_binary == 1) %>%
      distinct(`Incident ID`, Location_masked, Date2, Suspension_length, .keep_all = TRUE)
    
    if (nrow(suspended_raw) == 0) {
      return(plotly_empty(type = "violin") %>% 
               layout(title = "No suspended incidents in selection"))
    }
    
    plot_ly(
      data = suspended_raw,
      x = ~Location_masked,
      y = ~Suspension_length,
      type = "violin",
      box = list(visible = TRUE),
      meanline = list(visible = TRUE),
      points = "all",
      jitter = 0.2
    ) %>%
      layout(
        title = "Suspension Length Distribution by Location",
        xaxis = list(title = "Location"),
        yaxis = list(title = "Suspension Length (Days)")
      )
  })
  
  # Incidents plot
  output$plot_PITS_by_type_location_time <- renderPlotly({
    incident_counts <- data() %>%
      filter(!is.na(parsed_categories)) %>%
      distinct(`Incident ID`, parsed_categories, Location_masked, Date2, hour_label, .keep_all = TRUE) %>%
      group_by(Location_masked, Date2, hour_label, Incident_Type = parsed_categories) %>%
      summarise(Count = n(), .groups = 'drop') %>%
      arrange(as.numeric(substr(hour_label, 1, 2)))
    
    plot_ly(
      incident_counts, 
      x = ~hour_label, 
      y = ~Count, 
      color = ~Incident_Type, 
      type = "bar",
      hovertemplate = ~paste(
        "Hour: ", hour_label,
        "<br>Location: ", Location_masked,
        "<br>Incident Type: ", Incident_Type,
        "<br>Date: ", Date2,
        "<extra></extra>"
      )
    ) %>%
      layout(
        title = "Incidents by Location, Hour, and Type",
        xaxis = list(title = "Hour of Day", tickangle = 45),
        yaxis = list(title = "Number of Incidents"),
        barmode = "stack"
      )
  })
  
  # Incident type table
  output$table_data <- renderDT({
    table_data <- data() %>%
      group_by(parsed_categories) %>%
      summarise(Count = n(), .groups = "drop") %>%
      mutate(Percentage = paste0(round((Count / sum(Count)) * 100, 1), "%")) %>%
      rename(Categories = parsed_categories) %>%
      arrange(desc(Count))
    
    datatable(
      table_data,
      options = list(
        pageLength = 8,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf')
      ),
      extensions = 'Buttons',
      rownames = FALSE
    ) %>%
      formatStyle(
        'Count',
        background = styleColorBar(c(0, max(table_data$Count)), 'lightblue'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })
}

# Run the Shiny app
shinyApp(ui, server)
