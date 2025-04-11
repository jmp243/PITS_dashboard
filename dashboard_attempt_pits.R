# 2025-4-10
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
  group_by(Location_Standard, hour_label, booking_date) %>%
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
  group_by(Location_Standard, date) %>%
  summarize(user_count = n(), .groups = "drop")

# Process PITS data
PITS_counts <- PITS_public %>%
  mutate(date = as.Date(Date2)) %>%
  group_by(Location_Standard, date) %>%
  summarize(infraction_count = n(), .groups = "drop")

# Aggregate MyPC data by location and date
MyPC_counts <- MyPC_public %>%
  rename(date = booking_date) %>%
  group_by(Location_Standard, date) %>%
  summarize(user_count = n(), .groups = "drop")

# Step 2: Join the datasets
ratio_data <- MyPC_counts %>%
  full_join(PITS_counts, by = c("Location_Standard", "date")) %>%
  mutate(
    infraction_count = ifelse(is.na(infraction_count), 0, infraction_count),
    user_count = ifelse(is.na(user_count), 0, user_count)
  ) 
# # Step 3: Calculate the ratio
# ratio_data <- ratio_data %>%
#   # Avoid division by zero
#   mutate(
#     ratio = ifelse(user_count > 0, infraction_count / user_count, 0),
#     # Multiply by 100 to get percentage
#     ratio_percentage = ratio * 100
#   )
# 
# Prepare the data
# summary_table <- ratio_data %>%
#   group_by(Location_Standard, date) %>%
#   summarize(
#     Total_Users = sum(user_count, na.rm = TRUE),
#     Total_Infractions = sum(infraction_count, na.rm = TRUE),
#     Average_Daily_Users = mean(user_count, na.rm = TRUE),
#     Average_Daily_Infractions = mean(infraction_count, na.rm = TRUE),
#     
#     # Calculate ratio per 1000 users
#     Infractions_Per_1000_Users = ifelse(
#       Total_Users > 0, 
#       (Total_Infractions / Total_Users) * 1000, 
#       0
#     )
#   ) %>%
#   # Round numeric columns to 2 decimal places
#   mutate(
#     Total_Users = ifelse(Total_Users == 0, 0.1, Total_Users),
#     Average_Daily_Users = round(Average_Daily_Users, 2),
#     Average_Daily_Infractions = round(Average_Daily_Infractions, 2),
#     Infractions_Per_1000_Users = round(Infractions_Per_1000_Users, 2)
#   )

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
locations <- sort(unique(PITS_public$Location_Standard))  # Sort locations alphabetically
all_locations <- c("All" = "All", locations)

# Define dashboard UI
ui <- dashboardPage(
  title = "PITS Dashboard",
  freshTheme = theme,
  dark = NULL,
  help = NULL,
  
  # Header ----
  header = dashboardHeader(
    status = "lime",
    title = dashboardBrand(
      title = "Filters",
      color = "olive"
      # image = "https://images.unsplash.com/photo-1539664030485-a936c7d29c6e?q=80&w=1160&auto=format&fit=crop&ixlib=rb-4.0.3&ixid=M3wxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8fA%3D%3D"
    )
  ),
  
  # Sidebar ----
  sidebar = dashboardSidebar(
    minified = FALSE,
    width = "350px",
    
    # Filters
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
      # choices = c(unique(PITS_public$Location_Standard)),
      # multiple = TRUE,
      choices = all_locations,
      multiple = TRUE,
      selected = "All", 
      width = "100%"
    )
  ),
  body = dashboardBody(
    
    fluidRow(
      box(
        title = "MyPC",
        width = 4,
        plotlyOutput("plot_MyPC")
      ),
      box(
        title = "Ratio Table",
        width = 8,
        DTOutput("summary_ratio_table")
      )
    ),
    
    fluidRow(
      box(
        title = "Incidents by Type, Location, and Time",
        width = 8,
        plotlyOutput("plot_PITS_by_type_location_time")
      ),

      box(
        title = "Types of incidents",
        width = 4,
        DTOutput("table_data")
      )
    ),
    
  )
)



# Define dashboard server
server <- function(input, output, session) {
  # 
  # PITS data reactive
  data <- reactive({
    filtered_data <- PITS_public
    
    # Apply date filter if available
    if (!is.null(input$selected_date)) {
      filtered_data <- filtered_data %>% 
        filter(between(Date2, input$selected_date[1], input$selected_date[2]))
    }
    
    # Apply location filter
    if (!is.null(input$Location) && !"All" %in% input$Location) {
      filtered_data <- filtered_data %>% 
        filter(Location_Standard %in% input$Location)
    }
    
    return(filtered_data)
  })
  
  # MyPC data reactive
  data2 <- reactive({
    filtered_data <- MyPC_public
    # Apply date filter if available
    if (!is.null(input$selected_date)) {
      filtered_data <- filtered_data %>% 
        filter(between(booking_from, input$selected_date[1], input$selected_date[2]))
    }
    
    # Apply location filter
    if (!is.null(input$Location) && !"All" %in% input$Location) {
      filtered_data <- filtered_data %>% 
        filter(Location_Standard %in% input$Location)
    }
    
    return(filtered_data)
  })
  # Add new reactive for the summary table data
  summary_table_data <- reactive({
    # Prepare the summary table
    filtered_data <- ratio_data %>%
      group_by(Location_Standard) %>%
      summarize(
        Total_Users = sum(user_count, na.rm = TRUE),
        Total_Infractions = sum(infraction_count, na.rm = TRUE),
        Average_Daily_Users = mean(user_count, na.rm = TRUE),
        Average_Daily_Infractions = mean(infraction_count, na.rm = TRUE),
        
        # Calculate ratio per 1000 users
        Infractions_Per_1000_Users = ifelse(
          Total_Users > 0, 
          (Total_Infractions / Total_Users) * 1000, 
          0
        )
      ) %>%
      # Round numeric columns to 2 decimal places
      mutate(
        Total_Users = ifelse(Total_Users == 0, 0.1, Total_Users),
        Average_Daily_Users = round(Average_Daily_Users, 2),
        Average_Daily_Infractions = round(Average_Daily_Infractions, 2),
        Infractions_Per_1000_Users = round(Infractions_Per_1000_Users, 2)
      )
    # Apply date filter if available
    if (!is.null(input$date)) {
      filtered_data <- filtered_data %>% 
        filter(between(date, input$selected_date[1], input$selected_date[2]))
    }
    
    # Apply location filter
    if (!is.null(input$Location) && !"All" %in% input$Location) {
      filtered_data <- filtered_data %>% 
        filter(Location_Standard %in% input$Location)
    }
    return(filtered_data)
  })

  # Render the summary table output
  output$summary_ratio_table <- renderDT({
    datatable(summary_table_data(),
              options = list(
                pageLength = 8,
                scrollX = TRUE,
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel', 'pdf')
              ),
              extensions = 'Buttons',
              rownames = FALSE) %>%
      formatStyle('Infractions_Per_1000_Users',
                  background = styleColorBar(c(0, max(summary_table_data()$Infractions_Per_1000_Users)), 'lightblue'),
                  backgroundSize = '100% 90%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center')
  })
  
  # MyPC plot output
  output$plot_MyPC <- renderPlotly({
    library(RColorBrewer)
    
    # Process data from reactive
    hourly_usage <- data2() %>%
      group_by(Location_Standard, hour_label, booking_date) %>%
      summarize(count = n(), .groups = "drop")
    
    # Format hour_label consistently as "HH:00" (ensuring two digits)
    hourly_usage$hour_label <- sprintf("%02d:00", as.numeric(substr(hourly_usage$hour_label, 1, 2)))
    
    # Sort based on the hour number for data processing
    hourly_usage <- hourly_usage %>% 
      arrange(as.numeric(substr(hour_label, 1, 2)))
    
    # Get unique locations for color palette
    n_locations <- length(unique(hourly_usage$Location_Standard))
    
    # Choose a ColorBrewer palette
    my_palette <- brewer.pal(n = min(n_locations, 8), name = "Set3")
    
    # Interpolate if more than 8 locations
    if (n_locations > 8) {
      my_palette <- colorRampPalette(my_palette)(n_locations)
    }
    
    # Create the ordered hour categories
    hour_categories <- sprintf("%02d:00", 0:23)
    
    # Create plot
    plot_ly(hourly_usage, 
            x = ~factor(hour_label, levels = hour_categories), # Force proper ordering 
            y = ~count, 
            color = ~Location_Standard, 
            type = "bar",
            colors = my_palette,
            hovertemplate = ~paste(
              "Hour: ", hour_label,
              "<br>Location: ", Location_Standard,
              "<br>Count: ", count,
              "<br>Date: ", format(booking_date, "%Y-%m-%d"),
              "<extra></extra>"
            )) %>%
      layout(
        title = "Computer Usage by Hour of Day",
        xaxis = list(
          title = "Hour of Day", 
          tickangle = 45
        ),
        yaxis = list(title = "Number of Sessions"),
        barmode = "stack",
        hovermode = "compare",
        showlegend = FALSE # Changed to TRUE for better readability
      )
  })

  # PITS plot output
  output$plot_PITS_by_type_location_time <- renderPlotly({
    incident_counts <- data() %>%
      filter(!is.na(parsed_categories)) %>%
      distinct(`Incident ID`, parsed_categories, Location_Standard, Date2, 
               hour_label, .keep_all = TRUE) %>%
      group_by(Location_Standard, Date2, hour_label, Incident_Type = parsed_categories) %>%
      summarise(Count = n(), .groups = 'drop') %>%
      mutate(Date2 = as.Date(Date2)) %>%
      arrange(as.numeric(substr(hour_label, 1, 2)))
    
    
    plot_ly(incident_counts, 
            x = ~hour_label, 
            y = ~Count, 
            color = ~Incident_Type, 
            type = "bar",
            text = ~paste("Hour: ", hour_label,
                          "<br>Location: ", Location_Standard,
                          "<br>Incident Type: ", Incident_Type,
                          "<br>Date: ", Date2),
            hoverinfo = "text") %>% 
      layout(
        title = "Incidents by Location, Hour, and Type",
        xaxis = list(title = "Hour of Day", tickangle = 45),
        yaxis = list(title = "Number of Incidents"),
        barmode = "stack",
        hovermode = "compare"
      )
  })
  output$table_data <- renderDT({
    # Create the data frame first
    table_data <- data() %>%
      select(Location_Standard, Date2, parsed_categories) %>% 
      group_by(parsed_categories) %>%
      summarise(count = n(), .groups = "drop") %>%
      mutate(
        percentage = paste0(round((count / sum(count)) * 100, 1), "%")
      ) %>%
      rename(categories = parsed_categories) %>% 
      arrange(desc(count))
    
    # Then apply the formatting
    datatable(table_data,
              options = list(
                pageLength = 8,
                scrollX = TRUE,
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel', 'pdf')
              ),
              extensions = 'Buttons',
              rownames = FALSE) %>%
      formatStyle('count',
                  background = styleColorBar(c(0, max(table_data$count)), 'lightblue'),
                  backgroundSize = '100% 90%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center')
  })
  # # Data table output
  # output$table_data <- renderDT({
  #   data() %>%
  #     select(Location_Standard, Date2, parsed_categories) %>% 
  #     group_by(parsed_categories) %>%
  #     summarise(count = n(), .groups = "drop") %>%
  #     mutate(
  #       percentage = paste0(round((count / sum(count)) * 100, 1), "%")
  #     ) %>%
  #     rename(categories = parsed_categories) %>% 
  #     arrange(desc(count))
  #   
  #   
  # })
}

# Run the Shiny app
shinyApp(ui, server)
