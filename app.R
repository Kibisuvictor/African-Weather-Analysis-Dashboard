# --- 0. Load Libraries ---
library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(plotly)
library(janitor) # For clean_names()
library(Rtsne)
library(uwot) 
library(igraph)
library(gganimate)
library(ggrepel)
library(viridis)
library(DT)
library(shinycssloaders) # For loading animations

# --- 1. Data Loading and Initial Global Preprocessing (runs once at app start) ---
GlobalWeatherRepository_df_raw <- tryCatch({
  read_csv("GlobalWeatherRepository.csv") %>%
    clean_names() # Clean column names early
}, error = function(e) {
  showNotification(paste("Critical Error: Could not read GlobalWeatherRepository.csv:", e$message), type = "error", duration = NULL)
  # Return a minimal empty dataframe with expected cleaned names to prevent further startup errors
  data.frame(country=character(), last_updated=character(), temperature_celsius=numeric(), 
             pressure_mb=numeric(), humidity=numeric(), precip_mm=numeric(), cloud=numeric(), 
             wind_degree=numeric(), wind_mph=numeric(), wind_direction=character(), 
             location_name=character(), air_quality_us_epa_index=numeric(), 
             stringsAsFactors = FALSE)
})

# Recode country names
GlobalWeatherRepository_df_processed <- GlobalWeatherRepository_df_raw %>%
  mutate(
    country = recode(country,
                     "bélgica" = "Belgium", "mexique" = "Mexico", "malásia" = "Malaysia", # Assuming clean_names makes them lowercase
                     "polônia" = "Poland", "marrocos" = "Morocco", "letonia" = "Latvia",
                     "estonie" = "Estonia", "турция" = "Turkey", "كولومبيا" = "Colombia",
                     "гватемала" = "Guatemala", "火鸡" = "Turkey", "saudi_arabien" = "Saudi Arabia", # Adjusted for clean_names
                     "südkorea" = "South Korea", "turkménistan" = "Turkmenistan", "jemen" = "Yemen",
                     "inde" = "India", "komoren" = "Comoros", "польша" = "Poland",
                     "saint_vincent_et_les_grenadines" = "Saint Vincent and the Grenadines", # Adjusted for clean_names
                     # Add other recodes as needed, assuming lowercase and underscore from clean_names
                     .default = country # Keep original if no match
    ),
    # Add continent (ensuring country names used here match post-recode and clean_names)
    continent = case_when(
      country %in% c("Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cameroon", "Cape Verde", "Central African Republic", "Chad", "Comoros", "Congo", "Democratic Republic of Congo", "Djibouti", "Egypt", "Equatorial Guinea", "Eritrea", "Ethiopia", "Gabon", "Gambia", "Ghana", "Guinea", "Guinea-Bissau", "Ivory Coast", "Kenya", "Lesotho", "Liberia", "Libya", "Madagascar", "Malawi", "Mali", "Mauritania", "Mauritius", "Morocco", "Mozambique", "Namibia", "Niger", "Nigeria", "Rwanda", "Senegal", "Seychelles Islands", "Sierra Leone", "Somalia", "South Africa", "South Sudan", "Sudan", "Swaziland", "Tanzania", "Togo", "Tunisia", "Uganda", "Zambia", "Zimbabwe") ~ "Africa",
      country %in% c("Afghanistan", "Armenia", "Azerbaijan", "Bahrain", "Bangladesh", "Bhutan", "Brunei Darussalam", "Cambodia", "China", "Cyprus", "Georgia", "India", "Indonesia", "Iran", "Iraq", "Israel", "Japan", "Jordan", "Kazakhstan", "North Korea", "South Korea", "Kuwait", "Kyrghyzstan", "Lao People's Democratic Republic", "Lebanon", "Malaysia", "Maldives", "Mongolia", "Myanmar", "Nepal", "Oman", "Pakistan", "Palau", "Philippines", "Qatar", "Saudi Arabia", "Singapore", "Sri Lanka", "Syria", "Tajikistan", "Thailand", "Timor-Leste", "Turkmenistan", "United Arab Emirates", "Uzbekistan", "Vietnam", "Yemen") ~ "Asia",
      country %in% c("Albania", "Andorra", "Austria", "Belarus", "Belgium", "Bosnia and Herzegovina", "Bulgaria", "Croatia", "Czech Republic", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Italy", "Kosovo", "Latvia", "Lithuania", "Luxembourg", "Macedonia", "Malta", "Monaco", "Montenegro", "Netherlands", "Norway", "Poland", "Portugal", "Romania", "Russia", "Serbia", "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland", "Turkey", "Ukraine", "United Kingdom", "Vatican City", "Liechtenstein", "San Marino") ~ "Europe",
      country %in% c("Antigua and Barbuda", "Bahamas", "Barbados", "Belize", "Canada", "Costa Rica", "Cuba", "Dominica", "Dominican Republic", "El Salvador", "Grenada", "Guatemala", "Haiti", "Honduras", "Jamaica", "Mexico", "Nicaragua", "Panama", "Saint Kitts and Nevis", "Saint Lucia", "Saint Vincent and the Grenadines", "Trinidad and Tobago", "United States of America", "USA United States of America") ~ "North America",
      country %in% c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Ecuador", "Guyana", "Paraguay", "Peru", "Suriname", "Uruguay", "Venezuela") ~ "South America",
      country %in% c("Australia", "Fiji Islands", "Kiribati", "Marshall Islands", "Micronesia", "New Zealand", "Palau", "Papua New Guinea", "Samoa", "Solomon Islands", "Tonga", "Tuvalu", "Vanuatu") ~ "Oceania",
      TRUE ~ "Other"
    )
  )

# Parse dates - crucial step
if ("last_updated" %in% names(GlobalWeatherRepository_df_processed) && is.character(GlobalWeatherRepository_df_processed$last_updated)) {
  GlobalWeatherRepository_df_processed$last_updated <- parse_datetime(GlobalWeatherRepository_df_processed$last_updated) 
}
GlobalWeatherRepository_df_processed <- GlobalWeatherRepository_df_processed %>%
  filter(!is.na(last_updated))


# African subregion mapping (ensure country names here match those in GlobalWeatherRepository_df_processed)
african_subregion_map <- c(
  "Algeria" = "Northern Africa", "Angola" = "Middle Africa", "Benin" = "Western Africa",
  "Botswana" = "Southern Africa", "Burkina Faso" = "Western Africa", "Burundi" = "Eastern Africa",
  "Cameroon" = "Middle Africa", "Cape Verde" = "Western Africa", "Central African Republic" = "Middle Africa",
  "Chad" = "Middle Africa", "Comoros" = "Eastern Africa", "Congo" = "Middle Africa",
  "Democratic Republic of Congo" = "Middle Africa", "Djibouti" = "Eastern Africa", "Egypt" = "Northern Africa",
  "Equatorial Guinea" = "Middle Africa", "Eritrea" = "Eastern Africa", "Ethiopia" = "Eastern Africa",
  "Gabon" = "Middle Africa", "Gambia" = "Western Africa", "Ghana" = "Western Africa",
  "Guinea" = "Western Africa", "Guinea-Bissau" = "Western Africa", "Kenya" = "Eastern Africa",
  "Lesotho" = "Southern Africa", "Liberia" = "Western Africa", "Libya" = "Northern Africa",
  "Madagascar" = "Eastern Africa", "Malawi" = "Eastern Africa", "Mali" = "Western Africa",
  "Mauritania" = "Western Africa", "Mauritius" = "Eastern Africa", "Morocco" = "Northern Africa",
  "Mozambique" = "Eastern Africa", "Namibia" = "Southern Africa", "Niger" = "Western Africa",
  "Nigeria" = "Western Africa", "Rwanda" = "Eastern Africa", "Senegal" = "Western Africa",
  "Seychelles Islands" = "Eastern Africa", "Sierra Leone" = "Western Africa", "Somalia" = "Eastern Africa",
  "South Africa" = "Southern Africa", "Sudan" = "Northern Africa", "Swaziland" = "Southern Africa",
  "Tanzania" = "Eastern Africa", "Togo" = "Western Africa", "Tunisia" = "Northern Africa",
  "Uganda" = "Eastern Africa", "Zambia" = "Southern Africa", "Zimbabwe" = "Southern Africa"
)
GlobalWeatherRepository_df_processed <- GlobalWeatherRepository_df_processed %>%
  mutate(subregion = ifelse(continent == "Africa", african_subregion_map[country], NA_character_))

# Define available choices for inputs
available_continents <- sort(unique(GlobalWeatherRepository_df_processed$continent))
min_year_data <- if(nrow(GlobalWeatherRepository_df_processed) > 0) min(year(GlobalWeatherRepository_df_processed$last_updated), na.rm = TRUE) else 2020
max_year_data <- if(nrow(GlobalWeatherRepository_df_processed) > 0) max(year(GlobalWeatherRepository_df_processed$last_updated), na.rm = TRUE) else 2025 # Default to current year if no data

date_col_present <- "last_updated" %in% names(GlobalWeatherRepository_df_processed) && inherits(GlobalWeatherRepository_df_processed$last_updated, "POSIXt")
min_date_data <- if(date_col_present && nrow(GlobalWeatherRepository_df_processed) > 0) min(as.Date(GlobalWeatherRepository_df_processed$last_updated), na.rm=TRUE) else Sys.Date() - 365
max_date_data <- if(date_col_present && nrow(GlobalWeatherRepository_df_processed) > 0) max(as.Date(GlobalWeatherRepository_df_processed$last_updated), na.rm=TRUE) else Sys.Date()

# --- 2. UI Definition ---
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "African Weather\n Dashboard with a focus on Africa"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview_tab", icon = icon("fas fa-tachometer-alt")),
      menuItem("High-Dim Analysis", tabName = "dim_reduction_tab", icon = icon("fas fa-sitemap")),
      menuItem("Weather Similarity Network", tabName = "network_tab", icon = icon("fas fa-project-diagram")),
      menuItem("Temporal Trends", tabName = "temporal_tab", icon = icon("fas fa-chart-line")),
      menuItem("EA Network Case Study", tabName = "ea_network_tab", icon = icon("fas fa-microscope")),
      menuItem("Data Explorer", tabName = "data_explorer_tab", icon = icon("fas fa-table"))
    ),
    hr(),
    h5("Global Filters", style = "padding-left: 15px;"),
    selectInput("selected_continent_sidebar", "Continent:", choices = available_continents, selected = "Africa"),
    numericInput("selected_year_sidebar", "Year:", value = max_year_data, min = min_year_data, max = max_year_data, step = 1),
    uiOutput("african_subregion_selector_sidebar_ui"), # filter for country
    uiOutput("country_selector_sidebar_ui") # filter for country
  ),
  dashboardBody(
    tags$head(tags$style(HTML(".content-wrapper { overflow: auto; }"))), # Ensure scrollability if content overflows
    tabItems(
      # Overview Tab
      tabItem(tabName = "overview_tab",
              fluidRow(box(width = 12, title = "About this Dashboard", status = "primary", solidHeader = TRUE, collapsible = TRUE,
                           p("This dashboard provides an interactive platform to analyze global weather patterns, with a particular focus on data from the African continent. Navigate through the tabs to explore different aspects of the data, including high-dimensional views, network analyses of weather similarity, temporal trends, and a detailed data explorer."))),
              fluidRow(
                column(width=12, box(width = NULL, title = "Summary Weather Metrics Heatmap (by Subregion, if applicable)", status = "info", solidHeader = TRUE, collapsible = TRUE,
                                     withSpinner(plotOutput("overview_heatmap_plot"), type=6)))
              ),
              fluidRow(
                box(width = 6, title = "Observations by Country", status = "info", solidHeader = TRUE, collapsible = TRUE,
                    withSpinner(plotlyOutput("overview_country_bar_plot"), type=6)),
                box(width = 6, title = "Observations by Subregion", status = "info", solidHeader = TRUE, collapsible = TRUE,
                    withSpinner(plotlyOutput("overview_subregion_bar_plot"), type=6))
              )
      ),
      # High-Dimensional Analysis Tab
      tabItem(tabName = "dim_reduction_tab",
              fluidRow(
                box(width = 12, title = "Dimensionality Reduction Controls", status = "primary", solidHeader = TRUE, collapsible = TRUE,
                    selectInput("dimred_method_input", "Method:", choices = c("t-SNE", "UMAP"), selected = "UMAP"),
                    # Use cleaned column name
                    selectInput("dimred_color_var_input", "Color Points By:", choices = c("air_quality_us_epa_index", "subregion", "country"), selected = "air_quality_us_epa_index"),
                    conditionalPanel(
                      condition = "input.dimred_method_input == 't-SNE'",
                      sliderInput("dimred_perplexity_input", "t-SNE Perplexity:", min = 2, max = 50, value = 30, step = 1) # Ensure min perplexity is valid
                    ),
                    actionButton("dimred_run_button", "Run Analysis", icon = icon("play"), class = "btn-success"))
              ),
              fluidRow(
                box(width = 12, title = "Dimensionality Reduction Visualization", status = "info", solidHeader = TRUE,
                    withSpinner(plotlyOutput("dimred_plot_output", height = "600px"), type=6))
              )
      ),
      # Weather Similarity Network Tab
      tabItem(tabName = "network_tab",
              fluidRow(
                box(width = 12, title = "Network Controls", status = "primary", solidHeader = TRUE, collapsible = TRUE,
                    sliderInput("network_similarity_thresh_input", "Similarity Threshold (1 / (1+dist)):", min = 0.05, max = 0.95, value = 0.5, step = 0.05),
                    selectInput("network_layout_method_input", "Layout Method (igraph):", choices = c("Fruchterman-Reingold"="layout_with_fr", "Kamada-Kawai"="layout_with_kk", "Circle"="layout_in_circle", "Large Graph"="layout_with_lgl"), selected = "layout_with_fr"),
                    checkboxInput("network_show_communities_input", "Show Communities (Louvain)", value = TRUE))
              ),
              fluidRow(
                box(width = 12, title = "Weather Similarity Network", status = "info", solidHeader = TRUE,
                    withSpinner(plotOutput("network_plot_output", height = "700px"), type=6))
              )
      ),
      # Temporal Trends Tab
      tabItem(tabName = "temporal_tab",
              fluidRow(
                box(width = 12, title = "Animation Controls", status = "primary", solidHeader = TRUE, collapsible = TRUE,
                    fluidRow(
                      column(4, selectInput("temporal_x_var_input", "X-axis Variable:", choices = c("temperature_celsius", "humidity", "pressure_mb", "wind_mph"), selected = "temperature_celsius")),
                      column(4, selectInput("temporal_y_var_input", "Y-axis Variable:", choices = c("humidity", "temperature_celsius", "pressure_mb", "wind_mph"), selected = "humidity")),
                      column(4, selectInput("temporal_size_var_input", "Bubble Size Variable:", choices = c("wind_degree", "precip_mm", "cloud", "wind_mph"), selected = "wind_degree"))
                    ),
                    sliderInput("temporal_animation_speed_input", "Animation Speed (FPS):", min = 1, max = 10, value = 3, step = 1),
                    actionButton("temporal_run_animation_button", "Generate Animation", icon = icon("film"), class="btn-success"))
              ),
              fluidRow(
                box(width = 12, title = "Temporal Trends Animation", status = "info", solidHeader = TRUE,
                    uiOutput("temporal_animation_output_ui")) # Using uiOutput for conditional display or spinner
              )
      ),
      # EA Network Case Study Tab
      tabItem(tabName = "ea_network_tab",
              h2("Specific Case: Eastern African Capitals Network - April 1st, 2025"),
              p("This analysis focuses on weather similarity for Eastern African capitals on a specific date (April 1st, 2025). Node size in graphs is proportional to degree centrality."),
              fluidRow(
                box(title = "Network Graph (Modularity Groups)", width = 6, status = "danger", solidHeader = TRUE,
                    withSpinner(plotOutput("ea_network_mod_plot"), type=6)),
                box(title = "Network Graph (Edge Betweenness Groups)", width = 6, status = "danger", solidHeader = TRUE,
                    withSpinner(plotOutput("ea_network_btw_plot"), type=6))
              ),
              fluidRow(
                box(title = "Centrality Measures for EA Network (April 1st, 2025)", width = 12, status = "danger", solidHeader = TRUE,
                    withSpinner(DTOutput("ea_centrality_table_output"), type=6))
              )
      ),
      # Data Explorer Tab
      tabItem(tabName = "data_explorer_tab",
              fluidRow(
                box(width = 12, title = "Data Filters (Applied to table below, based on Global Filters)", status = "primary", solidHeader = TRUE, collapsible = TRUE,
                    fluidRow(
                      column(4, uiOutput("explorer_subregion_filter_ui")), # Dynamic based on continent
                      column(4, selectInput("explorer_country_filter_input", "Country:", choices = c("All"="All"), selected = "All")),
                      column(4, dateRangeInput("explorer_date_range_input", "Date Range:", start = min_date_data, end = max_date_data, min = min_date_data, max = max_date_data))
                    ))
              ),
              fluidRow(
                box(width = 12, title = "Weather Data Explorer", status = "info", solidHeader = TRUE,
                    withSpinner(DTOutput("explorer_data_table_output"), type=6))
              )
      )
    )
  )
)

# --- 3. Server Logic ---
server <- function(input, output, session) {
  
  # --- A. Global Reactive Data & Dynamic UI ---
  output$african_subregion_selector_sidebar_ui <- renderUI({
    req(input$selected_continent_sidebar)
    if (input$selected_continent_sidebar == "Africa") {
      sub_choices <- c("All", sort(unique(na.omit(GlobalWeatherRepository_df_processed$subregion[GlobalWeatherRepository_df_processed$continent == "Africa"]))))
      selectInput("selected_african_subregion_sidebar_input", "African Subregion:", choices = sub_choices, selected = "All")
    } else {
      NULL
    }
  })
  
  
  # NEW: Dynamic UI for Global Country Selector in Sidebar
  output$country_selector_sidebar_ui <- renderUI({
    req(input$selected_continent_sidebar, GlobalWeatherRepository_df_processed)
    
    df_for_country_choices <- GlobalWeatherRepository_df_processed
    
    # Filter by selected continent 
    df_for_country_choices <- df_for_country_choices %>%
      filter(continent == input$selected_continent_sidebar)
    
    # If Africa is selected and a specific subregion is chosen, filter further
    if (input$selected_continent_sidebar == "Africa" &&
        !is.null(input$selected_african_subregion_sidebar_input) &&
        input$selected_african_subregion_sidebar_input != "All") {
      # req(input$selected_african_subregion_sidebar_input) # This req might be too strict if subregion UI hasn't rendered yet or is NULL
      df_for_country_choices <- df_for_country_choices %>%
        filter(subregion == input$selected_african_subregion_sidebar_input)
    }
    
    country_choices <- c("All", sort(unique(na.omit(df_for_country_choices$country))))
    
    # Only show the selectInput if there are countries to choose from
    if (length(country_choices) > 1) {
      selectInput("selected_country_sidebar_input", "Country:", choices = country_choices, selected = "All")
    } else {
      # Potentially return a message or NULL if no specific countries are available under current filters
  
      selectInput("selected_country_sidebar_input", "Country:", choices = country_choices, selected = "All")
    }
  })
  
  data_selected_by_global_filters <- reactive({
    req(GlobalWeatherRepository_df_processed, input$selected_continent_sidebar, input$selected_year_sidebar)
    
    df <- GlobalWeatherRepository_df_processed %>%
      filter(continent == input$selected_continent_sidebar, 
             year(last_updated) == input$selected_year_sidebar)
    
    # Filter by African Subregion (if Africa is selected and subregion is chosen)
    if (input$selected_continent_sidebar == "Africa" && 
        !is.null(input$selected_african_subregion_sidebar_input) && 
        input$selected_african_subregion_sidebar_input != "All") {
      # req(input$selected_african_subregion_sidebar_input) # This req might cause issues if the UI element isn't fully ready.
      df <- df %>% filter(subregion == input$selected_african_subregion_sidebar_input)
    }
    
    # Filter by selected Country from the sidebar (NEW)
    if (!is.null(input$selected_country_sidebar_input) && 
        input$selected_country_sidebar_input != "All") {
      # req(input$selected_country_sidebar_input) 
      df <- df %>% filter(country == input$selected_country_sidebar_input)
    }
    
    validate(
      need(nrow(df) > 0, 
           paste("No data available for the selected global filters:",
                 input$selected_continent_sidebar, 
                 "in", input$selected_year_sidebar,
                 if(!is.null(input$selected_african_subregion_sidebar_input) && input$selected_african_subregion_sidebar_input != "All" && input$selected_continent_sidebar == "Africa") paste(" (Subregion:", input$selected_african_subregion_sidebar_input, ")") else "",
                 if(!is.null(input$selected_country_sidebar_input) && input$selected_country_sidebar_input != "All") paste(" (Country:", input$selected_country_sidebar_input, ")") else "",
                 ". Please adjust global filters."
           )
      )
    )
    return(df)
  })
  
  # data_selected_by_global_filters <- reactive({
  #   req(GlobalWeatherRepository_df_processed, input$selected_continent_sidebar, input$selected_year_sidebar)
  #   
  #   df <- GlobalWeatherRepository_df_processed %>%
  #     filter(continent == input$selected_continent_sidebar, 
  #            year(last_updated) == input$selected_year_sidebar)
  #   
  #   if (input$selected_continent_sidebar == "Africa" && !is.null(input$selected_african_subregion_sidebar_input) && input$selected_african_subregion_sidebar_input != "All") {
  #     req(input$selected_african_subregion_sidebar_input)
  #     df <- df %>% filter(subregion == input$selected_african_subregion_sidebar_input)
  #   }
  #   # Validate if data frame is empty after filtering
  #   validate(need(nrow(df) > 0, paste("No data available for",input$selected_continent_sidebar, "in", input$selected_year_sidebar,ifelse(!is.null(input$selected_african_subregion_sidebar_input) && input$selected_african_subregion_sidebar_input != "All", paste("and subregion", input$selected_african_subregion_sidebar_input), ""), ". Please adjust global filters.")))
  #   return(df)
  # })
  # 
  
  
  
  observe({ 
    df_for_country_choices <- data_selected_by_global_filters() # This now reflects global country filter too
    
    if (input$selected_continent_sidebar == "Africa" && # Check global continent selection
        !is.null(input$explorer_subregion_filter_input) && # Explorer's own subregion filter
        input$explorer_subregion_filter_input != "All") {
      df_for_country_choices <- df_for_country_choices %>% 
        filter(subregion == input$explorer_subregion_filter_input)
    }
    available_countries <- sort(unique(na.omit(df_for_country_choices$country)))
    updateSelectInput(session, "explorer_country_filter_input", 
                      choices = c("All", available_countries), 
                      selected = input$explorer_country_filter_input) 
  })
  
  # --- B. Overview Tab ---
  output$overview_heatmap_plot <- renderPlot({
    req(data_selected_by_global_filters())
    df_for_heatmap <- data_selected_by_global_filters()
    
    # For overview, focus on Africa or selected continent's primary regions if available
    if(input$selected_continent_sidebar == "Africa" && "subregion" %in% names(df_for_heatmap)){
      grouping_var <- sym("subregion")
      plot_title <- paste("Heatmap of Average Weather Conditions by African Subregions -", input$selected_year_sidebar)
      y_label <- "African Subregion"
      df_for_heatmap <- df_for_heatmap %>% filter(!is.na(subregion))
      validate(need(nrow(df_for_heatmap) > 0, "No subregional data for heatmap for selected filters."))
    } else { # Generic case for other continents (or Africa if subregion not used)
      grouping_var <- sym("country")
      plot_title <- paste("Heatmap of Average Weather Conditions by Country -", input$selected_continent_sidebar, input$selected_year_sidebar)
      y_label <- "Country"
      validate(need(nrow(df_for_heatmap) > 0, "No country data for heatmap for selected filters."))
    }
    
    summary_data <- df_for_heatmap %>%
      group_by(!!grouping_var) %>%
      summarise(
        Avg_Temp = mean(temperature_celsius, na.rm = TRUE),
        Avg_Humidity = mean(humidity, na.rm = TRUE),
        Avg_cloud = mean(cloud, na.rm = TRUE),
        Avg_Precip = mean(precip_mm, na.rm = TRUE),
        Avg_Wind_MPH = mean(wind_mph, na.rm = TRUE),
        Avg_air_quality = mean(air_quality_us_epa_index),
        .groups = 'drop'
      ) %>%
      pivot_longer(cols = -!!grouping_var, names_to = "metric", values_to = "value") %>%
      filter(!is.na(value)) # Remove metrics that are all NA for a group
    
    validate(need(nrow(summary_data) > 0, "Not enough data to generate heatmap after summarization. Check for NAs or try different filters."))
    
    ggplot(summary_data, aes(x = metric, y = !!grouping_var, fill = value)) +
      geom_tile(color = "white", lwd=0.5) +
      geom_text(aes(label = round(value, 1)), size = 10) +
      scale_fill_viridis_c(option = "turbo", direction = -1) +
      labs(title = plot_title, x = "Weather Conditions", y = y_label, fill = "Avg. Value") +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(hjust = 0.5, face="bold"),
            panel.grid = element_blank())
  })
  
  #  Country bar plot to show average temperature
  output$overview_country_bar_plot <- renderPlotly({
    req(data_selected_by_global_filters())
    df_for_plot <- data_selected_by_global_filters()
    
    validate(need("temperature_celsius" %in% names(df_for_plot), 
                  "Temperature data (temperature_celsius) is not available to generate this plot."))
    
    country_avg_temps <- df_for_plot %>%
      filter(!is.na(country) & !is.na(temperature_celsius)) %>% # Ensure country and temperature are not NA
      group_by(country) %>%
      summarise(
        avg_temp = mean(temperature_celsius, na.rm = TRUE),
        observation_count = n(), # Keep count for tooltip or secondary info if desired
        .groups = 'drop'
      ) %>%
      filter(!is.na(avg_temp)) %>% # Ensure avg_temp is not NA after summarization
      arrange(desc(avg_temp)) %>% # Sort by average temperature, hottest first
      head(20) # Display top 20 hottest countries
    
    validate(need(nrow(country_avg_temps) > 0, "No valid country average temperature data to display for the selected filters."))
    
    plot_ly(country_avg_temps, 
            x = ~reorder(country, -avg_temp), # Reorder by descending average temperature
            y = ~avg_temp, 
            type = 'bar',
            text = ~paste0(country, "<br>Avg Temp: ", round(avg_temp, 1), "°C<br>Observations: ", observation_count), # Custom hover text
            hoverinfo = 'text',
            marker = list(color = 'rgba(255, 127, 80, 0.8)', # Coral color for temperature
                          line = list(color = 'rgba(255, 127, 80, 1.0)', width = 1))) %>% # Border for bars
      layout(title = list(text = paste("Top 20 Countries by Average Temperature <br>(", 
                                       input$selected_continent_sidebar, " - ", input$selected_year_sidebar, ")"),
                          font = list(size=16)), # Title font size
             xaxis = list(title = "", tickangle = -45, categoryorder = "array", categoryarray = ~reorder(country, -avg_temp)), # Ensure order is maintained
             yaxis = list(title = "Average Temperature (°C)"),
             margin = list(b = 120, t = 80)) # Adjust margins for tilted labels and longer title
  })
  
  output$overview_subregion_bar_plot <- renderPlotly({
    req(data_selected_by_global_filters())
    validate(need("subregion" %in% names(data_selected_by_global_filters()), "Subregion data not available for selected continent."))
    
    counts <- data_selected_by_global_filters() %>% 
      filter(!is.na(subregion)) %>%
      count(subregion, sort = TRUE)
    validate(need(nrow(counts) > 0, "No subregion data for bar chart."))
    plot_ly(counts, x = ~reorder(subregion, n), y = ~n, type = 'bar', marker = list(color = 'rgba(238, 96, 85, 0.8)')) %>%
      layout(title = paste("Observations by Subregion <br>(", input$selected_continent_sidebar, "-", input$selected_year_sidebar, ")"),
             xaxis = list(title = ""), yaxis = list(title = "Number of Observations"),
             margin = list(b = 100))
  })
  
  # --- C. High-Dimensional Analysis Tab ---
  dimred_plot_data <- eventReactive(input$dimred_run_button, {
    df <- data_selected_by_global_filters()
    validate(need(nrow(df) > 3, "Not enough data points (<3) for dimensionality reduction. Adjust global filters."))
    
    cols_for_dimred <- c("temperature_celsius", "pressure_mb", "humidity", "precip_mm", "cloud", "wind_degree", "wind_mph")
    
    missing_cols <- setdiff(cols_for_dimred, names(df))
    validate(need(length(missing_cols) == 0, paste("Missing columns for dimensionality reduction:", paste(missing_cols, collapse=", "))))
    
    # Select the color variable and ensure it's part of the data for reduction
    # The name of the color variable input is input$dimred_color_var_input
    # The actual column name will be the value of input$dimred_color_var_input (e.g., "air_quality_us_epa_index")
    color_var_name <- input$dimred_color_var_input
    validate(need(color_var_name %in% names(df), paste("Selected color variable '", color_var_name, "' not found in the data.")))
    
    data_for_reduction <- df %>% 
      select(all_of(c(cols_for_dimred, color_var_name))) %>%  # Include the color variable column
      drop_na() # Drop rows with NA in any of these columns
    
    validate(need(nrow(data_for_reduction) > 3, "Not enough data points after NA removal for dimensionality reduction."))
    
    numeric_df_for_scaling <- data_for_reduction %>% select(all_of(cols_for_dimred))
    scaled_df <- scale(numeric_df_for_scaling)
    
    # Extract the actual color values from the data_for_reduction dataframe
    color_values <- data_for_reduction[[color_var_name]]
    
    if (input$dimred_method_input == "t-SNE") {
      req(input$dimred_perplexity_input)
      n_points <- nrow(scaled_df)
      # Rtsne perplexity should be less than (n-1)/3. Min perplexity for Rtsne is usually low but >0.
      # Let's ensure perplexity is at least 1 and respects the n-1/3 rule.
      max_perplexity <- max(1, floor((n_points - 1) / 3) -1) # ensure it's at least 1
      if (n_points <=3) max_perplexity <- 1 # handle very small n
      
      chosen_perplexity <- min(input$dimred_perplexity_input, max_perplexity)
      if (chosen_perplexity < 1) chosen_perplexity <- 1 
      
      validate(need(n_points > 3 * chosen_perplexity || n_points <=3 , # Rtsne condition, or if n is too small to satisfy, it might still run with low perplexity
                    paste0("Not enough data for t-SNE with perplexity ", chosen_perplexity, ". Max valid perplexity is approx ", max_perplexity," for ",n_points, " points. Try reducing perplexity or changing filters.")))
      
      set.seed(123)
      # Added pca_scale = FALSE (or TRUE), pca_center for Rtsne default behavior and stability
      # Also, ensure pca_dims is less than or equal to number of columns
      pca_dims_val = min(50, ncol(scaled_df))
      result <- Rtsne(scaled_df, perplexity = chosen_perplexity, check_duplicates = FALSE, pca = (ncol(scaled_df) > pca_dims_val), pca_dims = pca_dims_val)$Y
    } else { # UMAP
      set.seed(123)
      result <- umap(scaled_df)
    }
    return(list(coords = result, color_data = color_values, method = input$dimred_method_input, color_by = color_var_name))
  })
  
  output$dimred_plot_output <- renderPlotly({
    plot_data <- dimred_plot_data()
    req(plot_data)
    
    # Create a data frame for Plotly
    # Ensure 'Color' column in df_plot retains its original type from plot_data$color_data
    df_plot <- data.frame(
      X1 = plot_data$coords[,1], 
      X2 = plot_data$coords[,2], 
      Color = plot_data$color_data, 
      stringsAsFactors = FALSE # Important to preserve numeric/character types
    )
    
    # Prepare common plot_ly arguments
    plot_args <- list(
      data = df_plot,
      x = ~X1,
      y = ~X2,
      type = 'scatter',
      mode = 'markers',
      text = ~paste0(gsub("_", " ", plot_data$color_by), ": ", Color), # Make label more readable
      hoverinfo = 'text+x+y'
    )
    
    if (is.numeric(df_plot$Color)) {
      # For numeric data, map to color aesthetic and specify a colorscale within marker
      plot_args$color <- ~Color # Map the numeric variable to color
      plot_args$marker <- list(colorscale = 'Viridis', # Valid Plotly colorscale string
                               showscale = TRUE,       # Show color bar legend
                               cmin = min(df_plot$Color, na.rm=TRUE), # Optional: set color range
                               cmax = max(df_plot$Color, na.rm=TRUE)  # Optional: set color range
      ) 
    } else {
      # For categorical data, ensure it's treated as a factor.
      # Plotly will use its default categorical color palette.
      plot_args$color <- ~as.factor(Color)
     
    }
    
    # Construct the plot
    p <- do.call(plot_ly, plot_args)
    
    # Apply layout
    p %>% layout(title = paste(plot_data$method, "Visualization (Colored by", gsub("_", " ", plot_data$color_by), ")"),
                 xaxis = list(title = ifelse(plot_data$method == "t-SNE", "t-SNE Dimension 1", "UMAP Dimension 1")),
                 yaxis = list(title = ifelse(plot_data$method == "t-SNE", "t-SNE Dimension 2", "UMAP Dimension 2")),
                 legend = list(title=list(text=gsub("_", " ", plot_data$color_by)))) # Make legend title readable
  })
  
  # --- D. Weather Similarity Network Tab ---
  network_graph_object <- reactive({
    df <- data_selected_by_global_filters()
    # Use location_name for nodes as it's more specific than country
    validate(need("location_name" %in% names(df), "Column 'location_name' needed for network nodes is missing."))
    
    # Columns for similarity - ensure these are numeric and exist
    cols_for_similarity <- c("temperature_celsius", "wind_mph", "pressure_mb", "precip_mm", "humidity", "cloud")
    missing_cols <- setdiff(cols_for_similarity, names(df))
    validate(need(length(missing_cols) == 0, paste("Missing columns for network similarity:", paste(missing_cols, collapse=", "))))
    
    similarity_data <- df %>%
      select(location_name, all_of(cols_for_similarity)) %>%
      group_by(location_name) %>%
      summarise(across(all_of(cols_for_similarity), ~mean(., na.rm = TRUE)), .groups = 'drop') %>%
      drop_na() # Remove locations where any key metric is NA after averaging
    
    validate(need(nrow(similarity_data) >= 2, "At least 2 locations with complete data are needed for network analysis."))
    
    data_scaled <- scale(similarity_data[, cols_for_similarity])
    rownames(data_scaled) <- similarity_data$location_name
    
    dist_matrix_val <- dist(data_scaled, method = "euclidean")
    sim_matrix_val <- 1 / (1 + as.matrix(dist_matrix_val)) # Convert dist to matrix before operations
    
    edge_list_indices <- which(sim_matrix_val > input$network_similarity_thresh_input & upper.tri(sim_matrix_val), arr.ind = TRUE)
    
    if (nrow(edge_list_indices) == 0) {
      # If no edges, create a graph with nodes only
      g <- make_empty_graph(n = nrow(similarity_data), directed = FALSE)
      V(g)$name <- similarity_data$location_name
      V(g)$label <- V(g)$name # For plotting
      # Add other node attributes if needed
      return(g) # Return graph with nodes but no edges
    }
    
    edges_df <- data.frame(
      from = rownames(sim_matrix_val)[edge_list_indices[, 1]],
      to = colnames(sim_matrix_val)[edge_list_indices[, 2]],
      weight = sim_matrix_val[edge_list_indices] # for edge width
    )
    
    # Ensure all nodes in edges_df are included as vertices, even if some nodes end up isolated
    all_nodes_in_data <- similarity_data %>% select(location_name)
    g <- graph_from_data_frame(d = edges_df, vertices = all_nodes_in_data, directed = FALSE)
    V(g)$label <- V(g)$name # For plotting
    
    return(g)
  })
  
  output$network_plot_output <- renderPlot({
    g <- network_graph_object()
    validate(need(length(V(g)) > 0, "No nodes to plot in the network."))
    
    layout_func <- get(input$network_layout_method_input) # Get layout function by name
    
    # Set a default plot if no edges
    if (length(E(g)) == 0) {
      plot(g, layout = layout_func, vertex.label = V(g)$label, vertex.size = 8, vertex.color = "skyblue",
           main = paste("Weather Similarity Network (Threshold:", input$network_similarity_thresh_input, ")\nNo edges meet threshold."))
      return()
    }
    
    plot_args <- list(
      x = g,
      layout = layout_func(g), # Apply layout function
      vertex.label = V(g)$label,
      vertex.size = 8,
      vertex.label.cex = 0.9,
      edge.width = E(g)$weight * 5, # Scale edge width by similarity
      edge.color = "gray70",
      main = paste("Weather Similarity Network (Threshold:", input$network_similarity_thresh_input, ")")
    )
    
    if (input$network_show_communities_input) {
      communities <- cluster_louvain(g)
      V(g)$community <- communities$membership
      num_communities <- length(unique(V(g)$community))
      community_colors_palette <- if (num_communities > 0) viridis::viridis_pal(option="D")(num_communities) else "grey"
      
      plot_args$vertex.color <- if(num_communities > 0) community_colors_palette[V(g)$community] else "skyblue"
      plot_args$main <- paste(plot_args$main, "- Communities (Louvain)")
      
      # Plot with community colors
      do.call(plot, plot_args)
      
      if (num_communities > 0 && num_communities <= 15) { # Add legend if manageable number of communities
        legend("topright", legend = paste("Comm.", unique(V(g)$community)), 
               fill = community_colors_palette[unique(V(g)$community)], 
               cex = 0.8, title = "Communities", bty = "n")
      }
    } else {
      plot_args$vertex.color <- "skyblue"
      do.call(plot, plot_args)
    }
  }, res = 96) # Good resolution for plots
  
  # --- E. Temporal Trends Animation Tab ---
  # This will hold the path to the generated GIF
  animation_file_path <- reactiveVal(NULL)
  
  observeEvent(input$temporal_run_animation_button, {
    # Indicate processing
    animation_file_path(NULL) # Clear previous
    showModal(modalDialog("Generating animation, please wait...", footer=NULL, easyClose=FALSE))
    
    temp_data_for_anim <- data_selected_by_global_filters() %>%
      mutate(year_month = floor_date(last_updated, unit = "month") %>% as.Date()) %>%
      group_by(year_month, country, subregion) %>% # Ensure subregion exists if used for color
      summarise(
        # Ensure all selected vars are aggregated
        !!input$temporal_x_var_input := mean(!!sym(input$temporal_x_var_input), na.rm = TRUE),
        !!input$temporal_y_var_input := mean(!!sym(input$temporal_y_var_input), na.rm = TRUE),
        !!input$temporal_size_var_input := mean(!!sym(input$temporal_size_var_input), na.rm = TRUE),
        # Add other potentially needed vars like subregion if it's not already grouped by
        subregion = first(subregion), 
        .groups = 'drop'
      ) %>%
      drop_na() # Drop rows where any of the key aesthetics are NA
    
    validate(
      need(nrow(temp_data_for_anim) > 0, "No data for animation after filtering and aggregation."),
      need(input$temporal_x_var_input %in% names(temp_data_for_anim), "X variable not found."),
      need(input$temporal_y_var_input %in% names(temp_data_for_anim), "Y variable not found."),
      need(input$temporal_size_var_input %in% names(temp_data_for_anim), "Size variable not found.")
    )
    
    # Create a unique file name for the animation in www for accessibility
    outfile_name <- paste0("animation_", gsub("[ :]", "_", Sys.time()), ".gif")
    outfile_path <- file.path("www", outfile_name) # Save in www to be served directly
    if (!dir.exists("www")) dir.create("www")
    
    p <- ggplot(temp_data_for_anim, 
                aes(x = .data[[input$temporal_x_var_input]], 
                    y = .data[[input$temporal_y_var_input]], 
                    size = .data[[input$temporal_size_var_input]], 
                    color = subregion)) + # Assuming subregion for color
      geom_point(alpha = 0.7) +
      geom_text_repel(aes(label = country), size = 3.5, max.overlaps = 15, segment.alpha = 0.5) +
      scale_color_viridis_d(option = "turbo") + # Using a vibrant palette
      scale_size_continuous(range = c(3, 15)) + # Enhanced size range
      labs(
        title = paste("Monthly Trends:", input$selected_continent_sidebar, input$selected_year_sidebar),
        subtitle = "Date: {frame_time}",
        caption = "Source: Global Weather Repository",
        x = str_to_title(gsub("_", " ", input$temporal_x_var_input)),
        y = str_to_title(gsub("_", " ", input$temporal_y_var_input)),
        color = "Subregion",
        size = str_to_title(gsub("_", " ", input$temporal_size_var_input))
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 13, hjust = 0.5),
        legend.position = "bottom",
        panel.grid.minor = element_blank()
      ) +
      transition_time(year_month) +
      ease_aes("linear") +
      enter_fade() +
      exit_fade()
    
    num_frames <- length(unique(temp_data_for_anim$year_month))
    anim_duration <- max(5, num_frames / input$temporal_animation_speed_input) # Ensure minimum duration
    
    tryCatch({
      anim_save(outfile_path, animate(p, nframes = num_frames, fps = input$temporal_animation_speed_input, 
                                      duration = anim_duration,
                                      width = 1000, height = 750, res = 96, renderer = gifski_renderer()))
      animation_file_path(outfile_name) # Store only the name for relative path from www
    }, error = function(e) {
      showNotification(paste("Error generating animation:", e$message), type = "error", duration = 10)
      animation_file_path("ERROR") # Signal an error
    })
    removeModal()
  })
  
  output$temporal_animation_output_ui <- renderUI({
    path_val <- animation_file_path()
    if (is.null(path_val)) {
      return(p("Click 'Generate Animation' to view trends.", style="text-align:center; padding: 20px;"))
    } else if (path_val == "ERROR") {
      return(p("An error occurred during animation generation. Please check data and parameters.", style="color:red; text-align:center; padding: 20px;"))
    } else {
      # Use timestamp to force browser refresh of GIF
      return(tags$img(src = paste0(path_val, "?t=", Sys.time()), 
                      alt = "Temporal Trends Animation", 
                      style = "max-width:100%; height:auto; display:block; margin-left:auto; margin-right:auto;"))
    }
  })
  
  # --- F. EA Network Case Study Tab ---
  ea_network_data_reactive <- reactive({
    # Data specific to EA April 1st, 2025 - largely independent of global filters for this case study
    Africa_data_for_ea <- GlobalWeatherRepository_df_processed %>% 
      filter(continent == "Africa", year(last_updated) == 2025) # Base year for case study
    
    
    
    april1_data_ea <- Africa_data_for_ea %>%
      filter(as.Date(last_updated) == as.Date("2025-04-01") & subregion == "Eastern Africa")
    
    cols_for_similarity <- c("temperature_celsius", "wind_mph", "pressure_mb", "precip_mm", "humidity", "cloud")
    missing_cols <- setdiff(cols_for_similarity, names(april1_data_ea))
    validate(need(length(missing_cols) == 0, paste("Missing columns for EA network:", paste(missing_cols, collapse=", "))))
    
    similarity_data_ea <- april1_data_ea %>%
      select(location_name, all_of(cols_for_similarity)) %>%
      distinct(location_name, .keep_all = TRUE) %>%
      drop_na()
    
    validate(need(nrow(similarity_data_ea) >= 2, "Not enough data for Eastern Africa on April 1st, 2025, for network analysis."))
    
    data_for_dist_ea <- similarity_data_ea %>% select(-location_name)
    rownames(data_for_dist_ea) <- similarity_data_ea$location_name
    
    dist_matrix_ea <- dist(scale(data_for_dist_ea))
    dist_matrix_m_ea <- as.matrix(dist_matrix_ea)
    
    threshold_ea <- mean(dist_matrix_m_ea) # Threshold from original script logic
    
    graph_edges_list_ea <- list()
    for(i in 1:(nrow(dist_matrix_m_ea)-1)){
      for(j in (i+1):nrow(dist_matrix_m_ea)){
        if(dist_matrix_m_ea[i,j] <= threshold_ea){ # Original script uses <= threshold
          graph_edges_list_ea[[length(graph_edges_list_ea) + 1]] <- c(source = rownames(dist_matrix_m_ea)[i], 
                                                                      target = rownames(dist_matrix_m_ea)[j])
        }
      }
    }
    
    if(length(graph_edges_list_ea) == 0 && nrow(data_for_dist_ea) > 0) {
      g_ea <- make_empty_graph(n = nrow(data_for_dist_ea), directed = FALSE)
      V(g_ea)$name <- rownames(data_for_dist_ea)
    } else if (length(graph_edges_list_ea) > 0) {
      graph_edges_df_ea <- do.call(rbind, graph_edges_list_ea)
      g_ea <- graph_from_edgelist(as.matrix(graph_edges_df_ea), directed=FALSE)
    } else {
      validate("No data or no edges found for EA network graph.")
    }
    V(g_ea)$label <- V(g_ea)$name
    
    centrality_df_ea <- data.frame(label=character(), degree=numeric(), eigen=numeric(), closeness=numeric(), betweenness=numeric())
    if(length(V(g_ea)) > 0) {
      # Only calculate centrality if there are vertices
      degr_cent_ea <- if(length(V(g_ea)) > 0) centr_degree(g_ea, mode = 'all')$res else numeric(0)
      eign_cent_ea <- if(length(V(g_ea)) > 0 && length(E(g_ea)) > 0) eigen_centrality(g_ea)$vector else rep(0, length(V(g_ea))) # Eigen needs edges
      clos_cent_ea <- if(length(V(g_ea)) > 0) igraph::closeness(g_ea, normalized = TRUE) else numeric(0)
      betw_cent_ea <- if(length(V(g_ea)) > 0) igraph::betweenness(g_ea, normalized = TRUE) else numeric(0)
      
      centrality_df_ea <- data.frame(
        label = V(g_ea)$name,
        degree = degr_cent_ea,
        eigen = eign_cent_ea,
        closeness = clos_cent_ea,
        betweenness = betw_cent_ea
      ) %>% arrange(desc(degree))
    }
    return(list(graph = g_ea, centrality_data = centrality_df_ea))
  })
  
  output$ea_network_mod_plot <- renderPlot({
    data_ea <- ea_network_data_reactive()
    g_ea <- data_ea$graph
    validate(need(length(V(g_ea)) > 0, "Empty graph for EA Modularity plot."))
    
    mod_groups_ea <- if(length(E(g_ea)) > 0) cluster_fast_greedy(g_ea)$membership else rep(1, length(V(g_ea)))
    degr_cent_val_ea <- if(length(V(g_ea)) > 0) centr_degree(g_ea, mode='all')$res else rep(1, length(V(g_ea)))
    
    plot(g_ea, vertex.color = mod_groups_ea, edge.color = 'black',
         vertex.size = 5 + degr_cent_val_ea / max(1, degr_cent_val_ea, na.rm=TRUE) * 5, # Normalize size
         vertex.label = V(g_ea)$label, vertex.label.cex = 0.9,
         layout = layout_with_fr(g_ea), main = 'EA Network (Modularity Groups) - April 1, 2025')
  }, res=96)
  
  output$ea_network_btw_plot <- renderPlot({
    data_ea <- ea_network_data_reactive()
    g_ea <- data_ea$graph
    validate(need(length(V(g_ea)) > 0, "Empty graph for EA Betweenness plot."))
    
    btw_groups_ea <- if(length(E(g_ea)) > 0) cluster_edge_betweenness(g_ea)$membership else rep(1, length(V(g_ea)))
    degr_cent_val_ea <- if(length(V(g_ea)) > 0) centr_degree(g_ea, mode='all')$res else rep(1, length(V(g_ea)))
    
    plot(g_ea, vertex.color = btw_groups_ea, edge.color = 'black',
         vertex.size = 5 + degr_cent_val_ea / max(1, degr_cent_val_ea, na.rm=TRUE) * 5,
         vertex.label = V(g_ea)$label, vertex.label.cex = 0.9,
         layout = layout_with_kk(g_ea), main = 'EA Network (Edge Betweenness Groups) - April 1, 2025')
  }, res=96)
  
  output$ea_centrality_table_output <- renderDT({
    data_ea <- ea_network_data_reactive()
    validate(need(nrow(data_ea$centrality_data) > 0, "No centrality data to display."))
    datatable(data_ea$centrality_data, options = list(pageLength = 5, scrollX = TRUE, autoWidth=TRUE), rownames = FALSE)
  })
  
  # --- G. Data Explorer Tab ---
  output$explorer_subregion_filter_ui <- renderUI({
    req(input$selected_continent_sidebar) # Based on global continent filter
    if (input$selected_continent_sidebar == "Africa") {
      # Choices based on data already filtered by global continent and year
      available_subregions <- sort(unique(na.omit(data_selected_by_global_filters()$subregion)))
      selectInput("explorer_subregion_filter_input", "Filter by Subregion (in Explorer):", choices = c("All", available_subregions), selected = "All")
    } else {
      NULL # No subregion filter if not Africa
    }
  })
  
  observe({ # Update country choices based on globally filtered data AND explorer subregion filter
    df_for_country_choices <- data_selected_by_global_filters()
    
    if (input$selected_continent_sidebar == "Africa" && !is.null(input$explorer_subregion_filter_input) && input$explorer_subregion_filter_input != "All") {
      df_for_country_choices <- df_for_country_choices %>% filter(subregion == input$explorer_subregion_filter_input)
    }
    available_countries <- sort(unique(na.omit(df_for_country_choices$country)))
    updateSelectInput(session, "explorer_country_filter_input", choices = c("All", available_countries), selected = "All")
  })
  
  data_for_explorer_table <- reactive({
    df <- data_selected_by_global_filters() # Start with globally filtered data
    
    # Apply Explorer's Subregion filter (if Africa)
    if (input$selected_continent_sidebar == "Africa" && !is.null(input$explorer_subregion_filter_input) && input$explorer_subregion_filter_input != "All") {
      df <- df %>% filter(subregion == input$explorer_subregion_filter_input)
    }
    
    # Apply Explorer's Country filter
    if (!is.null(input$explorer_country_filter_input) && input$explorer_country_filter_input != "All") {
      df <- df %>% filter(country == input$explorer_country_filter_input)
    }
    
    # Apply Explorer's Date Range filter
    if (!is.null(input$explorer_date_range_input)) {
      start_date <- as.Date(input$explorer_date_range_input[1])
      end_date <- as.Date(input$explorer_date_range_input[2])
      df <- df %>% filter(as.Date(last_updated) >= start_date & as.Date(last_updated) <= end_date)
    }
    validate(need(nrow(df) > 0, "No data matches the selected explorer filters."))
    return(df)
  })
  
  output$explorer_data_table_output <- renderDT({
    df_to_display <- data_for_explorer_table() %>% 
      select(-any_of(c("continent_code", "country_code"))) # Remove less useful codes if they exist
    datatable(df_to_display, 
              options = list(pageLength = 10, scrollX = TRUE, autoWidth=TRUE, search = list(regex = TRUE, caseInsensitive = TRUE)),
              filter = 'top', # Enable column-wise filters
              rownames = FALSE,
              class = "display compact", # Styling
              escape = FALSE) # To render HTML if any
  })
  
  # Clean up temporary animation files when session ends
  session$onSessionEnded(function() {
    files_to_remove <- list.files(path="www", pattern="^animation_.*\\.gif$", full.names=TRUE)
    if (length(files_to_remove) > 0) {
      unlink(files_to_remove)
    }
  })
}

# --- 4. Run the Shiny App ---
shinyApp(ui, server)