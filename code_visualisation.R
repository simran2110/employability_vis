#install.packages("shiny")
library(tidyverse)
library(shiny)
library(leaflet)
library(ggplot2)
library(dplyr)
library(highcharter)
library(ggtext)
library(patchwork)
library(readr)
library(sf)
library(ggiraph)
library(ggtext)
library(plotly)
library(htmltools)
library(cowplot)
library(stringr)
library(dplyr)

combined_data <- read_csv("underemployment_regional_trends.csv")
unemployment_data <- read_csv("unemployed_data.csv")
underemployment_data <- read_csv("underemployment_trends.csv")
unemployment_data$Year <- as.numeric(unemployment_data$Year)
jobs_salaries_data_by_industry <- read_csv("job_vacancies_and_salaries_by_year.csv")

job_salaries_data <- read_csv("jobs_salary_without_region.csv")
job_salaries_data$Code <- as.character(sub("-.*", "", job_salaries_data$Code))

australia_sf <- read_sf("/Users/simran/Desktop/MONASH/Sem3/FIT5147/assignments/Ass1 DEP/SA4_2021_AUST_SHP_GDA94/SA4_2021_AUST_GDA94.shp")
geo_data_centroids <- st_centroid(australia_sf)
# Get longitude and latitude from the centroids
australia_sf <- australia_sf %>%
  mutate(
    lng = st_coordinates(geo_data_centroids)[, 1],  # Longitude
    lat = st_coordinates(geo_data_centroids)[, 2]   # Latitude
  )
australia_sf <- australia_sf %>% select('SA4_CODE21', 'geometry', 'lng','lat')

job_salaries_regional_data <- australia_sf %>%
  left_join(job_salaries_data, by = c("SA4_CODE21" = "Code")) 

# Define the UI
ui <- fluidPage(
  
  # Custom CSS for magazine-style layout
  tags$head(
    tags$style(HTML("
      body {
        font-family: 'Arial', sans-serif;
        background: #dadddb;
        font-size:16px;
        word-spacing:2.5px;
      }
      .header {
        color: #d3d9d4;
        text-align: center;
        font-weight: bold;
        padding: 20px;
        font-size: 24px;
        border-radius: 5px;
        background-color: #748292;
        
      }
      .container-fluid{
        margin-right:200px;
        margin-left:200px;
        background:white;
        padding:0px
      }
      .container {
        display: grid;
        #grid-template-columns: 1fr 1fr;
        grid-gap: 15px;
        padding: 15px;
      }
      .details:{
        grid-column: 1 / -1;
        padding: 20px;
        font-size: 20px;
        font-weight: bold;
      }
      .featured-article {
        grid-column: 1 / -1;
        background-color: #eee;
        padding: 20px;
        font-size: 20px;
        font-weight: bold;
      }
      .article {
        background-color: #fafafa;
        border: 1px solid #ddd;
        padding: 15px;
      }
      .article h3 {
        margin-top: 0;
      }
      .heading1 {
        background-color: #748D92;
        color: #D3D9D4;
        padding: 10px 70px;
        border-radius: 15px;
        display: inline-block;
        font-weight: bold;
        width:100%;
        text-align:center;
      }
      .vis_details {
        padding:15px;
        background-color: #f9f7f0;
        border-radius : 10px;
      }
      .sidebar {
        background-color: #f8f9fa;
        padding: 15px;
        border-radius: 5px;
        box-shadow: 0 0 10px rgba(0, 0, 0, 0.1);
      }
      .main-panel {
        padding: 15px;
        background-color: #ffffff;
        border-radius: 5px;
        box-shadow: 0 0 10px rgba(0, 0, 0, 0.1);
      }
      
      .filters_container {
        border: 2px solid #d4dae0;
        border-radius: 8px;
        padding: 15px;
        background-color: #f9f9f9;
        box-shadow: 0px 4px 8px rgba(0, 0, 0, 0.1);
        margin: 20px 0;
      }
      
    "))
  ),
  
  # Header Section
  div(
    class = "header",
    style = "font-family: 'Times New Roman'; background-color: #2E3944; color: #d3d9d4; padding: 10px; text-align: center; box-shadow: 0px 4px 10px rgba(0, 0, 0, 0.2);",
    
    h1("NAVIGATING AUSTRALIAN JOB MARKET", 
       style = "color: #d3d9d4; font-size: 1.5em; font-weight: bold; margin-bottom: 15px; text-shadow: 1px 1px 2px rgba(0, 0, 0, 0.1);"),
    
    h3("A Deep Dive into Employment, Unemployment, and Underemployment Trends",
       style = "font-size: 0.8em; font-weight: normal; color: #d3d9d4;")
  ),
  
  # Main Content with Grid Layout
  div(class = "container",
      div(class="details",
          h3(class="heading1", "About the data"),
          p("The dataset used in this project was obtained from the Australian Bureau of Statistics (ABS), providing a comprehensive and reliable view of national labor market trends. Spanning the years 2015 to 2021, this data encompasses annual insights on employment, underemployment, unemployment, and job vacancies.
            The ABS gathers this information through systematic labor force surveys, ensuring methodological consistency and accuracy in representing Australia’s diverse workforce demographics. This robust dataset is invaluable in capturing both short-term fluctuations and long-term shifts in employment patterns, segmented across age, gender, and regional variables, which are crucial for analyzing trends and understanding the factors impacting the Australian job market.")
      ),
      
     div( id= "underemployment_container",
       h3(class="heading1", "Employment & Underemployment Trends By Gender and Year"),
       column(width = 3, 
              style = "padding-top:20px;",
              p(class="vis_details","The visualization highlights the diverse landscape of employment types in our dataset, which includes categories such as full-time, part-time, cyclic underemployed (both full-time and part-time), and structural underemployed (both full-time and part-time)."),
              uiOutput(class="vis_details", "underemployed_dynamic_text")
              ),
              column(width = 9, 
              style = "padding-top:20px;",
              div(style = "margin-bottom:40px;",
                column(
                  width = 5,
                  selectInput(
                    "underemployed_status", 
                    "Select Employment Trend:",
                    choices = unique(underemployment_data$type),
                    selected = "Employed full-time"
                  )
                ),
                
                column(
                  width = 5,
                  selectInput(
                    "age_group", 
                    "Select Age Group:",
                    choices = unique(underemployment_data$agegroup),
                    selected = "15-19 years"
                  )
                )
              ),
              br(),
              div(
              plotlyOutput("employmentPlot")))
     ),
     
     div(
       h3(class="heading1", "Jobs and Salary Overview"),
       column(width = 12, 
              h4(style="color: #748D92; padding: 10px 70px; border-radius: 15px; display: inline-block; font-weight: bold; width: 100%; text-align: center;",
                 "Jobs and Salary Analysis By Region"),
              div( 
                  div(class = "filters_container",
                      h4(style = "font-weight:bold; margin-bottom: 15px;", "Filters: "),
                      div(class = "row",
                          div(class = "col-sm-4 filter-input",
                              selectInput("data_type", "Select Data Type:", choices = unique(job_salaries_data$Type), selected = "Salary")),
                          div(class = "col-sm-4 filter-input",
                              selectInput("year_pmap", "Select Year:", choices = unique(job_salaries_data$Year))),
                          div(class = "col-sm-4 filter-input",
                              selectInput("gender_pmap", "Select Gender:", choices = unique(job_salaries_data$Gender)))
                      )
                  ),
                  br(),
                ),
              div( 
                  leafletOutput("map")
                )
              ),
       div(
          p(class="vis_details", 
            style = "margin-top:8px;",
            "This Visualisation provides detailed overview for jobs and salaries in different regions of Australia from year 2016 to 2020 and in different gender. Data is pre-selected for Salary category and it can be clearly seen that North Territory is amongst the region with lowest median yearly income. Baulkham Hills and Hawkesbury is amongst the highest salary region in year 2016.")
        ),
       br(),
       div(
         h4(style="color: #748D92; padding: 10px 70px; border-radius: 15px; display: inline-block; font-weight: bold; width: 100%; text-align: center;",
           "Jobs and Salary Analysis by Industry"),
         girafeOutput("jobsPlot", width = "100%", height = "600px"),
         p(class="vis_details", 
           style = "margin-top:8px;",
           "This Visualisation provides detailed overview for average jobs and salaries in different industries of Australia from year 2016 to 2020. It can be clearly seem that mining industry offers the highest median salary followed by Electricity, gas, water and waste services. On other hand, the least salary is in Agriculture, forestry and fishing industry.")
       )
       ),
      div(
        h3(class="heading1", "Unemployment Duration Trends Over Years"),
        column(width = 3, p(class="vis_details", "The visualization highlights unemployment trends, showing long-term unemployment (104 weeks and over) peaking in the early 2000s before declining sharply. Mid-term unemployment (26 to 104 weeks) remains stable with minor fluctuations, while short-term durations (under 26 weeks) consistently stay low. Overall, long-term unemployment dominates the trends, with other categories remaining relatively stable.")),
        column(width = 9,  highchartOutput("unemployment_chart")),
        )
  )
)

server <- function(input, output) {
  scale_radius <- function(x) {
    radius_base <- 5
    max_radius <- 30
    scaled_radius <- sqrt(x)
    scaled_radius <- radius_base + (scaled_radius - min(scaled_radius)) / 
      (max(scaled_radius) - min(scaled_radius)) * (max_radius - radius_base)
    scaled_radius
  }
  
  # Define bins for the legend
  min_value <- 5
  max_value <- 3469
  num_bins <- 5  # Define the number of bins
  breaks <- seq(min_value, max_value, length.out = num_bins + 1)
  
  # Create labels for the legend
  legend_labels <- paste(head(breaks, -1), "-", tail(breaks, -1))
  
  
  # Define unique jitter values for each Employment Status
  jitter_values <- c(
    "Structural underemployed" = 0,
    "Cyclical underemployed" = 1
  )
  jitter_values_year <- c(
    "2015" = 0,
    "2016" = 0.2,
    "2017" = 0.4,
    "2018" = 0.6,
    "2019" = 0.8,
    "2020" = 1,
    "2021" = 1.5,
    "2022" = 1.9,
    "2023" = 1.9,
    "2024" = 1.9
  )
  
  # Color palette for Employment Status
  color_palette_status <- reactive({
    filtered_data <- combined_data %>% 
      filter(Year == input$year) 
    colorFactor(palette = c("red", "blue"), domain = unique(filtered_data$Employment_Status))
  })
  
  # Filtered data with custom jittering
  filtered_data <- reactive({
    req(input$employment_status, input$year)
    combined_data %>%
      filter(Employment_Status == input$employment_status) %>%
      mutate(
        lng_jittered = lng + jitter_values_years[Year],  # Apply unique jitter for lng
        lat_jittered = lat + jitter_values_years[Year]   # Apply unique jitter for lat
      )
  })
  
  # Render the map
  output$map <- renderLeaflet({
    data_to_plt <- filtered_data()
    
    mytext <- paste(
      "State: ", data_to_plt$STATE_NAME, "<br/>",
      "Status: ", data_to_plt$Employment_Status, "<br/>",
      "Gender: ", data_to_plt$Gender, "<br/>",
      "Count: ", data_to_plt$Count, "<br/>",
      sep = ""
    ) %>%
      lapply(htmltools::HTML)
    
    leaflet(data_to_plt) %>%
      setView(lng = 133.7751, lat = -25.2744, zoom = 4.9) %>%
      addTiles() %>%
      addCircleMarkers(
        ~lng_jittered, 
        ~lat_jittered, 
        radius = ~scale_radius(Count),
        color = ~color_palette_status()(Employment_Status),
        fillColor = ~color_palette_status()(Employment_Status),
        fillOpacity = 0.5,
        stroke = FALSE,
        popup = ~paste("<b>State:</b> ", STATE_NAME, "<br>",
                       "<b>Employment Status:</b> ", Employment_Status, "<br>",
                       "<b>Year:</b> ", Year, "<br>",
                       "<b>Gender:</b> ", Gender, "<br>",
                       "<b>Count:</b> ", Count),
        label = mytext,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "13px",
          direction = "auto"
        )
      ) %>%
      addLegend("bottomright", 
                pal = color_palette_status(), 
                values = ~Employment_Status, 
                title = "Employment Status",
                opacity = 0.9)
    
  })
  
  output$unemployment_chart <- renderHighchart({
    # Create the highchart plot
    hchart(unemployment_data, "line", hcaes(x = Year, y = Count, group = Time_Period)) %>%
      hc_xAxis(title = list(text = "Years")) %>%
      hc_yAxis(title = list(text = "The Count of People")) %>%
      hc_add_series(name = "104 weeks and over (2 years and over)", data = data$long_term, type = "line") %>%
      hc_add_series(name = "52 weeks and over (1 year)", data = data$under_1_year, type = "line") %>%
      hc_plotOptions(
        line = list(
          marker = list(
            enabled = TRUE,           # Show markers by default
            radius = 4,               # Size of the markers
            states = list(
              hover = list(
                enabled = TRUE,
                fillOpacity = 1       # Full opacity on hover (optional)
              )
            )
          ),
          dataLabels = list(
            enabled = FALSE           # Disable data labels (optional)
          )
        )
      ) %>%
      hc_annotations(
        list(
          labels = list(
            list(
              point = list(xAxis = 0, yAxis = 0, x = length(years) - 1, y = max(100) / 2),
              text = "This chart shows unemployment duration trends.",
              align = "right",
              style = list(fontSize = "13px", color = "#555555")
            )
          )
        )
      ) %>%
      hc_tooltip(pointFormat = '<b>Category:</b> {series.name}<br><b>Year:</b> {point.x}<br><b>Count:</b> {point.y}') %>%
      hc_legend(enabled = TRUE)
  })
   
  # Create a reactive expression to process data based on inputs
  processed_data <- reactive({
    # Filter and select data based on input values
    new_data_required2 <- underemployment_data %>% 
      filter(agegroup == input$age_group, type == input$underemployed_status) %>% 
      select("year", "Males", "Females")
    
    print(str(new_data_required2))  # Debugging print to check data structure
    
    # Calculate difference and transform data to long format
    data_long <- new_data_required2 %>%
      mutate(diff = abs(Males - Females)) %>%
      pivot_longer(cols = c(Males, Females), names_to = "Gender", values_to = "Value") %>%
      select(year, diff, Gender, Value)
    
    # Separate data for Males and Females
    Males <- data_long %>%
      filter(Gender == "Males")
    Females <- data_long %>%
      filter(Gender == "Females")
    
    # Calculate mean and standard deviation
    stats <- data_long %>%
      group_by(Gender) %>%
      summarise(mean = mean(Value),
                SE = sd(Value)) %>%
      mutate(meanpos = mean + SE,
             meanneg = mean - SE)
    
    #diff <- data_long %>% 
    #  filter(Gender == "Females") %>% #you can chose Males of Females, doesn't matter
    #  mutate(x_pos = Value + (diff/2)) #x position of label (Enrollment value of Males + diff/2)
    diff <- data.frame(
      year = unique(data_long$year),
      diff = abs(Males$Value - Females$Value),  # Calculate absolute difference
      x_pos = ifelse(Males$Value > Females$Value, 
                     (Males$Value + Females$Value) / 2, 
                     (Females$Value + Males$Value) / 2)  # Position in the middle of the two values
    )
    
    # Return all required datasets as a list
    list(data_long = data_long, Males = Males, Females = Females, stats_males = stats %>% filter(Gender == "Males"), stats_females = stats %>% filter(Gender == "Females"), diff = diff)
  })
  
  output$underemployed_dynamic_text <- renderUI({
    underemployed_status <- input$underemployed_status
    age_group <- input$age_group
    if (underemployed_status == "Employed full-time"){
      text_value <- paste("In the category of Employed Full-time, men consistently outnumber women across all age groups from 15 to 64 years, resulting in a significant disparity in the number of employed males compared to females. Notably, in 2024, the overall count of employed individuals has decreased significantly across all age groups compared to previous years. The age group with the highest number of full-time employees is 25-34 years.")
    } else if(underemployed_status == "Employed part-time"){
      # Create the dynamic text based on selected filters
      text_value <- paste("The highest number of individuals employed part-time is found in the 20-24 years age group, while the lowest is observed in the 35-44 years age group. There has been a gradual increase in the number of people employed part-time from 2015 to 2023. However, this number saw a significant decline in 2024. Throughout all years and age groups, females consistently outnumber males in the part-time employment category.")
    } else if(underemployed_status == "Cyclical underemployed part-time"){
      # Create the dynamic text based on selected filters
      text_value <- paste("Cyclical underemployment arises from economic fluctuations. Among individuals aged 25 to 44, males have consistently represented a larger portion of those experiencing cyclical part-time employment. This trend peaked in 2020, during the COVID-19 pandemic.")
    }else if(underemployed_status == "Cyclical underemployed full-time"){
      # Create the dynamic text based on selected filters
      text_value <- paste("Cyclical underemployment occurs due to economic fluctuations. From 2015 to 2024, females have been more prone to full-time cyclical underemployment across all age groups. This trend peaked in 2020, primarily during the COVID-19 pandemic, with the highest prevalence observed in the 25 to 44 age group.")
    }else if(underemployed_status == "Structural underemployed full-time"){
      # Create the dynamic text based on selected filters
      text_value <- paste("Structural underemployment stems from fundamental changes in the economy that shift the demand for certain skills or occupations. This type of full-time underemployment has been observed more frequently in females than in males, with the highest concentration in the 25 to 34 age group. While there was a gradual decline in this category from 2015 to 2023, 2024 saw a significant drop in the number of people affected.")
    }else if(underemployed_status == "Structural underemployed part-time"){
      # Create the dynamic text based on selected filters
      text_value <- paste("Structural underemployment arises from fundamental economic shifts that alter the demand for specific skills or occupations. In part-time structural underemployment, men aged 15 to 64 have been more affected than women. The peak in this category occurred in 2020, during the COVID-19 pandemic, followed by a significant decline in subsequent years.")
    }else{
      # Create the dynamic text based on selected filters
      text_value <- paste("You have selected:", 
                          strong(underemployed_status), 
                          "for the year,",
                          strong(age_group))
    }
      
    
    
    # Render the dynamic text
    p(class = "dynamic_info", text_value)
  
  })
  
  output$employmentPlot <- renderPlotly({
    data <- processed_data()
    print(data)
    stats_males <- data$stats_males
    stats_females <- data$stats_females
    Males <- data$Males
    Females <- data$Females
    diff <- data$diff
    
    ggplot(data$data_long) +
      # Add mean and standard deviation for both groups
      geom_rect(xmin = stats_males$meanneg, xmax = stats_males$meanpos,
                ymin = 2014, ymax = 2025, fill = "#009688", alpha = .05) +  
      geom_vline(xintercept = stats_males$mean, color = "#009688", linetype = "solid", size = .5, alpha = .8) +
      
      geom_rect(xmin = stats_females$meanneg, xmax = stats_females$meanpos,
                ymin = 2014, ymax = 2025, fill = "#762a83", alpha = .05) +
      geom_vline(xintercept = stats_females$mean, linetype = "solid", size = .5, alpha = .8, color = "#762a83") +
      
      # Add point range
      geom_segment(data = Males, aes(x = Value, y = year, yend = Females$year, xend = Females$Value),
                   color = "#aeb6bf", size = 4.5, alpha = .5) +
      
      # Add points
      geom_point(aes(x = Value, y = year, color = Gender, text = paste("Year:", year, "<br>Value:", Value, "<br>Gender:", Gender)), size = 4.5, show.legend = FALSE) +
      
      # Color points
      scale_color_manual(values = c("#762a83", "#009688")) +
      
      # Add point-range labels
      geom_text(data = diff, aes(label = paste("D: ", diff), x = x_pos, y = year), color = "#4a4e4d", size = 3) +
      
      # Add annotations for mean and standard deviations
      geom_text(x = stats_females$mean + 100, y = 1000, label = "MEAN", angle = 90, size = 6, color = "#4a4e4d") +
      geom_text(x = stats_females$meanpos - 1500, y = 1990, label = "STDEV", angle = 90, size = 6, color = "#4a4e4d") +
      
      # Add facets for more control
      facet_grid(year ~ ., scales = "free", space = "free", switch = "y", as.table = FALSE) -> p_styled
    
    p_styled <- p_styled +
      theme_minimal() +
      theme(
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(), 
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 13),
        axis.title.x = element_text(size = 13, margin = margin(r = 20)),
        strip.text.y = element_text(size = 10.5, angle = 0),
        panel.spacing.y = unit(1, "lines"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill = "#f7f7f7", color = NA),
        strip.background = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption = element_markdown(hjust = 0, lineheight = 1.5) 
      ) +   
      labs(
        subtitle = "<span style='color: #009688;font-size:20px;'><b>Male</b></span> and <span style='color:#762a83;font-size:20px;'><b>Female</b></span><span style='font-size:20px;'> Employment Trends from 2015 to 2024</span>",
        x = "Count of Employed People",
        y = "Year"
      ) +
      theme(
        plot.subtitle = element_markdown()  # This is important to render HTML-like text
      )
    ggplotly(p_styled, tooltip = "text") %>%
      layout(
        legend = list(
          orientation = "h",        # Vertical orientation
          x = 0.5,                # Position legend outside of the plot area
          y = -0.2,                 # Adjust vertical position
          xanchor = "center",        # Anchor the x position to the left
          yanchor = "bottom"       # Anchor the y position to the middle
        ),
        #margin = list(l = 20, r = 80, t = 40, b = 40),
        margin = list(l = 20, r = 40, t = 40, b = 40),
        
        height = 500,
        width = 800
      )
  })

 
 
    # Reactive expression to filter your data based on input
    filtered_data_2 <- reactive({
      job_salaries_regional_data  %>% filter(Type == input$data_type, Year == input$year_pmap, Gender == input$gender_pmap)
      
    })
    #job_salaries_regional_data2 <- job_salaries_regional_data %>% filter(Type == "Jobs", Year == 2016, Gender == "Males")
    # Render the Leaflet map
    output$map <- renderLeaflet({
      #data_info <- filtered_data_2()
      data4 <- filtered_data_2()
      # Cap the job values to 600 if "Jobs" is selected
      if ("Jobs" %in% data4$Type) {
        data4$Values[data4$Type == "Jobs" & data4$Values > 600] <- 600
      }
      if (input$data_type == "Jobs") {
        scale_name <- "Average Job Count"
      } else {
        scale_name <- "Median Salary (AUD)"
      }
      
      # Create a palette for the fill colors
      pal <- colorNumeric(palette = "YlOrBr", domain = data4$Values, na.color = "transparent")
      # Prepare the text for tooltips:
      mytext <- paste(
        "Region:", data4$Area, "<br/>",
        scale_name,":", data4$Values, "<br/>",
        "Gender:", data4$Gender, "<br/>",
        "Year:", data4$Year,
        sep = ""
      ) %>%
        lapply(htmltools::HTML)
      
      # Create the leaflet map
      leaflet(data = data4) %>%
        setView(lng = 133, lat = -25, zoom = 4) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(
          data = data4,
          fillColor = ~pal(data4$Values),
          weight = 0.5,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 0.7,
          highlightOptions = highlightOptions(
            weight = 2,
            color = "#666",
            fillOpacity = 0.9,
            bringToFront = TRUE
          ),
          label = mytext,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "13px",
            direction = "auto"
          ),
          group = scale_name
        ) %>%
        addLegend(
          pal = pal,
          values = ~data4$Values,
          title = scale_name,
          position = "topright"
        )
    })
    
    
    
  # Render the map plot
    output$jobsPlot <- renderGirafe({
      #print(jobs_salaries_data_by_industry)
      data5 <- data5 <- jobs_salaries_data_by_industry %>%
        filter(Year == 2016) %>%
        arrange(desc(Salary))
        #slice_head(n = 9) 
      
      industries <- unique(data5$Industry)
      #colors <- RColorBrewer::brewer.pal(length(industries), "Set1")
      colors <- hue_pal()(19)
      data5 <- data5 %>%
        mutate(IndustryColor = as.factor(Industry))  # Convert Industry to a factor
      
      # First Plot
      p1 <- ggplot(data5, aes(Jobs, Salary, tooltip = Industry, data_id = Industry, color = IndustryColor)) +
        geom_point_interactive(size = 4) +
        scale_color_manual(values = setNames(colors, industries)) +  # Apply the color palette
        theme_minimal() +
        labs(
          x = "Number of Jobs",
          y = "Salary"
        ) +
        theme(
          axis.text.y = element_text(size = 8),
          legend.position = "none"
        )
      
  
      # Second Plot
      p2 <- ggplot(data5, aes(x = reorder(Industry, Salary), y = Salary, tooltip = Industry, data_id = Industry, fill = IndustryColor)) +
        geom_col_interactive() +
        coord_flip() +
        scale_fill_manual(values = setNames(colors, industries)) +  # Apply the color palette
        theme_minimal() +
        labs(
          x = "Industry",        
          y = "Salary"           
        ) +
        theme(
          axis.text.y = element_text(size = 8),
          legend.position = "none"
        )+
        scale_y_discrete(labels = function(x) str_wrap(x, width = 10)) 
      # Use cowplot to combine the plots
      combined_plot <- plot_grid(p1, p2, ncol = 2, rel_widths = c(0.4, 0.6))
      
      
      girafe(ggobj = combined_plot, width_svg = 10, height_svg = 6)
      
    })
    
  
  
}

# Run the Application
shinyApp(ui = ui, server = server)
