# Load Packages -------------------------
pacman::p_load(
  rio, # importing data
  here, # relative file pathways
  janitor, # data cleaning and tables
  lubridate, # working with dates
  matchmaker, # dictionary-based cleaning
  tidyverse, # data management and visualization
  #
  styler, # source code formatting
  lintr, # detects bad code patterns, which are not errors
  #
  skimr, # preview tibbles (aka data frames)
  todor, # add TODO comments to your project)
  
  # Table Visualization
  flextable,      # make HTML tables 
  officer,        # helper functions for tables
  kableExtra,
  scales,
  DT,
  
  # ggplot extras
  ggforce,
  shiny
)

source("clean.R")
source("visualize.R")


# UI for Shiny Application
ui <- fluidPage(
  titlePanel("Hospital Admission Data"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Visualizations"),
      selectInput(
        "primary_diagnosis", 
        "Select Primary Diagnosis:", 
        choices = unique(hosp_admi$primary_diagnosis),
        selected = unique(hosp_admi$primary_diagnosis)[1]
      ),
      
      sliderInput(
        "age_group_slider_1", 
        "Select First Age Group Range:", 
        min = 0, 
        max = 90, 
        value = c(0, 45), 
        step = 5, 
        round = TRUE,
        animate = TRUE
      ),
      
      sliderInput(
        "age_group_slider_2", 
        "Select Second Age Group Range:", 
        min = 0, 
        max = 90, 
        value = c(46, 90), 
        step = 5, 
        round = TRUE,
        animate = TRUE
      ),
      
      selectInput(
        "gender_filter", 
        "Select Gender:", 
        choices = c("All", "Male", "Female", "Unknown"),
        selected = "All"
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Age Distribution by Primary Diagnosis", 
                 plotOutput("age_plot")),
        tabPanel("Gender Distribution by Primary Diagnosis", 
                 plotOutput("gender_plot")),
        tabPanel("Combined Scatter Plot",
                 plotOutput("combined_plot")),
        tabPanel("Age Table", htmlOutput("age_table")),
        tabPanel("Gender Table", htmlOutput("gender_table"))
      )
    )
  )
)

# Server Logic
server <- function(input, output) {
  
  filtered_data <- reactive({
    hosp_admi %>% 
      filter(primary_diagnosis == input$primary_diagnosis)
  })
  
  # Render age plot with two sliders
  output$age_plot <- renderPlot({
    plot_data <- filtered_data() %>%
      select(primary_diagnosis, starts_with("Age_")) %>%
      pivot_longer(cols = starts_with("Age_"), names_to = "Age_Group", values_to = "Count") %>%
      mutate(
        Age_Group_Num = as.integer(gsub("Age_(\\d+)_\\d+", "\\1", Age_Group))
      ) %>%
      filter(
        !is.na(Age_Group_Num) & 
          ((Age_Group_Num >= input$age_group_slider_1[1] & Age_Group_Num <= input$age_group_slider_1[2]) |
             (Age_Group_Num >= input$age_group_slider_2[1] & Age_Group_Num <= input$age_group_slider_2[2]))
      )
    
    ggplot(plot_data, aes(x = Age_Group, y = Count, fill = Age_Group)) +
      geom_bar(stat = "identity", position = "stack", color = "black") +
      geom_text(aes(label = Count), vjust = -0.5, size = 4) +
      labs(title = paste("Age Distribution for", input$primary_diagnosis),
           x = "Age Group", y = "Count", fill = "Age Group") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Render gender plot with filtering
  output$gender_plot <- renderPlot({
    plot_gender_data <- filtered_data() %>%
      summarise(
        male = sum(male, na.rm = TRUE),
        female = sum(female, na.rm = TRUE),
        gender_unknown = sum(gender_unknown, na.rm = TRUE)
      ) %>%
      pivot_longer(cols = everything(), names_to = "Gender", values_to = "Count") %>%
      filter(
        (input$gender_filter == "All") |
          (input$gender_filter == "Male" & Gender == "male") |
          (input$gender_filter == "Female" & Gender == "female") |
          (input$gender_filter == "Unknown" & Gender == "gender_unknown")
      )
    
    if (nrow(plot_gender_data) == 0) return(NULL)
    
    plot_gender_data$Gender <- factor(plot_gender_data$Gender, levels = unique(plot_gender_data$Gender))
    
    gender_colors <- c("male" = "#ADD8E6", "female" = "#FFDAB9", "gender_unknown" = "#90EE90")
    gender_colors <- gender_colors[names(gender_colors) %in% plot_gender_data$Gender]
    
    ggplot(plot_gender_data, aes(x = Gender, y = Count, fill = Gender)) +
      geom_bar(stat = "identity", position = "stack", color = "black") +
      geom_text(aes(label = Count), vjust = -0.5, size = 4) +
      scale_fill_manual(values = gender_colors) +
      labs(title = paste("Gender Distribution for", input$primary_diagnosis),
           x = "Gender", y = "Count", fill = "Gender") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Render combined scatter plot with gender filtering
  output$combined_plot <- renderPlot({
    plot_combined_data <- filtered_data() %>%
      select(primary_diagnosis, starts_with("Age_"), male, female, gender_unknown) %>%
      pivot_longer(cols = starts_with("Age_"), names_to = "Age_Group", values_to = "Count") %>%
      pivot_longer(cols = c(male, female, gender_unknown), names_to = "Gender", values_to = "Gender_Count") %>%
      mutate(Age_Group_Num = as.integer(gsub("Age_(\\d+)_\\d+", "\\1", Age_Group))) %>%
      filter(
        !is.na(Age_Group_Num) & 
          ((Age_Group_Num >= input$age_group_slider_1[1] & Age_Group_Num <= input$age_group_slider_1[2]) |
             (Age_Group_Num >= input$age_group_slider_2[1] & Age_Group_Num <= input$age_group_slider_2[2]))
      ) %>%
      filter(
        (input$gender_filter == "All") |
          (input$gender_filter == "Male" & Gender == "male") |
          (input$gender_filter == "Female" & Gender == "female") |
          (input$gender_filter == "Unknown" & Gender == "gender_unknown")
      )
    
    if (nrow(plot_combined_data) == 0) return(NULL)
    
    plot_combined_data$Gender <- factor(plot_combined_data$Gender, levels = unique(plot_combined_data$Gender))
    
    gender_colors <- c("male" = "#ADD8E6", "female" = "#FFDAB9", "gender_unknown" = "#90EE90")
    gender_colors <- gender_colors[names(gender_colors) %in% plot_combined_data$Gender]
    
    ggplot(plot_combined_data, aes(x = Age_Group, y = Count, color = Gender)) +
      geom_point(aes(size = Gender_Count), alpha = 0.8, position = position_jitter(width = 0.2)) +
      facet_wrap(~ primary_diagnosis, scales = "free_y") +  
      labs(title = "Scatter Plots for Each Age Group by Primary Diagnosis",
           x = "Age Group", y = "Count", color = "Gender", size = "Total Count") +
      scale_color_manual(values = gender_colors) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Render age table
  output$age_table <- renderUI({
    table_age_for_kable %>%
      kable(
        format = "html",
        caption = "Age Distribution by Primary Diagnosis"
      ) %>%
      kable_styling(
        bootstrap_options = c("striped", "hover", "condensed", "responsive"),
        full_width = FALSE
      ) %>%
      row_spec(0, bold = TRUE, color = "white", background = "#ADD8E6", extra_css = "border: 1px solid black;") %>%
      row_spec(1, extra_css = "border-top: 1px solid black;") %>%
      row_spec(nrow(table_age_for_kable), extra_css = "border-bottom: 1px solid black;") %>%
      column_spec(1, bold = TRUE, border_right = TRUE, border_left = TRUE) %>%
      column_spec(2:ncol(table_age_for_kable), border_right = TRUE, border_left = TRUE) %>%
      HTML()
  })
  
  # Render gender table
  output$gender_table <- renderUI({
    table_gender_for_kable %>%
      kable(
        format = "html",
        caption = "Gender Distribution by Primary Diagnosis",
        align = "c"
      ) %>%
      kable_styling(
        bootstrap_options = c("striped", "hover", "condensed", "responsive"),
        full_width = FALSE,
        position = "center"
      ) %>%
      row_spec(0, bold = TRUE, color = "white", background = "#ADD8E6", extra_css = "border: 1px solid black;") %>%
      row_spec(1, extra_css = "border-top: 1px solid black;") %>%
      row_spec(nrow(table_gender_for_kable), extra_css = "border-bottom: 1px solid black;") %>%
      column_spec(1, bold = TRUE, border_right = TRUE, border_left = TRUE) %>%
      column_spec(2:ncol(table_gender_for_kable), border_right = TRUE, border_left = TRUE) %>%
      HTML()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
