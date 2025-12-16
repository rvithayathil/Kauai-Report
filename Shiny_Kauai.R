# app.R
# Shiny Dashboard for Kauai vs Hawaii Census Analysis

library(shiny)
library(shinydashboard)
library(tidyverse)
library(sf)
library(leaflet)
library(plotly)
library(DT)
library(scales)

# Load the pre-saved data
hawaii_data <- readRDS("data/hawaii_data.rds")
hawaii_spatial <- readRDS("data/hawaii_spatial.rds")

# Define color palette
kauai_colors <- c("Kauai" = "darkgreen", "Other Counties" = "#2E86AB")

#==================== UI ====================
ui <- dashboardPage(
  skin = "green",
  
  # Header
  dashboardHeader(
    title = "Kauai vs Other Counties",
    titleWidth = 300
  ),
  
  # Sidebar
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "tabs",
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Interactive Maps", tabName = "maps", icon = icon("map")),
      menuItem("Economic Analysis", tabName = "economics", icon = icon("dollar-sign")),
      menuItem("Demographics", tabName = "demographics", icon = icon("users")),
      menuItem("Data Table", tabName = "data_table", icon = icon("table")),
      menuItem("Compare Counties", tabName = "compare", icon = icon("chart-bar"))
    ),
    
    # Filters
    hr(),
    h4("County Filter", style = "padding-left: 15px;"),
    h5("Interacts with most visuals", style = "padding-left: 15px;"),
    
    checkboxGroupInput(
      "selected_counties",
      "Select Counties:",
      choices = unique(hawaii_data$county_name),
      selected = unique(hawaii_data$county_name)
    )
   
  ),
  
  # Body
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper { background-color: #f4f6f9; }
        .box { border-top: 3px solid #2E86AB; }
        .info-box { min-height: 90px; }
        .small-box { border-radius: 5px; }
      "))
    ),
    
    tabItems(
      # Overview Tab
      tabItem(
        tabName = "overview",
        h2("Summary: Kauai County Analysis"),
        h3("A Comparative Study of Hawaiian Counties Using Averages from 2018-2022 ACS Data"),
        
        fluidRow(
          valueBoxOutput("pop_box", width = 3),
          valueBoxOutput("income_box", width = 3),
          valueBoxOutput("home_value_box", width = 3),
          valueBoxOutput("education_box", width = 3)
        ),
        
        fluidRow(
          box(
            title = "Key Findings",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            uiOutput("key_findings")
          )
        ),
        
        fluidRow(
          box(
            title = "County Comparison Overview",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("overview_comparison", height = 400)
          )
        )
      ),
      
      # Maps Tab
      tabItem(
        tabName = "maps",
        h2("Interactive Geographic Visualizations"),
        selectInput(
          "metric_choice",
          "Choose Metric:",
          choices = c(
            "Median Income" = "median_income",
            "Median Home Value" = "median_home_value",
            "Median Age" = "median_age",
            "Bachelor's %" = "bachelors_pct",
            "Unemployment %" = "unemployment_rate"
          ),
          selected = "median_income",
          width = "100%"
        ),
         fluidRow(
          box(
            title = "Population Distribution",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            leafletOutput("pop_map", height = 400)
          ),
          box(
            title = "Selected Metric",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            leafletOutput("metric_map", height = 400)
          )
        ),
        
        fluidRow(
          box(
            title = "Map Insights",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            uiOutput("map_insights")
          )
        )
      ),
      
      # Economics Tab
      tabItem(
        tabName = "economics",
        h2("Economic Indicators"),
        
        fluidRow(
          box(
            title = "Income vs Home Value",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("scatter_plot", height = 400)
          ),
          box(
            title = "Median Income by County",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("income_bar", height = 400)
          )
        ),
        
        fluidRow(
          box(
            title = "Housing Market Analysis",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("housing_bar", height = 400)
          ),
          box(
            title = "Affordability Ratio",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("affordability_plot", height = 400)
          )
        )
      ),
      
      # Demographics Tab
      tabItem(
        tabName = "demographics",
        h2("Demographic Characteristics"),
        
        fluidRow(
          box(
            title = "Educational Attainment",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("education_plot", height = 400)
          ),
          box(
            title = "Age Distribution",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("age_plot", height = 400)
          )
        ),
        
        fluidRow(
          box(
            title = "Employment Indicators",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("unemployment_plot", height = 400)
          ),
          box(
            title = "Demographic Summary",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            uiOutput("demo_summary")
          )
        )
      ),
      
      # Data Table Tab
      tabItem(
        tabName = "data_table",
        h2("Comprehensive Data View"),
        
        fluidRow(
          box(
            title = "Interactive Data Table",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            DTOutput("data_table")
          )
        ),
        
        fluidRow(
          box(
            title = "Download Data",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            downloadButton("download_csv", "Download CSV"),
            downloadButton("download_excel", "Download Excel")
          )
        )
      ),
      
      # Compare Tab
      tabItem(
        tabName = "compare",
        h2("Custom County Comparison"),
        
        fluidRow(
          box(
            title = "Select Counties to Compare",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            checkboxGroupInput(
              "compare_counties",
              "Choose up to 5 counties:",
              choices = unique(hawaii_data$county_name),
              selected = c("Kauai", "Honolulu"),
              inline = TRUE
            )
          )
        ),
        
        fluidRow(
          box(
            title = "Multi-Metric Comparison",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("radar_plot", height = 500)
          )
        ),
        
        fluidRow(
          box(
            title = "Side-by-Side Comparison",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("comparison_bars", height = 400)
          )
        )
      )
    )
  )
)

#==================== SERVER ====================
server <- function(input, output, session) {
  
  # Reactive data based on filters
  filtered_data <- reactive({
    hawaii_data %>%
      filter(county_name %in% input$selected_counties)
  })
  
  filtered_spatial <- reactive({
    hawaii_spatial %>%
      filter(county_name %in% input$selected_counties)
  })
  
  # Get Kauai stats
  kauai_stats <- hawaii_data %>% filter(county_name == "Kauai")
  
  #---------- Value Boxes ----------
  output$pop_box <- renderValueBox({
    valueBox(
      format(kauai_stats$total_pop, big.mark = ","),
      "Kauai Population",
      icon = icon("users"),
      color = "blue"
    )
  })
  
  output$income_box <- renderValueBox({
    valueBox(
      dollar(kauai_stats$median_income),
      "Median Income",
      icon = icon("dollar-sign"),
      color = "green"
    )
  })
  
  output$home_value_box <- renderValueBox({
    valueBox(
      dollar(kauai_stats$median_home_value, scale = 1e-3, suffix = "K"),
      "Median Home Value",
      icon = icon("home"),
      color = "yellow"
    )
  })
  
  output$education_box <- renderValueBox({
    valueBox(
      paste0(round(kauai_stats$bachelors_pct, 1), "%"),
      "Bachelor's Degree+",
      icon = icon("graduation-cap"),
      color = "purple"
    )
  })
  
  #---------- Key Findings ----------
  output$key_findings <- renderUI({
    state_avg <- hawaii_data %>%
      filter(county_name != "Kauai") %>%
      summarise(
        avg_income = mean(median_income, na.rm = TRUE),
        avg_home = mean(median_home_value, na.rm = TRUE)
      )
    
    rank_pop <- rank(-hawaii_data$total_pop)[hawaii_data$county_name == "Kauai"]
    
    HTML(paste0(
      "<ul style='font-size: 14px; line-height: 1.8;'>",
      "<li><strong>Population Rank:</strong> Kauai is one of the least populous counties in Hawaii (only larger thanKalawao county which has less than 100 individuals) </li>",
      "<li><strong>Income Comparison:</strong> Kauai's median income ($", format(kauai_stats$median_income, big.mark = ","), 
      ") is ", ifelse(kauai_stats$median_income > state_avg$avg_income, "above", "below"),
      " the state average ($", format(round(state_avg$avg_income), big.mark = ","), ")</li>",
      "<li><strong>Housing Market:</strong> Median home value of $", format(kauai_stats$median_home_value, big.mark = ","),
      " reflects island real estate dynamics</li>",
      "<li><strong>Education:</strong> ", round(kauai_stats$bachelors_pct, 1), 
      "% of residents hold bachelor's degrees or higher</li>",
      "<li><strong>Employment:</strong> Unemployment rate of ", round(kauai_stats$unemployment_rate, 1), 
      "% indicates labor market conditions</li>",
      "</ul>"
    ))
  })
  
  #---------- Overview Comparison Plot ----------
  output$overview_comparison <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = reorder(county_name, median_income), 
                                     y = median_income, 
                                     fill = is_kauai,
                                     text = paste0(county_name, "<br>",
                                                   "Income: ", dollar(median_income)))) +
      geom_col() +
      coord_flip() +
      scale_fill_manual(values = kauai_colors) +
      scale_y_continuous(labels = dollar_format()) +
      labs(
        title = "Median Household Income Comparison",
        x = NULL,
        y = "Median Income"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = "text")
  })
  
  #---------- Population Map ----------
  output$pop_map <- renderLeaflet({
    pal <- colorNumeric(palette = "YlOrRd", domain = filtered_spatial()$estimate)
    
    leaflet(filtered_spatial()) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -157.8, lat = 20.8, zoom = 7) %>%  # Add this line - centers on Hawaii
      addPolygons(
        fillColor = ~pal(estimate),
        weight = 2,
        opacity = 1,
        color = "white",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#666",
          fillOpacity = 0.9,
          bringToFront = TRUE
        ),
        label = ~paste0(county_name, ": ", format(estimate, big.mark = ","))
      ) %>%
      addLegend(
        pal = pal,
        values = ~estimate,
        opacity = 0.7,
        title = "Population",
        position = "bottomleft"
      )
  })
  
  #---------- Metric Map ----------
  output$metric_map <- renderLeaflet({
    metric_col <- input$metric_choice
    
    spatial_with_metric <- filtered_spatial() %>%
      left_join(filtered_data() %>% select(GEOID, !!sym(metric_col)), by = "GEOID")
    
    pal <- colorNumeric(palette = "Greens", domain = spatial_with_metric[[metric_col]])
    
    metric_labels <- c(
      "median_income" = "Median Income",
      "median_home_value" = "Median Home Value",
      "median_age" = "Median Age",
      "bachelors_pct" = "Bachelor's %",
      "unemployment_rate" = "Unemployment %"
    )
    
    leaflet(spatial_with_metric) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -157.8, lat = 20.8, zoom = 7) %>%  # Add this line - centers on Hawaii
      addPolygons(
        fillColor = ~pal(get(metric_col)),
        weight = 2,
        opacity = 1,
        color = "white",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#666",
          fillOpacity = 0.9,
          bringToFront = TRUE
        ),
        label = ~paste0(county_name, ": ", round(get(metric_col), 2))
      ) %>%
      addLegend(
        pal = pal,
        values = ~get(metric_col),
        opacity = 0.7,
        title = metric_labels[[metric_col]],
        position = "bottomleft"
      )
  })
  
  #---------- Map Insights ----------
  output$map_insights <- renderUI({
    HTML(paste0(
      "<p style='font-size: 14px;'>",
      "The interactive maps above allow you to explore spatial patterns across Hawaiian counties. ",
      "Kauai's geographic position in the northwest reflects its role as a distinct community within the archipelago. ",
      "Use the dropdown menu to explore different socioeconomic metrics and observe how they vary across the islands.",
      "</p>"
    ))
  })
  
  #---------- Scatter Plot ----------
  output$scatter_plot <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = median_income, y = median_home_value,
                                     color = is_kauai, size = total_pop,
                                     text = paste0(county_name, "<br>",
                                                   "Income: ", dollar(median_income), "<br>",
                                                   "Home Value: ", dollar(median_home_value), "<br>",
                                                   "Population: ", format(total_pop, big.mark = ",")))) +
      geom_point(alpha = 0.8) +
      scale_color_manual(values = kauai_colors, name = NULL) +
      scale_size_continuous(range = c(5, 15), guide = "none") +
      scale_x_continuous(
        labels = label_dollar(scale = 1e-3, suffix = "K"),  # Shortens labels
        expand = expansion(mult = c(0.4, 0.4))
      ) +
      scale_y_continuous(
        labels = label_dollar(scale = 1e-3, suffix = "K"),  # Show as "$800K" instead of "$800,000"
        expand = expansion(mult = c(0.4, 0.4))
      ) +
      labs(
        title = "Income vs. Home Value",
        x = "Median Household Income",
        y = "Median Home Value"
      ) +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  #---------- Income Bar ----------
  output$income_bar <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = reorder(county_name, median_income),
                                     y = median_income, fill = is_kauai,
                                     text = paste0(county_name, "<br>",
                                                   dollar(median_income)))) +
      geom_col() +
      coord_flip() +
      scale_fill_manual(values = kauai_colors) +
      scale_y_continuous(labels = dollar_format()) +
      labs(x = NULL, y = "Median Income") +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = "text")
  })
  
  #---------- Housing Bar ----------
  output$housing_bar <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = reorder(county_name, median_home_value),
                                     y = median_home_value, fill = is_kauai,
                                     text = paste0(county_name, "<br>",
                                                   dollar(median_home_value)))) +
      geom_col() +
      coord_flip() +
      scale_fill_manual(values = kauai_colors) +
      scale_y_continuous(labels = dollar_format()) +
      labs(x = NULL, y = "Median Home Value") +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = "text")
  })
  
  #---------- Affordability Plot ----------
  output$affordability_plot <- renderPlotly({
    affordability_data <- filtered_data() %>%
      mutate(ratio = median_home_value / median_income)
    
    p <- ggplot(affordability_data, aes(x = reorder(county_name, ratio),
                                        y = ratio, fill = is_kauai,
                                        text = paste0(county_name, "<br>",
                                                      "Ratio: ", round(ratio, 2)))) +
      geom_col() +
      coord_flip() +
      scale_fill_manual(values = kauai_colors) +
      labs(
        title = "Housing Affordability Ratio",
        subtitle = "Home Value / Income (Lower is more affordable)",
        x = NULL,
        y = "Ratio"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = "text")
  })
  
  #---------- Education Plot ----------
  output$education_plot <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = reorder(county_name, bachelors_pct),
                                     y = bachelors_pct, fill = is_kauai,
                                     text = paste0(county_name, "<br>",
                                                   round(bachelors_pct, 1), "%"))) +
      geom_col() +
      coord_flip() +
      scale_fill_manual(values = kauai_colors) +
      labs(
        title = "Bachelor's Degree or Higher",
        x = NULL,
        y = "Percentage (%)"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = "text")
  })
  
  #---------- Age Plot ----------
  output$age_plot <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = reorder(county_name, median_age),
                                     y = median_age, fill = is_kauai,
                                     text = paste0(county_name, "<br>",
                                                   round(median_age, 1), " years"))) +
      geom_col() +
      coord_flip() +
      scale_fill_manual(values = kauai_colors) +
      labs(
        title = "Median Age",
        x = NULL,
        y = "Years"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = "text")
  })
  
  #---------- Unemployment Plot ----------
  output$unemployment_plot <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = reorder(county_name, unemployment_rate),
                                     y = unemployment_rate, fill = is_kauai,
                                     text = paste0(county_name, "<br>",
                                                   round(unemployment_rate, 1), "%"))) +
      geom_col() +
      coord_flip() +
      scale_fill_manual(values = kauai_colors) +
      labs(
        title = "Unemployment Rate",
        x = NULL,
        y = "Percentage (%)"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = "text")
  })
  
  #---------- Demo Summary ----------
  output$demo_summary <- renderUI({
    HTML(paste0(
      "<h4>Kauai Demographic Profile</h4>",
      "<ul style='font-size: 13px; line-height: 1.8;'>",
      "<li><strong>Median Age:</strong> ", round(kauai_stats$median_age, 1), " years</li>",
      "<li><strong>Educational Attainment:</strong> ", round(kauai_stats$bachelors_pct, 1), "% bachelor's+</li>",
      "<li><strong>Labor Force:</strong> ", format(kauai_stats$labor_force, big.mark = ","), " workers</li>",
      "<li><strong>Unemployment:</strong> ", round(kauai_stats$unemployment_rate, 1), "%</li>",
      "</ul>",
      "<p style='font-size: 12px; color: gray;'>",
      "These indicators reflect Kauai's demographic composition and economic vitality.",
      "</p>"
    ))
  })
  
  #---------- Data Table ----------
  output$data_table <- renderDT({
    filtered_data() %>%
      select(
        County = county_name,
        Population = total_pop,
        `Median Income` = median_income,
        `Median Age` = median_age,
        `Median Home Value` = median_home_value,
        `Median Rent` = median_rent,
        `Bachelor's %` = bachelors_pct,
        `Unemployment %` = unemployment_rate
      ) %>%
      datatable(
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = 'Bfrtip'
        ),
        rownames = FALSE
      ) %>%
      formatCurrency(c("Median Income", "Median Home Value", "Median Rent"), digits = 0) %>%
      formatRound(c("Median Age", "Bachelor's %", "Unemployment %"), digits = 1) %>%
      formatStyle(
        "County",
        target = "row",
        backgroundColor = styleEqual("Kauai", "#E8F4F8")
      )
  })
  
  #---------- Downloads ----------
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("hawaii_census_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write_csv(filtered_data(), file)
    }
  )
  
  output$download_excel <- downloadHandler(
    filename = function() {
      paste0("hawaii_census_data_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      writexl::write_xlsx(filtered_data(), file)
    }
  )
  
  #---------- Radar Plot ----------
  output$radar_plot <- renderPlotly({
    compare_data <- hawaii_data %>%
      filter(county_name %in% input$compare_counties) %>%
      mutate(
        across(c(median_income, median_home_value, median_age, 
                 bachelors_pct, unemployment_rate), 
               ~scales::rescale(.x, to = c(0, 100)))
      ) %>%
      select(county_name, median_income, median_home_value, 
             median_age, bachelors_pct, unemployment_rate) %>%
      pivot_longer(-county_name, names_to = "metric", values_to = "value")
    
    plot_ly(
      compare_data,
      type = 'scatterpolar',
      r = ~value,
      theta = ~metric,
      fill = 'toself',
      color = ~county_name,
      mode = 'lines+markers'
    ) %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = TRUE,
            range = c(0, 100)
          )
        ),
        title = "Multi-Metric Radar Comparison (Normalized 0-100)"
      )
  })
  
  #---------- Comparison Bars ----------
  output$comparison_bars <- renderPlotly({
    compare_data <- hawaii_data %>%
      filter(county_name %in% input$compare_counties) %>%
      select(county_name, median_income, median_home_value, 
             bachelors_pct, unemployment_rate) %>%
      pivot_longer(-county_name, names_to = "metric", values_to = "value")
    
    metric_labels <- c(
      "median_income" = "Median Income",
      "median_home_value" = "Home Value",
      "bachelors_pct" = "Bachelor's %",
      "unemployment_rate" = "Unemployment %"
    )
    
    compare_data <- compare_data %>%
      mutate(metric_label = metric_labels[metric])
    
    plot_ly(compare_data, x = ~county_name, y = ~value, 
            color = ~county_name, type = 'bar') %>%
      layout(
        yaxis = list(title = "Value"),
        xaxis = list(title = ""),
        barmode = 'group'
      ) %>%
      facet_wrap(~metric_label, scales = "free_y")
  })
}

# Run the app
shinyApp(ui = ui, server = server)