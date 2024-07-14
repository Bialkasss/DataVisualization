library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)
library(DT)
library(dashboardthemes)

ui <- dashboardPage(
  dashboardHeader(title = "Beer Data"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = "about", icon = icon("book")),
      menuItem("Production in different states", tabName = "states", icon = icon("beer")),
      menuItem("Bitterness x alcohol", tabName = "point", icon = icon("dot-circle")),
      menuItem("Popularity of beer styles", tabName = "styles", icon = icon("bar-chart")),
      menuItem("Most alcohol volume", tabName = "alcohol", icon = icon("bar-chart")),
      menuItem("Production in different cities", tabName = "cities", icon = icon("city")),
      menuItem("Table", tabName = "table", icon = icon("table"))
    )
  ),
  dashboardBody(
    shinyDashboardThemes(theme = "purple_gradient"),
    tabItems(
      tabItem("about",
              fluidRow(column(1, align = "left"),
                box(status = "primary", title = "Welcome to Beer Study dashboard!", 
                   div("This dashboard presents data gathered from dataset available under link: ",
                      tags$a("https://www.kaggle.com/datasets/mexwell/beer-study?select=BrewPub.csv"), ". ",
                      "Presented are 6 visualizations, icluding one datatable. Each visualization can be adjusted with user's input. First
                      visualization shows data on production of beer in each state. Second one presents graph of bitterness in respect to alcohol volume. 
                      Third plot shows popularity of different beer styles. Fourth visualization presents average alcohol volume in each style of beer. 
                      The last plot displays production of beer in different cities."
                    )
                ),
                column(1, align = "right",
                       tags$img(align = "center", src = "OIP.jpeg", height = "400px"))
              )
      ),
      tabItem("states", 
              fluidRow(
                box(title = "States with most breweries:", width = 12, solidHeader = TRUE, status = "primary", colour = "EBBB40",
                    plotOutput("plot1", height = 600)),
                box(title = "Change the number of displayed states", width = 12, solidHeader = TRUE, status = "primary",
                    sliderInput("slider", "Number of observations:", 1, 50, 10))
              )
      ),
      tabItem("point",
              fluidRow(
                box(title = "Alcohol volume affecting bitterness of beer:", width = 12, solidHeader = TRUE, status = "primary", colour = "EBBB40",
                    plotlyOutput("plot2", height = 500)),
                box(title = "Change style of analyised beer", width = 12, solidHeader = TRUE, status = "primary",
                    selectInput("selectStyle", "Style of beer:", choices = NULL))
              )
      ),
      tabItem("styles",
              fluidRow(
                box(title = "The most popular beer styles:", width = 12, solidHeader = TRUE, status = "primary", colour = "EBBB40",
                    plotOutput("plot3", height = 600)),
                box(title = "Change the number of displayed styles", width = 12, solidHeader = TRUE, status = "primary",
                    sliderInput("stylesSlider", "Number of observations:", 1, 50, 10))
              )
      ),
      tabItem("alcohol",
              fluidRow(
                box(title = "Styles with highest alcohol volumes:", width = 12, solidHeader = TRUE, status = "primary", colour = "EBBB40",
                    plotOutput("plot4", height = 600)),
                box(title = "Change the number of displayed styles", width = 12, solidHeader = TRUE, status = "primary",
                    selectInput("alcoholSelect", "Number of observations:", choices = c(5, 10, 15, 20)))
              )
      ),
      tabItem("cities",
              fluidRow(
                box(title = "Cities producing the highest amount of beers:", width = 12, solidHeader = TRUE, status = "primary", colour = "EBBB40",
                    plotOutput("plot5", height = 600)),
                box(title = "Change the number of displayed cities", width = 12, solidHeader = TRUE, status = "primary",
                    sliderInput("citySlider", "Number of observations:", 1, 50, 10))
              )
      ),
      tabItem("table",
              box(title = "Data table:", width = 12, solidHeader = TRUE, status = "primary", colour = "EBBB40",
                  DTOutput("table"))
      )
    )
  )
)

server <- function(input, output, session) {
  dataSet <- reactive({
    beer <- read.csv("BrewPub.csv")
    beer <- na.omit(beer)
    
    beer <- beer %>%
      rename(Alcohol = ABV,
             Bitterness = IBU,
             Beer_brand = Name.x,
             Brewery_name = Name.y)
    return(beer)
  })
  
  observe({
    beer <- dataSet()
    noStyles <- length(unique(beer$Style))
    noCities <- length(unique(beer$City))
    styles <- c("All", sort(unique(beer$Style)))
    updateSelectInput(session, "selectStyle", choices = styles)
    updateSliderInput(session, "stylesSlider", max = noStyles)
  })
  
  dataStates <- reactive({
    beer <- dataSet()
    states <- beer %>%
      count(State) %>%
      rename(Count = n) %>%
      arrange(-Count)
    
    states <- states %>%
      mutate(State = factor(State, levels = State))
    return(states)
  })
  
  dataStyles <- reactive({
    beer <- dataSet()
    styles <- beer %>%
      count(Style) %>%
      rename(Count = n) %>%
      arrange(-Count)
    styles <- styles %>%
      mutate(Style = factor(Style, levels = Style))
    return(styles)
  })
  
  dataAvgAlcohol <- reactive({
    beer <- dataSet()
    avg_alcohol <- beer %>%
      group_by(Style) %>%
      summarise(Average_Alcohol = mean(Alcohol, na.rm = TRUE)) %>%
      arrange(desc(Average_Alcohol))
    return(avg_alcohol)
  })
  
  dataCities <- reactive({
    beer <- dataSet()
    cities <- beer %>%
      count(City) %>%
      rename(Count = n) %>%
      arrange(-Count)
    cities <- cities %>%
      mutate(City = factor(City, levels = City))
    return(cities)
  })
  
  output$plot1 <- renderPlot({
    states <- dataStates()
    ggplot(states[1:input$slider,], aes(x = State, y = Count, fill = State)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = Count), position = position_dodge(width = 1), vjust = -0.5, size = 4.5, fontface = "bold") +  
      theme_classic() +
      labs(title = NULL,
           x = NULL,
           y = "Count") +
      scale_fill_viridis_d() + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_text(size = 14, face = "bold"),
            axis.title.y = element_text(size = 16, face = "bold"),
            legend.title = element_text(size = 16, face = "bold"),
            legend.text = element_text(size = 14, face = "bold"))
  })
  
  output$plot2 <- renderPlotly({
    beer <- dataSet()
    filtered_beer <- beer
    if (input$selectStyle != "All") {
      filtered_beer <- filter(beer, Style == input$selectStyle)
    }
    pp <- ggplot(filtered_beer, aes(x = Alcohol, y = Bitterness, color = Alcohol, text = Beer_brand)) +
      geom_point(size = 2) +
      geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs")) +
      theme_bw() +
      labs(x = "Alcohol", y = "Bitterness") +
      theme(axis.title = element_text(size = 14, face = "bold"))
    ggplotly(pp, tooltip = "text")
  })
  
  output$plot3 <- renderPlot({
    styles <- dataStyles()
    ggplot(styles[1:input$stylesSlider,], aes(x = Style, y = Count, fill = Style)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = Count), position = position_dodge(width = 1), vjust = -0.5, size = 4, fontface = "bold") +  
      theme_classic() +
      labs(title = NULL,
           x = NULL,
           y = "Count") +
      scale_fill_viridis_d() + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_text(size = 14, face = "bold"),
            axis.title.y = element_text(size = 16, face = "bold"),
            legend.title = element_text(size = 16, face = "bold"),
            legend.text = element_text(size = 14, face = "bold"))
  })
  
  output$plot4 <- renderPlot({
    alcohol <- dataAvgAlcohol()
    ggplot(avg_alcohol[1:input$alcoholSelect,], aes(x = Average_Alcohol, y = Style, fill = Style)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = round(Average_Alcohol,2)), position = position_dodge(width = 1), hjust = -0.5, size = 4, fontface = "bold") +  
      theme_classic() +
      labs(title = "The most popular beer styles:",
           x = "Count",
           y = NULL) +
      scale_fill_viridis_d() + 
      theme(axis.text.y = element_blank(),
            axis.text.x = element_text(size = 14, face = "bold"),
            axis.title.x = element_text(size = 16, face = "bold"),
            legend.title = element_text(size = 16, face = "bold"),
            legend.text = element_text(size = 14, face = "bold"))
  })
  
  output$plot5 <- renderPlot({
    cities <- dataCities()
    ggplot(cities[1:input$citySlider,], aes(x = City, y = Count, fill = City)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = Count), position = position_dodge(width = 1), vjust = -0.5, size = 4, fontface = "bold") +  
      theme_classic() +
      labs(title = "Cities producing the most beers:",
           x = NULL,
           y = "Count") +
      scale_fill_viridis_d() + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_text(size = 14, face = "bold"),
            axis.title.y = element_text(size = 16, face = "bold"),
            legend.title = element_text(size = 16, face = "bold"),
            legend.text = element_text(size = 14, face = "bold"))
  })
  
  output$table <- renderDT({
    datatable(dataSet(), options = list(pageLength = 10, autoWidth = TRUE))
  })
}

shinyApp(ui, server)
