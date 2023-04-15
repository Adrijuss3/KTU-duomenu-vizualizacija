library(shiny)
library(tidyverse)
library(ggplot2)

ui = fluidPage(
  tags$style("body {background-color: #f0f0f0;}"), 
  titlePanel("412000 Gyvenamųjų ir negyvenamųjų pastatų statyba"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput(inputId = "imones_pavadinimas",
                     label = "Įmonės pavadinimas",
                     choices = NULL,
                     selected = NULL)),
    mainPanel(
      tabsetPanel(
        tabPanel("Informacija apie įmonę", tableOutput("lentele")),
        tabPanel("Vidutinis įmonės atlyginimas", plotOutput("grafikas1")),
        tabPanel("Įmonės mokesčiai", plotOutput("grafikas2")),
        tabPanel("Apdrausti darbuotojai", plotOutput("grafikas3"))
      )
    )
  )
)
server = function(input, output, session) 
{
  data = read.csv("../data/lab_sodra.csv")
  FilteredData = data%>%
    filter(ecoActCode == "412000")%>%
    mutate(year = as.integer(substr(month, 0 ,4)))%>%
    mutate(month_value = as.integer(substr(month, 5 ,7)))

  FilteredData = FilteredData[, -7]
  
  updateSelectizeInput(session, "imones_pavadinimas", choices = FilteredData$name, server = TRUE)
  
  output$lentele = renderTable(
    FilteredData %>%
      filter(name == input$imones_pavadinimas), digits = 0
  )
  output$grafikas1 = renderPlot(
    FilteredData%>%
      filter(name == input$imones_pavadinimas)%>%
      ggplot(aes(x = month_value, y = avgWage)) +
      theme_minimal()+
      geom_point()+
      scale_x_continuous("Month", breaks = 1:12, limits = c(1,12)) +
      geom_line(col = "red") +
      ylab("Average wage") 
  )
  
  output$grafikas2 = renderPlot(
    FilteredData%>%
      filter(name == input$imones_pavadinimas)%>%
      ggplot(aes(x = month_value, y = tax)) +
      theme_minimal()+
      geom_point()+      
      scale_x_continuous("Month", breaks = 1:12, limits = c(1,12)) +
      geom_line(col = "green") +
      ylab("Tax")  
  )
  
  output$grafikas3 = renderPlot(
    FilteredData%>%
      filter(name == input$imones_pavadinimas)%>%
      ggplot(aes(x = month_value, y = numInsured)) +
      theme_minimal()+
      geom_point()+
      scale_x_continuous("Month", breaks = 1:12, limits = c(1,12)) +
      geom_line(col = "blue") +
      ylab("Number of insured employees")
  )
}
shinyApp(ui, server)