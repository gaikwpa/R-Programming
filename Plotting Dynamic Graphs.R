
library(DT)
library(shiny)
library(tidyverse)

chartCol <- function(df, colN) {
  if (is.numeric(df[[colN]])) {
    ggplot(df, aes_string(x = colN)) + 
      geom_histogram()+
      theme(axis.text.x = element_text(angle = 50, hjust = 1.5))
  }
  
  else{
    ggplot(df, aes_string(x = colN)) + 
      geom_bar()+
      theme(axis.text.x = element_text(angle = 50, hjust = 1.5))
      
  }
}

ui <- fluidPage(titlePanel("Potting Dynamic Graphs"),
                sidebarLayout(
                  sidebarPanel(
                    width = 2,
                    selectInput(
                      width = "100%",
                      inputId = "dbList1",
                      label = "Default Dataset List",
                      choices = c(choose = "List of data frame...",
                                  "mpg", "diamonds", "msleep"),
                      selectize = FALSE
                    ),
                    uiOutput("obs1"),
                    actionButton(
                      inputId = "reset",
                      label = "Reset Data",
                      icon = icon("refresh"),
                      width = "100%"
                    ),
                    verbatimTextOutput("aaa")
                  ),
                  mainPanel(fluidPage(fluidRow(
                    column(6,
                           DT::dataTableOutput("dataSet")),
                    column(6,
                           plotOutput(
                             "plotChart",
                             width = "100%",
                             height = "300px"
                           ))
                  )))
                ))



server <- function(input, output) {
  values <- reactiveValues(tbl = NULL,
                           obsList = NULL,
                           plot.df = NULL)
  
  observeEvent(input$dbList1, {
    if (!NA %in% match(input$dbList1, c("mpg", "diamonds", "msleep"))) {
      values$tbl <- as.data.frame(get(input$dbList1))
      values$obsList <- colnames(values$tbl)
      
      output$obs1 <- renderUI({
        selectInput(
          inputId = "observationInput1",
          label = "1st observation",
          choices =  values$obsList
        )
      })
    }
  })
  
  observeEvent(input$observationInput1, {
    values$plot.df <-
      as.data.frame(values$tbl[, input$observationInput1])
    colnames(values$plot.df) <- input$observationInput1
    output$dataSet <- DT::renderDataTable({
      values$tbl
    },
    extensions = c('Scroller', 'FixedColumns'),
    options = list(
      deferRender = TRUE,
      scrollX = TRUE,
      scrollY = 200,
      scroller = TRUE,
      dom = 'Bfrtip',
      fixedColumns = TRUE
    ))
  })
  
  observe({
    output$plotChart <- renderPlot({
      shiny::validate(need(values$tbl, ""))
      chartCol(values$plot.df, colnames(values$plot.df))
    })
  })
  
  observeEvent(input$reset, {
    values$tbl <- NULL
    output$obs1 <- NULL
  })
  
  output$aaa <- renderPrint({
    values$obs1
  })
  
}
shinyApp(ui=ui, server=server)
