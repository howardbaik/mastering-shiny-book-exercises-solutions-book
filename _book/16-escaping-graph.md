# Escaping the graph




## 16.3.4 Exercises {-}

1.

```r
library(shiny)

ui <- fluidPage(
  actionButton("rnorm", "Normal"),
  actionButton("runif", "Uniform"),
  plotOutput("plot")
)

server <- function(input, output, session) {
  r <- reactiveValues(random_data = vector(mode = "numeric", length = 100))
  observeEvent(input$rnorm, {
    r$random_data <- rnorm(100)
  })
  observeEvent(input$runif, {
    r$random_data <- runif(100)
  })
  output$plot <- renderPlot({
    # Only show plot if input$rnorm 
    # OR input$runif is provided
    req(input$rnorm | input$runif)
    hist(r$random_data)
  })
}

shinyApp(ui, server)
```


2.

```r
library(shiny)

ui <- fluidPage(
  selectInput("type", "type", c("Normal", "Uniform")),
  actionButton("go", "go"),
  plotOutput("plot")
)

server <- function(input, output, session) {
  r <- reactiveValues(random_data = vector(mode = "numeric", length = 100))
  observeEvent(input$go, {
    if (input$type == "Normal") {
      r$random_data <- rnorm(100)
    } else {
      r$random_data <- runif(100)
    }
  })
  output$plot <- renderPlot({
    # Only show plot if "go" is clicked
    req(input$go)
    hist(r$random_data)
  })
}

shinyApp(ui, server)
```


3. 

```r
library(shiny)

ui <- fluidPage(
  selectInput("type", "type", c("Normal", "Uniform")),
  actionButton("go", "go"),
  plotOutput("plot")
)

server <- function(input, output, session) {
  r <- reactive({ 
    if (input$type == "Normal") {
      rnorm(100)
    } else if (input$type == "Uniform") {
      runif(100)
    }
  })
  output$plot <- renderPlot({
    req(input$go)
    hist(r())
  })
  
}

shinyApp(ui, server)
```


- You can do that for the second UI but not the first because the second UI has the `go` actionButton.

