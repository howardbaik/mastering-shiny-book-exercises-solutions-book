# Uploads and downloads




## 9.4 Exercises {-}

2. 

```r
library(shiny)
# Increase max limit of size of uploaded file
options(shiny.maxRequestSize = 10 * 1024^2)

ui <- fluidPage(
  # upload a csv file
  fileInput("upload", NULL, 
            buttonLabel = "Upload CSV", accept = ".csv"),
  # select a variable
  selectInput("var", "Select a variable", choices = NULL),
  # show output of t.test()
  verbatimTextOutput("t_test")
)

server <- function(input, output, session) {
  # uploaded dataset
  data <- reactive({
    req(input$upload)
    readr::read_csv(input$upload$datapath)
  })
  # once user uploads data, fill in the available variables
  observeEvent(data(), {
    choices <- unique(colnames(data()))
    updateSelectInput(inputId = "var", choices = choices) 
  })
  # show output of t-test
  output$t_test <- renderPrint({ 
    req(input$var)
    t.test(data()[[input$var]], mu = 0) 
  })
}

shinyApp(ui, server)
```


3. 


```r
library(shiny)
library(tidyverse)

ui <- fluidPage(
  # upload a csv file
  fileInput("upload", NULL, 
            buttonLabel = "Upload CSV", accept = ".csv"),
  # select a variable
  selectInput("var", "Select a variable", choices = NULL),
  # show histogram
  plotOutput("plot"),
  radioButtons("ext", "Save As:",
               choices = c("png", "pdf", "svg"), inline = TRUE),
  # download histogram
  downloadButton("download")
)

server <- function(input, output, session) {
  # uploaded dataset
  data <- reactive({
    req(input$upload)
    read_csv(input$upload$datapath)
  })
  # once user uploads data, fill in the available variables
  observeEvent(data(), {
    choices <- unique(colnames(data()))
    updateSelectInput(inputId = "var", choices = choices) 
  })
  # create reactive plot 
  plot_output <- reactive({
    req(input$var)
    ggplot(data()) +
      geom_histogram(aes(.data[[input$var]]))
  })
  # show histogram
  output$plot <- renderPlot({
    req(input$var)
    plot_output()
  })
  # download 
  output$download <- downloadHandler(
    filename = function() {
      paste("histogram", input$ext, sep = ".")
    }, 
    content = function(file) {
      ggsave(file, plot_output(), device = input$ext)
    }
  )
}

shinyApp(ui, server)
```



4. From [Mastering Shiny Solutions 2021](https://mastering-shiny-solutions.org/uploads-and-downloads.html#exercise-9.4.4):

```r
library(shiny)
library(brickr)
library(png)

# Function to provide user feedback (checkout Chapter 8 for more info).
notify <- function(msg, id = NULL) {
  showNotification(msg, id = id, duration = NULL, closeButton = FALSE)
}

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        fileInput("myFile", "Upload a PNG file", accept = c('image/png')),
        sliderInput("size", "Select size:", min = 1, max = 100, value = 35),
        radioButtons("color", "Select color palette:", choices = c("universal", "generic"))
      )
    ),
    mainPanel(
      plotOutput("result"))
  )
)

server <- function(input, output) {
  
  imageFile <- reactive({
    if(!is.null(input$myFile))
      png::readPNG(input$myFile$datapath)
  })
  
  output$result <- renderPlot({
    req(imageFile())
    
    id <- notify("Transforming image...")
    on.exit(removeNotification(id), add = TRUE)
    
    imageFile() %>%
      image_to_mosaic(img_size = input$size, color_palette = input$color) %>%
      build_mosaic()
  })
}

shinyApp(ui, server)
```


::: {.rmdwarning}
5. 
Not sure, but I think that as is, `janitor::make_clean_names()` will not be re-run when `input$empty` changes since they are in different if statements.
:::
