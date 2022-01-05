# Bookmarking



## 11.3 Exercises {-}

1.

```r
library(ambient)
simplex <- noise_simplex(c(500, 500), pertubation = 'normal', 
                         pertubation_amplitude = 40)
plot(as.raster(normalise(simplex)))
```


```r
library(shiny)
library(ambient)

ui <- function(request) {
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        sliderInput("freq", "frequency", value = 1, min = -2, max = 2, step = 0.01),
        selectInput("fractal", "fractal", choices = c("none", "fbm", "billow", "rigid-multi"), selected = "fbm"),
        sliderInput("lac", "lacunarity", value = 2, min = 0, max = 5, step = 0.001),
        sliderInput("gain", "gain", value = 0.5, min = 0, max = 1, step = 0.001),
        bookmarkButton()
      ),
      mainPanel(
        plotOutput("fig")
      )
    )
  )
}

server <- function(input, output, session) {
  
  simplex <- reactive({ 
    noise_simplex(dim = c(100, 100),
                  frequency = input$freq,
                  fractal = input$fractal,
                  lacunarity = input$lac,
                  gain = input$gain)
  })
  
  output$fig <- renderPlot({
    plot(as.raster(normalise(simplex())))
  }, res = 96)
}

shinyApp(ui, server, enableBookmarking = "url")
```



2. 

```r
library(shiny)

ui <- function(request) {
  fluidPage(
    fileInput("upload", "Upload CSV file", accept = ".csv", multiple = TRUE),
    bookmarkButton()
  )
}

server <- function(input, output, session) {
  
  output$head <- renderTable({
    head(data(), input$n)
  })
}

shinyApp(ui, server, enableBookmarking = "server")
```

`readRDS("shiny_bookmarks/cf6669ac8bfa4888/input.rds")` gives me a list with one dataframe, upload, with the name, size, type, and datapath of the uploaded datasets. Also, the uploaded datasets are saved inside shiny_bookmarks as `0.csv` and `1.csv`.


