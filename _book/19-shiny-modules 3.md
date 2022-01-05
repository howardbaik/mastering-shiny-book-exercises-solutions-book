# Shiny modules





## 19.2.6 Exercises {-}

1. It is good practice to put a module in its own file in the `R/` directory because of namespaces (“spaces” of “names” that are isolated from the rest of the app). Each module is an individual component in isolation from the other modules in the app. Namespacing makes it easier to understand how your app works because you can write, analyse, and test individual components in isolation. When you have the ui and server functions, you need to write a function that uses them to generate an app. See below for an example:


```r
# Example of a function that generates an app
histogramApp <- function() {
  ui <- fluidPage(
    histogramUI("hist1")
  )
  server <- function(input, output, session) {
    histogramServer("hist1")
  }
  shinyApp(ui, server)  
}

# Run app
histogramApp()
```


2. 

```r
histogramUI <- function(id) {
  tagList(
    selectInput("var", "Variable", choices = names(mtcars)),
    numericInput("bins", "bins", value = 10, min = 1),
    plotOutput("hist")
  )
}
```

It fails to wrap each existing ID in a call to NS(), so that (e.g.) "var" turns into NS(id, "var"). See below for fixed version:


```r
histogramUI <- function(id) {
  tagList(
    selectInput(NS(id, "var"), "Variable", choices = names(mtcars)),
    numericInput(NS(id, "bins"), "bins", value = 10, min = 1),
    plotOutput(NS(id, "hist"))
  )
}
```

3. 

```r
randomUI <- function(id) {
  tagList(
    textOutput(NS(id, "val")),
    actionButton(NS(id, "go"), "Go!")
  )
}
randomServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    rand <- eventReactive(input$go, sample(100, 1))
    output$val <- renderText(rand())
  })
}
```



```r
library(shiny)

randomApp <- function() {
  ui <- fluidPage(
    randomUI("rand1"),
    randomUI("rand2"),
    randomUI("rand3"),
    randomUI("rand4")
  )
  server <- function(input, output, session) {
    randomServer("rand1")
    randomServer("rand2")
    randomServer("rand3")
    randomServer("rand4")
  }
  shinyApp(ui, server)  
}

randomApp()
```

- We know that each module is independent because each returns a different random number when you click go.

- In the [Module UI Section](https://mastering-shiny.org/scaling-modules.html#module-ui), we learn that it’s the responsibility of the person calling the module UI to wrap the result in a layout function like column() or fluidRow() according to their needs. In our problem, we wrap the result in `fluidRow()` and `column()` to make the display more attractive.


```r
# module UI
randomUI <- function(id) {
  fluidRow(
    column(width = 1,
           textOutput(NS(id, "val"))),
    column(width = 11,
    actionButton(NS(id, "go"), "Go!"))
  )
}

# module server
randomServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    rand <- eventReactive(input$go, sample(100, 1))
    output$val <- renderText(rand())
  })
}
```



```r
library(shiny)

# generate app
randomApp <- function() {
  ui <- fluidPage(
    randomUI("rand1"),
    randomUI("rand2"),
    randomUI("rand3"),
    randomUI("rand4")
  )
  server <- function(input, output, session) {
    randomServer("rand1")
    randomServer("rand2")
    randomServer("rand3")
    randomServer("rand4")
  }
  shinyApp(ui, server)  
}

# run app
randomApp()
```



## 19.3.7 Exercises {-}


::: {.rmdwarning}
1. Not sure.
:::


2. 

```r
library(shiny)
library(tidyverse)

# Module: Upload dataset----
datasetInput <- function(id) {
  fileInput(NS(id, "upload"), "Upload a file")
}

datasetServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive({
      req(input$upload)
      read_csv(input$upload$datapath)
    })
  })
}

# Module: Select numeric variables---
numericVarSelectInput <- function(id) {
  selectInput(NS(id, "var"), "Variable", choices = NULL) 
}

find_vars <- function(data, filter) {
  names(data)[vapply(data, filter, logical(1))]
}

numericVarSelectServer <- function(id, data, filter = is.numeric) {
  moduleServer(id, function(input, output, session) {
    observeEvent(data(), {
      updateSelectInput(session, "var", choices = find_vars(data(), filter))
    })
    
    reactive(data()[[input$var]])
  })
}

# Module: Summary----
summaryOutput <- function(id) {
  tags$ul(
    tags$li("Min: ", textOutput(NS(id, "min"), inline = TRUE)),
    tags$li("Max: ", textOutput(NS(id, "max"), inline = TRUE)),
    tags$li("Missing: ", textOutput(NS(id, "n_na"), inline = TRUE))
  )
}

summaryServer <- function(id, var) {
  moduleServer(id, function(input, output, session) {
    rng <- reactive({
      req(var())
      range(var(), na.rm = TRUE)
    })

    output$min <- renderText(rng()[[1]])
    output$max <- renderText(rng()[[2]])
    output$n_na <- renderText(sum(is.na(var())))
  })
}

# Generate app---
summaryApp <- function() {
  ui <- fluidPage(
    sidebarLayout(
      sidebarPanel(
        datasetInput("data"),
        numericVarSelectInput("var"),
      ),
      mainPanel(
        summaryOutput("summary")    
      )
    )
  )
  
  server <- function(input, output, session) {
    data <- datasetServer("data")
    x <- numericVarSelectServer("var", data)
    summaryServer("summary", x)
  }
  shinyApp(ui, server)
} 


summaryApp()
```


3.

```r
library(shiny)

# Module UI---
ymdDateUI <- function(id, label) {
  label <- paste0(label, " (yyyy-mm-dd)")
  
  fluidRow(
    textInput(NS(id, "date"), label),
    textOutput(NS(id, "error"))
  )
}

# Module server---
ymdDateServer <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    # display a message if the entered value is not a valid date
    # NOTE: I changed the render function to renderPrint after getting a 
    # weird error message with renderText. See below SO question:
    # https://stackoverflow.com/questions/62814804/warning-error-in-cat-argument-1-type-list-cannot-be-handled-by-cat-no-s
    output$error <- renderPrint({
      # https://mastering-shiny.org/action-feedback.html?q=req()#req-and-validation
      req(input$date,cancelOutput = TRUE)
      
      date_mod <- strptime(input$date, "%Y-%m-%d")
      if (is.na(date_mod)) {
        print("Invalid date")
      } else {
        print(as.Date(date_mod))
      }
    })
  })
}

# Generate app---
ymdDateApp <- function() {
  ui <- fluidPage(
    ymdDateUI("date", "Date")
  )
  
  server <- function(input, output, session) {
    ymdDateServer("date")
  }
  shinyApp(ui, server)
} 

# Run app---
ymdDateApp()
```

