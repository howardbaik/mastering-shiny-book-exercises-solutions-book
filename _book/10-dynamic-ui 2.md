# Dynamic UI




## 10.1.6 Exercises {-}

1.

```r
library(shiny)

ui <- fluidPage(
  numericInput("year", "year", value = 2020),
  dateInput("date", "date")
)

server <- function(input, output, session) {
  # MSS 2021
  observeEvent(input$year, {
    req(input$year)
    date_range <- range(as.Date(paste0(input$year, "-01-01")),
                        as.Date(paste0(input$year, "-12-31")))
    updateDateInput(session, "date",
                    min = date_range[1], 
                    max = date_range[2]
    )
  }) 
}

shinyApp(ui, server)
```


2.

```r
library(shiny)
library(tidyverse)
library(openintro, warn.conflicts = FALSE)

# MSS 2021
states <- unique(county$state)

ui <- fluidPage(
  selectInput("state", "State", choices = states),
  selectInput("county", "County", choices = NULL)
)

server <- function(input, output, session) {
  observeEvent(input$state, {
    req(input$state)
    # pull out county names
    choices <- county %>% 
      filter(state == input$state) %>%
      pull(name) %>% 
      unique()
    
    updateSelectInput(inputId = "county", choices = choices)
  })
}

shinyApp(ui, server)
```


3.

```r
library(shiny)
library(gapminder)
continents <- unique(gapminder$continent)


ui <- fluidPage(
  # add "(All)" to the list of choices
  selectInput("continent", "Continent", choices = continents), 
  selectInput("country", "Country", choices = NULL),
  tableOutput("data")
)

server <- function(input, output, session) {
  observeEvent(input$continent, {
    req(input$continent)
    # pull out country names
    choices <- gapminder %>% 
      filter(continent == input$continent) %>%
      pull(country) %>% 
      unique()
    
    updateSelectInput(inputId = "country", choices = choices)
  })
  
  output$data <- renderTable({
    gapminder %>% 
      filter(continent == input$continent,
             country == input$country)
  })
  
}

shinyApp(ui, server)

```



4.

```r
library(shiny)
library(gapminder)
continents <- unique(gapminder$continent)


ui <- fluidPage(
  # add "(All)" to the list of choices
  selectInput("continent", "Continent", choices = c(as.character(continents), "(All)")), 
  selectInput("country", "Country", choices = NULL),
  tableOutput("data")
)

server <- function(input, output, session) {
  observeEvent(input$continent, {
    req(input$continent)
    
    if (input$continent == "(All)") {
      # pull out country names
      choices <- gapminder %>% 
        pull(country) %>% 
        unique()
      
      updateSelectInput(inputId = "country", choices = choices)
      
    } else {    
      # pull out country names
      choices <- gapminder %>% 
        filter(continent == input$continent) %>%
        pull(country) %>% 
        unique()
      
      updateSelectInput(inputId = "country", choices = choices)
    }
  })
  
  output$data <- renderTable({
    if (input$continent == "(All)") {
      gapminder %>% 
        filter(country == input$country)
    } else {    
      gapminder %>% 
        filter(continent == input$continent,
               country == input$country)
    }
  })
}

shinyApp(ui, server)
```


5.

```r
library(shiny)

u <- shinyUI(fluidPage(
  titlePanel("Mutually Dependent Input Values"),
  sidebarLayout(
    sidebarPanel(
      numericInput("A", "A",.333),
      numericInput("B", "B",.333),
      numericInput("C", "C",.333)
    ),
    mainPanel(
      verbatimTextOutput("result")
    )
  )
)) 

s <- shinyServer(function(input, output,session) {
  
  observeEvent(input$A,{
    newB <- 1 - input$A - input$C 
    updateNumericInput(session, "B", value = newB) 
    newC <- 1 - input$A - input$B 
    updateNumericInput(session, "C", value = newC) 
  })
  observeEvent(input$B,{
    newC <- 1 - input$B - input$A 
    updateNumericInput(session, "C", value = newC) 
    newA <- 1 - input$B - input$C 
    updateNumericInput(session, "A", value = newA) 
  })
  observeEvent(input$C,{
    newA <- 1 - input$C - input$B 
    updateNumericInput(session, "A", value = newA) 
    newB <- 1 - input$C - input$C 
    updateNumericInput(session, "B", value = newB) 
  })
  
  
})

shinyApp(u,s)
```

Circular reference seems to be the issue. Once you run this app, the numeric inputs continue to update autonomously.



## 10.1.6 Exercises {-}

2. 

```r
library(shiny)
library(tidyverse)

# Put the unique user interface for each geom in its own tabPanel(), 
# and then arrange the three tabs into a tabsetPanel()
parameter_tabs <- tabsetPanel(
  id = "params",
  type = "hidden",
  tabPanel("geom_histogram",
           numericInput("binwidth_hist", "binwidth", value = 0.2)
  ),
  tabPanel("geom_freqpoly", 
           numericInput("binwidth_freq", "binwidth", value = 0.2)
  ),
  tabPanel("geom_density",
           numericInput("bw_density", "bandwidth", value = 1),
  )
)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("geom", "Select geom", 
                  choices = c("geom_histogram",
                              "geom_freqpoly",
                              "geom_density")
      ),
      parameter_tabs
      
    ),
    mainPanel(
      plotOutput("final_plot")
    )
  )
)

server <- function(input, output, session) {
  # Change tabs depending on geom
  observeEvent(input$geom, {
    updateTabsetPanel(inputId = "params", selected = input$geom)
  }) 
  
  # Reactive plot
  final_plot <- reactive({
    switch(input$geom,
           geom_histogram = ggplot(diamonds, aes(carat)) + geom_histogram(binwidth = input$binwidth_hist),
           geom_freqpoly = ggplot(diamonds, aes(carat)) + geom_freqpoly(binwidth = input$binwidth_freq),
           geom_density = ggplot(diamonds, aes(carat)) + geom_density(bw = input$bw_density),
    )
  })
  
  # Plot
  output$final_plot <- renderPlot(final_plot(), res = 96)
}

shinyApp(ui, server)
```


3. Not sure about this question, but I thought of using `checkboxInput()`



## 10.3.5 Exercises {-}

1. 

```r
library(shiny)

parameter_tabs <- tabsetPanel(
  id = "params",
  type = "hidden",
  tabPanel("slider",
           sliderInput("n", "n", value = 0, min = 0, max = 100)
  ),
  tabPanel("numeric",
           numericInput("n", "n", value = 0, min = 0, max = 100)
  )
)

ui <- fluidPage(
  selectInput("type", "type", c("slider", "numeric")),
  parameter_tabs
)
server <- function(input, output, session) {
  # Change tabs depending on type
  observeEvent(input$type, {
    updateTabsetPanel(inputId = "params", selected = input$type)
  }) 
}

shinyApp(ui, server)
```



2.

```r
library(shiny)

ui <- fluidPage(
  actionButton("go", "Enter password"),
  textOutput("text")
)
server <- function(input, output, session) {
  observeEvent(input$go, {
    showModal(modalDialog(
      passwordInput("password", NULL),
      title = "Please enter your password"
    ))
  })
  
  output$text <- renderText({
    if (!isTruthy(input$password)) {
      "No password"
    } else {
      "Password entered"
    }
  })
}

shinyApp(ui, server)
```

This app has an action button titled "Enter password". Once we click on the button, we are shown a dialog box where we can enter our password. After we enter our password, we see a new message: "Password entered". When you click the enter password button a second time, we make the `input$password` NULL again, making the password disappear.

3. You lose the currently selected value. It ensures that we don’t create a reactive dependency that would cause this code to re-run every time `input$dynamic` changes (which will happen whenever the user modifies the value). We only want it to change when `input$type` or `input$label` changes.

4. Solution at [MSS 2021](https://mastering-shiny-solutions.org/dynamic-ui.html#exercise-10.3.4.3)
