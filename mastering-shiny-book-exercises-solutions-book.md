--- 
title: "Mastering Shiny Solutions"
author: "Howard Baek"
date: "2021-12-29"
site: bookdown::bookdown_site
documentclass: book
url: https://mastering-shiny-solutions.netlify.app/
cover-image: "img/mastering-shiny-cover.png"
bibliography:
- book.bib
- packages.bib
description: |
  Solutions manual to the exercises in Hadley Wickham's Mastering Shiny.
biblio-style: apalike
csl: chicago-fullnote-bibliography.csl
---

# Welcome {-}

<img src="img/mastering-shiny-cover.png" class="cover" width="250" height="328"/>This is the WIP website for Mastering Shiny Solutions, a solution manual to the exercises in [Mastering Shiny](https://mastering-shiny.org/), written by Hadley Wickham. 

[Mastering Shiny Solutions 2021](https://mastering-shiny-solutions.org/), by Maya Gans and Marly Gotti, was released in early 2021. Since then, there have been various changes to the exercises in Mastering Shiny, and this book serves as an _updated version_ of Mastering Shiny Solutions 2021. A few solutions in this book defer to those provided in Mastering Shiny Solutions 2021. Also, some exercises don't contain solutions, and for these exercises, the author writes, "Not sure".

If my work has helped you, [you can buy me a coffee on Ko-fi!](https://ko-fi.com/howardbaek)


## About the Author {-}

[_Howard Baek_](http://insidethetv.rbind.io/) is a Master’s student in Biostatistics at the University of Washington. His past experiences include a [Machine Learning Internship](https://github.com/orgs/UW-Upwelling-Project/teams/seattle) at Northwest Fisheries Science Center (NWFSC), where he created an algorithm in R to detect an oceanographic process, [NIH-funded Research Assistantship](https://github.com/howardbaek/addiction-dashboard-simple) at the Behavioral Research In Technology and Engineering (BRiTE) Center, where he developed a Shiny Dashboard that allows patients and clinicians in addiction treatment to monitor patients’ progress and goals over time, and an [Educational Data Mining Research Internship](https://github.com/howardbaek/mooc-project-github) at George Mason University, where he analyzed real world datasets from a Stanford online course and created a Shiny Dashboard for instructors to interact with the dataset.


## Acknowledgments {-}

Alison Hill and Desirée De Leon’s talk, [Sharing on Short Notice](https://youtu.be/QcE4RBH2auQ?t=1881), made getting this book online smooth and easy.

<!--chapter:end:index.Rmd-->

# (PART) Getting started {-} 

# Your first Shiny app

## 1.8 Exercises {-}




1. 

```r
library(shiny)

ui <- fluidPage(
    textInput("name", "What's your name?"),
    textOutput("greeting")
)

server <- function(input, output, session) {
    output$greeting <- renderText({
        paste0("Hello ", input$name)
    })
}

shinyApp(ui, server)
```


2.

```r
library(shiny)

ui <- fluidPage(
    sliderInput("x", label = "If x is", min = 1, max = 50, value = 30),
    "then x times 5 is",
    textOutput("product")
)

server <- function(input, output, session) {
    output$product <- renderText({ 
      # Fixed error
       input$x * 5
      # by adding input$ 
    })
}

shinyApp(ui, server)
```


3.

```r
library(shiny)

ui <- fluidPage(
    sliderInput("x", label = "If x is", min = 1, max = 50, value = 30),
    sliderInput("y", label = "and y is", min = 1, max = 50, value = 30),
    "then x times y is",
    textOutput("product")
)

server <- function(input, output, session) {
    output$product <- renderText({ 
       input$x * input$y
    })
}

shinyApp(ui, server)
```


4. 

```r
library(shiny)

ui <- fluidPage(
    sliderInput("x", "If x is", min = 1, max = 50, value = 30),
    sliderInput("y", "and y is", min = 1, max = 50, value = 5),
    "then, (x * y) is", textOutput("product"),
    "and, (x * y) + 5 is", textOutput("product_plus5"),
    "and (x * y) + 10 is", textOutput("product_plus10")
)

server <- function(input, output, session) {
    # Add this reactive expression to reduce 
    # amount of duplicated code
    product <- reactive({
        input$x * input$y
    })
    output$product <- renderText({ 
        product()
    })
    output$product_plus5 <- renderText({ 
        product() + 5
    })
    output$product_plus10 <- renderText({ 
        product() + 10
    })
}

shinyApp(ui, server)
```

- What's new is the additional calculation where 5 and 10 were added to the product and the outputs rendered as text.


5.

```r
library(shiny)
library(ggplot2)

datasets <- c("economics", "faithfuld", "seals")

ui <- fluidPage(
    selectInput("dataset", "Dataset", choices = datasets),
    verbatimTextOutput("summary"),
    # 1st Bug: tableOutput -> plotOutput
    plotOutput("plot")
)

server <- function(input, output, session) {
    dataset <- reactive({
        get(input$dataset, "package:ggplot2")
    })
    # Fixed spelling
    output$summary <- renderPrint({
        summary(dataset())
    })
    output$plot <- renderPlot({
      # dataset -> dataset() because its a reactive
        plot(dataset())
    }, res = 96)
}

shinyApp(ui, server)
```


<!--chapter:end:01-first-app.Rmd-->

# Basic UI



## 2.2.8 Exercises {-}

1. You can fill in the parameter, `value`, inside `textInput()`: `textInput("name", value = "Your name)`


3. 

```r
library(shiny)

ui <- fluidPage(
    sliderInput(inputId = "user_input",
                label = "User Input", 
                value = 10,
                min = 0, max = 100,
                step = 5,
                # Added animation
                animate = animationOptions(
                    interval = 1000,
                    loop = TRUE,
                    playButton = NULL,
                    pauseButton = NULL
                )
    )
    
)

server <- function(input, output, session) {}

shinyApp(ui, server)
```


4. The documentation states, "It's also possible to group related inputs by providing a named list whose elements are (either named or unnamed) lists, vectors, or factors. In this case, the outermost names will be used as the group labels (leveraging the  `<optgroup>` HTML tag) for the elements in the respective sublist. See the example section for a small demo of this feature."


## 2.3.5 Exercises {-}

1. 

a) `renderPrint(summary(mtcars))` should be paired with `verbatimTextOutput` since it is console output.
b) `renderText("Good morning!")` should be paired with `textOutput` since it is regular text
c) `renderPrint(t.test(1:5, 2:6))` should be paired with `verbatimTextOutput` since it is console output.
d) `renderText(str(lm(mpg ~ wt, data = mtcars)))` should be paired with `verbatimTextOutput` since it is console output.

2. 

```r
library(shiny)

ui <- fluidPage(
    plotOutput("plot", width = "700px", height = "300px")
)

server <- function(input, output, session) {
    output$plot <- renderPlot(plot(1:5), res = 96, 
                              alt = "Scatterplot of 5 random numbers")
}

shinyApp(ui, server)
```



3. (Borrowed from https://mastering-shiny-solutions.org/basic-ui.html#exercise-3.3.5.3)

```r
library(shiny)

ui <- fluidPage(
    dataTableOutput("table")
)

server <- function(input, output, session) {
    output$table <- renderDataTable(mtcars, 
                                    options = list(pageLength = 5,
                                                   ordering = FALSE, 
                                                   searching = FALSE))
}

shinyApp(ui, server)
```


4.

```r
library(shiny)
library(reactable)

ui <- fluidPage(
  reactableOutput("table")
)

server <- function(input, output) {
  output$table <- renderReactable({
    reactable(mtcars)
  })
}

shinyApp(ui, server)

```

<!--chapter:end:02-basic-ui.Rmd-->

# Basic reactivity




## 3.3.6 Exercises {-}

1.


```r
server1 <- function(input, output, server) {
  input$greeting <- renderText(paste0("Hello ", name))
}
```

- `input$greeting` --> `output$greeting`
- Inside `renderText`, `name` --> `input$name` 
- Fixed code:  `output$greeting <- renderText(paste0("Hello ", input$name))`



```r
server2 <- function(input, output, server) {
  greeting <- paste0("Hello ", input$name)
  output$greeting <- renderText(greeting)
}
```

- You can make `greeting` a reactive by adding `reactive()`: `greeting <- reactive(paste0("Hello ", input$name))`
- Since `greeting` is now a reactive, you need to add parenthesis around it: `output$greeting <- renderText(greeting())`



```r
server3 <- function(input, output, server) {
  output$greting <- paste0("Hello", input$name)
}
```

- Spelling error: `output$greting` --> `output$greeting`
- Missing `renderText()`
- Fixed code: `output$greeting <- renderText(paste0("Hello ", input$name))`

<br>

2. Solution at [Mastering Shiny Solutions 2021](https://mastering-shiny-solutions.org/basic-reactivity.html#solution-15)

<br>

3. Code will fail because of df[[input$var]]. When you use `range()` or `var()`, other readers won't know if you are using a reactive or the built-in R function. 

<!--chapter:end:03-basic-reactivity.Rmd-->

# Case study: ER injuries



## 4.8 Exercises {-}

1. Solution at [Mastering Shiny Solutions 2021](https://mastering-shiny-solutions.org/case-study-er-injuries.html#exercise-5.8.1) 

2. 

```r
injuries <- vroom::vroom("neiss/injuries.tsv.gz")
injuries

# Original code
injuries %>%
  mutate(diag = fct_lump(fct_infreq(diag), n = 5)) %>%
  group_by(diag) %>%
  summarise(n = as.integer(sum(weight)))

# Flipped code
injuries %>%
  mutate(diag = fct_infreq(fct_lump(diag, n = 5))) %>%
  group_by(diag) %>%
  summarise(n = as.integer(sum(weight)))
```

If you flip `fct_infreq()` and `fct_lump()`, than you order the output by the sum of the weight of each diagnosis.


3.

```r
library(dplyr)
library(ggplot2)
library(forcats)
library(vroom)
library(shiny)

if (!exists("injuries")) {
  injuries <- vroom::vroom("neiss/injuries.tsv.gz")
  products <- vroom::vroom("neiss/products.tsv")
  population <- vroom::vroom("neiss/population.tsv")
}

ui <- fluidPage(
  #<< first-row
  fluidRow(
    column(8,
           selectInput("code", "Product",
                       choices = setNames(products$prod_code, products$title),
                       width = "100%"
           )
    ),
    column(2, selectInput("y", "Y axis", c("rate", "count"))),
    # Input control that lets the user decide how many rows to show in the summary tables
    column(2, numericInput("num_rows", "Number of Rows", value = 5, min = 0, max = 6))
  ),
  #>>
  fluidRow(
    column(4, tableOutput("diag")),
    column(4, tableOutput("body_part")),
    column(4, tableOutput("location"))
  ),
  fluidRow(
    column(12, plotOutput("age_sex"))
  ),
  #<< narrative-ui
  fluidRow(
    column(2, actionButton("story", "Tell me a story")),
    column(10, textOutput("narrative"))
  )
  #>>
)

count_top <- function(df, var, n = 5) {
  df %>%
    mutate({{ var }} := fct_lump(fct_infreq({{ var }}), n = n)) %>%
    group_by({{ var }}) %>%
    summarise(n = as.integer(sum(weight)))
}

server <- function(input, output, session) {
  selected <- reactive(injuries %>% filter(prod_code == input$code))
  
  #<< tables
  output$diag <- renderTable(count_top(selected(), diag) %>% slice(1:input$num_rows), width = "100%")
  output$body_part <- renderTable(count_top(selected(), body_part) %>% slice(1:input$num_rows), width = "100%")
  output$location <- renderTable(count_top(selected(), location) %>% slice(1:input$num_rows), width = "100%")
  #>>
  
  summary <- reactive({
    selected() %>%
      count(age, sex, wt = weight) %>%
      left_join(population, by = c("age", "sex")) %>%
      mutate(rate = n / population * 1e4)
  })
  
  #<< plot
  output$age_sex <- renderPlot({
    if (input$y == "count") {
      summary() %>%
        ggplot(aes(age, n, colour = sex)) +
        geom_line() +
        labs(y = "Estimated number of injuries")
    } else {
      summary() %>%
        ggplot(aes(age, rate, colour = sex)) +
        geom_line(na.rm = TRUE) +
        labs(y = "Injuries per 10,000 people")
    }
  }, res = 96)
  #>>
  
  #<< narrative-server
  narrative_sample <- eventReactive(
    list(input$story, selected()),
    selected() %>% pull(narrative) %>% sample(1)
  )
  output$narrative <- renderText(narrative_sample())
  #>>
}

shinyApp(ui, server)
```


4. Solution at [Mastering Shiny Solutions 2021](https://mastering-shiny-solutions.org/case-study-er-injuries.html#exercise-5.8.4)

<!--chapter:end:04-case-study.Rmd-->

# (PART) Shiny in action {-} 

# Workflow

There are no exercises in this chapter.

<!--chapter:end:05-workflow.Rmd-->

# Layout, themes, HTML



## 6.2.4 Exercises {-}

1. Documentation on `sidebarLayout()`: "By default, the sidebar takes up 1/3 of the width, and the main panel 2/3". In other words, given the width is 12 columns, the sidebar is made up of 4 columns and the main panel 8 columns. 


```r
# Recreate sidebarLayout()
fluidRow(
  # sidebar (4 columns)
  column(4, 
         ...
  ),
  # # main panel (8 columns)
  column(8, 
         ...
  )
)
```



2. 

```r
library(shiny)

ui <- fluidPage(
  titlePanel("Central limit theorem"),
  sidebarLayout(
    sidebarPanel(
      numericInput("m", "Number of samples:", 2, min = 1, max = 100)
    ),
    mainPanel(
      plotOutput("hist")
    ),
    # Modified to put position of sidebar on the right
    position = "right"
  )
)
server <- function(input, output, session) {
  output$hist <- renderPlot({
    means <- replicate(1e4, mean(runif(input$m)))
    hist(means, breaks = 20)
  }, res = 96)
}


shinyApp(ui, server)
```



3.  Reference: https://shiny.rstudio.com/articles/layout-guide.html

```r
# UI ONLY
library(shiny)
library(ggplot2)

dataset <- diamonds

ui <- fluidPage(
  
  title = "Diamonds Explorer",
  
  fluidRow(
    column(6,
           # First plot taking up half the width
           plotOutput("plot1")
    ),
    
    column(6,
           # Second plot taking up half the width
           plotOutput("plot2")
    )
  ),
  # Horizontal Line
  hr(),
  
  fluidRow(
    column(3,
           h4("Diamonds Explorer"),
           sliderInput('sampleSize', 'Sample Size', 
                       min=1, max=nrow(dataset), value=min(1000, nrow(dataset)), 
                       step=500, round=0),
           br(),
           checkboxInput('jitter', 'Jitter'),
           checkboxInput('smooth', 'Smooth')
    ),
    column(4, offset = 1,
           selectInput('x', 'X', names(dataset)),
           selectInput('y', 'Y', names(dataset), names(dataset)[[2]]),
           selectInput('color', 'Color', c('None', names(dataset)))
    ),
    column(4,
           selectInput('facet_row', 'Facet Row', c(None='.', names(dataset))),
           selectInput('facet_col', 'Facet Column', c(None='.', names(dataset)))
    )
  )
)

shinyApp(ui, server)
```




<!--chapter:end:06-layout-theme-html.Rmd-->

# Graphics

There are no exercises in this chapter



<!--chapter:end:07-graphics.Rmd-->

# User feedback

There are no exercises in this chapter.

<!--chapter:end:08-user-feedback.Rmd-->

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


5. Not sure

<!--chapter:end:09-uploads-downloads.Rmd-->

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

4. Solution at [Mastering Shiny Solutions 2021](https://mastering-shiny-solutions.org/dynamic-ui.html#exercise-10.3.4.3)

<!--chapter:end:10-dynamic-ui.Rmd-->

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



<!--chapter:end:11-bookmarking.Rmd-->

# Tidy evaluation

There are no exercises in this chapter.

<!--chapter:end:12-tidy-eval.Rmd-->

# (PART) Mastering reactivity {-} 

# Why reactivity?

There are no exercises in this chapter.

<!--chapter:end:13-why-reactivity.Rmd-->

# The reactive graph

## 14.4.5 Exercises {-}

#### 1. {-}
<img src="img/reactive-graph.png" width="649" />

- Reactives are not run because there are no outputs. Server function only contains inputs and reactive expressions.


2. Not sure

3. When we start the session, `y` would not exist and thus `y()` would return an error since `y` is a reactive expression that consists of itself.

<!--chapter:end:14-reactive-graph.Rmd-->

# Reactive building blocks



## 15.1.1 Exercises {-}

1. `l1` is a reactive values class with values a and b, whereas `l2` is a list with a, a `reactiveVal` and b, a `reactiveVal`. 


```r
l1 <- reactiveValues(a = 1, b = 2)
l2 <- list(a = reactiveVal(1), b = reactiveVal(2))

# get reactive values in l1
l1$a
l1$b
# set reactive values in l1
l1$a <- 10
l1$b <- 20

# get reactive values in l2
l2$a()
l2$b()
# set reactive values in l2
l2$a(10) 
l2$b(20)
```

2. Not sure.



## 15.2.3 Exercises {-}


2.

```r
library(shiny)

ui <- fluidPage(
  checkboxInput("error", "error?"),
  textOutput("result")
)

server <- function(input, output, session) {
  a <- reactive({
    if (req(input$error, cancelOutput = TRUE)) {
      "Error!"
    } else {
      1
    }
  })
  b <- reactive(a() + 1)
  c <- reactive(b() + 1)
  output$result <- renderText(c())
}

shinyApp(ui, server)
```

- If I use `req()` and remove the `stop()`, I get an error message: `Error: non-numeric argument to binary operator`.
- Not sure about what happens when I use `cancelOutput` argument, but the documentation states: "When req(..., cancelOutput = TRUE) is used, the "silent" exception is also raised, but it is treated slightly differently if one or more outputs are currently being evaluated. In those cases, the reactive chain does not proceed or update, but the output(s) are left is whatever state they happen to be in (whatever was their last valid state)."


## 15.4.3 Exercises {-}


```r
library(shiny)

ui <- fluidPage(
  numericInput("x", "x", value = 50, min = 0, max = 100),
  actionButton("capture", "capture"),
  textOutput("out")
)

server <- function(input, output, session) {
  df <- eventReactive(input$capture, { 
    input$x
  })
  output$out <- renderText({ df() })
}

shinyApp(ui, server)
```


## 15.5.4 Exercises {-}

1. There is no output?



```r
library(shiny)

ui <- fluidPage(
  textInput("name", "name"),
  actionButton("add", "add"),
  actionButton("del", "delete"),
  textOutput("names")
)
server <- function(input, output, session) {
  r <- reactiveValues(names = character())
  observeEvent(input$add, {
    r$names <- union(r$names, input$name)
    updateTextInput(session, "name", value = "")
  })
  observeEvent(input$del, {
    r$names <- setdiff(r$names, input$name)
    updateTextInput(session, "name", value = "")
  })
  
  output$names <- renderText(r$names)
}

shinyApp(ui, server)
```


<!--chapter:end:15-reactive-blocks.Rmd-->

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


3. You can do that for the second UI but not the first because the second UI has the "go" actionButton.


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


<!--chapter:end:16-escaping-graph.Rmd-->

# (PART) Best practices {-} 

# General guidelines

There are no exercises in this chapter.

<!--chapter:end:17-general-guidelines.Rmd-->

# Functions

There are no exercises in this chapter.

<!--chapter:end:18-functions.Rmd-->

# Shiny modules

<!--chapter:end:19-shiny-modules.Rmd-->

# Packages

There are no exercises in this chapter.

<!--chapter:end:20-packages.Rmd-->

# Testing

There are no exercises in this chapter.

<!--chapter:end:21-testing.Rmd-->

# Security

There are no exercises in this chapter.

<!--chapter:end:22-security.Rmd-->

# Performance

There are no exercises in this chapter.

<!--chapter:end:23-performance.Rmd-->
