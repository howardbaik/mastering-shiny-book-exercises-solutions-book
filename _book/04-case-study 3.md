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

IF you flip `fct_infreq()` and `fct_lump()`, than you order the output by the sum of the weight of each diagnosis.


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
