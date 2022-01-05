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


::: {.rmdwarning}
2. Not sure.
:::


## 15.2.3 Exercises {-}


::: {.rmdwarning}
1. Not sure.
:::



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
- Not sure about what happens when I use `cancelOutput` argument, but the documentation states: 


::: {.rmdnote}
When `req(..., cancelOutput = TRUE)` is used, the "silent" exception is also raised, but it is treated slightly differently if one or more outputs are currently being evaluated. In those cases, the reactive chain does not proceed or update, but the output(s) are left is whatever state they happen to be in (whatever was their last valid state).
:::




## 15.4.3 Exercises {-}

1.

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

::: {.rmdwarning}
1. Not sure.

2. Not sure.
:::
