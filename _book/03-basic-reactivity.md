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
