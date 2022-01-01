# Basic reactivity




## 3.3.6 Exercises {-}

1.

::: {.rmdnote}
Server 1

- `input$greeting` --> `output$greeting`
- Inside `renderText`, `name` --> `input$name` 
- Fixed code:


```r
server1 <- function(input, output, server) {
  output$greeting <- renderText(paste0("Hello ", input$name))
}
```
:::


::: {.rmdnote}
Server 2

- Make `greeting` a reactive: `greeting <- reactive(paste0("Hello ", input$name))`
- Since `greeting` is now a reactive, add parenthesis around it: `output$greeting <- renderText(greeting())`
- Fixed code:


```r
server2 <- function(input, output, server) {
  greeting <- reactive(paste0("Hello ", input$name))
  output$greeting <- renderText(greeting())
}
```
:::


::: {.rmdnote}
Server 3 

- Spelling error: `output$greting` --> `output$greeting`
- Missing `renderText()`
- Fixed code: 


```r
server3 <- function(input, output, server) {
  output$greeting <- renderText(paste0("Hello ", input$name))
}
```
:::



2. Solution at [Mastering Shiny Solutions 2021](https://mastering-shiny-solutions.org/basic-reactivity.html#solution-15)


3. When you use `range()` or `var()`, other readers won't know if you are using a reactive or the built-in R function.

::: {.rmdwarning}
Not sure why code fails, but maybe reading the chapter on [Tidy evaluation](https://mastering-shiny.org/action-tidy.html#action-tidy) will help.
:::
