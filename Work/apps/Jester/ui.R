### Recommenderlab Example with Shiny by Michael Hahsler
###
### This work is licensed under the Creative Commons Attribution 4.0
### International License (http://creativecommons.org/licenses/by/4.0/).
### For questions please contact Michael Hahsler at http://michael.hahsler.net.

library(shiny)

### global variable
num_to_rate <- 3

shinyUI(
  fluidPage(

    titlePanel("Joke Recommender Using Jester Data"),

    sidebarLayout(position = "left",
      sidebarPanel(
        h3("Rate the following jokes:"),

        ### create the sliders
        lapply(1:num_to_rate, function(i) sliderInput(paste0("slider", i),
          label = textOutput(paste0("joke", i)),
          min = -10, max = 10, value = 0)),

        selectInput("select_algo", label = p("Select recommender algorithm"),
          choices = list("POPULAR", "UBCF", "IBCF", "SVD", "RANDOM"),
          selected = "UBCF"),

        numericInput("num_recoms",
          label = p("Number of recommended jokes"),
          value = 3, min = 1, max =10, step = 1),

        actionButton("new_jokes", "Change jokes to rate")
      ),

      mainPanel(tableOutput("joke_recom"),
        p(
          a("recommenderlab",
            href="https://cran.r-project.org/package=recommenderlab"),
          "example by",
          a("Michael Hahsler", href="http://michael.hahsler.net")
        )
      )
    )
  )
)
