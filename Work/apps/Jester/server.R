### Recommenderlab Example with Shiny by Michael Hahsler
###
### This work is licensed under the Creative Commons Attribution 4.0
### International License (http://creativecommons.org/licenses/by/4.0/).
### For questions please contact Michael Hahsler at http://michael.hahsler.net.

### How to:
# 1. Start/open a Shiny project in R Studio
# 2. Edit ui.R and server.R and test (using Run App button)
# 3. Deploy:
#     a) Create a shinyapps.io account (https://www.shinyapps.io/)
#     b) Go to account and create a token and set account using package rsconnect
#     c) deploy using: library(rsconnect); deployApp()

library(shiny)
library(recommenderlab)

### global variable
num_to_rate <- 3

shinyServer(
  function(input, output) {

    ## load data
    data("Jester5k")

    ## pick random jokes and display
    rand_jokes <- reactive({
      ignore <- input$new_jokes  ### listen to button

      rand_jokes <- sample(length(JesterJokes), num_to_rate)
      for(i in 1:num_to_rate) {
        output[[paste0("joke", i)]] <- renderText(JesterJokes[rand_jokes[i]])
      }

      rand_jokes
    })

    ### create and change recommender
    recom <- reactive({
      Recommender(Jester5k, method = input$select_algo)
    })

    ### make recommendations
    output$joke_recom <- renderTable({

      ### read ratings
      ratings <- matrix(NA, nrow = 1, ncol = ncol(Jester5k))
      for(i in 1:num_to_rate)
        ratings[1, rand_jokes()[i]] <- input[[paste0("slider", i)]]

      ### create recommendations
      pred <- predict(recom(), as(ratings, "realRatingMatrix"),
        n = input$num_recoms)

      cbind('Recommended Joke' = JesterJokes[as(pred, "list")[[1]]],
        'Predicted Rating' = sprintf("%1.1f", pred@ratings[[1]]))
    })
  }
)
