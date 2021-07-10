library(tidyverse)
library(shiny)

# 7/10/2021
# currently rb_data app
# focus on rushes only

rb_names <- setNames(rb_data_rushes$rusher_id, rb_data_rushes$rusher)

ui <- fluidPage(
  fluidRow(
    column(6,
           selectInput("player", "RB", choices = rb_names))
  ),
  fluidRow(
    column(4, tableOutput("attempts")),
    column(4, tableOutput("yards")),
    column(4, tableOutput("tds")),
    column(4, tableOutput("run_locations"))
  )
)

server <- function(input, output, session) {
  selected <- reactive(rb_data_rushes %>% filter(rusher_id == input$player))

  output$attempts <- renderTable(
    selected() %>% count(rusher, wt = rush_attempt, sort = TRUE)
  )
  output$yards <- renderTable(
    selected() %>% count(rusher, wt = rushing_yards, sort = TRUE)
  )
  output$tds <- renderTable(
    selected() %>% count(rusher, wt = rush_touchdown, sort = TRUE)
  )
  output$run_locations <- renderTable(
    selected() %>% count(run_location, wt = rush_attempt, sort = TRUE)
  )
}

# team_names <- setNames(rb_data$posteam, rb_data$posteam)
# 
# ui <- fluidPage(
#   fluidRow(
#     column(6,
#            selectInput("team", "possession team", choices = team_names))
#   ),
#   fluidRow(
#     column(4, tableOutput("attempts")),
#     column(4, tableOutput("yards")),
#     column(4, tableOutput("tds")),
#     column(4, tableOutput("run_locations"))
#   )
# )
# 
# server <- function(input, output, session) {
#   selected <- reactive(rb_data %>% filter(posteam == input$team))
#   
#   output$attempts <- renderTable(
#     selected() %>% count(rusher, wt = rush_attempt, sort = TRUE)
#   )
#   output$yards <- renderTable(
#     selected() %>% count(rusher, wt = rushing_yards, sort = TRUE)
#   )
#   output$tds <- renderTable(
#     selected() %>% count(rusher, wt = rush_touchdown, sort = TRUE)
#   )
#   output$run_locations <- renderTable(
#     selected() %>% count(run_location, wt = rush_attempt, sort = TRUE)
#   )
# }

shinyApp(ui = ui, server = server)