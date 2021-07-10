library(tidyverse)
library(plyr)
library(shiny)
library(shinyWidgets)

# full weekly with positions 2020
# select posteams first

posteam_options <- setNames(unique(full_weekly_with_positions_2020$posteam), 
                            unique(full_weekly_with_positions_2020$posteam))

defteam_options <- setNames(unique(full_weekly_with_positions_2020$defteam),
                            unique(full_weekly_with_positions_2020$defteam))

play_type_options <- setNames(unique(full_weekly_with_positions_2020$play_type),
                              unique(full_weekly_with_positions_2020$play_type))

condense_teams_func <- function(input_posteam) {
  # input_posteam are the selected teams for posteam
  test_reac_data_get <- full_weekly_with_positions_2020 %>% filter(posteam %in% input_posteam)
  
  # pseudocode
  # group key/value pairs of posteam->defteam, then check which defteam in all posteam
  
  s <- as.data.frame(test_reac_data_get) %>% # convert to dataframe
    select(posteam, defteam)# widdle down to the two columns i need for this
  s <- split(s, s$posteam) # split into a list of tibbles for posteam/defteam (games)
  s <- (lapply(s, function(x) crossing(x[, c("posteam", "defteam")]))) # show only one combination of each pos/def teams
  
  j <- join_all(s, by = "defteam") %>% # put back together based on defteam appearances
    drop_na() # then drop if there are any NA's, meaning a team didn't play that defteam
  return(j$defteam)
}

ui <- fluidPage(
  fluidRow(
    column(6,
           pickerInput("posteam", "possession team", choices = posteam_options, 
                       options = list(`actions-box` = TRUE), multiple = TRUE),
           pickerInput("defteam", "defensive team", choices = defteam_options, 
                       options = list(`actions-box` = TRUE), multiple = TRUE),
           pickerInput("playtype", "play type", choices = play_type_options,
                       options = list(`actions-box` = TRUE), multiple = TRUE)
           )
  ),
  # UI display of the table
  fluidRow(
    column(4, dataTableOutput("dynamicOutput")),
  )
)

server <- function(input, output, session) {
  
  # This is the data that will actually be displayed
  selected <- reactive(full_weekly_with_positions_2020 %>% 
                         filter(posteam %in% input$posteam) %>%
                         filter(defteam %in% input$defteam) %>%
                         filter(play_type %in% input$playtype))
  
  # Identify how posteam choice will affect defteam choices
  reac_data_get <- reactive(full_weekly_with_positions_2020 %>% 
                              filter(posteam %in% input$posteam))
  
  # Update defteam choices appropriately, currently shows options if it matches ANY posteam
  observeEvent(input$posteam, {
    updatePickerInput(session, "defteam", "defensive team", 
                      #choices = sort(unique(as.character(reac_data_get()$defteam))))
                      choices = condense_teams_func(input_posteam = input$posteam))
  })

  # Display the table
  output$dynamicOutput <- renderDataTable({
    if((
      is.null(input$posteam) || is.null(input$defteam) || is.null(input$playtype))
      ) {
    } else {
    selected()
    }
  })
}

shinyApp(ui = ui, server = server)