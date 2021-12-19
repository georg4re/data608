#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(lubridate)
library(shinydashboard)
library(shinyWidgets)
library(dashboardthemes)

covid_data <- read_csv(here('data', 
                            'processed_data',
                            'cases_data_plus_mandates-2.csv'), 
                       col_types = list(
                           X1 = col_double(),
                           X = col_double(),
                           state = col_character(),
                           cases = col_double(),
                           deaths = col_double(),
                           new_cases = col_double(),
                           new_deaths = col_double(),
                           mask_mandate = col_character(),
                           date = col_date(format = ""),
                           fine_enforced = col_logical(),
                           charge_enforced = col_logical(),
                           not_enforced = col_logical(),
                           mandate_start = col_date(format = ""),
                           mandate_start_2 = col_date(format = ""),
                           mandate_end = col_date(format = ""),
                           mandate_end_2 = col_date(format = "")
                       )) 

covid_data <- covid_data %>% filter(new_cases > 0) %>%
    mutate(weekday = weekdays(date)) %>%
    filter(weekday == "Tuesday")

states <- covid_data %>% group_by(state) %>% select(state)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Covid Mask Mandate Tracker"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectizeInput('var1', 'Select variable 1', choices = c("choose" = "", levels(tib$var_one)))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
