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
library(readr)
library(here)
library(dplyr)
library(ggplot2)
library(scales)

covid_data <- read.csv(here('CovidTracker', 
                            'weekly_cases_data_plus_mandates-4.csv')) %>%
                           mutate(date=as.Date(date),
                                  mandate_start=as.Date(mandate_start),
                                  mandate_end=as.Date(mandate_end))


national_cases <- covid_data %>%
    group_by(date) %>%
    summarise(us_new_cases=sum(new_cases, na.rm = TRUE), 
              us_cases=sum(cases, na.rm = TRUE), 
              us_new_deaths=sum(new_deaths, na.rm = TRUE), 
              us_deaths = sum(deaths, na.rm = TRUE)) 
    
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Covid Mask Mandate Tracker"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectizeInput('state', 'State', choices = state.name),
            selectizeInput('data', 'Variable', choices=c("Cumulative Cases", 
                                                         "Cumulative Deaths", 
                                                         "New Cases", 
                                                         "New Deaths")),
            h3("Summary"),
            p("The tracker allows you to visualize the weekly covid cases around mask mandates."),
            h4("The Data"),
            p("We used “Data from The New York Times, based on reports from state and local health agencies.”"),
            p("For the Mask Mandate information and the population data, we used the data from https://statepolicies.com/data/library/"),
            h4("Methodology"),
            p("We 'predicted' the cases by using a rate of change formula.  The Initial intention was to use the SIR Model for the spread of disease but we had missing information around susceptibility and recovered individuals in our data set to make it work. "),
            p("For the table, we calculated the rate of change of the two weeks prior/after the mandate threshold."),
            img(src = "death_plot_animation.gif", 
                height = 70, width = 200),
            br(),
            "This is a product of ", 
            span("George Cruz Deschamps", style = "color:blue")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           tableOutput('table')
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        
        # generate bins based on input$bins from ui.R
        data <- reactive({
            if ( "Cumulative Cases" %in% input$data) { 
                return(new_york$cases * 1000/new_york$population )
            }
            if ( "Cumulative Deaths" %in% input$data) { 
                return(new_york$deaths * 1000/new_york$population )
            }
            if ( "New Cases" %in% input$data) { 
                return(new_york$new_cases * 1000/new_york$population )
            }
            if ( "New Deaths" %in% input$data) { 
                return(new_york$new_deaths * 1000/new_york$population )
            }
        })
        
        pro_data <- reactive({
            if ( "Cumulative Cases" %in% input$data) { 
                return(new_york$cases_proyected * 1000/new_york$population)
            }
            if ( "Cumulative Deaths" %in% input$data) {
                return(new_york$deaths_proyected * 1000/new_york$population )
            }
            if ( "New Cases" %in% input$data) { 
                return(new_york$new_cases_proyected * 1000/new_york$population )
            }
            if ( "New Deaths" %in% input$data) { 
                return(new_york$new_deaths_proyected * 1000/new_york$population )
            }
        })
        
        us_data <- reactive({
            if ( "Cumulative Cases" %in% input$data) { 
                return(new_york$us_cases * 1000/new_york$us_population)
            }
            if ( "Cumulative Deaths" %in% input$data) { 
                return(new_york$us_deaths * 1000/new_york$us_population)
            }
            if ( "New Cases" %in% input$data) { 
                return(new_york$us_new_cases * 1000/new_york$us_population)
            }
            if ( "New Deaths" %in% input$data) { 
                return(new_york$us_new_deaths * 1000/new_york$us_population)
            }
        })
        
        #file_name <- gsub( " ", "_", paste(tolower(input$state), 
        #                                   "_", 
        #                                   tolower(input$data), ".gif"))
        #if (!file.exists(here("images", file_name))) { 
            state_data <- covid_data %>% 
                filter(state == input$state)
            start_date <- lubridate::ymd(state_data[1,]$mandate_start)
            end_date <- lubridate::ymd(state_data[1,]$mandate_end)
            
            if (!is.na(end_date)) {
                state_data <- state_data %>% filter(((date >= mandate_start - days(30)) & 
                                            (date <= mandate_start + days(30))) |
                                           (date >= mandate_end - days(30)) & 
                                           (date <= mandate_end + days(30)
                                           ))
                national_data <- national_cases %>%
                    filter(((date >= start_date - days(30)) & 
                                (date <= start_date + days(30))) |
                               (date >= end_date - days(30)) & 
                               (date <= end_date + days(30)
                               ))
            } else if (!is.na(start_date)) { 
                    state_data <- state_data %>% filter(((date >= mandate_start - days(60)) & 
                                                (date <= mandate_start + days(60))))
                national_data <- national_cases %>%
                    filter((date >= start_date - days(60))) %>%
                    filter((date <= start_date + days(60)))
            } else { 
                national_data <- national_cases 
            }
    
            new_york <- merge(x = state_data, 
                              y = national_data, 
                              by='date', 
                              all.x=TRUE)
    
            ggplot(new_york, aes(x=date, y=data(), group=1)) + 
                geom_line(aes(y = pro_data(), colour = "Predicted")) + 
                geom_line(aes(y = data(), colour = "Actual")) +
                geom_line(aes(y = us_data(), colour = "US Total")) +
    
                scale_x_date(date_breaks = "2 weeks",
                             date_labels = "%m/%d/%Y") +
                scale_color_manual(name = input$data, 
                                   values = c("Actual" = "darkblue", 
                                              "Predicted" = "red", 
                                              "US Total" = "gray"))+
                annotate(geom = "vline",
                         x = c(start_date, end_date),
                         xintercept = c(start_date, end_date),
                         color = c("green", "orange"),
                         linetype = c("dashed", "dotdash")) +
                theme_classic() +
                theme(axis.text.x = element_text(face = "bold", color = "#993333", 
                                                 size = 10, angle = 45, hjust=1),
                      legend.position ="bottom") +
                annotate(geom = "text",
                         label = c("mandate starts", "mandate ends"),
                         x = c(start_date, end_date),
                         y = c(mean(data()), mean(data())),
                         angle = 90, 
                         vjust = 1) +
                labs(title = paste("Covid-19 ",
                                   input$data, 
                                   " in ",
                                   input$state), 
                     x= "Date", y= paste(input$data, " per 1000 hab.")) +
                theme(plot.title = element_text(hjust = 0.5))
        }
    )
    
    output$table <- renderTable({
        state_data <- covid_data %>% 
            filter(state == input$state)
        start_date <- lubridate::ymd(state_data[1,]$mandate_start)
        end_date <- lubridate::ymd(state_data[1,]$mandate_end)
        new_york <- data.frame(Data=character(), Value=character(), RateOfChange = character())
        
        if (!is.na(start_date)) { 
            before_mandate_start <- state_data %>% 
                filter(((date >= mandate_start - days(14)) & 
                            (date <= mandate_start))) %>% 
                mutate(cases_rate = (cases - lag(cases))/lag(cases),
                       deaths_rate = (deaths - lag(deaths))/lag(deaths),
                       new_cases_rate = (new_cases - lag(new_cases))/lag(new_cases),
                       new_deaths_rate = (new_deaths - lag(new_deaths))/lag(new_deaths)) %>%
                select(c(state, mandate_start, mandate_end,cases, 
                         deaths, 
                         new_cases, 
                         new_deaths,
                         cases_rate, 
                         deaths_rate,
                         new_cases_rate,
                         new_deaths_rate))
            after_mandate_start <- state_data %>% filter(((date >= mandate_start) & 
                                                              (date <= mandate_start + days(14))))%>% 
                mutate(cases_rate = (cases - lag(cases))/lag(cases),
                       deaths_rate = (deaths - lag(deaths))/lag(deaths),
                       new_cases_rate = (new_cases - lag(new_cases))/lag(new_cases),
                       new_deaths_rate = (new_deaths - lag(new_deaths))/lag(new_deaths)) %>%
                select(c(state, mandate_start, mandate_end,cases, 
                         deaths, 
                         new_cases, 
                         new_deaths,
                         cases_rate, 
                         deaths_rate,
                         new_cases_rate,
                         new_deaths_rate))
            
            label <- gsub("Cumulative ","", input$data)
            row_label <- tolower(gsub(" ","_", label))
            rate_label <- paste(row_label, "rate", sep="_")
            
            new_york <- new_york %>% add_row(Data="Mask Mandate Start", Value=as.character(start_date))%>% 
                add_row(Data=paste(label, "before mandate start"), 
                        Value=as.character(first(before_mandate_start[2,row_label])),
                        RateOfChange =scales::percent(first(before_mandate_start[2,rate_label]))) %>%
                add_row(Data=paste(label, "after mandate start"), 
                        Value=as.character(first(after_mandate_start[2,row_label])), 
                        RateOfChange =scales::percent(first(after_mandate_start[2,rate_label])))
        } else { 
            new_york <- new_york %>% add_row(Data=input$state, Value="No Mask mandate was instituted in this state")
        }
        
        if (!is.na(end_date)) {
            before_mandate_end <- state_data %>% filter(
                                                    (date >= mandate_end - days(30)) & 
                                                    (date <= mandate_end
                                                    ))%>% 
                mutate(cases_rate = (cases - lag(cases))/lag(cases),
                       deaths_rate = (deaths - lag(deaths))/lag(deaths),
                       new_cases_rate = (new_cases - lag(new_cases))/lag(new_cases),
                       new_deaths_rate = (new_deaths - lag(new_deaths))/lag(new_deaths)) %>%
                select(c(state, mandate_start, mandate_end,cases, 
                         deaths, 
                         new_cases, 
                         new_deaths,
                         cases_rate, 
                         deaths_rate,
                         new_cases_rate,
                         new_deaths_rate))
            after_mandate_end <- state_data %>% filter(((date <= mandate_end + days(30)) & 
                                                               (date >= mandate_end)))%>% 
                mutate(cases_rate =  (cases - lag(cases))/lag(cases),
                       deaths_rate =  (deaths - lag(deaths))/lag(deaths),
                       new_cases_rate =  (new_cases - lag(new_cases))/lag(new_cases),
                       new_deaths_rate =  (new_deaths - lag(new_deaths))/lag(new_deaths)) %>%
                select(c(state, mandate_start, mandate_end,cases, 
                         deaths, 
                         new_cases, 
                         new_deaths,
                         cases_rate, 
                         deaths_rate,
                         new_cases_rate,
                         new_deaths_rate))
            
            label <- gsub("Cumulative ","", input$data)
            row_label <- tolower(gsub(" ","_", label))
            rate_label <- paste(row_label, "rate", sep="_")
            
            new_york <- new_york %>%  add_row(Data="Mask Mandate End", 
                                              Value=as.character(end_date)) %>%
                add_row(Data=paste(label, "before mandate end"), 
                        Value=as.character(first(before_mandate_end[2, row_label])), 
                        RateOfChange =scales::percent(first(before_mandate_end[2,rate_label]))) %>%
                add_row(Data=paste(label, "after mandate end"), 
                        Value=as.character(first(after_mandate_end[2,row_label])), 
                        RateOfChange =scales::percent(first(after_mandate_end[2,rate_label])))
        } 
        new_york
    })
}


# Run the application 
shinyApp(ui = ui, server = server)
