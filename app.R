library(shiny)

# Define basic save and load data functions
library(googlesheets4)

table <- "https://docs.google.com/spreadsheets/d/1TFhO8xeayIuFCcGSDR8fhqGaD1N_QK122JLEzDJY8v4/edit?usp=sharing"

saveData <- function(data) {
    # Add the data as a new row
    sheet_append(table, data)
}

loadData <- function() {
    # Read the table
    read_sheet(table)
}

library(tidyverse)
# make the breaks for the Count per Minute y-axis ----
breaks <- c(10^(-10:10), 5*10^(-10:10))
breaks <- as.data.frame( data.frame(breaks) %>% filter(breaks >= 1e-03, breaks <= 1e+03) )$breaks
minor_breaks <- rep(1:9, 21)*(10^rep(-10:10, each=9))
minor_breaks <- as.data.frame( data.frame(minor_breaks) %>% filter(minor_breaks >= 1e-03, minor_breaks <= 1e+03) )$minor_breaks

# create key-value pairs for some handy conversions of the low count (bottom) portion of the y-axis ----
min_key <- 
    data.frame(
        value = c(1, 0.5, 0.2, 0.1, 
                  0.05, 0.02, 0.01, 
                  0.005, 0.002, 0.001)) %>% 
    mutate(key = 1/value)

hrs_key <- 
    data.frame(
        value = c(15, 30, 60, 300,
                  480, 960, 1440)) %>% 
    mutate(key = value/60) %>% 
    mutate(value = 1/value)

x_values <- seq(0,150,7) # week duration values for the x-axis
d <- data.frame() # blank dataframe

# standard celeration chart using hrs_key ----
scc <- ggplot(d, aes(x =0, y = 1/2000)) +
    geom_point(alpha = 0) + # default point; remove 'alpha = 0' to make visible
    #geom_point(pch = 4) + # x for errors
    scale_y_log10(
        name = "Behavior Count per Minute",
        breaks = breaks, 
        minor_breaks = minor_breaks, 
        limits = c(1/2000,1e+03), 
        labels = breaks, 
        expand = c(0,0),
        sec.axis = dup_axis(
            name = "Hours to Complete 1 Behavior",
            breaks = hrs_key$value,
            labels = hrs_key$key) )+
    scale_x_continuous(
        name = "Successive Calendar Days",
        limits = c(min(x_values), max(x_values)),
        breaks = seq(min(x_values), max(x_values), 7),
        minor_breaks = seq(min(x_values), max(x_values), 1), 
        expand = c(0,1)) +
    annotation_logticks(sides="l") +
    theme_bw() +
    theme(
        panel.grid.major = element_line(color = "darkgrey"),    
        panel.grid.minor = element_line(color = "grey"))

# Shiny app with 3 fields that the user can submit data for
shinyApp(
    ui = fluidPage(
        DT::dataTableOutput("responses", width = 300), tags$hr(),
        textInput("behavior.date", "Behavior Date (e.g.: 2020-11-17)", ""),
        textInput("behaver", "Behaver", ""),
        textInput("project", "Project", ""),
        textInput("behavior", "Behavior", ""),
        textInput("count", "Number of Behaviors Completed", ""),
        textInput("duration", "Duration (Minutes)", ""),
        textInput("notes", "Notes", ""),
        actionButton("submit", "Submit"),
        plotOutput("plot", width = "400px")
    ),
    server = function(input, output, session) {
        
        # Whenever a field is filled, aggregate all form data
        formData <- reactive({
            data <- data.frame(data.entry.date = Sys.Date(),
                               behavior.date = input$behavior.date,
                               behaver = input$behaver, 
                               project = input$project, 
                               behavior = input$behavior, 
                               count = input$count, 
                               duration = input$duration,
                               notes = input$notes)
            
        })
        
        # When the Submit button is clicked, save the form data
        observeEvent(input$submit, {
            saveData(formData())
        })
        
        # Show the previous responses
        # (update with current response when Submit is clicked)
        output$responses <- DT::renderDataTable({
            input$submit
            loadData()
        })     
        
        output$plot <- renderPlot(scc, res = 100)
    }
)