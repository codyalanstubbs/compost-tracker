library(shiny)
library(dplyr)
# Define basic save and load data functions
library(googlesheets4)

table <- "https://docs.google.com/spreadsheets/d/15kufKnNNG2S18CvzNWUayN0QaG8J1ZJ9OD1tf0G203U/edit?usp=sharing"

#drive_auth(cache = ".secrets") #for the first time running the app in R to get the OAuth token
gs4_auth(cache = ".secrets", email = TRUE, use_oob = TRUE)

saveData <- function(data) {
    # Add the data as a new row
    sheet_append(table, data)
}

loadData <- function() {
    # Read the table
    read_sheet(table)
}

# Shiny app with 3 fields that the user can submit data for
shinyApp(
    ui = fluidPage(
        textInput("initials", "Initials (e.g.: CAS):", ""),
        textInput("item", "Item (e.g.: apple core, banana peel, egg shell, etc.", ""),
        textInput("item.count", "Item Count (e.g.: if you weighed the egg shells of two eggs, then enter 2.)", ""),
        textInput("weight", "Weight (grams)", ""),
        textInput("notes", "Notes", ""),
        actionButton("submit", "Submit"),
        DT::dataTableOutput("responses", width = 300), tags$hr()
        # ,
        # plotOutput("plot", width = "400px")
    ),
    server = function(input, output, session) {
        
        # Whenever a field is filled, aggregate all form data
        formData <- reactive({
            data <- data.frame(record.date = Sys.Date(),
                               initials = input$initials, 
                               item = input$item,
                               item.count = input$item.count %>% as.numeric(.), 
                               weight = input$weight %>% as.numeric(.),
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
         
        # output$plot <- renderPlot(plot(x = formData$item, y = formData$weight), res = 100)
    }
)