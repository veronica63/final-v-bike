library(shiny)
library(shinydashboard)

ui <- dashboardPage(
    dashboardHeader(),
    dashboardSidebar(),
    dashboardBody(
        fluidRow(
            valueBoxOutput("vbox")
        )
    )
)

server <- function(input, output) {
    output$vbox <- renderValueBox({
        valueBox(
            100, "Test",
            icon = icon("thumbs-up"), color = "green"
        )
    })
}

shinyApp(ui, server)
