#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)

df <- read_csv("df.csv")

df <- cbind(df,Depar_Level_ID=NA)
df$Depar_Level_ID <- paste(df$Department,"_",df$Level,sep="")

colnames(df)[which(colnames(df) == "Title_Catalog")] <- "Title"

depar_reference <- read_csv("department_students.csv")
colnames(depar_reference) <- c("Department","Typical for Department")

level_reference <- read_csv("level_students.csv")

depar_level_reference <- read_csv("depar_level_students.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(

  tags$head(
    # Note the wrapping of the string in HTML()
    tags$style(HTML("

    .well {
      background-color: white;
      border: none;
      padding: 10px;
      margin-bottom: 10px;
    }

    .col-sm-4 {
      width: 30%;
      padding: 0px;
      float: left;
    }

    .col-sm-8 {
      width: 70%;
      float: left;
    }

                      "))
  ),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput(
              'classes',
              'Select your classes:',
              choices = df$`Title`,
              multiple = TRUE,
              selectize = TRUE
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
           tableOutput("table")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$table <- renderTable(
      df %>% select(`Title`,Department,Enrolled,Level,Depar_Level_ID) %>%
        filter(`Title` %in% input$classes) %>%
        left_join(depar_reference,by="Department") %>%
        left_join(level_reference,by="Level") %>%
        left_join(depar_level_reference,by="Depar_Level_ID") %>%
        select(!Department) %>%
        select(!Level) %>%
        select(!Depar_Level_ID)
    )
}

# Run the application
shinyApp(ui = ui, server = server)
