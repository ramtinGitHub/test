# ---- Module 10 Assignment ----

# ---- Set Up ----
# Import libraries
# Remember that if you need to install a package locally,
# use install.packages("shiny")
install.packages("shiny")
library(shiny)
library(dplyr)
library(ggplot2)

# Save this script and the income.csv file (available on Canvas under Module 10)
# in the same folder on your computer
# Use setwd() to point R to this correct folder
# Import income.csv to an object called income

setwd("C:/Users/midia/Desktop/Documents/Emeritus/Week 10/Data set")
income_data <- read.csv("income.csv")
income_data$capital_loss<-as.numeric(income_data$capital_loss)
income_data$hours_per_week<-as.numeric(income_data$hours_per_week)
income_data$capital_gain<-as.numeric(income_data$capital_gain)
# ---- Submission ----
# 1. Fill in the 13 blanks in the code below
## NOTE: The numbers (e.g. 1_____) are used as placeholders to identify 
## the blank spaces - make sure you delete them as you fill in the appropriate code
# 2. Go to Canvas and open the Atomic Exercise for Module 10
# 3. Answer the questions based on the completed code
## NOTE: Each question will ask something like "What piece of code goes in blank #4?"
# 4. Run your shiny app and take a screenshot
# 5. Submit the screenshot to the Canvas Module 10 Discussion Board

# ---- Context ----
# Your goal is to create a shiny application that can be used
# to explore how occupation is related to hours worked per week
# and capital loss, based on which income category an individual
# belongs to (<=50K or > 50K).

# ---- Instructions: ui object ----
# 1. Set up the app to have a title panel, and a sidebar layout with a side panel and main panel

# 2. Set the title to "Module 10 Assignment"

# 3. On the sidebar panel, include three interactive pieces:
# (a) A drop down menu where users can choose either "<=50K" or "50K" as the income category
# - call this input "subset_income"
# (b) A drop down menu where users can choose either "hours_per_week" or "capital_loss" as the y-axis variable
# - call this input "set_yaxis"
# (c) A set of checkboxes where users can select which occupations should be included in the figure
# - by default, when the app starts, have all of the boxes checked
# - call this input "select_occupation"
# - hint: use checkboxGroupInput()

# 4. On the main panel, include a figure that reactis to changes in the user input
# - more details can be found in the server function instructions

# ---- Assignment Code: ui object ----
# Define the ui object
ui <- fluidPage(titlePanel("Module 10 Assignment"),
  sidebarLayout(
  sidebarPanel(
  selectInput(inputId = "subset_income", label = "The income is:", choices = unique(income_data$income)),
      # Interactive piece 2: inputId = "set_yaxis"
      selectInput(inputId = "set_yaxis", label = "The variable is:", 
      choices = c(hours_per_week = "hours_per_week",capital_loss="capital_loss")),
      # Interactive piece 3: inputId = "subset_occupation"
      checkboxGroupInput(inputId = "subset_occupation",label = "Occupation is:",
                        choices= unique(income_data$occupation),selected = unique(income_data$occupation))),
  
  # Main panel
  mainPanel(plotOutput(outputId = "myfigure"))))



# ---- Instructions: server function ----
# 1. Subset the income data to only include records where:
# - capital loss is greater than 0
# - income category is the input selected above ("subset_income"), and 
# - occupation is in the input selected above ("subset_occupation")

# 2. Render a boxplot with occupation on the x axis and the input selected above ("set_yaxis") on the y axis

# ---- Assignment Code: server function ----
# Define server function
server <- function(input, output) {
  # Create a reactive subset of the data
  create_subset <- reactive(filter(income_data,capital_loss > 0 & 
              income %in% input$subset_income &
              occupation %in% input$subset_occupation))
  
  # Render Plot
  output$myfigure <- renderPlot(ggplot(create_subset()) +
                                  # Boxplot of x = occupation, y = defined by input
                                  geom_boxplot(aes_string(x ="occupation", y = input$set_yaxis)) +
                                  theme_bw(18)+
                                  theme(axis.text.x = element_text(angle = 90, hjust = 1)))
}

# ---- Run App ----
shinyApp(ui, server)
