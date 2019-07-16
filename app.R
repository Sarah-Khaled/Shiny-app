
library(shiny)

# Define UI for application
ui <- fluidPage(
        
        headerPanel("Regression plot on 'mtcars' dataset "),
        sidebarPanel(
                radioButtons("variable", "Check all the variables you want to apply",
                             choices = c("Cylinders" = "cyl",
                                         "Transmission" = "am",
                                         "Gears" = "gear",
                                         "Gross horsepower"="hp"),
                             selected = NULL),
                submitButton("submit")
        ),
        
        mainPanel(
                h4('Regression Plot'),
                verbatimTextOutput("oid2"),
                plotOutput("distPlot")
                
        )
)

# Define server 
server <- function(input, output) {
        
        output$oid2<-renderPrint({
                x <- unlist(strsplit(as.character(input$variable)," "))
                Fit1<-lm(mpg~mtcars[,x],data=mtcars)
                print(Fit1)
                
                
        })
 
        output$distPlot <- renderPlot({
                x <- unlist(strsplit(as.character(input$variable)," "))
                Fit1<-lm(mpg~mtcars[,x],data=mtcars)
                print(Fit1)
                plot(mtcars$mpg~mtcars[,x],pch = 16, cex = 1.3,
                     col = "blue", ylab = "mpg", xlab=x)
                abline(Fit1,col="red")
                
                
        })
        
}

# Run the application 
shinyApp(ui = ui, server = server)

