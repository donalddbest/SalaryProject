rm(list=ls())
EnsurePackage <- function(x)
{
  
  x <- as.character( x )
  
  if (!require( x, character.only = TRUE))
  {
    
    install.packages( pkgs = x, repos = "http://cran.r-project.org")
    require( x, character.only = TRUE)
    
  }
  
  
}

# I make sure I have the sqlite package and load the library.
EnsurePackage( "RSQLite" )
EnsurePackage( "shiny" )
library(shiny)
library( "RSQLite" )

# Getting the data from my database.
connect <- dbConnect( RSQLite::SQLite(), "salarydb" )
salframe <- dbGetQuery( connect, "select * from salary where JobTitle != 'Not provided'" )

# Gets all the unique job titles.
uniques <- unique( salframe$JobTitle )

ui<-fluidPage(
   
              titlePanel("Shiny Salary App"),
              tabsetPanel(
                          tabPanel(title = "Pay Distribution", 
                                   sidebarLayout(
                                                 sidebarPanel(
                                                              selectInput(
                                                                          inputId = "graphselector",
                                                                          label = "Select your Job Title of Interest",
                                                                          uniques
                                                                          )
                                                              ),
                                                 mainPanel(
                                                           plotOutput(outputId = "graph"), 
                                                           verbatimTextOutput("stats")
                                                           )
                            
                            
                                                 )
                                   ),
                          tabPanel( title = "Full Data Set",
                                   h1( "Full Dataset" ),
                                   dataTableOutput( "fulldf" )
                                   )
                
                
                          )
              
            )


server<-function(input,output){
  output$fulldf <- renderDataTable({ salframe })
  output$graph <- renderPlot(
                             {
                              x <- subset( salframe, JobTitle == input$graphselector, JobTitle:TotalPay );
                              hist( x$TotalPay, main = paste( "Histogram of ", x$JobTitle[1] ), 
                                    xlab = paste( "Annual Total Pay of ", x$JobTitle[1] ), col = "goldenrod2" )
                             }
                            )
  output$stats <- renderPrint(
                              {
                                x <- subset( salframe , JobTitle == input$graphselector , JobTitle:TotalPay );
                                y <- subset( salframe , JobTitle == input$graphselector , BasePay:TotalPayBenefits );
                                r <- as.vector( colMeans( y ) );
                                w <- names( y );
                                cat( 
                                    x$JobTitle[1] , "\nMean of:" , "\n" , "\t" , w[1] , r[1] , w[2] , r[2] , w[3] , 
                                    r[3] , w[4] , r[4] , w[5] , r[5] , w[6] , r[6] , "\n" , sep = " " 
                                   )
                               }
                              )
                               }


shinyApp(ui=ui,server = server)