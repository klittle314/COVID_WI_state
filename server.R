#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

source("global.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

    make_plot <- reactive({
        
        location <- input$choose_location
        
        plot_out <- count_plot(df1_small,location)
        
    })
    
    make_slope_plot <- reactive({
        location <- input$choose_location
        
        slopes_plot_out <- slopes_plot(df1_small,location)
    })
    
    output$count_plot <- renderPlot({
        req(make_plot())
        
        print(make_plot())
        
    })
    
    output$slope_plot <- renderPlot({
        req(make_slope_plot())
        
        print(make_slope_plot())
    })

})
