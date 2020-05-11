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

    
    ################Create the plot objects as reactive functions
    make_plot <- reactive({
        
        location <- input$choose_location
        
        plot_out <- count_plot(df1_small,location)
        
    })
    
    make_slope_plot <- reactive({
        
        location <- input$choose_location
       
        slopes_plot_out <- slopes_plot(df1_small,location)
    })
    
    make_control_charts <- reactive({
       # browser()
        location <- input$choose_location
        
        control_charts_out <- control_chart_plots(df1_small,location)
    })
    
   ####################Render the plot objects
     
   ################Count Plots###########################
    output$count_plot <- renderPlot({
        req(make_plot())
        
        print(make_plot())
        
    })
    
    ###############Slope Plots###########################
    output$slope_chart <- renderPlot({
        
        req(make_slope_plot())
     
        if(make_slope_plot()$message == 'Sufficient data to calculate at least one slope') {
            
            make_slope_plot()$plot

        } 
        
    })
    
    output$slope_chart_tab <- renderUI({
        
        req(make_slope_plot())
        
        if(make_slope_plot()$message == 'Sufficient data to calculate at least one slope') {
            
           plotOutput("slope_chart")
            
        } else {
            
            h5(make_slope_plot()$message)
        }
   
     })
    
    ###############Control Charts###########################
    output$control_chart <- renderPlot({
        
        req(make_control_charts())
        
        if(make_control_charts()$message == paste0("Sufficient data to display control charts: ",
                                                    min_n_control_charts," records.")) {
           
            make_control_charts()$plot
            
        } 
    })
    
    output$control_chart_tab <- renderUI({
         
        req(make_control_charts())
        
        if(make_control_charts()$message == paste0("Sufficient data to display control charts: ",
                                                   min_n_control_charts," records.")) {
            
            plotOutput("control_chart")
            
        } 
        
        else {
            
            h5(make_control_charts()$message)
        }
    })
    
    ######################End of chart rendering##############

})
