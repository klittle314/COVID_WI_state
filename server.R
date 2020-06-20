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
        
        date_calc_end <- input$date_end_baseline
        
        date_calc_start <- input$date_start_baseline
       
        plot_out <- count_plot(df1_small,location,date_calc_end,date_calc_start)
        
    })
    
    make_slope_plot <- reactive({
        
        location <- input$choose_location
       
        slopes_plot_out <- slopes_plot(df1_small,location)
    })
    
    make_pcontrol_charts <- reactive({
      
        location <- input$choose_location
        
        date_calc_end <- input$date_end_baseline
        
        date_calc_start <- input$date_start_baseline
        #browser()
        control_charts_out <- p_control_chart_plots(df1_small,location,date_calc_end, date_calc_start)
    })
    
    make_ccontrol_chart <- reactive({
        
        location <- input$choose_location_low_count
        
        date_calc_end <- input$date_end_baseline
        
        date_calc_start <- input$date_start_baseline
        #browser()
        control_chart_out <- c_control_chart_plot(df1_small,location,date_calc_end,date_calc_start)
    })
   ####################Render the plot objects
     
   ################Count Plots###########################
    output$count_plot <- renderPlot({
        req(make_plot())
        
        print(make_plot())
        #make_plot()
        
    })
    
    ###############Slope Plots###########################
    output$slope_chart <- renderPlot({
        
        req(make_slope_plot())
     
        if(make_slope_plot()$message == 'Sufficient data to calculate at least one slope') {
            #do I need print function??
            make_slope_plot()$plot

        } 
        
    })
    
    output$slope_chart_tab <- renderUI({
        
        req(make_slope_plot())
        
        if(make_slope_plot()$message == 'Sufficient data to calculate at least one slope') {
            
           plotOutput("slope_chart",width="600px")
            
        } else {
            
            h5(make_slope_plot()$message)
        }
   
     })
    
    ###############Control Charts###########################
    output$pcontrol_chart <- renderPlot({
        
        req(make_pcontrol_charts())
        
        if(make_pcontrol_charts()$message == paste0("Sufficient data to display control charts: ",
                                                   make_pcontrol_charts()$n_records," records.")) {
           
            grid.arrange(grobs=make_pcontrol_charts()$plot)
            
        } 
    })
    
    output$p_control_chart_tab <- renderUI({
        
        req(make_pcontrol_charts())
        
        if(make_pcontrol_charts()$message == paste0("Sufficient data to display control charts: ",
                                                   make_pcontrol_charts()$n_records," records.")) {
            
            plotOutput("pcontrol_chart",width="600px",height="800px")
            
        } 
        
        else {
           
            h5(make_pcontrol_charts()$message)
        }
    })
    
    output$ccontrol_chart <- renderPlot({
        #browser()
        req(make_ccontrol_chart())
        #browser()
        print(make_ccontrol_chart())
    })
    
    
    ######################End of chart rendering##############

})
