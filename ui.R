library(shinyBS)

# simple UI for draft
shinyUI(navbarPage("Wisconsin Gating Criteria Data",
    
    tabPanel("Overview",
    h3("Web App:  Draft 1.0"),
    
    wellPanel(
        tags$style(type="text/css", '#leftPanel { width:200px; float:left;}'),
        
        tags$blockquote("This project shows simple data plots for the Wisconsin DPH gating criteria"),
        tags$hr(),
        
        
        br(),
        br(),
        h5('Click',
           tags$a('here', 
                  href = 'https://www.dhs.wisconsin.gov/publications/p02653a.pdf',
                  target = '_blank'),
           'for description of the gating criteria from DHS'),
           
       
        #helpText("Questions? Contact Kevin Little, Ph.D."),
        br(),
        
        #connection to GitHub repository here
        h5('Code for Shiny app and related RMarkdown file available on',
           tags$b('GitHub.'), 
           'Click ',
           tags$a('here',
                  href="https://github.com/klittle314/COVID_WI_state",
                  target = '_blank'),
           'to view and download.'),
        
        
        # author info
        shiny::hr(),
        em(
            span("Created by "),
            a("Kevin Little", href = "mailto:klittle@iecodesign.com"),
            
            span("5 May 2020"),
            
            br(), br(),
            
            helpText("only showing publicly available testing data")
            
        )
        
     )
    
   ),
    
    tabPanel("Testing Data",

        sidebarLayout(
            sidebarPanel(
                selectInput(
                    inputId  = 'choose_location',
                    label    = h5("Choose state or county"),
                    choices  = unique(df1_small$NAME),
                    selected = "WI",
                    width    = "100%")   
                ),
           

            # Show a plot of the generated distribution
            mainPanel(
               tabsetPanel(id = 'display-tab', type = 'tabs',
                tabPanel("Count Plots",
                
                         plotOutput("count_plot", height="600px")
                ),
               
                # tabPanel("Gating Slope Check Plot",
                #    
                #          plotOutput("slope_plot", height="400px")    
                # )
                tabPanel("Gating Slope Check Plot",
                         
                         uiOutput('slope_chart_tab')
                ),
                
                tabPanel("Control Charts",
                         
                         uiOutput('control_chart_tab'))
               )  
            )
        ) 
   )  
 )
)