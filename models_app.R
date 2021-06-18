library(shiny)

options(box.path = getwd())

box::use(dplyr[...],
         lme4[lmer, isSingular],
         texreg[screenreg, htmlreg],
         plm[plm],
         sandwich[vcovCL, vcovHC],
         lmtest[coeftest],
         ggplot2[...],
         rgdal[readOGR],
         broom[tidy],
         lubridate[date, as_date],
         magrittr[use_series, extract],
         readr[read_csv],
         # import own project functions
         functions/ts[get_coefs, load_project_data, test_lag],
         shinycssloaders[withSpinner])

# imports data for the project (function defined in functions/ts.r)
load_project_data()


ui <- fluidPage(

    # Application title
    titlePanel("Explore Models"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            checkboxGroupInput("vars", "Select Variables:", colnames(data), 
                               selected = c("deaths_per_mil_lag_5", "conf_govt", "ghs", "pop.km2",
                                            "democracy_index", "ethnic", "log_gdp", "log_conflict"))
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Model", uiOutput("model")),
                tabPanel("Time Plot", plotOutput("time_plot") %>% withSpinner())
            )
           
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$model <- renderUI({
        model <- data %>% 
            select(stringency_index, distrust_people, location, input$vars) %>% 
            lmer(stringency_index ~ distrust_people + . -location + (1 | location), .)
        
        htmlreg(model) %>% HTML()
    })
    
    output$time_plot <- renderPlot({
        data %>% 
            select(stringency_index, distrust_people, input$vars, date) %>% 
            get_coefs(stringency_index ~ distrust_people -date + ., 
                      n_days = 1, start = "2020-04-01", method = "lm")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
