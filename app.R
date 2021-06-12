library(tidyverse)
library(shiny)
country_from <- read_csv("Data/country_data.csv") 
country <- country_from %>%  
    filter(!is.na(total_cases_per_million)) %>% 
    filter(!location %in% c("Malta")) %>% 
    mutate(across(state_of_health:V001, .fns = ~.x*gwght)) %>% 
    select(alpha.3, location, median_age, aged_65_older, extreme_poverty, democracy_index, regime_type,
           gdp_per_capita, gdp_growth, pop.km2, sch_enrol_per_cap, geni, health_spending_pct_gdp,
           life_exp, positive_rate, avg_stringency, distrust_people, total_cases_per_million,
           total_tests_per_thousand, total_deaths_per_million, state_of_health, neighbour_distrust, 
           `fuctioning_of_govern­ment`, ghs, civil_liberties, total_vaccinations, sch_enrol_per_cap, 
           positive_rate, life_exp)
colnames(country)

theme_set(theme_minimal())

# Define UI for application that draws a histogram
ui <- navbarPage(title = "Covid Data",
                 tabPanel(
                     title = "Explore plots",
                     fluidPage(
                         sidebarLayout(
                             sidebarPanel(
                                 selectInput("x",
                                             "X Axis:", 
                                             choices = colnames(country),
                                             selected = "distrust_people"),
                                 selectInput("y", 
                                             "Y Axis:",
                                             choices = colnames(country),
                                             selected = "total_cases_per_million"),
                                 checkboxInput("lm", 
                                               "Show Trend Line",
                                               value = TRUE)
                             ),
                             mainPanel(
                                 plotOutput("scatter")
                             )
                         )
                     )
                 ), tabPanel(
                     title = "Confounding?",
                     fluidPage(
                         sidebarLayout(
                             sidebarPanel(
                                 selectInput("variable",
                                             "Potential Condounder:",
                                             choices = colnames(country),
                                             selected = "extreme_poverty"),
                                 numericInput("poly",
                                              "Polynomial Factor:",
                                              value = 1, min = 1, max = 10)
                             ),
                             mainPanel(
                                 plotOutput("upper")
                             )
                         ),
                         plotOutput("lower")
                     )
                 )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$scatter <- renderPlot({
        cp <- ggplot(country, aes_string(input$x, input$y)) +
            geom_point() 
        if (input$lm == TRUE) {
            cp + geom_smooth(method = lm)
        } else {
            cp
        }
    })
    output$upper <- renderPlot({
        ggplot(country, aes_string(input$variable, "total_cases_per_million")) +
            geom_point() +
            geom_smooth(method = lm, formula = y ~ poly(x, input$poly))
    })
    output$lower <- renderPlot({
        ggplot(country, aes_string(input$variable, "distrust_people")) +
            geom_point() +
            geom_smooth(method = lm, formula = y ~ poly(x, input$poly))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
