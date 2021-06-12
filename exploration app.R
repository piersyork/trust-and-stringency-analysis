box::use(dplyr[...],
         ggplot2[...],
         functions/ts[load_project_data],
         lubridate[as_date, date],
         plotly[ggplotly, renderPlotly, plotlyOutput, event_data],
         cowplot[plot_grid],
         WikipediR[page_content],
         magrittr[extract, use_series],
         rvest[read_html, html_attr, html_nodes, html_text])

load_project_data()

library(shiny)

countries <- NULL

# Set theme
my_theme <- theme_minimal() +
    theme(legend.position = "top",
          axis.line.x = element_line(), axis.ticks.x = element_line(),
          plot.caption = element_text(hjust = 0))

theme_set(my_theme)




ui <- fluidPage(
    
    # Application title
    titlePanel("Country Analysis"),
    
    
    sidebarLayout(
        sidebarPanel(
            selectInput("raw",
                        "Death Measure:",
                        choices = c("Raw", "Per Million"),
                        selected = "Per Million"),
            sliderInput("d_limits", 
                        "Deaths Limits:",
                        min = 0,
                        max = 20, 
                        step = 0.1,
                        value = 0),
            actionButton("add", "Add Country"),
            actionButton("reset", "Reset Countries")
        ),
        
        
        mainPanel(
            tabsetPanel(
                tabPanel("Pooled", plotlyOutput("pooled"), uiOutput("list")),
                tabPanel("Deaths", plotOutput("deaths")),
                tabPanel("Stringency", plotOutput("stringency")),
                tabPanel("Residential Change", plotOutput("res_chng")),
                tabPanel("Compare", plotOutput("compare")),
                tabPanel("Wiki", uiOutput("wiki"))
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    countries_selected <- reactive({

        click_x <- event_data("plotly_click", source = "plotly_out")[3] %>%
            pull()
        locat <- data %>%
            filter(round(distrust_people, 6) == round(click_x, 6)) %>%
            pull(location) %>% unique()
        countries <<- c(countries, locat)
        # countries <- c(countries, locat)
        # print(countries)
    })
    
    # get_countries <- reactive({
    #     countries <- countries
    #     return(countries)
    # })
    
    observeEvent(input$add, {
        if (!is.null(event_data("plotly_click", "plotly_out"))) {
            click_x <- event_data("plotly_click", source = "plotly_out")[3] %>%
                pull()
            
            locat <- data %>%
                filter(round(distrust_people, 6) == round(click_x, 6)) %>%
                pull(location) %>% unique()
            
            countries <<- c(countries, locat)
            
            print(countries)
            
            output$list <- renderUI({
                HTML(c("Countries Selected: <br/>", paste(unique(countries), "<br/>"))) #"Countries selected:", 
            })
        } else {
            output$list <- renderUI({
                HTML("No country selected")
            })
        }
    })
    
    observeEvent(input$reset, {
        countries  <<- NULL
    })

    output$pooled <- renderPlotly({
        
        pool <- data %>% 
            select(distrust_people, stringency_index, location) %>% 
            na.omit() %>% 
            group_by(location) %>% 
            summarise(distrust_people = mean(distrust_people), 
                      stringency_index = mean(stringency_index)) %>% 
            ggplot(aes(distrust_people, stringency_index, label = location)) +
            geom_point() +
            geom_smooth(method = lm)
        
        plotly::ggplotly(pool, source = "plotly_out")
    })
    


    output$deaths <- renderPlot({
        print(countries_selected)
        click_x <- event_data("plotly_click", source = "plotly_out")[3] %>% 
            pull()
        print(click_x)
        locat <- data %>% 
            filter(round(distrust_people, 6) == round(click_x, 6)) %>% 
            pull(location) %>% unique()
        print(locat)
        
        if (input$raw == "Per Million") {
            recorded <- data %>% 
                filter(location == locat) %>% 
                ggplot(aes(date, new_deaths_per_million)) +
                labs(title = paste("Daily deaths per million -", locat), x = "", y = "", colour = "",
                     caption = "Source: Our World in Data")
        } else {
            recorded <- data %>% 
                filter(location == locat) %>% 
                ggplot(aes(date, new_deaths)) +
                labs(title = paste("Daily deaths -", locat), x = "", y = "", colour = "",
                     caption = "Source: Our World in Data")
        }
        
        
        recorded <- recorded +
            geom_smooth(span = 0.1, se = FALSE, size = 0.5) +
            geom_line(alpha = 0.5) +
            scale_x_date(date_breaks = "10 weeks", date_labels = "%b %Y",
                         limits = c(date("2020-02-01"), date("2021-03-01"))) +
            ggsci::scale_color_lancet()
        
        
        excess <- weekly_data %>% 
            filter(location == locat) %>% 
            ggplot(aes(date, excess_deaths_per_100k * 10)) +
            geom_line(alpha = 0.5) +
            scale_x_date(date_breaks = "10 weeks", date_labels = "%b %Y",
                         limits = c(date("2020-02-01"), date("2021-03-01"))) +
            labs(title = paste("Excess deaths per million -", locat), x = "", y = "", colour = "",
                 caption = "Source: Our World in Data") +
            ggsci::scale_color_lancet()
        
        if (input$d_limits != 0) {
            recorded <- recorded + scale_y_continuous(limits = c(0, input$d_limits))
            excess <- excess + scale_y_continuous(limits = c(-4, input$d_limits))
        }
        
        plot_grid(recorded, excess, ncol = 1, rel_heights = 1.5)
        
    }, height = 600)
    
    output$stringency <- renderPlot({
        click_x <- event_data("plotly_click", source = "plotly_out")[3] %>% 
            pull()
        print(click_x)
        locat <- data %>% 
            filter(round(distrust_people, 6) == round(click_x, 6)) %>% 
            pull(location) %>% unique()
        print(locat)
        
        data %>% 
            filter(location == locat) %>% 
            ggplot(aes(date, stringency_index)) +
            geom_line() +
            scale_y_continuous(limits = c(0, 100)) +
            scale_x_date(date_breaks = "10 weeks", date_labels = "%b %Y",
                         limits = c(date("2020-02-01"), date("2021-03-01"))) +
            labs(title = paste("Daily Stringency Index -", locat), x = "", y = "", colour = "",
                 caption = "Source: Our World in Data") +
            ggsci::scale_color_lancet()
    })
    
    output$res_chng <- renderPlot({
        click_x <- event_data("plotly_click", source = "plotly_out")[3] %>% 
            pull()
        print(click_x)
        locat <- data %>% 
            filter(round(distrust_people, 6) == round(click_x, 6)) %>% 
            pull(location) %>% unique()
        print(locat)
        
        data %>% 
            filter(location == locat) %>% 
            ggplot(aes(date, res_pct_chng)) +
            geom_line(alpha = 0.5) +
            geom_smooth(span = 0.1, se = FALSE, size = 0.5) +
            # scale_y_continuous(limits = c(0, 100)) +
            scale_x_date(date_breaks = "10 weeks", date_labels = "%b %Y",
                         limits = c(date("2020-02-01"), date("2021-03-01"))) +
            labs(title = paste("Percentage change in residential areas -", locat), 
                 x = "", y = "", colour = "",
                 caption = "Source: Our World in Data") +
            ggsci::scale_color_lancet()
        
    })
    
    output$compare <- renderPlot({
        deaths <- data %>% 
            filter(location %in% countries_selected()) %>% 
            ggplot(aes(date, new_deaths_per_million, colour = location)) +
            # geom_line(alpha = 0.5) +
            geom_smooth(span = 0.1, se = FALSE, size = 0.5) +
            scale_x_date(date_breaks = "10 weeks", date_labels = "%b %Y",
                         limits = c(date("2020-02-01"), date("2021-03-01"))) +
            labs(title = "Deaths per million", x = "", y = "", colour = "",
                 caption = "Source: Our World in Data") +
            ggsci::scale_color_lancet()
        stringency <- data %>% 
            filter(location %in% countries_selected()) %>% 
            ggplot(aes(date, stringency_index, colour = location)) +
            geom_line() +
            scale_y_continuous(limits = c(0, 100)) +
            scale_x_date(date_breaks = "10 weeks", date_labels = "%b %Y",
                         limits = c(date("2020-02-01"), date("2021-03-01"))) +
            labs(title = "Daily Stringency Index", x = "", y = "", colour = "",
                 caption = "Source: Our World in Data") +
            ggsci::scale_color_lancet()
        
        res_chng <- data %>% 
            filter(location %in% countries_selected()) %>% 
            ggplot(aes(date, res_pct_chng, colour = location)) +
            geom_line(alpha = 0.5) +
            geom_smooth(span = 0.1, se = FALSE, size = 0.5) +
            # scale_y_continuous(limits = c(0, 100)) +
            scale_x_date(date_breaks = "10 weeks", date_labels = "%b %Y",
                         limits = c(date("2020-02-01"), date("2021-03-01"))) +
            labs(title = "Percentage change in residential areas", 
                 x = "", y = "", colour = "",
                 caption = "Source: Our World in Data") +
            ggsci::scale_color_lancet()
        
        plot_grid(deaths, stringency, res_chng, ncol = 1, rel_heights = 1.5)
    }, height = 900)
    
    output$wiki <- renderUI({
        click_x <- event_data("plotly_click", source = "plotly_out")[3] %>% 
            pull()
        locat <- data %>% 
            filter(round(distrust_people, 6) == round(click_x, 6)) %>% 
            pull(location) %>% unique()
        print(locat)
        
        page <- page_content(language = "en", project = "wikipedia",
                             page_name = paste("COVID-19 pandemic in", locat)) %>% 
            use_series(parse) %>% 
            use_series(text) %>% 
            use_series(`*`) %>% 
            extract(1)
        
        xml_page <- read_html(page)
        n_divs <- xml_page %>% 
            html_nodes("div") %>% 
            length()
        if (n_divs < 5) {
            redirect <- xml_page %>% 
                html_nodes(".redirectMsg") %>% 
                html_text() %>% 
                gsub("Redirect to:", "", .)
            
            page <- page_content(language = "en", project = "wikipedia",
                                 page_name = redirect) %>% 
                use_series(parse) %>% 
                use_series(text) %>% 
                use_series(`*`) %>% 
                extract(1)
        }
        
        
        HTML(page)
            
        
        
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)



