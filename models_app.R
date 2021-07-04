library(shiny)

options(box.path = getwd())

box::use(dplyr[...],
         lme4[lmer, isSingular, fixef],
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
         shinycssloaders[withSpinner],
         rgdal[readOGR],
         broom[tidy],
         tidyr[pivot_wider, unnest],
         # import own project functions
         functions/ts[...],
         functions/ap[...])

# imports data for the project (function defined in functions/ts.r)
load_project_data()

    

# import map data
map_data <- readOGR("Map Data/World_Countries/", "World_Countries")

# remove Antarctica from map and tidy map data
map_tidy <- tidy(map_data, "COUNTRY") %>% 
    filter(!id == "Antarctica")

# Set theme
my_theme <- theme_minimal() +
    theme(legend.position = "top",
          axis.line.x = element_line(), axis.ticks.x = element_line(),
          plot.caption = element_text(hjust = 0))

theme_set(my_theme)

ui <- fluidPage(

    # Application title
    titlePanel("Explore Models"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            checkboxGroupInput("vars", "Select Variables:", colnames(data), 
                               selected = c("deaths_per_mil_lag_5", "conf_govt", "ghs", "pop.km2",
                                            "regime_type", "ethnic", "log_gdp", "log_conflict",
                                            "education_index", "continent")),
            radioButtons("interaction", "Select Interaction:",
                         c("none", "continent", "regime_type"), 
                         selected = "none")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Model", 
                         uiOutput("model") %>% withSpinner(),
                         uiOutput("formula")),
                tabPanel("Across Time", plotOutput("time_plot") %>% withSpinner()),
                tabPanel("Sample Map", 
                         plotOutput("sample_map") %>% withSpinner(),
                         uiOutput("nobs"), # plotOutput("effective_map") %>% withSpinner()
                         tableOutput("sample_table")),
                tabPanel("Assumptions", 
                         plotOutput("colin") %>% withSpinner(),
                         plotOutput("linearity") %>% withSpinner(),
                         plotOutput("cooks"))
                         
            )
           
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    rval <- reactiveValues()
    observe({
        ind_vars <- input$vars
        # print(ind_vars)
        dep_var <- "stringency_index"
        
        inter <- paste(input$interaction)
        # print(inter)
        if (inter != "none") {
            ind_vars <- c(ind_vars, "distrust_people*", inter)
        }
        # print(ind_vars)
        
        .form <- formula(paste(c(dep_var, "~", 
                                 "distrust_people +",
                                 paste(ind_vars, "+"), "distrust_people"), 
                               collapse = " "))
        
        rval$formula <- .form
        rval$form_lmer <- update(.form, ~ . + (1 | location))
        
        # print(rval$formula)
    })

    output$model <- renderUI({
        
        lmer_model <- lmer(rval$form_lmer, data)
        rval$lmer_model <- lmer_model
        
        lm_model <- lm(rval$formula, data) %>% 
            coeftest(., vcovCL(., cluster = ~location))
        
        plm_model <- plm(rval$formula, data, index = c("location", "date"), effect = "time") %>% 
            coeftest(vcovHC, cluser = "group")
        
        htmlreg(list(lm_model, lmer_model, plm_model), 
                custom.model.names = c("Pooled OLS", "Random Effects", "Time Fixed Effects"),
                omit.coef = "Intercept", 
                include.aic = FALSE, include.bic = FALSE, include.log = FALSE) %>% 
            HTML()
    })
    
    output$formula <- renderUI({
        form_lmer <- update(rval$formula, ~ . + (1 | location))
        print(form_lmer)
        HTML(paste0("<br/><br/><b>", form_lmer, "<b/>"))
    })
    
    output$time_plot <- renderPlot({
        print(rval$formula)
        data %>% 
            get_coefs(rval$formula, 
                      n_days = 1, start = "2020-04-01", method = "lm")
    })
    
    output$sample_map <- renderPlot({
        cntry_data <- data %>% 
            select(location, distrust_people, input$vars) %>% #, input$vars
            na.omit() %>% 
            group_by(location) %>% 
            mutate(across(where(is.numeric), ~mean(.x))) %>% 
            distinct()
        print(nrow(cntry_data))
        
        map_tidy %>% 
            left_join(cntry_data, by = c("id" = "location")) %>%
            mutate(sample = ifelse(!is.na(distrust_people), 1, 0)) %>% 
            ggplot(aes(x = long, y = lat, group = group, fill = factor(sample))) +
            geom_polygon(show.legend = FALSE) + #, color = "grey", size = 0.1
            scale_fill_manual(values = c("#aecae5", "#3182bd")) + 
            #scale_fill_brewer(direction = 1) +
            coord_equal() +
            theme_void() +
            theme(plot.title = element_text(hjust = 0.05),
                  plot.tag.position = c(0.95, 1), plot.tag = element_text(size = 10))
    })
    
    output$effective_map <- renderPlot({
        # plot_sample_map(data, map_tidy, rval$formula)
    })
    
    output$sample_table <- renderTable({
        data %>% 
            select(location, input$vars, distrust_people, continent) %>% 
            na.omit() %>% 
            select(continent, location) %>% 
            distinct() %>% 
            table_continents()
    }, spacing = "xs", )
    
    output$nobs <- renderUI({
        n_countries <- data %>% 
            select(location, input$vars, distrust_people) %>% 
            na.omit() %>% 
            pull(location) %>% 
            unique() %>% 
            length()
        
        HTML(paste("<br/><b>Number of Countries:</b>", n_countries, "<br/><br/>"))
    })
    
    output$colin <- renderPlot({
        plot_vif(rval$lmer_model) +
            theme(axis.text = element_text(size = 11),
                  legend.text = element_text(size = 11),
                  axis.title = element_text(size = 12)) +
            labs(title = "Multicolinearity")
    })
    output$linearity <- renderPlot({
        plot.merMod(rval$lmer_model)
    })
    
    output$cooks <- renderPlot({
        plot_cooks_distance(rval$lmer_model)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)



