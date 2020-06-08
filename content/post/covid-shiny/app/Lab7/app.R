library(shiny)
library(gt)
library(covdata)
library(ggpubr)
library(tidyverse)

case_data <- nytcovstate
locations <- sort(unique(case_data$state))
mobility_data <- apple_mobility %>%
                 filter(region %in% locations) %>%
                 select(-alternative_name, -geo_type) %>%
                 mutate(state = region,
                        region = NULL)

data <- full_join(mobility_data, case_data) %>%
        mutate(fips = replace_na(fips, 0),
               cases = replace_na(cases, 0),
               deaths = replace_na(deaths, 0)) %>%
        group_by(state) %>% 
        arrange(date, .by_group = TRUE) %>%
        mutate(daily_cases = (cases - (lag(cases)))) %>%
        ungroup() %>%
        mutate(daily_cases = replace_na(daily_cases, 0))

transpo <- c(unique(data$transportation_type))
vec_brks <- c(-50, 0, 50)
vec_labs <- vec_brks + 100
start_date <- sort(data$date)[1]
end_date <- sort(desc(data$date))[1]

ui <- fluidPage(
titlePanel("Apple U.S. Mobility Dataset during COVID-19 Crisis"),

fluidRow(
   column(5, h3("Please begin by selecting your desired date range: ")),
   #column(3, selectInput("transpo_type", label="Transportation type: ", transpo, width='60%')),
   column(3, dateRangeInput('dateRange', label = '', format='mm-dd-yyyy', separator=' to ',
                            start = '2020-01-13', end = '2020-05-13', min = '2020-01-13', max = '2020-05-13')),
   column(4, h5("Upper plot displays the daily increase in COVID-19 cases for selected location. 
                Lower plot shows corresponding Apple Maps' mobility data, in terms of trends in driving since mid-January 2020 (dark green
                indicates a relative increase, yellow indicates a relative decrease). Did less driving correspond to \"flattening the curve\"? We can find out!"))
),

hr(),

fluidRow(
   column(4, plotOutput("plot1")),
   column(4, plotOutput("plot2")),
   column(4, plotOutput("plot3"))
),

fluidRow(
   column(3, offset = 1, selectInput("loc1", label="Location 1", multiple=FALSE,
                                     locations, width='70%', size=5, selectize=FALSE)),
   column(3, offset = 1, selectInput("loc2", label="Location 2", multiple=FALSE,
                                     locations, width='70%', size=5, selectize=FALSE)),
   column(3, offset = 1, selectInput("loc3", label="Location 3", multiple=FALSE, 
                                     locations, width='70%', size=5, selectize=FALSE)),
),

h5("Data from Kieran Healy's covdata package: https://kjhealy.github.io/covdata/. Bottom plots *very heavily* inspired by apple_mobility dataset vignette!")
)

server <- function(input, output, session) {
   
# transpo_filter <- reactive({
#       if(input$transpo_type == 'all') {
#          unique(data$transportation_type)
#       } else {
#          input$transpo_type
#       }
# })
   
output$plot1 <- renderPlot({
   case_plot1 <- data %>%
      filter(state == input$loc1,
             #transportation_type %in% transpo_filter(),
             date >= input$dateRange[1] & date <= input$dateRange[2]) %>%
      ggplot(mapping=aes(x=date, y=daily_cases)) +
      geom_line() +
      scale_y_log10(limits=c(1, 3000)) +
      theme(legend.position = "none") +
      xlab("") +
      ylab("Daily Case\nIncrease")
   
     mobi_plot1 <- data %>% 
     filter(state == input$loc1,
            #transportation_type %in% transpo_filter(),
            date >= input$dateRange[1] & date <= input$dateRange[2]) %>% 
     mutate(over_under = index < 100,
              index = index - 100) %>%
     ggplot(mapping=aes(x=date, y=index, fill=over_under, col=over_under)) +
     geom_col() +
     scale_color_manual(values = c('#004D40', '#FFC107')) +
     scale_fill_manual(values = c('#004D40', '#FFC107')) +
     theme(legend.position = "none") +
     scale_y_continuous(limits = c(-150, 150)) +
     geom_hline(yintercept = 0, color = "gray40") +
     xlab("Date") +
     ylab("Mobility Index")
     
     ggarrange(case_plot1, mobi_plot1, heights = c(0.8, 2),
               ncol = 1, nrow = 2, align = 'v',
               legend = NULL) 
 })
 
output$plot2 <- renderPlot({
   case_plot2 <- data %>%
      filter(state == input$loc2,
             #transportation_type %in% transpo_filter(),
             date >= input$dateRange[1] & date <= input$dateRange[2]) %>%
      ggplot(mapping=aes(x=date, y=daily_cases)) +
      geom_line() +
      scale_y_log10(limits=c(1, 3000)) +
      theme(legend.position = "none") +
      xlab("") +
      ylab("Daily Case\nIncrease")
   
   mobi_plot2 <- data %>% 
      filter(state == input$loc2,
             #transportation_type %in% transpo_filter(),
             date >= input$dateRange[1] & date <= input$dateRange[2]) %>% 
      mutate(over_under = index < 100,
             index = index - 100) %>%
      ggplot(mapping=aes(x=date, y=index, fill=over_under, col=over_under)) +
      geom_col() +
      theme(legend.position = "none") +
      scale_y_continuous(limits = c(-150, 150)) +
      scale_color_manual(values = c('#004D40', '#FFC107')) +
      scale_fill_manual(values = c('#004D40', '#FFC107')) +
      geom_hline(yintercept = 0, color = "gray40") +
      xlab("Date") +
      ylab("Mobility Index")
   
   ggarrange(case_plot2, mobi_plot2, heights = c(0.8, 2),
             ncol = 1, nrow = 2, align = 'v',
             legend = NULL) 
})


output$plot3 <- renderPlot({
   case_plot3 <- data %>%
      filter(state == input$loc3,
             #transportation_type %in% transpo_filter(),
             date >= input$dateRange[1] & date <= input$dateRange[2]) %>%
      ggplot(mapping=aes(x=date, y=daily_cases)) +
      theme(legend.position = "none") +
      scale_y_log10(limits=c(1, 3000)) +
      geom_line() +
      xlab("") +
      ylab("Daily Case\nIncrease")
   
   mobi_plot3 <- data %>% 
      filter(state == input$loc3,
             #transportation_type %in% transpo_filter(),
             date >= input$dateRange[1] & date <= input$dateRange[2]) %>% 
      mutate(over_under = index < 100,
             index = index - 100) %>%
      ggplot(mapping=aes(x=date, y=index, fill=over_under, col=over_under)) +
      geom_col() +
      theme(legend.position = "none") +
      scale_color_manual(values = c('#004D40', '#FFC107')) +
      scale_fill_manual(values = c('#004D40', '#FFC107')) +
      scale_y_continuous(limits = c(-150, 150)) +
      geom_hline(yintercept = 0, color = "gray40") +
      xlab("Date") +
      ylab("Mobility Index")
   
   ggarrange(case_plot3, mobi_plot3, heights = c(0.8, 2),
             ncol = 1, nrow = 2, align = 'v',
                             legend = NULL) 
})

# valid_regions <- reactive({
#    mobility_data %>% 
#       filter(transportation_type %in% transpo_filter(),
#              is.na(index) == FALSE) %>% 
#       arrange(state) %>%
#       pull(state) %>%
#       unique()
# })

# observeEvent(valid_regions(), {
#    updateSelectInput(session, "loc1", 
#                      choices=valid_regions(),
#                      selected="Alabama")
# })
# 
# observeEvent(valid_regions(), {
#    updateSelectInput(session, "loc2", 
#                      choices=valid_regions(),
#                      selected="Alabama")
# })
# 
# observeEvent(valid_regions(), {
#    updateSelectInput(session, "loc3", 
#                      choices=valid_regions(),
#                      selected="Alabama")
#})

}

shinyApp(ui = ui, server = server)
