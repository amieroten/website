library(shiny)
library(babynames)
library(gt)

topNames <- babynames %>% 
  filter(year==1988) %>% 
  group_by(sex) %>% 
  arrange(prop) %>% top_n(5) %>% 
  ungroup() %>% select(name)

ui <- fluidPage(
  titlePanel("Name Measurement"),
  numericInput("year", label="Year", min=1880, max=2017, value=2017),
  selectInput("name", label="Which Name?", c()),
  fluidRow(
    column(6, plotOutput("namePlot")),
    column(6, gt_output("nameTable"))
  )
)

server <- function(input, output, session) {
  output$namePlot <- renderPlot({
    babynames %>% 
      filter(name==input$name) %>% 
      ggplot(mapping=aes(x=year, y=prop, color=sex)) + 
      geom_line() + 
      ggtitle(paste0("Name Frequency: ", input$name)) 
  })
  
  output$nameTable <- render_gt({
    babynames %>% 
      filter(name==input$name) %>% 
      group_by(sex) %>% 
      arrange(year) %>% 
      top_n(1, prop) %>% 
      select(-name) %>% ungroup() %>% select(sex, year, prop) %>% 
      arrange(sex) %>% 
      gt() %>% 
      cols_label(sex="Sex", year="Year", prop="Proportion") %>% 
      fmt_number(columns=vars(prop), decimals=5) %>% 
      tab_header(title=paste0("Year of Maximum ", input$name, ", by sex"))  %>% 
      tab_source_note(md("From `babynames` package"))
    
  })
  
  namesToUse <- reactive({
    babynames %>% 
      filter(year==input$year) %>% 
      group_by(sex) %>% 
      arrange(prop) %>% top_n(5) %>% 
      ungroup() %>% select(name)
  })
  
  observeEvent(namesToUse(), {
    updateSelectInput(session, "name", 
                      choices=namesToUse(),
                      label=paste0("Popular names in ", input$year)
    )
  })
  
}



shinyApp(ui = ui, server = server)


