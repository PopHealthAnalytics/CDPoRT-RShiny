library(tmap)
library(sf)
library(dplyr)
library(ggplot2)
library(tibble)
library(stringr)

# The public health boundary file
phub <- st_read("./data/public_health_unit_boundaries.geojson")

# Assign column names to constants so they're easier to work with
WEIGHTED_ALIAS <- "Number of New Cases (1000s)"
MEAN_ALIAS <- "Ten-Year Risk per 100,000"

#' This function is for demonstration purposes with the upload csv functionality.
#' @param filepath  csv with a `region` column label and no leading comments
process_csv <- function(filepath) {
  user_csv <- read.csv(filepath)
  # construct merge columns from last 2 digits of PHU_ID and last 2 digits of region
  user_csv$short_id <- substr(user_csv$region, 4, 5)
  phub$short_id <- substr(phub$PHU_ID, 3, 4)
  merged <- merge(phub, user_csv, by = 'short_id')
  merged %>%
    rename("Region Name" = "NAME_ENG", "Region ID" = "region") %>%
    mutate(!!WEIGHTED_ALIAS := weighted / 10000,!!MEAN_ALIAS := mean * 10000)
}

merged <- process_csv("data/CDPORT export overall.csv")

regions = deframe(merged %>% st_drop_geometry %>% select('Region Name', 'Region ID'))

# age and sex tables for stratifying
cdport_by_age <- read.csv("data/CDPORT export age.csv") %>% 
                 mutate(!!WEIGHTED_ALIAS := weighted / 10000,
                        !!MEAN_ALIAS := mean * 10000)

cdport_by_sex <- read.csv("data/CDPORT export sex.csv") %>% 
                 mutate(!!WEIGHTED_ALIAS := weighted / 10000,
                        !!MEAN_ALIAS := mean * 10000) %>% 
                 mutate(sex = case_when(sex == 0 ~ "Female", 
                                        sex == 1 ~ "Male"))

ui <- navbarPage(
  "CDPoRT",
  tabPanel(title = "Upload demo",
           fluidRow(column(
             12, fileInput(
               inputId = "user_csv",
               label = h3("Upload a preprocessed csv to see a map")
             )
           )),
           fluidRow(column(
             12,
             tmapOutput("user_map")
           )), ),
  tabPanel(title = "Overview",
           fluidRow(column(
             12, div(style = "margin-bottom: 3px", downloadButton("download_overview"))
           )),
           fluidRow(column(
             12, dataTableOutput("basic_table")
           ))),
  tabPanel(title = "Explore by Geography", tmapOutput("map")),
  tabPanel(title = "Stratified", fluidPage(
    fluidRow(
      column(4,
             selectInput(
               "plotX",
               label = "Select X",
               choices = c("Age" = "age", "Sex" = "sex")
             )),
      column(4, selectInput(
        "plotY",
        label = "Select Y",
        choices = c(WEIGHTED_ALIAS, MEAN_ALIAS)
      )),
      column(
        4,
        selectInput("plotRegion",
                    label = "Select Region",
                    choices = regions)
      ),
    ),
    fluidRow(column(12, plotOutput('user_plot')))
  ), )
)

server <- function(input, output) {
  user_csv <- reactive({input$user_csv})
  
  formatted_csv <- reactive({
    # https://shiny.rstudio.com/articles/req.html
    req(user_csv())
    process_csv(user_csv()$datapath)
  })
  
  #FIXME - Causing problems with reactive elements
  # output$user_map <- renderTmap({
  #   req(user_csv())
  #   tm_shape(formatted_csv()) + tm_borders() + tm_fill(col = WEIGHTED_ALIAS, id =
  #                                                        "Region Name")
  # })
  # 
  # output$map <- renderTmap({
  #   tm_shape(merged) + tm_borders() + tm_fill(col = WEIGHTED_ALIAS, id = "Region Name")
  # })
  
  overview = reactive(
    merged %>%
      st_drop_geometry %>%
      select("Region Name",
              WEIGHTED_ALIAS,
              MEAN_ALIAS) %>% 
      mutate_if(is.numeric, ~round(., digits=0))
  )
  
  output$basic_table <- renderDataTable({
    overview()
  })
  
  output$download_overview <- downloadHandler(
    filename = function() {
      "overview.csv"
    },
    content = function(file) {
      write.csv(overview(), file)
    }
  )
  
  stratified_table = reactive({
    if (input$plotX == "sex") {
      cdport_by_sex
    } else {
      cdport_by_age
    }
  })
  
  stratified_table_palette = reactive({
    if (input$plotX == "sex"){
      c("#DB0085", "#0147AB")
    } else {
      c("#F2E4FD", "#D9ADFA", "#C585F7", "#6F2AF8", "#827892", "#878787", "#222222")
    }
  })
  
  filtered_by_region = reactive({
    filter(stratified_table(), region == input$plotRegion)
  })
  
  output$user_plot <- renderPlot({
    ggplot(filtered_by_region(),
           aes(x = !!sym(input$plotX), 
               y = filtered_by_region()[[input$plotY]],
               fill = !!sym(input$plotX))) + 
           geom_bar(stat = "identity", show.legend=FALSE) + 
           scale_fill_manual(values=stratified_table_palette())+
           labs(x=str_to_title(input$plotX), y=input$plotY)+
           theme_bw()+
           theme(axis.title.x=element_text(face="bold"),
                 axis.title.y=element_text(face="bold"))
  })
} # end server 

shinyApp(ui = ui, server = server)
