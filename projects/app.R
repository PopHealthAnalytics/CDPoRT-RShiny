library(tmap)
library(sf)
library(dplyr)
library(ggplot2)
library(plotly)
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
  # About page ----
  tabPanel(title = "About",
           fluidRow(column(
             12,
             h3("Welcome to the Chronic Disease Population Risk Tool (CDPoRT) dashboard!"), 
             p("Population-Based Prediction Tools (PoRTs), apply routinely collected population health data to a validated risk prediction 
               model to estimate the number of new and existing cases in a population of interest for the purpose of: 
               understanding distribution of risk in the population, intervention planning, resource planning, 
               and facilitating decision-making and priority setting."),
             p("CDPoRT is a validated sex-specific population based risk prediction tool that estimates the 10-year risk of 
               chronic disease in the adult population. CDPoRT was developed with a robust internal (two types of internal validation) 
               and external validation approach (in Manitoba). CDPoRT makes predictions based on risk factors routinely collected 
               from the", 
               a("Canadian Community Health Survey (CCHS).", href="https://www23.statcan.gc.ca/imdb/p2SV.pl?Function=getSurvey&SDDS=3226"),
               "More information on CDPoRT can be found at",
               a("Ng et al. 2020 JAMA Network Open.", 
                 href="https://jamanetwork.com/journals/jamanetworkopen/fullarticle/2766780"),
               "Below, you can find two video explanations of CDPoRT."),
             tags$video(src = "Video-Chapter1-Introduction.mp4", type = "video/mp4", width = "600", height = "360", controls = TRUE),
             tags$video(src = "Video-Chapter2-Overview.mp4", type = "video/mp4", width = "600", height = "360", controls = TRUE)
           )),
           fluidRow(column(
             12,
             h3("About the creators:"), 
           )),
           fluidRow(column(
             6,
             h4("Population Health Analytics Lab (PHAL)"), 
             p("The", 
              a("PHAL's", href="https://pophealthanalytics.com/"),
              "work draws from demographic, clinical, behavioural, social, and health outcomes information. From these sources, we gain a 
               comprehensive perspective on population health, allowing us to inform decision-making related to improved health system performance, 
               reduced inequities, and fiscal sustainability. Most importantly, we work directly with key health system decision makers, which maximizes the meaningfulness
               of our studies, and enhances their real-world impact."),
             p("The PHAL is based at the University of Toronto’s",
               a("Dalla Lana School of Public Health", href="https://www.dlsph.utoronto.ca/"),
               "— the largest public health school in Canada, with more than 800 faculty, 850 students, and research and training partnerships with institutions throughout Toronto and the world. 
               With $34.4 million in research funding per year, the School supports discovery in global health, tobacco impacts on health, occupational disease and disability, air pollution, 
               inner city and Indigenous health, among many other areas."),
             img(src = "Logo-PHAL.png",  width = "600px", height = "100")
           ),
           column(
             6,
             h4("Human Factors & Applied Statistics Lab (HFASt)"),
             p("The",
               a("HFASt Lab", href="https://hfast.mie.utoronto.ca/"), 
               "conducts research on understanding and improving human behaviour and performance in multi-task and complex situations, using a wide range of analytical techniques. 
               The application areas include surface transportation, healthcare, mining, and unmanned vehicle supervisory control."),
             p("The HFASt Lab is based at the University of Toronto's",
               a("Faculty of Applied Science & Engineering", href="https://www.engineering.utoronto.ca/"),
               "— the top engineering school in Canada, with more than 280 faculty, 8000 students, and 400+ collaborating industrial research partners worldwide.  With $108.6 
               million in research funding, the Faculty supports research guided by 6 innovation clusters including advanced manufacturing, data analytics & artificial intelligence, 
               human health, robotics, sustainability, and water."),
             img(src = "Logo-HFast.png", width="600px", height="175px")
           )),
           fluidRow(column(
             12,
             h3("Contact:"), 
             p("If you have questions about CDPoRT please reach out to",
               a("pophealthanalytics.dlsph@utoronto.ca", href="mailto: pophealthanalytics.dlsph@utoronto.ca"))
           )), 
           fluidRow(column(
             12,
             h3("The development of CDPoRT was generously supported by:"),
             img(src = "Logo-UofT.png", width="250px", height="150px"),
             img(src = "Logo-DSI.png", width="250px", height="150px"),
             img(src = "Logo-DLSPH.png", width="350px", height="100px", style = "margin-right: 30px;"),
             img(src = "Logo-CRC1.png", width="100px", height="100px", style = "margin-left: 30px;"),
             img(src = "Logo-CRC2.png", width="100px", height="100px"),
           )),
           fluidRow(column(
             12,
             div(style = "display: flex;",
                 div(style = "margin-right: 50px",
                     img(src = "Logo-FASE.png", width = "600px", height = "100px")),
                 img(src = "Logo-MIE.png", width = "450px", height = "100px"))
           )),
          ),
  # Upload demo page ----
  tabPanel(title = "Upload demo",
           fluidRow(column(
             12,
             p("RShiny dashboards are interactive. Upload a pre-processed CSV file from your computer to see a map.")
           )),
           fluidRow(column(
             12, fileInput(
               inputId = "user_csv",
               label = h3("CSV Upload")
             )
           )),
           fluidRow(column(
             12,
             tmapOutput("user_map")
           )), ),
  # Overview page----
  tabPanel(title = "Overview",
           fluidRow(column(
             12,
             p("Summary of the projected number of new cases and ten-year risk for chronic diseases in each 
               health region and Ontario as a whole. By default, health regions are sorted alphabetically, but 
               you can also sort by cases and risk by clicking on the arrows to the right of each column name.")
           )),
           fluidRow(column(
             12, div(style = "margin-bottom: 3px", downloadButton("download_overview"))
           )),
           fluidRow(column(
             12, dataTableOutput("basic_table")
           ))),
  # Geography page ----
  tabPanel(title = "Explore by Geography",
           fluidRow(column(
             12,
             p("Maps make it easy to compare regions at a glance. Select your metric of interest using the radio 
                buttons. Hover over a region to see it's name and click to see the value.")
           )),
           radioButtons(inputId = "map_var" , label = "Select the variable to map", choices = c("Cases", "Risk")),
           tmapOutput("map")),
  # Stratified view page ----
  tabPanel(title = "Stratified", fluidPage(
    fluidRow(column(
      12,
      p("View chronic disease projections within each health region and Ontario as a whole stratified by age and sex.
         Use the drop-down menus to select your region, stratifier, and metric of interest.")
    )),
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
    fluidRow(column(12, plotlyOutput('user_plot'))),
    fluidRow(column(12, dataTableOutput('stratified_table')))
  ), )
) # End defining UI ----

server <- function(input, output) {
  user_csv <- reactive({input$user_csv})
  
  formatted_csv <- reactive({
    # https://shiny.rstudio.com/articles/req.html
    req(user_csv())
    process_csv(user_csv()$datapath)
  })
  
  output$user_map <- renderTmap({
    req(user_csv())
    tm_shape(formatted_csv()) + tm_borders() + tm_fill(col = WEIGHTED_ALIAS, id =
                                                         "Region Name")
  })

  output$map <- renderTmap({
    if (input$map_var == "Cases"){
      tm_shape(merged) + tm_borders() + tm_fill(col = WEIGHTED_ALIAS, id = "Region Name")
    }else{
      tm_shape(merged) + tm_borders() + tm_fill(col = MEAN_ALIAS, id = "Region Name")
    }
  })
  
  overview = reactive({
    merged %>%
      st_drop_geometry %>%
      select("Region Name",
              WEIGHTED_ALIAS,
              MEAN_ALIAS) %>% 
      mutate_if(is.numeric, ~round(., digits=0))
  })
  
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
  
  output$user_plot <- renderPlotly({
    p <- ggplot(filtered_by_region(),
           aes(x = !!sym(input$plotX), 
               y = filtered_by_region()[[input$plotY]],
               text = paste(input$plotY,":",
                            filtered_by_region()[[input$plotY]]),
               fill = !!sym(input$plotX))) + 
           geom_bar(stat = "identity", show.legend=FALSE) + 
           scale_fill_manual(values=stratified_table_palette())+
           labs(x=str_to_title(input$plotX), y=input$plotY)+
           theme_bw()+theme(legend.position='none')+
           theme(axis.title.x=element_text(face="bold"),
                 axis.title.y=element_text(face="bold"))
    ggplotly(p, tooltip = "text")
  })
  
  output$stratified_table <- renderDataTable({
   stratified_table() %>% 
      filter(region == input$plotRegion) %>% 
      select(-c(region, weighted, mean)) %>%
      rename_at(c(1), .funs=str_to_title) %>% 
      select(1, input$plotY)
     
  })
} #End server 

shinyApp(ui = ui, server = server)
