library(shiny)
library(tidyverse)
library(ggplot2)
library(censusapi)
library(dplyr)

Sys.setenv(CENSUS_KEY="2af9f600486ef6ad342ed1e8a978c0956a70a52c")
readRenviron("~/.Renviron")
Sys.getenv("CENSUS_KEY")

## Data Extraction for pLot 1
cbp <- getCensus( name = "cbp" ,
                      vintage = 2020,
                      key = Sys.getenv("CENSUS_KEY"),
                      vars = c("NAME","NAICS2017","NAICS2017_LABEL","PAYANN","EMP","ESTAB"),
                      region = "county:*",
                      regionin = "state:11,24,51")
cbp$NAICS2017_LABEL <- gsub("Real estate and rental and leasing","Real Estate",cbp$NAICS2017_LABEL)
cbp$NAICS2017_LABEL <- gsub("Agriculture, forestry, fishing and hunting","Agriculture",cbp$NAICS2017_LABEL)
cbp$NAICS2017_LABEL <- gsub("Professional, scientific, and technical services","Technical Services",cbp$NAICS2017_LABEL)
cbp$NAICS2017_LABEL <- gsub("Administrative and support and waste management and remediation services","Admin/Support",cbp$NAICS2017_LABEL)
cbp$NAICS2017_LABEL <- gsub("Other services (except public administration)","Other",cbp$NAICS2017_LABEL,fixed = TRUE)
cbp$NAICS2017_LABEL <- gsub("Mining, quarrying, and oil and gas extraction","Mining",cbp$NAICS2017_LABEL)
cbp$NAICS2017_LABEL <- gsub("Health care and social assistance","Healthcare",cbp$NAICS2017_LABEL)
cbp$NAICS2017_LABEL <- gsub("Arts, entertainment, and recreation","Entertainment",cbp$NAICS2017_LABEL)
cbp$NAICS2017_LABEL <- gsub("Accommodation and food services","Accomodation",cbp$NAICS2017_LABEL)
cbp$NAICS2017_LABEL <- gsub("Management of companies and enterprises","MGMT of Companies",cbp$NAICS2017_LABEL)
cbp$state <- gsub("51","Virginia",cbp$state)
cbp$state <- gsub("24","Maryland",cbp$state)
cbp$state <- gsub("11","District of Columbia",cbp$state)

cbp <- cbp %>% 
  as_tibble() %>% 
  filter(NAICS2017 %in% c("11","21","22","23","31","42","44","48","51","52","53","54","55","56","61","62","71","72", "81","99"))

stateoptions <- c("Virginia","Maryland","District of Columbia")

ui <- fluidPage(
  titlePanel("Analysis of County Business Patterns Data"),
  selectInput("states1", "States", 
              choices = stateoptions),
  plotOutput("plot1"))


server <- function(input, output, session) {
  
  filtered_data1 <- reactive({
    dplyr::filter(cbp,state == input$states1)
  })
  
  output$plot1 <- renderPlot({
  ggplot(filtered_data1())+
      geom_boxplot(aes(x=NAICS2017_LABEL, y= log(EMP)))+
      geom_point(aes(x=NAICS2017_LABEL, y= log(EMP), fill = log(EMP)),
                 position=position_jitterdodge(),
                 alpha = 0.5)+
      labs(title = "Number of Employees Hired in DMV by industry",
           y = "Log of Number of Employees",
           x = NULL)+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
            legend.position = "none")
})
}
shinyApp(ui = ui, server = server)



## Data Extraction for plot 2

cbp20 <- getCensus( name = "cbp" ,
                      vintage = 2020,
                      key = Sys.getenv("CENSUS_KEY"),
                      vars = c("NAME","ESTAB","EMP","PAYANN"),
                      region = "state:*")
cbp19 <- getCensus( name = "cbp" ,
                    vintage = 2019,
                    key = Sys.getenv("CENSUS_KEY"),
                    vars = c("NAME","ESTAB","EMP","PAYANN"),
                    region = "state:*")
cbp18 <- getCensus( name = "cbp" ,
                    vintage = 2018,
                    key = Sys.getenv("CENSUS_KEY"),
                    vars = c("NAME","ESTAB","EMP","PAYANN"),
                    region = "state:*")
cbp17 <- getCensus( name = "cbp" ,
                    vintage = 2017,
                    key = Sys.getenv("CENSUS_KEY"),
                    vars = c("NAME","ESTAB","EMP","PAYANN"),
                    region = "state:*")
cbp16 <- getCensus( name = "cbp" ,
                    vintage = 2016,
                    key = Sys.getenv("CENSUS_KEY"),
                    vars = c("NAME","ESTAB","EMP","PAYANN"),
                    region = "state:*")
cbp15 <- getCensus( name = "cbp" ,
                    vintage = 2015,
                    key = Sys.getenv("CENSUS_KEY"),
                    vars = c("NAME","ESTAB","EMP","PAYANN"),
                    region = "state:*")
cbp14 <- getCensus( name = "cbp" ,
                    vintage = 2014,
                    key = Sys.getenv("CENSUS_KEY"),
                    vars = c("NAME","ESTAB","EMP","PAYANN"),
                    region = "state:*")
cbp13 <- getCensus( name = "cbp" ,
                    vintage = 2013,
                    key = Sys.getenv("CENSUS_KEY"),
                    vars = c("NAME","ESTAB","EMP","PAYANN"),
                    region = "state:*")
cbp12 <- getCensus( name = "cbp" ,
                    vintage = 2012,
                    key = Sys.getenv("CENSUS_KEY"),
                    vars = c("NAME","ESTAB","EMP","PAYANN"),
                    region = "state:*")
cbp_all <- left_join(cbp20, cbp19, by = c("NAME","state")) %>% 
  left_join(., cbp18, by=c("NAME","state")) %>% 
  left_join(., cbp17, by=c("NAME","state")) %>% 
  left_join(., cbp16, by=c("NAME","state")) %>% 
  left_join(., cbp15, by=c("NAME","state")) %>% 
  left_join(., cbp14, by=c("NAME","state")) %>% 
  left_join(., cbp13, by=c("NAME","state")) %>% 
  left_join(., cbp12, by=c("NAME","state")) %>% 
  rename("ESTAB20"="ESTAB.x",
         "ESTAB19"="ESTAB.y",
         "ESTAB18"="ESTAB.x.x",
         "ESTAB17"="ESTAB.y.y",
         "ESTAB16"="ESTAB.x.x.x",
         "ESTAB15"="ESTAB.y.y.y",
         "ESTAB14"="ESTAB.x.x.x.x",
         "ESTAB13"="ESTAB.y.y.y.y",
         "ESTAB12"="ESTAB",
         "EMP20"="EMP.x",
         "EMP19"="EMP.y",
         "EMP18"="EMP.x.x",
         "EMP17"="EMP.y.y",
         "EMP16"="EMP.x.x.x",
         "EMP15"="EMP.y.y.y",
         "EMP14"="EMP.x.x.x.x",
         "EMP13"="EMP.y.y.y.y",
         "EMP12"="EMP",
         "PAYANN20"="PAYANN.x",
         "PAYANN19"="PAYANN.y",
         "PAYANN18"="PAYANN.x.x",
         "PAYANN17"="PAYANN.y.y",
         "PAYANN16"="PAYANN.x.x.x",
         "PAYANN15"="PAYANN.y.y.y",
         "PAYANN14"="PAYANN.x.x.x.x",
         "PAYANN13"="PAYANN.y.y.y.y",
         "PAYANN12"="PAYANN") 


cbp_ESTAB <- cbp_all %>% 
  select(state, NAME, contains("ESTAB")) %>% 
  pivot_longer(
    cols = contains("ESTAB"),
    names_to = "YEAR",
    names_prefix = "ESTAB",
    values_to = "ESTAB"
  )
cbp_EMP <- cbp_all %>% 
  select(state, NAME, contains("EMP")) %>% 
  pivot_longer(
    cols = contains("EMP"),
    names_to = "YEAR",
    names_prefix = "EMP",
    values_to = "EMP"
  )
cbp_PAYANN <- cbp_all %>% 
  select(state, NAME, contains("PAYANN")) %>% 
  pivot_longer(
    cols = contains("PAYANN"),
    names_to = "YEAR",
    names_prefix = "PAYANN",
    values_to = "PAYANN"
  )
cbp_all_merged <- left_join(cbp_ESTAB, cbp_EMP, by = c("NAME","state","YEAR")) %>% 
  left_join(., cbp_PAYANN, by=c("NAME","state","YEAR")) %>% 
  mutate(YEAR = recode(YEAR, 
                       '20'= '2020',
                       '19'='2019',
                       '18'='2018',
                       '17'='2017',
                       '16'='2016',
                       '15'='2015',
                       '14'='2014',
                       '13'='2013',
                       '12'='2012'),
         YEAR = as.numeric(YEAR))



  
ui <- fluidPage(
  titlePanel("Analysis of County Business Patterns Data"),
  selectInput("states1", "States", 
              choices = cbp_all_merged$NAME),
  selectInput("states2", "States", 
              choices = cbp_all_merged$NAME),
  selectInput("states3", "States", 
              choices = cbp_all_merged$NAME),
  selectInput("states4", "States", 
              choices = cbp_all_merged$NAME),
  selectInput("states5", "States", 
              choices = cbp_all_merged$NAME),
   plotOutput(outputId = "plot"))

server <- function(input, output,session) {

  filtered_data1 <- reactive({
    dplyr::filter(cbp_all_merged,NAME == input$states1)
  })
  filtered_data2 <- reactive({
    dplyr::filter(cbp_all_merged,NAME == input$states2)
  })
  filtered_data3 <- reactive({
    dplyr::filter(cbp_all_merged,NAME == input$states3)
  })
  filtered_data4 <- reactive({
    dplyr::filter(cbp_all_merged,NAME == input$states4)
  })
  filtered_data5 <- reactive({
    dplyr::filter(cbp_all_merged,NAME == input$states5)
  })
  
  output$plot <- renderPlot({
    ggplot()+
      geom_line(data=filtered_data1(),aes(x=YEAR, y=log(ESTAB)), color ="blue")+
      geom_line(data=filtered_data2(),aes(x=YEAR, y=log(ESTAB)), color ="green")+
      geom_line(data=filtered_data3(),aes(x=YEAR, y=log(ESTAB)), color ="red")+
      geom_line(data=filtered_data4(),aes(x=YEAR, y=log(ESTAB)), color ="purple")+
      geom_line(data=filtered_data5(),aes(x=YEAR, y=log(ESTAB)), color ="yellow")
  })
}

shinyApp(ui = ui, server = server)


