
# Based on https://github.com/Sarah-2510/R-Shiny-Project---AIR-QUALITY-INDEX/blob/main/Rshiny%20final.R
# Last update: 2024-08-25

library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinycssloaders)
library(jsonlite)
library(DT)
library(leaflet)
library(leaflegend)
library(Hmisc)
library(corrplot)
library(PerformanceAnalytics)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(data.table)
library(tidyverse)
library(devtools)
library(openair)
library(openairmaps)


#  --------------------------------------------------------------------------------------------------------
#                                              READING THE FILES
#  --------------------------------------------------------------------------------------------------------
devtools::source_url("https://raw.githubusercontent.com/jessicajcss/Shiny_RMC/main/01-AQI_calculation_thermo_data.R")


localizacao <- read.csv("https://raw.githubusercontent.com/jessicajcss/Shiny_RMC/main/data/locais_sensores.csv",
                        sep = ";")


##### UNITS ACCORDING TO WHO AQG ----
Datafinal <- air_quality_data %>%
  # UNIT CONVERSION: https://www.breeze-technologies.de/blog/air-pollution-how-to-convert-between-mgm3-%C2%B5gm3-ppm-ppb/
  mutate(CO = CO*1.15, #from ppm to mg/m³
         O3 = O3*1.96, #from ppb to ug/m³
         NO2 = NO2*1.88, #from ppb to ug/m³
         SO2 = SO2*2.62, #from ppb to ug/m³
         PM2.5 = PM2.5, # ug/m³
         PM10 = PM10) #ug/m³

names(Datafinal)[names(Datafinal) == 'sample_day'] <- 'Date'



##### LOCATION: adding lat and long ----

thermo_localizacao <- localizacao %>%
  subset((Local == "Defesa Civil" | Local == "Prefeitura") & Tipo == 'outdoor') %>%
  subset(Cidade == "Almirante Tamandaré" | Cidade == "Rio Branco do Sul") %>%
  select(Cidade, Lat, Long)

colnames(thermo_localizacao) <- c('Cidade', 'Latitude', 'Longitude')

Datafinal <- left_join(Datafinal, thermo_localizacao, by = "Cidade")

Datafinal <- mutate(Datafinal, Year = format(Date,"%Y"))
Year <- unique(Datafinal$Year)
Cidade <- unique(Datafinal$Cidade)




# layout
title <- tags$img(src='https://raw.githubusercontent.com/jessicajcss/Shiny_RMC/main/www/ufpr.png', height='30', # width='46',
                  "Qualidade do Ar & Meteorologia", align = "left")
# ---------------------------------------------------------------------------------------------------------
#                                                USER INTERFACE
# ---------------------------------------------------------------------------------------------------------

ui <- dashboardPage(
  skin = 'yellow',
  dashboardHeader(
    title = "Qualidade do Ar & Meteorologia",
    titleWidth = 330),
  dashboardSidebar(
    width = 330,
    sidebarMenu(
      menuItem("Página Inicial", tabName = "home", icon = icon('home')),
      menuItem(("Mapa de Sensores"),tabName = "dashboard",icon=icon('map')),
      menuItem(HTML((paste0("MP",tags$sub("2.5"), " em tempo real"))), tabName = "purpleair",icon = icon('map')),
      menuItem(("Variação Temporal"), tabName = "Line_graph",icon = icon('chart-line')),
      menuItem(("Tendências de Poluição"), tabName = "year_data", icon = icon('th')),
      menuItem("Dados Brutos",tabName = "table",icon = icon('table'))
    )
  ),

  dashboardBody(
    #custom css
    tags$head(
      tags$style(
        HTML(" #compare_state_option,#compare_year_option ,.compare-county-wrapper { display:flex; margin-bottom:-10px;}
    				#compare_state_option > div, #compare_year_option > div, .compare-county-wrapper > div > div {padding-right: 15px;}
    				.shiny-output-error:before,.shiny-output-error{visibility: hidden !important;}
    				.compare-class > .tabbable > .nav-tabs {margin-bottom: 20px !important;}
    				.box.box-solid.box-primary {border: 1px solid #dd4b39 !important;}
    				.box.box-solid.box-primary>.box-header { background: #dd4b39 !important; background-color: #dd4b39 !important; }
    				.sidebar-menu>li {font-size:17px;}")
      )
    ),
    #--------------------------------------------RAW DATA TAB---------------------------------------------------------
    tabItems(
      tabItem(tabName = "table",
              tags$h3('Download de dados', width = 3),
              fluidRow(
                column(3,
                       dateInput("start_date",h3("Data inicial"),
                                 format = "yyyy-mm-dd",
                                 value="2023-08-10",
                                 min="2023-08-10",
                                 max=Sys.Date()),


                       dateInput("end_date",h3("Data final"),
                                 format = "yyyy-mm-dd",
                                 value=Sys.Date(),
                                 min="2023-08-10",
                                 max=Sys.Date())),
                column(9,
                       tableOutput("tableData"))
              ),
              br(),
              br(),
              downloadButton("downloadData"),

              # box(title = "Dataset",solidHeader = TRUE,status = "primary",height="100%", width =12,

      ),
      # ---------------------------------------------HOME TAB-------------------------------------------------------------


      tabItem(tabName = "home",
              fluidRow(
                column(width=7,align = "center",
                       tags$h2(width=5,"Inventário de material particulado em municípios com atividades minerais estabelecidas")),
                HTML('<html>
                            <head>
                            <style>
                            table {
                              font-family: arial, sans-serif;
                              border-collapse: collapse;
                              width: 100%;
                            }

                            td, th {
                              border: 1px solid #dddddd;
                              text-align: left;
                              padding: 8px;
                            }

                            tr:nth-child(even) {
                              background-color: #dddddd;
                            }

                            h3,h2{
                              font-weight:bold;
                            }
                            p{
                              font-size: 19px;
                            }
                            </style>
                            </html>'),
                column(width=5,tags$img(src="https://raw.githubusercontent.com/jessicajcss/Shiny_RMC/main/www/labair.png", height=200, align = "center"))),
              br(),
              fluidRow(
                column(width=12,tags$h2(width=5,"Página do Projeto"))),
              fluidRow(
                box(width=12, tags$iframe(
                  seamless = "seamless",
                  src = "https://rmcqualidadedoar.netlify.app/",
                  height = 430, width = '100%'))),
              fluidRow(
                column(width=12,tags$h2(width=5,"Video Of the Day")),
                box(width=12,HTML('<iframe width="100%" height="430" src="https://ai.invideo.io/watch/fY5jyjM1VR2"
                       frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media;
                       gyroscope; picture-in-picture" allowfullscreen></iframe>'))),
              br(),
              fluidRow(
                column(width=12,tags$h2(width=5,"Locais de Monitoramento")),
                box(title = "Qualidade do Ar e Meteorológico",solidHeader = TRUE, status = "primary",height=650,
                    width = 12,leafletOutput(height = 590,"sites")))
              # box(width=12,HTML('<iframe width="100%" height="430" src="https://rmcqualidadedoar.netlify.app/" clipboard-write; encrypted-media></iframe>'))), # IT MUST END WITH </iframe>
      ),

      # -------------------------------------------------MAP PURPLEAIR PAGE------------------------------------------------------------------

      tabItem(tabName = "purpleair",
              fluidRow(
                box(width=12,
                    tags$iframe(
                      seamless = "seamless",
                      src = "https://map.purpleair.com/1/lt/mPM25/a525600/p86400/cC5#9.7/-25.3517/-49.2683",
                      height = 800, width = "100%")))
      ),



      # -------------------------------------------------MAP AND BAR GRAPH TAB------------------------------------------------------------------
      tabItem(tabName = "dashboard",
              fluidRow(
                column(3,
                       dateInput("select_date",
                                 h3("Select Date"),
                                 format = "yyyy-mm-dd",
                                 value="2024-06-16",
                                 min="2023-08-10",
                                 max=Sys.Date()))),
              fluidRow(
                box(title = "Mapa",solidHeader = TRUE, status = "primary",height=650,
                    width = 6,leafletOutput(height = 590,"map")),
                box(title = "Distribuição dos poluentes",solidHeader = TRUE, status = "primary",width = 6,
                    tabBox(width=12,
                           tabPanel(title=HTML((paste0("SO",tags$sub("2")))),
                                    plotOutput(height = 500, "plotSO2")),
                           tabPanel(title=HTML((paste0("NO",tags$sub("2")))),
                                    plotOutput(height = 500, "plotNO2")),
                           tabPanel(title=HTML((paste0("O",tags$sub("3")))),
                                    plotOutput(height = 500, "plotO3")),
                           tabPanel(title="CO",
                                    plotOutput(height = 500, "plotCO")),
                           tabPanel(title=HTML((paste0("MP",tags$sub("2.5")))),
                                    plotOutput(height = 500,"plotPM25")),
                           tabPanel(title=HTML((paste0("MP",tags$sub("10")))),
                                    plotOutput(height = 500,"plotPM10"))
                    ))),
              br(),
              fluidRow(#DTOutput(outputId = "tableAQI", width = '70%')
                box(title = "Índice de Qualidade do Ar (IQA)",
                    footer = "Referência: <https://aqicn.org/scale/pt/>",
                    #background = "yellow",
                    #width = '100%',  height = 700,
                    # tableOutput("tableAQI")
                    #column(
                    width=12,
                    tags$img(width = '90%', src="https://raw.githubusercontent.com/jessicajcss/Shiny_RMC/main/www/aqi_table.png", align = "center"))
              ),

      ),
      # ---------------------------------------------------------------LINEGRAPH TAB---------------------------------------------------------
      tabItem(tabName = "Line_graph",
              fluidRow(
                column(3,
                       box(title = "Inputs",solidHeader = TRUE, status = "primary", width =12,height=600,
                           selectInput("Cities1",h3("Escolha uma Cidade"), Cidade, selected = 'Rio Branco do Sul'),
                           radioButtons("Poluente",
                                        "Poluente:",
                                        choiceNames = list(
                                          HTML((paste0("SO",tags$sub("2")))),
                                          HTML((paste0("NO",tags$sub("2")))),
                                          HTML((paste0("O",tags$sub("3")))),
                                          "CO",
                                          HTML((paste0("MP",tags$sub("2.5")))), #https://shiny.posit.co/r/articles/build/tag-glossary/ || https://shiny.posit.co/r/reference/shiny/latest/radiobuttons || https://stackoverflow.com/questions/67227902/how-to-add-text-formatting-like-superscript-or-subscript-to-a-label-in-a-shiny-a
                                          HTML((paste0("MP",tags$sub("10"))))
                                        ),
                                        choiceValues = list(
                                          "SO2", "NO2", "O3", "CO", "PM2.5", "PM10"
                                        )),

                           dateInput("start_date",h3("Data inicial"),
                                     format = "yyyy-mm-dd",
                                     value="2023-08-10",
                                     min="2023-08-10",
                                     max=Sys.Date()),


                           dateInput("end_date",h3("Data final"),
                                     format = "yyyy-mm-dd",
                                     value="2024-06-01",
                                     min="2023-08-10",
                                     max=Sys.Date())




                       )
                ),


                column(9,
                       box(title = "Série Temporal",solidHeader=TRUE, status = "primary",width = 12,height=600,
                           box(width=12,plotOutput(height = 500,"plots")))),
                box(title = "Concentrações e ventos",solidHeader = TRUE, status = "primary",height=650,
                    width = 12,leafletOutput(height = 590,"map_polarplot"))
              ),
              fluidRow(
                box(title="Condições Meteorológicas",
                    status="primary",
                    solidHeader=TRUE,
                    plotOutput("dist"),
                    width=8),
                box(title="Rosa dos ventos",
                    status="primary",
                    solidHeader=TRUE,
                    plotOutput("wrose"),
                    width=4)
              )
      ),


      # -----------------------------------------------------------CORRELATION MATRIX TAB--------------------------------------------------
      tabItem(tabName = "year_data",
              fluidRow(column(4,selectInput("Cities", ("Escolha uma Cidade:"),Cidade,selected = 'Rio Branco do Sul')),
                       column(8,selectInput("years",("Escolha um Ano:"),Year,selected="2024"))),
              fluidRow(
                column(6,
                       box(title = "Matriz de Correlação", solidHeader = TRUE, status = "primary", width = 12,
                           tabsetPanel(
                             tabPanel("Coeficientes de correlação", withSpinner(plotOutput("corrcoeff",height = 475))),
                             tabPanel("Correlação", withSpinner(plotOutput("corrscatt",height = 475))),
                             tabPanel("Heat map", withSpinner(plotOutput("heatmap",height = 475)))

                           )
                       )

                ),
                column(6,
                       box(title = "Risco e Prevenção", solidHeader = TRUE, status = "primary", width = 12,
                           tabsetPanel(footer = "Referência: <https://portal.ct.gov/deep/air/monitoring/aqi-health-effects-statements>",
                                       tabPanel(HTML(c(paste0("SO",tags$sub("2")))), withSpinner(dataTableOutput("tabSO2",height = 475))),
                                       tabPanel(HTML(c(paste0("NO",tags$sub("2")))), withSpinner(dataTableOutput("tabNO2",height = 475))),
                                       tabPanel(HTML(c(paste0("O",tags$sub("3")))), withSpinner(dataTableOutput("tabO3",height =475))),
                                       tabPanel("CO", withSpinner(dataTableOutput("tabCO",height = 475))),
                                       tabPanel(HTML(c(paste0("MP",tags$sub("2.5")))), withSpinner(dataTableOutput("tabPM25",height = 475))),
                                       tabPanel(HTML(c(paste0("MP",tags$sub("10")))), withSpinner(dataTableOutput("tabPM10",height = 475)))
                           )

                       ))
              ))
    ),


  )
)

# -------------------------------------------------------------------------------------------------------------------------------------
#                                                   SERVER
# -------------------------------------------------------------------------------------------------------------------------------------

server <- function(input, output) {

  # ----------------------------------------------------------TAB4-----------------------------------------------------------------------

  # --------------------------------------------------------CORRELATION MATRIX ----------------------------------------------------------

  output$corrcoeff <- renderPlot({
    mydata2 <- Datafinal %>% filter(Year==input$years, Cidade==input$Cities)
    mydata <- mydata2[,c(3:8)]
    mydata.rcorr = rcorr(as.matrix(mydata))
    mydata.coeff = mydata.rcorr$r
    corrplot(mydata.coeff,method="number")
  })

  # ------------------------------------------------------SCATTERPLOT CORRELATION-------------------------------------------------------

  output$corrscatt <- renderPlot({
    mydata2 <- Datafinal %>% filter(Year==input$years, Cidade==input$Cities)
    mydata<-mydata2[,c(3:8)]
    chart.Correlation(mydata, histogram=TRUE, pch=19)

  })

  # -----------------------------------------------------------HEAT MAP-----------------------------------------------------------------

  output$heatmap <- renderPlot({
    mydata2 <- Datafinal %>% filter(Year==input$years, Cidade==input$Cities)
    mydata<-mydata2[,c(3:8)] #ADD METEO
    mydata.rcorr = rcorr(as.matrix(mydata))
    mydata.coeff = mydata.rcorr$r
    palette = colorRampPalette(c("green", "white", "red")) (20)
    heatmap(x = mydata.coeff, col = palette, symm = TRUE)
  })


  # ----------------------------------------------------TABLES FOR POLLUTANT PRECAUTIONS--------------------------------------------------

  # reading csv file containing precautions from pollutants

  Poltab <- read.csv("https://raw.githubusercontent.com/jessicajcss/Shiny_RMC/main/data/pollutants_table.csv")

  # Table showing PM2.5 cautions

  pm2_5data<-Poltab[Poltab$Poluente == " PM2.5", ]
  output$tabPM25 <- DT::renderDataTable(
    DT::datatable({
      pm2_5data[,c(1:3)]
    },
    options = list(searching = FALSE,
                   pageLength = 10,
                   lengthChange = FALSE,
                   order = list(list(1, 'desc'))
    ), rownames = FALSE

    ))

  # Table showing PM10 cautions
  pm10data<- Poltab[Poltab$Poluente == " PM10"]
  output$tabPM10 <- DT::renderDataTable(
    DT::datatable({
      pm10data[,c(1:3)]
    },
    options = list(searching = FALSE,
                   pageLength = 10,
                   lengthChange = FALSE,
                   order = list(list(1, 'desc'))
    ), rownames = FALSE
    ))

  # Table showing NO2 cautions
  no2data<-Poltab[Poltab$Poluente == " NO2", ]
  output$tabNO2 <- DT::renderDataTable(
    DT::datatable({
      no2data[,c(1:3)]
    },
    options = list(searching = FALSE,
                   pageLength = 10,
                   lengthChange = FALSE,
                   order = list(list(1, 'desc'))
    ), rownames = FALSE

    ))

  # Table showing CO cautions
  codata<-Poltab[Poltab$Poluente == " CO", ]
  output$tabCO <- DT::renderDataTable(
    DT::datatable({
      codata[,c(1:3)]
    },
    options = list(searching = FALSE,
                   pageLength = 10,
                   lengthChange = FALSE,
                   order = list(list(1, 'desc'))
    ), rownames = FALSE

    ))

  # Table showing SO2 cautions
  so2data<-Poltab[Poltab$Poluente == " SO2", ]
  output$tabSO2 <- DT::renderDataTable(
    DT::datatable({
      so2data[,c(1:3)]
    },
    options = list(searching = FALSE,
                   pageLength = 10,
                   lengthChange = FALSE,
                   order = list(list(1, 'desc'))
    ), rownames = FALSE

    ))

  # Table showing O3 cautions
  o3data<-Poltab[Poltab$Poluente == " O3", ]
  output$tabO3 <- DT::renderDataTable(
    DT::datatable({
      o3data[,c(1:3)]
    },
    options = list(searching = FALSE,
                   pageLength = 10,
                   lengthChange = FALSE,
                   order = list(list(1, 'desc'))
    ), rownames = FALSE

    ))





  # ------------------------------------------------------TAB1--------------------------------------------------

  AQItab <- read.csv("https://raw.githubusercontent.com/jessicajcss/Shiny_RMC/04941d3078dd5e53233bab5253fad4ccc178bb7f/data/AQItab.csv")

  output$tableAQI <- renderTable({AQItab}, #https://gallery.shinyapps.io/109-render-table/
                                 striped = TRUE,
                                 spacing = 'm',
                                 width = '85%', align = 'c')



  output$PM25<-renderPlot({
    Day <- Datafinal %>%
      mutate(Mês = month(Date, label = T)) %>%
      subset(Cidade == input$Cities & Year == input$years) %>%
      group_by(Mês) %>%
      summarize(Média = mean(PM2.5, na.rm = T))
    df_base <- ggplot(data=Day, aes(x=Mês, y=Média, fill=Média))
    df_base + geom_col() + theme_classic() + ylab('Concentração média (ug/m³)')
  })






  # ----------------------------------------------------MAP FOR AQI--------------------------------------------------
  output$map<-renderLeaflet({

    # subseting the data according to the date selected
    Day <- Datafinal %>%
      subset(Datafinal$Date == input$select_date)

    # mutating the data with the popup info for easy access.
    Day<-Day %>%
      mutate(popup_Info=paste("Cidade: ",Cidade,"</br>",
                              "AQI: ",AQI,"</br>",
                              "Condition: ",AQI_Qualidade))
    #brewer.pal(10, "Spectral")
    # gradient based on the AQI level
    risk.bins <- c(0, 50, 100, 150, 200, 300)
    binpal <- colorBin(colorRamp(c("#5F0FA2", "#814FA7", "#F46D43", "#FDAE61", "yellow","#ABDDA4")),
                       Datafinal$AQI, bins = risk.bins, pretty = FALSE, reverse = T)

    # sending the data to the leaflet map to be rendered
    # the markers are provided the pallet colour
    leaflet(data=Day) %>%
      addTiles() %>%
      addCircleMarkers(lat=~Latitude, lng =~Longitude, opacity = 1, fillOpacity = 1,
                       radius = 20, popup = ~popup_Info, color = ~binpal(AQI))

  })

  # ----------------------------------------------------BAR GRAPHS FOR AQI--------------------------------------------------
  output$plotPM25<-renderPlot({
    Day <- subset(Datafinal,Datafinal$Date == input$select_date)
    df_base <- ggplot(data=Day, aes(x=Cidade, y=PM2.5, fill=AQI_PM25),
                      alpha = 0.8)
    df_base + geom_col() + theme_classic() +
      scale_y_continuous(breaks = seq(0, 30, 5)) +
      geom_hline(aes(yintercept = 15), #,linetype = "OMS Média Anual"),
                 linewidth = 0.5,
                 color = "red3") +
      annotate("text", x = 0.5, y = 15.5 , color = "red3",
               label = expression("Referência OMS 24h"),
               family = "Fira Sans", size = 6, hjust = 0) +
      ylab(expression(paste("Concentração de ", MP[2.5], " (", mu,"g ", m^-3,")")))
  })

  output$plotPM10<-renderPlot({
    Day <- subset(Datafinal,Datafinal$Date == input$select_date)
    df_base <- ggplot(data=Day, aes(x=Cidade, y=PM10, fill=AQI_PM10),
                      alpha = 0.8)
    df_base + geom_col() + theme_classic() +
      scale_y_continuous(breaks = seq(0, 150, 25)) +
      geom_hline(aes(yintercept = 45), #,linetype = "OMS Média Anual"),
                 linewidth = 0.5,
                 color = "red3") +
      annotate("text", x = 0.5, y = 47 , color = "red3",
               label = expression("Referência OMS 24h"),
               family = "Fira Sans", size = 6, hjust = 0) +
      ylab(expression(paste("Concentração de ", MP[10], " (", mu,"g ", m^-3,")")))  })


  output$plotNO2<-renderPlot({
    Day <- subset(Datafinal,Datafinal$Date == input$select_date)
    df_base <- ggplot(data=Day, aes(x=Cidade, y=NO2, fill=AQI_NO2),
                      alpha = 0.8)
    df_base + geom_col() + theme_classic() +
      scale_y_continuous(breaks = seq(0, max(Datafinal$NO2)*5, 25)) +
      geom_hline(aes(yintercept = 25), #,linetype = "OMS Média Anual"),
                 linewidth = 0.5,
                 color = "red3") +
      annotate("text", x = 0.5, y = 25*1.2 , color = "red3",
               label = expression("Referência OMS 24h"),
               family = "Fira Sans", size = 6, hjust = 0) +
      ylab(expression(paste("Concentração de ", NO[2], " (", mu,"g ", m^-3,")")))
  })


  output$plotCO<-renderPlot({
    Day <- subset(Datafinal,Datafinal$Date == input$select_date)
    df_base <- ggplot(data=Day, aes(x=Cidade, y=CO, fill=AQI_CO),
                      alpha = 0.8)
    df_base + geom_col() + theme_classic() +
      scale_y_continuous(breaks = seq(0, 5, 1)) +
      geom_hline(aes(yintercept = 4), #,linetype = "OMS Média Anual"),
                 linewidth = 0.5,
                 color = "red3") +
      annotate("text", x = 0.5, y = 4.1 , color = "red3",
               label = expression("Referência OMS 8h"),
               family = "Fira Sans", size = 6, hjust = 0) +
      ylab(expression(paste("Concentração de CO (mg ", m^-3,")")))
  })

  output$plotSO2<-renderPlot({
    Day <- subset(Datafinal,Datafinal$Date == input$select_date)
    df_base <- ggplot(data=Day, aes(x=Cidade, y=SO2, fill=AQI_SO2),
                      alpha = 0.8)
    df_base + geom_col() + theme_classic() +
      scale_y_continuous(breaks = seq(0, max(Datafinal$SO2)*5, 20)) +
      geom_hline(aes(yintercept = 40), #,linetype = "OMS Média Anual"),
                 linewidth = 0.5,
                 color = "red3") +
      annotate("text", x = 0.5, y = 47 , color = "red3",
               label = expression("Referência OMS 24h"),
               family = "Fira Sans", size = 6, hjust = 0) +
      ylab(expression(paste("Concentração de ", SO[2], " (", mu,"g ", m^-3,")")))
  })

  output$plotO3<-renderPlot({
    Day <- subset(Datafinal,Datafinal$Date == input$select_date)
    df_base <- ggplot(data=Day, aes(x= Cidade, y=O3, fill=AQI_O3),
                      alpha = 0.8)
    df_base + geom_col() + theme_classic() +
      scale_y_continuous(breaks = seq(0, max(Datafinal$O3)*3, 25)) +
      geom_hline(aes(yintercept = 100), #,linetype = "OMS Média Anual"),
                 linewidth = 0.5,
                 color = "red3") +
      annotate("text", x = 0.5, y = 102 , color = "red3",
               label = expression("Referência OMS 8h"),
               family = "Fira Sans", size = 6, hjust = 0) +
      ylab(expression(paste("Concentração de ", O[3], " (", mu,"g ", m^-3,")")))
  })

  # ----------------------------------------TAB3---------------------------------------------------------------------
  # ----------------------------------------LINE GRAPHS-------------------------------------------------------------

  output$plots <- renderPlot({
    Datafinal$Date <- as.Date(Datafinal$Date)
    week_new <- Datafinal[,c(1:8)]
    week_new <- subset(week_new,between(Date, as.Date(input$start_date), as.Date(input$end_date)))

    week_Cidade <- subset(week_new,Cidade==input$Cities1) %>%
      group_by(Date) %>%
      summarise_at(vars(SO2:PM2.5), mean, na.rm = TRUE)

    week_Cidade2 <- subset(week_new,Cidade!=input$Cities1) %>%
      group_by(Date) %>%
      summarise_at(vars(SO2:PM2.5), mean, na.rm = TRUE)

    if(input$Poluente == "PM2.5") {x    <- week_Cidade[, c(1,7)]
    color <- "green"
    y    <- week_Cidade2[, c(1,7)]}
    if(input$Poluente == "PM10") {x    <- week_Cidade[, c(1,6)]
    color <- "brown"
    y    <- week_Cidade2[, c(1,6)]}
    if(input$Poluente == "NO2") {x    <- week_Cidade[, c(1,3)]
    color <- "red"
    y    <- week_Cidade2[, c(1,3)]}
    if(input$Poluente == "CO") {x    <- week_Cidade[, c(1,5)]
    color <- "blue"
    y    <- week_Cidade2[, c(1,5)]}
    if(input$Poluente == "SO2") {x    <- week_Cidade[, c(1,2)]
    color <- "orange"
    y    <- week_Cidade2[, c(1,2)]}
    if(input$Poluente == "O3") {x    <- week_Cidade[, c(1,4)]
    color <- "grey50"
    y    <- week_Cidade2[, c(1,4)]}



    plot(x,type="b",lwd=2,
         xaxt="n", ylim=c(0, max(Datafinal[input$Poluente], na.rm = T)),
         col=color,
         xlab="Data",ylab="Concentração (ug/m³)")

    lines(y,col="black",type = "b",lwd=2)

    title(input$Cities1, col.main = color)

    axis.Date(1,
              at=seq(input$start_date, input$end_date, by="days"),
              format = "%b %d \n %Y", cex.axis = .7)



    legend("topright",
           legend=c(expression(SO["2"]),
                    expression(NO["2"]),
                    expression(O["3"]),
                    "CO",
                    expression(MP["2,5"]),
                    expression(MP["10"])),
           lty=5,lwd=4,pch=10,col=c("orange","red","grey50","blue","green","brown"),
           ncol=2,bty="n",cex=0.8,
           text.col=c("orange","red","grey50","blue","green","brown")
    )


  })


  # ----------------------------------------------------MAP FOR polarplots--------------------------------------------------
  #source("00-data_wrangling.R")


  output$sites <- renderLeaflet({


    df <- data.frame(Latitute = c(-25.322265201285667 , -25.193976227163617),
                     Longitude = c(-49.1578184144157 , -49.311729610632256),
                     ticker = c("Colombo", "Rio Branco do Sul"),
                     Estação = c("Estação Meteorológica em Colombo", "Estação Meterológica em Rio Branco do Sul"))


    IconSet <- awesomeIconList('Rio Branco do Sul' = makeAwesomeIcon(icon='cloud',
                                                                     squareMarker = F,
                                                                     library='glyphicon',
                                                                     markerColor = 'red', iconColor = 'white'),
                               'Colombo' = makeAwesomeIcon(icon='cloud',
                                                           squareMarker = F,
                                                           library='glyphicon',
                                                           markerColor = 'green', iconColor = 'white'))
    binPal <- colorFactor('black', df$Latitude)
    localizacao <- localizacao %>%
      mutate(site = as.factor("Local de Amostragem"))

    df %>%
      leaflet() %>%
      addTiles() %>%
      addAwesomeMarkers(lng = ~Longitude, lat = ~Latitute,
                        icon = ~IconSet[ticker],
                        label = ~ Estação) %>%
      addCircleMarkers(lng = localizacao$Long, lat = localizacao$Lat,
                       group = localizacao$Cidade,
                       fillColor = "black",
                       color = "black",
                       #icon = awesome, # lookup from list based on ticker
                       label = ~ paste("Estação de Monitoramento:", localizacao$Local,"-", localizacao$Cidade)) %>%
      addLegendFactor(
        values = ~ localizacao$site,
        pal = binPal,
        #labels = "Locais de Amostragem",
        title = "Pontos de Monitoramento",
        opacity = 0.5,
        position = "bottomleft") %>%
      addLegendAwesomeIcon(iconSet = IconSet,
                           orientation = 'horizontal',
                           title = htmltools::tags$div(
                             style = 'font-size: 12px;',
                             'Estações Meteorológicas'),
                           labelStyle = 'font-size: 0px;',
                           position = 'bottomleft',
                           group = 'Horizontal Legend') %>%
      addLayersControl(
        overlayGroups = localizacao$Cidade,
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      addProviderTiles(
        "CartoDB.Positron",
        group = "CartoDB.Positron"
      ) %>%
      addProviderTiles(
        "Esri.WorldImagery",
        group = "Esri.WorldImagery"
      ) %>%
      # add a layers control
      addLayersControl(
        baseGroups = c("CartoDB.Positron", "Esri.WorldImagery"
        ),
        # position it on the topleft
        position = "bottomleft"
      )



  })


  meteo <- read.csv("https://raw.githubusercontent.com/jessicajcss/Shiny_RMC/main/data/meteo_hour.csv")

  output$map_polarplot <- renderLeaflet({
    meteo <- meteo %>%
      mutate(data = ifelse(str_detect(date, ":00"),
                           as.character(date),
                           paste(as.character(date), "00:00:00", sep = " "))) %>%
      select(-date, -Local) %>%
      subset(site == "Rio Branco do Sul" | site == "Colombo") %>%
      rename(date = data,
             Cidade = site)  %>%
      mutate(date = as_datetime(date),
             Cidade = case_when(Cidade == "Colombo" ~ "Almirante Tamandaré",
                                TRUE ~ Cidade))

    week_new <- left_join(data_thermo_agg, meteo, by = c("Cidade", "date"))
    week_new <- week_new %>% mutate(Date = as.Date(date))
    week_new <- subset(week_new, between(Date, as.Date(input$start_date), as.Date(input$end_date)))

    thermo_localizacao <- localizacao %>%
      subset((Local == "Defesa Civil" | Local == "Prefeitura") & Tipo == 'outdoor') %>%
      subset(Cidade == "Almirante Tamandaré" | Cidade == "Rio Branco do Sul") %>%
      select(Cidade, Lat, Long)

    colnames(thermo_localizacao) <- c('Cidade', 'Latitude', 'Longitude')

    week_new <- left_join(week_new, thermo_localizacao, by = "Cidade")

    polarMap(week_new,
             pollutant = c("SO2", "NO2", "O3", "CO", "PM2.5", "PM10"),
             latitude = "Latitude",
             longitude = "Longitude",
             key = TRUE,
             provider = "CartoDB.Positron")

  })



  output$wrose <- renderPlot({

    Datafinal$Date <- as.Date(Datafinal$Date)

    meteo <- meteo %>%
      mutate(data = ifelse(str_detect(date, ":00"),
                           as.character(date),
                           paste(as.character(date), "00:00:00", sep = " "))) %>%
      select(-date, -Local) %>%
      subset(site == "Rio Branco do Sul" | site == "Colombo") %>%
      rename(Date = data,
             Cidade = site)  %>%
      mutate(Date = as_datetime(Date),
             Cidade = case_when(Cidade == "Colombo" ~ "Almirante Tamandaré",
                                TRUE ~ Cidade))

    Datafinal$Date <- as.Date(Datafinal$Date)
    week_new <- left_join(Datafinal, meteo, by = c("Cidade", "Date"))
    week_new <- subset(week_new, between(Date, as.Date(input$start_date), as.Date(input$end_date)))

    week_new %>%
      pollutionRose(pollutant = "ws",
                    type = "Cidade")
  })


  output$dist <- renderPlot({
    meteo <- meteo %>%
      mutate(data = ifelse(str_detect(date, ":00"),
                           as.character(date),
                           paste(as.character(date), "00:00:00", sep = " "))) %>%
      select(-date, -Local) %>%
      subset(site == "Rio Branco do Sul" | site == "Colombo") %>%
      rename(Date = data,
             Cidade = site)  %>%
      mutate(Date = as_datetime(Date),
             Cidade = case_when(Cidade == "Colombo" ~ "Almirante Tamandaré",
                                TRUE ~ Cidade))

    Datafinal$Date <- as.Date(Datafinal$Date)
    week_new <- left_join(Datafinal, meteo, by = c("Cidade", "Date"))
    week_new <- subset(week_new, between(Date, as.Date(input$start_date), as.Date(input$end_date)))

    week_new %>%
      ggplot(aes(x=Date, group=1))+
      geom_bar(aes(y=prec*10,col="Precipitação"),fill="green",
               stat = "identity",alpha=0.5)+
      geom_line(aes(y=umid,col="Umidade Relativa"),size=1)+
      geom_line(aes(y=temp,col="Temperatura"),size=1)+
      geom_point(aes(y=umid))+
      geom_point(aes(y=temp))+
      theme_bw()+
      facet_wrap(.~Cidade, scales= "free_y", axes = "all_y", ncol = 1)+
      theme(axis.text.x = element_text(angle = 0,vjust = 0.5),
            legend.position = "bottom")+
      scale_y_continuous(sec.axis = sec_axis(~./10, name= "Precipitação em mm"))+

      labs(x="Dia de amostragem",y="Temperatura (ºC) \n Umidade Relativa (%)",colour = "Variável")+
      scale_colour_manual(values = c("darkgreen","red","blue")) +
      scale_x_datetime(
        labels = scales::label_date_short(),  # automatically efficient date labels
        breaks = "1 month")
  })




  #------------------------------------------------------------TAB5------------------------------------------------------------
  #------------------------------------------------------------RAW DATA------------------------------------------------------------
  output$downloadData <- downloadHandler(
    filename=function(){
      paste("DayData","csv", sep = '.')
    },
    content=function(file){
      Datafinal = data_thermo_converted %>%
        mutate(Date = as.Date(date)) %>%
        subset(Date >= input$start_date & Date <= input$end_date) %>%
        mutate(Date = date) %>%
        select(date, Cidade:PM10)
      write_csv(Datafinal,file)
    }
  )

  output$tableData <- renderTable(width = "70%",
                                  data_thermo_agg %>%
                                    mutate(Date = as.Date(date)) %>%
                                    subset(Date >= input$start_date & Date <= input$end_date) %>%
                                    mutate(data = as.character(as_datetime(date))) %>%
                                    select(data, SO2:PM10) %>%
                                    mutate(date = ifelse(str_detect(data, ":00"),
                                                         as.character(data),
                                                         paste(as.character(data), "00:00:00", sep = " ")),
                                           data = date) %>%
                                    select(-date) %>%
                                    head(n = 20L)
  )

}
# ------------------------------------------------------------RUNNING THE PROJECT--------------------------------------------------



# Run the application
shinyApp(ui = ui, server = server)
