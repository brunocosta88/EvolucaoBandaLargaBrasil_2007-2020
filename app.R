# More info:
#   https://github.com/jcheng5/googleCharts
# Install:
#   devtools::install_github("jcheng5/googleCharts")
library(googleCharts)
library(shiny)
library(dplyr)

url <- "https://github.com/brunocosta88/EvolucaoBandaLargaBrasil_2007-2020/raw/master/acessosBandaLarga.rds"
destfile <- "./temp.rds"
download.file(url,destfile)
acessos <- readRDS("temp.rds")

acessos$area <- as.factor(acessos$area)
acessos$Regiao <- as.factor(acessos$Regiao)
acessos$UF <- as.factor(acessos$UF)
acessos$Ano <- as.numeric(acessos$Ano)
#View(acessos)
#print(unique(acessos$Faixa.de.Velocidade))

# Use global max/min for axes so the view window stays
# constant as the user moves between years

xlim2 <- list(min = 0.55,
              max = 0.86)
ylim2 <- list(min = min(acessos$Densidade),
              max = max(200))

ylim <- list(min = min(acessos$Densidade),
             max = 200)
xlim <- list(min = min(log(acessos$Acessos)),
              max = 25)

tecnologias = c("Cable Modem","xDSL","HFC","Fibra", "Outras")

faixas <- c("Baixa (ate 2Mbps)", "Media (2Mbps a 34Mbps)", "Alta (>34Mbps)")

ui <-
    navbarPage(
        "Projeto VISemap2020",
        tabPanel("Por Estado",
                 fluidPage(
                     titlePanel(
                         "Relacao entre Densidade de Acessos de Banda Larga e IDHM dos Municipios Brasileiros"
                     ),
                     sidebarLayout(
                         sidebarPanel(
                             checkboxGroupInput(
                                 "reg",
                                 "Selecione a Regiao",
                                 choices = unique(acessos$Regiao),
                                 selected = unique(acessos$Regiao)
                             ),
                             checkboxGroupInput(
                                 "area",
                                 "Selecione a Area",
                                 choices = unique(acessos$area),
                                 selected = unique(acessos$area)
                             ),
                             uiOutput("lista_UF")
                             
                         ),
                         mainPanel(
                             # This line loads the Google Charts JS library
                             googleChartsInit(),
                             
                             # Use the Google webfont "Source Sans Pro"
                             tags$link(
                                 href = paste0(
                                     "http://fonts.googleapis.com/css?",
                                     "family=Source+Sans+Pro:300,600,300italic"
                                 ),
                                 rel = "stylesheet",
                                 type = "text/css"
                             ),
                             tags$style(type = "text/css",
                                        "body {font-family: 'Source Sans Pro'}"),
                             
                             #h2("Google Charts demo"),
                             h2("Densidade de Acessos Telecom Brasil"),
                             
                             googleBubbleChart(
                                 "chart",
                                 width = "100%",
                                 height = "475px",
                                 # Set the default options for this chart; they can be
                                 # overridden in server.R on a per-update basis. See
                                 # https://developers.google.com/chart/interactive/docs/gallery/bubblechart
                                 # for option documentation.
                                 options = list(
                                     fontName = "Source Sans Pro",
                                     fontSize = 13,
                                     # Set axis labels and ranges
                                     hAxis = list(title = "IDHM",
                                                  viewWindow = xlim2),
                                     vAxis = list(title = "Densidade de Acessos Banda Larga",
                                                  viewWindow = ylim2),
                                     sizeAxis = list(maxValue =  max(acessos$Populacao),
                                                     minValue =  min(acessos$Populacao)),
                                     # The default padding is a little too spaced out
                                     chartArea = list(
                                         top = 50,
                                         left = 75,
                                         height = "75%",
                                         width = "75%"
                                     ),
                                     # Allow pan/zoom
                                     explorer = list(),
                                     # Set bubble visual props
                                     bubble = list(
                                         opacity = 0.4,
                                         stroke = "none",
                                         # Hide bubble label
                                         textStyle = list(color = "none")
                                     ),
                                     # Set fonts
                                     titleTextStyle = list(fontSize = 16),
                                     tooltip = list(textStyle = list(fontSize = 12))
                                 )
                             ),
                             fluidRow(shiny::column(
                                 4,
                                 offset = 4,
                                 sliderInput(
                                     "ano",
                                     "Ano",
                                     min = 2007,
                                     max = 2020,
                                     value = min(acessos$Ano),
                                     animate = TRUE
                                 )
                             ))
                         )
                     )
                     
                 )),
        tabPanel("Por Tecnologia",
                 
                 fluidPage(
                     titlePanel(
                         "Relacao entre Densidade de Acessos de Banda Larga e IDHM dos Municipios Brasileiros"
                     ),
                     sidebarLayout(
                         sidebarPanel(
                             checkboxGroupInput(
                                 "reg2",
                                 "Selecione a Regiao",
                                 choices = unique(acessos$Regiao),
                                 selected = unique(acessos$Regiao)
                             ),
                             checkboxGroupInput(
                                 "area2",
                                 "Selecione a Area",
                                 choices = unique(acessos$area),
                                 selected = unique(acessos$area)
                             ),
                             checkboxGroupInput(
                                 "faixa",
                                 "Selecione a Faixa de Velocidade",
                                 choices = faixas,
                                 selected = faixas
                             ),
                             checkboxGroupInput(
                                 "tecn",
                                 "Selecione a Tecnologia",
                                 choices = tecnologias,
                                 selected = tecnologias
                                 
                             )
                             
                         ),
                         mainPanel(
                             # This line loads the Google Charts JS library
                             
                             googleBubbleChart(
                                 "chart2",
                                 width = "100%",
                                 height = "475px",
                                 # Set the default options for this chart; they can be
                                 # overridden in server.R on a per-update basis. See
                                 # https://developers.google.com/chart/interactive/docs/gallery/bubblechart
                                 # for option documentation.
                                 options = list(
                                     fontName = "Source Sans Pro",
                                     fontSize = 13,
                                     # Set axis labels and ranges
                                     hAxis = list(title = "Log Acessos de Banda Larga",
                                                  viewWindow = xlim),
                                     vAxis = list(title = "Densidade de Acessos de Banda Larga",
                                                  viewWindow = ylim),
                                     sizeAxis = list(maxValue =  max(acessos$Populacao),
                                                     minValue =  min(acessos$Populacao)),
                                     # The default padding is a little too spaced out
                                     chartArea = list(
                                         top = 50,
                                         left = 75,
                                         height = "75%",
                                         width = "75%"
                                     ),
                                     # Allow pan/zoom
                                     explorer = list(),
                                     # Set bubble visual props
                                     bubble = list(
                                         opacity = 0.4,
                                         stroke = "none",
                                         # Hide bubble label
                                         textStyle = list(color = "none")
                                     ),
                                     # Set fonts
                                     titleTextStyle = list(fontSize = 16),
                                     tooltip = list(textStyle = list(fontSize = 12))
                                 )
                             ),
                             
                             fluidRow(shiny::column(
                                 4,
                                 offset = 4,
                                 sliderInput(
                                     "ano2",
                                     "Ano",
                                     min = 2007,
                                     max = 2020,
                                     value = min(acessos$Ano),
                                     animate = TRUE
                                 )
                             ))
                             
                             
                             
                         )
                         
                     )
                 )),
        tabPanel("Sobre",
                 fluidPage(
                     titlePanel(
                         "Projeto de VisEmap2020, Bruno M. Costa"
                     ),
                 p("Projeto do curso de Visualizacao da Informacao, da Escola de Matematica Aplicada da FGV no segundo semestre de 2020"),
                 p("Aluno: Bruno Martins Costa, email:",a("brunocosta88@gmail.com")),
                 p("Professor(a): Asla Medeiros e Sa")
                 )
                 )
    )



server <- function(input, output, session) {
    
    library(googleCharts)
    library(shiny)
    library(dplyr)
    
    url <- "https://github.com/brunocosta88/EvolucaoBandaLargaBrasil_2007-2020/raw/master/acessosBandaLarga.rds"
    destfile <- "./temp.rds"
    download.file(url,destfile)
    acessos <- readRDS("temp.rds")
    
    acessos$area <- as.factor(acessos$area)
    acessos$Regiao <- as.factor(acessos$Regiao)
    acessos$UF <- as.factor(acessos$UF)
    acessos$Ano <- as.numeric(acessos$Ano)
    
    
    tecnologias = c("Cable Modem","xDSL","HFC","Fibra", "Outras")
    faixas <- c("Baixa (ate 2Mbps)", "Media (2Mbps a 34Mbps)", "Alta (>34Mbps)")
    
    # Provide explicit colors for regions, so they don't get recoded when the
    # different series happen to be ordered differently from year to year.
    # http://andrewgelman.com/2014/09/11/mysterious-shiny-things/
    defaultColors <-
        c("#3366cc",
          "#dc3912",
          "#ff9900",
          "#109618",
          "#990099",
          "#0099c6",
          "#dd4477")
    series <-
        structure(lapply(defaultColors, function(color) {
            list(color = color)
        }),
        names = levels(acessos$Regiao))
    
    AnoData <- reactive({
        # Filter to the desired year, and put the columns
        # in the order that Google's Bubble Chart expects
        # them (name, x, y, color, size). Also sort by region
        # so that Google Charts orders and colors the regions
        # consistently.
        
        df2 <- acessos %>%
            filter(Ano == input$ano,
                   UF %in% input$estados,
                   area %in% input$area) %>%
            group_by(Municipio, IDHM, Densidade , Regiao, Populacao) %>%
            summarise(Acessos = sum(Acessos)) %>%
            select(Municipio, IDHM, Densidade, Regiao, Populacao) %>%
            arrange(Regiao)
        #View(df2)  
        df2
    })
    
    
    output$chart <- reactive({
        # Return the data and options
        list(
            data = googleDataTable(AnoData()),
            options = list(
                title = sprintf("IDHM versus Densidade, %s",
                                input$ano),
                series = series
            )
        )
    })
    
    output$chart2 <- reactive({
        # Return the data and options
        list(
            data = googleDataTable(TecData()),
            options = list(
                title = sprintf("Densidade de Acessos versus Acessos Telecom, %s",
                                input$ano2),
                series = series
            )
        )
    })
    
    TecData <- reactive({
        # Filter to the desired year, and put the columns
        # in the order that Google's Bubble Chart expects
        # them (name, x, y, color, size). Also sort by region
        # so that Google Charts orders and colors the regions
        # consistently.
        
        if ( "Outras" %in% input$tecn ){
            tecn <- c(unique(acessos$Tecnologia[!(acessos$Tecnologia %in% tecnologias)]), input$tecn)
            #print(tecn)
        }else{
            tecn <- input$tecn
            #print(tecn)
        }
        
        if (input$faixa == "Baixa (ate 2Mbps)"){
            faixa.sel <- unique(acessos$Faixa.de.Velocidade)[c(1,2,4,5)]
        }else if(input$faixa == "Media (2Mbps a 34Mbps)"){
            faixa.sel <- unique(acessos$Faixa.de.Velocidade)[c(3,6,7)]
        }else{
            faixa.sel <- unique(acessos$Faixa.de.Velocidade)[8]
        }
        
        df <- acessos %>%
            filter(
                Ano == input$ano2,
                Faixa.de.Velocidade %in% faixa.sel,
                Tecnologia %in% tecn,
                area %in% input$area2,
                Regiao %in% input$reg2
            ) %>%
            group_by(Municipio, Densidade, Regiao, Populacao) %>%
            #mutate(log.Acessos = log(sum(Acessos))) %>%
            summarise(Acessos = sum(Acessos)) %>%
            mutate(log.Acessos= log(Acessos) ) %>%
            select(Municipio, log.Acessos, Densidade, Regiao, Populacao) %>%
            arrange(Regiao)
        #View(df)
        df
    })
    
    output$lista_UF <- renderUI({
        data_sub <- subset(acessos, Regiao == input$reg)
        ufs <- unique(data_sub$UF)
        
        checkboxGroupInput(
            "estados",
            "Selecione os Estados",
            choices = unique(acessos$UF),
            selected = ufs
        )
    })
}

shinyApp(ui = ui, server = server)