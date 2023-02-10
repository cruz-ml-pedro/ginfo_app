library(shiny)
source("source.r")

#Define UI for application that draws a histogram
ui <- fluidPage(
    
  theme = bslib::bs_theme(bootswatch = "sketchy"),
  
    tabsetPanel(type = "pills",
        
        # primeiro painel----
        tabPanel(
            "Série Histórica e Mapa",
            #Sidebar  
            sidebarLayout(
              sidebarPanel(
                width = 3,
                #style = "position:fixed; overflow: visible;", 
                    textInput(
                        inputId = "keyword",
                        label = "Entre com uma palavra chave",
                        placeholder = "Keyword"
                        ),
                    selectInput(
                        inputId = "region",
                        label = "Escolha a região de interesse",
                        choices = locais
                        ),
                    selectInput(
                        inputId = "channel",
                        label = "Escolha o canal de busca",
                        choices = canais
                        ),
                    selectInput(
                        inputId = "time",
                        label = "intervalo de tempo",
                        choices = tempo
                        ),
                    actionButton(
                        "buscar",
                        "buscar",
                        icon("search")
                    ),
                   
                     hr(style = "border-top: 2px solid #E3EEF4FF;"),
                    
                  h5("Previsão do volume de buscas"),
                  HTML("Para visualisar a série histórica escolha o intervalo de tempo a ser previsto e clique em simular"),
                   h5("Buscas vs Previsão:"),
                   HTML("Horas,Dia = minutos <br/> Dias = Horas <br/> Mês,Anos = Dias"),
                   hr(style = "border-top: 2px solid #E3EEF4FF;"),
                    numericInput("period",
                                 label = "Nº Minutos/Horas/Dias",
                                 min = 1,
                                 max=365,
                                 step = 1,
                                 value = 1
                                 ),
                    actionButton(
                        "simular",
                        "Simular",
                        icon = icon("rocket")
                    ),
                    hr(style = "border-top: 2px solid #E3EEF4FF;"),
                    downloadButton(
                        outputId = "download1",
                        label = "Download Relatório",
                        icon("download")
                    )
                    ),
                
                # Painel principal da pagina1----
                mainPanel(width = 9,
                          
                          h2("Localidades que mais procuraram a palvra chave"),
                          leafletOutput(
                              outputId = "mapplot",
                              width = "100%"
                          ),# plot do mapa
                          hr(style = "border-top: 8px solid #E3EEF4FF;"),
                          tabsetPanel(type = "pills",
                              tabPanel(title = "Lista de cidades",
                                         reactableOutput(
                                              outputId =  "table_cidades",
                                              width = "auto"
                                  )
                              ),#fim da primeira tabela
                              
                              tabPanel(title = "Lista de buscas relacionadas",
                                             reactableOutput(
                                                   outputId =  "table_buscas",
                                                   width = "auto"
                                               )
                                       ),# fim da segunda tabela
                              
                              tabPanel(title ="Lista de topicos relacionados",
                                       reactableOutput(
                                           outputId =  "table_topicos",
                                           width = "auto"
                                       )
                                       )# fim da ultima tabela
                          ),#fim do tabsetpanel das tabelas do meio da pagina
                          hr(style = "border-top: 8px solid #E3EEF4FF;"),
                          h2("Série Histórica"),
                          dygraphOutput(
                              outputId = "trendplot",
                              width = "100%"
                          ),
                          
                          hr(style = "border-top: 8px solid #E3EEF4FF;"),
                          
                          tabsetPanel(type = "pills",
                              tabPanel(
                                title = "Componentes",
                                plotOutput(
                                    outputId = "plot_componentes",
                                    width = "100%"
                                )
                              ),
                              tabPanel(
                                  title = "Change Point",
                                  plotOutput(
                                      outputId = "plot_change_point",
                                      width = "100%"
                                  )
                              ),
                              tabPanel(
                                title = "Espectro de Potência",
                                plotOutput(
                                  outputId = "plot_spec",
                                  width = "100%"
                                )
                              )
                              
                          )#fim do segundo tabsetpanel
                         )# fim do main panel1----
                
                )#fim do sidebarLayout pagina1----
                
            ),# fim do primeiro tabpanel #cabeçalho principal da pagina
        
        #Segundo painel----
        tabPanel("Comparação entre palavras chave",
                 
                 sidebarLayout(
                   sidebarPanel(
                     width = 3,
                     #style = "position:fixed; overflow: visible;", 
                     textInput(
                       inputId = "keyword_compare1",
                       label = "Entre com a palavra chave 1",
                       placeholder = "Keyword 1"
                     ),
                     textInput(
                       inputId = "keyword_compare2",
                       label = "Entre com a palavra chave 2",
                       placeholder = "Keyword 2"
                     ),
                     selectInput(
                       inputId = "region_compare",
                       label = "Escolha a região de interesse",
                       choices = locais
                     ),
                     selectInput(
                       inputId = "channel_compare",
                       label = "Escolha o canal de busca",
                       choices = canais
                     ),
                     selectInput(
                       inputId = "time_compare",
                       label = "Intervalo de tempo",
                       choices = tempo
                     ),
                     actionButton(
                       "buscar2",
                       "buscar",
                       icon("search")
                     ),
                       hr(style = "border-top: 2px solid #E3EEF4FF;"),
                       downloadButton(
                         outputId = "download2",
                         label = "Download Relatório",
                         icon("download")
                       )
                   ),# fim do sidebar panel da segunda aba do corpo principal----
                   mainPanel(width = 9,
                             
                             h2("comparação em % por região"),
                             plotlyOutput(
                               outputId = "mapplot_compare",
                               width = "100%"
                             ),
                             
                             hr(style = "border-top: 8px solid #E3EEF4FF;"),
                             
                             tabsetPanel(type = "pills",
                               
                               tabPanel("Lista cidades - comparação em %",
                                        
                                 reactableOutput(
                                   outputId =  "table_cidades_compare",
                                   width = "auto")
                               ),
                               
                               tabPanel("Lista de buscas relacionadas - comparação em %",
                                        
                                  reactableOutput(
                                     outputId = "table_buscas_compare",
                                     width = "auto",
                                     
                                        )
                                  )
                              
                             ),#fim do panelset do meio da segunda pagina
                             
                             hr(style = "border-top: 8px solid #E3EEF4FF;"),
                             
                             h2("comparação em % séries históricas"),
                             
                             plotlyOutput(
                               outputId = "trend_compare",
                               width = "100%"
                             ),
                             
                             tabsetPanel(
                               type = "pills",
                               
                               tabPanel("Correlação entre os dados",
                                 plotOutput(
                                   "correlation",
                                   width = "100%"
                                 )
                               ),
                               
                               tabPanel("Correlação espectral entre os dados",
                                 plotOutput(
                                  "speccorr",
                                  width = "100%"
                                )
                                )
                             )
                             
                             
                   )# fim do painel principal da segunda aba----
                   
                 )# fim do sidebarlayout da segunda aba do corpo principal
                 
                 ),#fim do segundo painel da estrutura principal
        
        tabPanel("Resultados Google Search",
                 
                 sidebarLayout(
                   
                   sidebarPanel(
                     
                     width = 3,
                     #style = "position:fixed; overflow: visible;",
                     
                     actionButton(
                       inputId = "conectar",
                       "Iniciar conexão",
                       icon("link")
                       ),
                     hr(),
                      actionButton(
                        inputId = "desconectar",
                        "Interromper conexão",
                        icon("link-slash")
                      ),
                     hr(style = "border-top: 4px solid #E3EEF4FF;"),
                     textInput(
                       inputId = "search",
                       label = "Entre com a sua busca",
                       placeholder = "Tiger Style"
                     ),
                     numericInput(
                       inputId = "n_pages",
                       label = "Nº de páginas",
                       value = 5,
                       min = 1,
                       max = 100
                     ),
                     actionButton(
                       inputId = "buscar3",
                       label = "buscar informações",
                       icon = icon("search")
                     ),
                     hr(style = "border-top: 4px solid #E3EEF4FF;"),
                     selectInput(
                       inputId = "options",
                       label = "Opções de visualização",
                       choices = wordcloud_choices
                     ),
                     fileInput(
                       inputId = "lexicon_list",
                       label = "Arquivo Palavras p/ remover",
                       accept = ".txt",
                       placeholder = "Nenhum arquivo selecionado"
                       ),
                     actionButton(
                       inputId = "gerar_image",
                       label = "Gerar resultados",
                       icon = icon("rocket")
                     ),
                       hr(style = "border-top: 2px solid #E3EEF4FF;"),
                       downloadButton(
                         outputId = "download3",
                         label = "Download Relatório",
                         icon("download")
                       )
                   ),#fim do sidebar da terceira aba----
                   mainPanel( width = 9,
                              
                             wordcloud2Output(
                               outputId = "wordcloud",
                               width = "100%"
                             ) ,
                             h2("Número de aparições por palavra"),
                             
                             reactableOutput(
                               outputId = "n_palavras",
                               width = "auto"
                               ),
                             
                             hr(style = "border-top: 8px solid #E3EEF4FF;"),
                             
                             h2("Correlação entre palavras"),
                             plotOutput(
                               outputId = "corr_palavras",
                               width = "100%"
                             ),
                             hr(style = "border-top: 8px solid #E3EEF4FF;"),
                             tabsetPanel(type = "pills",
                                tabPanel("Links das página",
                                            
                                      reactableOutput(
                                       outputId = "table_links",
                                       width = "auto"
                                       )
                                      ),
                                      
                                   tabPanel("Links patrocinados",
                                            reactableOutput(
                                              outputId = "table_links_spon",
                                              width = "auto"
                                              )
                                            )
                                  
                             )
                   )#fim do main panel3----
                 )#fim do sidebarlayout
                 
                 )#fim do terceiro painel da estrutura principal
        )# fim da estrutura da tabsetpanel exterior # pagina principal
   )#fim do fluid page



server <- function(input, output) {
    
  
# dados para a primeira pagina----
    g_infos_complet <- reactive({
        
        id <- showNotification(# criando um aviso
        "Carregando seus dados, aguarde...", 
        duration = NULL, 
        #closeButton = FALSE
      )
      
      on.exit(removeNotification(id), add = TRUE)
        
      g_list <- gtrends(keyword = input$keyword,
                time = input$time,
                geo = input$region,
                gprop = input$channel,
                tz = "Brazil/East",
                low_search_volume = TRUE)
        
  g_list[[1]] <- g_list[[1]] %>%
                    mutate(
                     hits=ifelse(hits=="<1",0.5,hits),
                     hits=as.numeric(hits),
                     date = as_datetime(date, tz = "Brazil/East")
                     )
                                          
  g_list
    }) %>% # criando os dados que vem do gtrend
        bindEvent(input$buscar)
    

# CRIANDO as bases DOS MAPAS DE FORMA REATIVA PARA PODER USAR O bindCache()
# transformar isso em uma função para não repetir o código    
 # criando a base do mapa da pagina1----   
    df_mapa <- reactive({
      
     
      if(input$region == "BR"){
        
        mapa_br
        
      } else {
        
        codigo_uf <-  str_remove(input$region, "^BR-")
        read_municipality(code_muni = codigo_uf)
        
      }
      
    }) %>% 
      bindCache(input$region) %>% 
      bindEvent(input$buscar)
    
    # mapa da segunda pagina----
    df_mapa_2 <- reactive({
      
     
      if(input$region_compare == "BR"){
        
        mapa_br
        
      } else {
        
        codigo_uf <-  str_remove(input$region_compare, "^BR-")
        read_municipality(code_muni = codigo_uf)
        
      }
      
    }) %>% 
      bindCache(input$region_compare) %>% 
      bindEvent(input$buscar2)
    
    # plot mapa1----
    output$mapplot <- renderLeaflet({
      
      
      id3 <- showNotification(# criando um aviso
        "Criando o mapa e demais gráficos, aguarde...", 
        duration = NULL, 
        #closeButton = FALSE
      )
      
      on.exit(removeNotification(id3), add = TRUE)
      
      
      
      if(input$region == "BR"){
        
        df <- g_infos_complet()$interest_by_region %>%
          replace(is.na(.),0) %>% 
          mutate(
            hits=ifelse(hits=="<1",0.5,hits),
            hits=as.numeric(hits),
            location = str_to_lower(location),
            location = str_remove(location,"state of "),
            location = str_replace(location, "federal district", "distrito federal"),
            location = str_replace(location, "amazonas", "amazônas"),
            location = str_trim(location, side = "both")
          ) %>% 
          rename(
            name_state = location
          )
        
        states <- df_mapa() %>% 
          mutate(
            name_state = tolower(name_state)
          ) %>% 
          dplyr::left_join(x = .,
                           y = df,
                           by = "name_state"
          )
        
        pal <- colorFactor(palette = scico(10, 
                                           palette = "lajolla")[4:7],
                           domain =states$hits )
        #pal <- colorBin("viridis", domain = states()$total, bins = 7)
        #pal_fun <- colorQuantile("YlOrRd", NULL, n = 5)
        
        labels <- sprintf(
          "%s %f",
          states$name_state, states$hits) %>% 
          lapply(HTML)
        
        leaflet(states) %>%
          addPolygons(
            # fill
            fillColor   = ~pal(hits),
            fillOpacity = 0.7,
            # line
            dashArray   = "3",
            weight      = 2,
            color       = "white",
            opacity     = 1,
            # interaction
            highlight = highlightOptions(
              weight = 5,
              color = "#666",
              dashArray = "",
              fillOpacity = 0.7,
              bringToFront = TRUE),
            label = labels,
            labelOptions = labelOptions(
              style = list("font-weight" = "normal", padding = "3px 8px"),
              textsize = "15px",
              direction = "auto")) %>%
          addLegend(
            pal = pal, values = ~hits, opacity = 0.7, title = HTML("Total"),
            position = "bottomright")
        
        
        
      } else {
        
        
        df2 <- g_infos_complet()$interest_by_city %>% 
          replace(is.na(.),0) %>% 
          mutate(
            hits=ifelse(hits=="<1",0.5,hits),
            hits=as.numeric(hits),
            location = str_to_lower(location),
            location = str_trim(location, side = "both"),
            location = stri_trans_general(str = location, 
                                          id = "Latin-ASCII")
          )%>% 
          rename(name_muni = location)
        
        state_map <- df_mapa() %>%
          mutate(
            name_muni = tolower(name_muni),
            name_muni = stri_trans_general(str = name_muni, 
                                           id = "Latin-ASCII")
          ) %>% 
          dplyr::left_join(x = .,
                           y = df2,
                           by = "name_muni"
          )
        
        pal <- colorFactor(palette = scico(10, 
                                           palette = "lajolla")[3:7],
                           domain =state_map$hits )
        #pal <- colorBin("viridis", domain = states()$total, bins = 7)
        #pal_fun <- colorQuantile("YlOrRd", NULL, n = 5)
        
        labels <- sprintf(
          "%s %f",
          state_map$name_muni, state_map$hits) %>% 
          lapply(HTML)
        
        leaflet(state_map) %>%
          addPolygons(
            # fill
            fillColor   = ~pal(hits),
            fillOpacity = 0.7,
            # line
            dashArray   = "3",
            weight      = 2,
            color       = "white",
            opacity     = 1,
            # interaction
            highlight = highlightOptions(
              weight = 5,
              color = "#666",
              dashArray = "",
              fillOpacity = 0.7,
              bringToFront = TRUE),
            label = labels,
            labelOptions = labelOptions(
              style = list("font-weight" = "normal", padding = "3px 8px"),
              textsize = "15px",
              direction = "auto")) %>%
          addLegend(
            pal = pal, values = ~hits, opacity = 0.7, title = HTML("Total"),
            position = "bottomright")
        
        
      }# fim do if para gerar o mapa do br ou dos estados
      
      
    }) %>% #fim do mapplot
      bindEvent(input$buscar)
    
    
    # tabela com as cidades----   
    
    output$table_cidades <- renderReactable({
      reactable(
        g_infos_complet()$interest_by_city %>% 
          mutate(
            hits=ifelse(hits=="<1",0.5,hits),
            hits=as.numeric(hits),
            hits=tidyr::replace_na(hits,0)
          )%>%
          dplyr::arrange(-hits) %>% 
          as_tibble()
        
      )
      
    })#fim da tabela de cidades
    
    # tabela buscas relacionadas----
    output$table_buscas <- renderReactable({ 
      reactable(
        g_infos_complet()$related_queries %>% 
          drop_na()%>%
          as_tibble(),
        searchable = TRUE
      )
    })#fim da tabela de buscas relacionadas
    
    #tabela tópicos relacionados----
    output$table_topicos <- renderReactable({ 
      reactable(
        g_infos_complet()$related_topics %>% 
          drop_na()%>%
          as_tibble(),
        searchable = TRUE
      )
      
    })#fim da tabela de topicos relacionados 

 
# selecionando um modelo para o prophete pkg de acordo com o intervalo de amostragem
#criando o modelo para prophete----
prophet_setup <- reactive({
  
        id2 <- showNotification(
          "Gerando a predição, aguarde...", 
          duration = NULL, 
          #closeButton = FALSE
        )
        
        on.exit(removeNotification(id2), add = TRUE)
        
        ts_interestOverTime <- g_infos_complet()$interest_over_time %>% 
            select(date,hits) %>% 
            rename(
                "ds"= date,
                "y"= hits
            )
        
        # começando o modelo - futuro: colocar isso em um plumber/docker e só usar os resultados aqui
        dt <- ts_interestOverTime[2,1] - ts_interestOverTime[1,1]
        
        if(dt == round(now()-(now()-60))){
          #intervalo de tempo de 1 min - predição em minutos
          
          model <- prophet(ts_interestOverTime,
                           yearly.seasonality = "auto",
                           weekly.seasonality = "auto",
                           daily.seasonality = "auto")
          
          future <-  make_future_dataframe(model,freq = 60 ,periods = input$period)
          
          forecast <- predict(model,future)
          
        } else if(dt > round(now()-(now()-60)) &  dt < today()-(today()-1)){
            
        model <- prophet(ts_interestOverTime,
                         yearly.seasonality = "auto",
                         weekly.seasonality = "auto",
                         daily.seasonality = "auto")
        
        future <-  make_future_dataframe(model,freq = 3600 ,periods = input$period)
            
        forecast <- predict(model, future)
        
        }else if(dt == today()-(today()-1)) {
            
        model <- prophet(ts_interestOverTime,
                         yearly.seasonality = "auto",
                         weekly.seasonality = "auto",
                         daily.seasonality = "auto")
        
        future <-  make_future_dataframe(model,freq = "day" ,periods = input$period)
        
        forecast <- predict(model, future)    
        } else{
            
        model <- prophet(ts_interestOverTime,
                         yearly.seasonality = "auto",
                         weekly.seasonality = "auto",
                         daily.seasonality = "auto")
            
        future <-  make_future_dataframe(model, freq = "day" ,periods = input$period)    
        forecast <- predict(model, future)
        }
        
        list(model,forecast)
        
    }) %>% 
        bindEvent(input$simular)

    #gráfico da st e da previsão----
    
   output$trendplot <- renderDygraph({
     
     id4 <- showNotification(# criando um aviso
       "Gerando os gráficos, aguarde...", 
       duration = NULL, 
       #closeButton = FALSE
     )
     
     on.exit(removeNotification(id4), add = TRUE)
     
    par(bg="white")
   dyplot.prophet(prophet_setup()[[1]],prophet_setup()[[2]])
    
   }) %>% # fim do plot da série histórica
     bindEvent(input$simular)
    
    # gráfico dos componentes----
   output$plot_componentes <- renderPlot({
   
   prophet_plot_components(prophet_setup()[[1]], prophet_setup()[[2]])
   
   }) %>% 
     bindEvent(input$simular)
    
   # gráfico change point----
   
   output$plot_change_point <- renderPlot({
       
       plot(prophet_setup()[[1]], prophet_setup()[[2]])+
           add_changepoints_to_plot(prophet_setup()[[1]])+
           ylab("Search")+
           xlab("Date")
       
   }) %>% 
     bindEvent(input$simular)
    
   #plot espectrograma---- 
   
   output$plot_spec <- renderPlot({
     
     
     
    ts <- g_infos_complet()$interest_over_time %>% 
      select(1,2) %>% 
        rename(
        "ds"= date,
        "y"= hits
      ) 
      
    dt <- ts[2,1] - ts[1,1]
      
     if(dt == round(now()-(now()-60))){
       
       
       my.wt <- analyze.wavelet(ts,
                                "y",
                                loess.span = 0.75, 
                                dt = 1/60,
                                
                                make.pval = TRUE,
                                n.sim = 10
     )
       
       wt.avg(my.wt, siglvl = 0.05, sigcol = "red", 
              periodlab = "period (Horas)")
     
     
     }else if(dt > round(now()-(now()-60)) &  dt < today()-(today()-1)){
       
       my.wt <- analyze.wavelet(ts,
                                "y",
                                loess.span = 0.75, 
                                dt = 1/24,
                                make.pval = TRUE,
                                n.sim = 10)
       
       
       wt.avg(my.wt, siglvl = 0.05, sigcol = "red", 
              periodlab = "period (Dias)")
       
     } else if(dt == today()-(today()-1)){
       
       my.wt <- analyze.wavelet(ts,
                                "y",
                                loess.span = 0.75, 
                                dt = 1/30,
                                make.pval = TRUE,
                                n.sim = 10)
       
       wt.avg(my.wt, siglvl = 0.05, sigcol = "red", 
              periodlab = "period (Meses)")
       
       
     } else if(dt == today()-(today()-7)) {
       
       my.wt <- analyze.wavelet(ts,
                                "y",
                                loess.span = 0.75, 
                                dt = 1/52.18,
                                make.pval = TRUE,
                                n.sim = 10)
       
       wt.avg(my.wt, siglvl = 0.05, sigcol = "red", 
              periodlab = "period (Anos)")
       
     } else{
       
       my.wt <- analyze.wavelet(ts,
                                "y",
                                loess.span = 0.75, 
                                dt = 1/12,
                                make.pval = TRUE,
                                n.sim = 10)
       
       wt.avg(my.wt, siglvl = 0.05, sigcol = "red", 
              periodlab = "period (Anos)")
       
       
     }
     
     
   }) %>% 
     bindEvent(input$simular)
 
 # FIM DO SERVER DA PRIMEIRA PÁGINA----
    
 #dados para a segunda pagina----
   g_infos_compare <- reactive({
     
        id5 <- showNotification(# criando um aviso
           "Carregando seus dados, aguarde...", 
           duration = NULL, 
           #closeButton = FALSE
         )
         
         on.exit(removeNotification(id5), add = TRUE)
         
         keywords <- c(input$keyword_compare1, input$keyword_compare2)
         
        g_list <- gtrends(keyword = keywords ,
                           time = input$time_compare,
                           geo = input$region_compare,
                           gprop = input$channel_compare,
                           low_search_volume = TRUE,
                           compared_breakdown = TRUE,
                           tz = "Brazil/East")
         
        g_list[[1]] <- g_list[[1]] %>%
           mutate(
             hits=ifelse(hits=="<1",0.5,hits),
             hits=as.numeric(hits),
             date = as_datetime(date, tz = "Brazil/East")
           )
         
        g_list
   }) %>% # criando os dados que vem do gtrend
     bindEvent(input$buscar2)
 
 
 # gerando os mapas comparativos----
 
 output$mapplot_compare <- renderPlotly({
   
   
   if(input$region_compare == "BR"){
     
     
     id6 <- showNotification(# criando um aviso
       "Carregando seus dados, aguarde...", 
       duration = NULL, 
       #closeButton = FALSE
     )
     
     on.exit(removeNotification(id6), add = TRUE)
     
     
     df <- g_infos_compare()$interest_by_region %>%
       mutate(
         hits=str_remove(hits,"%"),
         hits=ifelse(hits=="<1",0.5,hits),
         hits=as.numeric(hits),
         hits=tidyr::replace_na(hits,0),
         location = str_to_lower(location),
         location = str_remove(location,"state of "),
         location = str_replace(location, "federal district", "distrito federal"),
         location = str_replace(location, "amazonas", "amazônas"),
         location = str_trim(location, side = "both")
         
       ) %>% 
       rename(
         name_state = location
       )
     
     states <- df_mapa_2() %>% 
       mutate(
         name_state = tolower(name_state)
       ) %>% 
       dplyr::left_join(x = .,
                        y = df,
                        by = "name_state"
       )
     ggplotly(
     
     states %>% 
       ggplot() +
       geom_sf(aes(fill=hits), color= NA, size=.15) +
       scale_fill_gradientn(colours = rev(scico(15, 
                                                palette = "tokyo")[2:7])) +
       my_theme2() +
       ggtitle("Google search interest for") +
       facet_wrap(~keyword, nrow = 1)
     )
     
   } else {
     
     
     df2 <- g_infos_compare()$interest_by_city %>%
       mutate(
         hits = str_remove(hits,"%"),
         hits = as.numeric(hits),
         hits = replace_na(hits,0),
         location = str_to_lower(location),
         location = str_trim(location, side = "both"),
         location = stri_trans_general(str = location, 
                                       id = "Latin-ASCII")
       ) %>% 
       rename(name_muni = location)
     
     state_map <- df_mapa_2() %>%
       mutate(
         name_muni = tolower(name_muni),
         name_muni = stri_trans_general(str = name_muni, 
                                        id = "Latin-ASCII")
       ) %>% 
       dplyr::left_join(x = .,
                        y = df2,
                        by = "name_muni"
       )
     
     ggplotly(
     state_map %>% 
       ggplot() +
       geom_sf(aes(fill=hits), color= NA, size=.15) +
       scale_fill_gradientn(colours = rev(scico(15, 
                                                palette = "tokyo")[2:7])) +
       my_theme2() +
       ggtitle("Google search interest for") +
       facet_wrap(~keyword, nrow = 1)
     )
     
   }# fim do if para gerar o mapa do br ou dos estados
   
   
 }) %>% #fim do mapplot
   bindEvent(input$buscar2)
 
 # tabela dos dados comparativos
   output$table_cidades_compare <- renderReactable({
     
     reactable(
       g_infos_compare()$interest_by_city %>% 
         mutate(
           hits=ifelse(hits=="<1",0.5,hits),
           hits=as.numeric(hits),
           hits=tidyr::replace_na(hits,0)
         )%>%
         dplyr::arrange(-hits) %>% 
          as_tibble()
       
     )
     
   })#fim da tabela de cidades
   
   # tabela busca camparativas----
   output$table_buscas_compare <- renderReactable({ 
     reactable(
       g_infos_compare()$related_queries %>% 
         as_tibble(),
         searchable = TRUE
     )
   })#fim da tabela de buscas relacionadas
   
  
  # st comparativas plot----
   output$trend_compare <- renderPlotly({
     
     ggplotly(
       g_infos_compare()$interest_over_time %>% 
         ggplot(aes(x=date, y=hits, group=keyword, col=keyword)) +
         geom_point()+
         geom_line(size = .9) + 
         xlab("Tempo") +
         ylab("Interesse relativo") +
         my_theme()+
         theme(legend.title = element_blank(),
               legend.position = "bottom",
               legend.text = element_text(size=12)) + 
         ggtitle("Volume de Busca no Google")
       
     )
     
   })
   
   # grafico correlação st comparação----
   output$correlation <- renderPlot({
     
     key_1 <- g_infos_compare()$interest_over_time %>% 
       filter(keyword == input$keyword_compare1)
     
     key_2 <- g_infos_compare()$interest_over_time %>% 
       filter(keyword == input$keyword_compare2)
     
     
   ccf(key_1$hits, key_2$hits)
     
     
   })
   
   #plot spec comparativo----
   output$speccorr <- renderPlot({
     
     my.data <- g_infos_compare()$interest_over_time %>% 
       pivot_wider(names_from = keyword, values_from = hits) %>% 
       rename(
         "x" = input$keyword_compare1,
         "y" = input$keyword_compare2
       ) %>% 
       as.data.frame()
     
     dt <- my.data[2,1] - my.data[1,1]
     
     if(dt == round(now()-(now()-60))){
     
     my.wc <- analyze.coherency(my.data,my.pair = c("x","y"), 
                                loess.span = 0.75, 
                                dt = 1/60,
                                make.pval = TRUE, n.sim = 10,
                                date.format = "%F %T", date.tz = "")
     
     wc.image(my.wc,
              color.key = "quantile",
              plot.coi = TRUE,
              siglvl.contour = 0.1, 
              siglvl.arrow = 0.05,
              which.arrow.sig = "wc",
              legend.params = list(lab = "wavelet coherence levels"),
              label.time.axis = TRUE, 
              show.date = TRUE,
              date.format = "%Y-%m-%d %H:%M:%S",
              label.period.axis = TRUE,
              periodlab = "period (Horas)")  
     
     } else if(dt > round(now()-(now()-60)) &  dt < today()-(today()-1)){
       
       my.wc <- analyze.coherency(my.data,my.pair = c("x","y"), 
                                  loess.span = 0.75, 
                                  dt = 1/24,
                                  make.pval = TRUE, n.sim = 10,
                                  date.format = "%F %T", date.tz = "")
       
       wc.image(my.wc,
                color.key = "quantile",
                plot.coi = TRUE,
                siglvl.contour = 0.1, 
                siglvl.arrow = 0.05,
                which.arrow.sig = "wc",
                legend.params = list(lab = "wavelet coherence levels"),
                label.time.axis = TRUE, 
                show.date = TRUE,
                date.format = "%Y-%m-%d %H:%M:%S",
                label.period.axis = TRUE,
                periodlab = "period (Dias)")  
       
       
       
       
     } else if(dt == today()-(today()-1)){
       
       
       my.wc <- analyze.coherency(my.data,my.pair = c("x","y"), 
                                  loess.span = 0.75, 
                                  dt = 1/30,
                                  make.pval = TRUE, n.sim = 10,
                                  date.format = "%F %T", date.tz = "")
       
       wc.image(my.wc,
                color.key = "quantile",
                plot.coi = TRUE,
                siglvl.contour = 0.1, 
                siglvl.arrow = 0.05,
                which.arrow.sig = "wc",
                legend.params = list(lab = "wavelet coherence levels"),
                label.time.axis = TRUE, 
                show.date = TRUE,
                date.format = "%Y-%m-%d",
                label.period.axis = TRUE,
                periodlab = "period (Meses)")  
       
       
     } else if(dt == today()-(today()-7)){
       
       
       my.wc <- analyze.coherency(my.data,my.pair = c("x","y"), 
                                  loess.span = 0.75, 
                                  dt = 1/52.18,
                                  make.pval = TRUE, n.sim = 10,
                                  date.format = "%F %T", date.tz = "")
       
       wc.image(my.wc,
                color.key = "quantile",
                plot.coi = TRUE,
                siglvl.contour = 0.1, 
                siglvl.arrow = 0.05,
                which.arrow.sig = "wc",
                legend.params = list(lab = "wavelet coherence levels"),
                label.time.axis = TRUE, 
                show.date = TRUE,
                date.format = "%Y-%m-%d",
                label.period.axis = TRUE,
                periodlab = "period (Anos)")  
       
       
       
       
     } else{
       
       
       my.wc <- analyze.coherency(my.data,my.pair = c("x","y"), 
                                  loess.span = 0.75, 
                                  dt = 1/12,
                                  make.pval = TRUE, n.sim = 10,
                                  date.format = "%F %T", date.tz = "")
       
       wc.image(my.wc,
                color.key = "quantile",
                plot.coi = TRUE,
                siglvl.contour = 0.1, 
                siglvl.arrow = 0.05,
                which.arrow.sig = "wc",
                legend.params = list(lab = "wavelet coherence levels"),
                label.time.axis = TRUE, 
                show.date = TRUE,
                date.format = "%Y-%m-%d",
                label.period.axis = TRUE,
                periodlab = "period (Anos)")  
       
       
     }
     
   })
   
 #FIM DO SERVER DA SEGUNDA PAGINA----

   #botoes de conectar e desconectar do browser que esta no docker----
   observe ({
     
     id7 <- showNotification(# criando um aviso
       "Conectando a um servidor remoto, aguarde...", 
       duration = NULL, 
       #closeButton = FALSE
     )
     
     on.exit(removeNotification(id7), add = TRUE)
     
     shell('docker run -d --name dooma -p 4445:4444 selenium/standalone-firefox', wait = TRUE)
     
    
   }) %>% 
     bindEvent(input$conectar)
   
  
    observe ({
      
     id8 <- showNotification(# criando um aviso
        "Desconectando, aguarde...", 
        duration = NULL, 
        #closeButton = FALSE
      )
      
      on.exit(removeNotification(id8), add = TRUE)
      
     #para e remove todos os container 
      shell('docker rm --force dooma', wait = TRUE)
    }) %>% 
      bindEvent(input$desconectar)
    
   # scraper das páginas do google----
   list_result <- reactive({
     
     id8 <- showNotification(# criando um aviso
       "Abrindo o browser e coletando as informações, aguarde...", 
       duration = NULL, 
       #closeButton = FALSE
     )
     
     on.exit(removeNotification(id8), add = TRUE)
     
     
     remDr <- remoteDriver(port = 4445L, browserName = "firefox")
     
     Sys.sleep(15)
     
     remDr$open()
     
     
     search_string = paste0(strsplit(input$search," ")[[1]],collapse="+")
      
     first_Page <- paste0('https://www.google.com/search?q=',search_string)
     result_By_Page <- list()
     
     for(i in 1:input$n_pages){
       print(i)
       remDr$navigate(first_Page)
       page_Content <- remDr$getPageSource()[[1]]
       result_By_Page[[i]] <- extract_Info_By_Page(page_Content)
       
       # criando o link para a próxima página
       
       first_Page <-read_html(page_Content) %>% 
         html_elements("#pnnext") %>% 
         html_attr("href") %>% 
         paste0("https://www.google.com/",.)
      }# fim do loop que faz o scrap
     
     df_final <- list()
     df_names <- c("link_patrocinado",
                   "link",
                   "titulo_patrocinado",
                   "titulo",
                   "paragrafo_patrocinado",
                   "paragrafo" )
     
     
     for (j in 1:6) {
       
       df <- c()
       
       for (i in 1: length(result_By_Page)) {
         
         df[[i]] <- result_By_Page[[i]][[1+j]]
         
       }
       
       df_final[[j]] <-as_tibble(unlist(df))
       names(df_final)[[j]] <- df_names[[j]]
       
       
     }
     
     df_final
   }) %>% 
     bindEvent(input$buscar3)
   
   
   # criando os dados de acordo com a entrada do usuário
   #removendo palavras usando user input----
   palavras_remove <- reactive({
     
     file <- input$lexicon_list
     ext <- tools::file_ext(file$datapath)
     
     req(file)
     validate(need(ext == "txt", "Please upload a txt file"))
     
     palavras_remove  <-  read.table(file$datapath, header = FALSE, sep = "\t")  
       
     palavras_remove <- c(palavras_remove) 
     
     palavras_remove
   }) 
   
   #display resultados escolha usuáruio----
   result_choose <- reactive({
     
     if(input$options == "total"){
       
       paragrafos <- list_result()[-c(1,2)] %>% 
         unlist() %>% 
         as_tibble() %>% 
         mutate(
           title_id = row_number()
           )
       
       
     } else {
       
       paragrafos  <-  list_result()[[as.numeric(input$options)]] %>% 
         as_tibble() %>% 
         mutate(
           title_id = row_number()
         )
       
       
     } 
     
     paragrafos_tokens<- paragrafos %>% 
       unnest_tokens(word,value)
     #  
     stop_words_pt<-data.frame(word=(stopwords(kind = "pt")))
     #  #
     paragrafos_tokens <- paragrafos_tokens %>% 
       anti_join(stop_words_pt)
     #  
     paragrafos_tokens<- paragrafos_tokens %>% 
       dplyr::filter(sapply(paragrafos_tokens$word, nchar)>2)
     
     
     
      if(!is.null(input$lexicon_list)){
        
       
    paragrafos_tokens <- paragrafos_tokens%>% 
         dplyr::filter(!word %in% palavras_remove()[[1]])
        
      } else{
        
        paragrafos_tokens
        
      }
     
   }) %>% 
     bindEvent(input$gerar_image)
  
#Correlação entre palavras google S----
   
output$corr_palavras <- renderPlot({
   
         correlacao <- result_choose() %>%
           group_by(word) %>% 
          dplyr::filter(n() > 3) %>%
           pairwise_cor(word, title_id, sort = TRUE,upper=F)%>%
           ungroup()
         
         correlacao<- correlacao %>%
          dplyr::filter(correlation >0.3)
         
         correlacao %>%
           arrange(-correlation) %>%
           top_n(15) %>% #Filtrar as 10 maiores
           graph_from_data_frame() %>%
           ggraph(layout = 'fr') + 
           guides(edge_alpha = "none", edge_width = "none") +
           scale_edge_colour_gradientn(limits = c(-1, 1), colors = c("firebrick2", "dodgerblue2")) +
           geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) + 
           geom_node_point(color = 'blue', size = 8) + 
           geom_node_text(aes(label = name), repel = TRUE) + 
           theme_graph() +
           labs(title = "Palavras que geralmente apareceram juntas")
   
   })
   

# tabela qunatidade de palavras----
   output$n_palavras <- renderReactable({
     
  reactable(
     result_choose() %>%
       group_by(word) %>%
       mutate(qnt_palavra = n()) %>%
      # dplyr::filter(qnt_palavra > 5) %>% 
       arrange(-qnt_palavra) %>% 
       ungroup() %>% 
       distinct(word,qnt_palavra) %>% 
       as_tibble()
       )
     
     
   })

  # plot wordcloud2----
   output$wordcloud <- renderWordcloud2({
     
     analise_palavras<-result_choose() %>%  
       group_by(word) %>%
       mutate(
         qnt_palavra = n()
       ) %>%
       arrange(-qnt_palavra) %>% 
       ungroup() %>% 
       distinct(word,qnt_palavra)
     
     
     wordcloud_dados <- analise_palavras %>% 
       #filter(qnt_palavra>2) %>% 
       select(word,qnt_palavra) %>%
       rename(freq = qnt_palavra)
     
     wordcloud2(data = wordcloud_dados)
   })
   

# tabela links----
output$table_links <- renderReactable({
    reactable(
    list_result()$link %>% 
     dplyr::filter(row_number() %% 2 != 0) %>% 
      as_tibble()
    )
  })

# tabela links patrocinados----
   output$table_links_spon <- renderReactable({
     reactable(
     list_result()$link_patrocinado %>% 
       filter(row_number() %% 2 != 0) %>% 
       as_tibble()
     )
   })
}#fim do servcer----

########## Run the application 
shinyApp(ui = ui, server = server)
