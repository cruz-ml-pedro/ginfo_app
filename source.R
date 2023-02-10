library(tm)
library(xml2)
library(scico)
library(geobr)
library(rvest)
library(widyr)
library(plotly)
library(igraph)
library(ggraph)
library(prophet)
library(stringi)
library(stringr)
library(gtrendsR)
library(dygraphs)
library(tidytext)
library(reactable)
library(lubridate)
library(tidyverse)
#library(lexiconPT)
library(RSelenium)
library(wordcloud2)
library(WaveletComp)
library(leaflet)
###########################################
##########################################
###########################################

my_theme <- function() {
  theme_bw() +
    theme(panel.background = element_blank()) +
    theme(plot.background = element_rect(fill = "seashell")) +
    theme(panel.border = element_blank()) +                     # facet border
    theme(strip.background = element_blank()) +                 # facet title background
    theme(plot.margin = unit(c(.5, .5, .5, .5), "cm")) +
    theme(panel.spacing = unit(3, "lines")) +
    theme(panel.grid.major = element_blank()) +
    theme(panel.grid.minor = element_blank()) +
    theme(legend.background = element_blank()) +
    theme(legend.key = element_blank()) +
    theme(legend.title = element_blank())
}

my_theme2 = function() {
  my_theme() +
    theme(axis.title = element_blank()) +
    theme(axis.text = element_blank()) +
    theme(axis.ticks = element_blank())
}

#####################################################

mapa_br <- read_state(year=2019)

#####################################################

#####################################################

#configuração do layout das tabelas
options(reactable.theme = reactableTheme(
  color = "hsl(233, 9%, 87%)",
  backgroundColor = "hsl(233, 9%, 19%)",
  borderColor = "hsl(233, 9%, 22%)",
  stripedColor = "hsl(233, 12%, 22%)",
  highlightColor = "hsl(233, 12%, 24%)",
  inputStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
  selectStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
  pageButtonHoverStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
  pageButtonActiveStyle = list(backgroundColor = "hsl(233, 9%, 28%)")
))# tema para as tabelas


#####################################################
#####################################################
#####################################################

locais <- c(
  "Brasil" = "BR",
  "Acre" = "BR-AC",
  "Alagoas" = "BR-AL",
  "Amapa" = "BR-AP",
  "Amazônas" = "BR-AM",
  "Bahia" = "BR-BA",
  "Ceará" = "BR-CE",
  "Distrito Federal" = "BR-DF",
  "Espirito Santo" = "BR-ES",
  "Goiás" = "BR-GO",
  "Maranhão" = "BR-MA",
  "Mato Grosso" = "BR-MT",
  "Mato Grosso do Sul" = "BR-MS",
  "Minas Gerais" = "BR-MG",
  "Pará" = "BR-PA",
  "Paraíba" = "BR-PB",
  "Paraná" = "BR-PR",
  "Pernanbuco" = "BR-PE",
  "Piauí" = "BR-PI",
  "Rio de Janeiro" = "BR-RJ",
  "Rio Grande do Norte" = "BR-RN",
  "Rio Grande do Sul" = "BR-RS",
  "Rondônia" = "BR-RO",
  "Roraima" = "BR-RR",
  "Santa Catarina" = "BR-SC",
  "São Paulo" = "BR-SP",
  "Sergipe" = "BR-SE",
  "Tocantins"="BR-TO")

canais <- c(
  "web",
  "news",
  "images",
  "froogle",
  "youtube")

tempo <- c(
  "Última Hora" = "now 1-H",
  "Últimas 4 Horas" = "now 4-H",
  "Último dia" = "now 1-d",
  "Últimos sete dias" = "now 7-d",
  "Últimos 30 dias" = "today 1-m",
  "Últimos 90 dias" = "today 3-m",
  "Últimos 12 meses" = "today 12-m",
  "Últimos 5 anos" = "today+5-y",
  "Todo o intervalo de tempo" = "all")

###########################################################
###########################################################
###########################################################

extract_Info_By_Page <- function(page_Content){
  
  page <- read_html(page_Content)
  
  link_patrocinado <- page %>% 
    html_elements("#tads .qzEoUe") %>% 
    html_text() 
  
  link <- page %>% 
    html_elements(".tjvcx") %>% 
    html_text()
  
  titulo_patrocinado <- page %>%
    html_elements(".v0nnCb span") %>% 
    html_text()
  
  titulo <- page %>% 
    html_elements(".DKV0Md") %>%
    html_text()
  
  paragrafo_patrocinado <- page %>%
    html_elements("#tads .lyLwlc span") %>% 
    html_text()
  
  
  paragrafo <- page %>% 
    html_elements(".lEBKkf") %>% 
    html_text()
  
  return(list(page = page,
              link_patrocinado=link_patrocinado, 
              link = link,
              titulo_patrocinado = titulo_patrocinado,
              titulo = titulo,
              paragrafo_patrocinado = paragrafo_patrocinado,
              paragrafo=paragrafo))
}

################################################
wordcloud_choices <- c(
                        "Total" = "total",
                        "Título Patrocinado" = "3",
                        "Título" = "4",
                        "Parágrafo Patrocinado" = "5",
                        "Parágrafo" = "6"
                        
                      )




