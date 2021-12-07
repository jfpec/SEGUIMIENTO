library(shiny)
if(require(shinyWidgets)==FALSE){install.packages("shinyWidgets",dependencies = TRUE)}
if(require(ggplot2)==FALSE){install.packages("ggplot2",dependencies = TRUE)}
if(require(leaflet)==FALSE){install.packages("leaflet",dependencies = TRUE)}
if(require(sf)==FALSE){install.packages("sf",dependencies = TRUE)}
if(require(plotly)==FALSE){install.packages("plotly",dependencies = TRUE)}
if(require(reshape2)==FALSE){install.packages("reshape2",dependencies = TRUE)}
if(require(raster)==FALSE){install.packages("raster",dependencies = TRUE)}
if(require(rgdal)==FALSE){install.packages("rgdal",dependencies = TRUE)}
if(require(dplyr)==FALSE){install.packages("dplyr",dependencies = TRUE)}

data_ndvi <- read.csv("DATOS.csv")

#CALCULO DE FECHAS Y DE ZAFRAS
ano.actual <- as.numeric(format(Sys.Date(), format='%Y'))
mes.actual <- as.numeric(format(Sys.Date(), format='%m'))
ano<-as.numeric(if(mes.actual>10){ano<-ano.actual} else{ano<- (as.numeric(ano.actual)-1)})
Zafra.actual<-paste0(ano-1,"/", ano)
Zafra.siguiente <- paste0(ano, "/", ano+1)
Zafra.siguiente2 <- paste0(ano+1, "/", ano+2)
Zafra.anterior <- paste0(ano-2,"/", ano-1)
Zafra.anterior1 <- paste0(ano-3,"/", ano-2)

data_prod <- read.csv("DATOS_PROD.csv")
data_prod_u <- subset(data_prod, UP=="SI")
data_prod_u <- data_prod_u[,c("LOTE", "TCH_ANTERIOR", "TCH", "DIF", "CATEGORIA")]

sf1 <- sf::st_as_sf(shapefile('LOTES_IMSA.shp'))
mun <- sf::st_transform(sf1, 4326)
mun<- merge(mun, data_prod_u, by.x ="LOTE_N", by.y = "LOTE" , all = FALSE)

ui <- fluidPage(
  titlePanel("Evolucion del desarollo  del cultivo caña de azucar"),
  fluidRow(
    column(
      width = 3,  offset = 0,
      tags$h3("Seleccionar filtro"),
      panel(
        selectizeGroupUI(
          inline=FALSE,
          id = "my-filters",
          params = list(
            REGION = list(inputId = "REGION", title = "REGION:"),
            FINCA = list(inputId = "FINCA", title = "FINCA:"),
            trans = list(inputId = "LOTE", title = "LOTE:"),
            CONDICION = list(inputId = "CONDICION", title = "CONDICION:")
          )
        ), status = "primary"
      ),
    ),
    
    column(
      width = 5, offset = 0,
      plotlyOutput("curvas_O")
    ),
    
    column(
      width = 4, offset = 0,
      plotlyOutput("produccion_O")
    )
    
  ),
  
  fluidRow(
    column(width = 3
    ),
    
    column(
      width = 3, offset = 0,
      tableOutput("variedad_O")
    ),
    
    column(
      width = 5, offset = 1,
      leafletOutput("leaflet_O")
    )
  )
)


server <- function(input, output, session) {
  
  res_mod <- callModule(
    module = selectizeGroupServer,
    id = "my-filters",
    data = data_ndvi,
    vars = c("REGION", "FINCA", "LOTE", "CONDICION")
  )
  
  #curva de ndvi
  output$curvas_O <- renderPlotly({
    table_f <- as.data.frame(res_mod())
    data <- subset(data_ndvi, LOTE %in% unique(table_f$LOTE))
    ggplotly(ggplot(data, aes(EDAD_IMAGEN, NDVI, colour= ANO_ZAFRA))+
               geom_smooth(method = if (nrow(data)>1000){"gam"}else{"loess"}, se=F)+
               labs(x="EDAD", 
                    y="NDVI")+
               theme(legend.position="bottom")+
               guides(colour=guide_legend(title="Temporada"))
             
    )
  })
  
  #graficas de produccion
  output$produccion_O <- renderPlotly({
    table_f <- as.data.frame(res_mod())
    data <- subset(data_prod, LOTE %in% unique(table_f$LOTE))
    data <- data %>% group_by(ANO_ZAFRA) %>% summarise(TCH = weighted.mean(TCH, AREA, na.rm=TRUE),
                                                       KgAzTc = weighted.mean(RENDIMIENTO_BASCULA, TM, na.rm = TRUE),
                                                       TAH = weighted.mean(TAH, AREA, na.rm=TRUE)/1000)
    
    data<-melt(data, id.vars = c("ANO_ZAFRA"), variable.name = "Variable", 
               value.name = "valor")
    
    ggplotly(ggplot(data, aes(ANO_ZAFRA, valor, fill = ANO_ZAFRA))+
               geom_bar(stat = "identity")+
               theme(legend.position="bottom")+
               guides(fill=guide_legend(title="Temporada"))+
               facet_wrap( ~ Variable, scales = "free_y")+
               theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())+
               labs(x="", 
                    y=""))
  })
  
  #CAPA DE LOTES
  output$leaflet_O <- renderLeaflet({
    table_f <- as.data.frame(res_mod())
    mun_estado <- subset(mun, LOTE_N %in% unique(table_f$LOTE))
    mun_estado$DIF <-as.numeric(mun_estado$DIF)
    bins<-c(-100, -10, -2, 2, 10,  100)
    pal<-colorBin("RdYlBu", domain = mun_estado$DIF, bins = bins)
    leaflet(mun_estado) %>%
      addTiles() %>%
      addPolygons(fillColor = ~pal(DIF), 
                  fillOpacity = 0.5,
                  color = "#777777",
                  label = (sprintf("<strong>%s</strong><br/>%s LOTE </sup><br/>%s CATEGORIA </sup>",
                                   mun_estado$FINCA, 
                                   mun_estado$LOTE_N, 
                                   mun_estado$CATEGORIA) %>% lapply(htmltools::HTML)),
                  weight = 0.1)%>%
      addLegend(pal = pal, values = ~LOTE_N, opacity = 0.6, title = "CONDICION",
                position = "bottomright")
  })
  output$variedad_O <- renderTable({
    table_f <- as.data.frame(res_mod())
    data <- data_prod %>% subset(LOTE %in% unique(table_f$LOTE))
    data <- distinct(data[,c("ANO_ZAFRA", "LOTE", "VARIEDAD", "AREA")] )
    data_tabla <- data %>% group_by(ANO_ZAFRA, VARIEDAD) %>% 
      summarise(AREA = sum(AREA))
    data_tabla <- dcast(data_tabla, VARIEDAD ~ ANO_ZAFRA, value.var = "AREA")
    data_tabla
    
  })
  
}

shinyApp(ui, server)
