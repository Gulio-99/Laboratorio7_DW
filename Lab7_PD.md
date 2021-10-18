    library(dplyr)

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    library(lubridate)

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

    library(tidyr)
    library(stringr)
    library(ggplot2)
    library(leaflet)
    library(ggQC)
    library(qcc)

    ## Package 'qcc' version 2.7

    ## Type 'citation("qcc")' for citing this R package in publications.

    library(plotly)

    ## 
    ## Attaching package: 'plotly'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     last_plot

    ## The following object is masked from 'package:stats':
    ## 
    ##     filter

    ## The following object is masked from 'package:graphics':
    ## 
    ##     layout

    data <- read.csv("c1.csv")

    # Formato de fecha a yyyy-mm-dd
    data$Fecha <- dmy(data$Fecha)
    # Eliminación de las ultimas variables (X,X.1, X.2, X.3, X.4, X.5) que no representan ningun valor al análisis
    data <- data %>% select(!c(X,X.1, X.2, X.3, X.4, X.5))
    # Formato tidy para las columnas de rango de distacia a una sola
    data <- data %>% 
      mutate(X5.30=gsub(pattern = "  ", replacement = NA, x = X5.30),
             X30.45=gsub(pattern = "  ", replacement = NA, x = X30.45),
             X45.75=gsub(pattern = "  ", replacement = NA, x = X45.75),
             X75.120=gsub(pattern = "  ", replacement = NA, x = X75.120),
             X120.=gsub(pattern = "  ", replacement = NA, x = X120.))
    data <- data %>% 
      gather(Distancia, temp, X5.30:X120. , factor_key = TRUE) %>% 
      na.omit() %>% 
      select(!temp)
    data <- data %>% 
      mutate(Distancia = str_replace_all(Distancia, c("X" = "", '\\.' = "-")))
    # Eliminación del simbolo "Q" de Quetzales a las columnas númericas
    data <- data %>% 
      mutate(Camion_5=gsub(pattern = 'Q-', replacement = NA, x = Camion_5),
             Pickup=gsub(pattern = 'Q-', replacement = NA, x = Pickup),
             Moto=gsub(pattern = 'Q-', replacement = NA, x = Moto))
    # Unir columnas Camión, pickup y Moto a una sola variable "TipoV"
    data <- data %>% 
      gather(Tipov, Costo, Camion_5:Moto, factor_key = TRUE) %>% 
      na.omit()
    data <- data %>% 
      mutate(Costo=gsub(pattern = "Q", replacement = "", x = Costo))
    data$Costo <- as.numeric(data$Costo)
    # Costos Directos
    # Unir columnas de costos directos de cada tipo de vehiculo
    data <- data %>% 
      mutate(DirectoCamion=gsub(pattern = 'Q-', replacement = NA, x = directoCamion_5),
             DirectoPickup=gsub(pattern = 'Q-', replacement = NA, x = directoPickup),
             DirectoMoto=gsub(pattern = 'Q-', replacement = NA, x = directoMoto))
    # Unir las columnas de costos directos en una sola "Cst_Directo"
    data <- data %>% 
      gather(Tipo, Cst_Directo, DirectoCamion:DirectoMoto, factor_key = TRUE) %>% 
      na.omit() %>% 
      select(!Tipo)
    # Eliminación del simbolo "Q" en Cst_Directo
    data <- data %>% 
      mutate(Cst_Directo=gsub(pattern = "Q", replacement = "", x = Cst_Directo))
    data$Cst_Directo <- as.numeric(data$Cst_Directo)
    # Costos Fijos
    # Unir columnas de costos fijos de cada tipo de vehiculo
    data <- data %>% 
      mutate(FijoCamion=gsub(pattern = 'Q-', replacement = NA, x = fijoCamion_5),
             FijoPickup=gsub(pattern = 'Q-', replacement = NA, x = fijoPickup),
             FijoMoto=gsub(pattern = 'Q-', replacement = NA, x = fijoMoto))
    # Unir las columnas de costos fijos en una sola "Cst_Fijo"
    data <- data %>% 
      gather(Tipo, Cst_Fijo, FijoCamion:FijoMoto, factor_key = TRUE) %>% 
      na.omit() %>% 
      select(!Tipo)
    # Eliminación del simbolo "Q" en Cst_Fijo
    data <- data %>% 
      mutate(Cst_Fijo=gsub(pattern = "Q", replacement = "", x = Cst_Fijo))
    data$Cst_Fijo <- as.numeric(data$Cst_Fijo)
    # Eliminación del simbolo "Q" en "factura"
    data <- data %>% 
      mutate(factura=gsub(pattern = "Q", replacement = "", x = factura))
    data$factura <- as.numeric(data$factura)
    # Fortmato a FACTOR a variables categoricas
    data <- data %>% 
      mutate(Cod = as.factor(Cod),
             origen = as.factor(origen),
             height = as.factor(height),
             Distancia = as.factor(Distancia))
    # Calculando Ingresos
    data$Ingreso <- data$factura - data$Costo
    # Eliminación de las columnas antiguas de costos
    data <- data %>% select(!c(directoCamion_5,directoPickup,directoMoto,fijoCamion_5,fijoPickup,fijoMoto))

    # situacion de la emporesa
    vetnas <- data %>% 
      mutate(Mes = month(Fecha)) %>% 
      group_by(Mes) %>% 
      summarise(Ventas = sum(factura),
                Costo = sum(Costo))
    ggventas <- ggplot(vetnas,aes(x = Mes, y = Ventas))+
      geom_bar(stat='identity', width = 0.5, fill = "steelblue")+
      ggtitle("Ventas")+
      labs(x = "2017", y = "Q")+
      theme_minimal()
    ggventas2 <- ggplotly(ggventas)
    ggventas2

<div id="htmlwidget-4f7a70e20218ec5d8aa8" style="width:672px;height:480px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-4f7a70e20218ec5d8aa8">{"x":{"data":[{"orientation":"v","width":[0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5],"base":[0,0,0,0,0,0,0,0,0,0,0,0],"x":[1,2,3,4,5,6,7,8,9,10,11,12],"y":[3093370.98,2766568.99,3139264.25,3007125.67,3113697.82,2992436.49,3135909.11,3118555.04,3040445.51,3163691.25,3014113.74,3102917.46],"text":["Mes:  1<br />Ventas: 3093371","Mes:  2<br />Ventas: 2766569","Mes:  3<br />Ventas: 3139264","Mes:  4<br />Ventas: 3007126","Mes:  5<br />Ventas: 3113698","Mes:  6<br />Ventas: 2992436","Mes:  7<br />Ventas: 3135909","Mes:  8<br />Ventas: 3118555","Mes:  9<br />Ventas: 3040446","Mes: 10<br />Ventas: 3163691","Mes: 11<br />Ventas: 3014114","Mes: 12<br />Ventas: 3102917"],"type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(70,130,180,1)","line":{"width":1.88976377952756,"color":"transparent"}},"showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":43.7625570776256,"r":7.30593607305936,"b":40.1826484018265,"l":54.7945205479452},"font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"title":{"text":"Ventas","font":{"color":"rgba(0,0,0,1)","family":"","size":17.5342465753425},"x":0,"xref":"paper"},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[0.175,12.825],"tickmode":"array","ticktext":["2.5","5.0","7.5","10.0","12.5"],"tickvals":[2.5,5,7.5,10,12.5],"categoryorder":"array","categoryarray":["2.5","5.0","7.5","10.0","12.5"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.65296803652968,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"y","title":{"text":"2017","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-158184.5625,3321875.8125],"tickmode":"array","ticktext":["0e+00","1e+06","2e+06","3e+06"],"tickvals":[0,1000000,2000000,3000000],"categoryorder":"array","categoryarray":["0e+00","1e+06","2e+06","3e+06"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.65296803652968,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"x","title":{"text":"Q","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":false,"legend":{"bgcolor":null,"bordercolor":null,"borderwidth":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.689497716895}},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"source":"A","attrs":{"1018a78a48bc1":{"x":{},"y":{},"type":"bar"}},"cur_data":"1018a78a48bc1","visdat":{"1018a78a48bc1":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>

    #Estado de resultados
    Factura <- sum(data$factura)
    Costo <- sum(data$Costo)
    Utilidad_Bruta <- Factura - Costo
    Margen_Bruto <- Utilidad_Bruta/Factura
    Margen_2018 <- Margen_Bruto/1.25
    Utilidad_2018 <- Margen_2018*Factura
    Factura

    ## [1] 36688096

    Costo

    ## [1] 28174019

    Utilidad_Bruta

    ## [1] 8514077

    Margen_Bruto

    ## [1] 0.2320665

    Utilidad_2018

    ## [1] 6811262

    Margen_2018

    ## [1] 0.1856532

    #pareto
    tabla_postes <- table(data$ID)
    Pareto_factura <- pareto.chart(tabla_postes,
                                    main = "Pareto de Factura")

![](Lab7_PD_files/figure-markdown_strict/unnamed-chunk-6-1.png)

    #analisis de recorrido
    distanciasefectivas <- data %>% 
      group_by(Distancia) %>% 
      summarise(Costo = sum(Costo),
                Ingreso = sum(Ingreso),
                Viajes = n(),
                Relacion = round(Ingreso/Viajes,2))
    ggdistanciasingreso <- ggplot(distanciasefectivas,aes(x = reorder(Distancia, Ingreso), y = Ingreso))+
      geom_bar(stat='identity', width = 0.5, fill = "steelblue")+
      geom_text(aes(label=Ingreso), vjust=0.5, color="black", size=4)+
      ggtitle("Ingreso por Distancia")+
      labs(x = "Distancia", y = "Ingreso")+
      coord_flip()+
      theme_minimal()
    ggdistanciasingreso2 <- ggplotly(ggdistanciasingreso)
    ggdistanciasingreso2

<div id="htmlwidget-ece192a20ba5c68d7347" style="width:672px;height:480px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-ece192a20ba5c68d7347">{"x":{"data":[{"orientation":"h","width":[0.5,0.5,0.5,0.5,0.5],"base":[0,0,0,0,0],"x":[979332,1641827,1076186,1210636,3606096],"y":[1,4,2,3,5],"text":["reorder(Distancia, Ingreso): 120-<br />Ingreso:  979332","reorder(Distancia, Ingreso): 30-45<br />Ingreso: 1641827","reorder(Distancia, Ingreso): 45-75<br />Ingreso: 1076186","reorder(Distancia, Ingreso): 5-30<br />Ingreso: 1210636","reorder(Distancia, Ingreso): 75-120<br />Ingreso: 3606096"],"type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(70,130,180,1)","line":{"width":1.88976377952756,"color":"transparent"}},"showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[979332,1641827,1076186,1210636,3606096],"y":[1,4,2,3,5],"text":[979332,1641827,1076186,1210636,3606096],"hovertext":["reorder(Distancia, Ingreso): 120-<br />Ingreso:  979332<br />Ingreso:  979332","reorder(Distancia, Ingreso): 30-45<br />Ingreso: 1641827<br />Ingreso: 1641827","reorder(Distancia, Ingreso): 45-75<br />Ingreso: 1076186<br />Ingreso: 1076186","reorder(Distancia, Ingreso): 5-30<br />Ingreso: 1210636<br />Ingreso: 1210636","reorder(Distancia, Ingreso): 75-120<br />Ingreso: 3606096<br />Ingreso: 3606096"],"textfont":{"size":15.1181102362205,"color":"rgba(0,0,0,1)"},"type":"scatter","mode":"text","hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":43.7625570776256,"r":7.30593607305936,"b":40.1826484018265,"l":60.6392694063927},"font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"title":{"text":"Ingreso por Distancia","font":{"color":"rgba(0,0,0,1)","family":"","size":17.5342465753425},"x":0,"xref":"paper"},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-180304.8,3786400.8],"tickmode":"array","ticktext":["0e+00","1e+06","2e+06","3e+06"],"tickvals":[0,1000000,2000000,3000000],"categoryorder":"array","categoryarray":["0e+00","1e+06","2e+06","3e+06"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.65296803652968,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"y","title":{"text":"Ingreso","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[0.4,5.6],"tickmode":"array","ticktext":["120-","45-75","5-30","30-45","75-120"],"tickvals":[1,2,3,4,5],"categoryorder":"array","categoryarray":["120-","45-75","5-30","30-45","75-120"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.65296803652968,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"x","title":{"text":"Distancia","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":false,"legend":{"bgcolor":null,"bordercolor":null,"borderwidth":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.689497716895}},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"source":"A","attrs":{"1018a56ccec74":{"x":{},"y":{},"type":"bar"},"1018a27b7e031":{"x":{},"y":{},"label":{}}},"cur_data":"1018a56ccec74","visdat":{"1018a56ccec74":["function (y) ","x"],"1018a27b7e031":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>

    ggdistanciasef <- ggplot(distanciasefectivas,aes(x = reorder(Distancia, Relacion), y = Relacion))+
      geom_bar(stat='identity', width = 0.5, fill = "steelblue")+
      geom_text(aes(label=Relacion), vjust=0.5, color="black", size=4)+
      ggtitle("Relacion por Distancia")+
      labs(x = "Distancia", y = "Relacion")+
      coord_flip()+
      theme_minimal()
    ggdistanciasef2 <- ggplotly(ggdistanciasef)
    ggdistanciasef2

<div id="htmlwidget-6664eebabb484d309efa" style="width:672px;height:480px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-6664eebabb484d309efa">{"x":{"data":[{"orientation":"h","width":[0.5,0.5,0.5,0.5,0.5],"base":[0,0,0,0,0],"x":[37.13,31.13,31.39,30.6,32.56],"y":[5,2,3,1,4],"text":["reorder(Distancia, Relacion): 120-<br />Relacion: 37.13","reorder(Distancia, Relacion): 30-45<br />Relacion: 31.13","reorder(Distancia, Relacion): 45-75<br />Relacion: 31.39","reorder(Distancia, Relacion): 5-30<br />Relacion: 30.60","reorder(Distancia, Relacion): 75-120<br />Relacion: 32.56"],"type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(70,130,180,1)","line":{"width":1.88976377952756,"color":"transparent"}},"showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[37.13,31.13,31.39,30.6,32.56],"y":[5,2,3,1,4],"text":[37.13,31.13,31.39,30.6,32.56],"hovertext":["reorder(Distancia, Relacion): 120-<br />Relacion: 37.13<br />Relacion: 37.13","reorder(Distancia, Relacion): 30-45<br />Relacion: 31.13<br />Relacion: 31.13","reorder(Distancia, Relacion): 45-75<br />Relacion: 31.39<br />Relacion: 31.39","reorder(Distancia, Relacion): 5-30<br />Relacion: 30.60<br />Relacion: 30.60","reorder(Distancia, Relacion): 75-120<br />Relacion: 32.56<br />Relacion: 32.56"],"textfont":{"size":15.1181102362205,"color":"rgba(0,0,0,1)"},"type":"scatter","mode":"text","hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":43.7625570776256,"r":7.30593607305936,"b":40.1826484018265,"l":60.6392694063927},"font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"title":{"text":"Relacion por Distancia","font":{"color":"rgba(0,0,0,1)","family":"","size":17.5342465753425},"x":0,"xref":"paper"},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-1.8565,38.9865],"tickmode":"array","ticktext":["0","10","20","30"],"tickvals":[0,10,20,30],"categoryorder":"array","categoryarray":["0","10","20","30"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.65296803652968,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"y","title":{"text":"Relacion","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[0.4,5.6],"tickmode":"array","ticktext":["5-30","30-45","45-75","75-120","120-"],"tickvals":[1,2,3,4,5],"categoryorder":"array","categoryarray":["5-30","30-45","45-75","75-120","120-"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.65296803652968,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"x","title":{"text":"Distancia","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":false,"legend":{"bgcolor":null,"bordercolor":null,"borderwidth":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.689497716895}},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"source":"A","attrs":{"1018a51899369":{"x":{},"y":{},"type":"bar"},"1018a1f2cfc51":{"x":{},"y":{},"label":{}}},"cur_data":"1018a51899369","visdat":{"1018a51899369":["function (y) ","x"],"1018a1f2cfc51":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>

    #Unidades eficientes
    Vehiculos_eficientes <- data %>% 
      group_by(Tipov) %>%
      summarise(CostoDirecto = sum(Cst_Directo),
                CostoFijo = sum(Cst_Fijo),
                Factura = sum(factura),
                Ingreso = sum(Ingreso))
    ggdirectcost <- ggplot(Vehiculos_eficientes,aes(x = reorder(Tipov, CostoDirecto), y = CostoDirecto))+
      geom_bar(stat='identity', width = 0.5, fill = "steelblue")+
      geom_text(aes(label=CostoDirecto), vjust=0.5, color="black", size=4)+
      ggtitle("Costo Directo por Unidad")+
      labs(x = "Unidad", y = "Cst. Directo")+
      coord_flip()+
      theme_minimal()
    ggdirectcost2 <- ggplotly(ggdirectcost)    
    ggdirectcost2

<div id="htmlwidget-1b9efd04c8da104b8f98" style="width:672px;height:480px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-1b9efd04c8da104b8f98">{"x":{"data":[{"orientation":"h","width":[0.5,0.5,0.5],"base":[0,0,0],"x":[5499059.54,12144675.48,249872.04],"y":[2,3,1],"text":["reorder(Tipov, CostoDirecto): Camion_5<br />CostoDirecto:  5499060","reorder(Tipov, CostoDirecto): Pickup<br />CostoDirecto: 12144675","reorder(Tipov, CostoDirecto): Moto<br />CostoDirecto:   249872"],"type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(70,130,180,1)","line":{"width":1.88976377952756,"color":"transparent"}},"showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[5499059.54,12144675.48,249872.04],"y":[2,3,1],"text":[5499059.54,12144675.48,249872.04],"hovertext":["reorder(Tipov, CostoDirecto): Camion_5<br />CostoDirecto:  5499060<br />CostoDirecto:  5499060","reorder(Tipov, CostoDirecto): Pickup<br />CostoDirecto: 12144675<br />CostoDirecto: 12144675","reorder(Tipov, CostoDirecto): Moto<br />CostoDirecto:   249872<br />CostoDirecto:   249872"],"textfont":{"size":15.1181102362205,"color":"rgba(0,0,0,1)"},"type":"scatter","mode":"text","hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":43.7625570776256,"r":7.30593607305936,"b":40.1826484018265,"l":72.3287671232877},"font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"title":{"text":"Costo Directo por Unidad","font":{"color":"rgba(0,0,0,1)","family":"","size":17.5342465753425},"x":0,"xref":"paper"},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-607233.774,12751909.254],"tickmode":"array","ticktext":["0","2500000","5000000","7500000","10000000","12500000"],"tickvals":[0,2500000,5000000,7500000,10000000,12500000],"categoryorder":"array","categoryarray":["0","2500000","5000000","7500000","10000000","12500000"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.65296803652968,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"y","title":{"text":"Cst. Directo","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[0.4,3.6],"tickmode":"array","ticktext":["Moto","Camion_5","Pickup"],"tickvals":[1,2,3],"categoryorder":"array","categoryarray":["Moto","Camion_5","Pickup"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.65296803652968,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"x","title":{"text":"Unidad","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":false,"legend":{"bgcolor":null,"bordercolor":null,"borderwidth":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.689497716895}},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"source":"A","attrs":{"1018a5d530a17":{"x":{},"y":{},"type":"bar"},"1018a76bb9bde":{"x":{},"y":{},"label":{}}},"cur_data":"1018a5d530a17","visdat":{"1018a5d530a17":["function (y) ","x"],"1018a76bb9bde":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>

    ggfijocost <- ggplot(Vehiculos_eficientes,aes(x = reorder(Tipov, CostoFijo), y = CostoFijo))+
      geom_bar(stat='identity', width = 0.5, fill = "steelblue")+
      geom_text(aes(label=CostoFijo), vjust=0.5, color="black", size=4)+
      ggtitle("Costo Fijo por Unidad")+
      labs(x = "Unidad", y = "Cst. Fijo")+
      coord_flip()+
      theme_minimal()
    ggfijocost2 <- ggplotly(ggfijocost)
    ggfijocost2

<div id="htmlwidget-bdfce6baa1e2c75d16a6" style="width:672px;height:480px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-bdfce6baa1e2c75d16a6">{"x":{"data":[{"orientation":"h","width":[0.5,0.5,0.5],"base":[0,0,0],"x":[3159303.74,6977232.39,143875.87],"y":[2,3,1],"text":["reorder(Tipov, CostoFijo): Camion_5<br />CostoFijo: 3159303.7","reorder(Tipov, CostoFijo): Pickup<br />CostoFijo: 6977232.4","reorder(Tipov, CostoFijo): Moto<br />CostoFijo:  143875.9"],"type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(70,130,180,1)","line":{"width":1.88976377952756,"color":"transparent"}},"showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[3159303.74,6977232.39,143875.87],"y":[2,3,1],"text":[3159303.74,6977232.39,143875.87],"hovertext":["reorder(Tipov, CostoFijo): Camion_5<br />CostoFijo: 3159303.7<br />CostoFijo: 3159303.7","reorder(Tipov, CostoFijo): Pickup<br />CostoFijo: 6977232.4<br />CostoFijo: 6977232.4","reorder(Tipov, CostoFijo): Moto<br />CostoFijo:  143875.9<br />CostoFijo:  143875.9"],"textfont":{"size":15.1181102362205,"color":"rgba(0,0,0,1)"},"type":"scatter","mode":"text","hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":43.7625570776256,"r":7.30593607305936,"b":40.1826484018265,"l":72.3287671232877},"font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"title":{"text":"Costo Fijo por Unidad","font":{"color":"rgba(0,0,0,1)","family":"","size":17.5342465753425},"x":0,"xref":"paper"},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-348861.6195,7326094.0095],"tickmode":"array","ticktext":["0e+00","2e+06","4e+06","6e+06"],"tickvals":[0,2000000,4000000,6000000],"categoryorder":"array","categoryarray":["0e+00","2e+06","4e+06","6e+06"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.65296803652968,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"y","title":{"text":"Cst. Fijo","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[0.4,3.6],"tickmode":"array","ticktext":["Moto","Camion_5","Pickup"],"tickvals":[1,2,3],"categoryorder":"array","categoryarray":["Moto","Camion_5","Pickup"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.65296803652968,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"x","title":{"text":"Unidad","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":false,"legend":{"bgcolor":null,"bordercolor":null,"borderwidth":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.689497716895}},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"source":"A","attrs":{"1018a7c80ed9f":{"x":{},"y":{},"type":"bar"},"1018a7470a194":{"x":{},"y":{},"label":{}}},"cur_data":"1018a7c80ed9f","visdat":{"1018a7c80ed9f":["function (y) ","x"],"1018a7470a194":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>

    ggingreso <- ggplot(Vehiculos_eficientes,aes(x = reorder(Tipov, Ingreso), y = Ingreso))+
      geom_bar(stat='identity', width = 0.5, fill = "steelblue")+
      geom_text(aes(label=Ingreso), vjust=0.5, color="black", size=4)+
      ggtitle("Ingreso por Unidad")+
      labs(x = "Unidad", y = "Ingreso")+
      coord_flip()+
      theme_minimal()
    ggingreso2 <- ggplotly(ggingreso)
    ggingreso2

<div id="htmlwidget-81d9ffaae0e453b7f800" style="width:672px;height:480px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-81d9ffaae0e453b7f800">{"x":{"data":[{"orientation":"h","width":[0.5,0.5,0.5],"base":[0,0,0],"x":[2803617,5380176,330284],"y":[2,3,1],"text":["reorder(Tipov, Ingreso): Camion_5<br />Ingreso: 2803617","reorder(Tipov, Ingreso): Pickup<br />Ingreso: 5380176","reorder(Tipov, Ingreso): Moto<br />Ingreso:  330284"],"type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(70,130,180,1)","line":{"width":1.88976377952756,"color":"transparent"}},"showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[2803617,5380176,330284],"y":[2,3,1],"text":[2803617,5380176,330284],"hovertext":["reorder(Tipov, Ingreso): Camion_5<br />Ingreso: 2803617<br />Ingreso: 2803617","reorder(Tipov, Ingreso): Pickup<br />Ingreso: 5380176<br />Ingreso: 5380176","reorder(Tipov, Ingreso): Moto<br />Ingreso:  330284<br />Ingreso:  330284"],"textfont":{"size":15.1181102362205,"color":"rgba(0,0,0,1)"},"type":"scatter","mode":"text","hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":43.7625570776256,"r":7.30593607305936,"b":40.1826484018265,"l":72.3287671232877},"font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"title":{"text":"Ingreso por Unidad","font":{"color":"rgba(0,0,0,1)","family":"","size":17.5342465753425},"x":0,"xref":"paper"},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-269008.8,5649184.8],"tickmode":"array","ticktext":["0e+00","2e+06","4e+06"],"tickvals":[0,2000000,4000000],"categoryorder":"array","categoryarray":["0e+00","2e+06","4e+06"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.65296803652968,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"y","title":{"text":"Ingreso","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[0.4,3.6],"tickmode":"array","ticktext":["Moto","Camion_5","Pickup"],"tickvals":[1,2,3],"categoryorder":"array","categoryarray":["Moto","Camion_5","Pickup"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.65296803652968,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"x","title":{"text":"Unidad","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":false,"legend":{"bgcolor":null,"bordercolor":null,"borderwidth":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.689497716895}},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"source":"A","attrs":{"1018a5d192dd4":{"x":{},"y":{},"type":"bar"},"1018a200fe90c":{"x":{},"y":{},"label":{}}},"cur_data":"1018a5d192dd4","visdat":{"1018a5d192dd4":["function (y) ","x"],"1018a200fe90c":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>

    #Actividades de la empresa
    actividadesef <- data %>% 
      group_by(Cod) %>% 
      summarise(costo = sum(Costo),
                Ingreso = sum(Ingreso),
                Cantidad = n(),
                relacion = sum(Ingreso)/Cantidad)
    ggactingreso <- ggplot(actividadesef,aes(x = reorder(Cod, Ingreso), y = Ingreso))+
      geom_bar(stat='identity', width = 0.5, fill = "steelblue")+
      ggtitle("Ingreso por Actividad")+
      labs(x = "Actividad", y = "Ingreso")+
      coord_flip()+
      theme_minimal()
    ggactingreso2 <- ggplotly(ggactingreso)
    ggactingreso2

<div id="htmlwidget-eaba4ab2e69975abc81c" style="width:672px;height:480px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-eaba4ab2e69975abc81c">{"x":{"data":[{"orientation":"h","width":[0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5],"base":[0,0,0,0,0,0,0,0,0,0],"x":[1011009,854277,74874,259247,2652070,442789,1044181,1365161,68471,741998],"y":[7,6,2,3,10,4,8,9,1,5],"text":["reorder(Cod, Ingreso): CAMBIO_CORRECTIVO<br />Ingreso: 1011009","reorder(Cod, Ingreso): CAMBIO_FUSIBLE<br />Ingreso:  854277","reorder(Cod, Ingreso): CAMBIO_PUENTES<br />Ingreso:   74874","reorder(Cod, Ingreso): OTRO<br />Ingreso:  259247","reorder(Cod, Ingreso): REVISION<br />Ingreso: 2652070","reorder(Cod, Ingreso): REVISION_TRANSFORMADOR<br />Ingreso:  442789","reorder(Cod, Ingreso): VERIFICACION_INDICADORES<br />Ingreso: 1044181","reorder(Cod, Ingreso): VERIFICACION_MEDIDORES<br />Ingreso: 1365161","reorder(Cod, Ingreso): VISITA<br />Ingreso:   68471","reorder(Cod, Ingreso): VISITA_POR_CORRECCION<br />Ingreso:  741998"],"type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(70,130,180,1)","line":{"width":1.88976377952756,"color":"transparent"}},"showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":43.7625570776256,"r":7.30593607305936,"b":40.1826484018265,"l":165.844748858448},"font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"title":{"text":"Ingreso por Actividad","font":{"color":"rgba(0,0,0,1)","family":"","size":17.5342465753425},"x":0,"xref":"paper"},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-132603.5,2784673.5],"tickmode":"array","ticktext":["0e+00","1e+06","2e+06"],"tickvals":[0,1000000,2000000],"categoryorder":"array","categoryarray":["0e+00","1e+06","2e+06"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.65296803652968,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"y","title":{"text":"Ingreso","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[0.4,10.6],"tickmode":"array","ticktext":["VISITA","CAMBIO_PUENTES","OTRO","REVISION_TRANSFORMADOR","VISITA_POR_CORRECCION","CAMBIO_FUSIBLE","CAMBIO_CORRECTIVO","VERIFICACION_INDICADORES","VERIFICACION_MEDIDORES","REVISION"],"tickvals":[1,2,3,4,5,6,7,8,9,10],"categoryorder":"array","categoryarray":["VISITA","CAMBIO_PUENTES","OTRO","REVISION_TRANSFORMADOR","VISITA_POR_CORRECCION","CAMBIO_FUSIBLE","CAMBIO_CORRECTIVO","VERIFICACION_INDICADORES","VERIFICACION_MEDIDORES","REVISION"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.65296803652968,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"x","title":{"text":"Actividad","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":false,"legend":{"bgcolor":null,"bordercolor":null,"borderwidth":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.689497716895}},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"source":"A","attrs":{"1018adb95b1d":{"x":{},"y":{},"type":"bar"}},"cur_data":"1018adb95b1d","visdat":{"1018adb95b1d":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>

    ggactrel <- ggplot(actividadesef,aes(x = reorder(Cod, relacion), y = relacion))+
      geom_bar(stat='identity', width = 0.5, fill = "steelblue")+
      ggtitle("Relacion por Actividad")+
      labs(x = "Actividad", y = "Relacion")+
      coord_flip()+
      theme_minimal()
    ggactrel2 <- ggplotly(ggactrel)
    ggactrel2

<div id="htmlwidget-04d5963af2e593c8e289" style="width:672px;height:480px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-04d5963af2e593c8e289">{"x":{"data":[{"orientation":"h","width":[0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5],"base":[0,0,0,0,0,0,0,0,0,0],"x":[28.1861495999331,44.6237463435019,40.7145187601958,44.1572134219043,29.3734494063441,35.3326683689754,32.7124373433584,28.8051188994155,28.8785322648671,44.9750272760335],"y":[1,9,7,8,4,6,5,2,3,10],"text":["reorder(Cod, relacion): CAMBIO_CORRECTIVO<br />relacion: 28.18615","reorder(Cod, relacion): CAMBIO_FUSIBLE<br />relacion: 44.62375","reorder(Cod, relacion): CAMBIO_PUENTES<br />relacion: 40.71452","reorder(Cod, relacion): OTRO<br />relacion: 44.15721","reorder(Cod, relacion): REVISION<br />relacion: 29.37345","reorder(Cod, relacion): REVISION_TRANSFORMADOR<br />relacion: 35.33267","reorder(Cod, relacion): VERIFICACION_INDICADORES<br />relacion: 32.71244","reorder(Cod, relacion): VERIFICACION_MEDIDORES<br />relacion: 28.80512","reorder(Cod, relacion): VISITA<br />relacion: 28.87853","reorder(Cod, relacion): VISITA_POR_CORRECCION<br />relacion: 44.97503"],"type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(70,130,180,1)","line":{"width":1.88976377952756,"color":"transparent"}},"showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":43.7625570776256,"r":7.30593607305936,"b":40.1826484018265,"l":165.844748858448},"font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"title":{"text":"Relacion por Actividad","font":{"color":"rgba(0,0,0,1)","family":"","size":17.5342465753425},"x":0,"xref":"paper"},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-2.24875136380167,47.2237786398351],"tickmode":"array","ticktext":["0","10","20","30","40"],"tickvals":[0,10,20,30,40],"categoryorder":"array","categoryarray":["0","10","20","30","40"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.65296803652968,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"y","title":{"text":"Relacion","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[0.4,10.6],"tickmode":"array","ticktext":["CAMBIO_CORRECTIVO","VERIFICACION_MEDIDORES","VISITA","REVISION","VERIFICACION_INDICADORES","REVISION_TRANSFORMADOR","CAMBIO_PUENTES","OTRO","CAMBIO_FUSIBLE","VISITA_POR_CORRECCION"],"tickvals":[1,2,3,4,5,6,7,8,9,10],"categoryorder":"array","categoryarray":["CAMBIO_CORRECTIVO","VERIFICACION_MEDIDORES","VISITA","REVISION","VERIFICACION_INDICADORES","REVISION_TRANSFORMADOR","CAMBIO_PUENTES","OTRO","CAMBIO_FUSIBLE","VISITA_POR_CORRECCION"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.65296803652968,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"x","title":{"text":"Actividad","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":false,"legend":{"bgcolor":null,"bordercolor":null,"borderwidth":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.689497716895}},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"source":"A","attrs":{"1018a376f6d73":{"x":{},"y":{},"type":"bar"}},"cur_data":"1018a376f6d73","visdat":{"1018a376f6d73":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
