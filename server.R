##### LIBRARIES ####
library(shiny)
library(shinythemes)
library(leaflet)
library(sp)
library(dplyr)
library(ggplot2)
library(rgeos)
library(maptools)
library(rgdal)
library(ggmap)
library(highcharter)
library(RColorBrewer)
##### DATA ####
load("./data/HOGARES.Rdata")
load("./data/SHPWORLDUN.Rdata")
load("./data/INDIVIDUOS.Rdata")
load("./data/indicators.Rdata")

#load("C:\\Users\\jgaleano\\Desktop\\DATOS UN\\APP\\data\\HOGARES.Rdata")
#load("C:\\Users\\jgaleano\\Desktop\\DATOS UN\\APP\\data\\SHPWORLDUN.Rdata")
#load("C:\\Users\\jgaleano\\Desktop\\DATOS UN\\APP\\data\\indicators.Rdata")

#AVER<-SHPWORLDUN@data

##### GLOBAL ####
HOGARES$indicator2<-with(HOGARES, ifelse(indicator=="H1",  "Mean Household size",
                                  ifelse(indicator=="H2",  "1 person HH/all HH",
                                  ifelse(indicator=="H3",  "2 person HH/all HH",
                                  ifelse(indicator=="H4",  "3 person HH/all HH",
                                  ifelse(indicator=="H5",  "4 person HH/all HH",
                                  ifelse(indicator=="H6",  "5 person HH/all HH",
                                  ifelse(indicator=="H7",  "6 person HH/all HH",
                                  ifelse(indicator=="H8",  "HH with a female head (%)", 
                                  ifelse(indicator=="H9",  "HH with children under 15 (%)", 
                                  ifelse(indicator=="H10", "HH with person over 60 (%)",
                                  ifelse(indicator=="H11", "Average number of children for HH with children under 15",
                                  ifelse(indicator=="H12", "HH with children under 15 only with mother (%)",
                                  ifelse(indicator=="H13", "HH with children under 15 only with father (%)",
                                  ifelse(indicator=="H14", "HH with children under 15  with both parents (%)",
                                  ifelse(indicator=="H15", "HH with children under 15  with none parents (%)",
                                  ifelse(indicator=="H16", "Head 60+ (%)",
                                  ifelse(indicator=="H17", "Single mother/Female Head (%)",
                                  ifelse(indicator=="H18", "HH with children under 15 & person(s) over 60 (%)",Value)))))))))))))))))))

HOGARES$DataSource <- factor(HOGARES$DataSource,
                             levels = c("IPUMS",
                                        "DHS",
                                        "LFS"))





INDI$variableN<-with(INDI, ifelse(Variable=="P1",  "Living alone",
                            ifelse(Variable=="P2", "Living with father",
                            ifelse(Variable=="P3", "Living with mother",
                            ifelse(Variable=="P4", "Living with spouse",
                            ifelse(Variable=="P5", "Living with children",
                            ifelse(Variable=="P6", "Living with others","no")))))))


setentacinco<-c("0-4","5-9","10-14","15-19","20-24",
           "25-29" ,"30-34", "35-39" ,"40-44", "45-49",
           "50-54" ,"55-59" ,"60-64" ,"65-69", "70-74", 
           "75+")

ochenta<-c("0-4","5-9","10-14","15-19","20-24",
           "25-29" ,"30-34", "35-39" ,"40-44", "45-49",
           "50-54" ,"55-59" ,"60-64" ,"65-69", "70-74", 
           "75-79" ,"80+")

ochentacinco<-c("0-4",   "5-9",   "10-14" ,"15-19", "20-24",
                "25-29" ,"30-34", "35-39" ,"40-44", "45-49",
                "50-54" ,"55-59" ,"60-64" ,"65-69", "70-74", 
                "75-79" ,"80-84", "85+")
noventa<-c("0-4",   "5-9",   "10-14" ,"15-19", "20-24",
           "25-29" ,"30-34", "35-39" ,"40-44", "45-49",
           "50-54" ,"55-59" ,"60-64" ,"65-69", "70-74", 
           "75-79" ,"80-84", "85-89" ,"90+")

noventacinco<-c("0-4",   "5-9",   "10-14" ,"15-19", "20-24",
                "25-29" ,"30-34", "35-39" ,"40-44", "45-49",
                "50-54" ,"55-59" ,"60-64" ,"65-69", "70-74", 
                "75-79" ,"80-84", "85-89" ,"90-94", "95+")

cien<-c("0-4",   "5-9",   "10-14" ,"15-19", "20-24",
        "25-29" ,"30-34", "35-39" ,"40-44", "45-49",
        "50-54" ,"55-59" ,"60-64" ,"65-69", "70-74", 
        "75-79" ,"80-84", "85-89" ,"90-94", "95-99", "100+")


gg_color_hue <- function(n) {
  hues = seq(15, 375, length=n+1)
  hcl(h=hues, l=65, c=100)[1:n]
}

###### SERVER #####
shinyServer(function(input, output,session) {  
  options(shiny.sanitize.errors = FALSE)
  
  ###### HOUSEHOLDS: WORLD MAP1 LATEST AVAILABLE DATA #####
  
  output$mymap <- renderLeaflet({
    

    
    D2000A <- reactive({HOGARES[HOGARES$indicator %in% input$indicator & 
                               HOGARES$Areatype=='Total', ]  })
    
    
    #D2000 <-HOGARES[HOGARES$indicator=="H2" & HOGARES$Areatype=='Total', ]
    D2000<-D2000A()
    DFSHPMUNUN<-SHPWORLDUN@data
   
     observe({
      if ("H1" %in% input$indicator) {
        selected_choices <- "H1"
        updateSelectInput(session, "indicator", selected = selected_choices)
      }
    })
    
    ss1 <- split.data.frame(D2000,D2000$Country)
    
    
    dfList1 <- lapply(ss1, function(df) {
      df<- df[order(-df$Year),]
      df <-df[c(1:1), ]
      df
    })
    
    
    df <- do.call("rbind", dfList1) 
    
    colnames(df)[1]<-"NAME"
   
    AVER <-merge(DFSHPMUNUN,df, by="NAME", all.x = TRUE )

   
    AVER <- AVER[order(order(SHPWORLDUN@data$NAME)), ]
    SHPWORLDUN@data<-cbind(SHPWORLDUN@data,AVER[,c(10:21)] )

    state_popup <- paste0("<br><strong>Country: </strong>", 
                          SHPWORLDUN$NAME, 
                          "<br><strong>Year of data: </strong>", 
                          SHPWORLDUN$Year, 
                          "<br><strong>Data source: </strong>", 
                          SHPWORLDUN$DataSource, 
                          "<br><strong>Value: </strong>", 
                          round(SHPWORLDUN$Value2,2))
    

    
    bins<-if(input$indicator=="H1"){c(2,2.5,3,3.5,4,4.5,5,5.5,6,10) #LISTO
    }else {if(input$indicator=="H2"|input$indicator=="H3"|input$indicator=="H4"|
              input$indicator=="H5"|input$indicator=="H6"|input$indicator=="H7")
           {c(0,5,10,15,20,25,30,35,100)  #LISTO
    }else {if(input$indicator=="H8"){c(0,10,20,30,40,50,60,70)  #LISTO
    }else {if(input$indicator=="H9"){c(0,20,30,40,50,60,70,80,90)#LISTO
    }else {if(input$indicator=="H10"){c(0,20,30,40,50,60)#LISTO
    }else {if(input$indicator=="H12"){c(0,10,20,30,40,50)#LISTO
    }else {if(input$indicator=="H13"){c(0,5,15,30,45,60) #LISTO
    }else {if(input$indicator=="H14"){c(0,30,40,50,60,70,80,90,100)
    }else {if(input$indicator=="H15"){c(0,5,10,15,20,25,30,35,50)#LISTO
    }else {if(input$indicator=="H16"){c(0,5,10,15,20,25,30,35,50)#LISTO
    }else {if(input$indicator=="H17"){c(0,10,20,30,40,50,60,70)  #LISTO
    }else {if(input$indicator=="H18"){c(0,5,10,15,20,25,30,35,40)#LISTO
    }else {c(0,1.5,2,2.5,3,3.5,4,4.5,5)}}}}}}}}}}}}

    
    ##### color palettes for each group of variables #####    
    palette <-ifelse(input$indicator=="H1","YlOrRd",
              ifelse(input$indicator=="H2"|input$indicator=="H3"|input$indicator=="H4"|
                       input$indicator=="H5"|input$indicator=="H6"|input$indicator=="H7","YlGnBu",
              ifelse(input$indicator=="H8"|input$indicator=="H17","RdPu",
              ifelse(input$indicator=="H9"|input$indicator=="H10"|input$indicator=="H12"|
                     input$indicator=="H13"|input$indicator=="H14"|input$indicator=="H15"|
                     input$indicator=="H16"|input$indicator=="H18","BuPu","PuRd"))))
    pal <- colorBin(palette, domain = SHPWORLDUN$Value, bins = bins)
    
    ##### LEGEND TITLES  ##### 
    TITLE <-ifelse(input$indicator=="H1", "Household mean size (persons)", 
            ifelse(input$indicator=="H2", "1 person HH/all HH (%)",
            ifelse(input$indicator=="H3", "2 person HH/all HH (%)",
            ifelse(input$indicator=="H4", "3 person HH/all HH (%)",
            ifelse(input$indicator=="H5", "4 person HH/all HH (%)",
            ifelse(input$indicator=="H6", "5 person HH/all HH (%)",
            ifelse(input$indicator=="H7", "6 or more person HH/all HH (%)",
            ifelse(input$indicator=="H8", "HH with a female head (%)",
            ifelse(input$indicator=="H9", "HH with children under 15 (%)",   
            ifelse(input$indicator=="H10", "HH with person over 60 (%)",
            ifelse(input$indicator=="H11", "Average number of children for HH with children under 15",
            ifelse(input$indicator=="H12", "HH with children under 15 only with mother (%)",
            ifelse(input$indicator=="H13", "HH with children under 15 only with father (%)",
            ifelse(input$indicator=="H14", "HH with children under 15  with both parents (%)",
            ifelse(input$indicator=="H15", "HH with children under 15  with none parents (%)",
            ifelse(input$indicator=="H16", "Head 60+ (%)",
            ifelse(input$indicator=="H17", "Single mother/Female Head (%)",
            ifelse(input$indicator=="H18", "HH with children under 15 & person(s) over 60 (%)",0))))))))))))))))))
    
    leaflet(SHPWORLDUN,leafletOptions(minZoom = 5, maxZoom = 10)) %>%
      #addProviderTiles("CartoDB.Positron", options=tileOptions(maxZoom=4, minZoom=2))%>%
      addPolygons(fillColor = ~pal(Value2), 
                  fillOpacity = 0.8, 
                  color = "#BDBDC3", 
                  weight = 1,
                  popup = state_popup,
                  highlightOptions = highlightOptions(color = "white", weight = 2.5,bringToFront = TRUE))%>%
      addLegend("bottomright", 
                pal = pal, 
                values = ~Value2,
                title = TITLE,
                labFormat = labelFormat(suffix = ""),
                opacity = 1)
    
    
   
   
  })
  observe({
    
    
    leafletProxy("mymap", data = df)
  
  })
  
  ###### HOUSEHOLDS: WORLD MAP2 LATEST CENSUS ROUNDS #####
  
  output$mymap1 <- renderLeaflet({
    D2000A <- reactive({HOGARES[HOGARES$indicator %in% input$indicator2 & 
                                  HOGARES$Areatype=='Total'&
                                  HOGARES$DECADE %in% input$censusround, ]  })
    
    
    #D2000 <-HOGARES[HOGARES$indicator=="H2" & HOGARES$Areatype=='Total', ]
    D2000<-D2000A()
    DFSHPMUNUN<-SHPWORLDUN@data
    
    observe({
      if ("H1" %in% input$indicator2) {
        selected_choices <- "H1"
        updateSelectInput(session, "indicator2", selected = selected_choices)
      }
    })
    
    ss1 <- split.data.frame(D2000,D2000$Country)
    
    dfList1 <- lapply(ss1, function(df) {
      df<- df[order(-df$Year),]
      df <-df[c(1:1), ]
      df
    })
    
    df <- do.call("rbind", dfList1) 
    
    colnames(df)[1]<-"NAME"
    
    AVER <-merge(DFSHPMUNUN,df, by="NAME", all.x = TRUE )

    AVER <- AVER[order(order(SHPWORLDUN@data$NAME)), ]
    SHPWORLDUN@data<-cbind(SHPWORLDUN@data,AVER[,c(10:21)] )
    
    state_popup <- paste0("<br><strong>Country: </strong>", 
                          SHPWORLDUN$NAME, 
                          "<br><strong>Year of data: </strong>", 
                          SHPWORLDUN$Year, 
                          "<br><strong>Data source: </strong>", 
                          SHPWORLDUN$DataSource, 
                          "<br><strong>Value: </strong>", 
                          round(SHPWORLDUN$Value2,2))
    
    
    
    bins<-if(input$indicator2=="H1"){c(2,2.5,3,3.5,4,4.5,5,5.5,6,10)
    }else {if(input$indicator2=="H2"|input$indicator2=="H3"|input$indicator2=="H4"|
              input$indicator2=="H5"|input$indicator2=="H6"|input$indicator2=="H7")
    {c(0,5,10,15,20,25,30,35,100)
    }else {if(input$indicator2=="H8"){c(0,10,20,30,40,50,60,70)
    }else {if(input$indicator2=="H9"){c(0,20,30,40,50,60,70,80,90)
    }else {if(input$indicator2=="H10"){c(0,20,30,40,50,60)
    }else {if(input$indicator2=="H12"){c(0,10,20,30,40,50)
    }else {if(input$indicator2=="H13"){c(0,5,15,30,45,60)
    }else {if(input$indicator2=="H14"){c(0,30,40,50,60,70,80,90,100)
    }else {if(input$indicator2=="H15"){c(0,5,10,15,20,25,30,35,50)
    }else {if(input$indicator2=="H16"){c(0,5,10,15,20,25,30,35,50)
    }else {if(input$indicator2=="H17"){c(0,10,20,30,40,50,60,70)
    }else {if(input$indicator2=="H18"){c(0,5,10,15,20,25,30,35,40)
    }else {c(0,1.5,2,2.5,3,3.5,4,4.5,5)}}}}}}}}}}}}
    
    
    ##### color palettes for each group of variables #####    
    palette <-ifelse(input$indicator2=="H1","YlOrRd",
                     ifelse(input$indicator2=="H2"|input$indicator2=="H3"|input$indicator2=="H4"|
                              input$indicator2=="H5"|input$indicator2=="H6"|input$indicator2=="H7","YlGnBu",
                            ifelse(input$indicator2=="H8"|input$indicator2=="H17","RdPu",
                                   ifelse(input$indicator2=="H9"|input$indicator2=="H10"|input$indicator2=="H12"|
                                            input$indicator2=="H13"|input$indicator2=="H14"|input$indicator2=="H15"|
                                            input$indicator2=="H16"|input$indicator2=="H18","BuPu","PuRd"))))
    pal <- colorBin(palette, domain = SHPWORLDUN$Value, bins = bins)
    
    ##### LEGEND TITLES  ##### 
    TITLE <-ifelse(input$indicator2=="H1", "Household mean size (persons)", 
                   ifelse(input$indicator2=="H2", "1 person HH/all HH (%)",
                          ifelse(input$indicator2=="H3", "2 person HH/all HH (%)",
                                 ifelse(input$indicator2=="H4", "3 person HH/all HH (%)",
                                        ifelse(input$indicator2=="H5", "4 person HH/all HH (%)",
                                               ifelse(input$indicator2=="H6", "5 person HH/all HH (%)",
                                                      ifelse(input$indicator2=="H7", "6 or more person HH/all HH (%)",
                                                             ifelse(input$indicator2=="H8", "HH with a female head (%)",
                                                                    ifelse(input$indicator2=="H9", "HH with children under 15 (%)",   
                                                                           ifelse(input$indicator2=="H10", "HH with person over 60 (%)",
                                                                                  ifelse(input$indicator2=="H11", "Average number of children for HH with children under 15",
                                                                                         ifelse(input$indicator2=="H12", "HH with children under 15 only with mother (%)",
                                                                                                ifelse(input$indicator2=="H13", "HH with children under 15 only with father (%)",
                                                                                                       ifelse(input$indicator2=="H14", "HH with children under 15  with both parents (%)",
                                                                                                              ifelse(input$indicator2=="H15", "HH with children under 15  with none parents (%)",
                                                                                                                     ifelse(input$indicator2=="H16", "Head 60+ (%)",
                                                                                                                            ifelse(input$indicator2=="H17", "Single mother/Female Head (%)",
                                                                                                                                   ifelse(input$indicator2=="H18", "HH with children under 15 & person(s) over 60 (%)",0))))))))))))))))))
    
    leaflet(SHPWORLDUN) %>%
      #addProviderTiles("CartoDB.Positron", options=tileOptions(maxZoom=4, minZoom=2))%>%
      addPolygons(fillColor = ~pal(Value2), 
                  fillOpacity = 0.8, 
                  color = "#BDBDC3", 
                  weight = 1,
                  popup = state_popup,
                  highlightOptions = highlightOptions(color = "white", weight = 2.5,bringToFront = TRUE))%>%
      addLegend("bottomright", pal = pal, values = ~Value2,
                title = TITLE,
                labFormat = labelFormat(suffix = ""),
                opacity = 1)
    
    
    
    
  })
  observe({
    
    
    leafletProxy("mymap1", data = df)
    
  })
  
  
  ###### INDIVIDUALS WORLD MAP3 LATEST AVAILABLE DATA  #########
  
  output$mymapIND <- renderLeaflet({
    D2000A <- reactive({INDI[INDI$Variable %in% input$variable & 
                             INDI$grup_edat %in% input$ageg &
                             INDI$sex %in% input$sex &
                             INDI$Areatype=='Total', ]  })
    
    #D2000A <- reactive({INDI[INDI$Variable == "P1" & 
     #                          INDI$grup_edat %in% input$ageg &
      #                         INDI$sex %in% input$sex &
       #                        INDI$Areatype=='Total', ]  })
    #D2000 <-HOGARES[HOGARES$indicator=="H2" & HOGARES$Areatype=='Total', ]
    D2000<-D2000A()
    DFSHPMUNUN<-SHPWORLDUN@data
    
    observe({
      if ("P1" %in% input$variable) {
        selected_choices <- "P1"
        updateSelectInput(session, "variable", selected = selected_choices)
      }
    })
    
    ss1 <- split.data.frame(D2000,D2000$Country)
    
    
    dfList1 <- lapply(ss1, function(df) {
      df<- df[order(-df$Year),]
      df <-df[c(1:1), ]
      df
    })
    
    
    df <- do.call("rbind", dfList1) 
    
    colnames(df)[2]<-"NAME"
    
    AVER <-merge(DFSHPMUNUN,df, by="NAME", all.x = TRUE )
    
    
    AVER <- AVER[order(order(SHPWORLDUN@data$NAME)), ]
    SHPWORLDUN@data<-cbind(SHPWORLDUN@data,AVER[,c(10:20)] )
    
    state_popup <- paste0("<br><strong>Country: </strong>", 
                          SHPWORLDUN$NAME, 
                          "<br><strong>Year of data: </strong>", 
                          SHPWORLDUN$Year, 
                          "<br><strong>Data source: </strong>", 
                          SHPWORLDUN$DataSource, 
                          "<br><strong>Value: </strong>", 
                          round(SHPWORLDUN$Value2,2))
    
    
    ##### color palettes for each group of variables #####    
    palette <-ifelse(input$indicator=="H1","YlOrRd",
                     ifelse(input$indicator=="H2"|input$indicator=="H3"|input$indicator=="H4"|
                              input$indicator=="H5"|input$indicator=="H6"|input$indicator=="H7","YlGnBu",
                            ifelse(input$indicator=="H8"|input$indicator=="H17","RdPu",
                                   ifelse(input$indicator=="H9"|input$indicator=="H10"|input$indicator=="H12"|
                                            input$indicator=="H13"|input$indicator=="H14"|input$indicator=="H15"|
                                            input$indicator=="H16"|input$indicator=="H18","BuPu","PuRd"))))
   
    
     #pal <- colorBin(palette, domain = SHPWORLDUN$Value, bins = bins)
     pal <- colorNumeric("YlOrRd", domain = c(SHPWORLDUN$Value2))
    ##### LEGEND TITLES  ##### 
     
     
   
    TITLE <-ifelse(input$variable=="P1", "Living alone", 
                   ifelse(input$variable=="P2", "Living with father",
                          ifelse(input$variable=="P3", "Living with mother",
                                 ifelse(input$variable=="P4", "Living with spouse",
                                        ifelse(input$variable=="P5", "Living with children",
                                               ifelse(input$variable=="P6", "Living with others",
                                                      ifelse(input$variable=="P11", "Only with a partner",
                                                      ifelse(input$variable=="P12", "With partner and children",
                                                      ifelse(input$variable=="P13", "Without partner and with children",
                                                      ifelse(input$variable=="P14", "Only with father)",
                                                      ifelse(input$variable=="P15", "Only with mother",
                                                      ifelse(input$variable=="P16", "With both parents",0))))))))))))
    
    leaflet(SHPWORLDUN) %>%
      #addProviderTiles("CartoDB.Positron", options=tileOptions(maxZoom=4, minZoom=2))%>%
      addPolygons(fillColor = ~pal(Value2), 
                  fillOpacity = 0.8, 
                  color = "#BDBDC3", 
                  weight = 1,
                  popup = state_popup,
                  highlightOptions = highlightOptions(color = "white", weight = 2.5,bringToFront = TRUE))%>%
      addLegend("bottomright", pal = pal, values = ~Value2,
                title = TITLE,
                labFormat = labelFormat(suffix = ""),
                opacity = 1)
    
    
    
    
  })
  observe({
    
    
    leafletProxy("mymapIND", data = df)
    
  })
  
  ###### INDIVIDUALS WORLD MAP4 CENSUS ROUNDS #########
  
  output$mymapIND1 <- renderLeaflet({
    D2000A <- reactive({INDI[INDI$Variable %in% input$variable2 & 
                               INDI$grup_edat %in% input$ageg2 &
                               INDI$DECADE %in% input$censusround2 &
                               INDI$sex %in% input$sex2 &
                               INDI$Areatype=='Total', ]  })
    
    
    #D2000 <-HOGARES[HOGARES$indicator=="H2" & HOGARES$Areatype=='Total', ]
    D2000<-D2000A()
    DFSHPMUNUN<-SHPWORLDUN@data
    
    observe({
      if ("P1" %in% input$variable2) {
        selected_choices <- "P1"
        updateSelectInput(session, "variable2", selected = selected_choices)
      }
    })
    
    ss1 <- split.data.frame(D2000,D2000$Country)
    
    
    dfList1 <- lapply(ss1, function(df) {
      df<- df[order(-df$Year),]
      df <-df[c(1:1), ]
      df
    })
    
    
    df <- do.call("rbind", dfList1) 
    
    colnames(df)[2]<-"NAME"
    
    AVER <-merge(DFSHPMUNUN,df, by="NAME", all.x = TRUE )
    
    
    AVER <- AVER[order(order(SHPWORLDUN@data$NAME)), ]
    SHPWORLDUN@data<-cbind(SHPWORLDUN@data,AVER[,c(10:21)] )
    
    state_popup <- paste0("<br><strong>Country: </strong>", 
                          SHPWORLDUN$NAME, 
                          "<br><strong>Year of data: </strong>", 
                          SHPWORLDUN$Year, 
                          "<br><strong>Data source: </strong>", 
                          SHPWORLDUN$DataSource, 
                          "<br><strong>Value: </strong>", 
                          round(SHPWORLDUN$Value2,2))
    
    
    ##### color palettes for each group of variables #####    
    palette <-ifelse(input$indicator=="H1","YlOrRd",
                     ifelse(input$indicator=="H2"|input$indicator=="H3"|input$indicator=="H4"|
                              input$indicator=="H5"|input$indicator=="H6"|input$indicator=="H7","YlGnBu",
                            ifelse(input$indicator=="H8"|input$indicator=="H17","RdPu",
                                   ifelse(input$indicator=="H9"|input$indicator=="H10"|input$indicator=="H12"|
                                            input$indicator=="H13"|input$indicator=="H14"|input$indicator=="H15"|
                                            input$indicator=="H16"|input$indicator=="H18","BuPu","PuRd"))))
    
    
    #pal <- colorBin(palette, domain = SHPWORLDUN$Value, bins = bins)
    pal <- colorNumeric("YlOrRd", domain = c(SHPWORLDUN$Value2))
    ##### LEGEND TITLES  ##### 
    
    
    
    TITLE <-ifelse(input$variable=="P1", "Living alone", 
                   ifelse(input$variable=="P2", "Living with father",
                          ifelse(input$variable=="P3", "Living with mother",
                                 ifelse(input$variable=="P4", "Living with spouse",
                                        ifelse(input$variable=="P5", "Living with children",
                                               ifelse(input$variable=="P6", "Living with others",
                                                      ifelse(input$variable=="P11", "Only with a partner",
                                                             ifelse(input$variable=="P12", "With partner and children",
                                                                    ifelse(input$variable=="P13", "Without partner and with children",
                                                                           ifelse(input$variable=="P14", "Only with father)",
                                                                                  ifelse(input$variable=="P15", "Only with mother",
                                                                                         ifelse(input$variable=="P16", "With both parents",0))))))))))))
    
    leaflet(SHPWORLDUN) %>%
      #addProviderTiles("CartoDB.Positron", options=tileOptions(maxZoom=4, minZoom=2))%>%
      addPolygons(fillColor = ~pal(Value2), 
                  fillOpacity = 0.8, 
                  color = "#BDBDC3", 
                  weight = 1,
                  popup = state_popup,
                  highlightOptions = highlightOptions(color = "white", weight = 2.5,bringToFront = TRUE))%>%
      addLegend("bottomright", pal = pal, values = ~Value2,
                title = TITLE,
                labFormat = labelFormat(suffix = ""),
                opacity = 1)
    
    
    
    
  })
  observe({
    
    
    leafletProxy("mymapIND1", data = df)
    
  })
  ###### DOWNLOAD IMAGE OF MAP 1 #####
  
  output$MAP_IMAGE1 <- downloadHandler(
    filename = function() {paste("Your map", paste("by CED", '.png', sep=''), sep=" ")}, 
    content = function(file) {
      png(file,width = 1150, height = 500)
      
      D2000A <- reactive({HOGARES[HOGARES$indicator %in% input$indicator & 
                                    HOGARES$Areatype=='Total', ]  })
      
      
     # D2000 <-HOGARES[HOGARES$indicator=="H1" & HOGARES$Areatype=='Total', ]
      D2000<-D2000A()
      DFSHPMUNUN<-SHPWORLDUN@data
      
      observe({
        if ("H1" %in% input$indicator) {
          selected_choices <- "H1"
          updateSelectInput(session, "indicator", selected = selected_choices)
        }
      })
      
      ss1 <- split.data.frame(D2000,D2000$Country)
      
      
      dfList1 <- lapply(ss1, function(df) {
        df<- df[order(-df$Year),]
        df <-df[c(1:1), ]
        df
      })
      
      
      df <- do.call("rbind", dfList1) 
      
      colnames(df)[1]<-"NAME"
      
      AVER <-merge(DFSHPMUNUN,df, by="NAME", all.x = TRUE )
      
      
      AVER <- AVER[order(order(SHPWORLDUN@data$NAME)), ]
      SHPWORLDUN@data<-cbind(SHPWORLDUN@data,AVER[,c(10:23)] )
      

        
        colorsPaleta<-if(input$indicator=="H1"){c(brewer.pal(length(unique(SHPWORLDUN@data$CATH1)), "YlOrRd"),"#848484") #LISTO
        }else {if(input$indicator=="H2"|input$indicator=="H3"|input$indicator=="H4"|
                  input$indicator=="H5"|input$indicator=="H6"|input$indicator=="H7")
        {c(brewer.pal(8, "YlGnBu"),"#848484")  #LISTO
        }else {if(input$indicator=="H8"){c(brewer.pal(7, "RdPu"),"#848484") #LISTO
        }else {if(input$indicator=="H9"){c(brewer.pal(8, "BuPu"),"#848484")#LISTO
        }else {if(input$indicator=="H10"){c(brewer.pal(5, "BuPu"),"#848484")#LISTO
        }else {if(input$indicator=="H12"){c(brewer.pal(5, "BuPu"),"#848484")#LISTO
        }else {if(input$indicator=="H13"){c(brewer.pal(5, "BuPu"),"#848484") #LISTO
        }else {if(input$indicator=="H14"){c(brewer.pal(8, "BuPu"),"#848484")#LISTO
        }else {if(input$indicator=="H15"){c(brewer.pal(8, "BuPu"),"#848484")#LISTO
        }else {if(input$indicator=="H16"){c(brewer.pal(8, "BuPu"),"#848484")#LISTO
        }else {if(input$indicator=="H17"){c(brewer.pal(7, "RdPu"),"#848484")  #LISTO
        }else {if(input$indicator=="H18"){c(brewer.pal(8, "BuPu"),"#848484")#LISTO
        }else {c(brewer.pal(8, "PuRd"),"#848484")}}}}}}}}}}}}  
     
                             
      SHPWORLDUN@data$CATH1[is.na(SHPWORLDUN@data$CATH1)]<-"Data not available"
     
      SHPWORLDUN@data$CATH1 <- as.factor(SHPWORLDUN@data$CATH1)
      
      
      flevels<-if(input$indicator=="H1"){c("2-2.5","2.5-3","3-3.5","3.5-4",
                                           "4-4.5","4.5-5","5-5.5","5.5-6","6+","Data not available") #LISTO
      }else {if(input$indicator=="H2"|input$indicator=="H3"|input$indicator=="H4"|
                input$indicator=="H5"|input$indicator=="H6"|input$indicator=="H7")
      {c("0-5","5-10","10-15","15-20","20-25","25-30","30-35","35+","Data not available")  #LISTO
      }else {if(input$indicator=="H8"){c("0-10","10-20","20-30","30-40","40-50","50-60","60-70","Data not available")  #LISTO
      }else {if(input$indicator=="H9"){c("0-20","20-30","30-40","40-50","50-60","60-70","70-80","80-90", "Data not available")#LISTO
      }else {if(input$indicator=="H10"){c("0-20","20-30","30-40","40-50","50-60", "Data not available")#LISTO
      }else {if(input$indicator=="H12"){c("0-10","10-20","20-30","30-40","40-50", "Data not available")#LISTO
      }else {if(input$indicator=="H13"){c("0-5","5-15","15-30","30-45","45-60", "Data not available") #LISTO
      }else {if(input$indicator=="H14"){c("0-30","30-40","40-50","50-60","60-70","70-80","80-90","90-100","Data not available")
      }else {if(input$indicator=="H15"){c("0-5","5-10","10-15","15-20","20-25","25-30","30-35","35+","Data not available" )#LISTO
      }else {if(input$indicator=="H16"){c("0-5","5-10","10-15","15-20","20-25","25-30","30-35","35+","Data not available" )#LISTO
      }else {if(input$indicator=="H17"){c("0-10","10-20","20-30","30-40","40-50","50-60","60-70","Data not available")  #LISTO
      }else {if(input$indicator=="H18"){c("0-5","5-10","10-15","15-20","20-25","25-30","30-35","35+","Data not available")#LISTO
      }else {c("0-1.5","1.5-2","2-2.5","2.5-3","3-3.5","3.5-4","4-4.5","4.5-5","Data not available")}}}}}}}}}}}}
      
      
      SHPWORLDUN@data$CATH1  <- factor(SHPWORLDUN@data$CATH1,
                               levels = c(flevels))
      
      names(colorsPaleta) <- levels( SHPWORLDUN@data$CATH1)
      
    
      
      CATALONIA2 <- fortify(SHPWORLDUN, region = "NAME")
      CATALONIA2 <- merge(CATALONIA2, SHPWORLDUN@data, by.x = "id", by.y = "NAME")
      
      
      
      ##### LEGEND TITLES  ##### 
      TITLE <-ifelse(input$indicator=="H1", "Household mean size (persons)", 
                     ifelse(input$indicator=="H2", "1 person HH/all HH (%)",
                            ifelse(input$indicator=="H3", "2 person HH/all HH (%)",
                                   ifelse(input$indicator=="H4", "3 person HH/all HH (%)",
                                          ifelse(input$indicator=="H5", "4 person HH/all HH (%)",
                                                 ifelse(input$indicator=="H6", "5 person HH/all HH (%)",
                                                        ifelse(input$indicator=="H7", "6 or more person HH/all HH (%)",
                                                               ifelse(input$indicator=="H8", "HH with a female head (%)",
                                                                      ifelse(input$indicator=="H9", "HH with children under 15 (%)",   
                                                                             ifelse(input$indicator=="H10", "HH with person over 60 (%)",
                                                                                    ifelse(input$indicator=="H11", "Average number of children for HH with children under 15",
                                                                                           ifelse(input$indicator=="H12", "HH with children under 15 only with mother (%)",
                                                                                                  ifelse(input$indicator=="H13", "HH with children under 15 only with father (%)",
                                                                                                         ifelse(input$indicator=="H14", "HH with children under 15  with both parents (%)",
                                                                                                                ifelse(input$indicator=="H15", "HH with children under 15  with none parents (%)",
                                                                                                                       ifelse(input$indicator=="H16", "Head 60+ (%)",
                                                                                                                              ifelse(input$indicator=="H17", "Single mother/Female Head (%)",
                                                                                                                                     ifelse(input$indicator=="H18", "HH with children under 15 & person(s) over 60 (%)",0))))))))))))))))))
      
      LTITLE <-ifelse(input$indicator=="H1", "Persons", 
                     ifelse(input$indicator=="H2", "Percentage",
                            ifelse(input$indicator=="H3", "Percentage",
                                   ifelse(input$indicator=="H4", "Percentage",
                                          ifelse(input$indicator=="H5", "Percentage",
                                                 ifelse(input$indicator=="H6", "Percentage",
                                                        ifelse(input$indicator=="H7", "Percentage",
                                                               ifelse(input$indicator=="H8", "Percentage",
                                                                      ifelse(input$indicator=="H9", "Percentage",   
                                                                             ifelse(input$indicator=="H10", "Percentage",
                                                                                    ifelse(input$indicator=="H11", "Number of children",
                                                                                           ifelse(input$indicator=="H12", "Percentage",
                                                                                                  ifelse(input$indicator=="H13", "Percentage",
                                                                                                         ifelse(input$indicator=="H14", "Percentage",
                                                                                                                ifelse(input$indicator=="H15", "Percentage",
                                                                                                                       ifelse(input$indicator=="H16", "Percentage",
                                                                                                                              ifelse(input$indicator=="H17", "Percentage",
                                                                                                                                     ifelse(input$indicator=="H18", "Percentage",0))))))))))))))))))
      
      
      
      # print(ggplot(CATALONIA2, aes(long, lat, group = group, fill = Prop)) + 
      ss<- ggplot(CATALONIA2, aes(long, lat, group = group, fill = CATH1))+
        geom_polygon(colour = "Black", size=.15, alpha=1) + 
        # coord_equal() +  
        labs(
             title= TITLE, 
             subtitle = "Latest available data",
             caption = "\nElaboration: Centre d'Estudis Demogràfics/CERCA\nData: Multiple sources (IPUMS, LFS, DHS)")+
        # ggtitle("PoblaciÃ³ estrangera (%) per secciÃ³ censal")+
        scale_fill_manual(values=colorsPaleta)+
        guides(fill=guide_legend(title=LTITLE))
      
      
      print(ss  + #geom_polygon(data=dfm, 
              #            aes(x=long, y=lat, group = group),
              #           colour="Black",size=1, fill=NA)+
              theme(plot.title = element_text(lineheight=5.6, size=15, face="bold"),
                    legend.title = element_text(angle = 0,vjust=0.5, size=12,colour="black",face="bold"),
                    legend.text = element_text(colour="black", size = 12),
                    legend.position = "right",
                    legend.background = element_rect(fill=NA),
                    legend.key.size = unit(1.5, "lines"),
                    strip.text=element_text(angle = 0,vjust=0.5, size=15,colour="black",face="bold"),
                    axis.title.x = element_blank(),
                    axis.text.x  = element_blank(),
                    axis.title.y = element_blank(),
                    axis.text.y  = element_blank(),
                    axis.ticks = element_blank(),
                    panel.grid.major=element_line(colour="#D8D8D8"),
                    panel.grid.minor=element_line(colour="#D8D8D8"),
                    plot.background = element_rect(fill = "transparent",colour = NA),
                    panel.background = element_rect(fill = "#D8D8D8",colour = NA)))
      
      
      dev.off()
    }) 
  
  
  
  
  
  
  
  
  
  
  ###### DOWNLOAD IMAGE OF MAP 2 #####
  
  output$MAP_IMAGE2 <- downloadHandler(
    filename = function() {paste("Your map", paste("by CED", '.png', sep=''), sep=" ")}, 
    content = function(file) {
      png(file,width = 1150, height = 500)
      
      D2000A <- reactive({HOGARES[HOGARES$indicator %in% input$indicator2 & 
                                    HOGARES$Areatype=='Total'&
                                    HOGARES$DECADE %in% input$censusround, ]  })
      
      
       #D2000 <-HOGARES[HOGARES$indicator=="H9" &
        #                 HOGARES$Areatype=='Total' &
         #                HOGARES$DECADE=='2010 round', ]
      D2000<-D2000A()
      DFSHPMUNUN<-SHPWORLDUN@data
      
      observe({
        if ("H1" %in% input$indicator2) {
          selected_choices <- "H1"
          updateSelectInput(session, "indicator2", selected = selected_choices)
        }
      })
      
      ss1 <- split.data.frame(D2000,D2000$Country)
      
      dfList1 <- lapply(ss1, function(df) {
        df<- df[order(-df$Year),]
        df <-df[c(1:1), ]
        df
      })
      
      df <- do.call("rbind", dfList1) 
      colnames(df)[1]<-"NAME"
      
      AVER <-merge(DFSHPMUNUN,df, by="NAME", all.x = TRUE )
      AVER <- AVER[order(order(SHPWORLDUN@data$NAME)), ]
      SHPWORLDUN@data<-cbind(SHPWORLDUN@data,AVER[,c(10:23)] )
      
      
      colorsPaleta<-if(input$indicator2=="H1"){c(brewer.pal(length(unique(SHPWORLDUN@data$CATH1)), "YlOrRd"),"#848484") #LISTO
      }else {if(input$indicator2=="H2"|input$indicator2=="H3"|input$indicator2=="H4"|
                input$indicator2=="H5"|input$indicator2=="H6"|input$indicator2=="H7")
      {c(brewer.pal(8, "YlGnBu"),"#848484")  #LISTO
      }else {if(input$indicator2=="H8"){c(brewer.pal(7, "RdPu"),"#848484") #LISTO
      }else {if(input$indicator2=="H9"){c(brewer.pal(8, "BuPu"),"#848484")#LISTO
      }else {if(input$indicator2=="H10"){c(brewer.pal(5, "BuPu"),"#848484")#LISTO
      }else {if(input$indicator2=="H12"){c(brewer.pal(5, "BuPu"),"#848484")#LISTO
      }else {if(input$indicator2=="H13"){c(brewer.pal(5, "BuPu"),"#848484") #LISTO
      }else {if(input$indicator2=="H14"){c(brewer.pal(8, "BuPu"),"#848484")#LISTO
      }else {if(input$indicator2=="H15"){c(brewer.pal(8, "BuPu"),"#848484")#LISTO
      }else {if(input$indicator2=="H16"){c(brewer.pal(8, "BuPu"),"#848484")#LISTO
      }else {if(input$indicator2=="H17"){c(brewer.pal(7, "RdPu"),"#848484")  #LISTO
      }else {if(input$indicator2=="H18"){c(brewer.pal(8, "BuPu"),"#848484")#LISTO
      }else {c(brewer.pal(8, "PuRd"),"#848484")}}}}}}}}}}}}  
     
      #colorsPaleta<-c(brewer.pal(8, "BuPu"),"#D8D8D8")
      
      SHPWORLDUN@data$CATH1[is.na(SHPWORLDUN@data$CATH1)]<-"Data not available"
      SHPWORLDUN@data$CATH1 <- as.factor(SHPWORLDUN@data$CATH1)
   
      flevels<-if(input$indicator2=="H1"){c("2-2.5","2.5-3","3-3.5","3.5-4",
                                           "4-4.5","4.5-5","5-5.5","5.5-6","6+","Data not available") #LISTO
      }else {if(input$indicator2=="H2"|input$indicator2=="H3"|input$indicator2=="H4"|
                input$indicator2=="H5"|input$indicator2=="H6"|input$indicator2=="H7")
      {c("0-5","5-10","10-15","15-20","20-25","25-30","30-35","35+","Data not available")  #LISTO
      }else {if(input$indicator2=="H8"){c("0-10","10-20","20-30","30-40","40-50","50-60","60-70","Data not available")  #LISTO
      }else {if(input$indicator2=="H9"){c("0-20","20-30","30-40","40-50","50-60","60-70","70-80","80-90", "Data not available")#LISTO
      }else {if(input$indicator2=="H10"){c("0-20","20-30","30-40","40-50","50-60", "Data not available")#LISTO
      }else {if(input$indicator2=="H12"){c("0-10","10-20","20-30","30-40","40-50", "Data not available")#LISTO
      }else {if(input$indicator2=="H13"){c("0-5","5-15","15-30","30-45","45-60", "Data not available") #LISTO
      }else {if(input$indicator2=="H14"){c("0-30","30-40","40-50","50-60","60-70","70-80","80-90","90-100","Data not available")
      }else {if(input$indicator2=="H15"){c("0-5","5-10","10-15","15-20","20-25","25-30","30-35","35+","Data not available" )#LISTO
      }else {if(input$indicator2=="H16"){c("0-5","5-10","10-15","15-20","20-25","25-30","30-35","35+","Data not available" )#LISTO
      }else {if(input$indicator2=="H17"){c("0-10","10-20","20-30","30-40","40-50","50-60","60-70","Data not available")  #LISTO
      }else {if(input$indicator2=="H18"){c("0-5","5-10","10-15","15-20","20-25","25-30","30-35","35+","Data not available")#LISTO
      }else {c("0-1.5","1.5-2","2-2.5","2.5-3","3-3.5","3.5-4","4-4.5","4.5-5","Data not available")}}}}}}}}}}}}
      
     # flevels<-c("0-20","20-30","30-40","40-50","50-60","60-70","70-80","80-90", "Data not available")
      
      SHPWORLDUN@data$CATH1  <- factor(SHPWORLDUN@data$CATH1,levels = c(flevels))
      names(colorsPaleta) <- levels( SHPWORLDUN@data$CATH1)
     
      CATALONIA2 <- fortify(SHPWORLDUN, region = "NAME")
      CATALONIA2 <- merge(CATALONIA2, SHPWORLDUN@data, by.x = "id", by.y = "NAME")
      
      ##### LEGEND TITLES  ##### 
      TITLE <-ifelse(input$indicator2=="H1", "Household mean size (persons)", 
                     ifelse(input$indicator2=="H2", "1 person HH/all HH (%)",
                            ifelse(input$indicator2=="H3", "2 person HH/all HH (%)",
                                   ifelse(input$indicator2=="H4", "3 person HH/all HH (%)",
                                          ifelse(input$indicator2=="H5", "4 person HH/all HH (%)",
                                                 ifelse(input$indicator2=="H6", "5 person HH/all HH (%)",
                                                        ifelse(input$indicator2=="H7", "6 or more person HH/all HH (%)",
                                                               ifelse(input$indicator2=="H8", "HH with a female head (%)",
                                                                      ifelse(input$indicator2=="H9", "HH with children under 15 (%)",   
                                                                             ifelse(input$indicator2=="H10", "HH with person over 60 (%)",
                                                                                    ifelse(input$indicator2=="H11", "Average number of children for HH with children under 15",
                                                                                           ifelse(input$indicator2=="H12", "HH with children under 15 only with mother (%)",
                                                                                                  ifelse(input$indicator2=="H13", "HH with children under 15 only with father (%)",
                                                                                                         ifelse(input$indicator2=="H14", "HH with children under 15  with both parents (%)",
                                                                                                                ifelse(input$indicator2=="H15", "HH with children under 15  with none parents (%)",
                                                                                                                       ifelse(input$indicator2=="H16", "Head 60+ (%)",
                                                                                                                              ifelse(input$indicator2=="H17", "Single mother/Female Head (%)",
                                                                                                                                     ifelse(input$indicator2=="H18", "HH with children under 15 & person(s) over 60 (%)",0))))))))))))))))))
     # TITLE<- "HH with children under 15 (%)"
      LTITLE <-ifelse(input$indicator2=="H1", "Persons", 
                      ifelse(input$indicator2=="H2", "Percentage",
                             ifelse(input$indicator2=="H3", "Percentage",
                                    ifelse(input$indicator2=="H4", "Percentage",
                                           ifelse(input$indicator2=="H5", "Percentage",
                                                  ifelse(input$indicator2=="H6", "Percentage",
                                                         ifelse(input$indicator2=="H7", "Percentage",
                                                                ifelse(input$indicator2=="H8", "Percentage",
                                                                       ifelse(input$indicator2=="H9", "Percentage",   
                                                                              ifelse(input$indicator2=="H10", "Percentage",
                                                                                     ifelse(input$indicator2=="H11", "Number of children",
                                                                                            ifelse(input$indicator2=="H12", "Percentage",
                                                                                                   ifelse(input$indicator2=="H13", "Percentage",
                                                                                                          ifelse(input$indicator2=="H14", "Percentage",
                                                                                                                 ifelse(input$indicator2=="H15", "Percentage",
                                                                                                                        ifelse(input$indicator2=="H16", "Percentage",
                                                                                                                               ifelse(input$indicator2=="H17", "Percentage",
                                                                                                                                      ifelse(input$indicator2=="H18", "Percentage",0))))))))))))))))))
      
      
      #LTITLE<- "Percentage"

      ss<- ggplot(CATALONIA2, aes(long, lat, group = group, fill = CATH1))+
        geom_polygon(colour = "Black", size=.15, alpha=1) + 
       
        labs(title= TITLE,   
          subtitle = paste("Census round", substr(input$censusround, 1,4), sep=" "),
          caption = "\nElaboration: Centre d'Estudis Demogràfics/CERCA\nData: Multiple sources (IPUMS, LFS, DHS)")+
        
        scale_fill_manual(values=colorsPaleta)+
        guides(fill=guide_legend(title=LTITLE))
      
      
      print(ss  + #geom_polygon(data=dfm, 
              #            aes(x=long, y=lat, group = group),
              #           colour="Black",size=1, fill=NA)+
              theme(plot.title = element_text(lineheight=5.6, size=15, face="bold"),
                    legend.title = element_text(angle = 0,vjust=0.5, size=12,colour="black",face="bold"),
                    legend.text = element_text(colour="black", size = 12),
                    legend.position = "right",
                    legend.background = element_rect(fill=NA),
                    legend.key.size = unit(1.5, "lines"),
                    strip.text=element_text(angle = 0,vjust=0.5, size=15,colour="black",face="bold"),
                    axis.title.x = element_blank(),
                    axis.text.x  = element_blank(),
                    axis.title.y = element_blank(),
                    axis.text.y  = element_blank(),
                    axis.ticks = element_blank(),
                    panel.grid.major=element_line(colour="#D8D8D8"),
                    panel.grid.minor=element_line(colour="#D8D8D8"),
                    plot.background = element_rect(fill = "transparent",colour = NA),
                    panel.background = element_rect(fill = "#D8D8D8",colour = NA)))
      
      
      dev.off()
    }) 
 
  
  
  ###### DOWNLOAD IMAGE OF MAP 3 #####
  
  output$MAP_IMAGE1IND <- downloadHandler(
    filename = function() {paste("Your map", paste("by CED", '.png', sep=''), sep=" ")}, 
    content = function(file) {
      png(file,width = 1150, height = 500)
  

     # D2000A <- reactive({INDI[INDI$Variable %in% input$variable & 
      #                           INDI$agegroup2 %in% input$ageg &
       #                          INDI$sex %in% input$sex &
        #                         INDI$Areatype=='Total', ]  })
      
      
      D2000A <- reactive({INDI[INDI$Variable %in% input$variable & 
                                 INDI$grup_edat %in% input$ageg &
                                 INDI$sex %in% input$sex &
                                 INDI$Areatype=='Total', ]  })
      
      #D2000 <- INDI[INDI$Variable =="P1" & 
      #                          INDI$agegroup2 == "20-24" &
       #                          INDI$sex == "Male" &
        #                         INDI$Areatype=='Total', ]  
      
      
      #D2000 <-HOGARES[HOGARES$indicator=="H2" & HOGARES$Areatype=='Total', ]
      D2000<-D2000A()
      DFSHPMUNUN<-SHPWORLDUN@data
      
      observe({
        if ("P1" %in% input$variable) {
          selected_choices <- "P1"
          updateSelectInput(session, "variable", selected = selected_choices)
        }
      })
      
      ss1 <- split.data.frame(D2000,D2000$Country)
      
      
      dfList1 <- lapply(ss1, function(df) {
        df<- df[order(-df$Year),]
        df <-df[c(1:1), ]
        df
      })
      
      
      df <- do.call("rbind", dfList1) 
      
      colnames(df)[2]<-"NAME"
      
      AVER <-merge(DFSHPMUNUN,df, by="NAME", all.x = TRUE )
      
      
      AVER <- AVER[order(order(SHPWORLDUN@data$NAME)), ]
      SHPWORLDUN@data<-cbind(SHPWORLDUN@data,AVER[,c(10:21)] )
      
      #colorsPaleta<-if(input$indicator2=="H1"){c(brewer.pal(length(unique(SHPWORLDUN@data$CATH1)), "YlOrRd"),"#D8D8D8") #LISTO
      #}else {if(input$indicator2=="H2"|input$indicator2=="H3"|input$indicator2=="H4"|
      #           input$indicator2=="H5"|input$indicator2=="H6"|input$indicator2=="H7")
      #  {c(brewer.pal(8, "YlGnBu"),"#D8D8D8")  #LISTO
      #  }else {if(input$indicator2=="H8"){c(brewer.pal(7, "RdPu"),"#D8D8D8") #LISTO
      #  }else {if(input$indicator2=="H9"){c(brewer.pal(8, "BuPu"),"#D8D8D8")#LISTO
      #  }else {if(input$indicator2=="H10"){c(brewer.pal(5, "BuPu"),"#D8D8D8")#LISTO
      #   }else {if(input$indicator2=="H12"){c(brewer.pal(5, "BuPu"),"#D8D8D8")#LISTO
      #   }else {if(input$indicator2=="H13"){c(brewer.pal(5, "BuPu"),"#D8D8D8") #LISTO
        #   }else {if(input$indicator2=="H14"){c(brewer.pal(8, "BuPu"),"#D8D8D8")#LISTO
      #    }else {if(input$indicator2=="H15"){c(brewer.pal(8, "BuPu"),"#D8D8D8")#LISTO
      #    }else {if(input$indicator2=="H16"){c(brewer.pal(8, "BuPu"),"#D8D8D8")#LISTO
      #    }else {if(input$indicator2=="H17"){c(brewer.pal(7, "RdPu"),"#D8D8D8")  #LISTO
      #     }else {if(input$indicator2=="H18"){c(brewer.pal(8, "BuPu"),"#D8D8D8")#LISTO
      #     }else {c(brewer.pal(8, "PuRd"),"#D8D8D8")}}}}}}}}}}}}  
      
      colorsPaleta<-c(brewer.pal(7, "YlOrRd"))
      #pal <- colorBin(palette, domain = SHPWORLDUN$Value, bins = bins)
     # pal <- colorNumeric("YlOrRd", domain = c(SHPWORLDUN$Value2))
      
      
      
      CATALONIA2 <- fortify(SHPWORLDUN, region = "NAME")
      CATALONIA2 <- merge(CATALONIA2, SHPWORLDUN@data, by.x = "id", by.y = "NAME")
      
      ##### LEGEND TITLES  ##### 
      TITLE <-ifelse(input$variable=="P1", "Living alone", 
                     ifelse(input$variable=="P2", "Living with father",
                            ifelse(input$variable=="P3", "Living with mother",
                                   ifelse(input$variable=="P4", "Living with spouse",
                                          ifelse(input$variable=="P5", "Living with children",
                                                 ifelse(input$variable=="P6", "Living with others",
                                                        ifelse(input$variable=="P11", "Only with a partner",
                                                               ifelse(input$variable=="P12", "With partner and children",
                                                                      ifelse(input$variable=="P13", "Without partner and with children",
                                                                             ifelse(input$variable=="P14", "Only with father)",
                                                                                    ifelse(input$variable=="P15", "Only with mother",
                                                                                           ifelse(input$variable=="P16", "With both parents",0))))))))))))
     
      
      
      #LTITLE<- "Percentage"
      
      ss<- ggplot(CATALONIA2, aes(long, lat, group = group, fill = Value2))+
        geom_polygon(colour = "Black", size=.15, alpha=1) + 
        
        labs(title= TITLE,   
             subtitle = paste(paste('Sex:',input$sex, sep=" "), paste('Age group:',input$ageg, sep=" "),sep=" / "), 
             caption = "\nElaboration: Centre d'Estudis Demogràfics/CERCA\nData: Multiple sources (IPUMS, LFS, DHS)")+
        
        scale_fill_gradient(low = colorsPaleta[1], high =colorsPaleta[7],name="Percentage") 
        #guides(fill=guide_legend(title=LTITLE))
      
      
      print(ss  + #geom_polygon(data=dfm, 
              #            aes(x=long, y=lat, group = group),
              #           colour="Black",size=1, fill=NA)+
              theme(plot.title = element_text(lineheight=5.6, size=15, face="bold"),
                    legend.title = element_text(angle = 0,vjust=0.5, size=12,colour="black",face="bold"),
                    legend.text = element_text(colour="black", size = 12),
                    legend.position = "right",
                    legend.background = element_rect(fill=NA),
                    legend.key.size = unit(1.5, "lines"),
                    strip.text=element_text(angle = 0,vjust=0.5, size=15,colour="black",face="bold"),
                    axis.title.x = element_blank(),
                    axis.text.x  = element_blank(),
                    axis.title.y = element_blank(),
                    axis.text.y  = element_blank(),
                    axis.ticks = element_blank(),
                    panel.grid.major=element_line(colour="#D8D8D8"),
                    panel.grid.minor=element_line(colour="#D8D8D8"),
                    plot.background = element_rect(fill = "transparent",colour = NA),
                    panel.background = element_rect(fill = "#D8D8D8",colour = NA)))
      
      
      dev.off()
    }) 
  
  
  ###### DOWNLOAD IMAGE OF MAP 4 #####
  
  output$MAP_IMAGE1IND2 <- downloadHandler(
    filename = function() {paste("Your map", paste("by CED", '.png', sep=''), sep=" ")}, 
    content = function(file) {
      png(file,width = 1150, height = 500)
      
      
     # D2000A <- reactive({INDI[INDI$Variable %in% input$variable2 & 
      #                           INDI$agegroup2 %in% input$ageg2 &
       #                          INDI$DECADE %in% input$censusround2 &
        #                         INDI$sex %in% input$sex2 &
         #                        INDI$Areatype=='Total', ]  })
      
      
      
      D2000A <- reactive({INDI[INDI$Variable %in% input$variable2 & 
                                 INDI$grup_edat %in% input$ageg2 &
                                 INDI$DECADE %in% input$censusround2 &
                                 INDI$sex %in% input$sex2 &
                                 INDI$Areatype=='Total', ]  })
      
      #D2000 <-HOGARES[HOGARES$indicator=="H2" & HOGARES$Areatype=='Total', ]
      D2000<-D2000A()
      DFSHPMUNUN<-SHPWORLDUN@data
      
      observe({
        if ("P1" %in% input$variable2) {
          selected_choices <- "P1"
          updateSelectInput(session, "variable2", selected = selected_choices)
        }
      })
      
      ss1 <- split.data.frame(D2000,D2000$Country)
      
      
      dfList1 <- lapply(ss1, function(df) {
        df<- df[order(-df$Year),]
        df <-df[c(1:1), ]
        df
      })
      
      
      df <- do.call("rbind", dfList1) 
      
      colnames(df)[2]<-"NAME"
      
      AVER <-merge(DFSHPMUNUN,df, by="NAME", all.x = TRUE )
      
      
      AVER <- AVER[order(order(SHPWORLDUN@data$NAME)), ]
      SHPWORLDUN@data<-cbind(SHPWORLDUN@data,AVER[,c(10:21)] )
      
      #colorsPaleta<-if(input$indicator2=="H1"){c(brewer.pal(length(unique(SHPWORLDUN@data$CATH1)), "YlOrRd"),"#D8D8D8") #LISTO
      #}else {if(input$indicator2=="H2"|input$indicator2=="H3"|input$indicator2=="H4"|
      #           input$indicator2=="H5"|input$indicator2=="H6"|input$indicator2=="H7")
      #  {c(brewer.pal(8, "YlGnBu"),"#D8D8D8")  #LISTO
      #  }else {if(input$indicator2=="H8"){c(brewer.pal(7, "RdPu"),"#D8D8D8") #LISTO
      #  }else {if(input$indicator2=="H9"){c(brewer.pal(8, "BuPu"),"#D8D8D8")#LISTO
      #  }else {if(input$indicator2=="H10"){c(brewer.pal(5, "BuPu"),"#D8D8D8")#LISTO
      #   }else {if(input$indicator2=="H12"){c(brewer.pal(5, "BuPu"),"#D8D8D8")#LISTO
      #   }else {if(input$indicator2=="H13"){c(brewer.pal(5, "BuPu"),"#D8D8D8") #LISTO
      #   }else {if(input$indicator2=="H14"){c(brewer.pal(8, "BuPu"),"#D8D8D8")#LISTO
      #    }else {if(input$indicator2=="H15"){c(brewer.pal(8, "BuPu"),"#D8D8D8")#LISTO
      #    }else {if(input$indicator2=="H16"){c(brewer.pal(8, "BuPu"),"#D8D8D8")#LISTO
      #    }else {if(input$indicator2=="H17"){c(brewer.pal(7, "RdPu"),"#D8D8D8")  #LISTO
      #     }else {if(input$indicator2=="H18"){c(brewer.pal(8, "BuPu"),"#D8D8D8")#LISTO
      #     }else {c(brewer.pal(8, "PuRd"),"#D8D8D8")}}}}}}}}}}}}  
      
      colorsPaleta<-c(brewer.pal(7, "YlOrRd"))
      #pal <- colorBin(palette, domain = SHPWORLDUN$Value, bins = bins)
      # pal <- colorNumeric("YlOrRd", domain = c(SHPWORLDUN$Value2))
      
      
      
      CATALONIA2 <- fortify(SHPWORLDUN, region = "NAME")
      CATALONIA2 <- merge(CATALONIA2, SHPWORLDUN@data, by.x = "id", by.y = "NAME")
      
      ##### LEGEND TITLES  ##### 
      TITLE <-ifelse(input$variable2=="P1", "Living alone", 
                     ifelse(input$variable2=="P2", "Living with father",
                            ifelse(input$variable2=="P3", "Living with mother",
                                   ifelse(input$variable2=="P4", "Living with spouse",
                                          ifelse(input$variable2=="P5", "Living with children",
                                                 ifelse(input$variable2=="P6", "Living with others",
                                                        ifelse(input$variable2=="P11", "Only with a partner",
                                                               ifelse(input$variable2=="P12", "With partner and children",
                                                                      ifelse(input$variable2=="P13", "Without partner and with children",
                                                                             ifelse(input$variable2=="P14", "Only with father)",
                                                                                    ifelse(input$variable2=="P15", "Only with mother",
                                                                                           ifelse(input$variable2=="P16", "With both parents",0))))))))))))
      
      
      
      #LTITLE<- "Percentage"
      
      ss<- ggplot(CATALONIA2, aes(long, lat, group = group, fill = Value2))+
        geom_polygon(colour = "Black", size=.15, alpha=1) + 
        
        labs(title= TITLE,   
             subtitle = paste(paste('Sex:',input$sex, sep=" "), paste('Age group:',input$ageg, sep=" "),sep=" / "), 
             caption = "\nElaboration: Centre d'Estudis Demogràfics/CERCA\nData: Multiple sources (IPUMS, LFS, DHS)")+
        
        scale_fill_gradient(low = colorsPaleta[1], high =colorsPaleta[7],name="Percentage") 
      #guides(fill=guide_legend(title=LTITLE))
      
      
      print(ss  + #geom_polygon(data=dfm, 
              #            aes(x=long, y=lat, group = group),
              #           colour="Black",size=1, fill=NA)+
              theme(plot.title = element_text(lineheight=5.6, size=15, face="bold"),
                    legend.title = element_text(angle = 0,vjust=0.5, size=12,colour="black",face="bold"),
                    legend.text = element_text(colour="black", size = 12),
                    legend.position = "right",
                    legend.background = element_rect(fill=NA),
                    legend.key.size = unit(1.5, "lines"),
                    strip.text=element_text(angle = 0,vjust=0.5, size=15,colour="black",face="bold"),
                    axis.title.x = element_blank(),
                    axis.text.x  = element_blank(),
                    axis.title.y = element_blank(),
                    axis.text.y  = element_blank(),
                    axis.ticks = element_blank(),
                    panel.grid.major=element_line(colour="#D8D8D8"),
                    panel.grid.minor=element_line(colour="#D8D8D8"),
                    plot.background = element_rect(fill = "transparent",colour = NA),
                    panel.background = element_rect(fill = "#D8D8D8",colour = NA)))
      
      
      dev.off()
    }) 
  
  
  ########### HOUSEHOLDS BAR PLOTS ######
  observe({
    
    updateSelectizeInput(session, 'DS', 
                         choices = sort(as.data.frame(unique(HOGARES[HOGARES$Country==input$country, "DataSource"]))[,1]), 
                         selected=as.data.frame(as.data.frame((HOGARES[HOGARES$Country==input$country,])) %>% 
                                                  group_by(DataSource) %>%  
                                                  summarise(secciones = n()))[1,1]) })

  output$barplotaverage <-renderHighchart({ 
    MHHZ <- HOGARES[HOGARES$Country %in% input$country & 
                    HOGARES$indicator=="H1"& 
                    HOGARES$Areatype %in% input$ambit & 
                    HOGARES$DataSource %in% input$DS, ]
    
    #MHHZ <- HOGARES[HOGARES$Country =="Spain" & 
     #                 HOGARES$indicator=="H1"& 
      #                HOGARES$Areatype=="Total" & 
       #               HOGARES$DataSource =="IPUMS", ]
  
#   POP <- as.data.frame(as.data.frame((HOGARES[HOGARES$Country=="Senegal",])) %>% 
 #                                          group_by(DataSource) %>%  
  #                                         summarise(secciones = n()))[1,1]
 
    colorsPaleta <- brewer.pal(3, "YlOrRd")[2]
    
    
    highchart() %>%
      hc_chart(type = "column") %>%
      hc_xAxis(title = list(text = ''), categories=MHHZ$Year) %>%
      hc_yAxis(title = list(text = 'Mean Household size (persons)')) %>%
      hc_add_series(data = MHHZ$Value,
                    name = unique(MHHZ$Areatype2),colorByPoint = TRUE) %>%
    hc_title(text =paste(unique(input$country)," ", "(Data Source: ",input$DS,")", sep=""),
             align = 'left') %>%
      hc_colors(c(colorsPaleta)) %>%
      hc_credits(enabled = TRUE, text = "HIGHCHARTS",href = "https://www.highcharts.com/")%>%
      hc_add_theme(hc_theme_smpl())
    
    #hchart(MHHZ, "column", hcaes(x = as.character(Year), y = round(Value,2), group = Areatype2))%>%
    #hc_yAxis(title = list(text = 'Mean Household size (persons)')) %>%
    #hc_xAxis(title = list(text = '')) %>%
    #hc_title(text =paste(unique(input$country)," ", "(Data Source: ",input$DS,")", sep=""),
     #        align = 'left') %>%
    #  hc_colors(c(colorsPaleta)) %>%
    #  hc_credits(enabled = TRUE, text = "HIGHCHARTS",href = "https://www.highcharts.com/")%>%
      #hc_add_theme(hc_theme_smpl())
    })
  
  output$barplothousehold <-renderHighchart({ 
    PH <-c("H2","H3","H4","H5","H6","H7")
    df <- HOGARES[HOGARES$Country %in% input$country & 
                      HOGARES$indicator %in% PH & 
                      HOGARES$Areatype %in% input$ambit & 
                      HOGARES$DataSource %in% input$DS, ]
    
    colorsPaleta <- brewer.pal(7, "YlGnBu")[c(2:7)]
    #df$indicator2<- as.factor( df$indicator2)
    #levels( df$indicator2)<-levels( df$indicator2)
    #df$indicator<-factor(df$indicator, levels=rev(c("H2","H3","H4","H5","H6","H7")))
    
    
    hchart(df, "column", hcaes(x = as.character(Year), y = round(Value2,2), group = indicator2))%>%
      hc_plotOptions(
        
        series = list(stacking = "normal",animation=FALSE), #borderColor= '#303030'
        bar = list(groupPadding = 0, pointPadding =  0, borderWidth = 0))%>%
      hc_yAxis(min=0,
               max=100,
               title = list(text = 'Percentage')) %>%
      hc_xAxis(title = list(text = '')) %>%
      hc_title(text =paste(unique(input$country)," ", "(Data Source: ",input$DS,")", sep=""),
               align = 'left') %>%
      hc_colors(c(colorsPaleta)) %>%
      hc_credits(enabled = TRUE, text = "HIGHCHARTS",href = "https://www.highcharts.com/")%>%
      hc_add_theme(hc_theme_smpl())
  })
  
  output$V817 <-renderHighchart({ 
    PH <-c("H8","H17")
    df <- HOGARES[HOGARES$Country %in% input$country & 
                    HOGARES$indicator %in% PH & 
                    HOGARES$Areatype %in% input$ambit & 
                    HOGARES$DataSource %in% input$DS, ]
    
  
    colorsPaleta <- brewer.pal(6, "RdPu")[c(3,4)]
    hchart(df, "column", hcaes(x = as.character(Year), y = round(Value2,2), group = indicator2))%>%
      hc_yAxis(min=0,
               max=60,
               title = list(text = 'Percentage')) %>%
      hc_xAxis(title = list(text = '')) %>%
      hc_title(text =paste(unique(input$country)," ", "(Data Source: ",input$DS,")", sep=""),
               align = 'left') %>%
      hc_colors(c(colorsPaleta)) %>%
      hc_credits(enabled = TRUE, text = "HIGHCHARTS",href = "https://www.highcharts.com/")%>%
      hc_add_theme(hc_theme_smpl())
  })
  
  output$V910 <-renderHighchart({ 
    PH <-c("H9","H10", "H18")
    df <- HOGARES[HOGARES$Country %in% input$country & 
                    HOGARES$indicator %in% PH & 
                    HOGARES$Areatype %in% input$ambit & 
                    HOGARES$DataSource %in% input$DS, ]
    
   
    colorsPaleta <- rev(brewer.pal(5, "BuPu")[c(2,3,4)])
    
    hchart(df, "column", hcaes(x = as.character(Year), y = round(Value2,2), group = indicator2))%>%
      hc_yAxis(min=0,
               max=100,
               title = list(text = 'Percentage')) %>%
      hc_xAxis(title = list(text = '')) %>%
      hc_title(text =paste(unique(input$country)," ", "(Data Source: ",input$DS,")", sep=""),
               align = 'left') %>%
      hc_colors(c(colorsPaleta)) %>%
      hc_credits(enabled = TRUE, text = "HIGHCHARTS",href = "https://www.highcharts.com/")%>%
      hc_add_theme(hc_theme_smpl())
  })
  
  output$V1215 <-renderHighchart({ 
    PH <-c("H12","H13", "H14", "H15")
    df <- HOGARES[HOGARES$Country %in% input$country & 
                    HOGARES$indicator %in% PH & 
                    HOGARES$Areatype %in% input$ambit & 
                    HOGARES$DataSource %in% input$DS, ]
 
    colorsPaleta <- rev(brewer.pal(7, "BuPu")[c(3,4,5,6)])
    
    hchart(df, "column", hcaes(x = as.character(Year), y = round(Value2,2), group = indicator2))%>%
      hc_yAxis(#min=0,
               #max=100,
               title = list(text = 'Percentage')) %>%
      hc_xAxis(title = list(text = '')) %>%
      hc_title(text =paste(unique(input$country)," ", "(Data Source: ",input$DS,")", sep=""),
               align = 'left') %>%
      hc_colors(c(colorsPaleta)) %>%
      hc_credits(enabled = TRUE, text = "HIGHCHARTS",href = "https://www.highcharts.com/")%>%
      hc_add_theme(hc_theme_smpl())
  })
  
  output$V16 <-renderHighchart({ 
    PH <-c("H16")
    df <- HOGARES[HOGARES$Country %in% input$country & 
                    HOGARES$indicator %in% PH & 
                    HOGARES$Areatype %in% input$ambit & 
                    HOGARES$DataSource %in% input$DS, ]
    

    colorsPaleta <- rev(brewer.pal(7, "BuPu")[c(5)])
    
    hchart(df, "column", hcaes(x = as.character(Year), y = round(Value2,2), group = indicator2))%>%
      hc_yAxis(#min=0,
        #max=100,
        title = list(text = 'Percentage')) %>%
      hc_xAxis(title = list(text = '')) %>%
      hc_title(text =paste(unique(input$country)," ", "(Data Source: ",input$DS,")", sep=""),
               align = 'left') %>%
      hc_colors(c(colorsPaleta)) %>%
      hc_credits(enabled = TRUE, text = "HIGHCHARTS",href = "https://www.highcharts.com/")%>%
      hc_add_theme(hc_theme_smpl())
  })
  
  output$V11 <-renderHighchart({ 
    PH <-c("H11")
    df <- HOGARES[HOGARES$Country %in% input$country & 
                    HOGARES$indicator %in% PH & 
                    HOGARES$Areatype %in% input$ambit & 
                    HOGARES$DataSource %in% input$DS, ]
    
   
    colorsPaleta <- rev(brewer.pal(7, "PuRd")[c(4)])
    
    hchart(df, "column", hcaes(x = as.character(Year), y = round(Value2,2), group = indicator2))%>%
      hc_yAxis(#min=0,
        #max=100,
        title = list(text = 'Average number of children')) %>%
      hc_xAxis(title = list(text = '')) %>%
      hc_title(text =paste(unique(input$country)," ", "(Data Source: ",input$DS,")", sep=""),
               align = 'left') %>%
      hc_colors(c(colorsPaleta)) %>%
      hc_credits(enabled = TRUE, text = "HIGHCHARTS",href = "https://www.highcharts.com/")%>%
      #hc_exporting(enabled = TRUE) %>%
      hc_add_theme(hc_theme_smpl())
  })
  
  
  ########### HOUSEHOLDS BAR PLOTS COMPARISONS #####
  
  observe({
    updateSelectizeInput(session, 'countrycom', 
                         choices = unique(HOGARES[HOGARES$REGION==input$region, "Country"]),
                         selected=ifelse(input$region=="EU", "Spain",
                                  ifelse(input$region=="AM", "Brazil",
                                  ifelse(input$region=="AS", "China",
                                  ifelse(input$region=="AF", "Morocco",
                                  ifelse(input$region=='0', "United States","999")))))
                         )
  })
  
  output$lineplotcom <-renderHighchart({ 
    MHHZ <- HOGARES[HOGARES$Country %in% input$countrycom & 
                      HOGARES$indicator%in% input$indicatorcom &
                      HOGARES$Areatype =="Total", ]
    
    #MHHZ <- HOGARES[HOGARES$Country =="Spain" & 
    #                 HOGARES$indicator=="H1"& 
    #                HOGARES$Areatype=="Total" & 
    #               HOGARES$DataSource =="IPUMS", ]
    
    #   POP <- as.data.frame(as.data.frame((HOGARES[HOGARES$Country=="Senegal",])) %>% 
    #                                          group_by(DataSource) %>%  
    #                                         summarise(secciones = n()))[1,1]
    
    
    MHHZ$Country2<-paste(MHHZ$Country, MHHZ$DataSource, sep="-")
    hchart(MHHZ, "spline", hcaes(x = Year, y = round(Value2,2), group = Country2))%>%
      hc_xAxis(categories = MHHZ$Year, title = list(text = '')) %>%
      hc_yAxis(title = list(text = ifelse(input$indicatorcom=="H1"|input$indicatorcom=="H11", "Persons", "Percentage"))) %>%
      hc_title(text =unique(MHHZ$indicator2),
               align = 'left') %>%
      hc_credits(enabled = TRUE, text = "HIGHCHARTS",href = "https://www.highcharts.com/")%>%
      hc_add_theme(hc_theme_smpl())
    
    #hchart(MHHZ, "column", hcaes(x = as.character(Year), y = round(Value,2), group = Areatype2))%>%
     # hc_yAxis(title = list(text = 'Mean Household size (persons)')) %>%
      #hc_xAxis(title = list(text = '')) %>%
      #hc_title(text =paste(unique(input$country)," ", "(Data Source: ",input$DS,")", sep=""),
      #         align = 'left') %>%
      #hc_colors(c(colorsPaleta)) %>%
      #hc_add_theme(hc_theme_smpl())
  })
  
  
  ##### DOWNLOAD BARPLOTS HOSUEHOLDS #####
  
                 ####### MEAN HH SIZE ########   
  output$IMAGEN_EVO <- downloadHandler(
    filename = function() {paste("HH_MEAN_SIZE_", paste(input$country, '.png', sep=''), sep=" ")},
    content = function(file) {
      png(file,width = 800, height = 500)
      MHHZ <- HOGARES[HOGARES$Country %in% input$country & 
                        HOGARES$indicator=="H1"& 
                        HOGARES$Areatype %in% input$ambit & 
                        HOGARES$DataSource %in% input$DS, ]
      
      MHHZ$Areatype4<-with(MHHZ, paste(Areatype, "\n","(", DataSource,")", sep=""))
      
      colorsPaleta <- brewer.pal(3, "YlOrRd")[2]
      
      print(ggplot(data=MHHZ, aes(x=factor(Year), y=Value2, fill=Areatype4)) +
              labs(title= paste(unique(input$country)," ", "(Data Source: ",input$DS,")", sep=""),
                   subtitle = "Mean Hosusehold size",
                   caption = "\nElaboration: Centre d'Estudis Demogràfics/CERCA")+
              geom_bar(stat="identity", colour="black")+
              #scale_x_continuous(breaks=seq(2000,2016, 1))+
              scale_fill_manual(values=colorsPaleta,
                                name="")+
              theme(plot.title = element_text(lineheight=5.6, vjust=1.5,size=12, face="bold"),
                    legend.title = element_blank(),
                    legend.text = element_text(colour="black", size = 10,vjust=2),
                    legend.position="right",
                    legend.background = element_rect(fill="#FfFfFf"),
                    strip.text=element_text(angle = 0,vjust=0.5, size=10,colour="black",face="bold"),
                    axis.title.x = element_blank(),
                    axis.text.x  = element_text(angle = 00, colour="black", size=10),
                    axis.title.y = element_text(colour="black",vjust=2, size=10),
                    axis.text.y  = element_text(vjust=0.5, size=10,colour="black"),
                    panel.background = element_rect(fill = "#ffffff"), 
                    panel.grid = element_line(colour="red"),
                    panel.grid.major=element_line(colour="#f2f2f2",size = .2), 
                    panel.grid.minor=element_line(colour="#f2f2f2"),
                    plot.background = element_rect(fill = "#ffffff"))+
              ylab("Mean Hosusehold size (persons)"))
      
      
      dev.off()
    })
                 ####### HH SIZE RELATIVE ########  
  output$IMAGEN_EVO2 <- downloadHandler(
    filename = function() {paste("HH_SIZE_RELATIVE_", paste(input$country, '.png', sep=''), sep=" ")},
    content = function(file) {
      png(file,width = 800, height = 500)
      PH <-c("H2","H3","H4","H5","H6","H7")
      df <- HOGARES[HOGARES$Country %in% input$country & 
                      HOGARES$indicator %in% PH & 
                      HOGARES$Areatype %in% input$ambit & 
                      HOGARES$DataSource %in% input$DS, ]
      
      #df <- HOGARES[HOGARES$Country =="Argentina" & 
      #               HOGARES$indicator %in% PH & 
      #              HOGARES$Areatype =="Total" & 
      #               HOGARES$DataSource=="IPUMS", ]
      
      #display.brewer.pal(6, "YlGnBu")
      colorsPaleta <- brewer.pal(6, "YlGnBu")
      
      print(ggplot(data=df, aes(x=factor(Year), y=Value2, fill=indicator2)) +
              labs(title= paste(unique(input$country)," ", "(Data Source: ",input$DS,")", sep=""),
                   subtitle = "Hosusehold size (relative distribution)",
                   caption = "\nElaboration: Centre d'Estudis Demogràfics/CERCA")+
              geom_bar(stat="identity", colour="black")+
              #scale_x_continuous(breaks=seq(2000,2016, 1))+
              scale_fill_manual(values=colorsPaleta,
                                name="")+
              theme(plot.title = element_text(lineheight=5.6, vjust=1.5,size=12, face="bold"),
                    legend.title = element_blank(),
                    legend.text = element_text(colour="black", size = 10,vjust=2),
                    legend.position="right",
                    legend.background = element_rect(fill="#FfFfFf"),
                    strip.text=element_text(angle = 0,vjust=0.5, size=10,colour="black",face="bold"),
                    axis.title.x = element_blank(),
                    axis.text.x  = element_text(angle = 00, colour="black", size=10),
                    axis.title.y = element_text(colour="black",vjust=2, size=10),
                    axis.text.y  = element_text(vjust=0.5, size=10,colour="black"),
                    panel.background = element_rect(fill = "#ffffff"), 
                    panel.grid = element_line(colour="red"),
                    panel.grid.major=element_line(colour="#f2f2f2",size = .2), 
                    panel.grid.minor=element_line(colour="#f2f2f2"),
                    plot.background = element_rect(fill = "#ffffff"))+
              ylab("Percentage"))
      
      
      dev.off()
    })
  
                 ####### FEMALE HEADS AND SINGLE MOTHERS ########  
  
  output$IMAGEN_EVO3 <- downloadHandler(
    filename = function() {paste("FEMALEHEAD_SINGLEMOTHERS_", paste(input$country, '.png', sep=''), sep=" ")},
    content = function(file) {
      png(file,width = 800, height = 500)
  PH <-c("H8","H17")
  df <- HOGARES[HOGARES$Country %in% input$country & 
                  HOGARES$indicator %in% PH & 
                  HOGARES$Areatype %in% input$ambit & 
                  HOGARES$DataSource %in% input$DS, ]
  
  # df <- HOGARES[HOGARES$Country =="Argentina" & 
  #               HOGARES$indicator %in% PH & 
  #             HOGARES$Areatype =="Total" & 
  #             HOGARES$DataSource=="IPUMS", ]
  
  #display.brewer.pal(5, "RdPu")
  colorsPaleta <- brewer.pal(6, "RdPu")[c(3,4)]
  
  print(ggplot(data=df, aes(x=factor(Year), y=Value2, fill=indicator2)) +
          labs(title= paste(unique(input$country)," ", "(Data Source: ",input$DS,")", sep=""),
               subtitle = "Female head and single mother",
               caption = "\nElaboration: Centre d'Estudis Demogràfics/CERCA")+
          geom_bar(stat="identity", position="dodge", colour="black")+
          ylim(0,60)+
          #scale_x_continuous(breaks=seq(2000,2016, 1))+
          scale_fill_manual(values=colorsPaleta,
                            name="")+
          theme(plot.title = element_text(lineheight=5.6, vjust=1.5,size=12, face="bold"),
                legend.title = element_blank(),
                legend.text = element_text(colour="black", size = 10,vjust=2),
                legend.position="bottom",
                legend.background = element_rect(fill="#FfFfFf"),
                strip.text=element_text(angle = 0,vjust=0.5, size=10,colour="black",face="bold"),
                axis.title.x = element_blank(),
                axis.text.x  = element_text(angle = 00, colour="black", size=10),
                axis.title.y = element_text(colour="black",vjust=2, size=10),
                axis.text.y  = element_text(vjust=0.5, size=10,colour="black"),
                panel.background = element_rect(fill = "#ffffff"), 
                panel.grid = element_line(colour="red"),
                panel.grid.major=element_line(colour="#f2f2f2",size = .2), 
                panel.grid.minor=element_line(colour="#f2f2f2"),
                plot.background = element_rect(fill = "#ffffff"))+
          ylab("Percentage"))
  
  
  dev.off()
    }) 
  
  
                 ####### HH WITH CHILDREN AND ELDERLY ########  
  
  output$IMAGEN_EVO4 <- downloadHandler(
    filename = function() {paste("HH_CHILDREN_ELDERLY_", paste(input$country, '.png', sep=''), sep=" ")},
    content = function(file) {
      png(file,width = 800, height = 500)
      PH <-c("H9","H10", "H18")
      df <- HOGARES[HOGARES$Country %in% input$country & 
                      HOGARES$indicator %in% PH & 
                      HOGARES$Areatype %in% input$ambit & 
                      HOGARES$DataSource %in% input$DS, ]
      
      # df <- HOGARES[HOGARES$Country =="Argentina" & 
      #               HOGARES$indicator %in% PH & 
      #             HOGARES$Areatype =="Total" & 
      #             HOGARES$DataSource=="IPUMS", ]
      
      
      #display.brewer.pal(5, "BuPu")
      colorsPaleta <- rev(brewer.pal(5, "BuPu")[c(2,3,4)])
      
      print(ggplot(data=df, aes(x=factor(Year), y=Value2, fill=indicator2)) +
              labs(title= paste(unique(input$country)," ", "(Data Source: ",input$DS,")", sep=""),
                   subtitle = "Households with children and elderly",
                   caption = "\nElaboration: Centre d'Estudis Demogràfics/CERCA")+
              geom_bar(stat="identity", position="dodge", colour="black")+
              ylim(0,100)+
              #scale_x_continuous(breaks=seq(2000,2016, 1))+
              scale_fill_manual(values=colorsPaleta,
                                name="")+
              theme(plot.title = element_text(lineheight=5.6, vjust=1.5,size=12, face="bold"),
                    legend.title = element_blank(),
                    legend.text = element_text(colour="black", size = 10,vjust=2),
                    legend.position="bottom",
                    legend.background = element_rect(fill="#FfFfFf"),
                    strip.text=element_text(angle = 0,vjust=0.5, size=10,colour="black",face="bold"),
                    axis.title.x = element_blank(),
                    axis.text.x  = element_text(angle = 00, colour="black", size=10),
                    axis.title.y = element_text(colour="black",vjust=2, size=10),
                    axis.text.y  = element_text(vjust=0.5, size=10,colour="black"),
                    panel.background = element_rect(fill = "#ffffff"), 
                    panel.grid = element_line(colour="red"),
                    panel.grid.major=element_line(colour="#f2f2f2",size = .2), 
                    panel.grid.minor=element_line(colour="#f2f2f2"),
                    plot.background = element_rect(fill = "#ffffff"))+
              ylab("Percentage"))
      
      dev.off()
    }) 
  
                 ####### HH WITH CHILDREN UNDER 15 ######## 
  output$IMAGEN_EVO5 <- downloadHandler(
    filename = function() {paste("HH_CHILDREN15_", paste(input$country, '.png', sep=''), sep=" ")},
    content = function(file) {
      png(file,width = 800, height = 500)
      PH <-c("H12","H13", "H14", "H15")
      df <- HOGARES[HOGARES$Country %in% input$country & 
                      HOGARES$indicator %in% PH & 
                      HOGARES$Areatype %in% input$ambit & 
                      HOGARES$DataSource %in% input$DS, ]
      
      # df <- HOGARES[HOGARES$Country =="Argentina" & 
      #               HOGARES$indicator %in% PH & 
      #             HOGARES$Areatype =="Total" & 
      #             HOGARES$DataSource=="IPUMS", ]
      
      
      # display.brewer.pal(7, "BuPu")
      colorsPaleta <- rev(brewer.pal(7, "BuPu")[c(3,4,5,6)])
      
      print(ggplot(data=df, aes(x=factor(Year), y=Value2, fill=indicator2)) +
              labs(title= paste(unique(input$country)," ", "(Data Source: ",input$DS,")", sep=""),
                   subtitle = "Households with children under 15",
                   caption = "\nElaboration: Centre d'Estudis Demogràfics/CERCA")+
              geom_bar(stat="identity", position="dodge", colour="black")+
              ylim(0,100)+
              #scale_x_continuous(breaks=seq(2000,2016, 1))+
              scale_fill_manual(values=colorsPaleta,
                                name="", 
                                label= c("Children < 15 with both parents",
                                         "Children < 15 with none parents",
                                         "Children < 15 only with father",
                                         "Children < 15 only with mother"))+
              theme(plot.title = element_text(lineheight=5.6, vjust=1.5,size=12, face="bold"),
                    legend.title = element_blank(),
                    legend.text = element_text(colour="black", size = 10,vjust=2),
                    legend.position="bottom",
                    legend.background = element_rect(fill="#FfFfFf"),
                    strip.text=element_text(angle = 0,vjust=0.5, size=10,colour="black",face="bold"),
                    axis.title.x = element_blank(),
                    axis.text.x  = element_text(angle = 00, colour="black", size=10),
                    axis.title.y = element_text(colour="black",vjust=2, size=10),
                    axis.text.y  = element_text(vjust=0.5, size=10,colour="black"),
                    panel.background = element_rect(fill = "#ffffff"), 
                    panel.grid = element_line(colour="red"),
                    panel.grid.major=element_line(colour="#f2f2f2",size = .2), 
                    panel.grid.minor=element_line(colour="#f2f2f2"),
                    plot.background = element_rect(fill = "#ffffff"))+
              ylab("Percentage"))
      dev.off()
    })
  
  
                 ####### HH head 60+ ########
  output$IMAGEN_EVO6 <- downloadHandler(
    filename = function() {paste("HH_HEAD60_", paste(input$country, '.png', sep=''), sep=" ")},
    content = function(file) {
      png(file,width = 800, height = 500)
      PH <-c("H16")
      df <- HOGARES[HOGARES$Country %in% input$country & 
                      HOGARES$indicator %in% PH & 
                      HOGARES$Areatype %in% input$ambit & 
                      HOGARES$DataSource %in% input$DS, ]
      
      # df <- HOGARES[HOGARES$Country =="Argentina" & 
      #               HOGARES$indicator %in% PH & 
      #             HOGARES$Areatype =="Total" & 
      #             HOGARES$DataSource=="IPUMS", ]
      
      
      # display.brewer.pal(7, "BuPu")
      colorsPaleta <- rev(brewer.pal(7, "BuPu")[c(5)])
      print(ggplot(data=df, aes(x=factor(Year), y=Value2, fill=indicator2)) +
              labs(title= paste(unique(input$country)," ", "(Data Source: ",input$DS,")", sep=""),
                   subtitle = "Households head 60+",
                   caption = "\nElaboration: Centre d'Estudis Demogràfics/CERCA")+
              geom_bar(stat="identity", position="dodge", colour="black")+
             # ylim(0,100)+
              #scale_x_continuous(breaks=seq(2000,2016, 1))+
              scale_fill_manual(values=colorsPaleta,
                                name="")+
              theme(plot.title = element_text(lineheight=5.6, vjust=1.5,size=12, face="bold"),
                    legend.title = element_blank(),
                    legend.text = element_text(colour="black", size = 10,vjust=2),
                    legend.position="bottom",
                    legend.background = element_rect(fill="#FfFfFf"),
                    strip.text=element_text(angle = 0,vjust=0.5, size=10,colour="black",face="bold"),
                    axis.title.x = element_blank(),
                    axis.text.x  = element_text(angle = 00, colour="black", size=10),
                    axis.title.y = element_text(colour="black",vjust=2, size=10),
                    axis.text.y  = element_text(vjust=0.5, size=10,colour="black"),
                    panel.background = element_rect(fill = "#ffffff"), 
                    panel.grid = element_line(colour="red"),
                    panel.grid.major=element_line(colour="#f2f2f2",size = .2), 
                    panel.grid.minor=element_line(colour="#f2f2f2"),
                    plot.background = element_rect(fill = "#ffffff"))+
              ylab("Percentage"))
  
      
      dev.off()
    })
  
                 ####### Average number of childres ########
  output$IMAGEN_EVO7 <- downloadHandler(
    filename = function() {paste("AVERAGE_NCHILDREN_", paste(input$country, '.png', sep=''), sep=" ")},
    content = function(file) {
      png(file,width = 800, height = 500)
      PH <-c("H11")
      df <- HOGARES[HOGARES$Country %in% input$country & 
                      HOGARES$indicator %in% PH & 
                      HOGARES$Areatype %in% input$ambit & 
                      HOGARES$DataSource %in% input$DS, ]
      
      colorsPaleta <- rev(brewer.pal(7, "PuRd")[c(4)])
      print(ggplot(data=df, aes(x=factor(Year), y=Value2, fill=indicator2)) +
              labs(title= paste(unique(input$country)," ", "(Data Source: ",input$DS,")", sep=""),
                   subtitle = "Average number of children",
                   caption = "\nElaboration: Centre d'Estudis Demogràfics/CERCA")+
              geom_bar(stat="identity", position="dodge", colour="black")+
              # ylim(0,100)+
              #scale_x_continuous(breaks=seq(2000,2016, 1))+
              scale_fill_manual(values=colorsPaleta,
                                name="")+
              theme(plot.title = element_text(lineheight=5.6, vjust=1.5,size=12, face="bold"),
                    legend.title = element_blank(),
                    legend.text = element_text(colour="black", size = 10,vjust=2),
                    legend.position="bottom",
                    legend.background = element_rect(fill="#FfFfFf"),
                    strip.text=element_text(angle = 0,vjust=0.5, size=10,colour="black",face="bold"),
                    axis.title.x = element_blank(),
                    axis.text.x  = element_text(angle = 00, colour="black", size=10),
                    axis.title.y = element_text(colour="black",vjust=2, size=10),
                    axis.text.y  = element_text(vjust=0.5, size=10,colour="black"),
                    panel.background = element_rect(fill = "#ffffff"), 
                    panel.grid = element_line(colour="red"),
                    panel.grid.major=element_line(colour="#f2f2f2",size = .2), 
                    panel.grid.minor=element_line(colour="#f2f2f2"),
                    plot.background = element_rect(fill = "#ffffff"))+
              ylab("Average number of children"))
      
      dev.off()
    })
  
  
  
  
  
  ###### HOUSEHOLDS: WORLD MAP1 LATEST AVAILABLE DATA penn#####
  
  output$mymappenn <- renderLeaflet({
    D2000A <- reactive({indicators[indicators$indicator %in% input$indicatorpenn,]  })
    
  
        
    #D2000 <-indicators[indicators$indicator=="gnu", ]
    D2000<-D2000A()
    DFSHPMUNUN1<-SHPWORLDUN@data
    
    observe({
      if ("r238_wom" %in% input$indicatorpenn) {
        selected_choices <- "r238_wom"
        updateSelectInput(session, "indicatorpenn", selected = selected_choices)
      }
    })
    
    ss1 <- split.data.frame(D2000,D2000$country)
    
    
    dfList1 <- lapply(ss1, function(df) {
      df<- df[order(-df$syear_init),]
      df <-df[c(1:1), ]
      df
    })
    
    
    df <- do.call("rbind", dfList1) 
    
    colnames(df)[2]<-"NAME"
    
    AVER <-merge(DFSHPMUNUN1,df, by="NAME", all.x = TRUE )
    
    
    AVER <- AVER[order(order(SHPWORLDUN@data$NAME)), ]
    SHPWORLDUN@data<-cbind(SHPWORLDUN@data,AVER[,c(12:14)] )
    #head(SHPWORLDUN@data)

    state_popup <- paste0("<br><strong>Country: </strong>", 
                          SHPWORLDUN$NAME, 
                          "<br><strong>Year of data: </strong>", 
                          SHPWORLDUN$syear_init, 
                          "<br><strong>Value: </strong>", 
                          round(SHPWORLDUN$value,2))
    
    
    #bins<-c(seq(0,1,.2))

    bins<-if(input$indicatorpenn=="r238_wom"){c(seq(1,8,1)) #LISTO
    }else {if(input$indicatorpenn=="smas_wom"|input$indicatorpenn=="smas_men"|input$indicatorpenn=="smam_wom"|
              input$indicatorpenn=="smam_men"|input$indicatorpenn=="smab_wom"|input$indicatorpenn=="smab_men")
    {c(10,15,20,25,30,35)  #LISTO
    }else {if(input$indicatorpenn=="nrr_wom"){c(seq(0,3.5,.5)) #LISTO
    }else {if(input$indicatorpenn=="r743a_d_wom"|input$indicatorpenn=="r743b_d_wom"|input$indicatorpenn=="r743d_d_wom")
    {c(seq(0,1,.2))
    }else {if(input$indicatorpenn=="raw_chmothfath14_hou"|input$indicatorpenn=="gnu"|input$indicatorpenn=="gtg"|input$indicatorpenn=="gcx")
    {c(seq(0,1,.2))
    }else {if(input$indicatorpenn=="marmd_wom"){c(seq(0,1,.2))#LISTO
    }else {if(input$indicatorpenn=="ema_wom"){c(seq(0,30,5)) #LISTO
    }else {if(input$indicatorpenn=="marcd_wom"){c(seq(0,.75,.25))
    }else {if(input$indicatorpenn=="eco_wom"){c(seq(0,25,5))#LISTO
    }else {if(input$indicatorpenn=="emc_wom"){c(seq(10,30,5))}}}}}}}}}}
    
    
    ##### color palettes for each group of variables #####    
    
    #palette<-"YlOrRd"
    palette <-ifelse(input$indicatorpenn=="r238_wom","YlOrRd",
              ifelse(input$indicatorpenn=="smas_wom"|input$indicatorpenn=="smas_men"|input$indicatorpenn=="smam_wom"|
                     input$indicatorpenn=="smam_men"|input$indicatorpenn=="smab_wom"|input$indicatorpenn=="smab_men","YlGnBu",
              ifelse(input$indicatorpenn=="nrr_wom","RdPu",
              ifelse(input$indicatorpenn=="r743a_d_wom"|input$indicatorpenn=="r743b_d_wom"|input$indicatorpenn=="r743d_d_wom"|
                     input$indicatorpenn=="raw_chmothfath14_hou"|input$indicatorpenn=="gnu"|input$indicatorpenn=="gtg"|
                     input$indicatorpenn=="gcx","BuPu","PuRd"))))
    
    pal <- colorBin(palette, domain = SHPWORLDUN$value, bins = bins)
    
    ##### LEGEND TITLES  ##### 
    #TITLE<-"Share of women living in a nuclear household"
    
    TITLE <-ifelse(input$indicatorpenn=="r238_wom", "Total Fertility Rate (TFR)", 
            ifelse(input$indicatorpenn=="nrr_wom", "Net Reproduction Rate (NRR)",
            ifelse(input$indicatorpenn=="smas_wom", "Singulate Mean Age at First Sex - women",
            ifelse(input$indicatorpenn=="smas_men", "Singulate Mean Age at First Sex - men",
            ifelse(input$indicatorpenn=="smam_wom", "Singulate Mean Age at First Marriage - women",
            ifelse(input$indicatorpenn=="smam_men", "Singulate Mean Age at First Marriage - men",
            ifelse(input$indicatorpenn=="smab_wom", "Singulate Mean Age at First Birth - women",
            ifelse(input$indicatorpenn=="smab_men", "Singulate Mean Age at First Birth - men",
            ifelse(input$indicatorpenn=="r743a_d_wom", "Husband decides about women's health (proportion of HH)",   
            ifelse(input$indicatorpenn=="r743b_d_wom", "Husband decides about household purchases (proportion of HH)",
            ifelse(input$indicatorpenn=="r743d_d_wom", "Husband decides about women's visits (proportion of HH)",
            ifelse(input$indicatorpenn=="raw_chmothfath14_hou", "Share of children living with both parents",
            ifelse(input$indicatorpenn=="gnu", "Share of women living in a nuclear household",
            ifelse(input$indicatorpenn=="gtg", "Share of women living in a three-generation household",
            ifelse(input$indicatorpenn=="gcx", "Share of women living in a complex household",
            ifelse(input$indicatorpenn=="marmd_wom", "Prevalence of marriage (proportion)",
            ifelse(input$indicatorpenn=="ema_wom", "Marital expectancy at age 15 (years)",
            ifelse(input$indicatorpenn=="marcd_wom", "Prevalence of cohabitation (proportion)",
            ifelse(input$indicatorpenn=="eco_wom", "Cohabitation expectancy at age 15 (years)",
            ifelse(input$indicatorpenn=="emc_wom", "Marital and cohabitation expectancy at age 15 (years)",0))))))))))))))))))))
    
    
    
    ##### LEGEND TITLES  ##### 
 
    leaflet(SHPWORLDUN) %>%
      #addProviderTiles("CartoDB.Positron", options=tileOptions(maxZoom=4, minZoom=2))%>%
      addPolygons(fillColor = ~pal(value), 
                  fillOpacity = 0.8, 
                  color = "#BDBDC3", 
                  weight = 1,
                  popup = state_popup,
                  highlightOptions = highlightOptions(color = "white", 
                                                      weight = 2.5,
                                                      bringToFront = TRUE))%>%
      addLegend("bottomright", pal = pal, values = ~value,
                title = TITLE,
                labFormat = labelFormat(suffix = ""),
                opacity = 1)
   
    
    
  })
    
    
  observe({
    
    
    leafletProxy("mymappenn", data = df)
    
  })
  
  
  ###### DOWNLOAD IMAGE OF MAP pemm #####
  
  output$MAP_IMAGE1penn <- downloadHandler(
    filename = function() {paste("Your map", paste("by CED", '.png', sep=''), sep=" ")}, 
    content = function(file) {
      png(file,width = 1150, height = 500)
      
      D2000A <- reactive({indicators[indicators$indicator %in% input$indicatorpenn,]  })
      
      
     # D2000 <-indicators[indicators$indicator=="r238_wom", ]
      D2000<-D2000A()
      DFSHPMUNUN1<-SHPWORLDUN@data
      
      observe({
        if ("r238_wom" %in% input$indicatorpenn) {
          selected_choices <- "r238_wom"
          updateSelectInput(session, "indicatorpenn", selected = selected_choices)
        }
      })
      
      ss1 <- split.data.frame(D2000,D2000$country)
      
      
      dfList1 <- lapply(ss1, function(df) {
        df<- df[order(-df$syear_init),]
        df <-df[c(1:1), ]
        df
      })
      
      
      df <- do.call("rbind", dfList1) 
      
      colnames(df)[2]<-"NAME"
      
      AVER <-merge(DFSHPMUNUN1,df, by="NAME", all.x = TRUE )
      
      
      AVER <- AVER[order(order(SHPWORLDUN@data$NAME)), ]
      SHPWORLDUN@data<-cbind(SHPWORLDUN@data,AVER[,c(12:15)] )
      #head( SHPWORLDUN@data)
      #bins<-c(seq(1,8,1))
      
      
      bins<-if(input$indicatorpenn=="r238_wom"){c(seq(1,8,1)) #LISTO
      }else {if(input$indicatorpenn=="smas_wom"|input$indicatorpenn=="smas_men"|input$indicatorpenn=="smam_wom"|
                input$indicatorpenn=="smam_men"|input$indicatorpenn=="smab_wom"|input$indicatorpenn=="smab_men")
      {c(10,15,20,25,30,35)  #LISTO
      }else {if(input$indicatorpenn=="nrr_wom"){c(seq(0,3.5,.5)) #LISTO
      }else {if(input$indicatorpenn=="r743a_d_wom"|input$indicatorpenn=="r743b_d_wom"|input$indicatorpenn=="r743d_d_wom")
      {c(seq(0,1,.2))
      }else {if(input$indicatorpenn=="raw_chmothfath14_hou"|input$indicatorpenn=="gnu"|input$indicatorpenn=="gtg"|input$indicatorpenn=="gcx")
      {c(seq(0,1,.2))
      }else {if(input$indicatorpenn=="marmd_wom"){c(seq(0,1,.2))#LISTO
      }else {if(input$indicatorpenn=="ema_wom"){c(seq(0,30,5)) #LISTO
      }else {if(input$indicatorpenn=="marcd_wom"){c(seq(0,.75,.25))
      }else {if(input$indicatorpenn=="eco_wom"){c(seq(0,25,5))#LISTO
      }else {if(input$indicatorpenn=="emc_wom"){c(seq(10,30,5))}}}}}}}}}}
      
      
      ##### color palettes for each group of variables #####    
      
     # palette<-"YlOrRd"
      palette <-ifelse(input$indicatorpenn=="r238_wom","YlOrRd",
                       ifelse(input$indicatorpenn=="smas_wom"|input$indicatorpenn=="smas_men"|input$indicatorpenn=="smam_wom"|
                                input$indicatorpenn=="smam_men"|input$indicatorpenn=="smab_wom"|input$indicatorpenn=="smab_men","YlGnBu",
                              ifelse(input$indicatorpenn=="nrr_wom","RdPu",
                                     ifelse(input$indicatorpenn=="r743a_d_wom"|input$indicatorpenn=="r743b_d_wom"|input$indicatorpenn=="r743d_d_wom"|
                                              input$indicatorpenn=="raw_chmothfath14_hou"|input$indicatorpenn=="gnu"|input$indicatorpenn=="gtg"|
                                              input$indicatorpenn=="gcx","BuPu","PuRd"))))
      
      #colorsPaleta<-c(brewer.pal(7, "YlOrRd"),"#848484") 
      
      colorsPaleta<-if(input$indicatorpenn=="r238_wom"){c(brewer.pal(7, "YlOrRd"),"#848484") #LISTO
      }else {if(input$indicatorpenn=="smas_wom"|input$indicatorpenn=="smas_men"|input$indicatorpenn=="smam_wom"|
                input$indicatorpenn=="smam_men"|input$indicatorpenn=="smab_wom"|input$indicatorpenn=="smab_men")
      {c(brewer.pal(5, "YlGnBu"),"#848484")  #LISTO
      }else {if(input$indicatorpenn=="nrr_wom"){c(brewer.pal(7, "RdPu"),"#848484") #LISTO
      }else {if(input$indicatorpenn=="r743a_d_wom"){c(brewer.pal(8, "BuPu"),"#848484")#LISTO
      }else {if(input$indicatorpenn=="r743b_d_wom"){c(brewer.pal(5, "BuPu"),"#848484")#LISTO
      }else {if(input$indicatorpenn=="r743d_d_wom"){c(brewer.pal(5, "BuPu"),"#848484")#LISTO
      }else {if(input$indicatorpenn=="raw_chmothfath14_hou"){c(brewer.pal(5, "BuPu"),"#848484") #LISTO
      }else {if(input$indicatorpenn=="gnu"){c(brewer.pal(8, "BuPu"),"#848484")#LISTO
      }else {if(input$indicatorpenn=="gtg"){c(brewer.pal(8, "BuPu"),"#848484")#LISTO
      }else {if(input$indicatorpenn=="gcx"){c(brewer.pal(8, "BuPu"),"#848484")#LISTO
      }else {if(input$indicatorpenn=="marmd_wom"){c(brewer.pal(5, "PuRd"),"#848484")  #LISTO
      }else {if(input$indicatorpenn=="ema_wom"){c(brewer.pal(6, "PuRd"),"#848484")  #LISTO
      }else {if(input$indicatorpenn=="marcd_wom"){c(brewer.pal(3, "PuRd"),"#848484")  #LISTO
      }else {if(input$indicatorpenn=="eco_wom"){c(brewer.pal(5, "PuRd"),"#848484")  #LISTO
      }else {if(input$indicatorpenn=="emc_wom"){c(brewer.pal(4, "PuRd"),"#848484")  #LISTo
      }else {c(0)}}}}}}}}}}}}}}}  
      
      
      SHPWORLDUN@data$CATH1[is.na(SHPWORLDUN@data$CATH1)]<-"Data not available"
      
      SHPWORLDUN@data$CATH1 <- as.factor(SHPWORLDUN@data$CATH1)
      
      
      #flevels<-c("1-2","2-3","3-4","4-5","5-6","6-7","7-8","Data not available")
      flevels<-if(input$indicatorpenn=="r238_wom"){c("1-2","2-3","3-4","4-5",
                                                "5-6","6-7","7-8",
                                                "Data not available") #LISTO
      }else {if(input$indicatorpenn=="smas_wom"|input$indicatorpenn=="smas_men"|input$indicatorpenn=="smam_wom"|
                input$indicatorpenn=="smam_men"|input$indicatorpenn=="smab_wom"|input$indicatorpenn=="smab_men")
      {c("10-15","15-20","20-25","25-30","30-35","Data not available")  #LISTO
      }else {if(input$indicatorpenn=="nrr_wom"){c("0-0.5","0.5-1", "1-1.5","1.5-2",
                                                  "2-2.5","2.5-3","3-3.5",
                                                  "Data not available")  #LISTO
      }else {if(input$indicatorpenn=="r743a_d_wom"|
                input$indicatorpenn=="r743b_d_wom"|
                input$indicatorpenn=="r743d_d_wom"|
                input$indicatorpenn=="raw_chmothfath14_hou"|
                input$indicatorpenn=="gnu"|
                input$indicatorpenn=="gtg"|
                input$indicatorpenn=="gcx"|
                input$indicatorpenn=="marmd_wom"){c("0-0.2","0.2-0.4","0.4-0.6","0.8-1", "Data not available")#LISTO
      }else {if(input$indicatorpenn=="ema_wom"){c("0-5","5-10","10-15","15-20","20-25","25-30", "Data not available")#LISTO
      }else {if(input$indicatorpenn=="marcd_wom"){c("0-0.25","0.25-0.5","0.5-0.75","Data not available")#LISTO
      }else {if(input$indicatorpenn=="eco_wom"){c("0-5","5-10","10-15","15-20","20-25", "Data not available") #LISTO
      }else {if(input$indicatorpenn=="emc_wom"){c("10-15","15-20","20-25","25-30","Data not available")}}}}}}}}
      
      
      SHPWORLDUN@data$CATH1  <- factor(SHPWORLDUN@data$CATH1,
                                       levels = c(flevels))
      
      names(colorsPaleta) <- levels( SHPWORLDUN@data$CATH1)
      
      
      
      CATALONIA2 <- fortify(SHPWORLDUN, region = "NAME")
      CATALONIA2 <- merge(CATALONIA2, SHPWORLDUN@data, by.x = "id", by.y = "NAME")
      
      
      
      ##### LEGEND TITLES  ##### 
      #TITLE<-"Total Fertility Rate (TFR)"
      TITLE <-ifelse(input$indicatorpenn=="r238_wom", "Total Fertility Rate (TFR)", 
              ifelse(input$indicatorpenn=="nrr_wom", "Net Reproduction Rate (NRR)",
              ifelse(input$indicatorpenn=="smas_wom", "Singulate Mean Age at First Sex - women",
              ifelse(input$indicatorpenn=="smas_men", "Singulate Mean Age at First Sex - men",
              ifelse(input$indicatorpenn=="smam_wom", "Singulate Mean Age at First Marriage - women",
              ifelse(input$indicatorpenn=="smam_men", "Singulate Mean Age at First Marriage - men",
              ifelse(input$indicatorpenn=="smab_wom", "Singulate Mean Age at First Birth - women",
              ifelse(input$indicatorpenn=="smab_men", "Singulate Mean Age at First Birth - men",
              ifelse(input$indicatorpenn=="r743a_d_wom", "Husband decides about women's health (proportion of HH)",   
              ifelse(input$indicatorpenn=="r743b_d_wom", "Husband decides about household purchases (proportion of HH)",
              ifelse(input$indicatorpenn=="r743d_d_wom", "Husband decides about women's visits (proportion of HH)",
              ifelse(input$indicatorpenn=="raw_chmothfath14_hou", "Share of children living with both parents",
              ifelse(input$indicatorpenn=="gnu", "Share of women living in a nuclear household",
              ifelse(input$indicatorpenn=="gtg", "Share of women living in a three-generation household",
              ifelse(input$indicatorpenn=="gcx", "Share of women living in a complex household",
              ifelse(input$indicatorpenn=="marmd_wom", "Prevalence of marriage (proportion)",
              ifelse(input$indicatorpenn=="ema_wom", "Marital expectancy at age 15 (years)",
              ifelse(input$indicatorpenn=="marcd_wom", "Prevalence of cohabitation (proportion)",
              ifelse(input$indicatorpenn=="eco_wom", "Cohabitation expectancy at age 15 (years)",
              ifelse(input$indicatorpenn=="emc_wom", "Marital and cohabitation expectancy at age 15 (years)",      
                                  0))))))))))))))))))))
      
      #LTITLE<-"(TFR)"
      LTITLE <-ifelse(input$indicatorpenn=="r238_wom", "(TFR)", 
                      ifelse(input$indicatorpenn=="nrr_wom", "(NRR)",
                             ifelse(input$indicatorpenn=="smas_wom", "Age",
                                    ifelse(input$indicatorpenn=="smas_men", "Age",
                                           ifelse(input$indicatorpenn=="smam_wom", "Age",
                                                  ifelse(input$indicatorpenn=="smam_men", "Age",
                                                         ifelse(input$indicatorpenn=="smab_wom", "Age",
                                                                ifelse(input$indicatorpenn=="smab_men", "Age",
                                                                       ifelse(input$indicatorpenn=="r743a_d_wom", "Proportion",   
                                                                              ifelse(input$indicatorpenn=="r743b_d_wom", "Proportion",
                                                                                     ifelse(input$indicatorpenn=="r743d_d_wom", "Proportion",
                                                                                            ifelse(input$indicatorpenn=="raw_chmothfath14_hou", "Proportion",
                                                                                                   ifelse(input$indicatorpenn=="gnu", "Proportion",
                                                                                                          ifelse(input$indicatorpenn=="gtg", "Proportion",
                                                                                                                 ifelse(input$indicatorpenn=="gcx", "Proportion",
                                                                                                                        ifelse(input$indicatorpenn=="marmd_wom", "Proportion",
                                                                                                                               ifelse(input$indicatorpenn=="ema_wom", "Years",
                                                                                                                                      ifelse(input$indicatorpenn=="marcd_wom", "Proportion",
                                                                                                                                             ifelse(input$indicatorpenn=="eco_wom", "Years",
                                                                                                                                                    ifelse(input$indicatorpenn=="emc_wom", "Years",      
                                                                                                                                                           0))))))))))))))))))))
      
      
      
      # print(ggplot(CATALONIA2, aes(long, lat, group = group, fill = Prop)) + 
      ss<- ggplot(CATALONIA2, aes(long, lat, group = group, fill = CATH1))+
        geom_polygon(colour = "Black", size=.15, alpha=1) + 
        # coord_equal() +  
        labs(
          title= TITLE, 
          subtitle = "Latest available data",
          caption = "\nElaboration: Centre d'Estudis Demogràfics/CERCA\nData: Multiple sources (IPUMS, LFS, DHS)")+
        # ggtitle("PoblaciÃ³ estrangera (%) per secciÃ³ censal")+
        scale_fill_manual(values=colorsPaleta)+
        guides(fill=guide_legend(title=LTITLE))
      
      
      print(ss  + #geom_polygon(data=dfm, 
              #            aes(x=long, y=lat, group = group),
              #           colour="Black",size=1, fill=NA)+
              theme(plot.title = element_text(lineheight=5.6, size=15, face="bold"),
                    legend.title = element_text(angle = 0,vjust=0.5, size=12,colour="black",face="bold"),
                    legend.text = element_text(colour="black", size = 12),
                    legend.position = "right",
                    legend.background = element_rect(fill=NA),
                    legend.key.size = unit(1.5, "lines"),
                    strip.text=element_text(angle = 0,vjust=0.5, size=15,colour="black",face="bold"),
                    axis.title.x = element_blank(),
                    axis.text.x  = element_blank(),
                    axis.title.y = element_blank(),
                    axis.text.y  = element_blank(),
                    axis.ticks = element_blank(),
                    panel.grid.major=element_line(colour="#D8D8D8"),
                    panel.grid.minor=element_line(colour="#D8D8D8"),
                    plot.background = element_rect(fill = "transparent",colour = NA),
                    panel.background = element_rect(fill = "#D8D8D8",colour = NA)))
      
      
      dev.off()
    }) 
  
  
  
  ########### penn PLOTS ######

  output$barplotpenn <-renderHighchart({ 
    MHH <- reactive({indicators[(indicators$country %in% input$countrypenn & 
                       indicators$indicator %in% input$indicatorpenn1), ]})
    MHHZ<-MHH()
    
   # MHHZ<-MHHZ[complete.cases(MHHZ),]
    #MHHZ <- indicators[indicators$country =="Colombia" & 
     #                  indicators$indicator =="smab_men", ]
    
    #MHHZ <- HOGARES[HOGARES$Country =="Spain" & 
    #                 HOGARES$indicator=="H1"& 
    #                HOGARES$Areatype=="Total" & 
    #               HOGARES$DataSource =="IPUMS", ]
    
    #   POP <- as.data.frame(as.data.frame((HOGARES[HOGARES$Country=="Senegal",])) %>% 
    #                                          group_by(DataSource) %>%  
    #                                         summarise(secciones = n()))[1,1]
    
    colorsPaleta <- brewer.pal(3, "YlOrRd")[2]
    
    
    highchart() %>%
      hc_chart(type = "column") %>%
      hc_xAxis(title = list(text = ''), categories=MHHZ$syear_init) %>%
      hc_yAxis(title = list(text = ifelse(input$indicatorpenn1=="r238_wom", "Total Fertility Rate (TFR)", 
                                          ifelse(input$indicatorpenn1=="nrr_wom", "Net Reproduction Rate (NRR)",
                                                 ifelse(input$indicatorpenn1=="smas_wom", "Singulate Mean Age at First Sex - women",
                                                        ifelse(input$indicatorpenn1=="smas_men", "Singulate Mean Age at First Sex - men",
                                                               ifelse(input$indicatorpenn1=="smam_wom", "Singulate Mean Age at First Marriage - women",
                                                                      ifelse(input$indicatorpenn1=="smam_men", "Singulate Mean Age at First Marriage - men",
                                                                             ifelse(input$indicatorpenn1=="smab_wom", "Singulate Mean Age at First Birth - women",
                                                                                    ifelse(input$indicatorpenn1=="smab_men", "Singulate Mean Age at First Birth - men",
                                                                                           ifelse(input$indicatorpenn1=="r743a_d_wom", "Husband decides about women's health (proportion of HH)",   
                                                                                                  ifelse(input$indicatorpenn1=="r743b_d_wom", "Husband decides about household purchases (proportion of HH)",
                                                                                                         ifelse(input$indicatorpenn1=="r743d_d_wom", "Husband decides about women's visits (proportion of HH)",
                                                                                                                ifelse(input$indicatorpenn1=="raw_chmothfath14_hou", "Share of children living with both parents",
                                                                                                                       ifelse(input$indicatorpenn1=="gnu", "Share of women living in a nuclear household",
                                                                                                                              ifelse(input$indicatorpenn1=="gtg", "Share of women living in a three-generation household",
                                                                                                                                     ifelse(input$indicatorpenn1=="gcx", "Share of women living in a complex household",
                                                                                                                                            ifelse(input$indicatorpenn1=="marmd_wom", "Prevalence of marriage (proportion)",
                                                                                                                                                   ifelse(input$indicatorpenn1=="ema_wom", "Marital expectancy at age 15 (years)",
                                                                                                                                                          ifelse(input$indicatorpenn1=="marcd_wom", "Prevalence of cohabitation (proportion)",
                                                                                                                                                                 ifelse(input$indicatorpenn1=="eco_wom", "Cohabitation expectancy at age 15 (years)",
                                                                                                                                                                        ifelse(input$indicatorpenn1=="emc_wom", "Marital and cohabitation expectancy at age 15 (years)",      
                                                                                                                                                                               0)))))))))))))))))))))) %>%
      hc_add_series(data = MHHZ$value,
                    name = unique(MHHZ$indicator),colorByPoint = TRUE) %>%
      hc_title(text =paste(unique(input$countrypenn), sep=""),
               align = 'left') %>%
      hc_colors(c(colorsPaleta)) %>%
      hc_credits(enabled = TRUE, text = "HIGHCHARTS",href = "https://www.highcharts.com/")%>%
      hc_add_theme(hc_theme_smpl())
    
    #hchart(MHHZ, "column", hcaes(x = as.character(Year), y = round(Value,2), group = Areatype2))%>%
    #hc_yAxis(title = list(text = 'Mean Household size (persons)')) %>%
    #hc_xAxis(title = list(text = '')) %>%
    #hc_title(text =paste(unique(input$country)," ", "(Data Source: ",input$DS,")", sep=""),
    #        align = 'left') %>%
    #  hc_colors(c(colorsPaleta)) %>%
    #  hc_credits(enabled = TRUE, text = "HIGHCHARTS",href = "https://www.highcharts.com/")%>%
    #hc_add_theme(hc_theme_smpl())
  })
  
  
  ##### DOWNLOAD BARPLOTS PENN #####
  
  ####### INDICATORS ########   
  output$IMAGEN_penn <- downloadHandler(
    filename = function() {paste(input$indicatorpenn1, paste(input$countrypenn, '.png', sep=''), sep=" ")},
    content = function(file) {
      png(file,width = 800, height = 500)
      MHHZ <- indicators[indicators$country %in% input$countrypenn & 
                           indicators$indicator %in% input$indicatorpenn1, ]
      
      colorsPaleta <- brewer.pal(3, "YlOrRd")[2]
      
      print(ggplot(data=MHHZ, aes(x=factor(syear_init), y=value, fill=indicator)) +
              labs(title= paste(unique(input$countrypenn1), sep=""),
                   caption = "\nElaboration: Centre d'Estudis Demogràfics/CERCA")+
              geom_bar(stat="identity", colour="black")+
              #scale_x_continuous(breaks=seq(2000,2016, 1))+
              scale_fill_manual(values=colorsPaleta,
                                name="")+
              labs(title=input$countrypenn)+
              theme(plot.title = element_text(lineheight=5.6, vjust=1.5,size=20, face="bold"),
                    legend.title = element_blank(),
                    legend.text = element_text(colour="black", size = 10,vjust=2),
                    legend.position="none",
                    legend.background = element_rect(fill="#FfFfFf"),
                    strip.text=element_text(angle = 0,vjust=0.5, size=10,colour="black",face="bold"),
                    axis.title.x = element_blank(),
                    axis.text.x  = element_text(angle = 00, colour="black", size=12),
                    axis.title.y = element_text(colour="black",vjust=2, size=15),
                    axis.text.y  = element_text(vjust=0.5, size=12,colour="black"),
                    panel.background = element_rect(fill = "#ffffff"), 
                    panel.grid = element_line(colour="red"),
                    panel.grid.major=element_line(colour="#f2f2f2",size = .2), 
                    panel.grid.minor=element_line(colour="#f2f2f2"),
                    plot.background = element_rect(fill = "#ffffff"))+
              ylab(paste(ifelse(input$indicatorpenn1=="r238_wom", "Total Fertility Rate (TFR)", 
                          ifelse(input$indicatorpenn1=="nrr_wom", "Net Reproduction Rate (NRR)",
                                 ifelse(input$indicatorpenn1=="smas_wom", "Singulate Mean Age at First Sex - women",
                                        ifelse(input$indicatorpenn1=="smas_men", "Singulate Mean Age at First Sex - men",
                                               ifelse(input$indicatorpenn1=="smam_wom", "Singulate Mean Age at First Marriage - women",
                                                      ifelse(input$indicatorpenn1=="smam_men", "Singulate Mean Age at First Marriage - men",
                                                             ifelse(input$indicatorpenn1=="smab_wom", "Singulate Mean Age at First Birth - women",
                                                                    ifelse(input$indicatorpenn1=="smab_men", "Singulate Mean Age at First Birth - men",
                                                                           ifelse(input$indicatorpenn1=="r743a_d_wom", "Husband decides about women's health (proportion of HH)",   
                                                                                  ifelse(input$indicatorpenn1=="r743b_d_wom", "Husband decides about household purchases (proportion of HH)",
                                                                                         ifelse(input$indicatorpenn1=="r743d_d_wom", "Husband decides about women's visits (proportion of HH)",
                                                                                                ifelse(input$indicatorpenn1=="raw_chmothfath14_hou", "Share of children living with both parents",
                                                                                                       ifelse(input$indicatorpenn1=="gnu", "Share of women living in a nuclear household",
                                                                                                              ifelse(input$indicatorpenn1=="gtg", "Share of women living in a three-generation household",
                                                                                                                     ifelse(input$indicatorpenn1=="gcx", "Share of women living in a complex household",
                                                                                                                            ifelse(input$indicatorpenn1=="marmd_wom", "Prevalence of marriage (proportion)",
                                                                                                                                   ifelse(input$indicatorpenn1=="ema_wom", "Marital expectancy at age 15 (years)",
                                                                                                                                          ifelse(input$indicatorpenn1=="marcd_wom", "Prevalence of cohabitation (proportion)",
                                                                                                                                                 ifelse(input$indicatorpenn1=="eco_wom", "Cohabitation expectancy at age 15 (years)",
                                                                                                                                                        ifelse(input$indicatorpenn1=="emc_wom", "Marital and cohabitation expectancy at age 15 (years)",      
                                                                                                                                                               0)))))))))))))))))))),"\n")))
      
      
      dev.off()
    })
  
  
 
  
  
   ########### PYRAMIDS INDIVIDUALS ######
  

  
  observe({   
    updateSelectizeInput(session, 'YearIND', 
                         choices = sort(as.data.frame(unique(INDI[INDI$Country==input$country2, "Year"]))[,1]), 
                         selected=as.data.frame(tail(as.data.frame(INDI[INDI$Country==input$country2&INDI$Areatype=="Total",]) %>% 
                                                  group_by(Year) %>%  
                                                  summarise(secciones = n()),1)[1,1])) })
  

  observe({
    updateSelectizeInput(session, 'DS2', 
                         choices = sort(as.data.frame(unique(INDI[INDI$Country==input$country2 & INDI$Year==input$YearIND, "DataSource"]))[,1]), 
                         selected=as.data.frame(head(as.data.frame((INDI[INDI$Country==input$country2& INDI$Year==input$YearIND,])) %>% 
                                                  group_by(DataSource) %>%  
                                                  summarise(secciones = n()),1)[1,1])) })
 ##### para eliminar#### 
  output$livingalone <-renderHighchart({ 
    PH <-c("P1") 
    PIR <- INDI[INDI$Country %in% input$country2 & 
                 INDI$Variable %in% PH & 
                 INDI$Areatype %in% input$ambit2 &
                 INDI$Year %in% input$YearIND &
                 INDI$DataSource %in% input$DS2, ]
    
   
    
    
    PIR$nrel <-with(PIR, ifelse((sex=="Male"),Value2*-1,Value2))
    PIR<-as.data.frame(PIR)
    
    xaxis <- list(categories = sort(unique(PIR$agegroup)),
                  reversed = FALSE, tickInterval =1,
                  labels = list(step = 1))
    
    max<-round(max(PIR$Value2),0)
    
    color1 <-c("#00BFC4","#F8766D")
    highchart() %>%
      hc_chart(type = "bar",zoomType= 'xy') %>%
      hc_add_series(name = 'Males', data = c(PIR[PIR$sex=="Male","nrel"])) %>%
      hc_add_series(name = 'Females', data = c(PIR[PIR$sex== "Female","nrel"])) %>%
      hc_plotOptions(series = list(stacking = "normal",animation=FALSE),
                     bar = list(groupPadding = 0, pointPadding =  0, borderWidth =.25))%>%
      hc_yAxis(labels = list(formatter = JS("function(){ return Math.abs(this.value) + '%'; }")),
               tickInterval=2.5, 
               min=-max, 
               max=max) %>% 
      hc_xAxis(xaxis, rlist::list.merge(xaxis, list(opposite = TRUE, linkedTo = 0))) %>% 
    hc_colors(c(color1)) %>%
      hc_add_theme(hc_theme_smpl()) %>%
      hc_title(text =paste(unique(input$country2)," ", "(Data Source: ",input$DS2,")", sep=""),
               align = 'left') %>%
      hc_exporting(enabled = FALSE) %>%
    hc_tooltip(shared = FALSE,
               formatter = JS("function () { return '<b>' + this.series.name + '<br>' +'Age: ' + this.point.category + '</b><br/>' + 'Value: ' + Math.abs(this.point.y);}"))
  })
  
  output$livinfather <-renderHighchart({ 
    PH <-c("P2") 
    PIR <- INDI[INDI$Country %in% input$country2 & 
                  INDI$Variable %in% PH & 
                  INDI$Areatype %in% input$ambit2 &
                  INDI$Year %in% input$YearIND &
                  INDI$DataSource %in% input$DS2, ]

    PIR$nrel <-with(PIR, ifelse((sex=="Male"),Value2*-1,Value2))
    PIR<-as.data.frame(PIR)
    
    xaxis <- list(categories = sort(unique(PIR$agegroup)),
                  reversed = FALSE, tickInterval =1,
                  labels = list(step = 1))
    
    max<-round(max(PIR$Value2),0)
    
    color1 <-c("#00BFC4","#F8766D")
    highchart() %>%
      hc_chart(type = "bar",zoomType= 'xy') %>%
      hc_add_series(name = 'Males', data = c(PIR[PIR$sex=="Male","nrel"])) %>%
      hc_add_series(name = 'Females', data = c(PIR[PIR$sex== "Female","nrel"])) %>%
      hc_plotOptions(series = list(stacking = "normal",animation=FALSE),
                     bar = list(groupPadding = 0, pointPadding =  0, borderWidth =.25))%>%
      hc_yAxis(labels = list(formatter = JS("function(){ return Math.abs(this.value) + '%'; }")),
               tickInterval=2.5, 
               min=-max, 
               max=max) %>% 
      hc_xAxis(xaxis, rlist::list.merge(xaxis, list(opposite = TRUE, linkedTo = 0))) %>% 
      hc_colors(c(color1)) %>%
      hc_add_theme(hc_theme_smpl()) %>%
      hc_title(text =paste(unique(input$country2)," ", "(Data Source: ",input$DS2,")", sep=""),
               align = 'left') %>%
      hc_exporting(enabled = FALSE)%>%
      hc_tooltip(shared = FALSE,
               formatter = JS("function () { return '<b>' + this.series.name + '<br>' +'Age: ' + this.point.category + '</b><br/>' + 'Value: ' + Math.abs(this.point.y);}"))
  })
  
  output$livinmother <-renderHighchart({ 
    PH <-c("P3") 
    PIR <- INDI[INDI$Country %in% input$country2 & 
                  INDI$Variable %in% PH & 
                  INDI$Areatype %in% input$ambit2 &
                  INDI$Year %in% input$YearIND &
                  INDI$DataSource %in% input$DS2, ]
    
    PIR$nrel <-with(PIR, ifelse((sex=="Male"),Value2*-1,Value2))
    PIR<-as.data.frame(PIR)
    
    xaxis <- list(categories = sort(unique(PIR$agegroup)),
                  reversed = FALSE, tickInterval =1,
                  labels = list(step = 1))
    
    max<-round(max(PIR$Value2),0)
    
    color1 <-c("#00BFC4","#F8766D")
    highchart() %>%
      hc_chart(type = "bar",zoomType= 'xy') %>%
      hc_add_series(name = 'Males', data = c(PIR[PIR$sex=="Male","nrel"])) %>%
      hc_add_series(name = 'Females', data = c(PIR[PIR$sex== "Female","nrel"])) %>%
      hc_plotOptions(series = list(stacking = "normal",animation=FALSE),
                     bar = list(groupPadding = 0, pointPadding =  0, borderWidth =.25))%>%
      hc_yAxis(labels = list(formatter = JS("function(){ return Math.abs(this.value) + '%'; }")),
               tickInterval=2.5, 
               min=-max, 
               max=max) %>% 
      hc_xAxis(xaxis, rlist::list.merge(xaxis, list(opposite = TRUE, linkedTo = 0))) %>% 
      hc_colors(c(color1)) %>%
      hc_add_theme(hc_theme_smpl()) %>%
      hc_exporting(enabled = FALSE)%>%
      hc_title(text =paste(unique(input$country2)," ", "(Data Source: ",input$DS2,")", sep=""),
               align = 'left') %>%
      hc_tooltip(shared = FALSE,
                 formatter = JS("function () { return '<b>' + this.series.name + '<br>' +'Age: ' + this.point.category + '</b><br/>' + 'Value: ' + Math.abs(this.point.y);}"))
  })
  
  output$livinspouse <-renderHighchart({ 
    PH <-c("P4") 
    PIR <- INDI[INDI$Country %in% input$country2 & 
                  INDI$Variable %in% PH & 
                  INDI$Areatype %in% input$ambit2 &
                  INDI$Year %in% input$YearIND &
                  INDI$DataSource %in% input$DS2, ]
    
    PIR$nrel <-with(PIR, ifelse((sex=="Male"),Value2*-1,Value2))
    PIR<-as.data.frame(PIR)
    
    xaxis <- list(categories = sort(unique(PIR$agegroup)),
                  reversed = FALSE, tickInterval =1,
                  labels = list(step = 1))
    
    max<-round(max(PIR$Value2),0)
    
    color1 <-c("#00BFC4","#F8766D")
    highchart() %>%
      hc_chart(type = "bar",zoomType= 'xy') %>%
      hc_add_series(name = 'Males', data = c(PIR[PIR$sex=="Male","nrel"])) %>%
      hc_add_series(name = 'Females', data = c(PIR[PIR$sex== "Female","nrel"])) %>%
      hc_plotOptions(series = list(stacking = "normal",animation=FALSE),
                     bar = list(groupPadding = 0, pointPadding =  0, borderWidth =.25))%>%
      hc_yAxis(labels = list(formatter = JS("function(){ return Math.abs(this.value) + '%'; }")),
               tickInterval=2.5, 
               min=-max, 
               max=max) %>% 
      hc_xAxis(xaxis, rlist::list.merge(xaxis, list(opposite = TRUE, linkedTo = 0))) %>% 
      hc_colors(c(color1)) %>%
      hc_add_theme(hc_theme_smpl()) %>%
      hc_title(text =paste(unique(input$country2)," ", "(Data Source: ",input$DS2,")", sep=""),
               align = 'left') %>%
      hc_exporting(enabled = FALSE)%>%
      hc_tooltip(shared = FALSE,
                 formatter = JS("function () { return '<b>' + this.series.name + '<br>' +'Age: ' + this.point.category + '</b><br/>' + 'Value: ' + Math.abs(this.point.y);}"))
  })
  
  output$livinchildren <-renderHighchart({ 
    PH <-c("P5") 
    PIR <- INDI[INDI$Country %in% input$country2 & 
                  INDI$Variable %in% PH & 
                  INDI$Areatype %in% input$ambit2 &
                  INDI$Year %in% input$YearIND &
                  INDI$DataSource %in% input$DS2, ]
    
    PIR$nrel <-with(PIR, ifelse((sex=="Male"),Value2*-1,Value2))
    PIR<-as.data.frame(PIR)
    
    xaxis <- list(categories = sort(unique(PIR$agegroup)),
                  reversed = FALSE, tickInterval =1,
                  labels = list(step = 1))
    
    max<-round(max(PIR$Value2),0)
    
    color1 <-c("#00BFC4","#F8766D")
    highchart() %>%
      hc_chart(type = "bar",zoomType= 'xy') %>%
      hc_add_series(name = 'Males', data = c(PIR[PIR$sex=="Male","nrel"])) %>%
      hc_add_series(name = 'Females', data = c(PIR[PIR$sex== "Female","nrel"])) %>%
      hc_plotOptions(series = list(stacking = "normal",animation=FALSE),
                     bar = list(groupPadding = 0, pointPadding =  0, borderWidth =.25))%>%
      hc_yAxis(labels = list(formatter = JS("function(){ return Math.abs(this.value) + '%'; }")),
               tickInterval=2.5, 
               min=-max, 
               max=max) %>% 
      hc_xAxis(xaxis, rlist::list.merge(xaxis, list(opposite = TRUE, linkedTo = 0))) %>% 
      hc_colors(c(color1)) %>%
      hc_add_theme(hc_theme_smpl()) %>%
      hc_title(text =paste(unique(input$country2)," ", "(Data Source: ",input$DS2,")", sep=""),
               align = 'left') %>%
      hc_exporting(enabled = FALSE)%>%
      hc_tooltip(shared = FALSE,
                 formatter = JS("function () { return '<b>' + this.series.name + '<br>' +'Age: ' + this.point.category + '</b><br/>' + 'Value: ' + Math.abs(this.point.y);}"))
  })
  
  output$livinothers <-renderHighchart({ 
    PH <-c("P6") 
    PIR <- INDI[INDI$Country %in% input$country2 & 
                  INDI$Variable %in% PH & 
                  INDI$Areatype %in% input$ambit2 &
                  INDI$Year %in% input$YearIND &
                  INDI$DataSource %in% input$DS2, ]
    
    PIR$nrel <-with(PIR, ifelse((sex=="Male"),Value2*-1,Value2))
    PIR<-as.data.frame(PIR)
    
    xaxis <- list(categories = sort(unique(PIR$agegroup)),
                  reversed = FALSE, tickInterval =1,
                  labels = list(step = 1))
    
    max<-round(max(PIR$Value2),0)
    
    color1 <-c("#00BFC4","#F8766D")
    highchart() %>%
      hc_chart(type = "bar",zoomType= 'xy') %>%
      hc_add_series(name = 'Males', data = c(PIR[PIR$sex=="Male","nrel"])) %>%
      hc_add_series(name = 'Females', data = c(PIR[PIR$sex== "Female","nrel"])) %>%
      hc_plotOptions(series = list(stacking = "normal",animation=FALSE),
                     bar = list(groupPadding = 0, pointPadding =  0, borderWidth =.25))%>%
      hc_yAxis(labels = list(formatter = JS("function(){ return Math.abs(this.value) + '%'; }")),
               tickInterval=2.5, 
               min=-max, 
               max=max) %>% 
      hc_xAxis(xaxis, rlist::list.merge(xaxis, list(opposite = TRUE, linkedTo = 0))) %>% 
      hc_colors(c(color1)) %>%
      hc_add_theme(hc_theme_smpl()) %>%
      hc_title(text =paste(unique(input$country2)," ", "(Data Source: ",input$DS2,")", sep=""),
               align = 'left') %>%
      hc_exporting(enabled = FALSE)%>%
      hc_tooltip(shared = FALSE,
                 formatter = JS("function () { return '<b>' + this.series.name + '<br>' +'Age: ' + this.point.category + '</b><br/>' + 'Value: ' + Math.abs(this.point.y);}"))
  })

######## hasta qui #######  
    
  output$linesm <-renderHighchart({ 
    PH <-c("P1", "P2", "P3", "P4", "P5", "P6") 
    PIR <- INDI[INDI$Country %in% input$country2 & 
                  INDI$Variable %in% PH & 
                  INDI$Areatype %in% input$ambit2 &
                  INDI$Year %in% input$YearIND &
                  INDI$DataSource %in% input$DS2, ]
    

    PIR<-PIR[PIR$sex=="Male",]
    
    PIR$Variable2<-paste(PIR$variableN, PIR$DataSource, sep="-")
    hchart(PIR, "spline", hcaes(x = as.character(grup_edat), y = round(Value2,2), group = Variable2))%>%
      hc_xAxis(categories =as.character(PIR$grup_edat), title = list(text = ''))%>%
      hc_title(text =paste(unique(input$country2)," Males ", "(",input$DS2,")", sep=""),
               align = 'left') %>%
      hc_add_theme(hc_theme_smpl()) %>%
      hc_colors(c(gg_color_hue(6))) %>%
      hc_credits(enabled = TRUE, text = "HIGHCHARTS",href = "https://www.highcharts.com/")%>%
      hc_yAxis(min=0, max=100,title = list(text = "%"))
  })
  
  output$linesf <-renderHighchart({ 
    PH <-c("P1", "P2", "P3", "P4", "P5", "P6") 
    PIR <- INDI[INDI$Country %in% input$country2 & 
                  INDI$Variable %in% PH & 
                  INDI$Areatype %in% input$ambit2 &
                  INDI$Year %in% input$YearIND &
                  INDI$DataSource %in% input$DS2, ]
    
    PIR<-PIR[PIR$sex=="Female",]

    
    PIR$Variable2<-paste(PIR$variableN, PIR$DataSource, sep="-")
    hchart(PIR, "spline", hcaes(x = as.character(grup_edat), y = round(Value2,2), group = Variable2))%>%
      hc_xAxis(categories =as.character(PIR$grup_edat), title = list(text = ''))%>%
      hc_title(text =paste(unique(input$country2)," Females ", "(",input$DS2,")", sep=""),
               align = 'left') %>%
      hc_add_theme(hc_theme_smpl()) %>%
      hc_colors(c(gg_color_hue(6))) %>%
      hc_credits(enabled = TRUE, text = "HIGHCHARTS",href = "https://www.highcharts.com/")%>%
      hc_yAxis(min=0, max=100,title = list(text = "%"))
  })
  
 ###### DOWNLOAD IMAGES LIVING ##### 
  
          ####### LIVING ALONE #####
  output$IMAGEN_IVO <- downloadHandler(
    filename = function() { paste(input$country2,input$YearIND, '.png', sep='') },
    content = function(file) {
      png(file,width = 800, height = 700)
      PH <-c("P1") 
      PIR <- INDI[INDI$Country %in% input$country2 & 
                    INDI$Variable %in% PH & 
                    INDI$Areatype %in% input$ambit2 &
                    INDI$Year %in% input$YearIND &
                    INDI$DataSource %in% input$DS2, ]
      
      PIR$nrel <-with(PIR, ifelse((sex=="Male"),Value2*-1,Value2))
      PIR<-as.data.frame(PIR)
      
      
      max<-round(max(PIR$Value2)+3,0)
      color1 <-c("#00BFC4","#F8766D")
      
      
      print(ggplot(PIR, aes(x=agegroup, y=nrel, fill=sex ))+ 
              # geom_text(position = position_dodge(0.9),aes(x = agegroup, y = nrel, label=nrel))+
              geom_bar(data = PIR[(PIR$sex=="Male"),], 
                       colour = I("Black"),stat="identity", size=.3, colour="black",
                       aes(x = agegroup, y = nrel,fill = sex), alpha = 1)+
              geom_bar(data = PIR[(PIR$sex=="Female"),], 
                       colour = I("Black"),stat="identity", size=.3, colour="black",
                       aes(x = agegroup, y = nrel,fill = sex), alpha = 1)+
              geom_text(data = PIR[(PIR$sex=="Male"),],aes(x = agegroup, y = nrel-2.5, label=round(abs(nrel),2)))+
              geom_text(data = PIR[(PIR$sex=="Female"),],aes(x = agegroup, y = nrel+2.5, label=round(abs(nrel),2)))+
              coord_flip()+
              labs(title= paste(unique(input$country2)," ", "(Data Source: ",input$DS2,")", sep=""),
                   subtitle = "Living alone",
                   caption = "\nElaboration: Centre d'Estudis Demogràfics/CERCA")+
              scale_y_continuous(limits=c(-max,max),breaks = c(-max,-max/2,0,max/2,max), 
                                 labels =paste0(as.character(c(seq(max, 0, -max/2), seq(max/2, max,max/2))), "%")) +
              scale_x_continuous(breaks=seq(0,100,5))+
             # labs(title= paste(input$provp,unique(CATFINAL2014$year)),
              #     subtitle = paste("Total population:", a(sum(CATFINAL2014[,1:2])),"|",
               #                     "Spanish-born population:", a(sum(CATFINAL2014[,2])),"|",
                #                    "Foreign-born population:", a(sum(CATFINAL2014[,1])),
                 #                   "Foreign-born (%):", round((sum(CATFINAL2014[,1])/sum(CATFINAL2014[,1:2])*100),digits=2)),
                  # caption = "\nElaboration: GEDEM-CED.\nData: Padrón Continuo (INE)")+
              theme(plot.title = element_text(lineheight=5.6, vjust=1.5,size=12, face="bold"),
                    legend.title = element_blank(),
                    legend.text = element_text(colour="black", size = 10,vjust=2),
                    legend.position="right",
                    legend.background = element_rect(fill="#FfFfFf"),
                    strip.text=element_text(angle = 0,vjust=0.5, size=10,colour="black",face="bold"),
                    axis.title.x = element_blank(),
                    axis.text.x  = element_text(angle = 00, colour="black", size=10),
                    axis.title.y = element_text(colour="black",vjust=2, size=10),
                    axis.text.y  = element_text(vjust=0.5, size=10,colour="black"),
                    panel.background = element_rect(fill = "#ffffff"), 
                    panel.grid = element_line(colour="red"),
                    panel.grid.major=element_line(colour="#f2f2f2",size = .2), 
                    panel.grid.minor=element_line(colour="#f2f2f2"),
                    plot.background = element_rect(fill = "#ffffff"))+ylab("")+xlab("Age"))
      
      
      
      
      
      dev.off()
    })
  
         ####### LIVING with father #####
  output$IMAGEN_IVO2 <- downloadHandler(
    filename = function() { paste(input$country2,input$YearIND, '.png', sep='') },
    content = function(file) {
      png(file,width = 800, height = 700)
      PH <-c("P2") 
      PIR <- INDI[INDI$Country %in% input$country2 & 
                    INDI$Variable %in% PH & 
                    INDI$Areatype %in% input$ambit2 &
                    INDI$Year %in% input$YearIND &
                    INDI$DataSource %in% input$DS2, ]
      
      PIR$nrel <-with(PIR, ifelse((sex=="Male"),Value2*-1,Value2))
      PIR<-as.data.frame(PIR)
      
      
      max<-round(max(PIR$Value2+6),0)
      color1 <-c("#00BFC4","#F8766D")
      
      
      print(ggplot(PIR, aes(x=agegroup, y=nrel, fill=sex ))+ 
              # geom_text(position = position_dodge(0.9),aes(x = agegroup, y = nrel, label=nrel))+
              geom_bar(data = PIR[(PIR$sex=="Male"),], 
                       colour = I("Black"),stat="identity", size=.3, colour="black",
                       aes(x = agegroup, y = nrel,fill = sex), alpha = 1)+
              geom_bar(data = PIR[(PIR$sex=="Female"),], 
                       colour = I("Black"),stat="identity", size=.3, colour="black",
                       aes(x = agegroup, y = nrel,fill = sex), alpha = 1)+
              geom_text(data = PIR[(PIR$sex=="Male"),],aes(x = agegroup, y = nrel-4, label=round(abs(nrel),2)))+
              geom_text(data = PIR[(PIR$sex=="Female"),],aes(x = agegroup, y = nrel+4, label=round(abs(nrel),2)))+
              coord_flip()+
              labs(title= paste(unique(input$country2)," ", "(Data Source: ",input$DS2,")", sep=""),
                   subtitle = "Living with father",
                   caption = "\nElaboration: Centre d'Estudis Demogràfics/CERCA")+
              scale_y_continuous(limits=c(-max,max),breaks = c(-max,-max/2,0,max/2,max), 
                                 labels =paste0(as.character(c(seq(max, 0, -max/2), seq(max/2, max,max/2))), "%")) +
              scale_x_continuous(breaks=seq(0,100,5))+
              # labs(title= paste(input$provp,unique(CATFINAL2014$year)),
              #     subtitle = paste("Total population:", a(sum(CATFINAL2014[,1:2])),"|",
              #                     "Spanish-born population:", a(sum(CATFINAL2014[,2])),"|",
              #                    "Foreign-born population:", a(sum(CATFINAL2014[,1])),
              #                   "Foreign-born (%):", round((sum(CATFINAL2014[,1])/sum(CATFINAL2014[,1:2])*100),digits=2)),
              # caption = "\nElaboration: GEDEM-CED.\nData: Padrón Continuo (INE)")+
              theme(plot.title = element_text(lineheight=5.6, vjust=1.5,size=12, face="bold"),
                    legend.title = element_blank(),
                    legend.text = element_text(colour="black", size = 10,vjust=2),
                    legend.position="right",
                    legend.background = element_rect(fill="#FfFfFf"),
                    strip.text=element_text(angle = 0,vjust=0.5, size=10,colour="black",face="bold"),
                    axis.title.x = element_blank(),
                    axis.text.x  = element_text(angle = 00, colour="black", size=10),
                    axis.title.y = element_text(colour="black",vjust=2, size=10),
                    axis.text.y  = element_text(vjust=0.5, size=10,colour="black"),
                    panel.background = element_rect(fill = "#ffffff"), 
                    panel.grid = element_line(colour="red"),
                    panel.grid.major=element_line(colour="#f2f2f2",size = .2), 
                    panel.grid.minor=element_line(colour="#f2f2f2"),
                    plot.background = element_rect(fill = "#ffffff"))+ylab("")+xlab("Age"))
    
      dev.off()
    })
  
         ####### LIVING with mother ######
  output$IMAGEN_IVO3 <- downloadHandler(
    filename = function() { paste(input$country2,input$YearIND, '.png', sep='') },
    content = function(file) {
      png(file,width = 800, height = 700)
      PH <-c("P3") 
      PIR <- INDI[INDI$Country %in% input$country2 & 
                    INDI$Variable %in% PH & 
                    INDI$Areatype %in% input$ambit2 &
                    INDI$Year %in% input$YearIND &
                    INDI$DataSource %in% input$DS2, ]
      
      PIR$nrel <-with(PIR, ifelse((sex=="Male"),Value2*-1,Value2))
      PIR<-as.data.frame(PIR)
      
      
      max<-round(max(PIR$Value2+6),0)
      color1 <-c("#00BFC4","#F8766D")
      
      
      print(ggplot(PIR, aes(x=agegroup, y=nrel, fill=sex ))+ 
              # geom_text(position = position_dodge(0.9),aes(x = agegroup, y = nrel, label=nrel))+
              geom_bar(data = PIR[(PIR$sex=="Male"),], 
                       colour = I("Black"),stat="identity", size=.3, colour="black",
                       aes(x = agegroup, y = nrel,fill = sex), alpha = 1)+
              geom_bar(data = PIR[(PIR$sex=="Female"),], 
                       colour = I("Black"),stat="identity", size=.3, colour="black",
                       aes(x = agegroup, y = nrel,fill = sex), alpha = 1)+
              geom_text(data = PIR[(PIR$sex=="Male"),],aes(x = agegroup, y = nrel-4, label=round(abs(nrel),2)))+
              geom_text(data = PIR[(PIR$sex=="Female"),],aes(x = agegroup, y = nrel+4, label=round(abs(nrel),2)))+
              coord_flip()+
              labs(title= paste(unique(input$country2)," ", "(Data Source: ",input$DS2,")", sep=""),
                   subtitle = "Living with mother",
                   caption = "\nElaboration: Centre d'Estudis Demogràfics/CERCA")+
              scale_y_continuous(limits=c(-max,max),breaks = c(-max,-max/2,0,max/2,max), 
                                 labels =paste0(as.character(c(seq(max, 0, -max/2), seq(max/2, max,max/2))), "%")) +
              scale_x_continuous(breaks=seq(0,100,5))+
              # labs(title= paste(input$provp,unique(CATFINAL2014$year)),
              #     subtitle = paste("Total population:", a(sum(CATFINAL2014[,1:2])),"|",
              #                     "Spanish-born population:", a(sum(CATFINAL2014[,2])),"|",
              #                    "Foreign-born population:", a(sum(CATFINAL2014[,1])),
              #                   "Foreign-born (%):", round((sum(CATFINAL2014[,1])/sum(CATFINAL2014[,1:2])*100),digits=2)),
              # caption = "\nElaboration: GEDEM-CED.\nData: Padrón Continuo (INE)")+
              theme(plot.title = element_text(lineheight=5.6, vjust=1.5,size=12, face="bold"),
                    legend.title = element_blank(),
                    legend.text = element_text(colour="black", size = 10,vjust=2),
                    legend.position="right",
                    legend.background = element_rect(fill="#FfFfFf"),
                    strip.text=element_text(angle = 0,vjust=0.5, size=10,colour="black",face="bold"),
                    axis.title.x = element_blank(),
                    axis.text.x  = element_text(angle = 00, colour="black", size=10),
                    axis.title.y = element_text(colour="black",vjust=2, size=10),
                    axis.text.y  = element_text(vjust=0.5, size=10,colour="black"),
                    panel.background = element_rect(fill = "#ffffff"), 
                    panel.grid = element_line(colour="red"),
                    panel.grid.major=element_line(colour="#f2f2f2",size = .2), 
                    panel.grid.minor=element_line(colour="#f2f2f2"),
                    plot.background = element_rect(fill = "#ffffff"))+ylab("")+xlab("Age"))

      
      dev.off()
    })
  
         ####### LIVING with SPOUSE ######
  output$IMAGEN_IVO4 <- downloadHandler(
    filename = function() { paste(input$country2,input$YearIND, '.png', sep='') },
    content = function(file) {
      png(file,width = 800, height = 700)
      PH <-c("P4") 
      PIR <- INDI[INDI$Country %in% input$country2 & 
                    INDI$Variable %in% PH & 
                    INDI$Areatype %in% input$ambit2 &
                    INDI$Year %in% input$YearIND &
                    INDI$DataSource %in% input$DS2, ]
      
      PIR$nrel <-with(PIR, ifelse((sex=="Male"),Value2*-1,Value2))
      PIR<-as.data.frame(PIR)
      
      
      max<-round(max(PIR$Value2+6),0)
      color1 <-c("#00BFC4","#F8766D")
      
      
      print(ggplot(PIR, aes(x=agegroup, y=nrel, fill=sex ))+ 
              # geom_text(position = position_dodge(0.9),aes(x = agegroup, y = nrel, label=nrel))+
              geom_bar(data = PIR[(PIR$sex=="Male"),], 
                       colour = I("Black"),stat="identity", size=.3, colour="black",
                       aes(x = agegroup, y = nrel,fill = sex), alpha = 1)+
              geom_bar(data = PIR[(PIR$sex=="Female"),], 
                       colour = I("Black"),stat="identity", size=.3, colour="black",
                       aes(x = agegroup, y = nrel,fill = sex), alpha = 1)+
              geom_text(data = PIR[(PIR$sex=="Male"),],aes(x = agegroup, y = nrel-4, label=round(abs(nrel),2)))+
              geom_text(data = PIR[(PIR$sex=="Female"),],aes(x = agegroup, y = nrel+4, label=round(abs(nrel),2)))+
              coord_flip()+
              labs(title= paste(unique(input$country2)," ", "(Data Source: ",input$DS2,")", sep=""),
                   subtitle = "Living with spouse",
                   caption = "\nElaboration: Centre d'Estudis Demogràfics/CERCA")+
              scale_y_continuous(limits=c(-max,max),breaks = c(-max,-max/2,0,max/2,max), 
                                 labels =paste0(as.character(c(seq(max, 0, -max/2), seq(max/2, max,max/2))), "%")) +
              scale_x_continuous(breaks=seq(0,100,5))+
              # labs(title= paste(input$provp,unique(CATFINAL2014$year)),
              #     subtitle = paste("Total population:", a(sum(CATFINAL2014[,1:2])),"|",
              #                     "Spanish-born population:", a(sum(CATFINAL2014[,2])),"|",
              #                    "Foreign-born population:", a(sum(CATFINAL2014[,1])),
              #                   "Foreign-born (%):", round((sum(CATFINAL2014[,1])/sum(CATFINAL2014[,1:2])*100),digits=2)),
              # caption = "\nElaboration: GEDEM-CED.\nData: Padrón Continuo (INE)")+
              theme(plot.title = element_text(lineheight=5.6, vjust=1.5,size=12, face="bold"),
                    legend.title = element_blank(),
                    legend.text = element_text(colour="black", size = 10,vjust=2),
                    legend.position="right",
                    legend.background = element_rect(fill="#FfFfFf"),
                    strip.text=element_text(angle = 0,vjust=0.5, size=10,colour="black",face="bold"),
                    axis.title.x = element_blank(),
                    axis.text.x  = element_text(angle = 00, colour="black", size=10),
                    axis.title.y = element_text(colour="black",vjust=2, size=10),
                    axis.text.y  = element_text(vjust=0.5, size=10,colour="black"),
                    panel.background = element_rect(fill = "#ffffff"), 
                    panel.grid = element_line(colour="red"),
                    panel.grid.major=element_line(colour="#f2f2f2",size = .2), 
                    panel.grid.minor=element_line(colour="#f2f2f2"),
                    plot.background = element_rect(fill = "#ffffff"))+ylab("")+xlab("Age"))
      
      
      dev.off()
    })
  
         ####### living arregements: FEMALES ######
  output$IMAGEN_IVO5 <- downloadHandler(
    filename = function() { paste(input$country2,input$YearIND, '.png', sep='') },
    content = function(file) {
      png(file,width = 850, height = 700)
      PH <-c("P1", "P2", "P3", "P4", "P5", "P6") 
      PIR <- INDI[INDI$Country %in% input$country2 & 
                    INDI$Variable %in% PH & 
                    INDI$Areatype %in% input$ambit2 &
                    INDI$Year %in% input$YearIND &
                    INDI$DataSource %in% input$DS2, ]
      
      
      PIR<-PIR[PIR$sex=='Female',]
      g <- ggplot(PIR, aes(agegroup,Value2)) + 
        geom_line(aes(colour=variableN),size=1.25)+
        geom_point(aes(colour=variableN),size=2)+ 
        #facet_wrap(~ sex, ncol=2)+
        scale_y_continuous(breaks = c(0,20,40,60,80,100))
      
     
      print( g+ scale_x_continuous(breaks=unique(PIR$agegroup), 
                                  labels=if(length(unique(PIR$agegroup))==21){cien
                                  }else{if(length(unique(PIR$agegroup))==20)
                                  {noventacinco  #LISTO
                                  }else{if(length(unique(PIR$agegroup))==19)
                                  {noventa 
                                  }else{if(length(unique(PIR$agegroup))==18)
                                  {ochentacinco 
                                  }else{if(length(unique(PIR$agegroup))==17)
                                  {ochenta
                                  }else{if(length(unique(PIR$agegroup))==16)
                                  {setentacinco 
                                    }}}}}} ) +
               labs(title= paste(unique(input$country2)," ",unique(input$YearIND)," ", "(Data Source: ",input$DS2,")", sep=""),
                    subtitle = "Sex: Female",
                    caption = "\nElaboration: Centre d'Estudis Demogràfics/CERCA")+
               theme(plot.title = element_text(lineheight=5.6, vjust=1.5,size=14, face="bold"),
                     legend.title = element_blank(),
                     legend.text = element_text(colour="black", size = 10,vjust=2),
                     legend.position="bottom",
                     legend.background = element_rect(fill="#FfFfFf"),
                     strip.text=element_text(angle = 0,vjust=0.5, size=10,colour="black",face="bold"),
                     axis.title.x = element_blank(),
                     axis.text.x  = element_text(angle = 00, colour="black", size=10),
                     axis.title.y = element_text(colour="black",vjust=2, size=10),
                     axis.text.y  = element_text(vjust=0.5, size=10,colour="black"),
                     panel.background = element_rect(fill = "#ffffff"), 
                     panel.grid = element_line(colour="red"),
                     panel.grid.major=element_line(colour="#f2f2f2",size = .2), 
                     panel.grid.minor=element_line(colour="#ffffff"),
                     plot.background = element_rect(fill = "#ffffff"))+ylab("%"))
      
      
      dev.off()
    })
  
         ####### living arregements: MALES ######
  output$IMAGEN_IVO6 <- downloadHandler(
    filename = function() { paste(input$country2,input$YearIND, '.png', sep='') },
    content = function(file) {
      png(file,width = 850, height = 700)
      PH <-c("P1", "P2", "P3", "P4", "P5", "P6") 
      PIR <- INDI[INDI$Country %in% input$country2 & 
                    INDI$Variable %in% PH & 
                    INDI$Areatype %in% input$ambit2 &
                    INDI$Year %in% input$YearIND &
                    INDI$DataSource %in% input$DS2, ]
      
      
      PIR<-PIR[PIR$sex=='Male',]
      g <- ggplot(PIR, aes(agegroup,Value2)) + 
        geom_line(aes(colour=variableN),size=1.25)+
        geom_point(aes(colour=variableN),size=2)+ 
        #facet_wrap(~ sex, ncol=2)+
        scale_y_continuous(breaks = c(0,20,40,60,80,100))
      
      
      print( g+ scale_x_continuous(breaks=unique(PIR$agegroup), 
                                   labels=if(length(unique(PIR$agegroup))==21){cien
                                   }else{if(length(unique(PIR$agegroup))==20)
                                   {noventacinco  #LISTO
                                   }else{if(length(unique(PIR$agegroup))==19)
                                   {noventa 
                                   }else{if(length(unique(PIR$agegroup))==18)
                                   {ochentacinco 
                                   }else{if(length(unique(PIR$agegroup))==17)
                                   {ochenta
                                   }else{if(length(unique(PIR$agegroup))==16)
                                   {setentacinco 
                                   }}}}}} ) +
               labs(title= paste(unique(input$country2)," ",unique(input$YearIND)," ", "(Data Source: ",input$DS2,")", sep=""),
                    subtitle = "Sex: Male",
                    caption = "\nElaboration: Centre d'Estudis Demogràfics/CERCA")+
               theme(plot.title = element_text(lineheight=5.6, vjust=1.5,size=14, face="bold"),
                     legend.title = element_blank(),
                     legend.text = element_text(colour="black", size = 10,vjust=2),
                     legend.position="bottom",
                     legend.background = element_rect(fill="#FfFfFf"),
                     strip.text=element_text(angle = 0,vjust=0.5, size=10,colour="black",face="bold"),
                     axis.title.x = element_blank(),
                     axis.text.x  = element_text(angle = 00, colour="black", size=10),
                     axis.title.y = element_text(colour="black",vjust=2, size=10),
                     axis.text.y  = element_text(vjust=0.5, size=10,colour="black"),
                     panel.background = element_rect(fill = "#ffffff"), 
                     panel.grid = element_line(colour="red"),
                     panel.grid.major=element_line(colour="#f2f2f2",size = .2), 
                     panel.grid.minor=element_line(colour="#ffffff"),
                     plot.background = element_rect(fill = "#ffffff"))+ylab("%"))
      
      
      dev.off()
    })
}) 