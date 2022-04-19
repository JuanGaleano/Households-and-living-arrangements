
#### LIBRARIES ####
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

#### LOAD DATA ####
load("./data/HOGARES.Rdata")
load("./data/INDIVIDUOS.Rdata")
load("./data/indicators.Rdata")

##### GLOBAL #####
HOGARES$DataSource <- factor(HOGARES$DataSource,
                             levels = c("IPUMS",
                                        "DHS",
                                        "LFS"))

choices <- c(unique(levels(HOGARES$DataSource)) )



indicator <- structure(c("H1", "H2", "H3", "H4", "H5", "H6", "H7", "H8", "H9", "H10", "H11",
                         "H12", "H13", "H14", "H15", "H16", "H17", "H18"), 
                    .Names = c("Mean household size", 
                               "1 person HH/all HH",
                               "2 person HH/all HH",
                               "3 person HH/all HH",
                               "4 person HH/all HH",
                               "5 person HH/all HH",
                               "6 person HH/all HH",
                               "HH with a female head (%)", 
                               "HH with children under 15 (%)", 
                               "HH with person over 60 (%)",
                               "Average number of children for HH with children under 15",
                               "HH with children under 15 only with mother (%)",
                               "HH with children under 15 only with father (%)",
                               "HH with children under 15  with both parents (%)",
                               "HH with children under 15  with none parents (%)",
                               "Head 60+ (%)",
                               "Single mother/Female Head (%)",
                               "HH with children under 15 & person(s) over 60 (%)"))


indicatorpenn <- structure(c(as.character(unique(indicators$indicator))), 
                       .Names = c("Total Fertility Rate (TFR)",
                                  "Net Reproduction Rate (NRR)",
                                  "Singulate Mean Age at First Sex - women",
                                  "Singulate Mean Age at First Sex - men",
                                  "Singulate Mean Age at First Marriage - women",
                                  "Singulate Mean Age at First Marriage - men",
                                  "Singulate Mean Age at First Birth - women",
                                  "Singulate Mean Age at First Birth - men",
                                  "Husband decides about women's health (proportion of HH)",
                                  "Husband decides about household purchases (proportion of HH)",
                                  "Husband decides about women's visits (proportion of HH)",
                                  "Share of children living with both parents",
                                  "Share of women living in a nuclear household",
                                  "Share of women living in a three-generation household",
                                  "Share of women living in a complex household",
                                  "Prevalence of marriage (proportion)",
                                  "Marital expectancy at age 15 (years)",
                                  "Prevalence of cohabitation (proportion)",
                                  "Cohabitation expectancy at age 15 (years)",
                                  "Marital and cohabitation expectancy at age 15 (years)"))


variable <- structure(c("P1", "P2", "P3", "P4", "P5", "P6","P11",
                         "P12", "P13", "P14", "P15", "P16"), 
                       .Names = c("Living alone", 
                                  "Living with father",
                                  "Living with mother",
                                  "Living with spouse",
                                  "Living with children",
                                  "Living with others",
                                  "Only with a partner",
                                  "With partner and children", 
                                  "Without partner and with children", 
                                  "Only with father",
                                  "Only with mother",
                                  "With both parents"))

sex <-c("Male", "Female")

ageg<- c("0-4", "5-9", "10-14", "15-19","20-24", "25-29","30-34", "35-39","40-44", "45-49",
         "50-54", "55-59","60-64", "65-69","70-74", "75-79","80-84", "85-89","90-94", "95+")

REGION<-structure(c("EU", "AM", "AS", "AF", "0"), 
                  .Names = c('Europe','America','Asia','Africa','Other'))

unique(HOGARES$REGION)
                    
                    
HOGARES$DECADE <- factor(HOGARES$DECADE,
                             levels = c("1960 round",
                                        "1970 round",
                                        "1980 round",
                                        "1990 round",
                                        "2000 round",
                                        "2010 round"))

INDI$DECADE <- factor(INDI$DECADE,
                         levels = c("1960 round",
                                    "1970 round",
                                    "1980 round",
                                    "1990 round",
                                    "2000 round",
                                    "2010 round"))


YEARS <-sort(unique(INDI$Year))

##### GLOBAL ####
HOGARES$indicator2<-with(HOGARES, ifelse(indicator=="H2",  "1 person HH/all HH",
                                         ifelse(indicator=="H3", "2 person HH/all HH",
                                                ifelse(indicator=="H4","3 person HH/all HH",
                                                       ifelse(indicator=="H5",  "4 person HH/all HH",
                                                              ifelse(indicator=="H6",  "5 person HH/all HH",
                                                                     ifelse(indicator=="H7",  "6 person HH/all HH",
                                                                            ifelse(indicator=="H8", "HH with a female head (%)", 
                                                                                   ifelse(indicator=="H9",  "HH with children under 15 (%)", 
                                                                                          ifelse(indicator=="H10",  "HH with person over 60 (%)",
                                                                                                 ifelse(indicator=="H11",  "Average number of children for HH with children under 15",
                                                                                                        ifelse(indicator=="H12", "HH with children under 15 only with mother (%)",
                                                                                                               ifelse(indicator=="H13",   "HH with children under 15 only with father (%)",
                                                                                                                      ifelse(indicator=="H14",  "HH with children under 15  with both parents (%)",
                                                                                                                             ifelse(indicator=="H15",  "HH with children under 15  with none parents (%)",
                                                                                                                                    ifelse(indicator=="H16", "Head 60+ (%)",
                                                                                                                                           ifelse(indicator=="H17",  "Single mother/Female Head (%)",
                                                                                                                                                  ifelse(indicator=="H18",  "HH with children under 15 & person(s) over 60 (%)",Value))))))))))))))))))

HOGARES$DataSource <- factor(HOGARES$DataSource,
                             levels = c("IPUMS",
                                        "DHS",
                                        "LFS"))



##### UI ####

shinyUI(navbarPage(div(img(src="logotipCED_nuevo.png",height = 60, width = 160),
                       title="WEB CED", href="http://ced.uab.es"), 
                  
                       windowTitle="Households and living arrengements", 
                       theme = shinytheme("spacelab"),  
                      
          
                  ###### HOGARES #####
                  navbarMenu(h5(span(strong("Households (HH)" ))),
                              ####### WORLD MAP LATEST DATA AVAILABLE ####
                              tabPanel(h5(span("World Map (latest available data)" )),                          
                                       
                                       leafletOutput("mymap",  width = "100%", height = 700),
                                       
                                       fluidPage(absolutePanel(top=70, left =70,draggable = FALSE,
                                                               br(),
                                                               br(),
                                                               h5(strong("World map (latest available data)")),
                                                               br(),
                                                               selectizeInput("indicator", "Select an indicator:", 
                                                                              choices=indicator,selected = "H1",
                                                                              multiple = FALSE,width = "90%")
                                                               
                                       )),
                                       
                                       fluidPage(absolutePanel(top=70, left ='72.5%',draggable = TRUE,
                                                               br(),
                                                               br(),
                                                               br(),
                                                               downloadButton('MAP_IMAGE1','Download an image of the map')
                                       )),
                                       
                                       fluidPage(absolutePanel(top=600, left =70,draggable = TRUE,
                                                               h5(strong(""))#,
                                                               #p(includeHTML("http://gedemced.uab.cat/images/TWEETCED.htm")),
                                                              # h5(strong("Follow us")),
                                                               #p(includeHTML("http://gedemced.uab.cat/images/FOLLOWCED.htm")),
                                                               #tags$iframe(src="//www.facebook.com/plugins/follow.php?href=https%3A%2F%2Fwww.facebook.com%2FCEDemografia&amp;width&amp;height=80&amp;colorscheme=light&amp;layout=standard&amp;show_faces=true",
                                                                #           scrolling="no",
                                                                 #          frameborder="0",
                                                                  #         style="border:none; overflow:hidden; height:80px;",
                                                                   #        allowTransparency="true"))
                                                 ))),
                              
                              ####### WORLD MAP LATEST CENSUS ROUNDS ####
                              tabPanel(h5(span("World Map (Census rounds)")),                          
                                       
                                       leafletOutput("mymap1",  width = "100%", height = 700),
                                       
                                       fluidPage(absolutePanel(top=70, left =70,draggable = FALSE,
                                                               br(),
                                                               br(),
                                                               h5(strong("World map (Census rounds)")),
                                                               #h3(strong("Seccions censals 2015")),
                                                               br(),
                                                               selectizeInput("censusround", "Select a census round:", 
                                                                              choices=levels(HOGARES$DECADE),selected = "2010 round",
                                                                              multiple = FALSE,width = "90%"),
                                                               selectizeInput("indicator2", "Select an indicator:", 
                                                                              choices=indicator,selected = "H1",
                                                                              multiple = FALSE,width = "90%")
                                                               
                                       )),
                                       
                                       
                                       fluidPage(absolutePanel(top=70, left ='72.5%',draggable = TRUE,
                                                               br(),
                                                               br(),
                                                               br(),
                                                               downloadButton('MAP_IMAGE2','Download an image of the map')
                                       )),
                                       
                                       fluidPage(absolutePanel(top=600, left =70,draggable = TRUE,
                                                               
                                                               h5(strong(""))#,
                                                               #p(includeHTML("http://gedemced.uab.cat/images/TWEETCED.htm")),
                                                               #h5(strong("Follow us")),
                                                               #p(includeHTML("http://gedemced.uab.cat/images/FOLLOWCED.htm")),
                                                               #tags$iframe(src="//www.facebook.com/plugins/follow.php?href=https%3A%2F%2Fwww.facebook.com%2FCEDemografia&amp;width&amp;height=80&amp;colorscheme=light&amp;layout=standard&amp;show_faces=true",
                                                                #           scrolling="no",
                                                                 #          frameborder="0",
                                                                  #         style="border:none; overflow:hidden; height:80px;",
                                                                   #        allowTransparency="true")
                                                               ))),
                              
                              ####### INDICATORS BARPLOTS ####
                  tabPanel(h5(span("Households (HH) indicators by country" )),
                      
                                     sidebarPanel(
                                     selectizeInput(inputId = "country", 
                                         label = "Select a country",
                                         multiple  = F,
                                         selected="Spain",
                                         choices =   unique(HOGARES[["Country"]])
                                       ),
                                       selectizeInput("ambit", "Choose a population type:", 
                                                      choices=c("Total", "Urban", "Rural"),
                                                      selected="Total",multiple = FALSE),
                                       selectizeInput("DS", "Select a database", 
                                                      choices=choices,multiple = F),
                                       br(),
                                       br(),
                                       h5(strong("Download barplots"), align="center"),
                                  
                                       downloadLink('IMAGEN_EVO', label =  h5(strong('HH mean size')), align="center"),
                                       downloadLink('IMAGEN_EVO2', label =  h5(strong('HH size (Relative distribution)')), align="center"),
                                       downloadLink('IMAGEN_EVO3', label =  h5(strong('Female head and single mother')), align="center"),
                                       downloadLink('IMAGEN_EVO4', label =  h5(strong('HH with children and elderly')), align="center"),
                                       downloadLink('IMAGEN_EVO5', label =  h5(strong('HH with children under 15')), align="center"),
                                       downloadLink('IMAGEN_EVO6', label =  h5(strong('HH with head 60+')), align="center"),
                                       downloadLink('IMAGEN_EVO7', label =  h5(strong('Average number of children')), align="center"),
                                       br(),
                                       br(),
                                       br(),
                                       br(),
                                       br(),
                                       br()),
                                     
                                     ### Main Panel
                                     mainPanel(
                                       fluidPage(tabsetPanel(
                                         tabPanel(h6('Mean HH Size'),
                                                  br(),
                                                  # Show the plot
                                                  highchartOutput("barplotaverage",height = 500, width = "90%")),
                                         tabPanel(h6('HH size (relative distribution)'),
                                                  br(),
                                                  # Show the plot
                                                  highchartOutput("barplothousehold",height = 500, width = "90%")),
                                         tabPanel(h6('Female head and single mothers'),
                                                  br(),
                                                  # Show the plot
                                                  highchartOutput("V817",height = 500, width = "90%")),
                                         tabPanel(h6('HH with children and elderly'),
                                                  br(),
                                                  # Show the plot
                                                  highchartOutput("V910",height = 500, width = "90%")),
                                         tabPanel(h6('HH with children under 15'),
                                                  br(),
                                                  # Show the plot
                                                  highchartOutput("V1215",height = 500, width = "90%")),
                                         tabPanel(h6('HH head 60+'),
                                                  br(),
                                                  # Show the plot
                                                  highchartOutput("V16",height = 500, width = "90%")),
                                         tabPanel(h6('Averge number of children'),
                                                  br(),
                                                  # Show the plot
                                                  highchartOutput("V11",height = 500, width = "90%"))
                                        
                                       ),width=9.5))),
                  ####### INDICATORS BARPLOTS ####
                  tabPanel(h5(span("Households (HH) indicators compare countries" )),
                           
                           sidebarPanel(
                             selectizeInput("indicatorcom", "Select an indicator:", 
                                            choices=indicator,selected = "H1",
                                            multiple = FALSE),
                             selectizeInput("region", "Select a region:", 
                                            choices=REGION,selected = "EU",
                                            multiple = T),
                             selectizeInput(inputId = "countrycom", 
                                            label = "Select a country",
                                            multiple  = T,
                                            selected="Spain",
                                            choices =   unique(HOGARES[["Country"]]))
                             ),
                           
                           ### Main Panel
                           mainPanel(
                           
                               tabPanel(h6('Mean HH Size'),
                                        br(),
                                        # Show the plot
                                        highchartOutput("lineplotcom",height = 600, width = "100%"))
                                        ))
                  ),
                   
                  
                   ###### INDIVIDUOS #####
                  navbarMenu(h5(strong("Individuals")),
                             ####### WORLD MAP LATEST DATA AVAILABLE ####
                             tabPanel(h5(span("World Map (latest available data)" )),                          
                                      
                                      leafletOutput("mymapIND",  width = "100%", height = 700),
                                      
                                      fluidPage(absolutePanel(top=70, left =70,draggable = FALSE,
                                                              br(),
                                                              br(),
                                                              h5(strong("World map (latest available data)")),
                                                              br(),
                                                              selectizeInput("variable", "Select an indicator:", 
                                                                             choices=variable,selected = "P1",
                                                                             multiple = FALSE,width = "90%"),
                                                              selectizeInput("sex", "Select a population group:", 
                                                                             choices=sex,selected = "Male",
                                                                             multiple = FALSE,width = "90%"),
                                                              selectizeInput("ageg", "Select an age group:", 
                                                                             choices=ageg,selected = "20-24",
                                                                             multiple = FALSE,width = "90%")
                                                              
                                      )),
                                      
                                      fluidPage(absolutePanel(top=70, left ='72.5%',draggable = TRUE,
                                                              br(),
                                                              br(),
                                                              br(),
                                                              #    h5(strong("Descarrega una imatge mapa:")),
                                                              downloadButton('MAP_IMAGE1IND','Download an image of the map')
                                      )),
                                      
                                      fluidPage(absolutePanel(top=600, left =70,draggable = TRUE,
                                                              h5(strong(""))#,
                                                             # p(includeHTML("http://gedemced.uab.cat/images/TWEETCED.htm")),
                                                             # h5(strong("Follow us")),
                                                            #  p(includeHTML("http://gedemced.uab.cat/images/FOLLOWCED.htm")),
                                                             # tags$iframe(src="//www.facebook.com/plugins/follow.php?href=https%3A%2F%2Fwww.facebook.com%2FCEDemografia&amp;width&amp;height=80&amp;colorscheme=light&amp;layout=standard&amp;show_faces=true",
                                                              #            scrolling="no",
                                                               #           frameborder="0",
                                                                #          style="border:none; overflow:hidden; height:80px;",
                                                                 #         allowTransparency="true")
                                                              ))  
                                      
                             ),  
                             ####### WORLD MAP LATEST CENSUS ROUNDS ####
                             tabPanel(h5(span("World Map (Census rounds)" )),                          
                                      
                                      leafletOutput("mymapIND1",  width = "100%", height = 700),
                                      
                                      fluidPage(absolutePanel(top=70, left =70,draggable = FALSE,
                                                              br(),
                                                              br(),
                                                              h5(strong("World map (latest available data)")),
                                                              br(),
                                                              selectizeInput("variable2", "Select an indicator:", 
                                                                             choices=variable,selected = "P1",
                                                                             multiple = FALSE,width = "90%"),
                                                              selectizeInput("sex2", "Select a population group:", 
                                                                             choices=sex,selected = "Male",
                                                                             multiple = FALSE,width = "90%"),
                                                              selectizeInput("ageg2", "Select an age group:", 
                                                                             choices=ageg,selected = "20-24",
                                                                             multiple = FALSE,width = "90%"),
                                                              selectizeInput("censusround2", "Select a census round:", 
                                                                             choices=levels(INDI$DECADE),selected = "2010 round",
                                                                             multiple = FALSE,width = "90%")
                                                              
                                      )),
                                      
                                      fluidPage(absolutePanel(top=70, left ='72.5%',draggable = TRUE,
                                                              br(),
                                                              br(),
                                                              br(),
                                                              #    h5(strong("Descarrega una imatge mapa:")),
                                                              downloadButton('MAP_IMAGE1IND2','Download an image of the map')
                                      )),
                                      
                                      fluidPage(absolutePanel(top=600, left =70,draggable = TRUE,
                                                              h5(strong(""))#,
                                                             # p(includeHTML("http://gedemced.uab.cat/images/TWEETCED.htm")),
                                                            #  h5(strong("Follow us")),
                                                             # p(includeHTML("http://gedemced.uab.cat/images/FOLLOWCED.htm")),
                                                              #tags$iframe(src="//www.facebook.com/plugins/follow.php?href=https%3A%2F%2Fwww.facebook.com%2FCEDemografia&amp;width&amp;height=80&amp;colorscheme=light&amp;layout=standard&amp;show_faces=true",
                                                               #           scrolling="no",
                                                                #          frameborder="0",
                                                                 #         style="border:none; overflow:hidden; height:80px;",
                                                                  #        allowTransparency="true")
                                                              ))  
                                      
                             ),
                             ####### INDICATORS BARPLOTS 6#####
                   tabPanel(h5(span("Indicators by country" )),
                            
                            sidebarPanel(
                               selectizeInput(
                                inputId = "country2", 
                                label = "Select a country",
                                multiple  = F,
                                selected="Spain",
                                choices = unique(INDI[["Country"]])
                              ),
                              selectizeInput("ambit2", "Choose a population type:", 
                                             choices=c("Total", "Urban", "Rural"),selected="Total",multiple = FALSE),
                              selectizeInput("YearIND", "Select a year", 
                                             choices=YEARS,multiple = F),
                              selectizeInput("DS2", "Select a database", 
                                             choices=choices,multiple = F),
                              
                              br(),
                              br(),
                              h5(strong("Download lineplots"),align="center"),
                             
                              downloadLink('IMAGEN_IVO6', label =  h5(strong('Males')), align="center"), 
                              downloadLink('IMAGEN_IVO5', label =  h5(strong('Females')), align="center"),
                             
                             
                              br(),
                              br(),
                              br(),
                              br(),
                              br()),
                            mainPanel(
                              fluidPage(
                              #tabsetPanel(
                               # tabPanel(h6('Living alone'),
                                #                    br(),
                                         # Show the plot
                                #                    highchartOutput("livingalone",height = 600, width = "100%")),
                                #           tabPanel(h6('Living with father'),
                                #                   br(),
                                         # Show the plot
                                #                    highchartOutput("livinfather",height = 600, width = "100%")),
                                #           tabPanel(h6('Living with mother'),
                                #                    br(),
                                         # Show the plot
                                #                    highchartOutput("livinmother",height = 600, width = "100%")),
                                #            tabPanel(h6('Living with spouse'),
                                #                 br(),
                                         # Show the plot
                                #                highchartOutput("livinspouse",height = 600, width = "100%")),
                                #       tabPanel(h6('Living with children'),
                                #                br(),
                                         # Show the plot
                                #                highchartOutput("livinchildren",height = 600, width = "100%")),
                              #       tabPanel(h6('Living with others'),
                                #               br(),
                                         # Show the plot
                                #                highchartOutput("livinothers",height = 600, width = "100%")),
                                      #tabPanel(h6('Lines'),
                                         
                                         br(),
                                         # Show the plot
                                         splitLayout(cellWidths = c("50%", "50%"),
                                                     highchartOutput("linesm",width = "100%",height = 600),
                                                     highchartOutput("linesf",width = "100%",height = 600)), #)
                             # ),
                              width=15))
                                       )),    
                  
    
                  
                  ###### penn ####              
                  
                  navbarMenu(h5(strong("Global Family Change")),
                             
                             ####### WORLD MAP LATEST DATA AVAILABLE penn ####
                             tabPanel(h5(span("World Map (latest available data)" )),                          
                                      
                                      leafletOutput("mymappenn",  width = "100%", height = 700),
                                      
                                      fluidPage(absolutePanel(top=70, left =70,draggable = FALSE,
                                                              br(),
                                                              br(),
                                                              h5(strong("World map (latest available data)")),
                                                              br(),
                                                              selectizeInput("indicatorpenn", "Select an indicator:", 
                                                                             choices=indicatorpenn,selected = "r238_wom",
                                                                             multiple = FALSE,width = "90%")
                                                              
                                      )),
                                      
                                      fluidPage(absolutePanel(top=70, left ='72.5%',draggable = TRUE,
                                                              br(),
                                                              br(),
                                                              br(),
                                                              downloadButton('MAP_IMAGE1penn','Download an image of the map')
                                      )),
                                      
                                      fluidPage(absolutePanel(top=600, left =70,draggable = TRUE,
                                                              h5(strong(""))#,
                                                              #p(includeHTML("http://gedemced.uab.cat/images/TWEETCED.htm")),
                                                              #h5(strong("Follow us")),
                                                              #p(includeHTML("http://gedemced.uab.cat/images/FOLLOWCED.htm")),
                                                              #tags$iframe(src="//www.facebook.com/plugins/follow.php?href=https%3A%2F%2Fwww.facebook.com%2FCEDemografia&amp;width&amp;height=80&amp;colorscheme=light&amp;layout=standard&amp;show_faces=true",
                                                               #           scrolling="no",
                                                                #          frameborder="0",
                                                                 #         style="border:none; overflow:hidden; height:80px;",
                                                                  #        allowTransparency="true")
                                                              ))  
                                      
                             ),
                             #### indicators ####
                             tabPanel(h5(span("Indicators" )),
                                      
                                      sidebarPanel(
                                        selectizeInput(inputId = "countrypenn", 
                                                       label = "Select a country",
                                                       multiple  = F,
                                                       selected="Colombia",
                                                       choices =   unique(indicators[["country"]])
                                        ),
                                        selectizeInput("indicatorpenn1", "Select an indicator:", 
                                                       choices=indicatorpenn,selected = "gnu",
                                                       multiple = FALSE,width = "90%"),
                                        br(),
                                        br(),
                                      #  h5(strong("Download barplots"), align="center"),
                                        
                                        downloadLink('IMAGEN_penn', label =  h5(strong('Download as image')), align="center"),
                                        br(),
                                        br(),
                                        br(),
                                        br(),
                                        br(),
                                        br()),
                                      
                                      ### Main Panel
                                      mainPanel(
                                        fluidPage(
                                         
                                                   # Show the plot
                                                   highchartOutput("barplotpenn",height = 500, width = "100%")
                                          ,width=9.5)))      
                             
                             ),
                  
                  ###### ABOUT THE PROJECT  #####        
                   navbarMenu(h5(strong("About")),
                              tabPanel(h5(span("Demographic Data Hub (DDH)" )),
                                       br(),
        
                                       h5(strong("Demographic Data Hub")),
                                       p("The Demographic Data Hub (DDH) is an open acces platform to consult, analyze, visualize and 
                                         download demographic data. It is and initiative aimed to fullfill the growing demand for 
                                         rigorous and truthful information on various current demographic events. 
                                         The DDH is structured in separate modules covering different different socio demographic fenomena.
                                         The idea is that data and results from ongoing research carry out at the ",a(strong("Centre for Demographic Studies"),href="http://ced.uab.es/en/"),  
                                        " can go out and impact policymakers, civil society, academic and media circles."),
                                       br(),
                                       h5(strong("Credits")),
                                       p(strong("Coordination: "),a(strong("Albert Esteve"), href="http://ced.uab.es/directori/albert-esteve-palos/")),
                                       p(strong("Data: "),a(strong("Teresa Menacho, "), href="http://ced.uab.es/directori/teresa-menacho-montes/"),
                                                           a(strong("Antonio Lopez Gay, "), href="http://ced.uab.es/directori/antonio-lopez-gay/"),
                                                           a(strong("Antonio Medina"), href="http://ced.uab.es/directori/antonio-jose-medina-cruz/"),"and",
                                                           a(strong("Joan Garcia Román"), href="http://ced.uab.es/directori/joan-garcia-roman/"),"."),
                                       p(strong("Web and data visualization (R & Shiny): "),a(strong("Juan Galeano"), href="http://ced.uab.es/directori/juan-galeano/")),
                                       p(strong("Shiny Server: "),a(strong("Xavier Ruiz Vilchez"), href="http://ced.uab.es/sobre-el-ced/directori/?tipo=suport-a-la-recerca")),
                                       
                                       br(),
                                       h5(strong("Suggested Citation")),
                                       p("The database for this project is under license Creative Commons Attribution 4.0 International."),
                                       #p(includeHTML("http://gedemced.uab.cat/images/LICENSE1.htm")),
                                       p("Data and graphics can be reused, but please, don't forget to refer this project: ", 
                                         strong("Centre d'Estudis Demografics (2017),",
                                                em("Household and living arrengements. Version 1.1"))),
                                       
                                       
                                       br(),
                                       h5(strong("Follow us")),
                                      # p(includeHTML("http://gedemced.uab.cat/images/FOLLOWCED.htm")),
                                      # tags$iframe(src="//www.facebook.com/plugins/follow.php?href=https%3A%2F%2Fwww.facebook.com%2FCEDemografia&amp;width&amp;height=80&amp;colorscheme=light&amp;layout=standard&amp;show_faces=true",
                                       #            scrolling="no",
                                        #           frameborder="0",
                                         #          style="border:none; overflow:hidden; height:80px;",
                                          #         allowTransparency="true")
                                       style='width: 1000px; height: 1000px'),
                              
                              ### esto es lo que hace que la leyenda de los mapas se vea correctamente 
                              
                              tags$style(type="text/css", "div.info.legend.leaflet-control br {clear: both;}"),
                              
                              tabPanel(h5(span("About data" )),
                                       h5(strong("About data")),
    
                                       p("Data presented in this site has been produced in the context of a collaborative project 
                                         between the ", a("Center for Demographic Studies (Barcelona)",href="http://ced.uab.es/en/"),
                                         "and ", a("United Nations", href="http://www.un.org/en/index.html"),"."),
                                       br(),
                                       h5(strong("Sources")),
                                      # p(a("IPUMS-International",href="https://international.ipums.org/international/")),
                                       #p(a("Labour Force Survey",href="https://www.ons.gov.uk/surveys/informationforhouseholdsandindividuals/householdandindividualsurveys/labourforcesurveylfs")),
                                       #p(a("Demographic and Health Surveys",href="http://dhsprogram.com/")),
                                       br(),
                                       h5(strong("Households level variables")),
                                
                                       h5(strong("1. IPUMS-International (IPUMS)")),
                                      p("We have used all the IPUMS samples where persons are organized into households."),
                                      p("Samples where persons were not organized into household were excluded."),
                                      p("We have included private households only.We have found irregular values
                                        for Puerto Rico 1990 in households with at least one member aged 60+."),
                                     # p(a("Download the complete list of countries and years included.",href="http://bancdadesced.uab.es/Households_and_living_arrengements/INVENTORY_IPUMS.csv")),
                                      h5(strong("2. LABOUR FOURCE SURVEY (LFS)")),
                                      p("We have included private households only."),
                                      p("We have selected the 2011 and 2001 samples. In 2011 there was household level information in 24 European countries. In 2001 there were 23 countries."),
                                      p("We have Lithuania 2002, Germany 2005 and Ireland 2006 as the earliest data points. 
                                         No data are available for Lithuania and Ireland before 2002 and 2006 respectively. 
                                         In the case of Germany, before 2005, there was a problem with the variables used to 
                                         link household members between them. Therefore, we decided to use Germany 2005."),
                                      p("Sweden, Norway, Denmark, Switzerland do not provide HH level information like HHLINK, HHSPOU, HHFATH, HHMOTH. 
                                         Therefore, they are excluded from the analysis."), 
                                      p("We have relatively high values for some countries in the variable HH with a female head (e.g in Latvia, Luxembourg,
                                         Ireland, Estonia, Lithuania and Ireland the values are above 0.55). This can be due to the selection of the household 
                                         head (reference person) in the LFS"),
                                      #p(a("Download the complete list of countries and years included.",href="http://bancdadesced.uab.es/Households_and_living_arrengements/INVENTORY_LFS.csv")),
                                      h5(strong("3. DEMOGRAPHIC AND HEALTH SURVEYS (DHS)")),
                                      #p(a("Download the complete list of countries and years included.",href="http://bancdadesced.uab.es/Households_and_living_arrengements/INVENTORY_DHS.csv")),
                                      br(),
                                      h5(strong("Individual level variables")),
                                    
                                      h5(strong("1. IPUMS-International (IPUMS)")),
                                      p("We have used all the IPUMS samples where persons are organized into households. Samples where persons were not organized into household were excluded."),
                                      p("We have included private households only."),
                                      p("The last age group with data for each country is the open group. For example, in Argentina 1970 the last age group is 95+."),
                                      p("We have detected a problem in the Spanish Census 2011: the original pointers show that there are are some children (0-14) with own children in the household (5-6% of the 0-14 y.o. population).  
                                        This is from the original data, we cannot do anything."),
                                     # p(a("Download the complete list of countries and years included",href="http://bancdadesced.uab.es/Households_and_living_arrengements/INVENTORY_IPUMS_INDIVIDUAL.csv")),
                                      h5(strong("2. LABOUR FOURCE SURVEY (LFS)")),
                                      p("We have included private households only."),
                                      p("We have selected the 2011 and 2001 samples. In 2011 there was household level information in 24 European countries. In 2001 there were 23 countries."),
                                      p("We have Lithuania 2002, Germany 2005 and Ireland 2006 as the earliest data points. No data are available for Lithuania and Ireland before 2002 and 2006 respectively. In the case of Germany, before 2005, there was a problem with the variables used to link household members between them. 
                                         Therefore, we decided to use Germany 2005."),
                                      p("Sweden, Norway, Denmark, Switzerland do not provide HH level information like HHLINK, HHSPOU, HHFATH, HHMOTH. Therefore, they are excluded from the analysis."),
                                      #p(a("Download the complete list of countries and years included",href="http://bancdadesced.uab.es/Households_and_living_arrengements/INVENTORY_LFS_INDIVIDUAL.csv")),
                                       h5(strong("3. DEMOGRAPHIC AND HEALTH SURVEYS (DHS)")),
                                      p("Because of their particular design (only individuals under 15 years old have a variable indicating if the parents live in the household), 
                                        we have not included DHS samples in the analysis of individual variables."),style='width: 1000px; height: 1000px'))
                   
                                       )) 
