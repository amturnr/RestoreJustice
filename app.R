#### Environment and library Loading ####

library(shiny)
library(tidyverse)
library(lubridate)
library(stringi)
library(readxl)
#library(censusapi)
#library(tigris)
library(sf)
library(DT)
#library(leaflet)
library(plotly)


### Uploading the RJ Color Scheme

# defining the color scheme for Restore Justice
mypal <- c("#213159", "#3D6098","#9fc5e8","#F04B4C","#80475E","#FEC20E", "#b4b4b4")

cvi_colours = list(
  RJ_colors = c("#213159","#3D6098","#9fc5e8","#F04B4C","#80475E","#FEC20E", "#b4b4b4")
)

cvi_palettes = function(name, n, all_palettes = cvi_colours, type = c("discrete", "continuous")) {
  palette = all_palettes[[name]]
  if (missing(n)) {
    n = length(palette)
  }
  type = match.arg(type)
  out = switch(type,
               continuous = grDevices::colorRampPalette(palette)(n),
               discrete = palette[1:n]
  )
  structure(out, name = name, class = "palette")
}


cvi_palettes("RJ_colors", type = "discrete")


scale_colour_cvi_d = function(name) {
  ggplot2::scale_colour_manual(values = cvi_palettes(name,
                                                     type = "discrete"))
}

scale_fill_cvi_d = function(name) {
  ggplot2::scale_fill_manual(values = cvi_palettes(name,
                                                   type = "discrete"))
}


# ### Read in latest IDOC data ###
# 
# 

full_IDOC <- read_rds("fullIDOC.rds")

full_IDOC <- full_IDOC %>% mutate("Age Range" = case_when(age < 18 ~ "<18",
                                                          age >18 & age<26 ~ "18 to 25",
                                                          age >=26 & age<36 ~"26 to 35",
                                                          age >=36 & age<51 ~ "36 to 50",
                                                          age >=51 & age<65 ~ "51 to 65",
                                                          age >65 ~ "over 65")) %>%
  mutate("TIS Class"= case_when(grepl("85%",`Truth in Sentencing`) ~"85%",
                                grepl("75%",`Truth in Sentencing`) ~"75%",
                                grepl("100%",`Truth in Sentencing`) ~"100%",
                                grepl("Day-for-Day",`Truth in Sentencing`) & year(`Current Admission Date`)>1998 ~"Post-TIS 50%",
                                grepl("Day-for-Day",`Truth in Sentencing`) & year(`Current Admission Date`)<=1998 ~"Pre-TIS 50%"
  ))
full_IDOC$`Sentence Years 2` <- as.numeric(full_IDOC$`Sentence Years`)



## pull OMR
OMR <- read_csv("OMR.csv")



# Load functions to clean ACS data
yeargeog <- function(year,state,vars){
  data.frame(year = year, 
             getCensus(name="acs/acs5",
                       vintage=year,
                       vars = vars,
                       key=key,
                       region = "tract:*",
                       regionin = paste0("state:",unique(fips_codes$state_code)[state])
             )
  )
  
}

fips_to_acs <- function(data){
  t <- left_join(data,fips_codes,by=c("state"="state_code","county"="county_code"))
  t
}



#### Shiny app ####


### UI (this is the visual component to the app) ###

ui <- navbarPage(
  
  ## Running header with identification of local data (link) and last update

  
  # Application title
  titlePanel(div(span(paste0("Restore Justice IDOC Data. Last Update: ", today())),
                 textOutput(outputId = "txttimeperiod"),
                 style={'padding-bottom: 35px; font-size: 10pt'},

                 
              
                 )), 

  
# Calling the Layout
tabPanel("Welcome",
         h4("Welcome to the Restore Justice Dashboard"),
         h5(" Use the data explorer to discover distributions about the IDOC population, the data select tool to select variables to get a count of people that adhere to each category, or the incidents tool to look at within institution incidents over time."),
         h5("Full information about this app and the documentation supporting it can be found at __.github.com.")
         ), 


tabPanel("Data Explorer",
         h4("Welcome to the Restore Justice Data Portal!"),
         h5("Explore data from the Illinois Department of Corrections (IDOC) Prison Stock files below by selecting a time period and maximum age at sentence at which to explore."),


  sidebarPanel(
    selectInput("timeperiod","Time Period",choices =sort(unique(full_IDOC$FileName),decreasing = TRUE)),
    numericInput("SentAge", "Individuals Sentenced before age:", value = 100),
    tableOutput("Statistics")
   # , uiOutput("ui_open_tab_button")
    #tabs
  ),
  
# Main Panel   
  mainPanel(
    # with a tabset
    tabsetPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Crime Class",
                           tabsetPanel(
                           tabPanel("Age",
                                    plotOutput("plot1"),
                                    dataTableOutput("table1"),
                                    downloadButton("download1","Download")),
                           tabPanel("Race",plotOutput("plot2"),
                                    dataTableOutput("table2"),
                                    downloadButton("download2","Download")))),
                  
                  
                  tabPanel("Sentence Date",
                           tabsetPanel(
                           tabPanel("Age", plotOutput("plot3"),
                                    dataTableOutput("table3"),
                                    downloadButton("download3","Download")),
                           tabPanel("Race",plotOutput("plot4"),
                                    dataTableOutput("table4"),
                                    downloadButton("download4","Download")))),
                  tabPanel("Length of Sentence",
                           tabsetPanel(
                             tabPanel("Age",
                                      plotOutput("plot7"),
                                      dataTableOutput("table7"),
                                      downloadButton("download7","Download")),
                             tabPanel("Race",plotOutput("plot8"),
                                      dataTableOutput("table8"),
                                      downloadButton("download8","Download")))),
                  
                  tabPanel("Truth In Sentencing", 
                           tabsetPanel(
                           tabPanel("Age", plotOutput("plot5"),
                                    dataTableOutput("table5"),
                                    downloadButton("download5","Download")),
                           tabPanel("Race",plotOutput("plot6"),
                                    dataTableOutput("table6"),
                                    downloadButton("download6","Download")))),
                  tabPanel("Time Remaining", 
                           tabsetPanel(
                             tabPanel("Age", plotOutput("plot9"),
                                      dataTableOutput("table9"),
                                      downloadButton("download9","Download")),
                             tabPanel("Race",plotOutput("plot10"),
                                      dataTableOutput("table10"),
                                      downloadButton("download10","Download")))),
                  
                  )
      ),

    
  ),


),

tabPanel("Data Select Tool",
sidebarPanel(         
  tabsetPanel(
    
    selectInput("timeperiod2","Time Period",choices =sort(unique(full_IDOC$FileName),decreasing = TRUE)),
    numericInput("age", "Sentenced Below age:", value = 50),
    selectInput("race","Race:",choices =unique(full_IDOC$Race),multiple=T,selectize = T#,selected = c(unique(full_IDOC$Race))
                ),
    selectInput("sex","Sex:",choices =unique(full_IDOC$Sex),multiple=T,selectize = T #,selected = c(unique(full_IDOC$Sex))
                ),
    selectInput("crime_class","Crime Class:",choices =unique(full_IDOC$`Crime Class`),multiple = T, selectize = T #,selected =c(unique(full_IDOC$`Crime Class`)) 
                ),
    # selectInput("holding_offense","Holding Offense:",choices =unique(full_IDOC$`Holding Offense`),multiple = T ),
    # selectInput("holding_off_cat","Holding Offense Category:",choices =unique(full_IDOC$`Holding Offense`),multiple=T,selectize = T,selected=c(unique(full_IDOC$`Holding Offense`))),
    selectInput("offense_type","Offense Type:",choices =unique(full_IDOC$`Offense Type`),multiple=T,selectize = T #,selected = c(unique(full_IDOC$`Offense Type`))
                ),
    selectInput("t_in_s","Truth in Sentencing:",choices =unique(full_IDOC$`Truth in Sentencing`),multiple = T,selectize = T #,selected = c(unique(full_IDOC$`Truth in Sentencing`))
                ),
    sliderInput("sentence_yrs","Sentence Years:",min=0,max=100,value=c(0,100)), #should be slider
    selectInput("life","Life Sentence:",choices = c(unique(full_IDOC$life)),multiple = T,selectize = T ),
    # sliderInput("sentence_months","Sentence Months:",choices =sort(unique(full_IDOC$FileName),decreasing = TRUE)), #should be slider
    sliderInput("sentence_date","Sentence Date:",min=as.Date("1903-03-31"), max=as.Date("2109-06-05"),value=c(as.Date("1903-03-31"),as.Date("2109-06-05"))), #should be slider
    sliderInput("proj_msr","Projected MSR Date:",min=as.Date("1939-06-03"), max=as.Date("9999-12-30"),value=c(as.Date("1939-06-03"),as.Date("9999-12-30") )), #should be slider
    sliderInput("proj_dis","Projected Discharge Date",min=as.Date("1924-08-23"), max=as.Date("9999-12-30"),value=c(as.Date("1924-08-23"),as.Date("9999-12-30")) ), #should be slider
    selectInput("county","County:",choices =unique(full_IDOC$`Sentencing County`),multiple=T, selectize = T  , selected = c(unique(full_IDOC$`Sentencing County`))
                ), 
    selectInput("inst","Institution:",choices =unique(full_IDOC$`Parent Institution`),multiple=T,selectize = T ,selected = c(unique(full_IDOC$`Parent Institution`))
                )
  )),

mainPanel(actionButton("go", "Populate Table", style="background-color:#F04B4C"),
          dataTableOutput("SVS"),
          h5("Table must be populated before downloading data."),
          downloadButton("downloadSVS")
)
)
,



tabPanel("Incidents Tool",
         mainPanel(
           selectInput(inputId = "OMRvar",label = "Select Incident Type:",choices = c(unique(OMR$`Incident Type`))),
           plotlyOutput("OMR"),
           dataTableOutput("OMR_table")
           #downloadButton("download_OMR","Download")
         )),

# tabPanel("Geographic Explorer",
#          mainPanel(
#            leafletOutput("ILmap")
#            
#          )),

#downloadButton("downloadData", "Download"),
tags$img(src="RJLogo.png",align="right"),
tags$img(src="MTLogo.png",align="bottomright"),
h6("For questions or issues with the app contact amturnr@gmail.com or see our github at https://github.com/MetricsTogether/RestoreJustice"),

tags$style(HTML(".navbar-header { width:100% }
                   .navbar-brand { width: 100%; text-align: center }")) # center text

)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  output$ui_open_tab_button <- renderUI({
    shiny::a(
      h4(paste0("Run Report: ",input$timeperiod),
         class = "btn btn-default action-button",
         style = "fontweight:600"),
      target = "_blank",
      href = paste0("https://www.metricstogether.com/restore-justice-",input$timeperiod)
    )
  })
  output$runreport <- renderUI({
    tags$iframe(src=paste0("https://www.metricstogether.com/restore-justice-",input$timeperiod), height=1000, width="100%")

  })

full_IDOC$`Sentence Years` <- as.numeric(full_IDOC$`Sentence Years`)
  
stock_sent <- reactive({full_IDOC %>% 
    filter(FileName == input$timeperiod, `Sentence Age`<input$SentAge) %>% 
    mutate("Sentence Year"=year(`Sentence Date`))%>% 
    mutate("Sentence Years2"= case_when(`Sentence Year`>100 ~ 100,                   
                                        `Sentence Year`<100 ~ `Sentence Year`)) %>%
    mutate("Time to Release"=ifelse(is.na(`Projected Discharge Date`),NA ,(`Projected Discharge Date`-today())/365))
    }) 

                              

  
output$txttimeperiod <- renderText({input$timeperiod})

#  possible alternatives (graphs, statistics, and the ability to pull raw data )  

stock_sentII <- reactive({full_IDOC %>% 
    filter(FileName == input$timeperiod2, 
           `Sentence Age`<input$age,
           Sex %in% c(input$sex),
           `Crime Class` %in% c(input$crime_class),
           #`Holding Offense` %in% c(input$holding_offense),
           #`Holding Offense Category` %in% c(input$holding_off_cat),
           `Offense Type` %in% c(input$offense_type),
           `Truth in Sentencing` %in% c(input$t_in_s),
           between(`Sentence Years 2`,input$sentence_yrs[1],input$sentence_yrs[2]),
           `life` %in% input$life,
           between(`Sentence Date`,input$sentence_date[1],input$sentence_date[2]),
           between(`Projected Mandatory Supervised Release (MSR) Date`,input$proj_msr[1],input$proj_msr[2]),
           between(`Projected Discharge Date`,input$proj_dis[1],input$proj_dis[2]),
           `Sentencing County` %in% input$county,
           `Parent Institution` %in% input$inst
           
    ) %>% 
    mutate("Sentence Year"=year(`Sentence Date`))%>% 
    mutate("Sentence Years2"= case_when(`Sentence Year`>100 ~ 100,                   
                                        `Sentence Year`<100 ~ `Sentence Year`))})

# randomVals <- eventReactive(input$go, {
#   runif(input$n)
# })
# 
# output$plot <- renderPlot({
#   hist(randomVals())
# })

mySVS <- eventReactive(input$go, {
    svs <- tibble(
      "Number of People" = length(unique(stock_sentII()$`IDOC #`)),
           "Selection Average Age" = round(mean(stock_sentII()$age,na.rm=TRUE),2),
           "Percent Black" = round((length(stock_sentII()$Race[stock_sentII()$Race=="Black" & stock_sentII()$`Sentence Age`<input$SentAge])/length(stock_sentII()$Race))*100,2),
           "Percent Male" = round((length(stock_sentII()$Sex[stock_sentII()$Sex == "Male"& stock_sentII()$`Sentence Age`<input$SentAge])/length(stock_sentII()$Sex))*100,2)
      )
  print(svs)
    })

output$SVS <- renderDataTable({
  mySVS()
  })


output$downloadSVS <- downloadHandler(
  filename = function(){"SentenceData.csv"}, 
  content = function(fname){
    write.csv(stock_sentII(),
              fname)
  }
)

  
  output$plot1 <- renderPlot({
    plot1 <- ggplot()+
      geom_bar(mapping=aes(x=stock_sent()$`Crime Class`,fill=stock_sent()$`Age Range`))+
      labs(title="People Committed by Crime Class", x="Crime Class",fill="Age Range")+scale_fill_cvi_d("RJ_colors")
    print(plot1)
  }) 
  output$table1 <- renderDataTable({
    stock_sent() %>% 
      group_by(`Age Range`,`Crime Class`) %>% 
      summarize(n()) %>%
      pivot_wider(names_from = `Age Range`, values_from = `n()`)},
    filter = 'top'
  )

  output$download1 <- downloadHandler(
    filename = function(){"SentenceData.csv"}, 
    content = function(fname){
      write.csv(stock_sent() %>%  
                  group_by(`Age Range`,`Crime Class`) %>% 
                  summarize(n()) %>%
                  pivot_wider(names_from = `Age Range`, values_from = `n()`),
                fname)
    }
  )

  output$plot2 <- renderPlot({
   plot2 <-  ggplot()+
      geom_bar(mapping=aes(x=stock_sent()$`Crime Class`,
                           fill=stock_sent()$`Race`))+
      labs(title="People Committed by Crime Class", x="Crime Class",fill="Race")+scale_fill_cvi_d("RJ_colors")
   print(plot2)
  })
  output$table2 <- renderDataTable({
    stock_sent() %>%
      group_by(`Race`,`Crime Class`) %>%
      summarize(n()) %>%
      pivot_wider(names_from = `Race`, values_from = `n()`)},
    filter = 'top'
  )

  output$download2 <- downloadHandler(
    filename = function(){"SentenceData.csv"},
    content = function(fname){
      write.csv(stock_sent() %>%
                  group_by(`Race`,`Crime Class`) %>%
                  summarize(n()) %>%
                  pivot_wider(names_from = `Race`, values_from = `n()`),
                fname)
    }
  )


  output$plot3 <- renderPlot({
    plot3 <- ggplot()+
      geom_histogram(mapping=aes(x=stock_sent()$`Sentence Year`,
                                 fill=stock_sent()$`Age Range`))+
      labs(title="People sentenced per year", x="Sentence Year",fill="Age Range")+scale_fill_cvi_d("RJ_colors")
    print(plot3)
  })
  output$table3 <- renderDataTable({
    stock_sent() %>% mutate("Sentence Year"=year(`Sentence Date`)) %>%
            group_by(`Age Range`,`Sentence Year`) %>%
      summarize(n()) %>%
      pivot_wider(names_from = `Age Range`, values_from = `n()`) %>%
      arrange(-`Sentence Year`)},
    filter = 'top'
  )

  output$download3 <- downloadHandler(
    filename = function(){"SentenceData.csv"},
    content = function(fname){
      write.csv(stock_sent() %>% mutate("Sentence Year"=year(`Sentence Date`)) %>%
                  group_by(`Age Range`,`Sentence Year`) %>%
                  summarize(n()) %>%
                  pivot_wider(names_from = `Age Range`, values_from = `n()`) %>%
                  arrange(-`Sentence Year`),
                fname)
    }
  )

 output$plot4 <- renderPlot({
   plot4 <- ggplot()+
     geom_histogram(mapping=aes(x=stock_sent()$`Sentence Year`,
                                fill=stock_sent()$`Race`))+
     labs(title="People sentenced per year", x="Sentence Year",fill="Race")+scale_fill_cvi_d("RJ_colors")
   print(plot4)
 })

 output$table4 <- renderDataTable({
   stock_sent() %>%
     filter(`Sentence Age`<input$SentAge) %>%
     group_by(`Race`,`Sentence Year`) %>%
     summarize(n()) %>%
     pivot_wider(names_from = `Race`, values_from = `n()`) %>%
     arrange(-`Sentence Year`)},
 filter = 'top'
 )

 output$download4 <- downloadHandler(
   filename = function(){"SentenceData.csv"},
   content = function(fname){
     write.csv(stock_sent() %>%
                 filter(`Sentence Age`<input$SentAge) %>%
                 group_by(`Race`,`Sentence Year`) %>%
                 summarize(n()) %>%
                 pivot_wider(names_from = `Race`, values_from = `n()`) %>%
                 arrange(-`Sentence Year`),
               fname)
   }
 )

  output$plot5 <- renderPlot({
    plot5 <- ggplot()+
      geom_bar(mapping=aes(x=stock_sent()$`TIS Class`,
                                 fill=stock_sent()$`Age Range`))+
      labs(title="People per TIS Class", x="TIS Class",fill="Age Range")+scale_fill_cvi_d("RJ_colors")  
    print(plot5)})

  output$table5 <- renderDataTable({
    stock_sent() %>%
      group_by(`Age Range`,`TIS Class`) %>%
      summarize(n()) %>%
      pivot_wider(names_from = `Age Range`, values_from = `n()`)},
    filter = 'top'
  )

  output$download5 <- downloadHandler(
    filename = function(){"SentenceData.csv"},
    content = function(fname){
      write.csv( stock_sent() %>%
                   group_by(`Age Range`,`TIS Class`) %>%
                   summarize(n()) %>%
                   pivot_wider(names_from = `Age Range`, values_from = `n()`),
                fname)
    }
  )

    output$plot6 <- renderPlot({
      plot6 <- ggplot()+
        geom_bar(mapping=aes(x=stock_sent()$`TIS Class`,
                                   fill=stock_sent()$`Race`))+
        labs(title="People per TIS Class", x="TIS Class",fill="Race")+scale_fill_cvi_d("RJ_colors")
      print(plot6)})



 output$table6 <- renderDataTable({
   stock_sent() %>%
     group_by(`Race`,`TIS Class`) %>%
     summarize(n()) %>%
     pivot_wider(names_from = `Race`, values_from = `n()`)},
   filter = 'top'
 )

output$download6 <- downloadHandler(
  filename = function(){"SentenceData.csv"},
  content = function(fname){
    write.csv(  stock_sent() %>%
                  group_by(`Race`,`TIS Class`) %>%
                  summarize(n()) %>%
                  pivot_wider(names_from = `Race`, values_from = `n()`),
               fname)
  }
)
    output$plot7 <- renderPlot({
      plot7 <- ggplot()+
        geom_bar(mapping=aes(x=stock_sent()$`Sentence Years2`,
                             fill=stock_sent()$`Age Range`))+
        labs(title="Years Sentenced", x="Sentence Length (Years)",fill="Age Range")+scale_fill_cvi_d("RJ_colors") 
      print(plot7)})

    output$table7 <- renderDataTable({
      stock_sent() %>%
        filter(`Sentence Age`<input$SentAge) %>%
        group_by(`Age Range`,`Sentence Years2`) %>%
        summarize(n()) %>%
        pivot_wider(names_from = `Age Range`, values_from = `n()`) %>%
        arrange(`Sentence Years2`)},
      filter = 'top'
    )

    output$download7 <- downloadHandler(
      filename = function(){"SentenceData.csv"},
      content = function(fname){
        write.csv(  stock_sent() %>%
                      filter(`Sentence Age`<input$SentAge) %>%
                      group_by(`Age Range`,`Sentence Years2`) %>%
                      summarize(n()) %>%
                      pivot_wider(names_from = `Age Range`, values_from = `n()`) %>%
                      arrange(`Sentence Years2`),
                    fname)
      }
    )

    output$plot8 <- renderPlot({
      plot8 <- ggplot()+
        geom_bar(mapping=aes(x=stock_sent()$`Sentence Years2`,
                             fill=stock_sent()$`Race`))+
        labs(title="Years Sentenced", x="Sentence Length (Years)",fill="Race")+scale_fill_cvi_d("RJ_colors")
      print(plot8)})

    output$table8 <- renderDataTable({
      stock_sent() %>% mutate("Sentence Year"=year(`Sentence Date`)) %>%
        filter(`Sentence Age`<input$SentAge) %>%
        group_by(`Race`,`Sentence Years2`) %>%
        summarize(n()) %>%
        pivot_wider(names_from = `Race`, values_from = `n()`) %>%
        arrange(`Sentence Years2`)},
      filter = 'top'
    )

    output$download8 <- downloadHandler(
      filename = function(){"SentenceData.csv"},
      content = function(fname){
        write.csv(  stock_sent() %>% mutate("Sentence Year"=year(`Sentence Date`)) %>%
                      filter(`Sentence Age`<input$SentAge) %>%
                      group_by(`Race`,`Sentence Years2`) %>%
                      summarize(n()) %>%
                      pivot_wider(names_from = `Race`, values_from = `n()`) %>%
                      arrange(`Sentence Years2`),
                    fname)
      }
    )
    
    
    output$plot9 <- renderPlot({
      plot9 <- stock_sent() %>% filter(`Time to Release`<50)%>%
        ggplot()+
        geom_bar(mapping=aes(x=`Time to Release`,
                             fill=`Age Range`))+
        labs(title="Years Sentenced", x="Sentence Length (Years)",fill="Age Range")+scale_fill_cvi_d("RJ_colors") 
      print(plot9)})
    
    output$table9 <- renderDataTable({
      stock_sent() %>% 
        filter(`Sentence Age`<input$SentAge) %>%
        group_by(`Race`,`Projected Discharge Date`) %>%
        summarize(n()) %>%
        pivot_wider(names_from = `Race`, values_from = `n()`) %>%
        arrange(`Projected Discharge Date`)},
      filter = 'top'
    )
    
    output$download9 <- downloadHandler(
      filename = function(){"SentenceData.csv"},
      content = function(fname){
        write.csv(  stock_sent() %>% mutate("Sentence Year"=year(`Sentence Date`)) %>%
                      filter(`Sentence Age`<input$SentAge) %>%
                      group_by(`Race`,`Sentence Years2`) %>%
                      summarize(n()) %>%
                      pivot_wider(names_from = `Race`, values_from = `n()`) %>%
                      arrange(`Sentence Years2`),
                    fname)
      }
    )
    
     output$plot10 <- renderPlot({
      plot10 <-  stock_sent() %>% filter(`Time to Release`<50)%>%
         ggplot()+
         geom_bar(mapping=aes(x=`Time to Release`,
                              fill=`Race`))+
         labs(title="Years Sentenced", x="Sentence Length (Years)",fill="Race")+scale_fill_cvi_d("RJ_colors") 
       print(plot10)})

    output$table10 <- renderDataTable({
      stock_sent() %>% mutate("Sentence Year"=year(`Sentence Date`)) %>%
        filter(`Sentence Age`<input$SentAge) %>%
        group_by(`Race`,`Sentence Years2`) %>%
        summarize(n()) %>%
        pivot_wider(names_from = `Race`, values_from = `n()`) %>%
        arrange(`Sentence Years2`)},
      filter = 'top'
    )

    output$download10 <- downloadHandler(
      filename = function(){"SentenceData.csv"},
      content = function(fname){
        write.csv(  stock_sent() %>% mutate("Sentence Year"=year(`Sentence Date`)) %>%
                      filter(`Sentence Age`<input$SentAge) %>%
                      group_by(`Race`,`Sentence Years2`) %>%
                      summarize(n()) %>%
                      pivot_wider(names_from = `Race`, values_from = `n()`) %>%
                      arrange(`Sentence Years2`),
                    fname)
      }
    )   
    
    
  output$Statistics <-renderTable(
    tibble("Number of People" = length(unique(stock_sent()$`IDOC #`)),
          "IDOC Age" = mean(stock_sent()$age,na.rm=TRUE),
           "IDOC Percent Black" = (length(stock_sent()$Race[stock_sent()$Race=="Black" & stock_sent()$`Sentence Age`<input$SentAge])/length(stock_sent()$Race))*100,
           "IDOC Percent Male" = (length(stock_sent()$Sex[stock_sent()$Sex == "Male"& stock_sent()$`Sentence Age`<input$SentAge])/length(stock_sent()$Sex))*100)

  )





  output$data <- renderDataTable({
    vals <- c(input$selection) 

    #stock_sent()$`Birth Date` <- as.character(stock_sent()$`Birth Date`)
    stock_sent()$`Current Admission Date`<- as.character(stock_sent()$`Current Admission Date`)
    stock_sent()$`Projected Mandatory Supervised Release Date`<- as.character(stock_sent()$`Projected Mandatory Supervised Release Date`)
    stock_sent()$`Projected Discharge Date` <- as.character(stock_sent()$`Projected Discharge Date`)
    #stock_sent()$`Custody Date` <- as.character(stock_sent()$`Custody Date`)
    stock_sent()$`Sentence Date` <- as.character(stock_sent()$`Sentence Date`)


    stock_sent() %>%
      dplyr::filter(between(`Custody Age`,vals[1],vals[2]))

  }
  )



output$OMR <- renderPlotly({
  OMR$date <- mdy(OMR$date)
  Type_sum <- OMR %>% 
    group_by(Prison.Type,`Incident Type`,date) %>% 
    summarize("Number of Incidents"=sum(Incidents)) %>%
    filter(`Incident Type`==input$OMRvar)  %>% 
    pivot_wider(names_from = Prison.Type,values_from = `Number of Incidents`)%>%
    arrange(date)

plot_ly(Type_sum, x = ~date, y = ~`Maximum Security`, type = 'scatter', mode = 'lines', name="Maximum Security",line=list(color=mypal[1])) %>%
   add_trace(y=~`Medium Security`,name="Medium Security",line=list(color=mypal[4]))%>%
   add_trace(y=~`Minimum Security`,name="Minimum Security",line=list(color=mypal[6])) %>% 
   layout(#title = "Average High and Low Temperatures in New York",
                        xaxis = list(title = "Date"),
                        yaxis = list (title = "Number of Incidents"))

})

output$OMR_table <- renderDataTable({
 OMR %>% filter(`Incident Type`==input$OMRvar)%>%
      group_by(Prison.Type,date) %>%
      summarize("Number of Incidents"=sum(Incidents,na.rm = TRUE))}
)

# output$download_OMR <- downloadHandler(
#   filename =function(){"OMRData.csv"},
#   content = function(){ write.csv(OMR%>%  
#                 group_by(Prison.Type,`Incident Type`,date) %>% 
#                 summarize("Number of Incidents"=sum(Incidents)) %>%
#                 filter(`Incident Type`==input$OMRvar)  %>% 
#                 pivot_wider(names_from = Prison.Type,values_from = `Number of Incidents`))}
# )



#IL_counties <- read_rds("IL_counties.rds")

# IDOC_geog <- full_IDOC %>% filter(year(FileName)==year(today())) %>% 
#   group_by(`Sentencing County`) %>% 
#   summarize("Number of incarcerated individuals" = n(),
#             "Percent of incarcerated population who is black" = length(Race=="Black"),
#             "Percent of incarcerated population sentenced before age 26" =length(`Sentence Age`<26))
# 
# full_IDOC %>% filter(year(FileName)==year(today())) %>% 
#   group_by(`Sentencing County`,race) %>%
#   mutate(countT= sum(count)) %>%
#   group_by(type, add=TRUE) %>%
#   mutate(per=paste0(round(100*count/countT,2),'%'))
# 
# IL_counties <- left_join(IL_counties,IDOC_geog,by=c("county"="Sentencing County"))

# output$ILmap <- renderLeaflet({
#   leaflet(IL_counties) %>% 
#     addPolygons(color = mypal[2],
#                 weight = 1,
#                 smoothFactor = .5,
#                 opacity = 1,
#                 fillOpacity = .5,
#                 label = "123")
  # ggplot()+
  #   geom_sf(data=IL_counties,fill="white")+
  #   theme_classic()+
  #   theme(axis.line=element_blank(),axis.text.x=element_blank(),
  #         axis.text.y=element_blank(),axis.ticks=element_blank(),
  #         axis.title.x=element_blank(),
  #         axis.title.y=element_blank())+
  #   labs(fill="")
#})




  output$downloadData <- downloadHandler({
    filename = function() {
      paste("IDOC", stock_date, ".csv", sep = "")
    }

    content = function(file) {
      write.csv(output$data, file, row.names = FALSE)
    }
  })


  
}


# Run the application 
shinyApp(ui = ui, server = server)
