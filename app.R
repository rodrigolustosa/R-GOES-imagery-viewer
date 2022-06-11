# ----------------------------------------------------------------------- #
# Information: 
# Created by: Rodrigo Lustosa
# Creation date:  9 Jun 2022 14:10 (GMT -03)
# ----------------------------------------------------------------------- #

# packages
library(shiny)
library(ncdf4)
library(tidyverse)
library(lubridate)
library(RCurl)

# directory and file names
dir_data <- "data"
file_functions <- "R/functions.R"

# initializations
app_version <- "Version 0.0.0"
source(file_functions)


ui <- fluidPage(
  # fixing checkbox alignment
  tags$head(
    tags$style(
      HTML(
        ".checkbox-inline { 
                    margin-left: 0px;
                    margin-right: 10px;
          }
         .checkbox-inline+.checkbox-inline {
                    margin-left: 0px;
                    margin-right: 10px;
          }
        "
      )
    ) 
  ),
  # header
  h1("GOES Imagery Viewer"),
  # Tabs
  tabsetPanel(
    # Viewer
    tabPanel("Viewer [under develop.]",
             sidebarLayout(sidebarPanel(
               textInput("area_name",label = "Area name",value = "América do Sul"),
               selectInput("source","Data Source",choices = list("CPTEC/INPE" = 1)),
               selectInput("goes","Satellite",choices = list("GOES 16" = 16)),
               fluidRow(
                 column(6,selectInput("type","Data type",choices = list("Chanels"         = 1,
                                                                        "RGB compositions"= 2))),
                 column(6,
                        conditionalPanel("input.type == 1",
                                         selectInput("chanel","Chanel",selected = 13,
                                                     choices = list("CH01" =  1,
                                                                    "CH02" =  2,
                                                                    "CH03" =  3,
                                                                    "CH04" =  4,
                                                                    "CH05" =  5,
                                                                    "CH06" =  6,
                                                                    "CH07" =  7,
                                                                    "CH08" =  8,
                                                                    "CH09" =  9,
                                                                    "CH10" = 10,
                                                                    "CH11" = 11,
                                                                    "CH12" = 12,
                                                                    "CH13" = 13,
                                                                    "CH14" = 14,
                                                                    "CH15" = 15,
                                                                    "CH16" = 16)
                                                     )),
                        conditionalPanel("input.type == 2",
                                         selectInput("comp","Composition",
                                                     choices = list("Air Mass"  = 1))))
                 ),
               dateRangeInput("dates","Date Range"),
               checkboxInput("show_time",label = "Show time selection",value = FALSE),
               conditionalPanel("input.show_time == 1",
                                fluidRow(
                                  column(6,
                                         checkboxGroupInput("hours",label = "Hours",#width = "50%",
                                                            selected = 0:24,inline = TRUE,
                                         choices = list("00" =  0, "01"=  1,"02"=  2,"03"=  3,
                                                        "04" =  4, "05"=  5,"06"=  6,"07"=  7,
                                                        "08" =  8, "09"=  9,"10"= 10,"11"= 11,
                                                        "12" = 12, "13"= 13,"14"= 14,"15"= 15,
                                                        "16" = 16, "17"= 17,"18"= 18,"19"= 19,
                                                        "20" = 20, "21"= 21,"22"= 22,"23"= 23,
                                                        "select all" = 24,"remove all" = 25))),
                                  column(6,
                                         checkboxGroupInput("min",label = "Minutes",#width = "50%",
                                                            selected = 0:5,#inline = TRUE,
                                                            choices = list("00" = 0, "10" = 1,
                                                                           "20" = 2, "30" = 3,
                                                                           "40" = 4, "50" = 5)))
                                  )
                                )
               ),
               mainPanel()
               ),
             helpText(app_version)
             ),
    
    # Download Tab
    tabPanel("Download Data",
             helpText(paste("Note: Viewer already downloads data if it is not already available.",
                            "This tab was made for cases where you want to view many images,",
                            "so you can download it before anything, making the Viewer faster.",
                            "It also works for cases where you just want to download data to use elsewhere.")),
             fluidRow(column(4,
                             selectInput("source_d","Data Source",choices = list("CPTEC/INPE" = 1)),
                             selectInput("goes_d","Satellite",choices = list("GOES 16" = 16)),
                             checkboxGroupInput("type_d","Data type",choices = list("Chanels"         = 1,
                                                                                    "RGB compositions [under develop.]"= 2)),
                             conditionalPanel("input.type_d.includes('1')",
                                              checkboxGroupInput("chanel_d","Chanel",inline = TRUE,#selected = 13,
                                                                 choices = list("CH01" =  1,
                                                                                "CH02" =  2,
                                                                                "CH03" =  3,
                                                                                "CH04" =  4,
                                                                                "CH05" =  5,
                                                                                "CH06" =  6,
                                                                                "CH07" =  7,
                                                                                "CH08" =  8,
                                                                                "CH09" =  9,
                                                                                "CH10" = 10,
                                                                                "CH11" = 11,
                                                                                "CH12" = 12,
                                                                                "CH13" = 13,
                                                                                "CH14" = 14,
                                                                                "CH15" = 15,
                                                                                "CH16" = 16))
                             ),
                             conditionalPanel("input.type_d.includes('2')",
                                              checkboxGroupInput("comp_d","Composition",inline = TRUE,
                                                                 choices = list("Air Mass"  = 1)))
             ),
             column(4,
                    dateRangeInput("dates_d","Date Range"),
                    checkboxInput("show_time_d",label = "Show time selection",value = FALSE),
                    conditionalPanel("input.show_time_d == 1",
                                     fluidRow(
                                       column(6,
                                              checkboxGroupInput("hours_d",label = "Hours",#width = "50%",
                                                                 selected = 0:24,inline = TRUE,
                                               choices = list("00" =  0, "01"=  1,"02"=  2,"03"=  3,
                                                              "04" =  4, "05"=  5,"06"=  6,"07"=  7,
                                                              "08" =  8, "09"=  9,"10"= 10,"11"= 11,
                                                              "12" = 12, "13"= 13,"14"= 14,"15"= 15,
                                                              "16" = 16, "17"= 17,"18"= 18,"19"= 19,
                                                              "20" = 20, "21"= 21,"22"= 22,"23"= 23,
                                                              "select all" = 24,"remove all" = 25))),
                                       column(6,
                                              checkboxGroupInput("min_d",label = "Minutes",#width = "50%",
                                                                 selected = 0:5,#inline = TRUE,
                                                                 choices = list("00" = 0, "10" = 1,
                                                                                "20" = 2, "30" = 3,
                                                                                "40" = 4, "50" = 5)))
                                     )
                    )
             )),
             actionButton("download","Download"),
             # actionButton("cancel_d","Cancel"),
             # textOutput("test"),
             br(),
             br(),
             helpText(app_version)
    )
  )
)


server <- function(input, output) {

  # initializations ---------------------------------------------------------
  input_hours <- reactiveValues(last=0:24,last_d=0:24)
  
   
  # Viewer Tab --------------------------------------------------------------
  
  # select all or remove all hours
  observe({
    added   <-      input$hours[!(input$hours %in% input_hours$last)]
    removed <- input_hours$last[!(input_hours$last %in% input$hours)]
    # when new box was added to hours
    if (length(added) > 0){
      # if 'remove all' was added
      if(25 %in% added)
        updateCheckboxGroupInput(inputId = "hours",selected = 25)
      # if 'select all' was added
      if(24 %in% added)
        updateCheckboxGroupInput(inputId = "hours",selected = 0:24)
      # if any hour was added and 'remove all' box is selected
      if(any(0:23 %in% added) & (25 %in% input$hours))
        updateCheckboxGroupInput(inputId = "hours",
                                 selected = input$hours[input$hours != 25])
    }
    # when new box was removed to hours
    if (length(removed) > 0){
      # if any hour was removed and 'select all' box is selected
      if(24 %in% input$hours & !(25 %in% removed))
        updateCheckboxGroupInput(inputId = "hours",
                                 selected = input$hours[input$hours != 24])
    }
    # save last input hour
    input_hours$last <- input$hours
  })
  
  
  

  # Downloader Tab ----------------------------------------------------------
  # select all or remove all hours
  observe({
    added   <-      input$hours_d[!(input$hours_d %in% input_hours$last_d)]
    removed <- input_hours$last_d[!(input_hours$last_d %in% input$hours_d)]
    # when new box was added to hours
    if (length(added) > 0){
      # if 'remove all' was added
      if(25 %in% added)
        updateCheckboxGroupInput(inputId = "hours_d",selected = 25)
      # if 'select all' was added
      if(24 %in% added)
        updateCheckboxGroupInput(inputId = "hours_d",selected = 0:24)
      # if any hour was added and 'remove all' box is selected
      if(any(0:23 %in% added) & (25 %in% input$hours_d))
        updateCheckboxGroupInput(inputId = "hours_d",
                                 selected = input$hours_d[input$hours_d != 25])
    }
    # when new box was removed to hours
    if (length(removed) > 0){
      # if any hour was removed and 'select all' box is selected
      if(24 %in% input$hours_d & !(25 %in% removed))
        updateCheckboxGroupInput(inputId = "hours_d",
                                 selected = input$hours_d[input$hours_d != 24])
    }
    # save last input hour
    input_hours$last_d <- input$hours_d
  })
  
  
  observeEvent(input$download,{
    download_cptec_data(ymd(input$dates_d),as.numeric(input$hours_d),
                        as.numeric(input$min_d)*10,as.numeric(input$chanel_d),
                        dir_data=file.path(dir_data,"CPTEC/GOES16"))
  })
  
  
  
  output$test <- renderText({input$download})
  
}

shinyApp(ui, server)


