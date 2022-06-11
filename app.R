# ----------------------------------------------------------------------- #
# Information:
# Created by: Rodrigo Lustosa
# Creation date:  9 Jun 2022 14:10 (GMT -03)
# ----------------------------------------------------------------------- #

# packages
library(shiny)
library(tidyverse)
library(lubridate)
library(RCurl)
library(ncdf4)
library(raster)
library(fields)

# directory and file names
dir_data <- "data"
file_functions <- "R/functions.R"
file_objects <- "R/objects.R"

# initializations
app_version <- "Version 0.0.0"
licence <- "Copyright (c) 2022 Rodrigo Lustosa"
source(file_functions)
source(file_objects)


ui <- fluidPage(
  # fixing checkbox alignment. solution from: https://stackoverflow.com/a/46493086/14874374
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
               dateRangeInput("dates","Date Range",start = "2022-06-10",end = "2022-06-10"
                                ),
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
                                                            selected = 0,#inline = TRUE,
                                                            choices = list("00" = 0, "10" = 1,
                                                                           "20" = 2, "30" = 3,
                                                                           "40" = 4, "50" = 5)))
                                  )
                                )
               ),
               mainPanel(
                 plotOutput("plot"),
                 sliderInput("image",label = "Image",animate = TRUE,
                             min = 1,max = 1,step = 1,value=1),
                 # sliderInput("image",label = "Actual date and time",
                 #             min = ymd_h("20200101 01"),
                 #             max = ymd_h("20200101 02"),step = as.difftime(10, units = "mins"),
                 #             value=ymd_h("20200101 02"),animate = TRUE),
                 # textOutput("test")
               )
               )

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
             # br(),
             # br(),
             # helpText(app_version)
    )
  ),
  hr(),
  fluidRow(column(6,helpText(licence)),
           column(6,helpText(app_version), align = 'right')
  )
)


server <- function(input, output) {

  # initializations ---------------------------------------------------------
  input_hours <- reactiveValues(last=0:24,last_d=0:24)


  # Viewer Tab --------------------------------------------------------------

  # 'select all' or 'remove all' hours
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



  ch_txt_1 <- reactive({str_c("ch" ,formatC(input$chanel,width = 2,flag=0))})
  ch_txt_2 <- reactive({str_c("ch_",formatC(input$chanel,width = 2,flag=0))})
  chanel_code <- reactive({chanels_code[[which(names(chanels_code) == ch_txt_2())]]})
  hours <- reactive({input$hours[!(input$hours %in% 24:25)]})
  imagery_names <- reactive({cptec_imagery_names(ymd(input$dates),as.numeric(hours()),
                                                 as.numeric(input$min)*10)})
  datetimes <- reactive({ymd_hm(imagery_names())})
  files_names <- reactive({str_c(chanel_code(),"_",imagery_names(),".nc")})
  file_paths <- reactive({str_c("data/CPTEC/GOES16/",ch_txt_1(),"/",files_names())})
  n_files <- reactive({length(imagery_names())})


  observe({
    n <- n_files()
    updateSliderInput(inputId = "image", max = n,value = n)
    ### datetimes <- ymd_hm(imagery_names())
    # if(n > 1){
    #   ##### interval <- datetimes[n] - datetimes[n-1]
    #   intervals <- datetimes()[2:n] - datetimes()[1:n-1]
    #   max_interval <- max(intervals)
    #   final_interval <- max_interval
    #   max_datetime <- datetimes()[n]
    #   min_datetime_real <- datetimes()[1]
    #   prov_dates <- seq(max_datetime,min_datetime_real,-max_interval)
    #   min_datetime <- min(prov_dates)
    #   if(any(!(prov_dates %in% datetimes()))){
    #     final_interval <- as.difftime(1, units = "days")
    #     prov_dates <- seq(max_datetime,min_datetime_real,-final_interval)
    #     min_datetime <- min(prov_dates)
    #   }
    # } else {
    #   final_interval <- as.difftime(1, units = "days")
    #   min_datetime <- max_datetime <- datetimes()[n]
    # }
    #
    # updateSliderInput(inputId = "image",step = final_interval,
    #                   min = min_datetime, max = max_datetime)
  })

  goes_data <- reactive({
    read_netcdf_data(file_paths()[input$image])
  })
  paleta <- pal.TbINPE
  divisoes <- val.TbINPE
  zlim <- c(-90, 55)

  output$plot <- renderPlot({
    # par(mar = c(10,10,10,10))
    image.plot(z = goes_data()$values/100 - 273.15,
               x = goes_data()$lon,
               y = goes_data()$lat,
               col = paleta,
               breaks = divisoes,
               # main = pula_linha(title),
               xlab = "Longitude",
               ylab = "Latitude",
               xlim = c(-80,-60),
               ylim = c(-20,0),
               zlim = zlim)
  }) %>% bindCache(input$dates,input$hours,input$min,input$image,input$chanel)


  # output$test <- renderText({input$image})
  # output$test <- renderText({goes_data()$lat})
  # output$test <- renderText({as.character(datetimes())})




  # Downloader Tab ----------------------------------------------------------
  # 'select all' or 'remove all' hours
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



}

shinyApp(ui, server)


