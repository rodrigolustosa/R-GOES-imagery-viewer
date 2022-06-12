# ----------------------------------------------------------------------- #
# Information: GOES Imagery Viewer
# Created by: Rodrigo Lustosa
# Creation date:  9 Jun 2022 14:10 (GMT -03)
# License: MIT License
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

# initialization
app_version <- "Version 0.0.0"
licence <- "Copyright (c) 2022 Rodrigo Lustosa"

# source functions and objects
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
             
             # side Panel
             sidebarLayout(sidebarPanel(
               textInput("area_name",label = "Area name",value = "América do Sul"),
               # data information for download
               fluidRow(
                 column(6,selectInput("source","Data Source",choices = list("CPTEC/INPE" = 1))),
                 column(6,selectInput("goes","Satellite",choices = list("GOES 16" = 16)))
               ),
               fluidRow(
                 column(6,selectInput("type","Data type",choices = list("Channels"         = 1,
                                                                        "RGB compositions"= 2))),
                 column(6,
                        conditionalPanel("input.type == 1",
                                         selectInput("channel","Channel",selected = 13,
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
                                ),
               actionButton("update","Update")
               ),
               
               # Plot area
               mainPanel(
                 plotOutput("plot"),
                 sliderInput("image",label = "Image",
                             animate = animationOptions(interval = 500),
                             min = 1,max = 1,step = 1,value=1),
                 # sliderInput("image",label = "Actual date and time",
                 #             min = ymd_h("20200101 01"),
                 #             max = ymd_h("20200101 02"),step = as.difftime(10, units = "mins"),
                 #             value=ymd_h("20200101 02"),animate = TRUE),
                 textOutput("test")
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
                             checkboxGroupInput("type_d","Data type",choices = list("Channels"         = 1,
                                                                                    "RGB compositions [under develop.]"= 2)),
                             conditionalPanel("input.type_d.includes('1')",
                                              checkboxGroupInput("channel_d","Channel",inline = TRUE,#selected = 13,
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

  # initialization ----------------------------------------------------------
  input_hours <- reactiveValues(last=0:24,last_d=0:24)
  n_pixels_min_raster <- 10^7 #2271308
  # plot_info <- reactiveValues(
  #   palette = pal.TbINPE,
  #   breaks = val.TbINPE,
  #   zlim = c(-90, 55)
  # )
  
  
  
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


  # imagery inputs
  # ch_txt_1 <- reactive({str_c("ch" ,formatC(as.numeric(input$channel),width = 2,flag=0))})
  # imagery_info <- eventReactive(input$update,{list(all_bellow)})
  channels_text <- eventReactive(input$update,{
    channels_names(as.numeric(input$channel))
    },ignoreNULL = FALSE)
  channel_code <- eventReactive(input$update,{
    channels_code[[which(names(channels_code) == channels_text())]]
    },ignoreNULL = FALSE)
  hours <- eventReactive(input$update,{
    input$hours[!(input$hours %in% 24:25)]
    },ignoreNULL = FALSE)
  dates <- eventReactive(input$update,{
    seq(ymd(input$dates[1]),ymd(input$dates[2]),1)
    },ignoreNULL = FALSE)
  datetime_names <- eventReactive(input$update,{
    cptec_datetime_names(dates(),as.numeric(hours()),as.numeric(input$min)*10)
    },ignoreNULL = FALSE)
  dates4title <- eventReactive(input$update,{
    str_replace(datetime_names(),"(^\\d{4})(\\d{2})(\\d{2})(\\d{4})","\\1-\\2-\\3 \\4UTC")
    },ignoreNULL = FALSE)
  datetimes <- eventReactive(input$update,{ymd_hm(datetime_names())},ignoreNULL = FALSE)
  files_names <- eventReactive(input$update,{
    str_c(channel_code(),"_",datetime_names(),".nc")
    },ignoreNULL = FALSE)
  file_paths <- eventReactive(input$update,{
    str_c("data/CPTEC/GOES16/",str_remove(channels_text(),"_"),"/",files_names())
    },ignoreNULL = FALSE)
  n_files <- eventReactive(input$update,{length(datetime_names())},ignoreNULL = FALSE)
  # update slider with number of images to be displayed
  observe({
    input$update
    n <- isolate({n_files()})
    updateSliderInput(inputId = "image", max = n,value = n)
  })
  # read data
  # inputs: image, channel, lat, lon; dates, hours, min
  # cond_read_data <- reactive({list(input$update,input$image)})
  goes_data <- eventReactive(c(input$update,input$image),{
    if(!file.exists(file_paths()[input$image])){
      download_cptec_data(images_text=datetime_names()[input$image],
                          channels=as.numeric(input$channel),
                          dir_data=file.path(dir_data,"CPTEC/GOES16"))
    }
    if(file.exists(file_paths()[input$image])){
      data_prov <- read_netcdf_data(file_paths()[input$image])
      n_pixels <- length(data_prov$lat)*length(data_prov$lon)
      # subset data
      if(n_pixels <= n_pixels_min_raster){
        read_data_with_raster <- FALSE
        data_prov_values <- data_prov$values/100 # - 273.15
        if(as.numeric(input$channel) >= 7) data_prov_values <- data_prov_values - 273.15
        i_s <- (which((data_prov$lon - (-80)) >= 0)[1]):(max(which((data_prov$lon - (-60)) <= 0)))
        j_s <- (which((data_prov$lat - (-20)) >= 0)[1]):(max(which((data_prov$lat - 0) <= 0)))
        # i_s <- (which((data_prov$lon - xlim[1]) >= 0)[1]):(max(which((data_prov$lon - xlim[2]) <= 0)))
        # j_s <- (which((data_prov$lat - ylim[1]) >= 0)[1]):(max(which((data_prov$lat - ylim[2]) <= 0)))
        final_data <- list(values = data_prov_values[i_s,j_s],
                           lon = data_prov$lon[i_s],lat=data_prov$lat[j_s])
      }else{ # read data with raster
        final_data <- raster(file_paths()[input$image])/100 #- 273.15
        if(as.numeric(input$channel) >= 7) data_prov_values <- data_prov_values - 273.15
        read_data_with_raster <- TRUE
      }
      # if(input$channel == "13")
      # final_data <- final_data
      list(data=final_data,read_with_raster=read_data_with_raster)
    }
  },ignoreNULL = FALSE)
  # palette initialization
  # inputs: channel
  plot_info <- eventReactive(input$update,{
    if(as.numeric(input$channel) %in% 1:6){
      palette <- pal.Rfl;breaks <- val.Rfl;zlim <- c(0, 100)
    }
    if(as.numeric(input$channel) %in% 8:10){
      palette <- pal.Wv;breaks <- val.Wv;zlim <- c(-90, 55)
    }
    if(as.numeric(input$channel) %in% c(7,11:16)){
      palette <- pal.TbINPE;breaks <- val.TbINPE;zlim <- c(-90, 55)
    }
    return(list(palette=palette,breaks=breaks,zlim=zlim))
  },ignoreNULL = FALSE)
  # title plot
  # inputs: channel, image, area_name; dates, hours, min
  title <- eventReactive(input$update,{
    if(as.numeric(input$channel) %in% 1:2)
      abbreviation <- " (VIS) "
    if(as.numeric(input$channel) %in% 3:6)
      abbreviation <- " (NIR) "
    if(as.numeric(input$channel) %in% 7)
      abbreviation <- " (SWIR) "
    if(as.numeric(input$channel) %in% 8:10)
      abbreviation <- " (WV) "
    if(as.numeric(input$channel) %in% c(7,11:16))
      abbreviation <- " (IR) "
    str_c("GOES-16 CH",formatC(as.numeric(input$channel),width = 2,flag = 0),
          abbreviation,dates4title()," - ",input$area_name
          )
  },ignoreNULL = FALSE)
  
  # plot
  # inputs: update; channel, image, area_name, dates, hours, min
  output$plot <- renderPlot({
    input$update
    input$image
    
      # par(mar = c(10,10,10,10))
      if(length(goes_data()) == 0)
        return()
      if(goes_data()$read_with_raster){
        image.plot(isolate({goes_data()$data}),
                   col = isolate({plot_info()$palette}),
                   breaks = isolate({plot_info()$breaks}),
                   main = isolate({pula_linha(title()[input$image])}),
                   xlab = "Longitude",
                   ylab = "Latitude",
                   xlim = c(-80,-60),
                   ylim = c(-20,0),
                   zlim = isolate({plot_info()$zlim}))
      } else {
        image.plot(z = isolate({goes_data()$data$values}),
                   x = isolate({goes_data()$data$lon}),
                   y = isolate({goes_data()$data$lat}),
                   col = isolate({plot_info()$palette}),
                   breaks = isolate({plot_info()$breaks}),
                   main = isolate({pula_linha(title()[input$image])}),
                   xlab = "Longitude",
                   ylab = "Latitude",
                   xlim = c(-80,-60),
                   ylim = c(-20,0),
                   zlim = isolate({plot_info()$zlim}))
      }
    
  }) %>% bindCache(length(goes_data()) == 0,
                   datetime_names()[input$image],
                   isolate(input$channel))


  output$test <- renderText({file_paths()[input$image]})
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

  # imagery inputs
  # hours_d <- reactive({input$hours_d[!(input$hours_d %in% 24:25)]})
  # dates_d <- reactive({seq(ymd(input$dates_d[1]),ymd(input$dates_d[2]),1)})
  # datetime_names_d <- reactive({cptec_datetime_names(dates_d(),as.numeric(hours_d()),
  #                                                  as.numeric(input$min_d)*10)})
  # download data
  observeEvent(input$download,{
    download_cptec_data(ymd(input$dates_d),as.numeric(input$hours_d),
                        as.numeric(input$min_d)*10,as.numeric(input$channel_d),
                        dir_data=file.path(dir_data,"CPTEC/GOES16"))
  })



}

shinyApp(ui, server)


