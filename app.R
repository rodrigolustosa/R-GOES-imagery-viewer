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
library(maps)

# directory and file names
dir_data <- "data"
file_functions <- "R/functions.R"
file_objects <- "R/objects.R"
file_chunks <- "R/layout_chunks.R"

# initialization
app_version <- "Version 0.1.0"
licence <- "Copyright (c) 2022 Rodrigo Lustosa"

# source functions and objects
source(file_functions)
source(file_objects)
source(file_chunks)


ui <- fluidPage(
  # fixing checkbox alignment. solution from: https://stackoverflow.com/a/46493086/14874374
  fix_checkbox_alignment,
  # header
  h1("GOES Imagery Viewer"),
  # Tabs
  tabsetPanel(
    # Viewer
    tabPanel("Viewer",
             sidebarLayout(sidebar_inputs,plot_panel)
             ),
    # Download Tab
    tabPanel("Download Data",
             download_tab_help_text,
             download_tab_inputs,
             # actionButton("cancel_d","Cancel"),
             # textOutput("test"),
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
  # n_pixels_min_raster <- 10^7 #2271308
  # plot_info <- reactiveValues(
  #   palette = pal.TbINPE,
  #   breaks = val.TbINPE,
  #   zlim = c(-90, 55)
  # )



  # Viewer Tab --------------------------------------------------------------

  # 'select all' or 'remove all' hours
  observe({
    update_select_remove_all(input$hours,input_hours$last,"hours")
    # save last input hour
    input_hours$last <- input$hours
  })
  
  # imagery inputs summary
  imagery_info <- eventReactive(input$update,{
    imagery_info_prov <- inputs_summary(input$dates,input$hours,input$min,input$channel,
                                        input$latmin,input$latmax,input$lonmin,input$lonmax)
    # exclude files after the last available
    update_inputs_summary(imagery_info_prov)
  },ignoreNULL = FALSE)
  # update maximum number of images in slider
  observe({
    input$update
    n <- isolate({imagery_info()$n_files})
    updateSliderInput(inputId = "image", max = n,value = n)
  })
  # read data
  goes_data <- eventReactive(c(input$update,input$image),{
    download_and_read_image(imagery_info(),input$image,input$channel,
                            input$plot_method)
  },ignoreNULL = FALSE)
  # palette initialization
  plot_info <- eventReactive(input$update,{
    plot_info_prov <- get_palette(input$channel)
    
    plot_info_prov <- append(plot_info_prov,
                             list(xlim=imagery_info()$xlim,
                                  ylim=imagery_info()$ylim))
  },ignoreNULL = FALSE)
  # title plot
  title <- eventReactive(input$update,{
    make_title(input$channel,imagery_info()$dates4title,input$area_name)
  },ignoreNULL = FALSE)

  # plot
  output$plot_ui <- renderUI({
    delta_x <- imagery_info()$xlim[2] - imagery_info()$xlim[1]
    delta_y <- imagery_info()$ylim[2] - imagery_info()$ylim[1]
    if(delta_x >= delta_y) {
      plot.height = 550
      plot.width = plot.height/delta_y*delta_x
      if(plot.width > 900){
        plot.width = 900
        plot.height = plot.width/delta_x*delta_y
      }
    }else{
      plot.width = 550
      plot.height = plot.width/delta_x*delta_y
      if(plot.height > 900){
        plot.height = 900
        plot.width = plot.height/delta_y*delta_x
      }
    }
    plotOutput('plot', width = plot.width, height = plot.height)
    
  })
  output$plot <- renderPlot({
    input$update
    input$image
    # plot(1:10,1:10)
    make_plot(isolate({goes_data()}),
              isolate({plot_info()}),
              isolate({title()[input$image]}))
  }) %>% 
    bindCache(file.exists(imagery_info()$files_paths[input$image]),
              imagery_info()$datetime_names[input$image],
              isolate(input$channel),
              isolate(input$plot_method),
              imagery_info()$xlim,
              imagery_info()$ylim
    )


  # output$test <- renderText({imagery_info()$n_files})
  # output$test <- renderText({goes_data()$lat})
  # output$test <- renderText({as.character(datetimes())})




  # Downloader Tab ----------------------------------------------------------
  # 'select all' or 'remove all' hours
  observe({
    update_select_remove_all(input$hours_d,input_hours$last_d,"hours_d")
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


