# ----------------------------------------------------------------------- #
# Information: Layout chunks to save space in app.R
# Created by: Rodrigo Lustosa
# Creation date: 12 Jun 2022 12:04 (GMT -03)
# ----------------------------------------------------------------------- #

# fixing checkbox alignment. solution from: https://stackoverflow.com/a/46493086/14874374
fix_checkbox_alignment <- 
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
  )


# viewer tab --------------------------------------------------------------

# sidebar layout and buttons
sidebar_inputs <- sidebarPanel(
  # area name to be ploted in title and file name
  textInput("area_name",label = "Area name",value = "América do Sul"),
  # data source information for download
  fluidRow(
    column(6,selectInput("source","Data Source",choices = list("CPTEC/INPE" = 1))),
    column(6,selectInput("goes","Satellite",choices = list("GOES 16" = 16)))
  ),
  # imagery product specifications
  fluidRow(
    # type of product
    column(6,selectInput("type","Data type",choices = list("Channels"         = 1#,
                                                           # "RGB compositions" = 2
                                                           ))),
    column(6,
           # goes channels
           conditionalPanel("input.type == 1",
                            selectInput("channel","Channel",selected = 13,
                                        choices = list("CH01" =  1,"CH02" =  2,
                                                       "CH03" =  3,"CH04" =  4,
                                                       "CH05" =  5,"CH06" =  6,
                                                       "CH07" =  7,"CH08" =  8,
                                                       "CH09" =  9,"CH10" = 10,
                                                       "CH11" = 11,"CH12" = 12,
                                                       "CH13" = 13,"CH14" = 14,
                                                       "CH15" = 15,"CH16" = 16)
                            )),
           # RGB compositions
           conditionalPanel("input.type == 2",
                            selectInput("comp","Composition",
                                        choices = list("Air Mass"  = 1))))
  ),
  # date range
  dateRangeInput("dates","Date Range"#,start = "2022-06-10",end = "2022-06-10"
  ),
  # check if you want to see time adjustments
  checkboxInput("show_time",label = "Show time selection",value = FALSE),
  # time ajustments
  conditionalPanel(
    "input.show_time == 1",
    fluidRow(
      column(8,
             checkboxGroupInput("hours",label = "Hours",#width = "50%",
                                selected = 12:18,inline = TRUE,
                                choices = list("00" =  0, "01"=  1,"02"=  2,"03"=  3,
                                               "04" =  4, "05"=  5,"06"=  6,"07"=  7,
                                               "08" =  8, "09"=  9,"10"= 10,"11"= 11,
                                               "12" = 12, "13"= 13,"14"= 14,"15"= 15,
                                               "16" = 16, "17"= 17,"18"= 18,"19"= 19,
                                               "20" = 20, "21"= 21,"22"= 22,"23"= 23,
                                               "select all" = 24,"remove all" = 25))),
      column(4,
             checkboxGroupInput("min",label = "Minutes",#width = "50%",
                                selected = 0,inline = TRUE,
                                choices = list("00" = 0, "10" = 1,
                                               "20" = 2, "30" = 3,
                                               "40" = 4, "50" = 5)))
    )
  ),
  # map limits
  # fluidRow(
  #   column(6,numericInput("latmin","Min. Latitude",value =  -80,min = -90,max = 90)),
  #   column(6,numericInput("latmax","Max. Latitude",value =   20,min = -90,max = 90)),
  # ),
  # fluidRow(
  #   column(6,numericInput("lonmin","Min. Longitude",value =-110,min = -90,max = 90)),
  #   column(6,numericInput("lonmax","Max. Longitude",value =  10,min = -90,max = 90)),
  # ),
  selectInput("plot_method","Plot Method",selected = 4,
              choices = list("Higher quality"=1,"Lower quality"=2,
                             "Higher velocity"=3,"Automatic"=4)),
  # update plot with previous information
  fluidRow(column(12,actionButton("update","Update"),align="right"))
)

# Plot area
plot_panel <- mainPanel(
  # main plot
  plotOutput("plot"),
  # sidebar for animation and image selection
  sliderInput("image",label = "Image",
              animate = animationOptions(interval = 500),
              min = 1,max = 1,step = 1,value=1),
  # sliderInput("image",label = "Actual date and time",
  #             min = ymd_h("20200101 01"),
  #             max = ymd_h("20200101 02"),step = as.difftime(10, units = "mins"),
  #             value=ymd_h("20200101 02"),animate = TRUE),
  # textOutput("test")
)


# download tab ------------------------------------------------------------

download_tab_help_text <-
  helpText(paste("Note: Viewer already downloads data if it is not already available.",
                 "This tab was made for cases where you want to view many images,",
                 "so you can download it before anything, making the Viewer faster.",
                 "It also works for cases where you just want to download data to use elsewhere."))


download_tab_inputs <- 
  fluidRow(
    column(4,
           # data source information for download
           selectInput("source_d","Data Source",choices = list("CPTEC/INPE" = 1)),
           selectInput("goes_d","Satellite",choices = list("GOES 16" = 16)),
           # type of product
           checkboxGroupInput("type_d","Data type",choices = list("Channels"        = 1#,
                                                                  # "RGB compositions"= 2
           )),
           # goes channels
           conditionalPanel(
             "input.type_d.includes('1')",
             checkboxGroupInput("channel_d","Channel",inline = TRUE,#selected = 13,
                                choices = list("CH01" =  1,"CH02" =  2,
                                               "CH03" =  3,"CH04" =  4,
                                               "CH05" =  5,"CH06" =  6,
                                               "CH07" =  7,"CH08" =  8,
                                               "CH09" =  9,"CH10" = 10,
                                               "CH11" = 11,"CH12" = 12,
                                               "CH13" = 13,"CH14" = 14,
                                               "CH15" = 15,"CH16" = 16))
           ),
           conditionalPanel(
             "input.type_d.includes('2')",
             checkboxGroupInput("comp_d","Composition",inline = TRUE,
                                choices = list("Air Mass"  = 1)))
    ),
    column(4,
           dateRangeInput("dates_d","Date Range"),
           checkboxInput("show_time_d",label = "Show time selection",value = FALSE),
           conditionalPanel(
             "input.show_time_d == 1",
             fluidRow(
               column(6,
                      checkboxGroupInput(
                        "hours_d",label = "Hours",selected = 0:24,inline = TRUE,
                        choices = list("00" =  0, "01"=  1,"02"=  2,"03"=  3,
                                       "04" =  4, "05"=  5,"06"=  6,"07"=  7,
                                       "08" =  8, "09"=  9,"10"= 10,"11"= 11,
                                       "12" = 12, "13"= 13,"14"= 14,"15"= 15,
                                       "16" = 16, "17"= 17,"18"= 18,"19"= 19,
                                       "20" = 20, "21"= 21,"22"= 22,"23"= 23,
                                       "select all" = 24,"remove all" = 25))),
               column(6,
                      checkboxGroupInput("min_d",label = "Minutes",selected = 0:5,
                                         choices = list("00" = 0, "10" = 1,
                                                        "20" = 2, "30" = 3,
                                                        "40" = 4, "50" = 5)))
             )
           ),
           fluidRow(column(12,actionButton("download","Download"), align = 'right'))
    )
  )