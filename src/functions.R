# ----------------------------------------------------------------------- #
# Information: 
# Created by: Rodrigo Lustosa
# Creation date: 10 Jun 2022 17:53 (GMT -03)
# ----------------------------------------------------------------------- #

# packages
# library("RCurl")
# library("XML")

## Cria nome das imagens a serem baixadas
cptec_datetime_names <- function(datas = today(), horas = 0, minutos = 0){
  #formata vetores
  datas <- str_remove_all(datas, "-")
  horas <- ifelse(horas < 10, str_c("0",horas),as.character(horas))
  minutos <- ifelse(minutos < 10, str_c("0",minutos),as.character(minutos))
  #combinacoes de minutos e horas
  n_minutos <- length(minutos)
  n_horas <- length(horas)
  horas <- sort(rep(horas, times = n_minutos))
  minutos <- rep(minutos, times = n_horas)
  horarios <- str_c(horas,minutos)
  #combinacao de datas e horarios
  n_datas <- length(datas)
  datas <- sort(rep(datas, times = n_horas*n_minutos))
  horarios <- rep(horarios, times = n_datas)
  #retorna valor
  imagens <- sort(str_c(datas,horarios))
  return(imagens)
}

#retorna canais em texto
channels_names <- function(channels = c(2,8,13)){
  ch <- vector(length = 0)
  if(any(channels < 10))
    ch <- c(ch, str_c("ch_0",channels[channels < 10]))
  if(any(channels >= 10 & channels <= 16))
    ch <- c(ch, str_c("ch_",channels[channels >= 10 & channels <= 16]))
  if(17 %in% channels)
    ch <- c(ch,"cln")
  # if(18 %in% channels)
  #   ch <- c(ch,"glm")
  return(ch)
}

# cptec_ftp_url <- function(){
#   # inicializacoes
#   ftp <- "http://ftp.cptec.inpe.br/goes/goes16/"
#   endereco <- str_c(ftp,pastas,str_sub(im,1,4),"/",str_sub(im,5,6),"/",arquivo)
# }

# download cptec goes 16
download_cptec_data <- function(daterange,hours,minutes,channels,dir_data="data/CPTEC/GOES16",images_text=NULL){
  # inicializacoes
  ftp <- "http://ftp.cptec.inpe.br/goes/goes16/"
  # imagery names and channels
  if(is.null(images_text))
    images_text <- cptec_datetime_names(seq(daterange[1],daterange[2],1),hours,minutes)
  channels_text <- channels_names(channels)
  n_images <- length(images_text)
  n_channels <- length(channels_text)
  # baixa arquivos
  withProgress(message=str_c("Downloading images"),value=0,{
    for(ch in channels_text){
      nom_ch <- names(channels_code)
      codigo <- channels_code[[which(nom_ch %in% ch)]]
      ch <- str_remove_all(ch,"_") #formato do site do impe
      #para os canais
      if(ch == "cln" | str_sub(ch,1,2) == "ch"){
        if(ch == "cln"){
          pastas <- "class_nuvens/class_nuvens_diurna_bin/"
          formato <- ".bin"
        }else{
          pastas <- str_c("retangular/",ch,"/")
          formato <- ".nc"
        }
        withProgress(message=str_c("Downloading images from ",ch),value=0,{
          for (im in images_text){
            arquivo <- str_c(codigo,"_",im,formato)
            path <- file.path(dir_data,ch,arquivo)
            endereco <- str_c(ftp,pastas,str_sub(im,1,4),"/",str_sub(im,5,6),"/",arquivo)
            if(!file.exists(path)){
              if(url.exists(endereco)){
                if(!dir.exists(file.path(dir_data,ch)))
                  dir.create(file.path(dir_data,ch))
                download.file(url = endereco, destfile = path, #quiet = T,
                              method = "curl")
              }
              incProgress(1/n_images, 
                          detail = str_c("Channel progress: ",round(which(im==images_text)/n_images*100),"%"))
            }
          }
        })
        
      }
      incProgress(1/n_channels)
    }
  })
  
}

# check_paths <- function(paths){
#   updated_paths <- paths[file.exists(paths)]
#   n_files  <- length(updated_paths)
#   return(list(files_paths=updated_paths,n_files=n_files))
# }
# 
# check_urls <- function(urls){
#   updated_urls <- paths[url.exists(urls)]
#   n_files  <- length(updated_urls)
#   return(list(files_paths=updated_paths,n_files=n_files))
# }

#pula linha do titulo dos mapas na plotagem, caso ele seja grande demais
pula_linha <- function(nome, n = 60){
  #numero de caracteres
  n_characters <- str_count(nome)
  #se o nome eh grande a ponto de precisar pular linha
  if(n_characters > n){
    #localiza boa posicao para pular linha (um traco com espacos)
    traco <- str_locate_all(nome," - ")[[1]][,1]
    #se essa posicao existe e esta em um lugar pulavel
    if(length(traco) > 0 & any(traco <= n)){
      #posicao do traco
      o_traco <- max(traco[traco <= n])
      #parte 1 do nome
      nome_1 <- str_sub(nome,1,o_traco-1)
      #parte 2 do nome
      nome_2 <- str_sub(nome,o_traco,n_characters)
      #nome com pulo de linha
      nome <- str_c(nome_1,"\n",nome_2)
    }else{
      #posicao dos espacos
      espacos <- str_locate_all(nome," - | ")[[1]][,1]
      #ultimo espaco em um lugar pulavel
      o_espaco <- max(espacos[espacos <= 60])
      #parte 1 do nome
      nome_1 <- str_sub(nome,1,o_espaco-1)
      #parte 2 do nome
      nome_2 <- str_sub(nome,o_espaco,n_characters)
      #nome com pulo de linha
      nome <- str_c(nome_1,"\n",nome_2)
    }
  }
  return(nome)
}

# open netcdf file
read_netcdf_data <- function(path){
  #abre arquivo
  ncin <- nc_open(path) #-- Abrindo o arquivo .nc
  #extrai valores
  values <- ncvar_get(ncin,"Band1")   #-- Extraindo a matriz das medicoes
  lon <- ncvar_get(ncin,"lon") #-- Extraindo o vetor de coordenadas de longitude
  lat <- ncvar_get(ncin,"lat") #-- Extraindo o vetor coordenadas de latitude
  #fecha arquivo
  nc_close(ncin)
  return(list(values=values,lat=lat,lon=lon))
}


# server functions --------------------------------------------------------

# 'select all' or 'remove all' hours
update_select_remove_all <- function(hours,last,inputID){
  added   <- hours[!(hours %in% last)]
  removed <-  last[!(last %in% hours)]
  # when new box was added to hours
  if (length(added) > 0){
    # if 'remove all' was added
    if(25 %in% added)
      updateCheckboxGroupInput(inputId = inputID,selected = 25)
    # if 'select all' was added
    if(24 %in% added)
      updateCheckboxGroupInput(inputId = inputID,selected = 0:24)
    # if any hour was added and 'remove all' box is selected
    if(any(0:23 %in% added) & (25 %in% hours))
      updateCheckboxGroupInput(inputId = inputID,
                               selected = hours[hours != 25])
  }
  # when new box was removed to hours
  if (length(removed) > 0){
    # if any hour was removed and 'select all' box is selected
    if(24 %in% hours & !(25 %in% removed))
      updateCheckboxGroupInput(inputId = inputID,
                               selected = hours[hours != 24])
  }
}

inputs_summary <- function(daterange,inp_hours,inp_mins,channel,
                           latmin,latmax,lonmin,lonmax){
  # channel info
  channel_text <- channels_names(as.numeric(channel))
  channel_code <- channels_code[[which(names(channels_code) == channel_text)]]
  # date and time info
  dates <- seq(ymd(daterange[1]),ymd(daterange[2]),1)
  hours <- as.numeric(inp_hours[!(inp_hours %in% 24:25)])
  mins  <- as.numeric(inp_mins)*10
  datetime_names <- cptec_datetime_names(dates,hours,mins)
  dates4title <- 
    str_replace(datetime_names,"(^\\d{4})(\\d{2})(\\d{2})(\\d{4})","\\1-\\2-\\3 \\4UTC")
  # datetimes <- ymd_hm(datetime_names())
  # files info
  files_names <- str_c(channel_code,"_",datetime_names,".nc")
  files_paths <- str_c("data/CPTEC/GOES16/",str_remove(channel_text,"_"),"/",files_names)
  n_files <- length(datetime_names)
  ftp <- "http://ftp.cptec.inpe.br/goes/goes16/retangular/"
  files_urls <- str_c(ftp,str_remove(channel_text,"_"),"/",str_sub(datetime_names,1,4),
                    "/",str_sub(datetime_names,5,6),"/",files_names)
  # limits info
  xlim <- c(lonmin,lonmax)
  ylim <- c(latmin,latmax)
  return(list(channel_text=channel_text,channel_code=channel_code,
              dates=dates,hours=hours,mins=mins,datetime_names=datetime_names,
              dates4title=dates4title,
              files_names=files_names,files_paths=files_paths,files_urls=files_urls,
              xlim=xlim,ylim=ylim,n_files=n_files
              ))
  
}

# update inputs_summary with only available files
update_inputs_summary <- function(inputs_summary){
  # detect which files exists locally or on the internet
  inputs_summary$files_urls
  inputs_summary$files_paths
  index_path <- file.exists(inputs_summary$files_paths)
  index_date <- ymd_hm(inputs_summary$datetime_names) <= now()
  i <- as.numeric(inputs_summary$n_files)
  last_available_found <- FALSE
  while(i > 0 & !last_available_found){
    if(index_date[i] & index_path[i])
      last_available_found <- TRUE
    if(index_date[i] & !index_path[i])
      if(url.exists(inputs_summary$files_urls[i]))
        last_available_found <- TRUE
    i <- i - 1
  }
  index <- 1:(i+1)
  # index_path <- file.exists(inputs_summary$files_paths)
  # index_url  <- url.exists(inputs_summary$files_urls[!index_path])
  # index <- sort(c(which(index_path),which(!index_path)[index_url]))
  # update inputs_summary with only available files
  updated_inputs_summary <- inputs_summary
  # updated_inputs_summary$channel_text   <- inputs_summary$channel_text[index]
  # updated_inputs_summary$channel_code   <- inputs_summary$channel_code[index]
  updated_inputs_summary$dates          <- inputs_summary$dates[index]
  updated_inputs_summary$hours          <- inputs_summary$hours[index]
  updated_inputs_summary$mins           <- inputs_summary$mins[index]
  updated_inputs_summary$datetime_names <- inputs_summary$datetime_names[index]
  updated_inputs_summary$dates4title    <- inputs_summary$dates4title[index]
  updated_inputs_summary$n_files        <- length(index)
  updated_inputs_summary$files_names    <- inputs_summary$files_names[index]
  updated_inputs_summary$files_paths    <- inputs_summary$files_paths[index]
  updated_inputs_summary$files_urls     <- inputs_summary$files_urls[index]
  return(updated_inputs_summary)
}

download_and_read_image <- function(imagery_info,image,channel,plot_method,dir_data){
  plot_method <- as.numeric(plot_method)
  lim_pixels_subset_method <- -1
  # Automatic
  if(plot_method == 4)
    lim_pixels_subset_method <- 10^7
  # Higher velocity
  if(plot_method == 3)
    lim_pixels_subset_method <- 473344
  # download
  if(!file.exists(imagery_info$files_paths[image])){
    download_cptec_data(images_text=imagery_info$datetime_names[image],
                        channels=as.numeric(channel),
                        # dir_data=file.path("data/CPTEC/GOES16")
                        dir_data=file.path(dir_data,"CPTEC/GOES16")
                        )
  }
  # read data
  if(file.exists(imagery_info$files_paths[image])){
    data_prov <- read_netcdf_data(imagery_info$files_paths[image])
    n_pixels <- length(data_prov$lat)*length(data_prov$lon)
    if(n_pixels <= lim_pixels_subset_method | plot_method == 1){
      # subset data method
      read_data_with_raster <- FALSE
      data_prov_values <- data_prov$values/100 # - 273.15
      if(as.numeric(channel) >= 7) data_prov_values <- data_prov_values - 273.15
      i_s <- (which((data_prov$lon - (-80)) >= 0)[1]):(max(which((data_prov$lon - (-60)) <= 0)))
      j_s <- (which((data_prov$lat - (-20)) >= 0)[1]):(max(which((data_prov$lat - 0) <= 0)))
      i_s <- (which((data_prov$lon - imagery_info$xlim[1]) >= 0)[1]):(max(which((data_prov$lon - imagery_info$xlim[2]) <= 0)))
      j_s <- (which((data_prov$lat - imagery_info$ylim[1]) >= 0)[1]):(max(which((data_prov$lat - imagery_info$ylim[2]) <= 0)))
      final_data <- list(values = data_prov_values[i_s,j_s],
                         lon = data_prov$lon[i_s],lat=data_prov$lat[j_s])
    }else{ 
      # read data with raster method
      final_data <- raster(imagery_info$files_paths[image])/100 #- 273.15
      if(as.numeric(channel) >= 7) final_data <- final_data - 273.15
      read_data_with_raster <- TRUE
    }
    # if(input$channel == "13")
    # final_data <- final_data
    return(list(data=final_data,read_with_raster=read_data_with_raster))
  }
}

get_palette <- function(channel){
  channel <- as.numeric(channel)
  if(channel %in% 1:6){
    palette <- pal.Rfl;breaks <- val.Rfl;zlim <- c(0, 100)
  }
  if(channel %in% 8:10){
    palette <- pal.Wv;breaks <- val.Wv;zlim <- c(-90, 55)
  }
  if(channel %in% c(7,11:16)){
    palette <- pal.TbINPE;breaks <- val.TbINPE;zlim <- c(-90, 55)
  }
  return(list(palette=palette,breaks=breaks,zlim=zlim))
}

make_title <- function(channel,dates4title,area_name){
  channel <- as.numeric(channel)
  if(channel %in% 1:2)
    abbreviation <- " (VIS) "
  if(channel %in% 3:6)
    abbreviation <- " (NIR) "
  if(channel %in% 7)
    abbreviation <- " (SWIR) "
  if(channel %in% 8:10)
    abbreviation <- " (WV) "
  if(channel %in% c(7,11:16))
    abbreviation <- " (IR) "
  return(str_c("GOES-16 CH",formatC(channel,width = 2,flag = 0),
               abbreviation,dates4title," - ",area_name)
  )
}

make_plot <- function(goes_data,plot_info,title){
  if(length(goes_data) == 0)
    return()
  par()
  if(goes_data$read_with_raster){
    image.plot(goes_data$data,
               col = plot_info$palette,
               breaks = plot_info$breaks,
               main = pula_linha(title),
               xlab = "Longitude",
               ylab = "Latitude",
               xlim = plot_info$xlim,
               ylim = plot_info$ylim,
               zlim = plot_info$zlim)
    map("world", add = TRUE, col = "white")
  } else {
    image.plot(z = goes_data$data$values,
               x = goes_data$data$lon,
               y = goes_data$data$lat,
               col = plot_info$palette,
               breaks = plot_info$breaks,
               main = pula_linha(title),
               xlab = "Longitude",
               ylab = "Latitude",
               xlim = plot_info$xlim,
               ylim = plot_info$ylim,
               zlim = plot_info$zlim)
    map("world", add = TRUE, col = "white")
  }
}


