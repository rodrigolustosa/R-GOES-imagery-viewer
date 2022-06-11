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
chanels_names <- function(chanels = c(2,8,13)){
  ch <- vector(length = 0)
  if(any(chanels < 10))
    ch <- c(ch, str_c("ch_0",chanels[chanels < 10]))
  if(any(chanels >= 10 & chanels <= 16))
    ch <- c(ch, str_c("ch_",chanels[chanels >= 10 & chanels <= 16]))
  if(17 %in% chanels)
    ch <- c(ch,"cln")
  # if(18 %in% chanels)
  #   ch <- c(ch,"glm")
  return(ch)
}

# download cptec goes 16
download_cptec_data <- function(daterange,hours,minutes,chanels,dir_data="data/CPTEC/GOES16",images_text=NULL){
  # inicializacoes
  ftp <- "http://ftp.cptec.inpe.br/goes/goes16/"
  # imagery names and chanels
  if(is.null(images_text))
    images_text <- cptec_datetime_names(seq(daterange[1],daterange[2],1),hours,minutes)
  chanels_text <- chanels_names(chanels)
  n_images <- length(images_text)
  n_chanels <- length(chanels_text)
  # baixa arquivos
  withProgress(message=str_c("Downloading images"),value=0,{
    for(ch in chanels_text){
      nom_ch <- names(chanels_code)
      codigo <- chanels_code[[which(nom_ch %in% ch)]]
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
      incProgress(1/n_chanels)
    }
  })
  
}

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



