# ----------------------------------------------------------------------- #
# Information: 
# Created by: Rodrigo Lustosa
# Creation date: 10 Jun 2022 17:53 (GMT -03)
# ----------------------------------------------------------------------- #

# packages
# library("RCurl")
# library("XML")

#informacoes dos canais
chanels_code <- list(ch_01 = "S10635333",ch_02 = "S10635334",ch_03 = "S10635335",
                     ch_04 = "S10635336",ch_05 = "S10635337",ch_06 = "S10635338",
                     ch_07 = "S10635339",ch_08 = "S10635340",ch_09 = "S10635341",
                     ch_10 = "S10635342",ch_11 = "S10635344",ch_12 = "S10635345",
                     ch_13 = "S10635346",ch_14 = "S10635347",ch_15 = "S10635348",
                     ch_16 = "S10635349",cln = "S11632406")

## Cria nome das imagens a serem baixadas
cptec_imagery_names <- function(datas = today(), horas = 0, minutos = 0){
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
  imagens <- str_c(datas,horarios)
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
download_cptec_data <- function(daterange,hours,minutes,chanels,dir_data="data/CPTEC/GOES16"){
  # inicializacoes
  ftp <- "http://ftp.cptec.inpe.br/goes/goes16/"
  # imagery names and chanels
  images_text <- cptec_imagery_names(seq(daterange[1],daterange[2],1),hours,minutes)
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
            if(!file.exists(arquivo)){
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




