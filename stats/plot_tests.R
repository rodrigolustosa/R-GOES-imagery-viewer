# ----------------------------------------------------------------------- #
# Information: Tests to see which plot method is faster and in which situ-
#  ation. In conclusion, it is best to use raster method for large numbers
#  of pixels and subset method for small number of points.
# Created by: Rodrigo Lustosa
# Creation date: 11 Jun 2022 11:45 (GMT -03)
# ----------------------------------------------------------------------- #

# packages
library(tidyverse)
library(lubridate)
library(ncdf4)
library(raster)
library(fields)

# directory and file names
dir_data <- "data"
file_functions <- "R/functions.R"
file_objects <- "R/objects.R"

# initializations
source(file_functions)
source(file_objects)


# im <- cptec_datetime_names(ymd('2022-06-10'),as.numeric(19),
#                     as.numeric(0)*10)
# ch_code <- chanels_code[[which(names(chanels_code) == "ch_13")]]
# 
# path <- str_c("data/CPTEC/GOES16/ch13/",str_c(ch_code,"_",im,".nc"))
path <- "data/CPTEC/GOES16/ch13/S10635346_202206101900.nc"

paleta <- pal.TbINPE
divisoes <- val.TbINPE
zlim <- c(-90, 55)

# change limits to have different number of pixels
xlim <- c(-65,-60)
ylim <- c(-5, 0)

# method 1: simple plot
dados <- read_netcdf_data(path)
dados_valores <- dados$values/100 - 273.15
agora <- now()
image.plot(z = dados_valores,
           x = dados$lon,
           y = dados$lat,
           col = paleta,
           breaks = divisoes,
           # main = pula_linha(title),
           xlab = "Longitude",
           ylab = "Latitude",
           xlim = xlim,
           ylim = ylim,
           zlim = zlim)
dif <- now() - agora
print(dif)

# method 2: subseting data before plot
dados <- read_netcdf_data(path)
dados_valores <- dados$values/100 - 273.15
i_s <- (which((dados$lon - xlim[1]) >= 0)[1]):(max(which((dados$lon - xlim[2]) <= 0)))
j_s <- (which((dados$lat - ylim[1]) >= 0)[1]):(max(which((dados$lat - ylim[2]) <= 0)))
agora <- now()
image.plot(z = dados_valores[i_s,j_s],
           x = dados$lon[i_s],
           y = dados$lat[j_s],
           col = paleta,
           breaks = divisoes,
           # main = pula_linha(title),
           xlab = "Longitude",
           ylab = "Latitude",
           xlim = xlim,
           ylim = ylim,
           zlim = zlim)
dif_2 <- now() - agora
print(dif_2)

# method 3: reading and ploting with raster
dados <- raster(path)
agora <-now()
# image.plot(dados/100 - 273.15,
#            col = paleta,
#            breaks = divisoes,
#            # main = pula_linha(title),
#            xlab = "Longitude",
#            ylab = "Latitude",
#            # xlim = c(-80,-60),
#            # ylim = c(-20,0),
#            zlim = zlim)
image.plot(dados/100 - 273.15,
           col = paleta,
           breaks = divisoes,
           # main = pula_linha(title),
           xlab = "Longitude",
           ylab = "Latitude",
           xlim = xlim,
           ylim = ylim,
           zlim = zlim)
dif_3 <- now() - agora
print(dif_3)

# time output from each method and number of pixels
length(i_s)*length(j_s)
print(dif)
print(dif_2)
print(dif_3)
# values got from outputs above, using different number of pixels
n_pix <- c(     29584,  473344, 1065024, 1893376, 2958400, 4262160)
t_1   <- c(  11.11414,13.19177,13.39893,14.09384,14.05170,14.11108)
t_2   <- c(0.02811718,1.274095,2.068337,3.624133,5.004652,7.192842)
t_3   <- c( 0.9781375,1.277793,1.361005,1.063474,1.324653,1.163297)
# plot data gotten above
plot(n_pix,t_2,ylim=c(0,14.2),xlim=c(0,4262170))
points(n_pix,t_1,col="blue")
points(n_pix,t_3,col="red")
# plot linear fit
ajuste <- lm(t_2 ~ n_pix)
coefs <- coef(ajuste)
abline(coefs[1],coefs[2])
# estimate time
coefs[1] + coefs[2]*1100000
# estimate number of pixels
(8 - coefs[1])/coefs[2]
