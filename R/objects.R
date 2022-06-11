# ----------------------------------------------------------------------- #
# Information: 
# Created by: Rodrigo Lustosa
# Creation date: 10 Jun 2022 22:01 (GMT -03)
# ----------------------------------------------------------------------- #

#informacoes dos canais
chanels_code <- list(ch_01 = "S10635333",ch_02 = "S10635334",ch_03 = "S10635335",
                     ch_04 = "S10635336",ch_05 = "S10635337",ch_06 = "S10635338",
                     ch_07 = "S10635339",ch_08 = "S10635340",ch_09 = "S10635341",
                     ch_10 = "S10635342",ch_11 = "S10635344",ch_12 = "S10635345",
                     ch_13 = "S10635346",ch_14 = "S10635347",ch_15 = "S10635348",
                     ch_16 = "S10635349",cln   = "S11632406")



# color palette -----------------------------------------------------------

#-- Temperatura de Brilho (Tb)

#--- Paleta toda em tons de cinza
pal.Tb <- gray(seq(1, 0, -0.001))
val.Tb <- seq(from = -90, to = 55, length.out = length(pal.Tb))

#--- Paleta em tons de cinza, destacando temperatura abaixo de 10oC
palCO <- c("#ff6500", "#ffff6f", "#30ccff", "#0100fe", "#ff99fe", "#b83ab7")
palCO <- palCO[length(palCO):1]
valCO <- seq(-90, -30, 10)
palPB <- gray(seq(0.8, 0, -0.001))
valPB <- seq(from = -29.999, to = 55, length.out = length(palPB))
pal.TbINPE <- c(palCO, palPB)
val.TbINPE <- c(valCO, valPB)

#--- Paleta em tons de cinza, colorindo temperaturas abaixo de -19oC
#palCO2 <- c()
#


#--- paleta vapor dagua
pal.Wv <- c("#FFFFFF",rev(gray(seq(0, 1, 0.001))),"#000000")
val.Wv <- c(-80,seq(from = -70, to = -20, length.out = length(pal.Wv) -1),-10)

#-- Reflect?ncia (0-100%)

#--- Paleta toda em tons de cinza
pal.Rfl <- gray(seq(0, 1, 0.001))
val.Rfl <- seq(from = 0, to = 100, length.out = length(pal.Rfl) + 1)


#-- Classifica??o de Nuvens

# #--- Paleta colorida
# pal.Cln <- as.character(as.matrix(read.table("arquivosauxiliares/paleta-ClassNuv.txt")))
# val.Cln <- seq(from = 1, to = 30, length.out = length(pal.Cln) + 1)
