#z metodo plsr (partial least square regression) bomo za vsako napovedovano spremenljivko posebej (cena_nadomestila, povecana_obiskanost_zaradi_glasbe)
#določili število komponent (na glasbaTrain), ki so potrebne za najboljšo vizualizacijo spremenljivk, z metodo PLSR bomo napovedali (na glasbaTest) Y 
#iz napovedovalnih spremenljivk in modela. Ugotavljali bomo tudi napako metode.

library(pls)
library(plsdepot)
library(readr)
#naložimo csv datoteko s podatki
glasba <- read_delim("music.csv", ";",
                     escape_double = FALSE, locale = locale(decimal_mark = ","), trim_ws = TRUE)

#podatke razdelimo na dva dela, na del, kjer bomo algoritem "trenirali", in del kjer bomo algoritem testirali.
glasbaTrain <- glasba[1:40,]
glasbaTest <- glasba[41:50,]


glasba.pls11 <- plsr(cena_nadomestila ~ kvadratura + zunanji_prostori + stevilo_sedisc + stevilo_zaposlenih + stevilo_glasbenih_naprav + povprecna_cena_alkoholnih_pijac + povprecna_cena_brezalkoholnih_pijac + stevilo_delavnih_dni + obiskanost + dobicek + oddaljenost_od_vecjega_kraja + ponudba_hrane + ponudba_za_mlajse + posebni_dogodki,
                     data = glasbaTrain, ncomp = 14, validation = "LOO")
summary(glasba.pls11)
RMSEP(glasba.pls11, estimate = "train", intercept = F)
plot(glasba.pls11, "validation", estimate = "CV")
plot(glasba.pls11, ncomp = 8, asp = 1, line = TRUE)
y11 = predict(glasba.pls11, ncomp = 8, newdata = glasbaTest)
y11
RMSEP(glasba.pls11, newdata = glasbaTest)


glasba.pls12 <- plsr(povecana_obiskanost_zaradi_glasbe ~ kvadratura + zunanji_prostori + stevilo_sedisc + stevilo_zaposlenih + stevilo_glasbenih_naprav + povprecna_cena_alkoholnih_pijac + povprecna_cena_brezalkoholnih_pijac + stevilo_delavnih_dni + obiskanost + dobicek + oddaljenost_od_vecjega_kraja + ponudba_hrane + ponudba_za_mlajse + posebni_dogodki,
                     data = glasbaTrain, ncomp = 14, validation = "LOO")
summary(glasba.pls12)
RMSEP(glasba.pls12, estimate = "train", intercept = F)
plot(glasba.pls12, "validation", estimate = "CV")
plot(glasba.pls12, ncomp = 6, asp = 1, line = TRUE)
y12 = predict(glasba.pls12, ncomp = 6, newdata = glasbaTest)
y12
RMSEP(glasba.pls12, newdata = glasbaTest)

pls13 <- plsreg1(glasbaTrain[, c(2,3,4,5,6,7,8,9,10,11,12,13,14,15)], glasbaTrain[,16, drop = FALSE], comp = 3)
plot(pls13)
pls14 <- plsreg1(glasbaTrain[, c(2,3,4,5,6,7,8,9,10,11,12,13,14,15)], glasbaTrain[,17, drop = FALSE], comp = 3)
plot(pls14)

glasba.pls2 <- plsr(matrix(c(cena_nadomestila, povecana_obiskanost_zaradi_glasbe), 40, 2) ~. , data = glasbaTrain, ncomp =14, validation = "LOO")
summary(glasba.pls2)
RMSEP(glasba.pls2, estimate = "train", intercept = F)
plot(glasba.pls2, "validation", estimate = "CV")
plot(glasba.pls2, ncomp = c(9,6), asp = 1, line = TRUE)
y <- predict(glasba.pls2, ncomp = c(9,6), newdata = glasbaTest)
y
rms <- function(x,y) sqrt(mean((x-y)^2))
rms(glasbaTest$cena_nadomestila, y[,1,])
rms(glasbaTest$povecana_obiskanost_zaradi_glasbe, y[,2,])

