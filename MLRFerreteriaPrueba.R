str(ferre)
head(ferre)
tail(ferre)
attach(ferre)
summary(ferre)

set.seed(65)
datos    <- ferre
n        <- nrow(datos)
muestra  <- sample(n, n * .70)
train    <- datos[muestra, ]
test     <- datos[-muestra, ]


# NORMALIZACION DE VARIABLES
# -----------------------------------------------------
library(MASS)
library(neuralnet)
library(ggplot2)
maxs      <- apply(train, 2, max)
mins      <- apply(train, 2, min)
datos_nrm <- as.data.frame(scale(datos, center = mins, scale = maxs - mins))
train_nrm <- datos_nrm[muestra, ]
test_nrm  <- datos_nrm[-muestra, ]


# FORMULA
# -----------------------------------------------------
nms  <- names(train_nrm)
frml <- as.formula(paste("V.Neta ~", paste(nms[!nms %in% "V.Neta"], collapse = " + ")))


# MODELO
# -----------------------------------------------------
modelo.nn <- neuralnet(frml,
                       data          = train_nrm,
                       hidden        = c(7,5), # ver Notas para detalle 
                       threshold     = 0.05,   # ver Notas para detalle
                       algorithm     = "rprop+" 
)


# PREDICCION
# -----------------------------------------------------
pr.nn   <- compute(modelo.nn,within(test_nrm,rm(V.Neta)))

# se transoforma el valor escalar al valor nominal original
V.Neta.predict <- pr.nn$net.result*(max(datos$V.Neta)-min(datos$V.Neta))+min(datos$V.Neta)
V.Neta.real    <- (test_nrm$V.Neta)*(max(datos$V.Neta)-min(datos$V.Neta))+min(datos$V.Neta)



# SUMA DE ERROR CUADRATICO
# -----------------------------------------------------
(se.nn <- sum((V.Neta.real - V.Neta.predict)^2)/nrow(test_nrm))


#GRAFICOS
# -----------------------------------------------------
# Errores
qplot(x=V.Neta.real, y=V.Neta.predict, geom=c("point","smooth"), method="lm", 
      main=paste("Real Vs Prediccion. Summa de Error Cuadratico=", round(se.nn,2)))
# Red
plot(modelo.nn)
