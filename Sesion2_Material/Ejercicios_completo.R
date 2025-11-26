## ------------------------------------------------------------------------
##
## Curso:    Introducción a R en investigación biomédica
## Sesion 2: Introducción a la bioestadística en R - PRÁCTICA
##
## ------------------------------------------------------------------------


## Configuracion inicial: cargar paquetes etc.
# install.packages("openxlsx")
library(openxlsx)


# Primera parte -----------------------------------------------------------

## importar los datos de la bbdd rotterdam
rotterdam <- read.xlsx("Sesion2_Material/Datos/rotterdam.xlsx")
## Cuántas filas (observaciones) tiene?
dim(rotterdam)
nrow(rotterdam)
## Cuántas variables (columnas)?
ncol(rotterdam)
names(rotterdam)


# |-- (a) El tamaño medio del tumor difiere de 25 mm ----------------------
summary(rotterdam$size_sim)
boxplot(rotterdam$size_sim)
res <- t.test(rotterdam$size_sim, mu = 25, alternative = "two.sided")
res
## rechazamos H0. el tamaño medio del tumor difiere de 25mm


# |-- (b) Diferencia de edad terapia hormo vs no --------------------------
## Hay alguna diferencia en la edad entre las pacientes que reciben terapia
## hormonal y las que no.
table(rotterdam$hormon)
age_horm0 <- rotterdam$age[which(rotterdam$hormon == 0)]
age_horm1 <- rotterdam$age[which(rotterdam$hormon == 1)]
shapiro.test(age_horm0)
shapiro.test(age_horm1)
## u de mann-whitney
wilcox.test(x = rotterdam$hormon, y = rotterdam$age,
            alternative = "two.sided", paired = FALSE)
## si, rechazamos H0 --> hay diferencias estadisticamente signif


# |-- (c) Tamaño tumoral terapia hormo vs no ------------------------------
## La distribución del tamaño de los tumores difiere entre los grupos que
## reciben terapia hormonal.
wilcox.test(x = rotterdam$hormon, y = rotterdam$size_sim, 
            alternative = "two.sided", paired = FALSE)
table(rotterdam$size, rotterdam$hormon)
chisq.test(table(rotterdam$size, rotterdam$hormon))
## rechazamos H0 --> hay dependencia/asociación entre las dos variables


# |-- (d) Asociacion recurrencia y categorías de ganglios pos -------------
## El estado de recurrencia está asociado con el número de categorías de
## ganglios positivos.
nodes_recur0 <- rotterdam$nodes[which(rotterdam$recur == 0)]
nodes_recur1 <- rotterdam$nodes[which(rotterdam$recur == 1)]
shapiro.test(nodes_recur0)
wilcox.test(x = rotterdam$recur, y = rotterdam$nodes,
            alternative = "two.sided", paired = FALSE)
## rechazamos H0 --> hay asociación entre las dos variables


# Segunda parte -----------------------------------------------------------

## importar datos
rotterdam <- read.xlsx("Sesion2_Material/Datos/rotterdam.xlsx")


# |-- (a) Modelo 1 --------------------------------------------------------
lm1 <- lm(size_sim ~ age, data = rotterdam)
summary(lm1)
## diagrama de dispersion (scatterplot o "punto de nubes")
plot(rotterdam$age, rotterdam$size_sim)
lines(rotterdam$age, predict(lm1, newdata = rotterdam), col = "red")
## chequeo de las asunciones
plot(lm1)


# |-- (b) Modelo 2 --------------------------------------------------------
rotterdam$meno <- factor(rotterdam$meno)
lm2 <- lm(size_sim ~ age + nodes + meno, data = rotterdam)
summary(lm2)


# |-- (c) Modelo 3 --------------------------------------------------------
hist(rotterdam$nodes)
hist(log(rotterdam$nodes + 1))
## crear variable
rotterdam$lognodes <- log(rotterdam$nodes + 1)
## ajustar modelo
lm3 <- lm(size_sim ~ age + lognodes + meno, data = rotterdam)
summary(lm3)


