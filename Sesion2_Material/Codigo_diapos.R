## ------------------------------------------------------------------------
##
## Curso:    Introducción a R en investigación biomédica
## Sesion 2: Introducción a la bioestadística en R - Codigo de las diaps.
##
## ------------------------------------------------------------------------


## Configuracion inicial: cargar paquetes etc.
# install.packages("openxlsx")
library(openxlsx)


# Primera parte -----------------------------------------------------------

## importar los datos del Titanic
titanic <- read.csv("Sesion2_Material/Datos/titanic.csv")
dim(titanic)
names(titanic)
head(titanic)

## test para la media de una poblacion (t-test)
## H0: La edad media de los pasajeros y tripulación del Titanic fue de 35 años
t_res <- t.test(titanic$Age, mu = 35)
t_res
## test unilateral, H0: la edad media de los pasajeros fue mayor o igual que 32 años
t_res2 <- t.test(titanic$Age, mu = 32, alternative = "less")
t_res2


## test de igualdad de dos medias dos muestras independientes
## H0: lxs pasajerxs que sobrevivieron pagaron, en promedio, tickets más caros
##     que los que no sobrevivieron
## - Creo la variable Survived_cat (la version "etiquetada/categorizada")
titanic$Survived_cat <- ifelse(titanic$Survived == 0, "Not Survived", "Survived")
# titanic <- titanic |> 
#   mutate(Survived_cat = case_when(Survived == 0 ~ "Not Survived",
#                                   Survived == 1 ~ "Survived"))
## - Chequeo la asunción de normalidad     
Fare_NSurv <- titanic$Fare[which(titanic$Survived_cat == "Not Survived")]
Fare_Surv  <- titanic$Fare[which(titanic$Survived_cat == "Survived")]
shapiro.test(x = Fare_NSurv)
shapiro.test(x = Fare_Surv)
## - Aplico el test adecuado (U de Mann-Whitney, unilateral)
u_res <- wilcox.test(x = Fare_NSurv,
                     y = Fare_Surv,
                     paired = FALSE,
                     alternative = "less")
u_res


## test de más de dos medias de muestras indep.
## H0: la tipología de lxs pasajerxs (viajerxs solos, parejas y familias) es 
##     igual en todos los puertos de embarque
SibSbC <- titanic$SibSp[which(titanic$Embarked == "Cherbourg")]
SibSbQ <- titanic$SibSp[which(titanic$Embarked == "Queenstown")]
SibSbS <- titanic$SibSp[which(titanic$Embarked == "Southampton")]

## - Chequeo la asunción de normalidad
shapiro.test(SibSbC)
# shapiro.test(SibSbQ)
# shapiro.test(SibSbS)
## - Aplicio el test adecuado (Kruskal-Wallis)
krus_res <- kruskal.test(SibSp ~ Embarked, data = titanic)
krus_res


## test de independencia (o de asociacion)
## H0: el sexo y la supervivencia son variables independientes
## - Calculo una tabla de contingencia
table(titanic$Sex, titanic$Survived_cat)
round(prop.table(table(titanic$Sex, titanic$Survived_cat), margin = 1)*100, 1)

## - Aplico el test adecuado
chisq.test(titanic$Sex, titanic$Survived_cat)

## - Categorizo la variable Age en 4 categorías (en 4 intervalos de dif longitud)
titanic$Age_cat <- cut(titanic$Age, breaks = c(0, 10, 20, 50, 80), include.lowest = T)
# titanic <- titanic |> 
#   mutate(Age_cat = cut(Age, breaks = c(0, 10, 20, 50, 80), include.lowest = T))
unique(titanic$Age_cat)
table(titanic$Age_cat)
is.na(titanic$Age_cat)
sum(is.na(titanic$Age_cat))
## Y planteo la hipótesis
## H0: Los niñxs tuvieron mayores tasas de supervivencia
## - Hago la tabla de contingencia
table(titanic$Age_cat, titanic$Survived_cat)
round(prop.table(table(titanic$Age_cat, titanic$Survived_cat), margin = 1)*100, 1)
## - Aplico el test adecuado
chisq.test(titanic$Age_cat, titanic$Survived_cat)

## Creare un subconjunto del titanic
idx <- which(is.na(titanic$Age)) ## los índices observaciones que no disponemos de su edad (Edad Missing = NA)
titanic_sub <- titanic[-idx, ]   ## me quedo con todos menos con el vector idx (filtro)
# titanic_sub <- titanic |> 
#   filter(!is.na(Age))
table(titanic_sub$Age_cat, titanic_sub$Survived_cat)
sum(is.na(titanic_sub$Age_cat))
chisq.test(titanic_sub$Age_cat, titanic_sub$Survived_cat) ## obtenemos el mismo resultado
?chisq.test ## Ver seccion "Details". (...) cases with missing values are removed (...)


# Segunda parte -----------------------------------------------------------


## importar datos
grades <- read.csv("Sesion2_Material/Datos/grades.csv")
dim(grades)
names(grades)

## ajustar modelo de regresion lineal: grade ~ studyTime
mylinearModel <- lm(grade ~ studyTime, data = grades)
mylinearModel
summary(mylinearModel)
coefficients(mylinearModel)

## ajustar una regresión lineal para el tiempo de estudio y la clase previa
## se debe cambiar priorClass a una variable tipo factor
grades$priorClass <- as.factor(grades$priorClass)

mylinearModel2 <- lm(grade ~ studyTime + priorClass, data = grades)
summary(mylinearModel2)

## imoprtar nueva base de datos (caffeine)
## Archivo en formato rds. Para importar readRDS(), para exportar/guardar saveRDS()
caffeine <- readRDS("Sesion2_Material/Datos/caffeine.rds")
dim(caffeine)
names(caffeine)
View(caffeine)

## ajustar un modelo de regresion lineal con cafeina como variable independiente
lmResultCafeina <- lm(HablarPubl ~ Cafeina, data = caffeine)
summary(lmResultCafeina)

## introducir la variable Ansiedad al modelo
lmResultCafAnx <- lm(HablarPubl ~ Cafeina + Ansiedad, data = caffeine)
summary(lmResultCafAnx)

## introducir la interaccion Cafeina * Ansiedad
lmResultInteraccion <- lm(
  HablarPubl ~ Cafeina + Ansiedad + Cafeina * Ansiedad,
  data = caffeine
)
summary(lmResultInteraccion)


