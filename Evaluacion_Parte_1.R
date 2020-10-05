##Se cuenta con una base de datos formada por diferentes variables y resultados de las elecciones en municipios concretos.
##Con esta información, se busca predecir dos cosas teniendo información demográfica de estos municipios:
##Por un lado, el porcentaje de habitantes de un muncicipio que son de derechas a través de la variable % de derechas
## Por otro, si habrá una abstención alta en las elecciones en municipios concretos a través de la variable dicotómica de abstención alta, que toma valor 1 si la abstención es superior al 30% o 0 si es inferior al 30%


#Para ello empezaremos primero importando los datos para inspeccionar si se ha asignado correctamente el tipo de cada variable.
#Cargo las librerías necesarias para la depuración de datos y las funciones

setwd("/Users/mariayacobi/Desktop/Master_Clases/Mineria_de_datos_apuntes/Temario")
source("Funciones_R.R")

install.packages(c("car","questionr","psych","corrplot","readxl"))

library(questionr)
library(psych)
library(car)
library(corrplot)
library(readxl)

#Importo los datos:
datos<-read_xlsx("/Users/mariayacobi/Desktop/Master_Clases/Mineria_de_datos_apuntes/Mineria_de_datos_ejercicios/DatosEleccionesEspana.xlsx")

#Antes de nada, elimino las variables que no voy a utilizar. Como solo me voy a quedar con porcentaje de derechas y abstención alta, elimino porcentaje de abstención, porcentaje de izquierda, porcentaje otros, izquierda y derecha
#También elimino Name, ya que es el identificador y no me sirve para nada.
datos<-subset(datos,select = -c(AbstentionPtge,Izda_Pct,Otros_Pct,Izquierda,Derecha,Name))
str(datos)

#Tras ver más detenidamente los porcentajes de edad, veo que la variable Age_under19_Ptge ya incluye a todos entre 0 y 19, así que no necesito Age_0-4_Ptge. No me aporta más infor de la
#que ya tengo así que la elimino.
datos<-datos[,-(7)]
str(datos)

#Tras inspeccionar la asignación de variables tengo claro que la variable AbstenciónAlta tiene que transformarse en dicotómica y que la variable Densidad es factor.
#Sin embargo voy a confirmar que no se me escapa nada mirando el número de valores distintos de cada variable numérica
sapply(Filter(is.numeric,datos),function(x) length(unique(x)))
#Veo que en princpio ninguna de las variables clasificadas como numéricas actualmente tiene pocos valores únicos por lo que de momento se quedan así. Sin embargo, en el caso de código de provincia y CCAA, 
#creo que es necesario transformarlas en factor, ya que el código representa una categoría: a qué provincia pertenece el municipio. Además, para poder hacer esto utilizaremos un método de tramificación para reducir el número de factores

#Además, aunque actividad principal sea del tipo character, quiero ver cuántos valores diferentes tiene, ya que intuyo que podría ser factor, así como ocurre con CCAA.
length(unique(datos$ActividadPpal))
#Efectivamente tiene 5. Factor.
length(unique(datos$CCAA))
#Tiene 19, pero no hay más remedio que transformarla en factor. Luego tramificaremos.

summary(datos) 

#AÑADIR COMENTARIOS CUADERNO####Solo tengo que cambiar a factor las variables AbstencionAlta, Densidad, Actividad Principal, CCAA y Provincia. Además, tras ver resúmenes estadísticos básicos de
#cada variable con la función summary, obtengo pistas de cosas que puedo depurar en este proceso. 
#Por un lado, veo que algunas variables como las de desempleo tienen de valor máximo 100%, lo cual es extraño. Estos podrían ser outliers.
#Otras variables que pueden tener outliers porque parecen tener una distribución con mucha cola son las de número de empresas,población o censo y explotaciones. En este caso voy a ver ahora qué distribución tienen con un boxplot
#También veo valores faltantes en muchos casos (declarados y no declarados) y valores fuera de rango en la variable SameComAutonPtge

#Para ver la distribución hago un boxplot de estas variables que he mencionado anteriormente
#También utilizo esta función para ver otros estadísticos interesantes de los datos
psych::describe(Filter(is.numeric,datos))

#Al observar la asimetría de cada variable, vemos que hay algunas variables extremadamente asimétricas, como población, censo, total de empresas (y todas las variables de número de empresas por sector) y número de inmuebles.
#Realmente casi todas muestran algo de asimetría aunque las mencionadas anteriormente son las que más asimetría presentan.
#Además, vemos muchos valores elevados de la medida de curtosis, especialmente en las variables que ya había mencionado anteriormente que sospechaba que
#podían tener bastante cola (población, censo, total de empresas y por tipo, etc). Posiblemente aprendamos más de lo que ocurre al estudiar la presencia de outliers en estas variables.

#Además, como ya habíamos visto con la función summary, hay datos faltantes.
any(is.na(datos))

#En conclusión vemos bastantes cosas que se pueden arreglar, datos faltantes, variables mal codificadas y outliers que inspeccionaremos más adelante.

##ARREGLO ERRORES

##Comenzamos con el arbol para CCAA y la variable objetivo continua

install.packages(c('rpart','rpart.plot'))
library(rpart)
library(rpart.plot)

varObjCont<-datos$Dcha_Pct
varObjBin<-datos$AbstencionAlta
#Ahora creo el árbol para crear los diferentes grupos a los que recategorizaré mi variable CCAA.
tree<-rpart(varObjCont~CCAA, data=datos)
rpart.plot(tree, box.palette="RdBu", shadow.col="gray", nn=TRUE)

#Se han creado cuatro grupos
datos$CCAA<-factor(datos$CCAA)
da<-cbind(var=datos$CCAA,tree=tree$where,obj=varObjCont)

aggregate(var~tree, data=da, mean)
aggregate(obj~tree, data=da, mean)
#Vemos que efectivamente hay diferencias entre las medias de votos a porcentajes de derecha en los cuatro grupos.

#Ahora factorizo y le cambio los niveles
#Para poder ver qué CCAA está en qué grupo, veo tree y observo que un grupo (2) es Cataluña y País Vasco, otro (6) es Andalucía, Asturias, Baleares, Canarias, Valencia, Extremadura y Navarra
#otro (14) es Aragón, Cantabria, Castilla la Mancha, Galicia, Madrid, Murcia y Rioja y un último grupo (15) que son CastillaLeón, Ceuta y Melilla
tree
#Ahora recodifico. He decidido crear una nueva variable para no machacar CCAA ya que viy a recategorizar la variable 2 veces: una en función de su respuesta frente a la binaria y otra en función de su respuesta frente a la continua ya que son variables diferentes y las CCAA se agruparán de forma diferente
datos$CCAACont<-factor(tree$where)
levels(datos$CCAACont)[levels(datos$CCAACont)=="2"] <- "Cat-PVasco"
levels(datos$CCAACont)[levels(datos$CCAACont)=="4"] <- "And-CVal-Ext-Bal-Cana-Ast-Nav"
levels(datos$CCAACont)[levels(datos$CCAACont)=="6"] <- "CMancha-Gal-Arag-Rioja-Mad-Mur-Cant"
levels(datos$CCAACont)[levels(datos$CCAACont)=="7"] <- "CLeon-Ceuta-Melilla"

##A continuación voy a hacer lo mismo para mi variable obj binaria y CCAA para usarla más adelante en la regresión logística
tree2<-rpart(varObjBin~CCAA, data=datos)
rpart.plot(tree2, box.palette="RdBu", shadow.col="gray", nn=TRUE)
#Se han creado tres grupos
datos$CCAA<-factor(datos$CCAA)
da2<-cbind(var=datos$CCAA,tree=tree2$where,obj=varObjBin)
##COMO COÑO INTERPRETO ESTO
aggregate(var~tree, data=da2, mean)
aggregate(obj~tree, data=da2, mean)

#Ahora factorizo y le cambio los niveles
#Para poder ver qué CCAA está en qué grupo, veo tree2 y observo que 
tree2
#Ahora recodifico. He decidido crear una nueva variable para no machacar CCAA ya que viy a recategorizar la variable 2 veces: una en función de su respuesta frente a la binaria y otra en función de su respuesta frente a la continua ya que son variables diferentes y las CCAA se agruparán de forma diferente
datos$CCAABin<-factor(tree2$where)
levels(datos$CCAABin)[levels(datos$CCAABin)=="2"] <- "Arag-Cant-CLeon-CMancha-CVal-Ext-Mad-Mur-Rioja"
levels(datos$CCAABin)[levels(datos$CCAABin)=="4"] <- "And-Gal-Nav-PVasco"
levels(datos$CCAABin)[levels(datos$CCAABin)=="5"] <- "Ast-Bal-Cana-Cat-Ceuta-Melilla"

##A continuación, voy a hacer exactamente lo mismo para la variable CodigoProvicia
tree3<-rpart(varObjCont~CodigoProvincia, data=datos)
rpart.plot(tree3, box.palette="RdBu", shadow.col="gray", nn=TRUE)
#Se han creado varios grupos
datos$CodigoProvincia<-factor(datos$CodigoProvincia)
da3<-cbind(var=datos$CodigoProvincia,tree=tree3$where,obj=varObjCont)
aggregate(var~tree, data=da3, mean)
aggregate(obj~tree, data=da3, mean)

#No se si me va a merecer la pena incluir la variable CodigoProvincia, ya que lo mejor que ha podido hacer el árbol es reducir
#a 22 grupos que siguen siendo demasiados... Además, en cierto sentido CCAA y Provincia están relacionadas, ya que la provincia es una
#subcategoría de CCAA. De momento la voy a obviar.
datos<-subset(datos,select = -c(CodigoProvincia))
str(datos)

#Una vez que tengo estas variables recategorizadas y convertidas a factor, puedo continuar con el resto de variables en las que he encontrado errores. Empezamos por convertir a factor aquellas variables cuyo tipo estaba mal codificado
#Concretamente, estas son: Abstención Alta, Densidad y Actividad Principal.
datos[,c(4,26,30)]<-lapply(datos[,c(4,26,30)],factor)
summary(datos$AbstencionAlta)
summary(datos$Densidad)
summary(datos$ActividadPpal)
#Ya hemos reconvertido estas variables a factor. Además, veo que tengo algún missing no declarado en Densidad. Los cambio a declarados
datos$Densidad<-recode.na(datos$Densidad,'?')

#Al hacer el summary anteriormente, también he encontrado otras cosillas, como valores fuera de rango, valores missing declarados y outliers. Empiezo ahora por arreglar los valores fuera de rango
#Para empezar, vemos que SameComAutonPtge tiene un valor de 127%, lo cual no debería ser. Además, ForeignersPtge tiene también un valor fuera de rango: un número negativo que en este caso no debería ser. 
#Arreglo los dos
datos$ForeignersPtge<-replace(datos$ForeignersPtge,which(datos$ForeignersPtge<0),NA)
datos$SameComAutonPtge<-replace(datos$SameComAutonPtge,which(datos$SameComAutonPtge>100),NA)
summary(datos$ForeignersPtge)
summary(datos$SameComAutonPtge)

##ESTUDIO DE OUTLIERS
#Antes de seguir, separo las variables objetivo de las input
varObjCont<-datos$Dcha_Pct
varObjBin<-datos$AbstencionAlta
inputdatos<-as.data.frame(datos[,-(4:5)])

#Calculo primero el porcentaje de atípicos de cada variable. Veo muchas con un porcentaje muy bajo y otras que habría que examinar, aunque parece que ninguna supera el 11% de la variable servicios, por lo que los podemos gestionar transformando esos outliers en missings
sapply(Filter(is.numeric, inputdatos),function(x) atipicosAmissing(x)[[2]])/nrow(inputdatos)
inputdatos[,as.vector(which(sapply(inputdatos, class)=="numeric"))]<-sapply(Filter(is.numeric, inputdatos),function(x) atipicosAmissing(x)[[1]])
sum(is.na(inputdatos))
#Por qué he decidido cargarme los outliers? Dice la literatura que afectan a la media, desviación típica etc y pierdo R2. Es verdad que en mi modelo de
#predicción, cuando tenga una caso real de un municipio como Madrid con 3 millones de habitantes, el modelo no va a poder predecirlo. Pero la mayoría de casos
#son municipios con mucha menor población, por lo que prefierto tener una buena capacidad de predicción para
#la mayoría de situaciones que se dan en este caso concreto.

#TRATAMIENTO DE VALORES MISSINGS
#A continuación realizaré un tratamiento de los valores faltantes en el dataset. Para ello, comienzo buscando un patrón en los missings.
corrplot(cor(is.na(inputdatos[colnames(inputdatos)[colSums(is.na(inputdatos))>0]])),method = "ellipse",type = "upper")
#Vemos que hay algo de correlación positiva (en muchos casos, una alta correlación positiva) entre los valores missing de
#las variables población y total de empresas (y población y cada una de los variables de número de empresas por sector), así como
#entre censo y total de empresas (y censo y cada una de las variables de número de empresas por sector) y entre las variables de número de empresas por sector.

#A continuación, veremos la proporción de missings por variable y observación
inputdatos$prop_missings<-apply(is.na(inputdatos),1,mean)
summary(inputdatos$prop_missings)

(prop_missingsVars<-apply(is.na(inputdatos),2,mean))
#En este caso vemos unas cuantas variables que rondan o superan el 10% de observaciones missing, aunque ninguna super el 50% por lo que no debo eliminar nada
#En el caso de las observaciones, el valor máximo es del 35.2% por lo que tampoco ejecuto el código para eliminar observaciones.

#Como no hemos eliminado variables ni observaciones, a continuación vamos a imputarlas utilizando el método aleatorio.
inputdatos[,as.vector(which(sapply(inputdatos, class)=="numeric"))]<-sapply(Filter(is.numeric, inputdatos),function(x) Hmisc::impute(x,"random"))

#Ahora imputo las cualitativas y aseguro que siguen siendo factores
inputdatos[,as.vector(which(sapply(inputdatos, class)=="factor"))]<-sapply(Filter(is.factor, inputdatos),function(x) ImputacionCuali(x,"aleatorio"))
inputdatos[,as.vector(which(sapply(inputdatos, class)=="character"))] <- lapply( inputdatos[,as.vector(which(sapply(inputdatos, class)=="character"))] , factor)

#Reviso que no queden missings
summary(inputdatos)
any(is.na(inputdatos))

par(mfrow=c(3,3)) 
dfplot(inputdatos)

#Guardo los datos depurados. Antes quito CCAA, ya que tengo mis dos variables de CCAA, una para cada variable objetivo
saveRDS(cbind(varObjBin,varObjCont,inputdatos),"datosEleccionesDep")

##EXPLORACIÓN VISUAL DE LOS DATOS
#Para comenzar con la regresión lineal, cargo los paquetes necesarios y los datos depurados (solo estos. questionr, psych y corrplot ya están cargados de la depuración)
install.packages(c("caret","ggplot2","lmSupport"))
library(caret)
library(ggplot2)
library(lmSupport)

#Antes quito CCAA, ya que ya tengo mis dos variables de CCAA, una para cada variable objetivo
EleccionesDep<-readRDS("datosEleccionesDep")
EleccionesDep<-EleccionesDep[,-(3)]

#Vuelvo a separar las target de las input y genero dos variables aleatorias para ayudarme a discernir entre las variables 
#Que realmente tienen una relación con la target
EleccionesObjCont<-EleccionesDep$varObjCont
EleccionesObjBin<-EleccionesDep$varObjBin
inputElecciones<-EleccionesDep[,-(1:2)]

inputElecciones$aleatorio1<-runif(nrow(inputElecciones))
inputElecciones$aleatorio2<-runif(nrow(inputElecciones))

#Ahora que ya tengo estos pasos previos completados, comienzo con el análisis descriptivo de las variables, empezando por
#gráficos que representan la V de cramer para todas las variables input contra las targets (tanto la binaria como la continua)
graficoVcramer(inputElecciones,EleccionesObjCont)

##Bajo este criterio podemos observar que las 5 variables más importantes que explican la variable Dcha_Ptge son:
#1Comunidad autónoma (la pensada para continua)
#2Porcentaje de menores de 19 años
#3Porcentaje de mayores de 65 años
#4Número medio de personas por inmueble
#5Tasa de desempleo para mayores de 40 años
##Otras que pueden tener algo de importancia son aquellas relacionadas con el porcentaje de desempleo (en el sector servicios y en el tramo de edad entre 25 y 40) así como la actividad principal y la densidad de población.

graficoVcramer(inputElecciones,EleccionesObjBin)

#En cuanto a la variable binaria, bajo este criterio vemos que pueden tener importancia las siguientes cinco variables:
#1Comunidad autónoma (la pensada para binaria)
#2Porcentaje de menores de 19 años
#3Actividad principal
#4Porcentaje de habitantes mayores de 65 años
#5Número de empresas en el sector industrial.
#Además, en este caso vemos que las relacionadas con el desempleo no parecen tener tanta importancia como en el caso de la target continua

#A continuación comienzo con la exploración visual de los datos. Primero hago gráficos de mosaico para visualizar
#las variables input categóricas frente a la variable objetivo binaria.
par(mfrow=c(1,2))
mosaico_targetbinaria(inputElecciones$CCAABin,EleccionesObjBin,"CCAA") #Solo hago en este caso con CCAABin ya que estoy mirando cómo influyen frente a la binaria. Sí influye
mosaico_targetbinaria(inputElecciones$Densidad,EleccionesObjBin,"Densidad de población") #Esta parece que también influye: vemos menos abstención a medida que aumenta la densidad de población
mosaico_targetbinaria(inputElecciones$ActividadPpal,EleccionesObjBin,"Actividad Principal") #También parece que tenga influencia. En aquellos municipios con un sector u otro de actividad principal podemos esperar ver una respuesta diferente.

#A continuación hago lo mismo pero por barras
barras_targetbinaria(inputElecciones$ActividadPpal,EleccionesObjBin,"Act.Ppal") #Bastante diferencia en la distribución de categorías de actividad principal y abstención alta
barras_targetbinaria(inputElecciones$Densidad,EleccionesObjBin,"Densidad") #En este caso no me queda tan claro.Es verdad que vemos que en municipios de densidad muy baja hay más de la mitad de los municipios tienen abstención alta y en densidad Alta al revés pero la distribución parece estar más equilibrada que en el caso anterior.
barras_targetbinaria(inputElecciones$CCAABin,EleccionesObjBin,"CCAA") #En este caso sí que veo bastante diferencia.

#Ahora voy a analizar visualmente el comportamiento de las variables numéricas frente a la variable objetivo binaria mediante boxplots e histogramas
boxplot_targetbinaria(inputElecciones$Population,EleccionesObjBin,"Población")
boxplot_targetbinaria(inputElecciones$TotalCensus,EleccionesObjBin,"Censo")
boxplot_targetbinaria(inputElecciones$Age_under19_Ptge,EleccionesObjBin,"% de Población menores de 19 años")
boxplot_targetbinaria(inputElecciones$Age_19_65_pct,EleccionesObjBin,"% de Población entre 19 y 65 años")
boxplot_targetbinaria(inputElecciones$Age_over65_pct,EleccionesObjBin,"% de Población mayores de 65 años")
boxplot_targetbinaria(inputElecciones$WomanPopulationPtge,EleccionesObjBin,"% de Mujeres")
boxplot_targetbinaria(inputElecciones$ForeignersPtge,EleccionesObjBin,"% de Extranjeros")
boxplot_targetbinaria(inputElecciones$SameComAutonPtge,EleccionesObjBin,"SameComAuton%")
boxplot_targetbinaria(inputElecciones$SameComAutonDiffProvPtge,EleccionesObjBin,"SameComAutonDiffProv%")
boxplot_targetbinaria(inputElecciones$DifComAutonPtge,EleccionesObjBin,"DiffComAuton%")
boxplot_targetbinaria(inputElecciones$UnemployLess25_Ptge,EleccionesObjBin,"Tasa de desempleo menores de 25 años")
boxplot_targetbinaria(inputElecciones$Unemploy25_40_Ptge,EleccionesObjBin,"Tasa de desempleo entre 25 y 40 años")
boxplot_targetbinaria(inputElecciones$UnemployMore40_Ptge,EleccionesObjBin,"Tasa de desempleo mayores de 40 años")
boxplot_targetbinaria(inputElecciones$AgricultureUnemploymentPtge,EleccionesObjBin,"Tasa de desempleo sector agricultura")
boxplot_targetbinaria(inputElecciones$IndustryUnemploymentPtge,EleccionesObjBin,"Tasa de desempleo sector industria")
boxplot_targetbinaria(inputElecciones$ServicesUnemploymentPtge,EleccionesObjBin,"Tasa de desempleo sector servicios")
boxplot_targetbinaria(inputElecciones$ConstructionUnemploymentPtge,EleccionesObjBin,"Tasa de desempleo sector construcción")
boxplot_targetbinaria(inputElecciones$totalEmpresas,EleccionesObjBin,"Número total de empresas")
boxplot_targetbinaria(inputElecciones$Industria,EleccionesObjBin,"Número de empresas sector industria")
boxplot_targetbinaria(inputElecciones$Servicios,EleccionesObjBin,"Número de empresas sector servicios")
boxplot_targetbinaria(inputElecciones$Construccion,EleccionesObjBin,"Número de empresas sector construcción")
boxplot_targetbinaria(inputElecciones$ComercTTEHosteleria,EleccionesObjBin,"Número de empresas sector ComerTTEHosteleria")
boxplot_targetbinaria(inputElecciones$inmuebles,EleccionesObjBin,"Número de inmuebles")
boxplot_targetbinaria(inputElecciones$Pob2010,EleccionesObjBin,"Población en 2010")
boxplot_targetbinaria(inputElecciones$SUPERFICIE,EleccionesObjBin,"Superficie")
boxplot_targetbinaria(inputElecciones$PobChange_pct,EleccionesObjBin,"Tasa de crecimiento de población")
boxplot_targetbinaria(inputElecciones$PersonasInmueble,EleccionesObjBin,"Número medio de personas por inmueble")
boxplot_targetbinaria(inputElecciones$Explotaciones,EleccionesObjBin,"Número de explotaciones agrícolas")

hist_targetbinaria(inputElecciones$Population,EleccionesObjBin,"Población")
hist_targetbinaria(inputElecciones$TotalCensus,EleccionesObjBin,"Censo")
hist_targetbinaria(inputElecciones$Age_under19_Ptge,EleccionesObjBin,"% de Población menores de 19 años")
hist_targetbinaria(inputElecciones$Age_19_65_pct,EleccionesObjBin,"% de Población entre 19 y 65 años")
hist_targetbinaria(inputElecciones$Age_over65_pct,EleccionesObjBin,"% de Población mayores de 65 años")
hist_targetbinaria(inputElecciones$WomanPopulationPtge,EleccionesObjBin,"% de Mujeres")
hist_targetbinaria(inputElecciones$ForeignersPtge,EleccionesObjBin,"% de Extranjeros")
hist_targetbinaria(inputElecciones$UnemployLess25_Ptge,EleccionesObjBin,"% de desempleo (-25)")
hist_targetbinaria(inputElecciones$Unemploy25_40_Ptge,EleccionesObjBin,"% de desempleo (25-40)")
hist_targetbinaria(inputElecciones$UnemployMore40_Ptge,EleccionesObjBin,"% de desempleo (+40)")
hist_targetbinaria(inputElecciones$SameComAutonPtge,EleccionesObjBin,"% de habitantes que viven en la misma CCAA en la que nacieron")
hist_targetbinaria(inputElecciones$SameComAutonDiffProvPtge,EleccionesObjBin,"% de habitantes que viven en la misma CCAA en la que nacieron (distinta prov")
hist_targetbinaria(inputElecciones$DifComAutonPtge,EleccionesObjBin,"% de habitantes que viven en distinta CCAA de la que nacieron")
hist_targetbinaria(inputElecciones$AgricultureUnemploymentPtge,EleccionesObjBin,"% de desempleo sector agricultura")
hist_targetbinaria(inputElecciones$IndustryUnemploymentPtge,EleccionesObjBin,"% de desempleo sector industria")
hist_targetbinaria(inputElecciones$ConstructionUnemploymentPtge,EleccionesObjBin,"% de desempleo sector construcción")
hist_targetbinaria(inputElecciones$ServicesUnemploymentPtge,EleccionesObjBin,"% de desempleo sector servicios")
hist_targetbinaria(inputElecciones$totalEmpresas,EleccionesObjBin,"Total de empresas")
hist_targetbinaria(inputElecciones$Industria,EleccionesObjBin,"Total de empresas sector industria")
hist_targetbinaria(inputElecciones$Construccion,EleccionesObjBin,"Total de empresas sector construcción")
hist_targetbinaria(inputElecciones$ComercTTEHosteleria,EleccionesObjBin,"Total de empresas sector Comenrcio, Tte y Hostelería")
hist_targetbinaria(inputElecciones$Servicios,EleccionesObjBin,"Total de empresas sector servicios")
hist_targetbinaria(inputElecciones$inmuebles,EleccionesObjBin,"Número total de inmuebles")
hist_targetbinaria(inputElecciones$Pob2010,EleccionesObjBin,"Población en 2010")
hist_targetbinaria(inputElecciones$SUPERFICIE,EleccionesObjBin,"Superficie")
hist_targetbinaria(inputElecciones$Explotaciones,EleccionesObjBin,"Explotaciones")

#Conclusiones sobre histogramas y boxplot: en población no veo una diferencia significativa en el comportamiento de la variable
#y abstención alta. Censo igual. En el caso de % de menores de 19, no es muy marcada la diferencia, pero cuando el porcentaje de pob
#se sitúa entre 0 y 15, hay menor incidencia de abstención alta, mientras que entre 15 y 25 hay mayor incidencia de abstención alta.
#En el caso de % entre 19 y 65 sí vemos que entre el 55 y el 65 % hay mayor incidencia de abstención alta. También en el caso de m% de mayores de 65
#vemos que entre un 15 y un 25% hay mayor incidencia de abstención alta y del 25 al 60% menor incidencia de abstención alta. En el caso de
#% de mujeres no veo una diferencia significativa. En % de extranjeros tampoco.Ni % de desempleo en menores de 25.En ninguna de desempleo por tramos de edad
#Hay diferencias. Tampoco diferencias de comportamiento en las de SameCom....Ninguna diferencia en variables de desempleo por sector ni en las variables de 
#numero total de empresas y por sector (numero total de empresas). en el resto ninguna diferencia

#A continuación, estudio visualmente la relación entre variables input y la objetivo continua. Empiezo por ver las numéricas frente a la obj continua. Luego categóricas frente a obj continua. 
##graficoCorrelacion(EleccionesObjCont,inputElecciones) No me funciona
corrplot(cor(cbind(EleccionesObjCont,Filter(is.numeric, inputElecciones)), use="pairwise", method="pearson"), method = "number",type = "upper",tl.cex = 0.3,number.cex = 0.5)

#Vemos que ninguna variable tiene una gran correlación con el porcentaje de votos a partidos de derecha, aunque de mayor a menor correlación tenemos: (apuntes cuaderno)
#Me preocupa más que en varios casos hay una muy alta correlación entre predictores. Para ello vuelvo a aplicar el gráfico de correlación
#pero esta vez agrupando por grupos de correlación
corMat <- cor(Filter(is.numeric, inputElecciones))
corrplot(corMat, order = "hclust", tl.cex = 0.7) 
#Busco exactamente estas variables (las que tengan un grado de correlación superior a 0.8)
highlyCor <- colnames(inputElecciones)[findCorrelation(corMat, cutoff = 0.8, verbose = TRUE)]
#Me las cargo tranquilamente.
input_cor <- inputElecciones[, which(!colnames(inputElecciones) %in% highlyCor)]
ncol(input_cor)
corrplot(cor(Filter(is.numeric,input_cor)), order = "hclust", tl.cex = 0.7)
str(input_cor)
#No estoy del todo satisfecha con como me ha gestionado la colinealidad esto. Las variables que más me preocupan son censo-población, los tramos de edades y las variables que indican el porcentaje
#de habitantes que son de la misma provincia, misma CCAA y diferente provincia y diferente CCAA. Me gusta que haya quitado población, porque me quedo con censo y porcentaje de cambio de población y pob 2010
#que me da una imagen completa, pero con edad, me ha dejado solo una: % de 19 a 65 años, la cual es la única de las tres de tramos de edad que no aparece en mi top five de importancia
#para las target en mis gráficos V de cramer. Por eso, manualmente voy a quitar población, % entre 19 y 65 y una de las de Provincia y comunidad autónoma (en estos dos últimos casos me quedo con dos variables de tres, ya que solo es necesario quitar una para librarnos de la colinealidad)
input_cor<-inputElecciones[,-c(1,4,8)]
corrplot(cor(Filter(is.numeric,input_cor)), order = "hclust", tl.cex = 0.7)

#Ahora vemos obj continua vs categóricas con boxplots. He decidido usar las funciones de boxplot e histograma creadas para ver visualmente
#la target binaria frente a los predictores categóricos porque el principio es el mismo, cambiando de orden las variables: pongo la continua en el eje x y la categórica n el eje y.
boxplot_targetbinaria(EleccionesObjCont,inputElecciones$CCAACont,"% votos partidos de derecha") #mucha diferencia de comportamineto
hist_targetbinaria(EleccionesObjCont,inputElecciones$CCAACont,"% votos partidos de derecha")#Muchas diferencia 
boxplot_targetbinaria(EleccionesObjCont,inputElecciones$ActividadPpal,"% votos partidos de derecha")
hist_targetbinaria(EleccionesObjCont,inputElecciones$ActividadPpal,"% votos partidos de derecha")
boxplot_targetbinaria(EleccionesObjCont,inputElecciones$Densidad,"% votos partidos de derecha")
hist_targetbinaria(EleccionesObjCont,inputElecciones$Densidad,"% votos partidos de derecha")

#TRANSFORMACIÓN DE VARIABLES
#Antes de comenzar la regresión voy a aplicar transformaciones a mis variables para ver si alguna de ellas no tiene una relación lineal con la target. Primero quito proporción de missings que no me sirve para mucho
input_cor<- input_cor[,-(30)]
input_cont<-cbind(input_cor,Transf_Auto(Filter(is.numeric, input_cor),EleccionesObjCont))
sapply(Filter(is.numeric, input_cor)[,-ncol(Filter(is.numeric, input_cor))],function(x) length(unique(x)))

input_bin<-cbind(input_cor,Transf_Auto(Filter(is.numeric, input_cor),EleccionesObjBin)) # Quito proporción de
saveRDS(data.frame(input_bin,EleccionesObjBin),"todo_bin_e")
saveRDS(data.frame(input_cont,EleccionesObjCont),"todo_cont_e")

#REGRESIÓN LINEAL
#Para hacer la regresión lineal, primero voy a hacer varios modelos con el método forward hasta seleccionar el que mejor funcione de los mejores con validación cruzada.
# Cargo las librerias que me van a hacer falta
library(glmnet)

#Cargo los datos depurados. Además, le quito la variable CCAABin que era específica para la regresión logística
datoscont<-readRDS("todo_cont_e") 
datoscont<-subset(datoscont,select = -c(CCAABin)) #Estos datos los usaré para la generación automática de modelos. En la parte manual, uso input_cor que contiene los datos originales.
datosorig <- data.frame(EleccionesObjCont,input_cor[,-(29)])

#Hago la partición
set.seed(12345678)
trainIndex <- createDataPartition(datosorig$EleccionesObjCont, p=0.8, list=FALSE)
data_train <- datosorig[trainIndex,]
data_test <- datosorig[-trainIndex,]

#Método manual. Empiezo con el modelo completo. Después introduciré poco a poco las que parezcan explicar más el modelo según VCramer
modelomanual1<-lm(EleccionesObjCont~.,data=data_train)
summary(modelomanual1)
#En este modelo no veo una mala r2 pero hay muchísimas variables que parecen tener un efecto poco significativo en la predicción.
Rsq(modelomanual1,"EleccionesObjCont",data_train)
Rsq(modelomanual1,"EleccionesObjCont",data_test)
modelEffectSizes(modelomanual1)
barplot(sort(modelEffectSizes(modelomanual1)$Effects[-1,4],decreasing =T),las=2,main="Importancia de las variables (R2)")
#Empiezo metiendo solo las top 5 según VdeCramer, a ver qué tal sale
modelomanual2<-lm(EleccionesObjCont~CCAACont+Age_over65_pct+Age_under19_Ptge+UnemployMore40_Ptge+PersonasInmueble,data=data_train)
summary(modelomanual2)
Rsq(modelomanual2,"EleccionesObjCont",data_train)
Rsq(modelomanual2,"EleccionesObjCont",data_test)
#Peores resultados. Aunque estas variables me salían como importantes en la exploración de datos, en el primer modelo completo muchas de ellas no eran significativas como porcentaje de menores
#de 19 años, y no me salían en el top 5 algunas que sí parecían significativas en el modelo completo. Voy a hacer un modelo teniendo en cuenta solo estas que considero significativas según el modelo completo.
modelomanual3<-lm(EleccionesObjCont~CCAACont+Age_over65_pct+ForeignersPtge+AgricultureUnemploymentPtge+IndustryUnemploymentPtge+ServicesUnemploymentPtge+ActividadPpal+Densidad,data=data_train)
summary(modelomanual3) #En este modelo todas son bastante significativas.
Rsq(modelomanual3,"EleccionesObjCont",data_train)
Rsq(modelomanual3,"EleccionesObjCont",data_test)#No hay mucha diferencia entre train y test en mi r2 y es un poco mejor que el anterior

#Son muchas variables y cuesta a veces ver el efecto que tiene meter y quitarlas del modelo. Voy a proceder a realizar una selección automática de variables y luego 
#testearé los mejores modelos (de la parte manual, me quedo con el último, que tiene buen R2 y no tantas variables como el modelo completo)

#Vuelvo a hacer la partición test para usar todo_cont_e que tiene las transformaciones también y así lo hago todo del tirón. He decidido hacer la selección automática con el método stepwise mirando
#Siempre tanto el criterio de akaike como el criterio de información bayesiano para elegir el mejor modelo que me saque del mejor de cada método
set.seed(12345678)
trainIndex <- createDataPartition(datoscont$EleccionesObjCont, p=0.8, list=FALSE)
data_train <- datoscont[trainIndex,]
data_test <- datoscont[-trainIndex,]

#Empezamos con una selección de variables con las variables originales: ninguna transformación ni interacción.
null<-lm(EleccionesObjCont~1, data=data_train) #Modelo minimo
full<-lm(EleccionesObjCont~., data=data_train[,c(1:30,58)]) #Modelo maximo, le quitamos las transformaciones
modeloStepAIC<-step(null, scope=list(lower=null, upper=full), direction="both")
summary(modeloStepAIC)
Rsq(modeloStepAIC,"EleccionesObjCont",data_test)
#Tiene buen R2 aunque se puede mejorar, aunque en data_test es un poco peor

modeloStepBIC<-step(null, scope=list(lower=null, upper=full), direction="both",k=log(nrow(data_train)))
summary(modeloStepBIC)
Rsq(modeloStepBIC,"EleccionesObjCont",data_test)

modeloStepAIC$rank
modeloStepBIC$rank
#El modeloStepBIC tiene peor r2 pero lo prefiero porque la diferencia en R2 no es tan grande como para asumir 9 parámetros de más en el modeloStepAIC.

#Interacciones
formInt<-formulaInteracciones(datoscont[,c(1:30,58)],31)
fullInt<-lm(formInt,data = data_train)

modeloStepAIC_int<-step(null, scope=list(lower=null, upper=fullInt), direction="both")
summary(modeloStepAIC_int)
Rsq(modeloStepAIC_int,"EleccionesObjCont",data_test)#Tiene un muy buen r2 en el summary pero hay diferencia con test.

modeloStepBIC_int<-step(null, scope=list(lower=null, upper=fullInt), direction="both",k=log(nrow(data_train)))
summary(modeloStepBIC_int)
Rsq(modeloStepBIC_int,"EleccionesObjCont",data_test)#El r2 es un pelín peor que en el anterior y la sigue habiendo diferencia con test.

modeloStepAIC_int$rank
modeloStepBIC_int$rank
#Prefiero el modeloStepBIC_int porque tiene menos parámetros (muchos menos)

#Transformaciones
fullT<-lm(EleccionesObjCont~., data=data_train)

modeloStepAIC_trans<-step(null, scope=list(lower=null, upper=fullT), direction="both")
summary(modeloStepAIC_trans)
Rsq(modeloStepAIC_trans,"EleccionesObjCont",data_test)#Tenemos un R2 un poco peor que con interacciones pero no hay casi diferencia con data_test.

modeloStepBIC_trans<-step(null, scope=list(lower=null, upper=fullT), direction="both",k=log(nrow(data_train)))
summary(modeloStepBIC_trans)
Rsq(modeloStepBIC_trans,"EleccionesObjCont",data_test) #R2 un poco peor que el anterior (aunque prácticamente igual) y poquísima diferencia con data_test.

modeloStepAIC_trans$rank 
modeloStepBIC_trans$rank
#Prefiero modeloStepBIC_trans porque tiene menos parámetros

#Transformaciones e interacciones
formIntT<-formulaInteracciones(datoscont,58)
fullIntT<-lm(formIntT, data=data_train)

modeloStepAIC_transInt<-step(null, scope=list(lower=null, upper=fullIntT), direction="both")
summary(modeloStepAIC_transInt)
Rsq(modeloStepAIC_transInt,"EleccionesObjCont",data_test) #Un R2 muy alto en summary pero luego vemos que hay bastante diferencia con test.

modeloStepBIC_transInt<-step(null, scope=list(lower=null, upper=fullIntT), direction="both",k=log(nrow(data_train)))
summary(modeloStepBIC_transInt)
Rsq(modeloStepBIC_transInt,"EleccionesObjCont",data_test) #R2 peor que el anterior, pero bueno. Además no hay mucha diferencia con test.

modeloStepAIC_transInt$rank 
modeloStepBIC_transInt$rank #obviamente... este

#Validación cruzada repetida
#Cojo los mejores modelos de cada uno de los pasos anteriores y el modelomanual3 y aplico validación cruzada repetida para elegir el mejor

total<-c()
modelos<-sapply(list(modelomanual3,modeloStepBIC,modeloStepBIC_int,modeloStepBIC_trans,modeloStepBIC_transInt),formula)
for (i in 1:length(modelos)){
  set.seed(1712)
  vcr<-train(as.formula(modelos[[i]]), data = data_train,
             method = "lm",
             trControl = trainControl(method="repeatedcv", number=5, repeats=20,
                                      returnResamp="all")
  )
  total<-rbind(total,cbind(vcr$resample[,1:2],modelo=rep(paste("Modelo",i),
                                                         nrow(vcr$resample))))
}

boxplot(Rsquared~modelo,data=total,main="R-Square") #El 3(modeloStepBIC_int) y el 5(modeloStepBIC_transInt) parecen los mejores. 
aggregate(Rsquared~modelo, data = total, mean) #No hay mucha diferencia entre el R2 de estos dos modelos
aggregate(Rsquared~modelo, data = total, sd) #En variabilidad son parecidos también, aunque también se parecen a los dos primeros en este sentido.

#Ahora compruebo el número de parámetros de estos dos modelos preseleccionados
length(coef(modeloStepBIC_int))
length(coef(modeloStepBIC_transInt))
formula(modeloStepBIC_int)
formula(modeloStepBIC_transInt)

##MODELO GANADOR
#El que contiene transformaciones e interacciones (modeloStepBIC_transInt) tiene dos parámetros menos. Sin embargo, el aumento de R2 no es lo suficiente como
#para justificar el tener que interpretar variables transformadas con logaritmos, que es mucho más complicado. Me quedo con el modeloStepBIC_int. Mi argumentación en general es la siguiente:
#de todos los modelos generados (manuales y automáticos) me he quedado con los que tenían menos parámetros en cada caso. También he analizado la estabilidad
#de los modelos a través de su diferencia en R2 entre los datasets de train y test. En este caso tengo algo de diferencia pero no la suficiente como para preocuparme. 
#Además, todas las variables son bastante sigificativas en el modelo
Rsq(modeloStepBIC_int,"EleccionesObjCont",data_train)
Rsq(modeloStepBIC_int,"EleccionesObjCont",data_test) 
modelEffectSizes(modeloStepBIC_int)
barplot(sort(modelEffectSizes(modeloStepBIC_int)$Effects[-1,4],decreasing =T),las=2,main="Importancia de las variables (R2)")

##INTERPRETACIÓN DE COEFICIENTES DEL MODELO
coef(modeloStepBIC_int)
#Por último, para interpretar coeficientes, en este caso elijo para interpretar el coeficiente de WomanPopulationPtge y el coeficiente de CCAAContCat-PVasco.
#WomanPopulationPtge: si el porcentaje (%) de mujeres en un municipio aumenta un 1%, el porcentaje de votos a partidos de derecha disminuye un  10,7%. 
#CCAAContCat-PVasco: esta es un poco más compleja porque tiene interacciones, pero en el caso de que el municipio pertenezca a Cataluña o al País Vasco, el porcentaje de votos 
#a partidos de derecha disminuye un 40.75%, al que añadimos el efecto que aporta la interacción entre CCAAContCat-PVasco con el porcentaje de habitantes del municipio que nacieron en una
#CCAA diferente a la que pertenece el municipio, que por cada aumento unitario (un 1%) del porcentaje de habitantes que nacieron en una CCAA diferente a la del municipio y el municipio
#está en Cataluña o PVasco. Por lo que a ese 45,75% añadimos un 21,36% por cada aumento unitario de DifComAutonPtge.

##REGRESIÓN LOGÍSTICA
library(pROC)
datosbin<-readRDS("todo_bin_e")
str(datosbin)
freq(datosbin$EleccionesObjBin)
datosbin<-subset(datosbin,select = -c(CCAACont))#Quito CCAACont y me quedo con CCAABin

#Partición de los datos
set.seed(12345678)
trainIndex <- createDataPartition(datosbin$EleccionesObjBin, p=0.8, list=FALSE)
data_train <- datosbin[trainIndex,]
data_test  <- datosbin[-trainIndex,]
#Bien, ambos datasets tienen la misma distribución de 1 y 0 para la target.
freq(data_train$EleccionesObjBin) 
freq(data_test$EleccionesObjBin) 

#Como hemos visto antes, la selección automática de variables es mucho más eficiente que la manual y para poder ahorrar espacio para cosas más importantes en esta regresión
#logística voy a pasar directamente a la selección automática de variables
#Empezamos con una selección de variables con las variables originales: ninguna transformación ni interacción.
null<-glm(EleccionesObjBin~1, data=data_train, family = binomial) #Modelo minimo
full<-glm(EleccionesObjBin~., data=data_train[,c(1:30,58)],family = binomial) #Modelo maximo, le quitamos las transformaciones
modelogStepAIC<-step(null, scope=list(lower=null, upper=full), direction="both")
summary(modelogStepAIC)#Casi todas las variables son significativas
pseudoR2(modelogStepAIC,data_train, "EleccionesObjBin")
pseudoR2(modelogStepAIC,data_test, "EleccionesObjBin")
#El pseudoR2 se podría mejorar un poco. Además, hay bastante diferencia entre train y test.

modelogStepBIC<-step(null, scope=list(lower=null, upper=full), direction="both",k=log(nrow(data_train)))
summary(modelogStepBIC)#Tiene alguna variable menos y en este caso, todas son muy siginificativas
pseudoR2(modelogStepBIC,data_train, "EleccionesObjBin") 
pseudoR2(modelogStepBIC,data_test, "EleccionesObjBin")#El PseudoR2 es peor y sigue habiendo bastante diferencia entre train y test

modelogStepAIC$rank
modelogStepBIC$rank
#El modelogStepBIC tiene peor r2 pero lo prefiero porque la diferencia en pseudoR2 no es tan grande como para asumir 10 parámetros de más en el modelogStepAIC.

#Interacciones
formInt<-formulaInteracciones(datosbin[,c(1:30,58)],31)
fullInt<-glm(formInt,data = data_train,family = binomial)

modelogStepAIC_int<-step(null, scope=list(lower=null, upper=fullInt), direction="both")
summary(modelogStepAIC_int)#Tiene un montón de variables, muchas no son significativas
pseudoR2(modelogStepAIC_int,data_train, "EleccionesObjBin") 
pseudoR2(modelogStepAIC_int,data_test, "EleccionesObjBin") #Súper poco estable, hay muchísima diferencia entre train y test.

modelogStepBIC_int<-step(null, scope=list(lower=null, upper=fullInt), direction="both",k=log(nrow(data_train)))
summary(modelogStepBIC_int)
pseudoR2(modelogStepBIC_int,data_train, "EleccionesObjBin") 
pseudoR2(modelogStepBIC_int,data_test, "EleccionesObjBin")#Este es un poco más estable que el anterior

modelogStepAIC_int$rank
modelogStepBIC_int$rank
#De todas formas, está muy claro cuál es el preferible. Aunque hemos sacado un 0.26 de pseudoR2 en train en el modelogStepAIC_int, no es muy estable ya ue hay mucha diferencia con test, por lo que me fío más del modelogStepBic_int
#que además tiene muchísimas menos interacciones.

#Transformaciones
fullT<-glm(EleccionesObjBin~., data=data_train, family = binomial)

modelogStepAIC_trans<-step(null, scope=list(lower=null, upper=fullT), direction="both")
summary(modelogStepAIC_trans)
pseudoR2(modelogStepAIC_trans,data_train, "EleccionesObjBin") 
pseudoR2(modelogStepAIC_trans,data_test, "EleccionesObjBin")#Este es un poco más estable que el modelo AIC con interacciones 

modelogStepBIC_trans<-step(null, scope=list(lower=null, upper=fullT), direction="both",k=log(nrow(data_train)))
summary(modelogStepBIC_trans)
pseudoR2(modelogStepBIC_trans,data_train, "EleccionesObjBin") 
pseudoR2(modelogStepBIC_trans,data_test, "EleccionesObjBin")#Estabilidad parecida al modelogstepAIC_trans pero psudeo R2 un poco inferior.

modelogStepAIC_trans$rank 
modelogStepBIC_trans$rank
#Prefiero modeloStepBIC_trans porque tiene menos parámetros

#Transformaciones e interacciones
formIntT<-formulaInteracciones(datosbin,58)
fullIntT<-glm(formIntT, data=data_train, family = binomial)

modelogStepAIC_transInt<-step(null, scope=list(lower=null, upper=fullIntT), direction="both")
summary(modelogStepAIC_transInt) #Tiene muchísimas variables y muchas no parece significativas.
pseudoR2(modelogStepAIC_transInt,data_train, "EleccionesObjBin")
pseudoR2(modelogStepAIC_transInt,data_test, "EleccionesObjBin")#Muy intestable: muchísima diferencia entre train y test.

modelogStepBIC_transInt<-step(null, scope=list(lower=null, upper=fullIntT), direction="both",k=log(nrow(data_train)))
summary(modelogStepBIC_transInt)
pseudoR2(modelogStepBIC_transInt,data_train, "EleccionesObjBin") 
pseudoR2(modelogStepBIC_transInt,data_test, "EleccionesObjBin")#Bastante más estable que el modeloStepAIC_transInt pero tenemos un problema, y es que ha incluido
#en el modelo Age_over65_pct y sqrtxAge_over65_pct lo que causa multicolinealidad

plot(datosbin$Age_over65_pct,datosbin$sqrtxAge_over65_pct)
cor(datosbin$Age_over65_pct,datosbin$sqrtxAge_over65_pct)#Tienen una correlación prácticamente perfecta. Voy a ver cuál de ellas tiene un mayor impacto en la respuesta.
aggregate(Age_over65_pct ~EleccionesObjBin, data = datosbin, mean) 
aggregate(sqrtxAge_over65_pct ~EleccionesObjBin, data = datosbin, mean) 
#En este caso veo que las medias difieren más entre los dos subgrupos con la variable sin transformar, por lo que elimino la transformada.
modelogStepBIC_transInt_update<-update(modelogStepBIC_transInt, .~.-sqrtxAge_over65_pct)
summary(modelogStepBIC_transInt_update)
pseudoR2(modelogStepBIC_transInt_update,data_train, "EleccionesObjBin") 
pseudoR2(modelogStepBIC_transInt_update,data_test, "EleccionesObjBin")#Al eliminar la multicolinealidadha bajad un poco la pseudoR2 pero el cambio es imperceptible.

modelogStepAIC_transInt$rank 
modelogStepBIC_transInt_update$rank #obviamente... este que tiene menos parámetros (muchísimos menos)

#Repaso de los mejores modelos:
pseudoR2(modelogStepBIC,data_test, "EleccionesObjBin")
pseudoR2(modelogStepBIC_int,data_test, "EleccionesObjBin")
pseudoR2(modelogStepBIC_trans,data_test, "EleccionesObjBin")
pseudoR2(modelogStepBIC_transInt_update,data_test, "EleccionesObjBin")

#Ahora paso a probar los modelos con validación cruzada repetida
total<-c()
modelos<-sapply(list(modelogStepBIC,
                     modelogStepBIC_int,modelogStepBIC_trans,
                     modelogStepBIC_transInt_update),formula)

auxVarObj<-datosbin$EleccionesObjBin
datosbin$EleccionesObjBin<-make.names(datosbin$EleccionesObjBin)

for (i in 1:length(modelos)){
  set.seed(1712)
  vcr<-train(as.formula(modelos[[i]]), data = datosbin,
             method = "glm", family=binomial,metric = "ROC",
             trControl = trainControl(method="repeatedcv", number=5, repeats=20,
                                      summaryFunction=twoClassSummary,
                                      classProbs=TRUE,returnResamp="all")
  )
  total<-rbind(total,cbind(vcr$resample[,1:2],modelo=rep(paste("Modelo",i),
                                                         nrow(vcr$resample))))
}
datosbin$EleccionesObjBin<-auxVarObj
boxplot(ROC ~modelo,data=total,main="Accuracy ") 
aggregate(ROC ~modelo, data = total, mean)#Todos tienen un área debajo de la curva ROC bastante parecida aunque el modelo 4 parece ser el mejor
aggregate(ROC ~modelo, data = total, sd)#Aquí también vemos con la variabilidad que el modelo 4 es el que mejor la maneja aunque con poca diferencia.

#En cuanto a número de parámetros:
modelogStepBIC$rank
modelogStepBIC_int$rank
modelogStepBIC_trans$rank
modelogStepBIC_transInt_update$rank
#Dada la poca diferencia en sesgo y variabilidad de los cuatro modelos seleccionados, finalmente optaré por el que menos parámetros tenga que en este caso es el modeloStepBIC sin transformaciones ni interacciones.

#A continuación, busco el mejor punto de corte para este modelo seleccionado.
#Obtengo un gráfico de las probabilidades obtenidas
hist_targetbinaria(predict(modelogStepBIC, newdata=data_test,type="response"),data_test$EleccionesObjBin,"probabilidad")

#Por lo que veo en el histograma está entre 0.25 y 0.50. Parece ser más o menos 0.40. Probaré algunos.
sensEspCorte(modelogStepBIC,data_test,"EleccionesObjBin",0.35,"1")
sensEspCorte(modelogStepBIC,data_test,"EleccionesObjBin",0.40,"1")
sensEspCorte(modelogStepBIC,data_test,"EleccionesObjBin",0.45,"1")

#A continuación genero una rejilla de posibles valores para el punto de corte
posiblesCortes<-seq(0,1,0.01)
rejilla<-data.frame(t(rbind(posiblesCortes,sapply(posiblesCortes,function(x) sensEspCorte(modelogStepBIC,data_test,"EleccionesObjBin",x,"1")))))
rejilla$Youden<-rejilla$Sensitivity+rejilla$Specificity-1
plot(rejilla$posiblesCortes,rejilla$Youden) #Parece que el punto de corte que maximiza el índice de Youden está en torno al 0.45
plot(rejilla$posiblesCortes,rejilla$Accuracy)#En este caso no está tan claro, parece estar entre 0.4 y 0.65
rejilla$posiblesCortes[which.max(rejilla$Youden)]
rejilla$posiblesCortes[which.max(rejilla$Accuracy)]

#Voy a comparar los resultados con estos dos puntos de corte y la estabilidad mediante las diferencias entre train y test
sensEspCorte(modelogStepBIC,data_test,"EleccionesObjBin",0.34,"1")
sensEspCorte(modelogStepBIC,data_test,"EleccionesObjBin",0.57,"1")
sensEspCorte(modelogStepBIC,data_train,"EleccionesObjBin",0.34,"1") #Es bastante estable.
sensEspCorte(modelogStepBIC,data_train,"EleccionesObjBin",0.57,"1")#Este también es bastante estable.

#No estoy muy satisfecha con el nivel de sensibilidad alcanzado en ninguno de los puntos de corte aunque en este caso me quedaría con el punto de corte 0.4 porque
#prefiero sacrificar algo de especificidad para aumentar la sensibilidad: en este caso prefiero predecir bien qué municipios van a tener abstención alta para poder
# actuar en consecuencia.
#En conclusión, he elegido este modelo de todos los posibles porque el sesgo y la varianza se parecían bastante y este era el que menos parámetros tenía y mayor facilidad para interpretar

##INTERPRETACIÓN MODELO GANADOR REGRESIÓN LOGÍSTICA
#A continuación los OR del modelo ganador
install.packages('epiDisplay')
library(epiDisplay)
logistic.display(modelogStepBIC)
#Interpretación parámetro de CCAABin grupo Asturias: La probabilidad de que en un municipio ubicado en Asturias, Baleares, Canarias, Ceuta o Melilla haya abstención alta es 4.44 veces mayor que en un municipio ubicado en Andalucía, Galicia, Navarra o País Vasco.
#Interpretación parámetro de Age_over_65_pct: por cada aumento de un punto porcentual de habitantes mayores de 65 años la probabilidad de que haya abstención alta en el municipio aumenta 1.05 veces.
