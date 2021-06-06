# TIPOLOGÍA Y CICLO DE VIDA DE LOS DATOS PRACTICA2
# DAVID ARBONA NAVARRO

#
# ABRIR LOS FICHERO CON LOS DATOS 
train <- read.csv("/Users/davidarb/Documents/PRACTICA2_R/titanic/train.csv", header=TRUE, encoding="UTF-8", sep=",")
test <- read.csv("/Users/davidarb/Documents/PRACTICA2_R/titanic/test.csv", header=TRUE, encoding="UTF-8", sep=",")

#2-OBSERVACION DE DATOS
# Examinamos que se han cargado correctamente
head(train)
head(test)
#
# Conozcamos la estructura de los datos
summary(train)
summary(test)
str(train)
str(test)
sapply(train, function(x) class(x))

#
# Cantidad de datos, proporcion y gráficos para ver la distribución de los valores:

# Cantidad de datos, proporcion y gráficos de barras para ver la distribución de valores
# TRAIN
# Atributo Survived
prop.table(table(train$Survived))
barplot(prop.table(table(train$Survived)), main="Survived")
# Atributo Pclass
prop.table(table(train$Pclass))
barplot(prop.table(table(train$Pclass)), main="Pclass")
# Atributo Sex
prop.table(table(train$Sex))
barplot(prop.table(table(train$Sex)), main="Sex")
# Atributo Age
plot(density(na.omit(train$Age)),main = "Age")
# Atributo SibSp
prop.table(table(train$SibSp))
barplot(prop.table(table(train$SibSp)), main="SibSp")
# Atributo Parch
prop.table(table(train $Parch))
barplot(prop.table(table(train $Parch)), main="Parch")
# Atributo Fare
plot(density(na.omit(train$Fare)),main = "Fare")
# Atributo Embarked
prop.table(table(train $Embarked))
barplot(prop.table(table(train $Embarked)), main="Embarked")

#SELECCION DE LOS DATOS
train_DC<-train[,c(2, 3, 5, 6,7,8, 10)]
summary(train_DC)


#3-LIMPIEZA DE DATOS
# Valores nulos
sapply(train_DC, function(x) sum(is.na(x)))

#Campo Age
train_DC$Age<-ifelse(is.na(train_DC$Age),mean(na.omit(train_DC$Age)), train_DC$Age)
sapply(train_DC, function(x) sum(is.na(x)))

  # Valores 0
sapply(train_DC, function(x) sum(x==0))
 
sapply(train_DC, function(x) sum(x==""))

 #Campo Fare
summary(train_DC)
summary(train_DC[train_DC$Pclass==1,]$Fare)
summary(train_DC$Fare)
 
train_1stClass <- train_DC[train_DC $Pclass == 1,]
train_2stClass <- train_DC[train_DC $Pclass == 2,]
train_3stClass <- train_DC[train_DC $Pclass == 3,]

train_1stClass$Fare[train_1stClass$Fare == 0] <-NA
train_2stClass$Fare[train_2stClass$Fare == 0] <-NA
train_3stClass$Fare[train_3stClass$Fare == 0] <-NA
 
 train_1stClass$Fare<-ifelse(is.na(train_1stClass $Fare),mean(na.omit(train_1stClass $Fare)), train_1stClass $Fare)
train_2stClass$Fare<-ifelse(is.na(train_2stClass$Fare),mean(na.omit(train_2stClass$Fare)), train_2stClass$Fare)
train_3stClass$Fare<-ifelse(is.na(train_3stClass$Fare),mean(na.omit(train_3stClass$Fare)), train_3stClass$Fare)
 
summary (train_1stClass$Fare)
 
train_DC<-rbind(train_1stClass, train_2stClass, train_3stClass)
summary(train_DC$Fare)
summary(train_DC)
 

 
 #Outliers 
a1<-boxplot(train_DC $Age,main="Age")
a1$out

a2<-boxplot(train_DC $Fare,main="Fare")
a2$out

boxplot (train_DC$Fare~ train_DC$Pclass, data= train_DC, xlab="Class", ylab="Fare", main="Tarifa por Clase",boxwex=0.5)

train[train$Fare > 500,]


#4-ANALISIS DE LOS DATOS
summary(train_DC)

shapiro.test(train_DC$Age)
ks.test(train_DC$Age, pnorm, mean(train_DC$Age), sd(train_DC$Age))


shapiro.test(train_DC$Fare)
ks.test(train_DC$Fare, pnorm, mean(train_DC$Fare), sd(train_DC$Fare))


fligner.test(Age ~ Pclass, data = train_DC)
fligner.test(Age ~ Survived, data = train_DC)
fligner.test(Fare ~ Survived, data = train_DC)
fligner.test(Fare ~ Pclass, data = train_DC)
fligner.test(Age ~ Sex, data = train_DC)
fligner.test(Fare ~ Sex, data = train_DC)


wilcox.test(Age ~ Sex, data = train_DC)
wilcox.test(Age ~ Survived, data = train_DC)
wilcox.test(Fare ~ Sex, data = train_DC)
wilcox.test(Fare ~ Survived, data = train_DC)


#Regresión
plot(train_DC)
m1 = lm(Age~Pclass,data=train_DC)
summary(m1)
m2 = lm(Age~ Sex,data=train_DC)
summary(m2)
m3 = lm(Fare~ Pclass,data=train_DC)
summary(m3)
m4 = lm(Pclass~Fare+I(Fare ^2),data=train_DC)
summary(m4)

#Correlación
cor.test(train_DC$Fare, train_DC$Pclass, method="spearman")
cor.test(train_DC$Age, train_DC$Pclass, method="spearman")
cor.test(train_DC$Age, train_DC$SibSp, method="spearman")
cor.test(train_DC$Pclass, train_DC$Survived, method="spearman")


#Representación resultados
hist(train$Survived)
hist(train$Pclass)
hist(train$Age)
hist(train$Fare)
hist(train$Fare)
qqnorm(train$Age, main="Age")
qqline(train$Age,col=2)
qqnorm(train$Fare, main="Fare")
qqline(train$Fare,col=2)

 boxplot (train_data_clean$Age~train_data_clean$Survived, data=train_data_clean, xlab="Sobrevive", ylab="Age", main="Edad por Sobrevive",boxwex=0.5)
 
 boxplot (train_data_clean$Age~train_data_clean$Pclass, data=train_data_clean, xlab="Pclass", ylab="Age", main="Edad por Pclass",boxwex=0.5)
 

train_AgeSurvived<-table(train_DC[,c(1,4)])
barplot(train_AgeSurvived, main = "Supervivientes del Titanic por Edad", xlab = "Clase",col = c("cadetblue4","aquamarine"))
legend("topleft", c("No sobrevive","Superviviente"), fill = c("cadetblue4","aquamarine"))

train_ClaseSurvived<-table(train_DC[,c(1,2)])
barplot(train_ClaseSurvived, main = "Supervivientes del Titanic por Clase", xlab = "Clase",col = c("cadetblue4","aquamarine"))
legend("topleft", c("No sobrevive","Superviviente"), fill = c("cadetblue4","aquamarine"))

train_SexoSurvived<-table(train_DC[,c(1,3)])
barplot(train_SexoSurvived, main = "Supervivientes del Titanic por Sexo", xlab = "Clase",col = c("cadetblue4","aquamarine"))
legend("topleft", c("No sobrevive","Superviviente"), fill = c("cadetblue4","aquamarine"))

train_HijosSurvived<-table(train_DC[,c(1,6)])
barplot(train_HijosSurvived, main = "Supervivientes del Titanic por Num Hijos", xlab = "Clase",col = c("cadetblue4","aquamarine"))
legend("topleft", c("No sobrevive","Superviviente"), fill = c("cadetblue4","aquamarine"))

train_FamSurvived<-table(train_DC[,c(1,5)])
barplot(train_FamSurvived, main = "Supervivientes del Titanic por Familiares a bordo", xlab = "Clase",col = c("cadetblue4","aquamarine"))
legend("topleft", c("No sobrevive","Superviviente"), fill = c("cadetblue4","aquamarine"))
