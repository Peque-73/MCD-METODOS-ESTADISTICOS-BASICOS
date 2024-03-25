#Alvaro Pequeño Mondragón 1726520 Grupo: 002 Reporte 4

#Se lee el archivo csv
Datos = read.csv('C:/Users/alvar/Documents/Maestria en Ciencia de datos/Métodos Estadisticos Básicos/Tareas/Reporte 2/Luxury Watches (With Ids).csv')
#Variable #1: Material de la caja (Discreta)
table(Datos$Case.Material)
boxplot(Datos$Price..USD. ~ Datos$Case.Material, main = "Material de la Caja y Precio en Dolares", ylab = "Precio (USD)", xlab = "Material de la Caja")
hist(Datos$Case.Material, main = "Histograma de Material de la caja", ylab = "Frecuencia", xlab = "Material de la Caja",xlim = c(0,14), ylim = c(0, 500), xaxp=c(0,14,14))

#Variable #2: Material  de la Correa (Discreta)
table(Datos$Strap.Material)
boxplot(Datos$Price..USD. ~ Datos$Strap.Material, main = "Material de la Correa y Precio en Dolares", ylab = "Precio (USD)", xlab = "Material de la Correa")
hist(Datos$Strap.Material, main = "Histograma de Material de la correa", ylab = "Frecuencia", xlab = "Material de la Correa", ylim = c(0, 300), xaxp=c(0,14,14), xlim = c(0,14))

#Variable #3: Tipo de movimiento (Discreta)
table(Datos$Movement.Type)
boxplot(Datos$Price..USD. ~ Datos$Movement.Type, main = "Tipo de Movimiento y Precio en Dolares", ylab = "Precio (USD)", xlab = "Tipo de Movimiento")
hist(Datos$Movement.Type, main = "Histograma de Tipo de movimiento", breaks = 4, ylab = "Frecuencia", xlab = "Tipo de Movimiento", ylim = c(0, 500), xaxp=c(0,4,4), xlim = c(0,4))

#Variable #4: Resistencia al agua en metros
mean(Datos$Water.Resistance)
table(Datos$Water.Resistance)
boxplot(Datos$Water.Resistance, main = "Resitencia al Agual", ylab = "Resistencia al agua en metros", xlab = "Variable de Resistencia al agua")
boxplot(Datos$Water.Resistance, main = "Resitencia al Agual", ylab = "Resistencia al agua en metros", xlab = "Variable de Resistencia al agua", ylim= c(0,250))
hist(Datos$Water.Resistance, main = "Histograma de Resitencia al Agua", breaks = 120, ylab = "Frecuencia", xlab = "Resistencia al Agua en Metros",ylim = c(0,140), xlim = c(0,600), xaxp=c(0,600,60), las=2)

#Variable #5: Diametro de la caja en mm (Continua)
max(Datos$Case.Diameter..mm.) - min(Datos$Case.Diameter..mm.)#Se calcula el rango
mean(Datos$Case.Diameter..mm.)
min(Datos$Case.Diameter..mm.)
max(Datos$Case.Diameter..mm.)
var(Datos$Case.Diameter..mm.)
table(Datos$Case.Diameter..mm.)
boxplot(Datos$Case.Diameter..mm., main = "Diametro de la caja", ylab = "Diametro de la caja en mm", xlab = "Variable de Diametro de la Caja")
hist(Datos$Case.Diameter..mm., main = "Histograma de Diametro de Caja en mm", ylab = "Frecuencia", xlab = "Diametro de Caja en mm",ylim = c(0,150), xlim = c(25,50), breaks = 25, xaxp=c(25,50,25), las=2)

#Variable #6: Material del cristal (Discreta)
table(Datos$Crystal.Material)
boxplot(Datos$Price..USD. ~ Datos$Crystal.Material, main = "Material del Cristal", ylab = "Precio (USD)", xlab = "Material del cristal")
hist(Datos$Crystal.Material, main = "Histograma de Material del Cristal", breaks = 4, ylab = "Frecuencia", xlab = "Material del Cristal",ylim = c(0,500), xlim = c(0,4), xaxp=c(0,4,4))

#Variable #7: Complicaciones (Discreta)
table(Datos$Complications)
boxplot(Datos$Price..USD. ~ Datos$Complications, main = "Material del Cristal", ylab = "Precio (USD)", xlab = "Material del cristal", las = 2)
hist(Datos$Complications, main = "Histograma de Complicaciones", ylab = "Frecuencia", xlab = "Complicaciones",ylim = c(0,350), xlim = c(0,30), xaxp=c(0,30,30), breaks = 30)

#Variable #8: Tiempo de reserva (Continua)
max(Datos$Power.Reserve) - min(Datos$Power.Reserve)#Se calcula el rango
mean(Datos$Power.Reserve)
min(Datos$Power.Reserve)
max(Datos$Power.Reserve)
var(Datos$Power.Reserve)
table(Datos$Power.Reserve)
boxplot(Datos$Power.Reserve, main = "Tiempo de Reserva", ylab = "Tiempo de Reserva en Horas", xlab = "Variable de Tiempo de Reserva")
boxplot(Datos$Power.Reserve, main = "Tiempo de Reserva", ylab = "Tiempo de Reserva en Horas", xlab = "Variable de Tiempo de Reserva", ylim = c(0,200))
Datos$Power.Reserve[Datos$Power.Reserve>170] = NA
hist(Datos$Power.Reserve, main = "Histograma de Tiempo de Reserva", ylab = "Frecuencia", xlab = "Tiempo de Reserva",ylim = c(0,100), xlim = c(30,120), breaks = 90, xaxp=c(30,120,45), las=2)

#Variable #9: Precio en dolares (Continua)
max(Datos$Price..USD.) - min(Datos$Price..USD.)#Se calcula el rango
mean(Datos$Price..USD.)
min(Datos$Price..USD.)
max(Datos$Price..USD.)
var(Datos$Price..USD.)
table(Datos$Price..USD.)
boxplot(Datos$Price..USD., main = "Precio del Reloj", ylab = "Precio (USD)", xlab = "Variable de Precio")
hist(Datos$Price..USD., main = "Histograma del Precio del Reloj", ylab = "Frecuencia", xlab = "Precio (USD)", ylim = c(0,120), xlim = c(0,70000), breaks = 40, xaxp=c(0,70000,35), las=2)