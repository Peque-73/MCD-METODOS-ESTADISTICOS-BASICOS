#Reporte #3 Alvaro Pequeño Mondragón 1726520 Grupo:002

library(car)
#Se leen los datos
Datos = read.csv('C:/Users/alvar/Documents/Maestria en Ciencia de datos/Métodos Estadisticos Básicos/Tareas/Reporte 2/Luxury Watches (With Ids).csv')

#Analisis descriptivo de los datos
pairs(Datos, main = "Matriz de Dispersión")
cor(Datos)#Nos muestra los valores de correlacion de variables

plot(Datos$Movement.Type, Datos$Case.Material, main = "Dispersion de Tipo de Movimiento V.S Material de la caja", xlab = "Tipo de Movimiento", ylab = "Material de la caja", xaxp=c(0,4,4))
plot(Datos$Price..USD., Datos$Case.Material, main = "Dispersion de Precio V.S Material de la caja", xlab = "Precio (USD)", ylab = "Material de la caja", xaxp=c(0,70000,14), las=2)
plot(Datos$Crystal.Material, Datos$Movement.Type, main = "Dispersion de Material del cristal V.S Tipo de movimiento", xlab = "Material de cristal", ylab = "Tipo de movimiento", xaxp=c(0,4,4), yaxp=c(0,4,4))
plot(Datos$Power.Reserve, Datos$Movement.Type, main = "Dispersion de Reserva de energía V.S Tipo de movimiento", xlab = "Reserva de energía (Horas)", ylab = "Tipo de movimiento", yaxp=c(0,4,4))
Datos$Power.Reserve[Datos$Power.Reserve>170] = NA
plot(Datos$Power.Reserve, Datos$Movement.Type, main = "Dispersion de Reserva de energía V.S Tipo de movimiento", xlab = "Reserva de energía (Horas)", ylab = "Tipo de movimiento", xaxp = c(0,180,18),yaxp=c(0,4,4), las = 2)
plot(Datos$Case.Diameter..mm., Datos$Water.Resistance, main = "Diametro de la caja V.S Resistencia al agua", xlab = "Diametro de la caja (mm)", ylab = "Resistencia al agua (m)", xaxp = c(25,50,25), las = 2)

#Se analiza para problemas de colinealidad
#Se vuelven a leer los datos para recuperar la información modificada anteriormente.
Datos = read.csv('C:/Users/alvar/Documents/Maestria en Ciencia de datos/Métodos Estadisticos Básicos/Tareas/Reporte 2/Luxury Watches (With Ids).csv')
reg_mult = lm(Datos$Price..USD.~Datos$Case.Material+Datos$Strap.Material+Datos$Movement.Type+Datos$Water.Resistance+Datos$Case.Diameter..mm.+Datos$Crystal.Material+Datos$Complications+Datos$Power.Reserve)
# Precio = 18027.452 + Material Caja * 1347.609 - Material de la correa * 742.123 - Tipo de movimiento * 61.348 - Resistencia al agua * 6.686 - Diametro de la caja * 42.302 - Material del cristal * 3171.067 - Complicaciones * 21.323 - Reserva de energia * 1.067 
vif(reg_mult)
# Ningun valor es mayor a 10 por lo que no hay problemas de colinealidad.

#Prueba de significancia (se revisa modelo)
#H0: B1=B2....=Bn = 0 vs Ha: Al menos una variable es diferente a 0
summary(reg_mult)
#p-value: 2.621e-13 | Se rechaza H0 si P valor < 0.05 | P valor < 0.05, por lo que se rechaza H0.
#Con 95% de confianza al menos una variable es diferente a 0 por lo que al menos 1 variable es significativa para nuestro analisis.

#Intervalos de confianza
confint(reg_mult)

library(olsrr)

ols_step_all_possible(lm(Datos$Price..USD.~Datos$Case.Material+Datos$Strap.Material+Datos$Movement.Type+Datos$Water.Resistance+Datos$Case.Diameter..mm.+Datos$Crystal.Material+Datos$Complications+Datos$Power.Reserve))


