#Evidencia # 6 Alvaro Pequeño Mondragón 1726520 Grupo:002

#Ejercicio #1
num_horas = c(2,4,6,8,10,12)
cloro_residual = c(1.8,1.5,1.4,1.1,1.1,0.9)

#1. Se identifican las variables
#X = Numero de horas
#Y = Cloro residual

#2. Descriptivas
cor(num_horas, cloro_residual)#Relación alta con -0.9759001
plot(num_horas, cloro_residual, main = "Diagrama de Dispercion")
#Del grafico se sospecha relación directa y modelo lineal.

#3. Se evalua el modelo lineal
mod_lin = lm(cloro_residual~num_horas)
#Modelo lineal estimado: Y = 1.9 - X*0.08571

#3.1 Prueba de hipotesis
#H0: B1 = 0 vs Ha: B1!=0
summary(mod_lin)
#p-value: 0.0008642 | Se rechaza H0 si P valor < 0.05 | P valor < 0.05, por lo que se rechaza H0.
#Con 95% de confianza B1 es diferente a 0 por lo que es significativa para nuestro analisis.

#3.2 Intervalos de confianza
confint(mod_lin)
#1.6927606 < B0 <  2.1072394 No hay evidencia de regresión al 0.
#-0.1123214 < B1 < -0.0591072 El aporte de las horas a la cantidad de cloro residual es negativo.

#3.3 Valores estimados
fitted.values(mod_lin)

#3.4 Errores
residuals(mod_lin)

#Comparación del modelo lineal con los datos
plot(num_horas, cloro_residual, main = "Comparatiba de modelos")
abline(mod_lin, col="darkseagreen")

#3. Se revisa el modelo exponencial
mod_exp = lm(log(cloro_residual)~num_horas)
#Modelo lineal asociado: y = 0.69957 - x*0.06625

exp(coefficients(mod_exp[1]))
#Modelo exponencial estimado: y = 2.0128808 * exp(x*0.9358987)

#3.1 Prueba de hipotesis
#H0: B1 = 0 vs Ha: B1!=0
summary(mod_exp)
#p-value: 0.0004913 | Se rechaza H0 si P valor < 0.05 | P valor < 0.05, por lo que se rechaza H0.
#Con 95% de confianza B1 es diferente a 0 por lo que es significativa para nuestro analisis.

#3.2 Intervalos de confianza
confint(mod_exp)
#0.56118824 < B0 <  0.83794560 No hay evidencia de regresión al 0.
#-0.08401422 < B1 < -0.04848186 El aporte de las horas a la cantidad de cloro residual es negativo.

#3.3 Valores estimados
est_exp = exp(coefficients(mod_exp)[1]) * exp(coefficients(mod_exp)[2]*num_horas)

#3.4 Errores
residuals(mod_exp)
anova(mod_exp)

# 4. comparacion del modelo lineal con exponencial y datos originales.
plot(num_horas, cloro_residual, main = "Comparación de modelos", ylab = "Cloro Residual (Partes por Millon)", xlab = "Número de Horas Transcurridas")
abline(mod_lin, col="darkseagreen")
lines(num_horas, est_exp, col="sienna1")

summary(mod_lin)#Adjusted R-squared: 0.9405
summary(mod_exp)#Adjusted R-squared: 0.955

#5. Elegir modelo
#Elegí el modelo exponencial ya que tiene un mayor numero en el r ajustado cuadrado.

#6. Revisar suspuestos del modelo.

#Residuales del modelo exponencial
res_exp = residuals(mod_exp)

#6.1 Prueba de hipotesis para Media = 0 y normalidad
#H0: Residuales provienen de una población con distribución normal y media 0
#Ha: Residuales NO provienen de una población con distribución normal y media 0
ks.test(res_exp, pnorm, mean = 0, sd=0.05354)
#p-value: 0.9957 | Se rechaza H0 si P valor < 0.05 | P valor > 0.05, por lo que NO se rechaza H0.
#Con 95% de confianza los residuales si provienen de una población con distribución normal y media 0

#6.2 Prueba de hipotesis para incorrelación
#H0: Residuales son incorrelacionados (Residuales independientes)
#Ha: Residuales son correlacionados
Box.test(res_exp, type="Box-Pierce")
#p-value: 0.05284 | Se rechaza H0 si P valor < 0.05 | P valor > 0.05, por lo que NO se rechaza H0
#Con 95% de confianza los residuales son incorrelacionados

#6.3 Varianza constante
plot(fitted.values(mod_lin), res_exp, main = "Valores Estimados V.S Errores", ylab = "Errores", xlab = "Valores estimados", xlim = c(0.8,1.8), ylim = c(-0.08, 0.06))
#No se observa un patron en la gráfica de los valores estimados y los residuales por lo que la varianza es constante.

#De lo anterior se cumplen los supuestos del modelo

#Modelo elegido: y = 2.0128808 * exp(x*0.9358987)

#Ejercicio #2
library(car)

iq = c(112,126,100,114,112,121,110,103,111,124)
horas_est = c(5,13,3,7,11,9,8,4,6,2)
puntuacion = c(79,97,51,65,82,93,81,38,60,86)
#1. Se identifican las variables:
# X1 = IQ
# X2 = Horas de Estudio
# Y = Puntuación

#2. Se verifica si existen problemas de multicolinealidad
vif(lm(puntuacion~iq+horas_est))

#3. Prueba de significancia del modelo completo (Se revisa modelo)
mod_lin_completo = lm(puntuacion~iq+horas_est)
#Y = -124.568 + X1*1.659 + X2*1.439

#3.1 Prueba de hipotesis
#H0: B1=B2=0 vs Ha: B1!=0 o B2!=0
summary(mod_lin_completo)
#p-value: 0.004864 | Se rechaza H0 si P valor < 0.05 | P valor < 0.05, por lo que se rechaza H0.
#Con 95% de confianza B1 o B2 es diferente a 0 por lo que al menos 1 variable es significativa para nuestro analisis.

#Intervalos de confianza
confint(mod_lin_completo)
#-237.7725918 < B0 <  -11.363715 No hay evidencia de regresión al 0.
# 0.6041921 < B1 < 2.714094 El aporte de iq a la puntuacion es positivo.
# -1.0832805 < B2 < 3.961876

#Se obtienen todos los  modelos posibles
ols_step_all_possible(mod_lin_completo)

#Se evaluan los modelos individuales

#Modelo solo iq
mod_lin_iq = lm(puntuacion~iq)
# Y = -145.090 + 1.927*X1

#Prueba de hipotesis
#H0: B1 = 0 vs Ha: B1!=0
summary(mod_lin_iq)
#p-value: 0.001776 | Se rechaza H0 si P valor < 0.05 | P valor < 0.05, por lo que se rechaza H0.
#Con 95% de confianza B1 es diferente a 0 por lo que es significativa para nuestro analisis.

#Intervalos de confianza
confint(mod_lin_iq)
#-255.0033445 < B0 <  -35.177191 No hay evidencia de regresión al 0.
#0.9589528 < B1 < 2.894362 El aporte de iq a la puntuacion es positivo.

#Valores estimados
fitted.values(mod_lin_iq)

# Errores
residuals(mod_lin_iq)

#Modelo solo Horas de estudio
mod_lin_h_e = lm(puntuacion~horas_est)
# Y = 51.423 + 3.203*X2

#Prueba de hipotesis
#H0: B1 = 0 vs Ha: B1!=0
summary(mod_lin_h_e)
#p-value: 0.07147 | Se rechaza H0 si P valor < 0.05 | P valor > 0.05, por lo que NO se rechaza H0.
#Con 95% de confianza B1 NO es diferente a 0 por lo que no es significativa para nuestro analisis.

#Intervalos de confianza
confint(mod_lin_h_e)
#24.4817775 < B0 <  78.364101 No hay evidencia de regresión al 0.
#-0.3534797 < B1 < 6.758498 

#Valores estimados
fitted.values(mod_lin_h_e)

#Errores
residuals(mod_lin_h_e)

#6. Revisar suspuestos del modelo.

#Residuales del modelo exponencial
res_lin_completo = residuals(mod_lin_completo)

#Prueba de hipotesis para Media = 0 y normalidad
#H0: Residuales provienen de una población con distribución normal y media 0
#Ha: Residuales NO provienen de una población con distribución normal y media 0
ks.test(res_lin_completo, pnorm, mean = 0, sd=10.1)
#p-value: 0.8617 | Se rechaza H0 si P valor < 0.05 | P valor > 0.05, por lo que NO se rechaza H0.
#Con 95% de confianza los residuales si provienen de una población con distribución normal y media 0

#Prueba de hipotesis para incorrelación
#H0: Residuales son incorrelacionados (Residuales independientes)
#Ha: Residuales son correlacionados
Box.test(res_lin_completo, type="Box-Pierce")
#p-value: 0.3836 | Se rechaza H0 si P valor < 0.05 | P valor > 0.05, por lo que NO se rechaza H0
#Con 95% de confianza los residuales son incorrelacionados

#6.3 Varianza constante
plot(fitted.values(mod_lin_completo), res_lin_completo, main = "Valores Estimados V.S Errores", ylab = "Errores", xlab = "Valores estimados",xlim = c(40, 110), ylim = c(-15, 15))
#No se observa un patron en la gráfica de los valores estimados y los residuales por lo que la varianza es constante.

#De lo anterior se cumplen los supuestos del modelo

#Modelo elegido: y = 2.0128808 * exp(x*0.9358987)
