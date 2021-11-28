# Modelo-Lineal-Multiple

---
title: 'Investigación NutreLight'
author: 'Marco E'
output: html_document
editor_options: 
  chunk_output_type: console
---

```{r include = FALSE}
knitr::opts_chunk$set(message = FALSE)
```

# paquetes

```{r}
xfun::pkg_attach(c('tidyverse','lmtest','car','lattice', 'readxl','corrgram', 'MASS', 'gridExtra'))
```


# Análisis descriptivo

```{r}
setwd("C:/Users/Marco Espinoza/OneDrive/Documents/Universidad de Costa Rica/II semestre 2021/regresión aplicada/papers para el proyecto")
base = read.csv('base.csv', header = TRUE, sep = ';')
```

+ Lo primero es hacer unos cambios

```{r}
#cambiamos el nombre de las variables
colnames(base) = c('factura', 'sexo', 'edad', 'nvleduc', 'servicio',
                   'sabor', 'present', 'frec', 'cant',
                   'tipo','dieta','diabetico',
                   'canton', 'ingre', 'gasto', 'dist')
#revisamos de nuevo
names(base)
```


```{r}
base$sexo = factor(base$sexo)
base$nvleduc = factor(base$nvleduc)
base$frec = factor(base$frec)
base$tipo = factor(base$tipo)
base$dieta = factor(base$dieta)
base$diabetico = factor(base$diabetico)

str(base)
#está como hombre y mujer, se le asigna 0 a hombre y 1 a mujer
levels(base$sexo) = c('0','1')
#está como Otro, secundaria completa, secundaria incompleta, universitaria. Se pasa a 4, 2, 1, 3
levels(base$nvleduc) = c('4', '2','1','3')
levels(base$frec)
#levels está: ambos, keto y light. Se pasa a 0 para keto, 1 para light y 2 para ambos
levels(base$tipo) = c('2','0','1')
#Se le asigna 0 a no y 1 a sí
levels(base$dieta) = c('0','1')
#se le asigna 0 a no y 1 a sí
levels(base$diabetico) = c('0','1')
```


```{r}
summary(base)
base1 = base[ ,-c(1,5,10,13,14)]
base1 = base1[c(10,1:9)]
base1$dist = base$dist
names(base1)
#pasamos la observación a Uni
```

+ Gráficos para iniciar el análisis

```{r}
scatterplotMatrix(base1[ ,-c(2,4,7,9,10)])
#la variable edad necesita una transformación cuadrática parece
```

```{r}
round(apply(base1[ ,-c(1,2,4,7,9,10)], 2, var), 2)
```


```{r}
#correlación
corrgram(base1, diag.panel=panel.density, upper.panel=panel.conf)
```


  - Se observó un poco sobre la linealidad, las transformaciones a la variable de edad y tal vez cantidad, también los posibles valores extremos que puedan estar afectando la linealidad. 
  
+ vamos a plantear un modelo inicial, el cual es el siguiente:

```{r}
#hacemos el modelo
mod = lm(gasto ~ ., base1)

```

```{r}
#gráficos
par(mfrow = c(2,3),oma = c(0, 0, 2, 0))
plot(base1$gasto ~ base1$edad, xlab = 'Edad', ylab = 'Gasto')
abline(lm(base1$gasto ~ base1$edad))
lines(lowess(base1$edad, base1$gasto), lty = 2, col = 'red')

plot(base1$gasto ~ base1$cant, xlab = 'Cantidad', ylab = '')
abline(lm(base1$gasto ~ base1$cant))
lines(lowess(base1$cant, base1$gasto), lty = 2, col = 'red')

plot(base1$gasto ~ base1$sabor, xlab = 'Sabor', ylab = '')
abline(lm(base1$gasto ~ base1$sabor))
lines(lowess(base1$sabor, base1$gasto), lty = 2, col = 'red')

plot(base1$gasto ~ base1$present, xlab = 'Presentación', ylab = 'Gasto')
abline(lm(base1$gasto ~ base1$present))
lines(lowess(base1$present, base1$gasto), lty = 2, col = 'red')

plot(base1$gasto ~ base1$dist, xlab = 'Distancia', ylab = '')
abline(lm(base1$gasto ~ base1$dist))
lines(lowess(base1$dist, base1$gasto), lty = 2, col = 'red')


mtext('Gasto por variable continua', outer = TRUE, cex = 1.5)
par(mfrow = c(1,1))
#boxplots
par(mfrow = c(1,3),oma = c(0, 0, 2, 0))
boxplot(base1$gasto ~ base1$sexo, xlab = 'Sexo', ylab = 'Gasto')
boxplot(base1$gasto ~ base1$dieta, xlab = 'Dieta', ylab = 'Gasto')
boxplot(base1$gasto ~ base1$diabetico, xlab = 'Diabético', ylab = '')

mtext('Gasto por variable categórica', outer = TRUE, cex = 1.5)
par(mfrow = c(1,1))
```


# transformaciones

## Valores extremos

```{r}
t = rstudent(mod)
n = nrow(base1)
p = length(mod$coefficients)
tc = qt(1 - 0.05/(2*n), n - p - 1)

plot(t)
abline(h = tc)
```

  - Parece que la variancia al quitar el valor extremo aumentó. 

  - En realidad parece no haber cambiado mucho, sigue habiendo un valor extremo en algunos boxplots.
  
## valores de influencia

```{r}
n = nrow(base1)
p = length(mod$coefficients)
dff = dffits(mod)
plot(abs(dff), ylab = 'DFFITS', main = 'DFFITS')
lim = 2*sqrt( (p+1)/(n-p-1))
abline(h = lim)
#distancia de cook

cook = cooks.distance(mod)
plot(cook, ylab = 'Distancia de Cook', main = 'Distancia de Cook')
q = qf(c(0.1,0.2,0.5), p, n - p)
abline(h = q, col = c(1,4,2))
```

```{r}
dfbetass = dfbetas(mod)
head(dfbetass)
```

```{r}
matplot(abs(dfbetass[ ,-1]), ylab = 'Cambio en coeficientes', pch = 18 )

lim = 2/sqrt(n)

abline(h = lim)
```

  - Con respecto a los valores de influencia, con la distancia de Cook no parece haber valores de influencia que estén cambiando los coeficientes. Por ahora vamos a dejar las observaciones así.
  
# linealidad

+ Vamos a observar la linealidad por medio de un crPlots, en este caso ignoramos los boxplots. 

```{r}
summary(mod)

crPlots(mod, main = 'Residuales parciales', y = '')
#El supuesto de la linealidad no se cumple. Hay variables que irrumpen el supuesto de la linealidad. 

res = mod$residuals
betas = mod$coefficients

resig1 = res + base1$edad*betas[3]
resig4 = res + base1$cant*betas[10]

scatterplot(resig1 ~ base1$edad, ylab = 'Residuales parciales', xlab = 'Edad')
#estos no se utilizan
scatterplot(resig4 ~ base1$cant)
#grid
par(mfrow = c(1,2),oma = c(0, 0, 2, 0))
#plot 1
plot(resig1 ~ base1$edad, xlab = 'Edad', ylab = 'Residuales Parciales', pch = 20)
abline(lm(resig1 ~ base1$edad))
lines(lowess(base1$edad, resig1), lty = 2, col = 'red')
#plot 2
plot(resig4 ~ base1$cant, xlab = 'Cantidad', ylab = 'Residuales Parciales', pch = 20)
abline(lm(resig4 ~ base1$cant))
lines(lowess(base1$cant, resig4), lty = 2, col = 'red')

mtext('Linealidad modelo inicial', outer = TRUE, cex = 1.5)
par(mfrow = c(1,1))
```


# Multicolinealidad, homocedasticidad normalidad

```{r}
#multicolinealidad
cor(base1[ ,-c(2,4,7,9,10)])
round(vif(mod), 4)
#homocedasticidad
par(mfrow = c(2,3),oma = c(0, 0, 2, 0))
plot(mod$residuals ~ mod$fitted.values, xlab = 'Valores ajustados', ylab = 'Residuales')
plot(mod$residuals ~ base1$edad, ylab = '', xlab = 'Edad')
boxplot(mod$residuals ~ base1$sexo, ylab = '', xlab = 'Sexo')
plot(mod$residuals ~ base1$cant, ylab = '', xlab = 'Cantidad')
boxplot(mod$residuals ~ base1$dieta, ylab = 'Residuales', xlab = 'Dieta')
boxplot(mod$residuals ~ base1$diabetico, ylab = '', xlab = 'Diabético')
mtext('Homocedasticidad', outer = TRUE, cex = 1.5)
par(mfrow = c(1,1))

bptest(mod, studentize = F)
boxcox(mod)
#normalidad
qqPlot(mod$residuals, ylab = 'Residuales', xlab = 'Cuantiles', main = 'Normalidad')
ks.test(mod$residuals, 'pnorm', sd(mod$residuals))
```

  - El factor de la variancia es bastante bajo
  
  - Por esta parte, podemos ver que una transformación de la variable respuesta a escala logaritmica puede ser de utilidad, al igual que variable cuadrática. Recordar que se va a hacer con el valor extremo ya que este no tiene mayor impacto, de igual manera se puede volver a este paso y hacerlo sin él para ver si mejora. 

  - Vamos a intentar también ponerle un término al cuadrado a sabor a ver si mejorar también un poco. 

# Modelo ajustado

+ vamos a centrar la variable para la eliminar la multicolinealidad

```{r}
base1 = base1 %>% mutate(edad = edad - mean(edad))
#volvemos a arriba para correr el modelo
```


```{r}
#vamos a ajustar un modelo nuevo
mod1 = lm(log(gasto) ~ sexo + edad + I(edad^2) + nvleduc + sabor + present + frec + cant + dieta + diabetico + dist, base1)
```

 - se decide acatar lo dicho por el método de akaike y eliminamos las variables. 

## Selección de variables

+ Para la selección de variables se tomó en cuenta el criterio de Akaike

```{r}
modprueba = mod1
#veamos con el drop
drop1(modprueba, test = 'F')
modprueba = update(modprueba, .~.-frec)
drop1(modprueba, test = 'F')
modprueba = update(modprueba, .~.-sabor)
drop1(modprueba, test = 'F')
modprueba = update(modprueba, .~.-present)
drop1(modprueba, test = 'F')
modprueba = update(modprueba, .~.-dist)
drop1(modprueba, test = 'F')
modprueba = update(modprueba, .~.-nvleduc)
drop1(modprueba, test = 'F')
#no eliminamos edad porque el término cuadrático es significativo
mod1 = modprueba
```


## Diagnósticos
  
### Linealidad

```{r}
#veamos un summary del modelo
summary(mod1)
#veamos los residuales parciales
crPlots(mod1, main = 'Residuales parciales', y = '')
res = mod1$residuals
betas = mod1$coefficients

resig1 = res + base1$edad*betas[3]
resig4 = res + base1$cant*betas[5]
#grid
par(mfrow = c(1,2),oma = c(0, 0, 2, 0))
#plot 1
plot(resig1 ~ base1$edad, xlab = 'Edad', ylab = 'Residuales Parciales', pch = 20)
abline(lm(resig1 ~ base1$edad))
lines(lowess(base1$edad, resig1), lty = 2, col = 'red')
#plot 2
plot(resig4 ~ base1$cant, xlab = 'Cantidad', ylab = '', pch = 20)
abline(lm(resig4 ~ base1$cant))
lines(lowess(base1$cant, resig4), lty = 2, col = 'red')

mtext('Linealidad modelo ajustado', outer = TRUE, cex = 1.5)
par(mfrow = c(1,1))

#podemos observar en este último gráfico, como la linealidad mejoró significativamente al meter los términos cuadráticos y la respuesta en logarítmo
```
  
  - El modelo sin el valor extremo parece haber arreglo la linealidad un poco después de haber hecho las transformaciones correspondientes. Parece ser que el modelo 1 planteado soluciona la linealidad. 

### multicolinealidad

```{r}
round(cor(base1[ ,-c(2,4,7,9,10)]), 3)
round(vif(mod1), 4)
round(apply(base1[ ,-c(1,2,4,7,9,10)], 2, var), 2)

#El factor de inflación de la variancia es bastante alto, pero la variancia para el sabor es bastante pequeña.
```


### Homocedasticidad

```{r}
par(mfrow = c(2,3),oma = c(0, 0, 2, 0))
plot(mod1$residuals ~ mod$fitted.values, xlab = 'Valores ajustados', ylab = 'Residuales')
plot(mod1$residuals ~ base1$edad, ylab = '', xlab = 'Edad')
boxplot(mod1$residuals ~ base1$sexo, ylab = '', xlab = 'Sexo')
plot(mod1$residuals ~ base1$cant, ylab = '', xlab = 'Cantidad')
boxplot(mod1$residuals ~ base1$dieta, ylab = 'Residuales', xlab = 'Dieta')
boxplot(mod1$residuals ~ base1$diabetico, ylab = '', xlab = 'Diabético')
mtext('Homocedasticidad modelo ajustado', outer = TRUE, cex = 1.5)
par(mfrow = c(1,1))
```

  - Al haber hecho la transformación de la variable respuesta a escala logarítmica, también se maximizó la verosimilitud y cuando analizamos los residuales contra sus variables en los dos modelo podemos observar que su escala en Y cambió. 

$H0:homocedasticidad$
$H1: heterocedasticidad$

```{r}
bptest(mod1, studentize = FALSE)
```

### Normalidad

```{r}
qqPlot(mod1$residuals, ylab = 'Residuales', xlab = 'Cuantiles', main = 'Normalidad modelo ajustado')

ks.test(mod1$residuals, 'pnorm', sd(mod1$residuals))
```

```{r}
anova(lm(log(gasto) ~ 1, base1), mod1)
```

- La prueba Omnibus fue superada con éxito.

# Interacción

```{r}
mod2 = lm(log(gasto) ~ sexo + edad + I(edad^2) + cant + dieta + diabetico + sexo:edad + sexo:I(edad^2) + sexo:cant + dieta:edad + dieta:I(edad^2) + dieta:cant + diabetico:edad + diabetico:I(edad^2) + diabetico:cant, base1)
```

+ Prueba de interacción

```{r}
p1 = ggplot(base1, aes(edad, gasto, color = sexo)) + geom_point() + geom_smooth(method = 'lm',se = F) + theme_test() + labs( y = '', x = 'Edad')
p4 = ggplot(base1, aes(cant, gasto, color = sexo)) + geom_point() + geom_smooth(method = 'lm',se = F) + theme_test()  + labs( y = '', x = 'Cantidad')

grid.arrange(p1,p4, top = 'Interacciones por sexo', left = 'Gasto')
```


```{r}
p1 = ggplot(base1, aes(edad, gasto, color = dieta)) + geom_point() + geom_smooth(method = 'lm',se = F) + theme_test()  + labs( y = '',x='Edad')
p2 = ggplot(base1, aes(cant, gasto, color = dieta)) + geom_point() + geom_smooth(method = 'lm',se = F) + theme_test()  + labs( y = '',x='Cantidad')
p3 = ggplot(base1, aes(edad, gasto, color = diabetico)) + geom_point() + geom_smooth(method = 'lm',se = F) + theme_test()  + labs( y = '')
p4 = ggplot(base1, aes(cant, gasto, color = diabetico)) + geom_point() + geom_smooth(method = 'lm',se = F) + theme_test()  + labs( y = '')

grid.arrange(p1,p2,p3, p4, top = 'Interacciones por dieta y diabetes', left = 'Gasto')

```


```{r}
anova(mod1, mod2)
```

  - No hay interacción
  

  - el modelo con la educación parece ser mejor para el modelo sin embargo no genera ninguna diferencia. 


```{r}
summary(mod1)
drop1(mod1, test = 'F')
anova(mod1)
```

## Interpretaciones razón de medias

+ para interpretar vamos a necesitar exponenciar esto

### Sexo
  - cuando es mujer: sexo es 1 y el resto es 0

- el modelo es

$$\mu_{G|X} = \beta_0 + \beta_1S+ \beta_2E+\beta_3E^2+\beta_4C+\beta_5D+\beta_6Diab$$
  
$$\frac{e^{\beta_0}}{e^{\beta_0+\beta_1}}$$
O sea... para mujeres

$$e^{\beta_1}$$

para hombres...

$$\frac{1}{e^{\beta_1}}$$

```{r}
1/exp(mod1$coefficients[2])
```

  - Se espera que los hombres gasten en promedio 1.34 veces más que las mujeres, manteniendo el resto constante.
  
### Dieta

$$\frac{e^{\beta_0}}{e^{\beta_0+\beta_1}}$$

$$\frac{1}{e^{\beta_1}}$$

```{r}
1/exp(mod1$coefficients[6])
```

  - Se espera que los consumidores que no siguen una dieta, gasten en promedio 1.35 veces más que los que siguen una dieta, manteniendo el resto de variables constante. 

### diabéticos

```{r}
exp(mod1$coefficients[7])
```

  - Se espera que los consumidores que son diabéticos, gasten en promedio 1.33 veces más que los consumidores no diabéticos, manteniendo el resto constante. 

### Edad

```{r}
sd(base1$edad)

exp(sd(base1$edad)*mod1$coefficients[3])
```

  - por cada 11 años,en promedio el gasto disminuye en 0.96 veces. 
  
### Cantidad

```{r}
sd(base1$cant)

exp(sd(base1$cant)* mod1$coefficients[5])
```

  - Por cada 4 productos, el gasto aumenta en promedio 1.30 veces. 
  

Dado que es hombre y diabético, manteniendo el resto constante

$$\frac{e^{\beta_0+\beta_6}}{e^{\beta_0+\beta_1}}$$

$$\frac{1}{e^{\beta_1+\beta_6}}$$

```{r}
1/exp(mod1$coefficients[2]+mod1$coefficients[6])
```

dado que es hombre + cant[variable]

$$\frac{e^{3.81*\beta_4}}{e^{\beta_1}}$$

```{r}
exp(sd(base1$cant)*mod1$coefficients[5])/exp(mod1$coefficients[2])
```


$$\frac{e^{}}{e^{}}$$

```{r}

1/exp(mod1$coefficients[2] + mod1$coefficients[5])
```

  - Se espera que un hombre que no sigue una dieta, gaste en promedio 1.25 veces más que una mujer que sigue una dieta, manteniendo el resto constante. 


