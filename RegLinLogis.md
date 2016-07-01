Machine Learning vs Análisis Estadístico
----------------------------------------

Ejemplo pricing de una empresa: si quiero optimizar precios sera machine
learning (no puedo interpretar coeficientes del modelo como en Analisis
Estadístico). En machine learning hago pruebas de estabilidad (cambio en
training) Si quieres algo que prediga directamente utilizas Machine
Learning.

Si quieres entender porque ocurren las cosas y como se interpretan las
cosas....Análisis estadístico. (como se fija y porque, por ejemplo, para
explicar a un cliente)

Modelos machine learning y Analisis Estadísticos no son los mismo.

Tenemos:

-   Modelo, saco coeficientes según que minimizo, no hay un modelo hay
    muchos modelos, hay diferentes coeficientes según si minimizo error,
    max beneficios, minimiza error cuadrado.... a partir de esa
    formula predecimos.
-   Estimación
-   Explotación

Analisis Estadístico

Regresión lineal: me interesan las variables que pueda controlar, sobre
las que pueda hacer algun cambio para mejorar el resultado.

Hipótesis: creo que si bajo el precio subirán las ventas un 20%, hago un
experimento y saco mi conclusion al 95%

Las variables tienen que ser independientes, la varianza tiene que ser
igual para todas las observaciones, los errores tienen distribucion
normal.

Si no se cumple lo anterior la interpretación nos dará datos pero no
será completamente correcto. Pero a veces es mejor tener esta
información a nada.

Estimación: MCO minimos cuadrados, el valor que tenía menos el valor que
tiene por las variables predictivas. El error es la realidad menos la
estimación. Lo elevo al cuadrado para que los negativos y positivos no
se compensen. Busco el valor de "a"(coeficientes) que haga que eso sea
más pequeño.(Los coeficientes solo son insesgados en MCO, el valor aprox
el limite al valor real) Si utilizo valores absolutos me dará otros
coeficientes.

RSA casos que no se pueden resolver. Buscar un password, hay tantas
combinaciones que la tecnologia no lo permite.

gradient accent:busca optimos locales, iterando, tenga la forma que
tenga la funcion, siempre te da algo.

Descomposición de la varianza:

Quiero calcular las variaciones de y (variable dependiente), tenemos la
varianza de y, tengo la varianza del error. varianza y = varianza modelo
+ varianza error

varianza del modelo/varianza y = % coef denominacion que pocentaje de y
está representado en el modelo

Hay que poner un criterio, entre que porcentaje tiene que estar. Hay
cosas que serán totalmente subjetivos. Si todas las variables que tu
quieres estudiar están en el modelo, para ti estará bien aunque salga
error alto.

Ejemplo Regresion lineal: lm(cantidad~Precio,data=ventas)

Tengo que saber en que unidades estanlas variables, miles.... Tiene que
tener sentido el modelo, puede no ser lineal.

std.error si es igual que a, mal asunto es error es igual que el
coeficiente. (cuando hago el lm)

t value es el coef entre estimate/15.778 o lo que es lo mismo
a/std.error . El p valor es la probabilidad que sea mayor que el valor
absoluto de t value.

Multiple R-squared:0.8233 el modelo ajusta el 0.82%, y está representado
en el modelo este porcentaje (coef.determinacion)

Adjusted R-squered: Te penalizaa por no tener algun parametro. Pero te
da una ganancia. La gente se suele fijar en éste.N registros,K
variables. Si aumento K le cociente es más grande

F-statistic: contraste de todo el modelo. el modelo sirve para algo??
compara modelos, mejora tener el modelo solo de la constante. p-value es
menor de 0.01, aporta algo.

El contraste F te permite comparar un modelo con el siguiente, el
primero tiene solo una variable, añado otra, entonces comparo los dos
para ver si me sirve o no meter esa variable.

Práctica Regresión Lineal
=========================

Iniciamos librerias:

    library(ggplot2)
    library(effects)
    library(plyr)
    library(ROCR)

    ## Loading required package: gplots

    ## 
    ## Attaching package: 'gplots'

    ## The following object is masked from 'package:stats':
    ## 
    ##     lowess

Carga de datos:

    creditos=read.csv("D:/master/data/Regresiones/creditos.csv",stringsAsFactors = FALSE)

Revisión de los datos:

-   income es salario hora
-   product contratados por el cliente
-   educacion años educacion, cuantos mas mas educacion
-   Balance saldo cuenta cliente

<!-- -->

    str(creditos)

    ## 'data.frame':    300 obs. of  10 variables:
    ##  $ Income   : num  14.9 106 104.6 148.9 55.9 ...
    ##  $ Rating   : int  283 483 514 681 357 569 259 512 266 491 ...
    ##  $ Products : int  2 3 4 3 2 4 2 2 5 3 ...
    ##  $ Age      : int  34 82 71 36 68 77 37 87 66 41 ...
    ##  $ Education: int  11 15 11 11 16 10 12 9 13 19 ...
    ##  $ Gender   : chr  " Male" "Female" " Male" "Female" ...
    ##  $ Mortgage : chr  "No" "Yes" "No" "No" ...
    ##  $ Married  : chr  "Yes" "Yes" "No" "No" ...
    ##  $ Ethnicity: chr  "Caucasian" "Asian" "Asian" "Asian" ...
    ##  $ Balance  : int  333 903 580 964 331 1151 203 872 279 1350 ...

    head(creditos)

    ##    Income Rating Products Age Education Gender Mortgage Married Ethnicity
    ## 1  14.891    283        2  34        11   Male       No     Yes Caucasian
    ## 2 106.025    483        3  82        15 Female      Yes     Yes     Asian
    ## 3 104.593    514        4  71        11   Male       No      No     Asian
    ## 4 148.924    681        3  36        11 Female       No      No     Asian
    ## 5  55.882    357        2  68        16   Male       No     Yes Caucasian
    ## 6  80.180    569        4  77        10   Male       No      No Caucasian
    ##   Balance
    ## 1     333
    ## 2     903
    ## 3     580
    ## 4     964
    ## 5     331
    ## 6    1151

    tail(creditos)

    ##     Income Rating Products Age Education Gender Mortgage Married
    ## 295 42.915    205        4  42        13   Male       No     Yes
    ## 296 27.272    149        5  67        10 Female       No     Yes
    ## 297 65.896    370        1  49        17 Female       No     Yes
    ## 298 55.054    321        3  74        17   Male       No     Yes
    ## 299 20.791    204        1  70        18 Female       No      No
    ## 300 24.919    372        3  76        11 Female       No     Yes
    ##            Ethnicity Balance
    ## 295            Asian       0
    ## 296        Caucasian       0
    ## 297        Caucasian     293
    ## 298            Asian     188
    ## 299 African American       0
    ## 300 African American     711

    summary(creditos)

    ##      Income           Rating         Products          Age       
    ##  Min.   : 10.35   Min.   : 93.0   Min.   :1.000   Min.   :24.00  
    ##  1st Qu.: 21.03   1st Qu.:235.0   1st Qu.:2.000   1st Qu.:41.00  
    ##  Median : 33.12   Median :339.0   Median :3.000   Median :55.00  
    ##  Mean   : 44.05   Mean   :348.1   Mean   :3.027   Mean   :54.98  
    ##  3rd Qu.: 55.98   3rd Qu.:433.0   3rd Qu.:4.000   3rd Qu.:69.00  
    ##  Max.   :186.63   Max.   :949.0   Max.   :8.000   Max.   :91.00  
    ##    Education        Gender            Mortgage           Married         
    ##  Min.   : 5.00   Length:300         Length:300         Length:300        
    ##  1st Qu.:11.00   Class :character   Class :character   Class :character  
    ##  Median :14.00   Mode  :character   Mode  :character   Mode  :character  
    ##  Mean   :13.39                                                           
    ##  3rd Qu.:16.00                                                           
    ##  Max.   :20.00                                                           
    ##   Ethnicity            Balance       
    ##  Length:300         Min.   :   0.00  
    ##  Class :character   1st Qu.:  15.75  
    ##  Mode  :character   Median : 433.50  
    ##                     Mean   : 502.69  
    ##                     3rd Qu.: 857.75  
    ##                     Max.   :1809.00

Tratamiento de variables:

La regresion lineal y logistica trabaja con Factores El coef sobre la
variable Educacion no es continua, aplico el mismo coeficiente a cada
año que tengo de mas de educacion b1\*x1, pero no es lo mismo 10 años
que 11 que he terminado.(eso sería tratarlo como una variable continua)

    creditos$Gender=as.factor(creditos$Gender)
    creditos$Mortgage=as.factor(creditos$Mortgage)
    creditos$Married=as.factor(creditos$Married)
    creditos$Ethnicity=as.factor(creditos$Ethnicity)

    summary(creditos)

    ##      Income           Rating         Products          Age       
    ##  Min.   : 10.35   Min.   : 93.0   Min.   :1.000   Min.   :24.00  
    ##  1st Qu.: 21.03   1st Qu.:235.0   1st Qu.:2.000   1st Qu.:41.00  
    ##  Median : 33.12   Median :339.0   Median :3.000   Median :55.00  
    ##  Mean   : 44.05   Mean   :348.1   Mean   :3.027   Mean   :54.98  
    ##  3rd Qu.: 55.98   3rd Qu.:433.0   3rd Qu.:4.000   3rd Qu.:69.00  
    ##  Max.   :186.63   Max.   :949.0   Max.   :8.000   Max.   :91.00  
    ##    Education        Gender    Mortgage  Married              Ethnicity  
    ##  Min.   : 5.00    Male :132   No :268   No :117   African American: 78  
    ##  1st Qu.:11.00   Female:168   Yes: 32   Yes:183   Asian           : 81  
    ##  Median :14.00                                    Caucasian       :141  
    ##  Mean   :13.39                                                          
    ##  3rd Qu.:16.00                                                          
    ##  Max.   :20.00                                                          
    ##     Balance       
    ##  Min.   :   0.00  
    ##  1st Qu.:  15.75  
    ##  Median : 433.50  
    ##  Mean   : 502.69  
    ##  3rd Qu.: 857.75  
    ##  Max.   :1809.00

*Test diferencia de medias Regresion lineal:*

    t.test(Income ~ Gender, data = creditos)#p-value=0.7345 t=0.3395,mean female=43,46, male=44,8

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  Income by Gender
    ## t = 0.3395, df = 284.51, p-value = 0.7345
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -6.405656  9.075923
    ## sample estimates:
    ##  mean in group  Male mean in group Female 
    ##             44.80207             43.46693

Resultado: **No hay evidencia significativa de que sean diferentes. No
podemos rechazar la igualdad de las medias**

*Modelo Lineal* En este caso, el R² es simplemente el cuadrado del
coeficiente de correlación de Pearson, lo cual es sólo cierto para la
regresión lineal simple

    modeloT=lm(Income ~ Gender, data = creditos)
    summary(modeloT)

    ## 
    ## Call:
    ## lm(formula = Income ~ Gender, data = creditos)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -34.45 -22.49 -11.33  11.99 143.17 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    44.802      2.952  15.178   <2e-16 ***
    ## GenderFemale   -1.335      3.944  -0.338    0.735    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 33.91 on 298 degrees of freedom
    ## Multiple R-squared:  0.0003843,  Adjusted R-squared:  -0.00297 
    ## F-statistic: 0.1146 on 1 and 298 DF,  p-value: 0.7352

Recta de regresión es y= 44.802 - 1.335\*x (x=1 cuando es mujer, 0
cuando es hombre) std-error= 2.952 p-value=0.7352 cuando x=1, y =43,46
Podría coger otro modelo e ir metiendo variables, el modelo perfecto es
el que las variables fueran independientes si no lo son te generan
ruido, cuando sube una baja la otra. Resultado:

**No aporta nada este modelo casi es el valor de la constante.**

Regresión Lineal Individual
---------------------------

Mide correlacion, no mide causa-efecto. Está relacionado, ejemplo, la
edad influye en el impago, con la edad gana más dinero, lo que influye
no es la edad es el ingreso...Si metes la variable ingreso la edad
desaparece. Vamos viendo una a una las variables:

    modeloInd1=lm(Income ~ Rating, data = creditos)# Rating es Puntuaje sobre la capacidad impago 
    #de 0-1000, cuanto mas grande mejor pagador.
    summary(modeloInd1)#el rating explica el 60% de los ingresos o los ingresos explican el 60% del 

    ## 
    ## Call:
    ## lm(formula = Income ~ Rating, data = creditos)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -40.05 -15.74  -0.80  14.14  81.48 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -16.200514   3.139692   -5.16 4.52e-07 ***
    ## Rating        0.173088   0.008278   20.91  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 21.59 on 298 degrees of freedom
    ## Multiple R-squared:  0.5947, Adjusted R-squared:  0.5933 
    ## F-statistic: 437.3 on 1 and 298 DF,  p-value: < 2.2e-16

    #rating

    modeloInd2=lm(Income ~ Products, data = creditos)#no influye
    summary(modeloInd2)

    ## 
    ## Call:
    ## lm(formula = Income ~ Products, data = creditos)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -36.86 -22.28 -10.90  12.55 143.32 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  41.8640     4.8091   8.705   <2e-16 ***
    ## Products      0.7237     1.4513   0.499    0.618    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 33.91 on 298 degrees of freedom
    ## Multiple R-squared:  0.0008337,  Adjusted R-squared:  -0.002519 
    ## F-statistic: 0.2487 on 1 and 298 DF,  p-value: 0.6184

    modeloInd3=lm(Income ~ Age, data = creditos)
    summary(modeloInd3)

    ## 
    ## Call:
    ## lm(formula = Income ~ Age, data = creditos)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -37.58 -23.34 -10.35  10.45 145.97 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  30.7310     6.5135   4.718 3.67e-06 ***
    ## Age           0.2423     0.1131   2.143   0.0329 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 33.66 on 298 degrees of freedom
    ## Multiple R-squared:  0.01518,    Adjusted R-squared:  0.01187 
    ## F-statistic: 4.593 on 1 and 298 DF,  p-value: 0.03291

    modeloInd4=lm(Income ~ Education, data = creditos)#p-value 0.22 no es significativo
    summary(modeloInd4)

    ## 
    ## Call:
    ## lm(formula = Income ~ Education, data = creditos)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -38.65 -22.54 -11.81  12.12 143.05 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  54.5197     8.7430   6.236 1.54e-09 ***
    ## Education    -0.7814     0.6363  -1.228     0.22    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 33.83 on 298 degrees of freedom
    ## Multiple R-squared:  0.005035,   Adjusted R-squared:  0.001696 
    ## F-statistic: 1.508 on 1 and 298 DF,  p-value: 0.2204

    modeloInd5=lm(Income ~ Gender, data = creditos)
    summary(modeloInd5)

    ## 
    ## Call:
    ## lm(formula = Income ~ Gender, data = creditos)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -34.45 -22.49 -11.33  11.99 143.17 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    44.802      2.952  15.178   <2e-16 ***
    ## GenderFemale   -1.335      3.944  -0.338    0.735    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 33.91 on 298 degrees of freedom
    ## Multiple R-squared:  0.0003843,  Adjusted R-squared:  -0.00297 
    ## F-statistic: 0.1146 on 1 and 298 DF,  p-value: 0.7352

    modeloInd6=lm(Income ~ Mortgage, data = creditos)#no influye la hipoteca
    summary(modeloInd6)

    ## 
    ## Call:
    ## lm(formula = Income ~ Mortgage, data = creditos)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -38.64 -22.94 -10.58  12.06 143.20 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   43.432      2.069  20.991   <2e-16 ***
    ## MortgageYes    5.833      6.335   0.921    0.358    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 33.87 on 298 degrees of freedom
    ## Multiple R-squared:  0.002837,   Adjusted R-squared:  -0.0005095 
    ## F-statistic: 0.8477 on 1 and 298 DF,  p-value: 0.3579

    modeloInd7=lm(Income ~ Married, data = creditos)
    summary(modeloInd7)

    ## 
    ## Call:
    ## lm(formula = Income ~ Married, data = creditos)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -34.76 -22.74 -10.78  11.93 141.53 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   42.404      3.134  13.533   <2e-16 ***
    ## MarriedYes     2.705      4.012   0.674    0.501    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 33.89 on 298 degrees of freedom
    ## Multiple R-squared:  0.001523,   Adjusted R-squared:  -0.001828 
    ## F-statistic: 0.4545 on 1 and 298 DF,  p-value: 0.5007

    modeloInd8=lm(Income ~ Ethnicity, data = creditos)
    summary(modeloInd8)

    ## 
    ## Call:
    ## lm(formula = Income ~ Ethnicity, data = creditos)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -36.14 -22.87 -11.75  12.21 139.99 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          46.641      3.843  12.136   <2e-16 ***
    ## EthnicityAsian       -3.557      5.384  -0.661    0.509    
    ## EthnicityCaucasian   -3.461      4.790  -0.723    0.471    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 33.94 on 297 degrees of freedom
    ## Multiple R-squared:  0.002059,   Adjusted R-squared:  -0.004661 
    ## F-statistic: 0.3064 on 2 and 297 DF,  p-value: 0.7363

    modeloInd9=lm(Income ~ Balance, data = creditos)#si influye, y explica el 0.1869 del income
    summary(modeloInd9)

    ## 
    ## Call:
    ## lm(formula = Income ~ Balance, data = creditos)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -57.246 -17.986  -5.543   9.792 119.167 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 28.295463   2.596872  10.896  < 2e-16 ***
    ## Balance      0.031349   0.003788   8.277  4.3e-15 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 30.59 on 298 degrees of freedom
    ## Multiple R-squared:  0.1869, Adjusted R-squared:  0.1842 
    ## F-statistic:  68.5 on 1 and 298 DF,  p-value: 4.3e-15

Regresión Linial Múltiple
-------------------------

Introduzco todas las variables, ahora la hipoteca si que influye, porque
las variables no son independientes a igualdad de todas las variables,
los que tienen hipoteca tendrán menos saldo, menor rating....efectos
conjuntos. Hemos capturado el 0.8965 del Income, todas las variables
explican el 89,65 % de los ingresos. Multiple R-Squared. vamos a quitar
el resto de las variables(menos rating, Balance y Mortgage), para ver
las diferencias....

    modeloMul1=lm(Income ~ ., data = creditos)
    summary(modeloMul1)

    ## 
    ## Call:
    ## lm(formula = Income ~ ., data = creditos)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -19.484  -7.151  -2.572   5.736  33.034 
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)        -55.765554   4.614857 -12.084   <2e-16 ***
    ## Rating               0.414226   0.009515  43.533   <2e-16 ***
    ## Products             0.194289   0.480488   0.404    0.686    
    ## Age                 -0.020811   0.038354  -0.543    0.588    
    ## Education           -0.114133   0.210614  -0.542    0.588    
    ## GenderFemale        -1.042506   1.296913  -0.804    0.422    
    ## MortgageYes         39.923786   2.455449  16.259   <2e-16 ***
    ## MarriedYes          -1.028593   1.337745  -0.769    0.443    
    ## EthnicityAsian       1.740019   1.782636   0.976    0.330    
    ## EthnicityCaucasian   0.291304   1.571722   0.185    0.853    
    ## Balance             -0.091405   0.003220 -28.386   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 11.08 on 289 degrees of freedom
    ## Multiple R-squared:  0.8965, Adjusted R-squared:  0.893 
    ## F-statistic: 250.4 on 10 and 289 DF,  p-value: < 2.2e-16

Multiple R-squared:

Múltiple R-cuadrado se utiliza para evaluar como el modelo se ajusta a
los datos. Te dice cuánto de la variación en la variable dependiente (
la variable predicha ) puede ser explicado por las variables
independientes ( las variables de predicción ). Por ejemplo, un R valor
de 0.75 -squared implica que el modelo puede explicar las tres cuartas
partes de la variación en el resultado. Ahora, para entender la
diferencia entre ellos, es importante saber que cada vez que se agrega
una variable independiente al modelo , el valor R-cuadrado se
incrementará . Porqué es eso ? Debido a que el modelo trata de capturar
tanto la información como cualquier ruido en la nueva variable. No
sabemos si el aumento en el valor R cuadrado es debido a la capacidad de
predicción real de la nueva variable o debido a la casualidad. Ajustado
R-cuadrado también proporciona la misma información que R cuadrado, pero
se ajusta para el número de términos en el modelo. No aumenta
monótonamente como R-cuadrado, pero sólo aumenta cuando la nueva
variable en realidad tiene un efecto sobre el valor predicho. Se
disminuye cuando la nueva variable no tiene ningún impacto real sobre el
valor predicho.

R &lt;- 0.8965 N &lt;- 300 p &lt;- 10

Radj &lt;- 1-(((1-R)\*(N-1))/(N-p-1))

Comparación de modelos
----------------------

Comparamos el Income con el Rating y el Income con el Rating y todas las
demás.El Rating explicaba el 60% del Income. modeloInd1=lm(Income ~
Rating, data = creditos) modeloMul1=lm(Income ~ ., data = creditos)

    anova(modeloInd1,modeloMul1)

    ## Analysis of Variance Table
    ## 
    ## Model 1: Income ~ Rating
    ## Model 2: Income ~ Rating + Products + Age + Education + Gender + Mortgage + 
    ##     Married + Ethnicity + Balance
    ##   Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
    ## 1    298 138964                                  
    ## 2    289  35476  9    103488 93.673 < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    #sale que tiene sentido

-   Sum of squares
-   df:degrees of freedom
-   RSS: Sum squares
-   F:F- ratio
-   Pr(&gt;F):p-value menor que 0.05

¿Cuales serian las variables que incluiriamos en el modelo?
-----------------------------------------------------------

    modeloMul2=lm(Income ~Rating+Balance+Mortgage, data = creditos)
    summary(modeloMul2)

    ## 
    ## Call:
    ## lm(formula = Income ~ Rating + Balance + Mortgage, data = creditos)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -20.263  -7.425  -2.544   6.158  32.580 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -58.178638   2.153774  -27.01   <2e-16 ***
    ## Rating        0.412810   0.009254   44.61   <2e-16 ***
    ## Balance      -0.090950   0.003122  -29.13   <2e-16 ***
    ## MortgageYes  39.811486   2.408393   16.53   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 11 on 296 degrees of freedom
    ## Multiple R-squared:  0.8955, Adjusted R-squared:  0.8944 
    ## F-statistic: 845.1 on 3 and 296 DF,  p-value: < 2.2e-16

Estas tres variables explican el 89.55% de la varianza del Income. 39.8
mas de income el que tiene hipoteca con respecto al que no. El dato que
sale es con respecto a los que no tienen hipoteca.

    anova(modeloInd1,modeloMul2)

    ## Analysis of Variance Table
    ## 
    ## Model 1: Income ~ Rating
    ## Model 2: Income ~ Rating + Balance + Mortgage
    ##   Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
    ## 1    298 138964                                  
    ## 2    296  35847  2    103117 425.74 < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

La suma de cuadrados es casi igual que cuando comparamos anteriormente
con todas las variables.

    anova(modeloMul2,modeloMul1)

    ## Analysis of Variance Table
    ## 
    ## Model 1: Income ~ Rating + Balance + Mortgage
    ## Model 2: Income ~ Rating + Products + Age + Education + Gender + Mortgage + 
    ##     Married + Ethnicity + Balance
    ##   Res.Df   RSS Df Sum of Sq      F Pr(>F)
    ## 1    296 35847                           
    ## 2    289 35476  7     370.7 0.4314 0.8822

Comparamos el modelo con las 3 variables con respecto al modelo que las
incluía todas. No aporta ninguna información. No aporta nada el modelo
Mul1 al modelo Mul2, si no fuera así hay una variable que se escapa.

Análisis del Modelo
===================

    modeloFinal=lm(Income ~ Rating+Mortgage+Balance, data = creditos)
    summary(modeloFinal)

    ## 
    ## Call:
    ## lm(formula = Income ~ Rating + Mortgage + Balance, data = creditos)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -20.263  -7.425  -2.544   6.158  32.580 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -58.178638   2.153774  -27.01   <2e-16 ***
    ## Rating        0.412810   0.009254   44.61   <2e-16 ***
    ## MortgageYes  39.811486   2.408393   16.53   <2e-16 ***
    ## Balance      -0.090950   0.003122  -29.13   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 11 on 296 degrees of freedom
    ## Multiple R-squared:  0.8955, Adjusted R-squared:  0.8944 
    ## F-statistic: 845.1 on 3 and 296 DF,  p-value: < 2.2e-16

    plot(modeloFinal$residuals)

![](RegLinLogis_files/figure-markdown_strict/unnamed-chunk-22-1.png)

    hist(modeloFinal$residuals)

![](RegLinLogis_files/figure-markdown_strict/unnamed-chunk-23-1.png)

    qqnorm(modeloFinal$residuals); qqline(modeloFinal$residuals,col=2)

![](RegLinLogis_files/figure-markdown_strict/unnamed-chunk-24-1.png)
Gráfico de percentiles tienen que estar todos en la recta si es normal
vemos que en las colas hay problemas. No vale la regressión lineal, no
es insesgada,el valor de la poblacion no se estima de manera exacta con
el coeficiente.

    confint(modeloFinal,level=0.95)

    ##                    2.5 %       97.5 %
    ## (Intercept) -62.41728868 -53.93998677
    ## Rating        0.39459856   0.43102128
    ## MortgageYes  35.07174280  44.55122908
    ## Balance      -0.09709433  -0.08480642

Indicamos al nivel que queremos los intervalos de confianza, entre que
rangos van los coeficientes.

    anova(modeloFinal,modeloMul1)

    ## Analysis of Variance Table
    ## 
    ## Model 1: Income ~ Rating + Mortgage + Balance
    ## Model 2: Income ~ Rating + Products + Age + Education + Gender + Mortgage + 
    ##     Married + Ethnicity + Balance
    ##   Res.Df   RSS Df Sum of Sq      F Pr(>F)
    ## 1    296 35847                           
    ## 2    289 35476  7     370.7 0.4314 0.8822

Modelo Final comparado con modelo con todas las variables. El resultado
es que el modelo con todas no aporta nada al modelo Final.

Los que tienen hipoteca no los estima tan bien, las rectas son muy
buenas en los centros, em los bordes no, no valen para predecir.

hay diferencias en las pendientes, por eso sale significativa mortgage

    ggplot(creditos, aes(x = Rating, y = Income)) + geom_point() + facet_grid(~ Mortgage) + 
      geom_smooth(method = "lm", se=TRUE, color="red", formula = y ~ x)

![](RegLinLogis_files/figure-markdown_strict/unnamed-chunk-27-1.png) mas
o menos todas se solapan por eso en el modelo no son significativas, no
es conjunto aquí no tenemos en cuenta el resto de variables

    ggplot(creditos, aes(x = Rating, y = Income)) + geom_point() + facet_grid(~ Ethnicity) + 
      geom_smooth(method = "lm", se=TRUE, color="red", formula = y ~ x)

![](RegLinLogis_files/figure-markdown_strict/unnamed-chunk-28-1.png)

    ggplot(creditos, aes(x = Balance, y = Income)) + geom_point() + facet_grid(~ Mortgage) + 
      geom_smooth(method = "lm", se=TRUE, color="red", formula = y ~ x)

    ggplot(creditos, aes(x = Balance, y = Income)) + geom_point() + facet_grid(~ Ethnicity) + 
      geom_smooth(method = "lm", se=TRUE, color="red", formula = y ~ x)

![](RegLinLogis_files/figure-markdown_strict/unnamed-chunk-29-1.png)

Análisis de interacciones
-------------------------

Ejemplo: Modelo ingresos dependen de y=a+b*sexo+c*estado civil, puedo
inventarme una nueva variable d*sexo*Edad (este efecto estoy diciendo
que la edad afecta conjuntamente con el sexo)

    modeloInter1=lm(Income ~ Balance+Rating*Mortgage+Balance:Mortgage, data = creditos)
    #con el : añado la variable sola
    summary(modeloInter1)

    ## 
    ## Call:
    ## lm(formula = Income ~ Balance + Rating * Mortgage + Balance:Mortgage, 
    ##     data = creditos)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -20.654  -7.174  -2.280   5.881  32.890 
    ## 
    ## Coefficients:
    ##                       Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)         -56.444729   2.259586 -24.980  < 2e-16 ***
    ## Balance              -0.090967   0.003376 -26.942  < 2e-16 ***
    ## Rating                0.407830   0.009885  41.258  < 2e-16 ***
    ## MortgageYes          23.681928   5.404132   4.382 1.64e-05 ***
    ## Rating:MortgageYes    0.050340   0.026038   1.933   0.0542 .  
    ## Balance:MortgageYes  -0.002179   0.008176  -0.266   0.7901    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 10.83 on 294 degrees of freedom
    ## Multiple R-squared:  0.8994, Adjusted R-squared:  0.8977 
    ## F-statistic: 525.6 on 5 and 294 DF,  p-value: < 2.2e-16

Influyen en Income el Balance, el Rating y el Mortgage.

    modeloInter2=lm(Income ~ Rating*Mortgage+Balance, data = creditos)
    summary(modeloInter2)

    ## 
    ## Call:
    ## lm(formula = Income ~ Rating * Mortgage + Balance, data = creditos)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -20.693  -7.213  -2.370   5.676  32.913 
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)        -56.612697   2.166470 -26.131  < 2e-16 ***
    ## Rating               0.408803   0.009171  44.578  < 2e-16 ***
    ## MortgageYes         24.046930   5.219430   4.607 6.08e-06 ***
    ## Balance             -0.091339   0.003070 -29.750  < 2e-16 ***
    ## Rating:MortgageYes   0.044344   0.013085   3.389 0.000797 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 10.81 on 295 degrees of freedom
    ## Multiple R-squared:  0.8994, Adjusted R-squared:  0.898 
    ## F-statistic: 659.1 on 4 and 295 DF,  p-value: < 2.2e-16

Si que encuentra relación entre Rating y Mortgage para explicar la
varianza de Income.

    modeloInter3=lm(Income ~ Rating:Mortgage+Balance, data = creditos)
    summary(modeloInter3)

    ## 
    ## Call:
    ## lm(formula = Income ~ Rating:Mortgage + Balance, data = creditos)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -21.240  -7.669  -1.837   7.255  35.209 
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)        -52.879756   2.076767  -25.46   <2e-16 ***
    ## Balance             -0.088485   0.003108  -28.47   <2e-16 ***
    ## Rating:MortgageNo    0.395390   0.008988   43.99   <2e-16 ***
    ## Rating:MortgageYes   0.493467   0.012566   39.27   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 11.18 on 296 degrees of freedom
    ## Multiple R-squared:  0.8921, Adjusted R-squared:  0.891 
    ## F-statistic:   816 on 3 and 296 DF,  p-value: < 2.2e-16

separo el efecto del rating en dos bloques si mortgage es no incrementa
el raiting 0.39

    efecto1 <- effect("Rating*Mortgage", modeloInter1, xlevels = 10)
    plot(efecto1)#la diferencia es que aqui le metes el modelo, con lo que le metes las 

![](RegLinLogis_files/figure-markdown_strict/unnamed-chunk-33-1.png)

    #relaciones con todas las variables

    efecto2 <- effect("Balance*Mortgage", modeloInter1, xlevels = 10)
    plot(efecto2)

![](RegLinLogis_files/figure-markdown_strict/unnamed-chunk-34-1.png)

    efecto3 <- effect("Rating*Mortgage", modeloInter2, xlevels = 10)
    plot(efecto3)

![](RegLinLogis_files/figure-markdown_strict/unnamed-chunk-35-1.png)

    efecto4 <- effect("Rating:Mortgage", modeloInter3, xlevels = 10)
    plot(efecto4)

![](RegLinLogis_files/figure-markdown_strict/unnamed-chunk-36-1.png)

    modeloInter5=lm(Income ~ Rating*Mortgage, data = creditos)
    summary(modeloInter5)

    ## 
    ## Call:
    ## lm(formula = Income ~ Rating * Mortgage, data = creditos)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -39.427 -15.551  -0.579  13.538  70.846 
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)        -15.322939   3.321568  -4.613 5.91e-06 ***
    ## Rating               0.169505   0.008795  19.274  < 2e-16 ***
    ## MortgageYes         -7.283979  10.207199  -0.714    0.476    
    ## Rating:MortgageYes   0.029810   0.026109   1.142    0.254    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 21.59 on 296 degrees of freedom
    ## Multiple R-squared:  0.5974, Adjusted R-squared:  0.5934 
    ## F-statistic: 146.4 on 3 and 296 DF,  p-value: < 2.2e-16

Aquí la hipoteca no representa nada en el Income

    efecto5 <- effect("Rating*Mortgage", modeloInter5, xlevels = 10)
    plot(efecto5)

![](RegLinLogis_files/figure-markdown_strict/unnamed-chunk-38-1.png)

Analisis de variable Balance
----------------------------

    modeloBalance=lm(Balance ~ ., data = creditos)
    summary(modeloBalance)

    ## 
    ## Call:
    ## lm(formula = Balance ~ ., data = creditos)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -196.28  -83.07  -12.69   63.88  310.43 
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)        -542.32300   42.50152 -12.760   <2e-16 ***
    ## Income               -8.05229    0.28367 -28.386   <2e-16 ***
    ## Rating                4.03048    0.06378  63.189   <2e-16 ***
    ## Products              5.86109    4.49787   1.303   0.1936    
    ## Age                  -0.77749    0.35725  -2.176   0.0303 *  
    ## Education            -1.41408    1.97605  -0.716   0.4748    
    ## GenderFemale         -5.91052   12.18127  -0.485   0.6279    
    ## MortgageYes         425.89676   19.73226  21.584   <2e-16 ***
    ## MarriedYes          -10.20768   12.55437  -0.813   0.4168    
    ## EthnicityAsian       13.03881   16.74156   0.779   0.4367    
    ## EthnicityCaucasian    2.56574   14.75205   0.174   0.8620    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 104 on 289 degrees of freedom
    ## Multiple R-squared:  0.9521, Adjusted R-squared:  0.9504 
    ## F-statistic: 574.1 on 10 and 289 DF,  p-value: < 2.2e-16

Todas las variables explican el 95.21 del Balance. Income, Rating y
Mortgage son representativas, Age un 0.0303 de p-valor.

Variables que incluiriamos en el modelo
---------------------------------------

    modeloBalanceFin=lm(Balance ~ Income*Age+Rating+Mortgage, data = creditos)
    summary(modeloBalanceFin)

    ## 
    ## Call:
    ## lm(formula = Balance ~ Income * Age + Rating + Mortgage, data = creditos)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -193.151  -83.464   -2.763   59.774  299.597 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -601.66420   36.21284 -16.615   <2e-16 ***
    ## Income        -6.94221    0.65555 -10.590   <2e-16 ***
    ## Age            0.16438    0.59065   0.278   0.7810    
    ## Rating         4.03363    0.06235  64.694   <2e-16 ***
    ## MortgageYes  429.59524   19.40727  22.136   <2e-16 ***
    ## Income:Age    -0.01959    0.01032  -1.898   0.0587 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 103.1 on 294 degrees of freedom
    ## Multiple R-squared:  0.952,  Adjusted R-squared:  0.9512 
    ## F-statistic:  1167 on 5 and 294 DF,  p-value: < 2.2e-16

    anova(modeloBalanceFin,modeloBalance)

    ## Analysis of Variance Table
    ## 
    ## Model 1: Balance ~ Income * Age + Rating + Mortgage
    ## Model 2: Balance ~ Income + Rating + Products + Age + Education + Gender + 
    ##     Mortgage + Married + Ethnicity
    ##   Res.Df     RSS Df Sum of Sq      F Pr(>F)
    ## 1    294 3127543                           
    ## 2    289 3125213  5    2329.9 0.0431 0.9989

p-valor de 0.9989 incluir el resto de variables no aporta nada al modelo
anterior.

MODELOS LINEALES GENERALIZADOS: REGRESION LOGISTICA
===================================================

Modelo que se utiliza cuando la variable "y"" es dicotómica, si o no,
true o false,bernuilli con probabilidad p. aplicaré la funcion
sigmoide,el resultado es la probabilidad de ocurrencia, para cada
familia de parametros iniciales la probabilidad de que ocurra mi output.
En los modelos de clasificacion todos cortan en el 0.5, acepto +0.5. que
hago entre 0.5 y 1?? minimizas el capital ponderado por riesgo. Buscas
un óptimo de beneficio y coste. el modelo ordena y yo elijo donde corto.

Máxima verosimilitud: para la regresion lineal si se cumple todo, MCO =
max.verosimilitud insesgado:el metodo usado no se va a aproximar a la
media poblacional. Máxima verosimilitud, me acepta un monton de modelos.
Se minimizan los errores al cuadrado, hay un metodo iterativo de Nelder
para encontrar los coeficientes mediante iteraciones.

Matriz de confusión: mezcla la realidad con el modelo, sacamos una serie
de ratios para medir como de bueno es el modelo. True positive rate =
TP/TP+FN (el porcentaje de los que hemos clasificado bien sobre el total
de los que eran buenos)

Curva ROC representa la seleccion del modelo, cuanto mas alta mejor el
modelo, se calcula el area debajo de la curva. cuanto mas cercano a 1
mejor, a 0.5 peor. Tengo que elegir el punto de corte. Mide la capacidad
predictiva de unmodelo,se usa más en machine learning que en analisis
estadístico.

Comparativa modelos:

-   relativos: R^2 ajustado (en regresion lineal)
-   absolutos: BIC bayesian mide la verosimilitud
    (regresion linea,logistica,) siempre se elige el mas bajo. AIC
    akaike
-   Comparativos: Contraste F para comparar varios modelos

Ejemplo regresión lineal generalizado:

Nº VECES ALQUILA BICIS EN MES, NO PUEDE SER NORMAL PORQUE NO PUEDE SER
NEGATIVO...

Carga de Datos
--------------

campaña de venta de depósitos, la "y"" es si compra o no el depósito, el
objetivo es quien va a contratar mi producto.

    BANK=read.csv2("bank-full.csv")

\*datos extraidos de
<https://archive.ics.uci.edu/ml/datasets/Bank+Marketing*>

Revisión Básica Dataset
-----------------------

    str(BANK)

    ## 'data.frame':    45211 obs. of  17 variables:
    ##  $ age      : int  58 44 33 47 33 35 28 42 58 43 ...
    ##  $ job      : Factor w/ 12 levels "admin.","blue-collar",..: 5 10 3 2 12 5 5 3 6 10 ...
    ##  $ marital  : Factor w/ 3 levels "divorced","married",..: 2 3 2 2 3 2 3 1 2 3 ...
    ##  $ education: Factor w/ 4 levels "primary","secondary",..: 3 2 2 4 4 3 3 3 1 2 ...
    ##  $ default  : Factor w/ 2 levels "no","yes": 1 1 1 1 1 1 1 2 1 1 ...
    ##  $ balance  : int  2143 29 2 1506 1 231 447 2 121 593 ...
    ##  $ housing  : Factor w/ 2 levels "no","yes": 2 2 2 2 1 2 2 2 2 2 ...
    ##  $ loan     : Factor w/ 2 levels "no","yes": 1 1 2 1 1 1 2 1 1 1 ...
    ##  $ contact  : Factor w/ 3 levels "cellular","telephone",..: 3 3 3 3 3 3 3 3 3 3 ...
    ##  $ day      : int  5 5 5 5 5 5 5 5 5 5 ...
    ##  $ month    : Factor w/ 12 levels "apr","aug","dec",..: 9 9 9 9 9 9 9 9 9 9 ...
    ##  $ duration : int  261 151 76 92 198 139 217 380 50 55 ...
    ##  $ campaign : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ pdays    : int  -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 ...
    ##  $ previous : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ poutcome : Factor w/ 4 levels "failure","other",..: 4 4 4 4 4 4 4 4 4 4 ...
    ##  $ y        : Factor w/ 2 levels "no","yes": 1 1 1 1 1 1 1 1 1 1 ...

    head(BANK)

    ##   age          job marital education default balance housing loan contact
    ## 1  58   management married  tertiary      no    2143     yes   no unknown
    ## 2  44   technician  single secondary      no      29     yes   no unknown
    ## 3  33 entrepreneur married secondary      no       2     yes  yes unknown
    ## 4  47  blue-collar married   unknown      no    1506     yes   no unknown
    ## 5  33      unknown  single   unknown      no       1      no   no unknown
    ## 6  35   management married  tertiary      no     231     yes   no unknown
    ##   day month duration campaign pdays previous poutcome  y
    ## 1   5   may      261        1    -1        0  unknown no
    ## 2   5   may      151        1    -1        0  unknown no
    ## 3   5   may       76        1    -1        0  unknown no
    ## 4   5   may       92        1    -1        0  unknown no
    ## 5   5   may      198        1    -1        0  unknown no
    ## 6   5   may      139        1    -1        0  unknown no

    summary(BANK)

    ##       age                 job           marital          education    
    ##  Min.   :18.00   blue-collar:9732   divorced: 5207   primary  : 6851  
    ##  1st Qu.:33.00   management :9458   married :27214   secondary:23202  
    ##  Median :39.00   technician :7597   single  :12790   tertiary :13301  
    ##  Mean   :40.94   admin.     :5171                    unknown  : 1857  
    ##  3rd Qu.:48.00   services   :4154                                     
    ##  Max.   :95.00   retired    :2264                                     
    ##                  (Other)    :6835                                     
    ##  default        balance       housing      loan            contact     
    ##  no :44396   Min.   : -8019   no :20081   no :37967   cellular :29285  
    ##  yes:  815   1st Qu.:    72   yes:25130   yes: 7244   telephone: 2906  
    ##              Median :   448                           unknown  :13020  
    ##              Mean   :  1362                                            
    ##              3rd Qu.:  1428                                            
    ##              Max.   :102127                                            
    ##                                                                        
    ##       day            month          duration         campaign     
    ##  Min.   : 1.00   may    :13766   Min.   :   0.0   Min.   : 1.000  
    ##  1st Qu.: 8.00   jul    : 6895   1st Qu.: 103.0   1st Qu.: 1.000  
    ##  Median :16.00   aug    : 6247   Median : 180.0   Median : 2.000  
    ##  Mean   :15.81   jun    : 5341   Mean   : 258.2   Mean   : 2.764  
    ##  3rd Qu.:21.00   nov    : 3970   3rd Qu.: 319.0   3rd Qu.: 3.000  
    ##  Max.   :31.00   apr    : 2932   Max.   :4918.0   Max.   :63.000  
    ##                  (Other): 6060                                    
    ##      pdays          previous           poutcome       y        
    ##  Min.   : -1.0   Min.   :  0.0000   failure: 4901   no :39922  
    ##  1st Qu.: -1.0   1st Qu.:  0.0000   other  : 1840   yes: 5289  
    ##  Median : -1.0   Median :  0.0000   success: 1511              
    ##  Mean   : 40.2   Mean   :  0.5803   unknown:36959              
    ##  3rd Qu.: -1.0   3rd Qu.:  0.0000                              
    ##  Max.   :871.0   Max.   :275.0000                              
    ## 

Formateo de Variables
---------------------

    BANK$day=as.factor(BANK$day)
    BANK$campaign=as.factor(BANK$campaign)
    BANK$IND_PREVIO=as.factor(as.numeric(BANK$pdays!=-1))

    str(BANK)

    ## 'data.frame':    45211 obs. of  18 variables:
    ##  $ age       : int  58 44 33 47 33 35 28 42 58 43 ...
    ##  $ job       : Factor w/ 12 levels "admin.","blue-collar",..: 5 10 3 2 12 5 5 3 6 10 ...
    ##  $ marital   : Factor w/ 3 levels "divorced","married",..: 2 3 2 2 3 2 3 1 2 3 ...
    ##  $ education : Factor w/ 4 levels "primary","secondary",..: 3 2 2 4 4 3 3 3 1 2 ...
    ##  $ default   : Factor w/ 2 levels "no","yes": 1 1 1 1 1 1 1 2 1 1 ...
    ##  $ balance   : int  2143 29 2 1506 1 231 447 2 121 593 ...
    ##  $ housing   : Factor w/ 2 levels "no","yes": 2 2 2 2 1 2 2 2 2 2 ...
    ##  $ loan      : Factor w/ 2 levels "no","yes": 1 1 2 1 1 1 2 1 1 1 ...
    ##  $ contact   : Factor w/ 3 levels "cellular","telephone",..: 3 3 3 3 3 3 3 3 3 3 ...
    ##  $ day       : Factor w/ 31 levels "1","2","3","4",..: 5 5 5 5 5 5 5 5 5 5 ...
    ##  $ month     : Factor w/ 12 levels "apr","aug","dec",..: 9 9 9 9 9 9 9 9 9 9 ...
    ##  $ duration  : int  261 151 76 92 198 139 217 380 50 55 ...
    ##  $ campaign  : Factor w/ 48 levels "1","2","3","4",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ pdays     : int  -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 ...
    ##  $ previous  : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ poutcome  : Factor w/ 4 levels "failure","other",..: 4 4 4 4 4 4 4 4 4 4 ...
    ##  $ y         : Factor w/ 2 levels "no","yes": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ IND_PREVIO: Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 1 1 ...

    head(BANK)

    ##   age          job marital education default balance housing loan contact
    ## 1  58   management married  tertiary      no    2143     yes   no unknown
    ## 2  44   technician  single secondary      no      29     yes   no unknown
    ## 3  33 entrepreneur married secondary      no       2     yes  yes unknown
    ## 4  47  blue-collar married   unknown      no    1506     yes   no unknown
    ## 5  33      unknown  single   unknown      no       1      no   no unknown
    ## 6  35   management married  tertiary      no     231     yes   no unknown
    ##   day month duration campaign pdays previous poutcome  y IND_PREVIO
    ## 1   5   may      261        1    -1        0  unknown no          0
    ## 2   5   may      151        1    -1        0  unknown no          0
    ## 3   5   may       76        1    -1        0  unknown no          0
    ## 4   5   may       92        1    -1        0  unknown no          0
    ## 5   5   may      198        1    -1        0  unknown no          0
    ## 6   5   may      139        1    -1        0  unknown no          0

    summary(BANK)

    ##       age                 job           marital          education    
    ##  Min.   :18.00   blue-collar:9732   divorced: 5207   primary  : 6851  
    ##  1st Qu.:33.00   management :9458   married :27214   secondary:23202  
    ##  Median :39.00   technician :7597   single  :12790   tertiary :13301  
    ##  Mean   :40.94   admin.     :5171                    unknown  : 1857  
    ##  3rd Qu.:48.00   services   :4154                                     
    ##  Max.   :95.00   retired    :2264                                     
    ##                  (Other)    :6835                                     
    ##  default        balance       housing      loan            contact     
    ##  no :44396   Min.   : -8019   no :20081   no :37967   cellular :29285  
    ##  yes:  815   1st Qu.:    72   yes:25130   yes: 7244   telephone: 2906  
    ##              Median :   448                           unknown  :13020  
    ##              Mean   :  1362                                            
    ##              3rd Qu.:  1428                                            
    ##              Max.   :102127                                            
    ##                                                                        
    ##       day            month          duration         campaign    
    ##  20     : 2752   may    :13766   Min.   :   0.0   1      :17544  
    ##  18     : 2308   jul    : 6895   1st Qu.: 103.0   2      :12505  
    ##  21     : 2026   aug    : 6247   Median : 180.0   3      : 5521  
    ##  17     : 1939   jun    : 5341   Mean   : 258.2   4      : 3522  
    ##  6      : 1932   nov    : 3970   3rd Qu.: 319.0   5      : 1764  
    ##  5      : 1910   apr    : 2932   Max.   :4918.0   6      : 1291  
    ##  (Other):32344   (Other): 6060                    (Other): 3064  
    ##      pdays          previous           poutcome       y         IND_PREVIO
    ##  Min.   : -1.0   Min.   :  0.0000   failure: 4901   no :39922   0:36954   
    ##  1st Qu.: -1.0   1st Qu.:  0.0000   other  : 1840   yes: 5289   1: 8257   
    ##  Median : -1.0   Median :  0.0000   success: 1511                         
    ##  Mean   : 40.2   Mean   :  0.5803   unknown:36959                         
    ##  3rd Qu.: -1.0   3rd Qu.:  0.0000                                         
    ##  Max.   :871.0   Max.   :275.0000                                         
    ## 

Modelo de Regresión Logística
-----------------------------

    model_logit=glm(y~., data=BANK,family=binomial(link="logit"))
    summary(model_logit)

    ## 
    ## Call:
    ## glm(formula = y ~ ., family = binomial(link = "logit"), data = BANK)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -5.9323  -0.3700  -0.2447  -0.1474   3.4526  
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)        -2.897e+00  9.902e-01  -2.925 0.003442 ** 
    ## age                 1.697e-04  2.217e-03   0.077 0.938994    
    ## jobblue-collar     -2.767e-01  7.324e-02  -3.778 0.000158 ***
    ## jobentrepreneur    -3.337e-01  1.266e-01  -2.636 0.008398 ** 
    ## jobhousemaid       -4.948e-01  1.369e-01  -3.613 0.000303 ***
    ## jobmanagement      -1.461e-01  7.378e-02  -1.980 0.047655 *  
    ## jobretired          2.327e-01  9.821e-02   2.370 0.017799 *  
    ## jobself-employed   -2.925e-01  1.127e-01  -2.596 0.009445 ** 
    ## jobservices        -1.998e-01  8.461e-02  -2.361 0.018201 *  
    ## jobstudent          3.794e-01  1.099e-01   3.454 0.000553 ***
    ## jobtechnician      -1.519e-01  6.947e-02  -2.186 0.028806 *  
    ## jobunemployed      -1.795e-01  1.126e-01  -1.594 0.110947    
    ## jobunknown         -2.818e-01  2.358e-01  -1.195 0.232120    
    ## maritalmarried     -1.767e-01  5.936e-02  -2.976 0.002919 ** 
    ## maritalsingle       7.966e-02  6.779e-02   1.175 0.239965    
    ## educationsecondary  1.860e-01  6.524e-02   2.851 0.004362 ** 
    ## educationtertiary   3.747e-01  7.587e-02   4.939 7.83e-07 ***
    ## educationunknown    2.283e-01  1.051e-01   2.173 0.029766 *  
    ## defaultyes         -1.014e-02  1.632e-01  -0.062 0.950448    
    ## balance             1.154e-05  5.199e-06   2.219 0.026510 *  
    ## housingyes         -6.434e-01  4.432e-02 -14.516  < 2e-16 ***
    ## loanyes            -4.191e-01  6.039e-02  -6.940 3.93e-12 ***
    ## contacttelephone   -1.705e-01  7.570e-02  -2.253 0.024285 *  
    ## contactunknown     -1.584e+00  7.430e-02 -21.318  < 2e-16 ***
    ## day2               -1.536e-01  1.870e-01  -0.821 0.411586    
    ## day3                3.656e-02  1.887e-01   0.194 0.846354    
    ## day4                2.827e-02  1.823e-01   0.155 0.876753    
    ## day5               -1.918e-01  1.821e-01  -1.054 0.292036    
    ## day6               -1.854e-01  1.861e-01  -0.996 0.319123    
    ## day7               -3.237e-01  1.890e-01  -1.713 0.086768 .  
    ## day8                4.270e-02  1.835e-01   0.233 0.816056    
    ## day9                6.352e-02  1.888e-01   0.336 0.736564    
    ## day10               5.136e-01  2.074e-01   2.477 0.013259 *  
    ## day11              -1.374e-02  1.861e-01  -0.074 0.941157    
    ## day12               3.184e-01  1.822e-01   1.748 0.080547 .  
    ## day13               4.331e-01  1.832e-01   2.364 0.018100 *  
    ## day14               1.642e-01  1.842e-01   0.892 0.372566    
    ## day15               2.420e-01  1.823e-01   1.327 0.184476    
    ## day16               6.602e-02  1.862e-01   0.355 0.722867    
    ## day17              -5.570e-01  1.863e-01  -2.990 0.002788 ** 
    ## day18              -8.157e-02  1.815e-01  -0.450 0.653068    
    ## day19              -5.871e-01  1.989e-01  -2.951 0.003169 ** 
    ## day20              -4.347e-01  1.852e-01  -2.348 0.018889 *  
    ## day21              -4.649e-03  1.873e-01  -0.025 0.980196    
    ## day22               1.710e-01  1.948e-01   0.878 0.380183    
    ## day23               5.176e-01  2.021e-01   2.561 0.010446 *  
    ## day24               2.563e-02  2.333e-01   0.110 0.912496    
    ## day25               3.283e-01  1.983e-01   1.656 0.097783 .  
    ## day26               2.638e-01  2.017e-01   1.308 0.190813    
    ## day27               6.479e-01  1.950e-01   3.323 0.000891 ***
    ## day28               5.705e-02  1.970e-01   0.290 0.772112    
    ## day29              -1.264e-01  1.990e-01  -0.636 0.525073    
    ## day30               4.531e-01  1.830e-01   2.476 0.013296 *  
    ## day31               7.429e-02  2.534e-01   0.293 0.769351    
    ## monthaug           -7.381e-01  8.604e-02  -8.579  < 2e-16 ***
    ## monthdec            6.776e-01  1.804e-01   3.757 0.000172 ***
    ## monthfeb           -2.295e-01  9.722e-02  -2.361 0.018244 *  
    ## monthjan           -1.294e+00  1.328e-01  -9.744  < 2e-16 ***
    ## monthjul           -9.496e-01  8.384e-02 -11.326  < 2e-16 ***
    ## monthjun            4.070e-01  9.782e-02   4.160 3.18e-05 ***
    ## monthmar            1.476e+00  1.244e-01  11.867  < 2e-16 ***
    ## monthmay           -5.895e-01  8.068e-02  -7.307 2.72e-13 ***
    ## monthnov           -6.728e-01  9.378e-02  -7.175 7.25e-13 ***
    ## monthoct            7.763e-01  1.118e-01   6.940 3.91e-12 ***
    ## monthsep            7.602e-01  1.229e-01   6.186 6.16e-10 ***
    ## duration            4.249e-03  6.532e-05  65.051  < 2e-16 ***
    ## campaign2          -3.464e-01  4.474e-02  -7.743 9.72e-15 ***
    ## campaign3          -2.707e-01  6.017e-02  -4.499 6.83e-06 ***
    ## campaign4          -4.846e-01  7.878e-02  -6.151 7.69e-10 ***
    ## campaign5          -5.887e-01  1.124e-01  -5.237 1.63e-07 ***
    ## campaign6          -6.608e-01  1.350e-01  -4.895 9.81e-07 ***
    ## campaign7          -7.511e-01  1.840e-01  -4.083 4.45e-05 ***
    ## campaign8          -6.394e-01  2.088e-01  -3.062 0.002197 ** 
    ## campaign9          -7.289e-01  2.782e-01  -2.620 0.008795 ** 
    ## campaign10         -6.882e-01  3.317e-01  -2.075 0.038030 *  
    ## campaign11         -2.000e-01  3.102e-01  -0.645 0.519114    
    ## campaign12         -1.579e+00  5.503e-01  -2.869 0.004117 ** 
    ## campaign13         -1.107e+00  4.865e-01  -2.275 0.022905 *  
    ## campaign14         -7.318e-01  5.706e-01  -1.283 0.199652    
    ## campaign15         -3.909e-01  5.529e-01  -0.707 0.479517    
    ## campaign16         -1.590e+00  9.686e-01  -1.642 0.100589    
    ## campaign17         -2.482e-01  5.282e-01  -0.470 0.638437    
    ## campaign18         -1.251e+01  1.848e+02  -0.068 0.946028    
    ## campaign19         -1.275e+01  2.045e+02  -0.062 0.950297    
    ## campaign20         -1.126e+00  1.090e+00  -1.033 0.301830    
    ## campaign21         -5.108e-01  1.040e+00  -0.491 0.623358    
    ## campaign22         -1.235e+01  2.838e+02  -0.044 0.965291    
    ## campaign23         -1.250e+01  2.902e+02  -0.043 0.965647    
    ## campaign24         -1.361e+00  1.316e+00  -1.035 0.300902    
    ## campaign25         -1.261e+01  2.822e+02  -0.045 0.964351    
    ## campaign26         -1.181e+01  3.866e+02  -0.031 0.975638    
    ## campaign27         -1.210e+01  4.443e+02  -0.027 0.978268    
    ## campaign28         -1.262e+01  3.306e+02  -0.038 0.969549    
    ## campaign29          1.733e-01  1.268e+00   0.137 0.891307    
    ## campaign30         -1.195e+01  4.961e+02  -0.024 0.980774    
    ## campaign31         -1.236e+01  3.969e+02  -0.031 0.975157    
    ## campaign32          1.750e+00  1.092e+00   1.602 0.109187    
    ## campaign33         -1.177e+01  5.844e+02  -0.020 0.983929    
    ## campaign34         -1.245e+01  6.391e+02  -0.019 0.984461    
    ## campaign35         -1.223e+01  7.156e+02  -0.017 0.986359    
    ## campaign36         -1.217e+01  7.213e+02  -0.017 0.986538    
    ## campaign37         -1.109e+01  1.027e+03  -0.011 0.991383    
    ## campaign38         -1.161e+01  8.270e+02  -0.014 0.988803    
    ## campaign39         -1.178e+01  1.455e+03  -0.008 0.993540    
    ## campaign41         -1.119e+01  9.548e+02  -0.012 0.990649    
    ## campaign43         -1.078e+01  8.198e+02  -0.013 0.989513    
    ## campaign44         -1.122e+01  1.455e+03  -0.008 0.993848    
    ## campaign46         -1.178e+01  1.455e+03  -0.008 0.993542    
    ## campaign50         -1.117e+01  1.027e+03  -0.011 0.991318    
    ## campaign51         -1.039e+01  1.455e+03  -0.007 0.994303    
    ## campaign55         -1.480e+01  1.455e+03  -0.010 0.991886    
    ## campaign58         -1.083e+01  1.455e+03  -0.007 0.994065    
    ## campaign63         -1.025e+01  1.455e+03  -0.007 0.994383    
    ## pdays               8.241e-05  3.072e-04   0.268 0.788535    
    ## previous            1.164e-02  6.733e-03   1.728 0.083903 .  
    ## poutcomeother       2.162e-01  9.060e-02   2.387 0.016995 *  
    ## poutcomesuccess     2.220e+00  8.293e-02  26.771  < 2e-16 ***
    ## poutcomeunknown     4.351e-01  9.656e-01   0.451 0.652304    
    ## IND_PREVIO1         4.669e-01  9.683e-01   0.482 0.629678    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 32631  on 45210  degrees of freedom
    ## Residual deviance: 21269  on 45092  degrees of freedom
    ## AIC: 21507
    ## 
    ## Number of Fisher Scoring iterations: 14

Nos da un valor mucho mas pequeño de AIC 21507

    model_logit1=glm(y~job+marital+education+default+balance+housing+loan+contact+month+poutcome, data=BANK,family=binomial(link="logit"))
    summary(model_logit1)

    ## 
    ## Call:
    ## glm(formula = y ~ job + marital + education + default + balance + 
    ##     housing + loan + contact + month + poutcome, family = binomial(link = "logit"), 
    ##     data = BANK)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.3429  -0.4762  -0.3823  -0.2487   2.9771  
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)        -1.268e+00  1.068e-01 -11.872  < 2e-16 ***
    ## jobblue-collar     -1.253e-01  6.490e-02  -1.931 0.053471 .  
    ## jobentrepreneur    -1.967e-01  1.112e-01  -1.769 0.076953 .  
    ## jobhousemaid       -2.849e-01  1.197e-01  -2.381 0.017270 *  
    ## jobmanagement      -5.305e-02  6.566e-02  -0.808 0.419082    
    ## jobretired          4.538e-01  7.798e-02   5.820 5.90e-09 ***
    ## jobself-employed   -9.552e-02  9.917e-02  -0.963 0.335483    
    ## jobservices        -8.396e-02  7.450e-02  -1.127 0.259714    
    ## jobstudent          3.309e-01  9.773e-02   3.386 0.000710 ***
    ## jobtechnician      -6.585e-02  6.188e-02  -1.064 0.287233    
    ## jobunemployed       1.262e-01  9.770e-02   1.292 0.196280    
    ## jobunknown         -1.978e-01  2.077e-01  -0.952 0.341032    
    ## maritalmarried     -2.106e-01  5.161e-02  -4.081 4.49e-05 ***
    ## maritalsingle       8.151e-02  5.556e-02   1.467 0.142387    
    ## educationsecondary  1.508e-01  5.654e-02   2.667 0.007659 ** 
    ## educationtertiary   3.116e-01  6.575e-02   4.740 2.14e-06 ***
    ## educationunknown    1.990e-01  9.244e-02   2.153 0.031319 *  
    ## defaultyes         -1.329e-01  1.470e-01  -0.904 0.366006    
    ## balance             1.703e-05  4.463e-06   3.816 0.000136 ***
    ## housingyes         -5.398e-01  3.813e-02 -14.157  < 2e-16 ***
    ## loanyes            -3.969e-01  5.312e-02  -7.472 7.91e-14 ***
    ## contacttelephone   -2.883e-01  6.408e-02  -4.499 6.83e-06 ***
    ## contactunknown     -1.346e+00  6.339e-02 -21.228  < 2e-16 ***
    ## monthaug           -9.711e-01  6.848e-02 -14.181  < 2e-16 ***
    ## monthdec            5.651e-01  1.621e-01   3.485 0.000491 ***
    ## monthfeb           -4.419e-01  7.506e-02  -5.887 3.93e-09 ***
    ## monthjan           -1.071e+00  1.063e-01 -10.077  < 2e-16 ***
    ## monthjul           -7.875e-01  6.773e-02 -11.627  < 2e-16 ***
    ## monthjun            1.052e-01  8.094e-02   1.300 0.193738    
    ## monthmar            1.063e+00  1.103e-01   9.639  < 2e-16 ***
    ## monthmay           -5.021e-01  6.345e-02  -7.913 2.52e-15 ***
    ## monthnov           -8.507e-01  7.457e-02 -11.409  < 2e-16 ***
    ## monthoct            6.755e-01  9.781e-02   6.907 4.96e-12 ***
    ## monthsep            6.544e-01  1.075e-01   6.089 1.13e-09 ***
    ## poutcomeother       2.516e-01  7.967e-02   3.159 0.001586 ** 
    ## poutcomesuccess     2.264e+00  7.346e-02  30.813  < 2e-16 ***
    ## poutcomeunknown     3.486e-02  5.156e-02   0.676 0.498980    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 32631  on 45210  degrees of freedom
    ## Residual deviance: 27282  on 45174  degrees of freedom
    ## AIC: 27356
    ## 
    ## Number of Fisher Scoring iterations: 6

Regresion logistica, es un modelo lineal igual que antes el coef de
casados es -2.1, si hacemos e^-0.21 = 0.81, los casados tiene 1/0.81 =
1.23 tienen 1.23 veces mas de contratar los casados. Casado es igual a
divorciado, soltero no es igual a divorciado hay que tener cuidado con
el caso base, normalmente se pone el que tenga más registros. Cuando hay
variables categoricas jugamos a cambiar el orden. las variables se estan
comparando con una. Si cogemos como caso base el que tenga el coef mas
pequeño conseguiremos que todos los coef sean positivos (ES LO MEJOR),
se hace un relevel,ganas en interpretabilidad de los modelos. Intento no
quitar variables porque luego puedo tener mas datos y a lo mejor tiene
más importancia. Si no se como agruparlos los dejo separados. Aquí no
hay R^2, maximizamos la verosimilitud, te compara el modelo Null. AIC
cuanto más pequeño mejor.

    model_probit1=glm(y~job+marital+education+default+balance+housing+loan+contact+month+poutcome, data=BANK,family=binomial(link="probit"))
    summary(model_probit1)

    ## 
    ## Call:
    ## glm(formula = y ~ job + marital + education + default + balance + 
    ##     housing + loan + contact + month + poutcome, family = binomial(link = "probit"), 
    ##     data = BANK)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.3690  -0.4792  -0.3836  -0.2513   3.0684  
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)        -7.929e-01  5.657e-02 -14.017  < 2e-16 ***
    ## jobblue-collar     -5.775e-02  3.325e-02  -1.737 0.082405 .  
    ## jobentrepreneur    -9.856e-02  5.625e-02  -1.752 0.079729 .  
    ## jobhousemaid       -1.537e-01  6.107e-02  -2.517 0.011826 *  
    ## jobmanagement      -3.585e-02  3.478e-02  -1.031 0.302638    
    ## jobretired          2.496e-01  4.240e-02   5.887 3.92e-09 ***
    ## jobself-employed   -5.269e-02  5.190e-02  -1.015 0.310009    
    ## jobservices        -4.880e-02  3.831e-02  -1.274 0.202662    
    ## jobstudent          1.986e-01  5.490e-02   3.617 0.000298 ***
    ## jobtechnician      -3.917e-02  3.241e-02  -1.209 0.226850    
    ## jobunemployed       6.648e-02  5.244e-02   1.268 0.204910    
    ## jobunknown         -1.213e-01  1.091e-01  -1.111 0.266409    
    ## maritalmarried     -1.136e-01  2.693e-02  -4.220 2.44e-05 ***
    ## maritalsingle       4.783e-02  2.914e-02   1.641 0.100742    
    ## educationsecondary  7.190e-02  2.869e-02   2.506 0.012215 *  
    ## educationtertiary   1.613e-01  3.406e-02   4.735 2.19e-06 ***
    ## educationunknown    9.669e-02  4.843e-02   1.996 0.045885 *  
    ## defaultyes         -6.124e-02  7.134e-02  -0.858 0.390676    
    ## balance             1.047e-05  2.456e-06   4.265 2.00e-05 ***
    ## housingyes         -2.705e-01  1.984e-02 -13.636  < 2e-16 ***
    ## loanyes            -1.943e-01  2.618e-02  -7.422 1.15e-13 ***
    ## contacttelephone   -1.551e-01  3.413e-02  -4.545 5.50e-06 ***
    ## contactunknown     -6.339e-01  3.094e-02 -20.486  < 2e-16 ***
    ## monthaug           -5.082e-01  3.736e-02 -13.601  < 2e-16 ***
    ## monthdec            3.585e-01  9.630e-02   3.723 0.000197 ***
    ## monthfeb           -2.301e-01  4.153e-02  -5.541 3.00e-08 ***
    ## monthjan           -5.711e-01  5.601e-02 -10.195  < 2e-16 ***
    ## monthjul           -4.115e-01  3.651e-02 -11.271  < 2e-16 ***
    ## monthjun            1.445e-02  4.374e-02   0.330 0.741171    
    ## monthmar            6.623e-01  6.634e-02   9.984  < 2e-16 ***
    ## monthmay           -2.585e-01  3.454e-02  -7.484 7.23e-14 ***
    ## monthnov           -4.455e-01  3.991e-02 -11.162  < 2e-16 ***
    ## monthoct            4.316e-01  5.745e-02   7.512 5.82e-14 ***
    ## monthsep            4.147e-01  6.327e-02   6.553 5.62e-11 ***
    ## poutcomeother       1.377e-01  4.343e-02   3.171 0.001517 ** 
    ## poutcomesuccess     1.334e+00  4.225e-02  31.574  < 2e-16 ***
    ## poutcomeunknown     1.695e-02  2.756e-02   0.615 0.538435    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 32631  on 45210  degrees of freedom
    ## Residual deviance: 27309  on 45174  degrees of freedom
    ## AIC: 27383
    ## 
    ## Number of Fisher Scoring iterations: 6

el efecto es un poco diferente, lo que si que es igual es el signo. Es
un poco mas pequeño el AIC del logit

Diferencia entre el logit y el probit
-------------------------------------

    X=seq(from=-4,to=4,by=0.1)
    sigmoide=1/(1+exp(-X))
    cumulative<-pnorm(X, 0, 1)
    plot(sigmoide,type="l",col="red")
    lines(cumulative,col="blue")

![](RegLinLogis_files/figure-markdown_strict/unnamed-chunk-49-1.png) la
roja es la sigmoide, es menos rigida. la azul se usa cuando quieres muy
fiables y muy sensibles(medicina)

Evaluación del Modelo
---------------------

Me quedo con el modelo logit1 probablemente tendría que quitar
variables, como evaluo? tengo un monton de 0 y 1, mi modelo me devuelve
probabilidades.

    BANK$prediccion=predict(model_logit1,type="response")
    head(BANK$prediccion)

    ## [1] 0.02831273 0.03067238 0.01374012 0.02339745 0.04753360 0.02743051

me puede dar el resultado despues de aplicar el sigmoide

    Pred_auxiliar= prediction(BANK$prediccion, BANK$y, label.ordering = NULL)
    auc.tmp = performance(Pred_auxiliar, "auc");
    auc_model_logit1_train = as.numeric(auc.tmp@y.values)
    auc_model_logit1_train #para medir la capacidad predictiva del modelo

    ## [1] 0.7641777

    CURVA_ROC_model_logit1_train <- performance(Pred_auxiliar,"tpr","fpr")

    plot(CURVA_ROC_model_logit1_train,colorize=TRUE)
    abline(a=0,b=1)
    abline(v=0.5)

![](RegLinLogis_files/figure-markdown_strict/unnamed-chunk-53-1.png)
cuando corte en la curva estoy capturando el 0.5 de los positivos y el
0.1 de los negativos

Capacidad del Modelo
--------------------

    mean(as.numeric(BANK$y)-1)

    ## [1] 0.1169848

Restamos 1, porque al pasar a numerico me pone 2 y 1, el 11% contratan
de cada 100 que coja. \`\`\`

    aggregate(BANK$prediccion~BANK$y,FUN=mean)

    ##   BANK$y BANK$prediccion
    ## 1     no      0.09752806
    ## 2    yes      0.26384662

comparo las predicciones con respecto al modelo, a los que si han
contratado mi modelo le da una probabilidad de 26%, le está dando más
probabilidad a los que han contratado.

Puesta en valor de un modelo: Fijación del Threshold
----------------------------------------------------

    ALPHA=0.5
    Confusion=table(BANK$y,BANK$prediccion>=ALPHA)
    Confusion

    ##      
    ##       FALSE  TRUE
    ##   no  39398   524
    ##   yes  4338   951

BANK*p**r**e**d**i**c**c**i**o**n**s**o**n**l**a**s**p**r**o**b**a**b**i**l**i**d**a**d**e**s**d**e**q**u**e**s**e**a**n**m**a**y**o**r**q**u**e*0.5*B**A**N**K*y
tiene 5289 positivos, y 39922 negativos. En alpha=0.5, hay 951 True
positive and 524 false positive

    Accuracy= (sum(BANK$y=="yes" & BANK$prediccion>=ALPHA)+sum(BANK$y=="no" & BANK$prediccion<ALPHA))/length(BANK$y)
    Accuracy

    ## [1] 0.8924598

TruePositive+True Negative/TOTALobs, es el porcentaje de aciertos,
aciertas un 89%

### Precisión

Precision is the number of True Positives divided by the number of True
Positives and False Positives. Put another way, it is the number of
positive predictions divided by the total number of positive class
values predicted. It is also called the Positive Predictive Value (PPV).

    Precision=sum(BANK$y=="yes" & BANK$prediccion>=ALPHA)/sum(BANK$y=="yes")
    Precision

    ## [1] 0.1798071

Recall is the number of True Positives divided by the number of True
Positives and the number of False Negatives. Put another way it is the
number of positive predictions divided by the number of positive class
values in the test data. It is also called Sensitivity or the True
Positive Rate. Recall 951/(951+524)

    recall=sum(BANK$y=="yes" & BANK$prediccion>=ALPHA)/sum(BANK$prediccion>=ALPHA)
    recall

    ## [1] 0.6447458

La cobertura la han calculado como la precision:

    Cobertura=sum(BANK$y=="yes" & BANK$prediccion>=ALPHA)/sum(BANK$y=="yes")
    Cobertura

    ## [1] 0.1798071

951/5289, Truepositive/suma todos los positivos BANK$y

Modificación de ALPHA
---------------------

Si bajamos alpha, la precision es peor, la cobertura es mayor. Tengo más
errores en la tabla de confusión.

Criterio maximizar F1-Score
---------------------------

En estadística análisis de clasificación binaria , la F 1 puntuación
(también F-Resultado o F-medida ) es una medida de la exactitud de una
prueba. Se considera tanto la precisión p y la retirada r de la prueba
para calcular la puntuación: p es el número de resultados positivos
correctos dividido por el número de todos los resultados positivos, y r
es el número de resultados positivos correctos dividido por el número de
positivos resultados que deberían haber sido devueltos. El F 1
puntuación puede ser interpretado como un promedio ponderado de la
precisión y la recuperación , en donde un F 1 puntuación alcanza su
mejor valor en 1 y lo peor a 0.

    Precisionf1 <- Precision
    Precisionf1

    ## [1] 0.1798071

    Recallf1 <- recall
    Recallf1

    ## [1] 0.6447458

    F=2*((Precisionf1*Recallf1)/(Precisionf1+Recallf1))
    F

    ## [1] 0.2811946

Índice Fowlkes-Malvas
---------------------

Índice Fowlkes-Malvas \[1\] es una evaluación externa método que se
utiliza para determinar la similitud entre dos agrupamientos (clusters
obtenidos después de un algoritmo de agrupamiento). Esta medida de
similitud podría ser o bien entre dos agrupamientos jerárquicos o de una
agrupación, a la nomenclatura de referencia. Un valor más alto para el
índice Fowlkes-Malvas indica una mayor similitud entre los clusters y
las clasificaciones de referencia.

    FM=sqrt(Precisionf1*Recallf1)
    FM

    ## [1] 0.3404848

MODELOS LINIALES GENERALIZADOS: MODELO POISSON
==============================================

Carga de Datos
--------------

    BICIS=read.csv("hour.csv")

\*datos extraidos de
<https://archive.ics.uci.edu/ml/datasets/Bike+Sharing+Dataset*>

Revisión básica dataset
-----------------------

    str(BICIS)

    ## 'data.frame':    17379 obs. of  17 variables:
    ##  $ instant   : int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ dteday    : Factor w/ 731 levels "2011-01-01","2011-01-02",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ season    : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ yr        : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ mnth      : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ hr        : int  0 1 2 3 4 5 6 7 8 9 ...
    ##  $ holiday   : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ weekday   : int  6 6 6 6 6 6 6 6 6 6 ...
    ##  $ workingday: int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ weathersit: int  1 1 1 1 1 2 1 1 1 1 ...
    ##  $ temp      : num  0.24 0.22 0.22 0.24 0.24 0.24 0.22 0.2 0.24 0.32 ...
    ##  $ atemp     : num  0.288 0.273 0.273 0.288 0.288 ...
    ##  $ hum       : num  0.81 0.8 0.8 0.75 0.75 0.75 0.8 0.86 0.75 0.76 ...
    ##  $ windspeed : num  0 0 0 0 0 0.0896 0 0 0 0 ...
    ##  $ casual    : int  3 8 5 3 0 0 2 1 1 8 ...
    ##  $ registered: int  13 32 27 10 1 1 0 2 7 6 ...
    ##  $ cnt       : int  16 40 32 13 1 1 2 3 8 14 ...

    head(BICIS)

    ##   instant     dteday season yr mnth hr holiday weekday workingday
    ## 1       1 2011-01-01      1  0    1  0       0       6          0
    ## 2       2 2011-01-01      1  0    1  1       0       6          0
    ## 3       3 2011-01-01      1  0    1  2       0       6          0
    ## 4       4 2011-01-01      1  0    1  3       0       6          0
    ## 5       5 2011-01-01      1  0    1  4       0       6          0
    ## 6       6 2011-01-01      1  0    1  5       0       6          0
    ##   weathersit temp  atemp  hum windspeed casual registered cnt
    ## 1          1 0.24 0.2879 0.81    0.0000      3         13  16
    ## 2          1 0.22 0.2727 0.80    0.0000      8         32  40
    ## 3          1 0.22 0.2727 0.80    0.0000      5         27  32
    ## 4          1 0.24 0.2879 0.75    0.0000      3         10  13
    ## 5          1 0.24 0.2879 0.75    0.0000      0          1   1
    ## 6          2 0.24 0.2576 0.75    0.0896      0          1   1

    summary(BICIS)

    ##     instant             dteday          season            yr        
    ##  Min.   :    1   2011-01-01:   24   Min.   :1.000   Min.   :0.0000  
    ##  1st Qu.: 4346   2011-01-08:   24   1st Qu.:2.000   1st Qu.:0.0000  
    ##  Median : 8690   2011-01-09:   24   Median :3.000   Median :1.0000  
    ##  Mean   : 8690   2011-01-10:   24   Mean   :2.502   Mean   :0.5026  
    ##  3rd Qu.:13034   2011-01-13:   24   3rd Qu.:3.000   3rd Qu.:1.0000  
    ##  Max.   :17379   2011-01-15:   24   Max.   :4.000   Max.   :1.0000  
    ##                  (Other)   :17235                                   
    ##       mnth              hr           holiday           weekday     
    ##  Min.   : 1.000   Min.   : 0.00   Min.   :0.00000   Min.   :0.000  
    ##  1st Qu.: 4.000   1st Qu.: 6.00   1st Qu.:0.00000   1st Qu.:1.000  
    ##  Median : 7.000   Median :12.00   Median :0.00000   Median :3.000  
    ##  Mean   : 6.538   Mean   :11.55   Mean   :0.02877   Mean   :3.004  
    ##  3rd Qu.:10.000   3rd Qu.:18.00   3rd Qu.:0.00000   3rd Qu.:5.000  
    ##  Max.   :12.000   Max.   :23.00   Max.   :1.00000   Max.   :6.000  
    ##                                                                    
    ##    workingday       weathersit         temp           atemp       
    ##  Min.   :0.0000   Min.   :1.000   Min.   :0.020   Min.   :0.0000  
    ##  1st Qu.:0.0000   1st Qu.:1.000   1st Qu.:0.340   1st Qu.:0.3333  
    ##  Median :1.0000   Median :1.000   Median :0.500   Median :0.4848  
    ##  Mean   :0.6827   Mean   :1.425   Mean   :0.497   Mean   :0.4758  
    ##  3rd Qu.:1.0000   3rd Qu.:2.000   3rd Qu.:0.660   3rd Qu.:0.6212  
    ##  Max.   :1.0000   Max.   :4.000   Max.   :1.000   Max.   :1.0000  
    ##                                                                   
    ##       hum           windspeed          casual         registered   
    ##  Min.   :0.0000   Min.   :0.0000   Min.   :  0.00   Min.   :  0.0  
    ##  1st Qu.:0.4800   1st Qu.:0.1045   1st Qu.:  4.00   1st Qu.: 34.0  
    ##  Median :0.6300   Median :0.1940   Median : 17.00   Median :115.0  
    ##  Mean   :0.6272   Mean   :0.1901   Mean   : 35.68   Mean   :153.8  
    ##  3rd Qu.:0.7800   3rd Qu.:0.2537   3rd Qu.: 48.00   3rd Qu.:220.0  
    ##  Max.   :1.0000   Max.   :0.8507   Max.   :367.00   Max.   :886.0  
    ##                                                                    
    ##       cnt       
    ##  Min.   :  1.0  
    ##  1st Qu.: 40.0  
    ##  Median :142.0  
    ##  Mean   :189.5  
    ##  3rd Qu.:281.0  
    ##  Max.   :977.0  
    ## 

Modelos Regresión de Poisson
----------------------------

    hist(BICIS$cnt)

![](RegLinLogis_files/figure-markdown_strict/unnamed-chunk-66-1.png)

    mean(BICIS$cnt)

    ## [1] 189.4631

    sd(BICIS$cnt)

    ## [1] 181.3876

Modelo Poisson quitando las variables instant, dteday, casual y
registered

    model_poisson=glm(cnt~.-instant-dteday-casual-registered, family=poisson(link = "log"),data=BICIS) #a la funcion lineal se le aplica la exponencial,que transforma los valores de 0 a positivo, lo transforma a unos valores positivos
    summary(model_poisson)

    ## 
    ## Call:
    ## glm(formula = cnt ~ . - instant - dteday - casual - registered, 
    ##     family = poisson(link = "log"), data = BICIS)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -30.221   -8.748   -3.022    3.962   38.708  
    ## 
    ## Coefficients:
    ##               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)  3.734e+00  3.953e-03  944.69   <2e-16 ***
    ## season       1.165e-01  1.098e-03  106.12   <2e-16 ***
    ## yr           4.393e-01  1.143e-03  384.37   <2e-16 ***
    ## mnth         7.090e-03  3.580e-04   19.80   <2e-16 ***
    ## hr           4.571e-02  9.071e-05  503.86   <2e-16 ***
    ## holiday     -1.319e-01  3.738e-03  -35.29   <2e-16 ***
    ## weekday      7.712e-03  2.796e-04   27.58   <2e-16 ***
    ## workingday   2.123e-02  1.235e-03   17.18   <2e-16 ***
    ## weathersit  -2.112e-02  1.051e-03  -20.10   <2e-16 ***
    ## temp         4.289e-02  1.802e-02    2.38   0.0173 *  
    ## atemp        1.651e+00  2.042e-02   80.87   <2e-16 ***
    ## hum         -1.018e+00  3.557e-03 -286.19   <2e-16 ***
    ## windspeed    3.070e-01  4.786e-03   64.15   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for poisson family taken to be 1)
    ## 
    ##     Null deviance: 2891591  on 17378  degrees of freedom
    ## Residual deviance: 1685957  on 17366  degrees of freedom
    ## AIC: 1796884
    ## 
    ## Number of Fisher Scoring iterations: 5

    model_poisson

    ## 
    ## Call:  glm(formula = cnt ~ . - instant - dteday - casual - registered, 
    ##     family = poisson(link = "log"), data = BICIS)
    ## 
    ## Coefficients:
    ## (Intercept)       season           yr         mnth           hr  
    ##    3.734459     0.116493     0.439322     0.007090     0.045705  
    ##     holiday      weekday   workingday   weathersit         temp  
    ##   -0.131925     0.007712     0.021229    -0.021123     0.042889  
    ##       atemp          hum    windspeed  
    ##    1.651171    -1.018081     0.306994  
    ## 
    ## Degrees of Freedom: 17378 Total (i.e. Null);  17366 Residual
    ## Null Deviance:       2892000 
    ## Residual Deviance: 1686000   AIC: 1797000

    plot(model_poisson)

![](RegLinLogis_files/figure-markdown_strict/unnamed-chunk-67-1.png)![](RegLinLogis_files/figure-markdown_strict/unnamed-chunk-67-2.png)![](RegLinLogis_files/figure-markdown_strict/unnamed-chunk-67-3.png)![](RegLinLogis_files/figure-markdown_strict/unnamed-chunk-67-4.png)

Todas las variables son significativas menos temp.

    BICIS$prediccion=predict(model_poisson,type="response") #prediccion valores con el modelo de poisson
    head(BICIS)

    ##   instant     dteday season yr mnth hr holiday weekday workingday
    ## 1       1 2011-01-01      1  0    1  0       0       6          0
    ## 2       2 2011-01-01      1  0    1  1       0       6          0
    ## 3       3 2011-01-01      1  0    1  2       0       6          0
    ## 4       4 2011-01-01      1  0    1  3       0       6          0
    ## 5       5 2011-01-01      1  0    1  4       0       6          0
    ## 6       6 2011-01-01      1  0    1  5       0       6          0
    ##   weathersit temp  atemp  hum windspeed casual registered cnt prediccion
    ## 1          1 0.24 0.2879 0.81    0.0000      3         13  16   34.61232
    ## 2          1 0.22 0.2727 0.80    0.0000      8         32  40   35.66395
    ## 3          1 0.22 0.2727 0.80    0.0000      5         27  32   37.33181
    ## 4          1 0.24 0.2879 0.75    0.0000      3         10  13   42.19960
    ## 5          1 0.24 0.2879 0.75    0.0000      0          1   1   44.17310
    ## 6          2 0.24 0.2576 0.75    0.0896      0          1   1   44.26412

    SCE=sum((BICIS$cnt-BICIS$prediccion)^2) #suma cuadrado de los errores
    STC=sum((BICIS$cnt-mean(BICIS$cnt))^2) 
    R2=1-(SCE/STC)
    R2

    ## [1] 0.3793564

Interpretación del resultado: El 37.94% de la varianza de cnt está
explicada por las variables de nuestro modelo, la varianza residual es
de 62,06%.

R2 = Varianza Explicada / Total Varianza

-   possibility 1 R2 &lt;- cor(y,predict(mod))^2

-   possibility 2 R2 &lt;- 1 -
    (sum((y-predict(mod))<sup>2)/sum((y-mean(y))</sup>2))

Formateo variables
------------------

    BICIS=read.csv("hour.csv")

    BICIS$season=as.factor(BICIS$season)
    BICIS$yr=as.factor(BICIS$yr)
    BICIS$mnth=as.factor(BICIS$mnth)
    BICIS$hr=as.factor(BICIS$hr)
    BICIS$holiday=as.factor(BICIS$holiday)
    BICIS$weekday=as.factor(BICIS$weekday)
    BICIS$workingday=as.factor(BICIS$workingday)
    BICIS$weathersit=as.factor(BICIS$weathersit)

    model_poisson=glm(cnt~.-instant-dteday-casual-registered, family=poisson(link = "log"),data=BICIS)
    summary(model_poisson)

    ## 
    ## Call:
    ## glm(formula = cnt ~ . - instant - dteday - casual - registered, 
    ##     family = poisson(link = "log"), data = BICIS)
    ## 
    ## Deviance Residuals: 
    ##      Min        1Q    Median        3Q       Max  
    ## -24.9615   -3.7666   -0.8567    3.0347   22.3749  
    ## 
    ## Coefficients: (1 not defined because of singularities)
    ##              Estimate Std. Error  z value Pr(>|z|)    
    ## (Intercept)  2.917336   0.006790  429.640  < 2e-16 ***
    ## season2      0.274076   0.003680   74.487  < 2e-16 ***
    ## season3      0.267228   0.004211   63.457  < 2e-16 ***
    ## season4      0.457991   0.004081  112.212  < 2e-16 ***
    ## yr1          0.468566   0.001151  407.080  < 2e-16 ***
    ## mnth2        0.113516   0.003776   30.060  < 2e-16 ***
    ## mnth3        0.223665   0.003937   56.817  < 2e-16 ***
    ## mnth4        0.181289   0.005235   34.629  < 2e-16 ***
    ## mnth5        0.244655   0.005477   44.668  < 2e-16 ***
    ## mnth6        0.196362   0.005585   35.158  < 2e-16 ***
    ## mnth7        0.098808   0.006064   16.294  < 2e-16 ***
    ## mnth8        0.195102   0.005898   33.078  < 2e-16 ***
    ## mnth9        0.270869   0.005427   49.914  < 2e-16 ***
    ## mnth10       0.187711   0.005395   34.794  < 2e-16 ***
    ## mnth11       0.061117   0.005303   11.524  < 2e-16 ***
    ## mnth12       0.045360   0.004676    9.700  < 2e-16 ***
    ## hr1         -0.466697   0.008182  -57.038  < 2e-16 ***
    ## hr2         -0.839682   0.009313  -90.160  < 2e-16 ***
    ## hr3         -1.507858   0.012163 -123.967  < 2e-16 ***
    ## hr4         -2.110448   0.015858 -133.084  < 2e-16 ***
    ## hr5         -0.956562   0.009787  -97.738  < 2e-16 ***
    ## hr6          0.400501   0.006619   60.509  < 2e-16 ***
    ## hr7          1.422874   0.005666  251.117  < 2e-16 ***
    ## hr8          1.916567   0.005423  353.411  < 2e-16 ***
    ## hr9          1.391883   0.005648  246.429  < 2e-16 ***
    ## hr10         1.123194   0.005806  193.438  < 2e-16 ***
    ## hr11         1.269596   0.005717  222.071  < 2e-16 ***
    ## hr12         1.447484   0.005642  256.544  < 2e-16 ***
    ## hr13         1.427091   0.005663  251.991  < 2e-16 ***
    ## hr14         1.364773   0.005707  239.121  < 2e-16 ***
    ## hr15         1.405023   0.005693  246.794  < 2e-16 ***
    ## hr16         1.628120   0.005593  291.123  < 2e-16 ***
    ## hr17         2.036233   0.005445  373.972  < 2e-16 ***
    ## hr18         1.970299   0.005443  362.018  < 2e-16 ***
    ## hr19         1.674864   0.005518  303.540  < 2e-16 ***
    ## hr20         1.377556   0.005648  243.882  < 2e-16 ***
    ## hr21         1.121995   0.005800  193.434  < 2e-16 ***
    ## hr22         0.864956   0.006005  144.039  < 2e-16 ***
    ## hr23         0.483910   0.006419   75.382  < 2e-16 ***
    ## holiday1    -0.160979   0.003797  -42.398  < 2e-16 ***
    ## weekday1     0.051206   0.002167   23.626  < 2e-16 ***
    ## weekday2     0.060927   0.002103   28.971  < 2e-16 ***
    ## weekday3     0.066408   0.002103   31.582  < 2e-16 ***
    ## weekday4     0.067338   0.002089   32.228  < 2e-16 ***
    ## weekday5     0.093581   0.002089   44.798  < 2e-16 ***
    ## weekday6     0.079608   0.002091   38.063  < 2e-16 ***
    ## workingday1        NA         NA       NA       NA    
    ## weathersit2 -0.064256   0.001422  -45.175  < 2e-16 ***
    ## weathersit3 -0.492963   0.002864 -172.124  < 2e-16 ***
    ## weathersit4 -0.469206   0.067067   -6.996 2.63e-12 ***
    ## temp         0.164396   0.019469    8.444  < 2e-16 ***
    ## atemp        0.946850   0.020326   46.584  < 2e-16 ***
    ## hum         -0.205716   0.004129  -49.824  < 2e-16 ***
    ## windspeed   -0.109958   0.004869  -22.581  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for poisson family taken to be 1)
    ## 
    ##     Null deviance: 2891591  on 17378  degrees of freedom
    ## Residual deviance:  572011  on 17326  degrees of freedom
    ## AIC: 683018
    ## 
    ## Number of Fisher Scoring iterations: 5

    model_poisson=glm(cnt~.-workingday-instant-dteday-casual-registered, family=poisson(link = "log"),data=BICIS)
    summary(model_poisson)

    ## 
    ## Call:
    ## glm(formula = cnt ~ . - workingday - instant - dteday - casual - 
    ##     registered, family = poisson(link = "log"), data = BICIS)
    ## 
    ## Deviance Residuals: 
    ##      Min        1Q    Median        3Q       Max  
    ## -24.9615   -3.7666   -0.8567    3.0347   22.3749  
    ## 
    ## Coefficients:
    ##              Estimate Std. Error  z value Pr(>|z|)    
    ## (Intercept)  2.917336   0.006790  429.640  < 2e-16 ***
    ## season2      0.274076   0.003680   74.487  < 2e-16 ***
    ## season3      0.267228   0.004211   63.457  < 2e-16 ***
    ## season4      0.457991   0.004081  112.212  < 2e-16 ***
    ## yr1          0.468566   0.001151  407.080  < 2e-16 ***
    ## mnth2        0.113516   0.003776   30.060  < 2e-16 ***
    ## mnth3        0.223665   0.003937   56.817  < 2e-16 ***
    ## mnth4        0.181289   0.005235   34.629  < 2e-16 ***
    ## mnth5        0.244655   0.005477   44.668  < 2e-16 ***
    ## mnth6        0.196362   0.005585   35.158  < 2e-16 ***
    ## mnth7        0.098808   0.006064   16.294  < 2e-16 ***
    ## mnth8        0.195102   0.005898   33.078  < 2e-16 ***
    ## mnth9        0.270869   0.005427   49.914  < 2e-16 ***
    ## mnth10       0.187711   0.005395   34.794  < 2e-16 ***
    ## mnth11       0.061117   0.005303   11.524  < 2e-16 ***
    ## mnth12       0.045360   0.004676    9.700  < 2e-16 ***
    ## hr1         -0.466697   0.008182  -57.038  < 2e-16 ***
    ## hr2         -0.839682   0.009313  -90.160  < 2e-16 ***
    ## hr3         -1.507858   0.012163 -123.967  < 2e-16 ***
    ## hr4         -2.110448   0.015858 -133.084  < 2e-16 ***
    ## hr5         -0.956562   0.009787  -97.738  < 2e-16 ***
    ## hr6          0.400501   0.006619   60.509  < 2e-16 ***
    ## hr7          1.422874   0.005666  251.117  < 2e-16 ***
    ## hr8          1.916567   0.005423  353.411  < 2e-16 ***
    ## hr9          1.391883   0.005648  246.429  < 2e-16 ***
    ## hr10         1.123194   0.005806  193.438  < 2e-16 ***
    ## hr11         1.269596   0.005717  222.071  < 2e-16 ***
    ## hr12         1.447484   0.005642  256.544  < 2e-16 ***
    ## hr13         1.427091   0.005663  251.991  < 2e-16 ***
    ## hr14         1.364773   0.005707  239.121  < 2e-16 ***
    ## hr15         1.405023   0.005693  246.794  < 2e-16 ***
    ## hr16         1.628120   0.005593  291.123  < 2e-16 ***
    ## hr17         2.036233   0.005445  373.972  < 2e-16 ***
    ## hr18         1.970299   0.005443  362.018  < 2e-16 ***
    ## hr19         1.674864   0.005518  303.540  < 2e-16 ***
    ## hr20         1.377556   0.005648  243.882  < 2e-16 ***
    ## hr21         1.121995   0.005800  193.434  < 2e-16 ***
    ## hr22         0.864956   0.006005  144.039  < 2e-16 ***
    ## hr23         0.483910   0.006419   75.382  < 2e-16 ***
    ## holiday1    -0.160979   0.003797  -42.398  < 2e-16 ***
    ## weekday1     0.051206   0.002167   23.626  < 2e-16 ***
    ## weekday2     0.060927   0.002103   28.971  < 2e-16 ***
    ## weekday3     0.066408   0.002103   31.582  < 2e-16 ***
    ## weekday4     0.067338   0.002089   32.228  < 2e-16 ***
    ## weekday5     0.093581   0.002089   44.798  < 2e-16 ***
    ## weekday6     0.079608   0.002091   38.063  < 2e-16 ***
    ## weathersit2 -0.064256   0.001422  -45.175  < 2e-16 ***
    ## weathersit3 -0.492963   0.002864 -172.124  < 2e-16 ***
    ## weathersit4 -0.469206   0.067067   -6.996 2.63e-12 ***
    ## temp         0.164396   0.019469    8.444  < 2e-16 ***
    ## atemp        0.946850   0.020326   46.584  < 2e-16 ***
    ## hum         -0.205716   0.004129  -49.824  < 2e-16 ***
    ## windspeed   -0.109958   0.004869  -22.581  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for poisson family taken to be 1)
    ## 
    ##     Null deviance: 2891591  on 17378  degrees of freedom
    ## Residual deviance:  572011  on 17326  degrees of freedom
    ## AIC: 683018
    ## 
    ## Number of Fisher Scoring iterations: 5

Quitando working day mi indice AIC es el mismo.

    BICIS$cnt

    ##     [1]  16  40  32  13   1   1   2   3   8  14  36  56  84  94 106 110  93
    ##    [18]  67  35  37  36  34  28  39  17  17   9   6   3   2   1   8  20  53
    ##    [35]  70  93  75  59  74  76  65  53  30  22  31   9   8   5   2   1   3
    ##    [52]  30  64 154  88  44  51  61  61  77  72  76 157 157 110  52  52  20
    ##    [69]  12   5   2   1   2   4  36  94 179 100  42  57  78  97  63  65  83
    ##    [86] 212 182 112  54  48  35  11   6   6   2   2   3  33  88 195 115  57
    ##   [103]  46  79  71  62  62  89 190 169 132  89  43  42  19  11   4   2   1
    ##   [120]   4  36  95 219 122  45  59  84  67  70  62  86 172 163 112  69  48
    ##   [137]  52  23  17   7   1   1   5  34  84 210 134  63  67  59  73  50  72
    ##   [154]  87 187 123  95  51  39  36  15  25  16  16   7   1   5   2   9  15
    ##   [171]  20  61  62  98 102  95  74  76  69  55  30  28  37  34  22  25  12
    ##   [188]  11   4   1   1   1   6  10  19  49  49  83  75  72  82  92  62  48
    ##   [205]  41  38  20  15   6   5   1   3   1   3   3  31  77 188  94  31  30
    ##   [222]  52  54  47  45  74 178 155  95  74  38  24  18  12   3   3   6  27
    ##   [239]  99 217 130  54  35  57  52  63  47  76 136  95  51  32  20  29  19
    ##   [256]   7   6   1   5  16  54 128  81  39  35  55  49  44  49  68 139 137
    ##   [273]  83  56  57  33  20   7   2   2   3   4   3  28  72 202 139  38  37
    ##   [290]  52  83  42  60  78 162 144  99  64  40  30  15  14   5   1   1   8
    ##   [307]  17  70 158 117  44  53  61  77  64  68  90 159 139  92  68  52  36
    ##   [324]  27  28  20  12   8   5   1   3  10  23  33  59  72  89 101 118 129
    ##   [341] 128  83  84  74  41  57  26  44  39  23  16  15   1   2   1   3  18
    ##   [358]  32  79  93 104 118  91 113  99 105  67  61  57  28  21  18  17  16
    ##   [375]   8   2   3   1   5  13  33  47  57  64  80  93  86  93  82  71  92
    ##   [392]  60  33  27  13   4   3  22  28  35  61 125 133  99  83  41  33  20
    ##   [409]   3   7   3   2   7  32  90 197 109  47  52  70  78  75  82 104 197
    ##   [426] 161 112  76  59  59  28  13   5   2   1   1   6  35 101 249 143  57
    ##   [443]  68  84  98  81  70  91 215 185 152 126  57  56  31  21   6   2   1
    ##   [460]   1   5  27  68 217 166  63  59  78  73  62  65  97 161 120  96  53
    ##   [477]  41  34  27  13  12  11   7   3   2   8  27  40  53  63  70  84  75
    ##   [494] 103  83  67  54  59  45  39  30  33  22  13  18   5   3   1   2  19
    ##   [511]  28  58  99 116  87 110  77  65  55  49  50  35  25  28  21   7   1
    ##   [528]   1   1   5  15  84 177 102  40  46  63  60  45  57  70 184 153 106
    ##   [545]  81  59  35  24   9   5   2   1   9  36 108 238 144  55  61 106  93
    ##   [562]  68  84 116 222 225 146 119  45  53  40  17   5  10   1   8  30  72
    ##   [579]  58  28  41  48  47  36  43  36  26  24  84 104  79  59  38  27  16
    ##   [596]   9   3   2   1   4  16  60 157 101  49  30  29  31  38  41  80 149
    ##   [613] 109  89  62  58  26  23  28  20  15   8   3   2   5  34  34  55  64
    ##   [630]  78  65  99 120 107  91  68  58  43  36  32  33  33  29  11   8   1
    ##   [647]   3   3  12  38  64  59  97  84 122 109 123  77  65  55  33  28  21
    ##   [664]  21   7   7   1   2   2   8  37  72 185 112  69  48  68  54  86  44
    ##   [681]  86 161 156 111  78  56  34  17   8   3   2   2   3  22  52 135 116
    ##   [698]  47  51  55  52  54  52  64 176 168 108  74  64  36  16   2   3   4
    ##   [715]   1   1   3  18  49 155 123  61  52  64  75  63  76 103 190 182  91
    ##   [732]  75  63  40  32  12   5   2   1   2  39  87 188 133  52  64  69  51
    ##   [749]  47  60  78 175 147  96 109  54  41  38  13   7   1   1   7  28  87
    ##   [766] 220 127  51  64  86  82  91  90  99 205 155 103  71  43  46  31  39
    ##   [783]  18  17  11   8   9   4   4  10  20  34  47  52  72  55  60  71  78
    ##   [800]  83  84  69  56  45  59  39  44  20  13   2   1   1   8  23  45  89
    ##   [817] 117 174 182 161 182 157 121  78  21  26  27  62  30  15   5   3   1
    ##   [834]   2  10  30  95 230 118  55  47  66  64  60  50 114 216 175 128  88
    ##   [851]  78  37  25  10   4   2   1   3   2  39 100 243 135  48  50  65  50
    ##   [868]  64  51  83 176 152 101  56  54  29  12  17   7   3   2   7  43  99
    ##   [885] 199 123  59  41  61  69  58  64  79 166 170  88  84  83  46  37  16
    ##   [902]   7   3   1   6  26  99 178 122  35  45  69  62  48  50  80 165 160
    ##   [919] 112  97  72  51  34  14   7   3   4  24  74 216 140  44  64  71 110
    ##   [936]  84  74 125 211 174 101  63  47  43  53  30  24  15  10   4   1   2
    ##   [953]  11  30  43  84 114 120 135 120 174 145 137  64  41  40  51  45  32
    ##   [970]  39  24  20  12   2   5   3  12  47 105 112 152 154 161 162 134 125
    ##   [987]  95  61  47  51  36  30  11   7   2   2   2   3  26  98 256 130  55
    ##  [1004]  53 126 120  90  90 118 243 205  98  70  43  45  20  19   5   3   1
    ##  [1021]   4  30 105 223 110  52  57  71  82  79  85  98 206 212 145 101  66
    ##  [1038]  41  20  15   9   1   1   5  33 108 230 124  69  66  86  93  82 117
    ##  [1055] 122 255 222 161 118  92  73  33  17   6   6   4   4  12  47 126 285
    ##  [1072] 179  88  90 101 123  81 106 146 274 222 175 139 124  76  44  32   8
    ##  [1089]   7   8   1   7  50 116 272 169  95 113 185 176 191 172 232 327 224
    ##  [1106] 162 124  72 107  77  29  31  17   7   3   3   6  22  45  55 106 123
    ##  [1123] 119 155 196 171 120 127  88  59  47  33  44  29  17  16  17  18   1
    ##  [1140]   2   2  24  55 104 161 182 198 212 168 147 146 101  42  42  56  47
    ##  [1157]  54  37  13   4   5   1   3   8  19  63  57  81  90  93 103 116  87
    ##  [1174]  56  80  66  49  29  34  11   2   7  40 114 106  44  50  55  55  58
    ##  [1191]  69  86 196 167 118 105  82  77  21   6   4   1   2   8  36  96 235
    ##  [1208] 139  51  69  67  87  78  72  94 222 209 141 109  91  61  39  11   7
    ##  [1225]   4   2   4  58 104 252 137  57  79  95  88  69  63  71 177 137 115
    ##  [1242]  89  83  55  50   9  10   3   2   6  11  35  73 114  49  59 115  83
    ##  [1259]  85  55 102 189 157  90  62  63  50  39  28  27  12   8   2  10   7
    ##  [1276]  22  57  74  85 126 160 174 211 165 171 183 137  94  68  68  46  34
    ##  [1293]  34  37  22  11   2   3  14  35  59 103 125 193 259 282 261 268 187
    ##  [1310] 127 102  81  69  69  59  30   8   1   5  28  92 255 142  92  87  36
    ##  [1327]  32  25  35  42  79 131  80  45  80  76  45   7   3   4   2   1   2
    ##  [1344]  46 107 214 124  68  56  86  78  79  64 102 242 224 119  88  63  47
    ##  [1361]  25   8   4   2   4   1   5  40 110 256 148  76  73  82 125  96 110
    ##  [1378] 139 268 201 150  95  71  48  22  13   1   3   1   1   8  35 111 220
    ##  [1395] 145  56  50  72  80  78  68  71 202 167 106  70  51  53  23  12   4
    ##  [1412]   2   1   1   7  29  83 228 150  66  89  97  87  95 106 118 228 187
    ##  [1429] 107  87  70  44  46  19  25  20   2   1   3   4  15  45  63 103 159
    ##  [1446] 167 190 198 205 204 164 152  89  70  72  63  44  52  39  32  11   3
    ##  [1463]   2   5   9  22  31  62  79 105  39   7  11  25  23  11  11   7  10
    ##  [1480]   9   4   2   1   1   1  34  91 211 134  58  60  96  70  77  96 122
    ##  [1497] 233 233 145  92  54  35  22  10   4   1   7   2  10  45 128 257 151
    ##  [1514]  71  78  99  98  72 112 108 248 235 150  76  75  55  41   9   4   2
    ##  [1531]   3   2   8  49 141 249 141  57  59  98  99  71  79 112 194 188 134
    ##  [1548]  91  62  21  18   3   2   1   3  12  37  44  24  17  11  34  12  12
    ##  [1565]  14  21 111  82  56  38  28  34  27   6   8   5   2   7  32 114 259
    ##  [1582] 156  78  81 111 107  89  85 113 223 153 118  76  53  64  37  34  18
    ##  [1599]  14   7   1   2   4  23  53 101  99 119 158 157 166 182 244 213 145
    ##  [1616] 112  86  82  63  49  38  33  23  13   3   2  10  34  44 122 181 194
    ##  [1633] 221 250 304 256 241 147 101  77  58  37  28  11   1   1   3   8  28
    ##  [1650]  88 241 140  84  71  94  75  79 101 107 271 235 150 129  63  47  19
    ##  [1667]  14  10   3   2  11  35 119 282 157  78  78 102  97  88  90 114 217
    ##  [1684] 225 152  88  55  21  18   8   3   3   1   3  31 111 253 154  68  75
    ##  [1701]  88  99 100 101 151 253 237 168 121  78  59  27  23  12   8   2   3
    ##  [1718]  13  51 140 299 185  78  86 105 111 119 125 142 313 310 207 137 138
    ##  [1735]  87  50  29  16  14   5  10  36 123 280 210 117 159 195 175 174 173
    ##  [1752] 204 332 331 190 149 113 118  86  76  51  25   8   3   2  10  13  48
    ##  [1769]  76 140 196 267 301 312 313 304 278 217 154 108  93  72  50  40  24
    ##  [1786]  30  12   3   9   4  14  37  78 136 203 228 203 279 308 224 226 156
    ##  [1803]  79  67  49  38  24  13   7   6   1   2  32  18  98 127  62  79  81
    ##  [1820] 112 101 107 141 272 276 221 122  91  78  30  11  19   1   5   1  12
    ##  [1837]  60 136 336 178 124  86 121 106 101 134 165 307 312 164 135  87  70
    ##  [1854]  32  11   7   2   3  10  44 118 261 182  75  71  87 108  84  85 136
    ##  [1871] 273 238 159  75  22  44  26  11   3   6   1   1   9  41 108 124  97
    ##  [1888]  65  71  94 101  83  90  84 233 214 138 118  78  62  33  18   5   5
    ##  [1905]   2   5   9  32 104 237 166  68  75 145 138 133 122 150 225 198 135
    ##  [1922]  81  75  43  39  28  27  25   9   6   8  10  41  66 100 125 168 227
    ##  [1939] 239 216 219 248 203 153 138  85  51  63  41  31  20  21  14   6   2
    ##  [1956]   7  14  54  86 108 163 147 164 172 143 159 137 103  57  36  30  19
    ##  [1973]  12   6   5   2   8  40 110 254 121  69  57  79  83  80  72 132 252
    ##  [1990] 241 173 112  61  40  19  16   4   3   3   2   8  50 133 287 172  59
    ##  [2007]  81 106 104  86  87 136 299 294 190 132  83  55  35  11  10   4   4
    ##  [2024]   1   7  48 136 263 154  64  90  80  98  80  70  36 116  85  72  44
    ##  [2041]  27  24  12   3   5   5   1   2   8  36  92 192 132  69  58  90  68
    ##  [2058]  76  65  85 168 177 130  85  60  51  27   6   4   7   4   3  12  28
    ##  [2075]  95 206 173  75  89  95 110  87 111 167 281 241 136  77  93  74  53
    ##  [2092]  32  32  21   9   5   5  12  18  55  87 154 198 184 137 190 136 196
    ##  [2109] 184 179 148  85  77  55  53  39  34  24  11   3   5  33  33  62 142
    ##  [2126] 215 243 313 367 349 292 303 274 172 144  79  45  36  31   6  11   2
    ##  [2143]   1   2   7  46 157 339 158  90 124 157 139 123 143 189 366 386 278
    ##  [2160] 173  95  75  48  22  15   5   4   2   5  38 134 157  46  28  19  38
    ##  [2177]  56  76  68 130 273 267 160  91  89  46  26  15   2   5   4   1  13
    ##  [2194]  56 133 325 165  69  93 134 112 115 117 169 356 325 232 141 102  84
    ##  [2211]  40  20  13   7   3   1   6  64 160 314 170  95 122 153 135 157 160
    ##  [2228] 213 343 333 226 203 108  81  54  25   9  13   1   1   9  35 113 221
    ##  [2245] 185  73  98  80  42  15  25  60 148  62  53  63  54  51  35  31  20
    ##  [2262]  17  14   3   5  13  21  54  98 133 142 202 222 212 201 191 257 177
    ##  [2279] 130  93  75  87  57  53  37  36  16   5   4   4  10  55  81 150 202
    ##  [2296] 230 263 281 297 288 236 240 131  92  95  57  32  23   4   9   1  13
    ##  [2313]  61 176 314 165  96 104 134 139 145 141 235 452 383 284 166 158  91
    ##  [2330]  54  24  13  14   1   6  16  58 189 226  54  40  49  92 106  44  78
    ##  [2347]  99 291 224 130 103  84  54  39  15   1   2   2   3   5  34  70 164
    ##  [2364]  65  46  61  95  61 106  81 142 309 321 175 126 117  99  62  21  10
    ##  [2381]   3   2   9  14  70 193 337 170  89 115 145 159 137 142 202 388 337
    ##  [2398] 259 169 145 104  47  27  15  18   3   3  13  47 149 300 178 122 136
    ##  [2415] 200 180 163 162 243 331 263 209 118 102  80  64  43  33  24  14   5
    ##  [2432]   4  10  21  45  29  22  31  32  58  63  78  33  15  38  71  53  14
    ##  [2449]  24  35  33  31  29  25   7   3   8  16  43 102 211 304 354 343 361
    ##  [2466] 388 343 306 277 210 141  83  70  56  39  19  16   7   6  16  54 168
    ##  [2483] 297 169 109 123 184 169 144 176 205 428 362 286 165 151  90  46  25
    ##  [2500]   7  13   3   5  18  65 188 351 162  75  72 103 129 107 127 192 411
    ##  [2517] 421 276 168 122 112  52  33   7   2   2   9  15  64 237 396 197 131
    ##  [2534] 118 145 177 148 145 255 432 441 383 225 172 117  93  44  26  13   7
    ##  [2551]   6  14  75 198 365 196  98 133 199 177 228 235 294 481 452 324 204
    ##  [2568] 184 137  99  33  24   9   2   3  16  49 180 346 195 136 106  80  40
    ##  [2585]  36  75  62  69  51  55  47  28  24  17  18  16  17   4   5   7  11
    ##  [2602]  19  47  68 129 232 291 312 391 397 426 383 376 283 183 135 169 117
    ##  [2619]  96  54  72  24   5  12  13  27  80 177 289 317 388 441 420 369 359
    ##  [2636] 331 270 194 105  61  45  42  12  10  10  10   5  17  66 196 353 174
    ##  [2653] 125 147 188 203 146 175 239 521 499 382 237 190 106  62  27  21  26
    ##  [2670]  21   6  16  80 270 449 199 120 172 186 176 155 153 291 521 528 328
    ##  [2687] 234 195 148  78  27  17   5   7   6  17  84 246 444 181  92 156 173
    ##  [2704] 150 148 138 218 521 412 260 218 145 129  78  54  20  21   3   9  13
    ##  [2721]  86 215 398 183  54  84 107 121 142 163 270 455 530 404 269 194 153
    ##  [2738] 110  48  31  10   5   6  25  71 240 421 230 156 157 223 308 248 252
    ##  [2755] 299 508 439 270 194 195 146 113 106  74  65  17   7   7  21  48  91
    ##  [2772] 156 279 321 416 455 452 499 464 407 371 387 224 177 143 125  96  59
    ##  [2789]  50  23  17  10  13  33  59 141 264 250 281 332 238 266 214 196 159
    ##  [2806] 178 121 105 100 146 177 114  35  16  17  20  62 209 371 172 108 130
    ##  [2823] 187 170 181 155 269 537 518 314 218 232 125  64  16  14   5   2   4
    ##  [2840]  14 109 265 459 186 127 140 217 194 158 154 271 517 544 365 290 225
    ##  [2857] 113  62  27   7   4   1   3   9  21  52  79  40  31  72 124 143 107
    ##  [2874] 129 195 410 396 296 191 150  89  57  27  10   5   4   7  30  89 271
    ##  [2891] 440 184 128 148 196 192 154 147 269 533 520 361 258 180 165 115  56
    ##  [2908]  32  16   9   1  16  82 222 450 188 127 165 246 253 213 260 344 553
    ##  [2925] 470 292 190 152 151 120  86  58  52  18   5   7  13  40 115 218 298
    ##  [2942] 347 373 436 378 378 342 354 289 267 219 182 120 119 100  64  59  31
    ##  [2959]  10   5   6  23  86 160 244 340 382 390 366 358 385 351 268 239 174
    ##  [2976] 127 115  50  53  33   8   7   4  23  89 237 374 178 119 125 195 174
    ##  [2993] 162 199 304 598 524 384 239 156 116  61  18  15   5   4   2  29 112
    ##  [3010] 314 425 205 127 125 217 202 176 180 327 611 550 434 291 203 150  81
    ##  [3027]  38  20  10   3   2  20  99 318 442 203 154 141 226 193 177 179 289
    ##  [3044] 259 274 401 273 234 146  81  44  17  15   5   6  29 112 297 421 212
    ##  [3061] 147 179 207 218 191 217 288 594 527 364 276 223 172 103  52  20  11
    ##  [3078]   4   5  25  82 157 387 233 130 146 223 226 190 198 300 491 398 270
    ##  [3095] 152 154 132 119 102  82  52  35   5  14  11  32  88 100 167 279 279
    ##  [3112] 248 216 294 295 272 304 248 157  59  26  44  39  47  50  32  13  14
    ##  [3129]  17  42  70 134 226 322 367 456 437 459 440 392 237 231 191 151 117
    ##  [3146]  69  38  14  13   4   6  21 104 272 394 194 112 185 209 214 193 165
    ##  [3163] 226 274 453 308 198 177 127  57  31  13   8   4   4  22  49 151 347
    ##  [3180] 207 104 130 153 172 184 210 290 604 480 349 283 174  98  56  23  12
    ##  [3197]   6   9   3   9 101 274 453 202 106  23  54 122 138 167 294 565 489
    ##  [3214] 331 213  85 117  59  29   6  15   4   5  26 103 257 487 216 130 164
    ##  [3231] 168 183 180 188 302 547 513 410 282 209  79  72  48  25   8   5   5
    ##  [3248]  28 117 319 517 234 148 196 255 232 197 242 330 554 473 302 220 195
    ##  [3265] 152 115  98  72  40  19   7   5  28  43 126 225 323 418 493 462 456
    ##  [3282] 506 471 444 444 301 248 227 204 145 131  98  68  33  12   6  18  40
    ##  [3299]  94 189 305 373 401 455 316 331 273 306 358 300 231 136 121  65  41
    ##  [3316]   9   5   4   4  11  96 236 409 167 121 180 204 204 187 176 279 429
    ##  [3333] 453 358 310 188 131  72  35  15  10   2   4  24 112 314 434 138 108
    ##  [3350] 175 170 190 153 188 277 548 564 355 280 189 141  66  35   8   8   9
    ##  [3367]   3  25 117 313 531 234 135 140 210 202 168 182 299 601 517 405 340
    ##  [3384] 236 174  86  45  16  14   4   2  27  98 286 489 216 143 160 194 197
    ##  [3401] 189 141 291 494 478 378 322 223 180  90  69  50  18  12   7  31  81
    ##  [3418] 249 418 227 153 166 243 209 239 356 419 491 399 279 144 151 141 127
    ##  [3435]  94  55  48  15   4   7  19  50 102 177 276 349 358 385 382 374 402
    ##  [3452] 341 385 252 196 201 161 125  83  66  61  26  23   6  15  30  96 152
    ##  [3469] 251 319 402 418 351 376 361 383 328 287 219 229 171 135 103  79  45
    ##  [3486]  18   6   7  16  31  94 128 269 321 366 350 327 311 309 304 249 225
    ##  [3503] 237 149 106  48  29  17  12   4   3  26 109 235 406 175 109 106 162
    ##  [3520] 147 140 175 249 495 417 285 251 210 159  61  34  17   3   6   4  21
    ##  [3537] 128 284 454 207 115 112 169 154 145 152 266 486 309 264 256 190 129
    ##  [3554]  69  42  15   6   4   3  31 113 304 467 225 139 165 232 203 185 188
    ##  [3571] 318 572 525 379 337 248 155 112  68  22  12   5   6  28 106 277 502
    ##  [3588] 230 161 192 259 248 240 212 349 558 564 391 293 247 171 171  93  94
    ##  [3605]  59  18  15   8  28  87 125 224 317 369 420 456 451 404 440 344 341
    ##  [3622] 285 239 185 190 150 113  96  64  47  12   6  21  27  98 164 251 335
    ##  [3639] 335 367 385 417 398 390 363 357 279 165 130  86  29  14   8   5   8
    ##  [3656]  31 112 299 438 188  95 111 153 180 145 165 267 579 556 407 336 189
    ##  [3673] 173  60  20   4   5   2   5  32 126 334 477 217 123 151 183 196 146
    ##  [3690] 183 308 539 551 424 346 218 153  90  41  23   7   3   6  21 116 353
    ##  [3707] 481 202  90 134 149 136 155 128 233 506 459 384 280 202 176 116  47
    ##  [3724]  16   6   2   6  20 118 314 432 168  94 118 140 136 118 104 200 409
    ##  [3741] 466 326 254 171 153  97  69  23  13   6   4  28 104 267 452 213 111
    ##  [3758] 153 207 189 165 212 285 517 463 306 250 218 185 146 117  77  60  28
    ##  [3775]  13  16  40  68 143 230 264 325 347 387 380 374 394 337 286 264 275
    ##  [3792] 220 190 131 119  93  66  28  14  10  14  34  92 182 311 367 420 373
    ##  [3809] 315 360 350 252 256 223 206 140 128 107  28  16  10   7   4  29 105
    ##  [3826] 328 476 224 118 159 196 227 200 185 326 601 586 423 331 218 134  89
    ##  [3843]  31  13  10   2   3  26 113 365 486 193 130 143 169 209 159 178 330
    ##  [3860] 569 538 386 297 243 188 110  52  14   9   1   4  21 121 370 498 207
    ##  [3877] 135 164 211 189 178 164 312 638 607 416 330 257 175 107  47  17   5
    ##  [3894]   4   6  25 112 188 388 261 121 150 206 151 153 159 294 272 325 327
    ##  [3911] 201 188 151  16  24  23  13   6   6  13  97 250 454 224 170 181 243
    ##  [3928] 204 215 243 355 552 450 328 232 208 175 178 104  95  53  20   5   7
    ##  [3945]  27  57 131 218 244 404 420 327 334 367 468 449 381 326 188 183 161
    ##  [3962] 150  89  76  72  30  17  19  33  71  84 169 244 377 396 363 317 362
    ##  [3979] 378 397 296 311 232 167 152  92  26  12   2   1   3   3  26  46 221
    ##  [3996] 219  91  94 162 179 175 205 299 565 538 431 316 206 130  60  33  14
    ##  [4013]   6   2   9  25 115 309 401 275 161 171 189 207 152 170 335 572 536
    ##  [4030] 425 326 229 109  64  33  21   7   7  11  23 122 311 423 219 135 145
    ##  [4047] 188 147 145 146 258 529 513 342 282 247 148 105  49  31   4   5   7
    ##  [4064]  24 100 327 432 227 127 136 160 186 149 189 275 569 540 400 352 234
    ##  [4081] 162 105  63  28  14   5   9  20  91 268 466 231 145 203 201 221 229
    ##  [4098] 216 327 557 452 385 288 233 167 172 116  95  59  39  11  14  29  42
    ##  [4115] 112 186 302 331 392 371 383 392 410 399 307 375 269 202 187 179 115
    ##  [4132]  90  87  34  11  12  20  51 107 206 332 346 443 442 443 403 455 421
    ##  [4149] 382 317 213 183 114  78  32  15   6   2   9  22  94 261 447 187 150
    ##  [4166] 150 170 164 158 202 275 604 591 396 305 242 145  81  30  13  11   2
    ##  [4183]   8  21 124 347 456 246 157 167 202 189 160 166 261 579 504 299 296
    ##  [4200] 195 134  81  36  28  16   5   5  22 116 336 540 221 126 170 243 231
    ##  [4217] 188 189 328 570 588 401 333 221 183 129  54  32   8   9   6  25 123
    ##  [4234] 320 555 260 132 169 233 232 163 240 323 594 586 455 340 323 207 126
    ##  [4251]  68  31  13  11   6  30 108 243 492 260 170 214 263 292 303 381 427
    ##  [4268] 461 422 318 269 218 222 140 115  78  52  26  11  14  32  45 132 179
    ##  [4285] 271 357 374 404 392 331 398 349 383 305 284 232 201 154 144  78  69
    ##  [4302]  33   5   1   3  25  92 181 264 390 404 421 375 376 414 358 181 183
    ##  [4319] 176 167 162 147 140 119  63  26  12   4  16  36  86 238 280 349 447
    ##  [4336] 423 408 408 452 436 418 375 386 457 326 138  55  39  13   8   5  20
    ##  [4353]  94 270 432 227 139 172 201 203 178 162 281 545 496 368 284 245 155
    ##  [4370]  73  37  15   5   6   6  35 121 312 428 189  43 127 152 140 163 185
    ##  [4387] 275 596 563 394 312 241 186  98  35  16   8   4   4  32 118 292 443
    ##  [4404] 179 101 142 171 155 160 166 265 569 562 365 285 209 182 129  47  34
    ##  [4421]  22   6  11  27 102 332 457 241 138 165 213 229 216 141 132 167 316
    ##  [4438] 270 250 204 169 151 121  53  55  30   6  15  38  71 182 247 264 290
    ##  [4455] 366 416 373 402 416 401 378 306 314 238 214 140 154 113  96  25   5
    ##  [4472]  17  25  65  92 169 268 287 377 367 349 361 372 363 363 303 268 227
    ##  [4489] 143  72  35  10  12  10   4  26 121 317 420 208 117 128 148 141 134
    ##  [4506] 146 239 514 472 373 236 130  90  55  22  14   9   8   6  25 115 330
    ##  [4523] 417 176 123 125 163 136 124 148 202 428 511 337 308 258 194  79  46
    ##  [4540]  12   5   4   5  26 121 313 449 186 113 165 174 158 116  46 141 488
    ##  [4557] 463 419 342 250 184 116  46  25  10   6   6  28 130 334 486 224 127
    ##  [4574] 148 223 177 178 180 274 595 578 400 348 261 176 124  80  20  38   7
    ##  [4591]  10  23 120 274 564 207 166 210 238 247 265 237 361 587 496 394 343
    ##  [4608] 261 223 167 110  75  58  26  16  10  37  52 127 222 322 396 463 497
    ##  [4625] 433 428 425 475 428 406 296 252 201 168 125 102  94  47  10  12  21
    ##  [4642]  50 118 184 304 391 420 472 430 324 317 369 390 379 262 207 181  93
    ##  [4659]  51  25  10   7   4  17 105 277 347 203 130 141 165 152 138 164 265
    ##  [4676] 545 558 398 323 202 140  91  51  13  13   3   5  19 134 315 412 199
    ##  [4693] 141 127 153 172 152 158 277 513 530 388 277 241 156  92  40  11   9
    ##  [4710]   6   4  23 116 303 438 209 110 142 149 120 127 138 234 455 497 382
    ##  [4727] 309 242 169  99  42  21   6   6   4  18 117 274 381 200  78 106 135
    ##  [4744] 102 147 133 210 414 382 284 237 225 149 113  60  24  14   7  13  19
    ##  [4761]  96 252 322 208 115 109 115 123 101 133 182 306 284 250 212 190 149
    ##  [4778] 103 101  70  61  22   9   7  25  45 109 132 198 216 235 245 236 215
    ##  [4795] 221 197 196 217 145 154 125 104 119  96  60  37   6   8  22  35 104
    ##  [4812] 154 226 243 246 252 283 238 249 249 243 240 195 153  98  50  34  12
    ##  [4829]   6   4   6  24 107 277 387 135  84 107  67  78  90 112 198 485 533
    ##  [4846] 386 302 194 125  87  28  12   7   4   6  27 110 318 424 187 138 151
    ##  [4863] 147 182 147 163 254 537 546 398 290 267 158  89  45  10   6   4   4
    ##  [4880]  18 123 347 433 216 114 136 169 180 127 140 268 572 541 375 294 245
    ##  [4897] 180 109  45  29   7   5   6  27 116 305 456 210 116 154 186 152 138
    ##  [4914] 146 256 468 483 322 270 196 174 123  57  34  13  11   6  27  90 254
    ##  [4931] 384 179 106 161 135 156 148 169 233 421 362 241 206 163 174 116 133
    ##  [4948]  90  42  43  10  13  24  42 114 174 261 317 341 308 305 302 302 302
    ##  [4965] 299 256 206 205 209 177 113  79  67  50  13   8  13  45 142 183 254
    ##  [4982] 275 338 294 326 293 306 320 304 274 275 164  96  70  29  17  11   4
    ##  [4999]   4  26 100 282 382 166  97 119 168 150 123 129 231 514 543 413 305
    ##  [5016] 220 137  96  29  12   8   5  10  21 117 335 435 213 140 128 157 203
    ##  [5033] 159 168 274 556 556 454 288 254 191 132  43  16  11   4   7  24 110
    ##  [5050] 271 437 195  68 122 161 144 147 118 150 425 492 321  87  88  77  56
    ##  [5067]  17  17   7   3   7  20  97 282 398 187 113 149 205 205 167 165 290
    ##  [5084] 555 523 366 286 211 174 132  54  19  19   6   7  16 101 270 454 226
    ##  [5101] 163 147 216 204 245 210 330 550 466 372 291 173 188 139 133  67  53
    ##  [5118]  42  10   6  27  63 121 215 290 349 382 441 380 362 343 149 211 171
    ##  [5135] 137 114 120 108  80  73  66  32   6   5  13  39  89 184 266 269 370
    ##  [5152] 324 322 326 193 194 229 228 183 136 103  55  30  12   7   1  10  15
    ##  [5169]  95 267 349 183 124 179 182 151 150 126 282 527 529 416 286 186 147
    ##  [5186]  72  35  14  12   3   7  30 101 343 392 175 144 149 185 175 148 164
    ##  [5203] 285 532 585 371 292 230 155  75  37  17  11   4   5  32 119 305 399
    ##  [5220] 180 113 149 240 191 178 194 282 584 559 413 312 209 139 108  46  26
    ##  [5237]  11   9   5  25 106 307 390 197 132 187 214 217 142 178 298 582 571
    ##  [5254] 371 288 206 157 127  65  23  15   9   5  26  76 264 426 205 130 156
    ##  [5271] 222 253 186 249 334 516 465 385 287 226 231 151  84  77  74  27  10
    ##  [5288]  18  22  44 118 208 260 314 421 153 259 358 281 269 292 237 183 167
    ##  [5305] 154 120  90  73  62  27   3  10   6  24  41 103 183 198 281 377 370
    ##  [5322] 331 292 329 347 259 193 155  39  27  25  10   3   7   5  17  90 258
    ##  [5339] 355 222 141 153 178 193 151 182 283 530 483 397 285 184  98  88  31
    ##  [5356]  16   4   6   5  30 119 346 441 198 111 150 162 192 182 159 303 600
    ##  [5373] 570 376 280 215 152  77  24  14   6   5   5  28 105 308 487 209 118
    ##  [5390] 128 189 176 192 159 300 552 556 347 280 230 171 105  56  24   6   6
    ##  [5407]   9  27 103 308 420 228 129 141 173 187 182 174 245 409 274 141 165
    ##  [5424] 149 135 114  62  20   8   3   8  17  93 263 398 218 165 165 222 232
    ##  [5441] 220 222 327 527 425 165  58 107 122 106 128  47  69  51   7   8  24
    ##  [5458]  44 133 190 306 338 409 378 375 390 373 383 394 291 248 225 223 157
    ##  [5475]  74  86  64  51   6   8  14  31  92 231 274 313 374 310 235  68 258
    ##  [5492] 270 311 282 181 138 134  68  40  21   8   5  12  24 112 281 351 187
    ##  [5509] 121 131 207 205 225 199 320 591 609 416 268 208 153  64  35  19   6
    ##  [5526]   6   5  36 114 344 504 225 148 186 237 234 651 601 444 472 519 367
    ##  [5543] 260 211 191  80  38  24   6   1   7  27 108 308 468 213 146 185 235
    ##  [5560] 236 204 204 271 567 621 440 334 236 181  70  52  15   5   4   7  26
    ##  [5577] 102 313 484 162 142 151  41  37 113 132 121 312 408 272 225 187 147
    ##  [5594]  84  51  23  20  11   8  26 105 266 404 230 123 214 243 237 219 249
    ##  [5611] 343 489 410 290 222 190 152 136 145  64  77  30   4  12  18  28  76
    ##  [5628] 156 205  88  76  43  23  30  24  16   1   8  53 142 237 306 431 447
    ##  [5645] 469 491 398 402 387 214 186  98  64  34  20  13   6   3  17 102 284
    ##  [5662] 404 188 115 138 207 218 182 220 282 591 547 367 277 202 142  75  27
    ##  [5679]  13  10   4   6  28 119 357 493 218 137 175 237 229 209 196 337 611
    ##  [5696] 576 465 298 223 143  93  32  15   6   6   5  27 111 328 467 244 138
    ##  [5713] 171 260 221 196 172 309 608 565 446 289 201 157  84  51  21  25  14
    ##  [5730]   7  23 105 342 498 207 138 192 229 219 198 174 308 628 531 443 280
    ##  [5747] 177 179 126  64  32  20   8   4  20  76 259 456 220 140 192 260 279
    ##  [5764] 283 346 381 455 355 287 196 149 147  98  87  74  41  35   7  12  23
    ##  [5781]  38  89 201 258 194 257 353 380 388 374 371 339 295 219 164 162 123
    ##  [5798] 109  75  60  43   4   6   3  30  70 189 297 337 435 419 361 410 337
    ##  [5815] 376 356 328 229 207 155 104  97  54  39  16   8   6  11  41 101 152
    ##  [5832] 244 308 353 389 357 253 284 114 123 175  82  60  64  20   8   2   1
    ##  [5849]   4  21  72 172 364 185 106  89  67  29  24  89 106 291 373 281 163
    ##  [5866] 121  84  58  13   5   6   2   4  16  75  86 328 190  66  10  22  11
    ##  [5883]  25  58 144 285 237 226  96  38  29  24  14   4   2   3  14  56 179
    ##  [5900] 195  69  65  98 102 126 120 139 156 113  68  52  89  82  70  26  16
    ##  [5917]   8   8   2   3  14  57 108 288 136  29  72  75 122 148 188 285 505
    ##  [5934] 410 349 210 203 160 148 116  83  64  37   7   8   6  52 119 207 318
    ##  [5951] 360 404 370 359 480 431 460 360 315 245 230 175 139 108  88  76  45
    ##  [5968]   9  39  20  37  94 227 334 402 411 350 342 404 483 380 339 333 226
    ##  [5985] 159  95  45  16  12   2   4  24 109 312 408 176 129 165 184 163 189
    ##  [6002] 212 298 590 571 436 299 195 150  69  26  10   2   4   8  20 122 362
    ##  [6019] 425 214 135 153 187 184 190 180 292 579 539 396 272 245 144  74  33
    ##  [6036]   8   5   8   9  31 145 370 429 202 142 164 212 178 124 176 277 599
    ##  [6053] 586 381 261 213 160  72  37  20   5   8   7  31 124 338 391 209 121
    ##  [6070] 159 179 189 170 135 222 261 244 232 248 145 115  69  48  26  10   6
    ##  [6087]   4  32  93 299 409 210 140 179 251 223 225 203 358 566 482 315 225
    ##  [6104] 163 167 126 108  80  79  28   5   5  22  37 108 177 249 367 372 349
    ##  [6121] 297 354 305 359 329 256 176 170 143 136 112  79  89  36   1   6  12
    ##  [6138]  44  69 140 260 317 364 376 311 365 369 325 289 245 180 147  84  54
    ##  [6155]  37  10  13   5   8  28 113 332 420 215 105 139 189 177 188 187 263
    ##  [6172] 568 540 374 231 200 129  68  25  14   5   3   6  22 114 329 209  69
    ##  [6189]  30  56  60 103 112 192 264 511 492 337 253 206 148  81  27  17   6
    ##  [6206]   6   7  31 117 330 462 208 133 180 193 181 179 185 281 536 408 283
    ##  [6223] 160 200 127  95  41  11  10  12   2  30 101 311 415 203 120 165 176
    ##  [6240] 193 183 181 301 563 551 420 285 221 187 113  52  31  20   8  13  22
    ##  [6257] 104 254 317 111  35  21  29  34  24  61  56  99 234 248 179 138 157
    ##  [6274] 148  93  79  68  31   8   5  18  46 121 175 293 386 407 460 390 452
    ##  [6291] 449 426 400 308 243 222 193 150 173 124 106  59   8   3  17  38  86
    ##  [6308] 155 275 361 413 369 347 353 464 420 404 329 186 137 104  79  29  17
    ##  [6325]   8   5   6  25 113 332 355 205 154 159 209 187 154 209 313 570 553
    ##  [6342] 383 251 182 136  75  37  10  11  10   8  24 124 249 427 193 122 123
    ##  [6359] 162 151 152 205 258 530 356 320 238 196 121  93  36  14   9   3   4
    ##  [6376]  22 114 185 287 155 129 151 184 162 170 181 280 591 524 372 102  74
    ##  [6393]  87  71  40  23   2   8   7  22 111 283 431 225 127 158 195 211 167
    ##  [6410] 227 291 589 584 384 273 225 175  81  60  23  15   3   8  26  98 297
    ##  [6427] 456 237 168 217 254 266 266 245 360 591 511 374 230 206 159 132 130
    ##  [6444]  58  67  25   8   5  19  36  67 129 121 132 158 125 180 195 223 228
    ##  [6461] 140 126  66  56  70  65  47  24  30   9   7   7  15  29  64 115 248
    ##  [6478] 292 321 308 256 173 206 167 174 162  87  88  64  25  16   6   4   7
    ##  [6495]   8  21  94 249 372 153 101 126 147 118 120 101 218 495 412 315 204
    ##  [6512] 138  91  54  27   9   3   6   7  27 112 309 400 187 126 124 199 173
    ##  [6529] 183 194 309 585 524 345 261 166 114  66  43  11   3   6   5  32 117
    ##  [6546] 301 467 219 133 170 214 219 190 182 282 591 593 374 246 201 154  73
    ##  [6563]  40  22  10   5   5  24 114 302 457 208 129 144 226 191 195 219 323
    ##  [6580] 568 538 343 260 195 148  99  67  28   5  10   5  27  87 242 417 241
    ##  [6597] 174 230 235 305 256 270 419 563 465 294 197 166 146 136  89  65  45
    ##  [6614]  20  10   8  22  73 103 217 337 397 446 485 465 496 451 393 359 248
    ##  [6631] 202 187 149 142 129  78  55  30  14  15  21  44 107 185 365 423 455
    ##  [6648] 418 500 407 470 438 414 302 193 173 152 123  53  34  19  11   6  21
    ##  [6665]  44 114 254 219 265 322 365 365 370 384 402 459 473 336 225 175 126
    ##  [6682]  75  30  11   5   3   5  20 123 352 406 235 149 123 181 180 171 175
    ##  [6699] 320 604 539 318 234 175 147  57  25  10   2   2   7  22 112 259 404
    ##  [6716] 197 113 120  62 101  78  45  55 251 172 109  90  83  43  54  15  12
    ##  [6733]   4   6   7  17  52  81 209 109  69  84 105 162 133 155 276 479 268
    ##  [6750] 205 181 123 114  47  39  24   6   7   8  17  94 147 209 180 125  87
    ##  [6767]  75 132 156 230 344 481 423 248 196 178 143  95 106  73  56  23   6
    ##  [6784]   7  20  57 125 207 322 374 456 394 472 377 443 441 372 238 179 168
    ##  [6801] 166 135 107  77  65  42  12   7  10  48  99 190 347 377 444 438 462
    ##  [6818] 412 466 405 328 233 178 102 130  62  37  17   7   2   5  29 114 309
    ##  [6835] 414 206 135 166 205 205 166 208 302 620 518 333 229 177 104  62  31
    ##  [6852]  11   5   1   4  21 109 341 499 227 102 147 201 219 169 205 304 625
    ##  [6869] 570 317 227 206 137  70  34  16   7   3   3  31  68 210 202 123 149
    ##  [6886] 130 106  97 120 201 181 140 197 145  85 107  69  26  15   4   1   8
    ##  [6903]  26  92 318 426 223 130 129 199 164 155 160 234 494 469 298 241 165
    ##  [6920] 128  90  50  26  18   7   6  31  85 233 402 222 138 174 240 206 198
    ##  [6937] 235 302 524 397 253 184 143 113 117  96  70  31  20   8   8  18  57
    ##  [6954] 114 169 236 282 343 322 310 382 359 360 335 220 159 157 144 108  88
    ##  [6971]  71  50  46   6   3  12  21  88 143 289 333 413 415 408 387 405 340
    ##  [6988] 284 193 144  99  94  49  32  15   7   4   6  21  86 285 398 195 129
    ##  [7005] 168 220 198 206 177 310 614 486 237 129 126  79  59  30  11   5   4
    ##  [7022]   5  24  98 314 408 223 128 166 200 198 200 234 285 585 518 338 248
    ##  [7039] 201 170  94  32  12   3   3   5  21  92 299 464 240 131 155 194  82
    ##  [7056]  66  70 213 454 453 331 207 157 135  75  26  11   8   4   4  15  46
    ##  [7073] 137 316 171  62  52  53  74  92 107 207 322 240 219 182 132 111  68
    ##  [7090]  44  16   4   8  10  25  75 205 374 229 130 168 213 228 185 215 308
    ##  [7107] 446 368 204 143  74  36  39  19  18  17   8   1   1   5   7  20  20
    ##  [7124]  12  27  50  30  29  41  22  31  43  39  47  50  54  36  54  43  50
    ##  [7141]  33  11   4  10  22  80 147 178 240 314 345 300 290 320 245 213 153
    ##  [7158]  92  79  71  37  23  13   3   3   6  18  86 227 372 211 105 128 170
    ##  [7175] 152 141 168 272 486 422 238 172 116  85  52  21  11   4   6   8  18
    ##  [7192]  93 254 419 222 135 117 176 167 150 186 298 506 460 313 209 127  96
    ##  [7209]  72  19  10   2   2   4  27  92 251 472 228 133 144 189 161 172 187
    ##  [7226] 266 553 479 282 182 168 106  57  31  15   7   5   4  28  99 292 402
    ##  [7243] 164 110 120 179 169 152 162 267 498 433 252 209 144 137  95  43  17
    ##  [7260]  10   8   7  23  66 245 395 254 134 162 197 194 201 237 290 480 374
    ##  [7277] 196 186 123 115  89  52  62  31   8   8   2  23  34  84 141 227 238
    ##  [7294] 372 355 386 343 335 334 237 204 165 118  82  85  75 115  29   9   6
    ##  [7311]   5  16  28  90 170 262 320 346 314 320 306 347 256 185 151 116  88
    ##  [7328]  45  50  15   8   2   3   6  26  99 311 410 174 123 128 179 189 145
    ##  [7345] 170 295 528 425 300 204 139 108  48  18  11   1   3   4  18  99 323
    ##  [7362] 466 191 129 147 203 196 161 188 289 499 462 278 181 182 108  48  13
    ##  [7379]  10   5   4   4  28  98 300 456 219 105 152 180 167 179 151 281 485
    ##  [7396] 408 304 219 161 114  66  24  11   5  11   3  23 114 272 436 211 117
    ##  [7413] 108 167  96  64  62  73 177 273 211 168 136  89  82  66  19  11   6
    ##  [7430]   8  13  46 121 258 201 200 170 239 236 247 216 238 310 208 199 109
    ##  [7447]  83  91  73  64  54  46  17   7   4   7  27 101 160 232 315 366 353
    ##  [7464] 435 386 361 285 203 171 132 122 118 101  73  71  57  28   6   5  23
    ##  [7481]  43  79 135 241 295 354 339 371 310 319 231 190 157 139  95  89  67
    ##  [7498]  30  11  17   7   5  19 114 322 452 233 111 128 202 203 185 195 315
    ##  [7515] 519 489 302 240 213 113  61  22  10  15   4   7  28 108 319 480 200
    ##  [7532] 138 134 176 153 147 191 271 451 441 286 238 168 128  80  31   5  10
    ##  [7549]   5   4  13  56 137 232  82  36  60  54  58  61  66 123 233 189 111
    ##  [7566]  99  75  46  31  24   5   5   3   4  22  75 168 355 198  81  97 121
    ##  [7583] 124 110 116 195 399 307 217 169 116  84  58  30  10  10   2   5  22
    ##  [7600]  71 216 375 213 115 140 165 177 154 178 209 378 328 190 137 111  97
    ##  [7617]  59  53  35  46  15   8   2  12  41  92 142 200 233 311 363 349 382
    ##  [7634] 287 232 208 182 163 108 106  93  93  84  56  37  12   4   9  23  65
    ##  [7651] 138 229 286 304 369 363 351 306 203 145 157 113 105  40  28  17  12
    ##  [7668]   3   4   5  20  76 246 391 237 135 128 162 134 135 108 108 210 192
    ##  [7685] 137  80 114  78  33  14   6   6   3   7  18  64 121 190 149  52  32
    ##  [7702]  21  26  35  34  62 165 148 111 127  90  87  39  16   8   6   3   5
    ##  [7719]  17  69 156 323 167  76 116 160 174 213 207 173 163 145 122  68  66
    ##  [7736]  62  51  23  24  22   5   2  11   6  40  46  81  98 142 150 175 168
    ##  [7753] 149 114  74  37  28  31  26  22  21  29  14  10   2   7   5  13  35
    ##  [7770]  74  95 142 226 272 283 272 310 261 184 153 134  95  85  58  33  47
    ##  [7787]  29  23  17   4   3  10  17  60  83 141 230 309 325 299 314 275 184
    ##  [7804] 185 146 141  92  65  69  46  44  31  17   4   5   9  23  42 111 189
    ##  [7821] 240 256 313 313 317 323 204 155 139 104  88  66  32  16  13   5   4
    ##  [7838]  34 104 277 407 208 107 118 178 162 143 150 264 468 416 264 218 156
    ##  [7855] 109  46  22  17   5   2   6  22  89 342 449 202  36  11  18  22  36
    ##  [7872]  65 151 354 358 250 184 125  95  53  24   8   5   1   5  22 103 285
    ##  [7889] 478 193 110 122 160 147 112 115 226 380 376 279 190 135  87  50  20
    ##  [7906]  10   9   7   1  24  97 276 477 224  93 102 148 154 150 154 201 418
    ##  [7923] 405 228 190 162 104  73  38  15  10   1   3  23  87 205 445 246 105
    ##  [7940] 160 204 207 170 199 276 411 372 265 190 111 102  95  75  71  50   9
    ##  [7957]   8   6  11  26  63 124 195 275 358 292 343 337 305 232 224 175 127
    ##  [7974] 109 114  85  83  74  62  41  11   4   4  26  55 126 216 254 329 357
    ##  [7991] 323 296 330 245 171 173 124  72  64  45  24  12   8   2   7  25  90
    ##  [8008] 283 404 190  87 123 154 171 133 148 248 446 399 308 205 176 106  62
    ##  [8025]  28  16   3   3   7  20  86 234 414 204  50  32  55  71  75  45  88
    ##  [8042] 262 304 233 110 114  87  53  33  13   6   3   1   3  18  46  86  64
    ##  [8059]  33  50  33  33  25  30  31  52  51  33  26   6  13  16  21   7   4
    ##  [8076]   1   2  12  71 244 429 225 110 106 156 124 137 117 208 331 318 232
    ##  [8093] 175 139  92  61  34  14  12   6   2  20  63 199 385 264 126 136 173
    ##  [8110] 168 187 189 250 362 318 199 150 140 129  94  77  66  60  25   3   6
    ##  [8127]  11  11  73 149 192 236 283 286 293 299 241 185 172 154 124  83  95
    ##  [8144]  66  74  62  52  19   5   6  10  31  72 107 194 197 288 268 242 266
    ##  [8161] 206 163 163 105  72  63  43  35  20   9   4   2   3  21  64 235 385
    ##  [8178] 220 105 115 145 166 111 132 214 359 357 241 165 121  71  45  17   4
    ##  [8195]   2   3   4  20  92 223 400 233 102 124 167 127 134 151 207 398 382
    ##  [8212] 267 161 139 107  59  22   8   2   3   4  26  87 247 467 239 142 115
    ##  [8229] 144 133 151 167 245 396 390 268 175 154 104  51  50  20   7   3   6
    ##  [8246]  24  92 258 457 280 109 135 157 164 132  82 131 324 377 287 221 155
    ##  [8263] 124 114  72  31  23   5   5  26  82 231 459 243 132 144 173 137 128
    ##  [8280] 153 224 365 295 214 152 102 106  75  62  43  58  26   9   9  17  20
    ##  [8297]  59 100 181 209 244 236 237 228 222 165 157 106 106  96  80  69  71
    ##  [8314]  46  41  18   1   5   3  12  32  81 129 188 228 228 219 207 234 136
    ##  [8331] 138 135 104  89  49  37  17  11   3   3   4  21  68 191 397 183 108
    ##  [8348] 127 141 142 137 159 208 378 354 251 206 127 107  60  21   6  11   2
    ##  [8365]   4  15  72 268 432 259 129 129 175 181 138 154 238 431 411 224 147
    ##  [8382] 143 107  53  25  11   4   2   2  28  76 229 405 234  89  56  61  58
    ##  [8399]  51  61 101 229 280 209 167 123 105  54  27  15  11   6   3  16  64
    ##  [8416] 186 343 228 141 145 202 188 159 214 256 356 288 142  33  17  17  11
    ##  [8433]   9  12  13   4   2   8  40  92 182 156 104 174 170 194 200 203 181
    ##  [8450] 140  95  76  50  32  47  25  23  16  26   5   3   4  10  10  27  56
    ##  [8467]  56  71  94 121  85  97  99  68  35  25  24  19  20  17   6   4   2
    ##  [8484]   4   1   1   4   5  23  43  85  66  79  86  91  86  44  30  16  26
    ##  [8501]  19  17  16  11  10   7   2   4   4  19  18  62  70  91 100 140 153
    ##  [8518] 131  97  97  75  65  56  49  27  29  12   6   4   3   3   8  35  80
    ##  [8535] 164 104  60  30  24  20  14  19  46 115 135  90  69  63  32  26  10
    ##  [8552]  12   7   4   9  43 110 217 189  96 111 140 159 120 117 167 250 169
    ##  [8569] 151  79  70  33  39  28  15   3   2   3  10  41 106 210 170 110 122
    ##  [8586] 130 131 142 177 177 240 207 135  96  68  55  45  30  20  12   6   2
    ##  [8603]  10  32  95 205 194 118 171 197 207 245 292 283 242 182 112  91  95
    ##  [8620]  85  73  44  35  28  18  10   1   6  19  49  90 128 217 273 313 300
    ##  [8637] 253 195 129  93  92  71  52  38  31  48  93  75  52   8   5   2   7
    ##  [8654]  14  40  70 138 201 223 267 265 215 111 106 105  83  71  66  29  39
    ##  [8671]  12   7   4   4  14  16  53  68 109 175 202 176 151 168 154 153 126
    ##  [8688]  93  88  66  39  34  13   6   3   2   5  12  85 170 354 153  60  75
    ##  [8705]  74  73  79  77 136 245 224 156 115  68  33  18   9   3   1   1   2
    ##  [8722]  14  59 152 315 180  64  46  84  91  75  90 131 281 289 184 123  90
    ##  [8739]  52  32  14   5   4   4   5  26  78 215 388 227  86  84 120 118 117
    ##  [8756] 119 197 412 383 265 177  97  71  60  25   8   5   4   3  13  70 205
    ##  [8773] 447 241 116 138 191 204 178 222 294 476 419 272 177 154 135 101  79
    ##  [8790]  62  38  20  10   9   7  20  64 130 203 324 376 445 512 449 401 322
    ##  [8807] 297 215 170 140 109 119  91  72  67  32   6   2   2  24  57 125 208
    ##  [8824] 256 351 343 328 330 314 219 181 112 126  91  58  30  15   5   5   3
    ##  [8841]   4  21  88 240 420 197 108  88 103  80  50  69  49 152 164 190 132
    ##  [8858]  79  62  52  14   5   4   4  23  79 222 471 195  83 101 140 178 106
    ##  [8875] 136 240 460 385 281 200 116 114  41  28   7   5   4   2  22  74 256
    ##  [8892] 499 225  97  99  55  65  73  79 118 134  98  93  57  29  49   9   3
    ##  [8909]   3   2   3   2  16  89 220 508 230  90 119 144 183 156 188 281 495
    ##  [8926] 421 330 240 174 133  67  42  12  14   7   3  21  73 177 419 214 103
    ##  [8943] 114 154 154 129 138 233 332 301 206 136 102  65  65  44  50  38  20
    ##  [8960]   3   4   5  24  92  81 149 178 219 220 231 226 206 144 143 102 103
    ##  [8977]  69  66  76  59  42  43  25   6   5   8  17  36  90 126 165 234 224
    ##  [8994] 188 206 205 146 120 110  95  72  60  29  25  20  17   3   8   6  13
    ##  [9011]  33  78  96 126 179 185 228 181 192 180 193 181 144  90  49  43  28
    ##  [9028]  12   2  12   2  13  57 126  93  68  62  75  86 103 132 129 219 462
    ##  [9045] 463 308 180 180  90  61  17  10   2   4   1  29  88 263 489 202  93
    ##  [9062] 114 161 130 105 130 176 346 355 256 165 124  78  38  16   5   3   4
    ##  [9079]   1  19  86 206 423 210 108  99 111 124 119 127 193 385 361 234 191
    ##  [9096] 121  90  56  27  15  11   4   3  19  68 185 425 263  97 123 124 156
    ##  [9113] 115 156 174 349 301 192 142 104  72  38  24  23  26  13   1   2   1
    ##  [9130]  13  25  42  45  63  67  76  95 119 118 106 110  85  67  69  59  52
    ##  [9147]  52  50  47  21   2   3   4  13  32  63 129 133 185 199 182 176 155
    ##  [9164] 108 121  89  74  44  58  37  19   4   2   1   1  19  42 122 272 211
    ##  [9181] 149  62  62  37  61  74 122 276 282 220 169 120  68  37  25   9   8
    ##  [9198]   3   2  26  89 232 490 255 116 153 175 203 173 208 278 515 423 331
    ##  [9215] 229 172 125  99  31  13   6   1   4  33  88 257 513 236 141 157 193
    ##  [9232] 212 160 155 246 470 439 316 235 185 113  66  25  21   7   6   3  28
    ##  [9249]  88 239 530 274 108 111 165 173 128 164 224 413 406 313 224 192 142
    ##  [9266]  91  38  29  13   8   4  24  72 134 220 107 126 105 180 201 176 197
    ##  [9283] 267 422 383 251 159 135 119  86  69  58  41  17   8  10  15  31  91
    ##  [9300] 158 191 296 314 373 362 410 407 290 241 206 120 127 103  85  96  73
    ##  [9317]  51  21   5   7   5  12  70 111 191 255 288 316 280 310 322 234 168
    ##  [9334] 159 111  69  64  25  10   9   7   4   5  26  80 225 493 198  85  99
    ##  [9351] 134 132 115 138 221 438 410 306 181 162  94  52  15   8   2   2   1
    ##  [9368]  22 104 279 512 261 144 139 194 198 160 177 307 559 502 328 238 165
    ##  [9385] 129  63  31   4   7   3   1  18  67 208 520 276 133 143 179 186 192
    ##  [9402] 194 283 539 526 335 264 233 140  97  31  17   9   2   1  19  74 205
    ##  [9419] 445 228  78 105 148 137 141 174 229 450 410 327 213 148 109  61  38
    ##  [9436]  22   6   4   2  17  88 226 441 270 129 164 216 198 170 216 311 488
    ##  [9453] 385 267 163 134 119  77  86  43  44  18   6   4   5  28  90 157 224
    ##  [9470] 270 300 302 276 305 143  67  64  80  77  78  95  70  61  51  63  17
    ##  [9487]   9   4   4  16  62  92 148 182 248 274 243 272 296 306 181  69  50
    ##  [9504]  66 191  42  21   6   3   2  17  72 234 444 277 116 116 150 141 140
    ##  [9521] 171 228 430 437 291 201 154  87  46  23  14   4   2   3  15 100 299
    ##  [9538] 529 289 116 129 172 184 138 146 276 509 518 338 212 190 118  51  23
    ##  [9555]  10   1   1   1  20  98 281 508 253 108 102 138 126 108  54  75 155
    ##  [9572] 167 161 125 129  98  60  26  12   2   4   1  18  76 267 484 238  97
    ##  [9589]  99 158 154 144 134 238 399 417 295 199 177 107  84  43  13  16   5
    ##  [9606]   2  17  64 230 448 275 124 153 176 192 146 191 268 396 359 262 151
    ##  [9623] 122 113  65  53  45  26   9   4   2   9  19  78  90 166 183 221 235
    ##  [9640] 243 124 146 144 108  87  63  48  46  20  21  24  27  14   1   3   2
    ##  [9657]  18  26  63  91 124 140 138 111 155 164  87  96  80  57  36  28  23
    ##  [9674]   9   6   4   1   2  17  72 195 417 205  71  90 118 140 135 155 211
    ##  [9691] 407 405 263 214 147 102  36  14   6   3   3   2  22  90 280 513 262
    ##  [9708]  98 112 172 173 124 145 266 523 406 281 184 109  77  57  22   5   4
    ##  [9725]   3   1  25  95 321 515 231 120 138 163 170 134 171 231 487 468 311
    ##  [9742] 211 178 107  58  23   5   7   1   3  20  87 288 498 202  44  71  45
    ##  [9759]  64  31  53 118 289 353 258 216 145 111  73  34  18  12   1   1  16
    ##  [9776]  71 226 520 283 116 142 198 205 170 221 307 465 377 282 171 129 107
    ##  [9793]  82  86  45  45  18   7   1   9  41 102 138 205 286 369 384 427 499
    ##  [9810] 404 364 246 188 148 114 118  74  74  72  48  15   3   2   5  14  56
    ##  [9827]  93 191 226 255 261 237 253 212 157 167 119 103  36  55  35  41  15
    ##  [9844]  20   8   6  10  44 120 110 143 208 281 312 263 289 280 253 242 172
    ##  [9861] 126  98  62  26  12   7   3   2  15  86 279 464 217  85  86 163 131
    ##  [9878] 124 160 254 432 425 304 227 154 100  47  24   7   6   4   2  30  90
    ##  [9895] 302 576 252 139 171 207 190 177 195 298 529 525 327 274 239 147  62
    ##  [9912]  42  15   9   6   1  32  91 326 560 242 148 182 224 218 192 217 343
    ##  [9929] 610 522 369 269 222 137  85  46  18   5   5   2  26  82 272 469 244
    ##  [9946] 141 163 228 183  31  94 178 256 300 233 167 152 106  86  61  46  38
    ##  [9963]   7   2   3  13  23  93 122 183 210 258 226 216 232 195 159 133 149
    ##  [9980] 105  85  82  91  70  55  42  21   6   5   2  22  59  87 188 227 305
    ##  [9997] 325 331 353 339 287 216 143  88  83  71  64  29   6   4   2   1  16
    ## [10014]  90 280 528 230 104 103 177 182 158 174 286 529 527 334 219 195  92
    ## [10031]  56  10   6   3   6   2  21 101 325 559 232 132 163 183 190 134 166
    ## [10048] 287 530 449 306 204 188 101  65  32   6   4   2  29 104 253 116  38
    ## [10065]  10  10  70  76  56  90 167 253 192 102  97  39  55  33  11  12   6
    ## [10082]   3   5  18 109 304 594 286 133 168 220 211 211 223 361 600 525 328
    ## [10099] 254 197 126  85  46  20   6   3   2  24  92 262 549 314 132 194 234
    ## [10116] 222 205  78  50 128 160 174 100 101  75  23  22  44  38  21   2   2
    ## [10133]   7  16  48  94 168 214 299 339 430 399 467 385 303 225 149 154 134
    ## [10150] 106  72  76  80  26  14   5   5  23  66 118 192 256 327 364 332 334
    ## [10167] 299 263 184 132  88  79  62  26  17   6   4   1   1  18  91 260 428
    ## [10184] 197  86 103 137 120 128 130 210 387 375 226 179 120  78  31   8   6
    ## [10201]   4   1   3  25 102 275 501 233 129 130 138 164 136 166 250 446 457
    ## [10218] 287 187 148 106  54  15   5   4   3   3  18 108 344 566 264 147 161
    ## [10235] 215 205 175 209 288 612 544 361 260 188 159  62  46  21  11   3   2
    ## [10252]  29 109 334 585 301 138 204 251 233 203 185 342 597 590 416 302 233
    ## [10269] 153  94  54  26  11   7   1  29  85 268 501 284 159 187 201 224 178
    ## [10286] 238 343 566 470 244 159 141 107  86  77  51  42  16   4  10  12  40
    ## [10303] 111 148 223 274 329 357 379 406 390 354 297 184 121 110 100  83  69
    ## [10320]  61  66  22   7  10  17  72  90 218 299 410 464 501 487 509 498 389
    ## [10337] 258 171 147  94  52  24  10   9   2   3  16  88 268 564 281 137 150
    ## [10354] 221 250 221 233 332 644 712 446 286 205 133  63  26  16   1   2   1
    ## [10371]  24 113 308 593 342 143 219 244 233 212 264 365 676 734 479 351 244
    ## [10388] 170  87  39  27   2   4   5  27 121 368 662 351 188 220 267 254 224
    ## [10405] 283 356 782 749 472 330 288 202  91  60  23  19   7  14  26 117 381
    ## [10422] 623 315 164 211 265 273 258 287 426 713 746 425 330 243 169  97  57
    ## [10439]  26   4   3   3  32  99 278 562 312 180 207 294 260 202 107 129 258
    ## [10456] 408 296 235 152 148 126 100  69  42  26   2   8  30  86 218 321 443
    ## [10473] 585 651 686 690 679 685 648 560 417 271 223 211 185 107 113  56  18
    ## [10490]  14   6  11  42  96 178 351 368 503 544 521 554 541 541 459 352 198
    ## [10507] 173 100  46  23  15   8   2   3  31 120 354 579 331 184 201 282 263
    ## [10524] 260 286 385 721 801 549 330 223 148  54  29  15  10   3   6  20 100
    ## [10541] 170 516 325 163 230 261 284 248 244 431 750 801 555 377 277 188  90
    ## [10558]  33  26  10   8   4  39 149 372 535 293 154 192 229 303 225 243 378
    ## [10575] 729 779 582 374 251 197 125  41  30   6   7   6  34 136 361 649 318
    ## [10592] 142 219 272 298 257 304 425 810 801 586 424 374 220 151  95  50  32
    ## [10609]  10   3  34 116 346 662 380 275 318 471 428 432 472 589 957 830 686
    ## [10626] 445 284 271 176 156 128  69  32   7   4  28  70 154 249 345 273 183
    ## [10643] 196 223 290 177 115 137 133 111 104  83 105  80  89  35  18   8   6
    ## [10660]  18  39  89 118 250 323 360 367 414 421 492 419 491 375 259 149 111
    ## [10677]  65  33  28  20   1   4  36 117 370 657 282 153 178 279 301 217 223
    ## [10694] 305 664 684 458 221 186  93  48  10   5   7   4   3  17 101 286 581
    ## [10711] 267 138 175 220 211 184 200 305 614 644 417 294 221 134  64  30   8
    ## [10728]   3   5   1  40 115 358 658 314 173 222 268 273 221 199 221 604 641
    ## [10745] 456 362 261 172  93  48  33   6   5   9  32 117 367 654 321 166 220
    ## [10762] 292 273 238 307 388 703 681 468 335 224 152  94  59  27   7   7   2
    ## [10779]  26  86 289 593 321 187 201 270 242 229 280 367 606 560 395 209 220
    ## [10796] 153 123  92  82  69  31   8   5  23  44 145 223 313 463 621 638 551
    ## [10813] 605 535 543 341 284 208 143 156 112  67  62  81  25  12  18  97  84
    ## [10830] 125 275 360 451 445 464 561 573 511 555 432 346 232 134  68  63  17
    ## [10847]   5   5   4  24 110 306 589 317 204 209 281 281 268 324 395 729 618
    ## [10864] 494 329 238 123  66  33  11   5   3   1  27 100 341 604 408 219 258
    ## [10881] 331 345 311 316 485 757 800 558 379 243 158  79  41  11   5   3   1
    ## [10898]  27 126 366 684 363 167 213 283 251 258 268 390 744 759 494 372 298
    ## [10915] 181 131  45  13  18   5   1  29 126 354 638 351 246 268 276 291 287
    ## [10932] 269 402 822 698 487 312 234 149 136  61  38  11   8   5  23  80 246
    ## [10949] 508 376 238 291 337 411 490 537 536 655 484 412 252 190 158 113  94
    ## [10966]  69  36  28   7   2  19  54 123 276 363 495 590 643 578 626 615 567
    ## [10983] 517 413 208 234 182 118  85  64  32  26   7  12  28  37  84 179 299
    ## [11000] 404 457 483 503 486 476 411 310 276 211 130 118  51  37  16   6   3
    ## [11017]   5  32 111 331 617 286 178 223 280 281 218 255 391 632 646 421 276
    ## [11034] 165 108  67  26   9   2   2   2  24 103 384 692 319 181 239 259 283
    ## [11051] 224 203 362 744 704 479 282 206 127  62  30  12   2   2  27 106 321
    ## [11068] 595 302 153 180 182 163 135 202 307 579 585 384 235 165 134  61  23
    ## [11085]   9   4   5   1  21  96 306 624 287 141 224 259 203 224 211 353 656
    ## [11102] 610 433 280 187 158  94  55  30  11  11   5  21  84 296 563 330 192
    ## [11119] 233 322 358 348 352 463 738 671 427 286 189 215 198  96  70  56  32
    ## [11136]  14   7  21  64 160 265 406 564 678 678 660 658 582 560 481 459 321
    ## [11153] 220 212 196 124 106  74  45  12  10  16  46  97 251 421 528 635 681
    ## [11170] 628 617 616 623 479 379 291 229 134  90  41  28  14   8   6  38 133
    ## [11187] 387 597 297 224 251 271 263 288 275 358 712 676 522 375 271 214 121
    ## [11204]  43  11   4   6   9  30 144 461 673 400 217 239 244 239 257 308 370
    ## [11221] 781 775 537 402 297 162  82  38  14   9   6   7  34 134 418 576 265
    ## [11238] 147 119 110  64  51  98 235 488 505 368 260 189 162  70  25  13   6
    ## [11255]   4   4  30 120 406 677 301 189 229 264 238 237 262 387 748 776 586
    ## [11272] 404 267 248 144  64  25  21  10   3  25 113 359 700 359 237 254 362
    ## [11289] 381 394 407 580 819 668 492 341 287 202 187 142  88  81  29   9   1
    ## [11306]  31  76 201 331 482 608 640 649 609 628 639 568 416  54  39 102 110
    ## [11323]  91 117  73  56  40   7   6  12  24  51  79  68  77  61  52  53  38
    ## [11340]  36  27  39  35  35  11  13  17   6   2   4   1   3  14  45 207 482
    ## [11357] 272 100 101 129 107 114 112 103 232 379 298 203 152  89  59  13   7
    ## [11374]   3   2   3  22 120 390 611 308 162 184 224 224 216 236 362 691 632
    ## [11391] 482 285 226 157  73  36  25   7   3   6  24 106 389 654 321 182 219
    ## [11408] 269 228 223 229 376 732 709 543 380 239 192 104  43  23  13   6   3
    ## [11425]  17 127 225 362 201 139 151 204 197 169 204 318 614 664 464 337 250
    ## [11442] 183 112  73  35  22   3   6  19 120 315 592 334 236 233 307 306 300
    ## [11459] 324 440 702 654 441 289 208 158 116 103  67  57  26   2   5  28  55
    ## [11476] 158 214 291 358 381 389 440 408 376 322  90 139 102  84  83  42  36
    ## [11493]  42  40  16   4   6   7  27 107 185 380 411 584 595 653 603 587 522
    ## [11510] 487 375 243 204 108  82  48  24   8   4   2  20 123 349 596 268 157
    ## [11527] 163 225 247 218 231 347 683 664 471 309 221 134  60  35  21   8   3
    ## [11544]   8  17  26 169 557 349 174 229 269 249 204 234 354 681 743 487 335
    ## [11561] 282 211  95  47  15  16   6   4  33 120 358 666 315 175 184 245 228
    ## [11578] 240 256 367 729 813 504 338 239 173  98  90  24  17   6   2  21 136
    ## [11595] 384 627 304 168 201 301 265 204 283 370 704 706 522 420 299 246 121
    ## [11612]  89  48  11  14   4  24 135 356 618 294 199 258 171 278 242 343 440
    ## [11629] 643 641 452 313 235 232 256 153 107  75  32   9  22  42  87 197 284
    ## [11646] 432 575 550 524 428 541 540 502 434 405 309 269 217 149 134 121  93
    ## [11663]  28  16  11  17  36 114 191 333 414 558 565 518 571 544 511 463 383
    ## [11680] 276 203 157 102  34   9   6   5   2  23 142 385 639 350 190 213 263
    ## [11697] 272 239 262 388 769 680 546 323 247 173 113  38   9  16   6   5  20
    ## [11714] 165 463 641 295 157 217 263 219  71 134 389 717 710 458 302 223 147
    ## [11731]  63  35  14   1   2   5  28 126 418 622 325 153 201 280 266 243 259
    ## [11748] 357 705 372  98  51  34  67  55  33  11   3   3   2  24 139 401 630
    ## [11765] 361 154 256 272 318 247 271 421 732 770 553 374 230 232 135  64  46
    ## [11782]  31  13   9  26 135 351 620 322 210 291 348 363 334 439 525 779 596
    ## [11799] 503 341 270 234 180 136  93  56  34   9  13  33  67 178 335 411 522
    ## [11816] 559 659 615 590 591 576 546 439 287 230 274 176  98  94  82  28   9
    ## [11833]  12  21  60 156 222 350 428 531 602 552 499 564 478 389 339 230 169
    ## [11850] 131  74  35  11   2   2   6  26  65  75 156 110  59  79 185 176 171
    ## [11867] 196 111 229 300 304 189 152 129  75  32  19   4   6   5   8  24  92
    ## [11884] 409 355 171 224 264 239 184 256 374 678 733 512 263 109  83  71  39
    ## [11901]  21  19   6   4  38 158 440 650 347 192 267 330 350 263 274 446 873
    ## [11918] 846 590 459 393 286 133  79  28  16   3  16  35 159 474 634 346 205
    ## [11935] 279 348 327 270 316 430 852 868 537 446 299 251 166  70  49  21  14
    ## [11952]  11  34 182 515 745 361 255 334 360 395 350 391 568 812 669 483 337
    ## [11969] 258 251 174 148 104  69  33  14  12  50  95 195 292 452 586 642 704
    ## [11986] 730 672 642 626 645 432 315 259 338 239 170 130  98  66  16   8  25
    ## [12003]  90 171 313 432 581 637 607 543 502 577 549 474 402 274 233 145  86
    ## [12020]  40  15  11   2   7  15  79 153 270 186  99 106 170 184 180 225 315
    ## [12037] 508 530 448 336 241 142  97  36  27  11   5   7  17 137 324 590 333
    ## [12054] 200 189 254 243 222 245 386 785 785 364 373 261 174 105  47  32  12
    ## [12071]   5   3  35 160 468 719 297 191 197 248 270 248 257 278 285 327 376
    ## [12088] 295 245 159 106  56  30  16   4   5  30 156 415 692 301 185 221 289
    ## [12105] 302 252 291 390 798 752 492 398 285 243 167  73  42  23  14   9  30
    ## [12122] 113 359 583 340 232 283 321 333 367 466 584 653 522 404 350 255 212
    ## [12139] 166 116  82  64  31   2  10  31  54 141 289 437 520 529 539 564 513
    ## [12156] 504 462 415 373 255 251 204 150 137  78  61  36  11   7  22  52 151
    ## [12173] 270 424 519 570 498 547 535 501 522 518 442 392 143  59  96  65  59
    ## [12190]  48  18   6   9  20  67 155 263 364 476 525 556 465 460 501 405 383
    ## [12207] 395 317 247 157  82  45  14   8   3   5  33 154 450 634 257 183 197
    ## [12224] 237 239 238 256 393 781 710 526 204  71  55  50  36   4   5   5   6
    ## [12241]  41 144 483 671 305 177 208 253 289 252 275 398 839 796 556 431 304
    ## [12258] 217 160  61  26  14   8   8  33 178 507 693 311 220 218 292 293 254
    ## [12275] 290 487 827 785 591 479 359 264 140  86  34  16   4   6  45 141 402
    ## [12292] 694 298 204 235 310 316 254 258 222 227 110  45  39  85  23  73  86
    ## [12309]  76  41  26  13  12  36  86 210 332 493 536 668 679 647 702 644 586
    ## [12326] 512 554 399 316 228 238 169 121  89  50  13  12  22  35 140 300 404
    ## [12343] 511 684 686 678 644 662 578 496 453 314 246 226 108  49  14  11   5
    ## [12360]   8  35 139 517 665 282 187 239 282 274 270 292 448 834 822 645 461
    ## [12377] 265 168  86  34  20   8   5   5  36 184 569 710 335 176 227 247 267
    ## [12394] 276 272 473 850 790 513 415 283 193 113  49  27  11   8  10  38 172
    ## [12411] 547 668 303 203 226 315 303 302 300 452 724 782 538 408 298 248 123
    ## [12428]  59  22  12   5   6  34 170 526 681 337 210 237 336 315 265 333 466
    ## [12445] 869 813 602 478 313 228 177  77  29  19   8  10  29 139 428 700 382
    ## [12462] 249 264 369 416 373 397 523 793 723 555 408 339 291 215 206 123  89
    ## [12479]  40   8  19  80  87 244 340 466 569 531 585 593 555 534 495 451 360
    ## [12496] 363 316 214 230 158 116  73  29  10  13  30  62 181 266 413 499 502
    ## [12513] 492 500 475 508 521 490 375 363 253 159 110  41  24  17   6   7  37
    ## [12530] 144 503 651 316 147 202 261 248 259 259 427 800 831 596 394 250 164
    ## [12547]  80  28  16   6  12   4  29 136 303 613 285 125  65  95  59  60 116
    ## [12564] 239 681 653 491 386 274 207  89  34  28   4   8  10  40 194 505 713
    ## [12581] 352 198 246 334 255 320 281 392 857 744 671 448 396 238 153  48  21
    ## [12598]   9   5   6  40 181 506 719 357 179 228 265 284 298 324 438 867 823
    ## [12615] 579 435 337 242 172  94  51  15   5  14  33 151 430 653 366 216 286
    ## [12632] 403 393 376 377 558 823 693 467 385 333 321 222 137  95  67  27   8
    ## [12649]  23  47  78 204 367 435 566 603 617 573 583 542 593 571 461 352 290
    ## [12666] 280 183 148  88  74  28  18  17  23  48 119 274 436 546 615 614 582
    ## [12683] 463 580 593 513 390 302 246 153 108  40  14   9   4   9  23  37 145
    ## [12700] 474 250  91 121 168 225 219 237 332 723 642 463 362 273 164  74  35
    ## [12717]  14  15   8  10  37 161 480 673 328 180 230 292 272 227 259 334 811
    ## [12734] 795 514 458 276 291 125  53  27   6   8   7  39 165 464 643 316 167
    ## [12751] 188 180 211 190 179 335 691 672 539 407 344 257 123  69  18  18  11
    ## [12768]  12  40 150 463 590 256 150 161 215 172 176 194 320 615 640 499 436
    ## [12785] 311 243 146  93  31  12   7  10  38 122 359 613 333 185 236 254 300
    ## [12802] 263 284 380 646 505 362 202 217 197 174 116 123  93  42  19  12  47
    ## [12819]  78 167 332 494 516 602 564 609 483 513 530 481 425 386 322 279 225
    ## [12836] 163 127 114  60  13   8  25  71 141 232 452 506 556 585 551 487 461
    ## [12853] 532 534 423 332 253 178  87  68  31  14   5   4  43 158 485 608 268
    ## [12870] 184 181 270 260 251 233 414 833 791 582 475 331 187 103  37  30   9
    ## [12887]   9  10  39 188 552 656 328 190 235 308 306 267 317 416 900 824 612
    ## [12904] 505 328 245 131  54  23  14   8  11  36 200 527 687 282 172 209 322
    ## [12921] 321 270 249 392 843 804 643 445 367 297 159  54  25  13   7  11  47
    ## [12938] 193 508 617 351 184 211 276 236 254 219 387 705 697 571 470 398 274
    ## [12955] 171 114  42  18   8  10  45 163 380 624 300 171 188 226 239 233 230
    ## [12972] 351 539 464 363 308 236 163  48  69  58  61  23  18  10  42  92 194
    ## [12989] 275 404 452 471 428 409 404 373 362 339 374 292 213 172 152 149  93
    ## [13006]  90  33   4  10  27  50 142 219 366 377 433 420 444 345 313 413 370
    ## [13023] 382 332 258 151 110  43  17  15   5  12  40 136 398 568 295 174 201
    ## [13040] 257 236 230 234 365 747 730 581 395 258 188 102  53  14  10   2   6
    ## [13057]  35 154 483 646 272 209 217 272 283 310 401 527 722 627 497 376 196
    ## [13074] 177 171 159 123  93  32  16  19  32  72 153 293 447 485 494 499 547
    ## [13091] 454 389 414 430 432 551 584 502 183  88  30  18   7   7  29 133 343
    ## [13108] 550 293 186 244 276 310 241 254 433 689 607 503 321 323 225 131 102
    ## [13125]  29  17   7   6  35 131 353 585 304 210 233 331 293 275 285 369 576
    ## [13142] 560 418 336 300 288 164 150  71  58  31  16  14  31  71 152 256 325
    ## [13159] 367 342 360 308 306 294 279 277 287 276 201 220 148 147 124  70  35
    ## [13176]  13   6  21  52 130 242 319 389 376 337 312 298 291 270 253 250 285
    ## [13193] 207 122 123  39  18   5   5   2  28  92 230 455 363 175 216 266 255
    ## [13210] 242 280 413 849 872 631 447 331 224 131  69  15  16   3   4  42 187
    ## [13227] 520 649 331 190 249 272 280 242 265 435 872 819 414  84  90 139 103
    ## [13244]  44  14   5   5   7  41 185 497 674 328 195 229 292 273 241 276 411
    ## [13261] 830 814 633 483 391 244 152  56  21   9  10   5  44 178 512 702 327
    ## [13278] 214 215 292 277 209 271 439 795 825 629 514 373 318 211  95  59  17
    ## [13295]   9  11  34 127 380 713 414 268 288 379 374 369 353 492 835 631 488
    ## [13312] 389 299 263 212 191 118 101  40   9   5  35  48 151 282 401 439 499
    ## [13329] 591 632 560 566 458 351 431 356 269 225 211 187 168 115  57  19   9
    ## [13346]  15  47 133 221 382 451 489 480 476 489 495 436 312 312 294 223 130
    ## [13363]  91  43  11  15   6  12  50 140 483 667 348 186 182 237 239 212 237
    ## [13380] 386 755 794 595 454 328 284 166  53  25   7   3   7  38 213 526 661
    ## [13397] 320 159 206 254 221 214 244 381 770 772 547 427 333 244 161  53  16
    ## [13414]   8   4   6  46 162 495 679 297 138 183 202 203 170 194 224 370 643
    ## [13431] 516 366 319 288 131  66  25   7   5   8  40 152 473 657 318 198 216
    ## [13448] 274 254 197 292 358 771 777 533 505 332  68  65  34  11  19  36   9
    ## [13465]  42 126 360 691 373 233 212 274 156 229 216 436 715 622 402 251 122
    ## [13482] 135 166 103  72  60  25  16  17  15  37 112 207 268 305 457 354 343
    ## [13499] 380 326 241 177 238 214 163 180 149 113 130 122  43  16   6  19  38
    ## [13516] 153 258 379 505 504 626 616 624 628 556 515 522 378 345 191 123  47
    ## [13533]  27  17   2   8  51 157 421 738 345 200 198 314 240 271 281 457 847
    ## [13550] 741 582 403 296 209 114  64  17  16   6   6  50 190 516 743 339 218
    ## [13567] 201 229 273 244 285 455 869 877 632 523 378 269 192  66  28  12  12
    ## [13584]   4  59 190 507 788 342 231 281 310 312 247 317 484 913 891 698 567
    ## [13601] 397 278 239  92  29  21   8   4  42 181 495 729 344 222 212 279 242
    ## [13618] 213 238 374 738 688 614 446 271 234 145  97  54  26  12   8  37 139
    ## [13635] 410 676 363 231 251 299 314 326 323 499 699 622 516 354 260 199 189
    ## [13652] 191  80  90  43  11  14  50  76 163 337 395 457 487 498 494 497 487
    ## [13669] 479 481 338 233 300 276 208 188 158 112  55  13  17  18  40 127 226
    ## [13686] 381 509 511 483 522 501 521 531 419 443 303 271 148 100  58  19   8
    ## [13703]   3   5  47 161 469 751 293 189 215 278 226 254 236 425 827 812 665
    ## [13720] 462 322 230 150  38  21   8   6   6  45 194 513 760 333 227 224 248
    ## [13737] 288 260 144 402 820 857 614 452 352 257 147  47  33  13   7   4  49
    ## [13754] 185 487 681 350 236 234 284 280 263 295 479 837 891 652 513 320 288
    ## [13771] 152  63  42  11   6   9  41 183 473 739 343 181 226 286 310 225 270
    ## [13788] 406 865 767 607 427 342 262 177  71  43  14   6  12  37 163 421 668
    ## [13805] 396 249 263 338 345 331 380 492 741 671 469 389 278 202 196 161 122
    ## [13822]  84  30  13  17  49  67 181 278 396 470 547 527 472 489 450 492 490
    ## [13839] 397 312 289 269 222 153  87  73  41  14  19  19  68 108 229 364 437
    ## [13856] 491 523 526 450 421 382 229 185 193 208 157  87  33  11   5   5  11
    ## [13873]  25 141 407 605 276 213 260 285 289 287 274 451 858 843 640 457 317
    ## [13890] 207 113  47  18  13   6   9  36 179 502 705 327 250 214 283 253 261
    ## [13907] 306 445 868 814 610 448 317 224 138  58  23   6   7   7  43 173 482
    ## [13924] 737 341 214 239 280 313 236 278 441 858 862 686 500 381 233 136  67
    ## [13941]  28  14   6  10  41 167 475 698 353 205 260 277 281 247 267 417 810
    ## [13958] 811 623 478 336 259 156  85  41  20   9   7  33  44 133 119 220 193
    ## [13975] 251 315 290 307 324 467 730 640 492 370 286 218 192 180 116  85  21
    ## [13992]  10  11  23  62 162 271 407 499 546 569 538 562 531 512 300 275 160
    ## [14009] 128 168 163 120 113  86  48  10  10  16  39 119 217 328 449 505 543
    ## [14026] 579 577 513 505 491 465 300 220 181 110  47  14   9   6  11  36 159
    ## [14043] 436 673 305 199 245 276 254 248 274 464 818 812 555 432 290 192 128
    ## [14060]  60  27  11   3   5  36  64 179 618 402 208 196 300 286 255 283 458
    ## [14077] 812 854 627 436 308 189 167  60  37   8  11   4  39 172 492 682 367
    ## [14094] 233 235 307 294 255 266 431 851 848 649 427 315 226 138  76  21  15
    ## [14111]   4   5  37 165 464 682 337 199 274 343 300 248 260 419 897 832 677
    ## [14128] 514 356 254 226  88  54  19   6   9  36 157 379 668 378 231 303 365
    ## [14145] 357 328 383 488 791 669 491 359 255 213 121 105  92  43  30  13   9
    ## [14162]  27  64 189 292 480 536 654 644 598 596 641 635 554 488 341 338 261
    ## [14179] 235 187 131 119  55  26  11  20  41 124 270 425 272 298 162 149 276
    ## [14196] 356 343 377 341 274 190  56  46  26  10   5   3   7  37 161 442 655
    ## [14213] 357 193 241 282 291 275 309 460 893 815 299 261 245 161 102  59  33
    ## [14230]   8   3   4  34 169 519 723 328 178 256 305 331 302 302 467 878 625
    ## [14247] 476 358 280 232 136  55  21  17   7   8  40 187 553 740 376 225 267
    ## [14264] 312 316 266 316 460 783 683 580 391 351 281 140  60  25  20   9   6
    ## [14281]  37 192 481 707 412 233 278 332 346 305 305 465 820 941 633 474 329
    ## [14298] 198 157 111  42  16   8   8  38 151 425 744 389 262 281 371 351 338
    ## [14315] 350 515 812 736 536 363 306 251 178 135 115  79  38  12  14  30 102
    ## [14332] 162 288 442 457 584 517 482 233 434 403 383 354 241 208 182 158 134
    ## [14349] 134 116  53  16  16  21  34 133 263 385 482 626 541 377 391 173 233
    ## [14366] 264 292 211 166 114  80  32   7   8   4  12  36 145 433 699 342 206
    ## [14383] 229 283 288 246 282 444 845 834 573 416 278 161 114  38  21  15   4
    ## [14400]   8  42 147 269 693 395 204 218 297 288 251 317 480 864 818 597 435
    ## [14417] 314 216 109  47  14   6   9  10  40 181 554 808 348 178 253 360 300
    ## [14434] 301 338 493 870 812 643 427 313 235 157  65  23  17   5   8  35 163
    ## [14451] 532 754 396 234 276 294 290 273 338 482 844 853 610 471 307 291 152
    ## [14468]  82  32  20   5   7  29 108 421 690 398 256 309 410 368 436 526 528
    ## [14485] 617 546 452 356 303 277 174 168  79  69  35  12  22  36  66 162 237
    ## [14502] 417 492 471 504 514 563 462 463 442 392 207 104  78 145  99  73  84
    ## [14519]  42  22  12  15  42 129 232 388 489 545 493 457 355 474 503 500 292
    ## [14536] 104 171 166 123 104  55  40  28  11   4  15  43 127 229 359 459 543
    ## [14553] 566 512 449 500 498 482 374 261 184 121  70  29  24   5  10   8  38
    ## [14570] 172 493 632 323 172 198 287 275 274 292 458 856 839 552 385 268 179
    ## [14587]  95  37  13   9   4   5  40 210 500 725 290 235 217 266 234 211 245
    ## [14604] 430 863 839 662 412 296 219 150  65  18   8   9   8  38 200 482 646
    ## [14621] 123  49  62 137 191 216 270 416 808 835 558 406 272 217 169  96  29
    ## [14638]  11   9   8  40 140 486 719 344 224 296 323 354 253 331 470 772 792
    ## [14655] 568 391 301 299 248 150 117  77  43  11  15  34  82 230 348 422 578
    ## [14672] 694 668 626 512 114 171 167 215 194 181 194 133 124 124  96  46  28
    ## [14689]  55  35  70 170 299 495 585 757 729 647 696 701 671 560 496 356 206
    ## [14706] 189  92  41  18  17  12   5  40 180 447 730 361 210 236 332 323 340
    ## [14723] 346 488 871 968 633 390 285 159  93  32  10   6   8   9  44 211 596
    ## [14740] 750 358 196 247 316 328 255 326 536 970 877 596 461 310 216 109  49
    ## [14757]  21  11   7   5  48 205 557 770 328 205 232 352 323 278 318 509 925
    ## [14774] 977 635 470 306 212 127  57  28   9  11   6  57 195 571 758 338 212
    ## [14791] 254 297 283 274 352 491 884 852 674 463 317 251 170  87  41  30  14
    ## [14808]  12  41 152 454 766 347 246 280 409 408 371 367 563 894 808 579 404
    ## [14825] 312 229 195 207 109  93  37   6  16  33  73 212 342 442 627 706 704
    ## [14842] 715 654 783 729 614 478 330 296 279 229 151 117  89  48   8  13  35
    ## [14859]  71 128 296 490 559 656 694 635 557 596 570 481 403 324 191 127  94
    ## [14876]  45  21  13   7  11  45 164 492 683 299 202 252 295 343 332 338 453
    ## [14893] 842 774 486 340 233 129  70  13   5   4   6   7  48 167 356 672 293
    ## [14910] 160 151  88 114 143  36 141 338 281 324 290 207 151  78  26  12   3
    ## [14927]   5  10  56 172 545 797 362 192 247 313 289 286 311 466 886 892 611
    ## [14944] 409 349 229 123  58  18  11   6   6  56 170 534 790 355 190 243 290
    ## [14961] 336 298 291 457 976 900 603 417 274 287 154  66  49  14  12  10  49
    ## [14978] 165 503 757 383 225 321 389 421 412 423 567 846 805 588 369 312 295
    ## [14995] 186 172 124  83  45  15  17  41  77 218 377 474 626 688 707 654 750
    ## [15012] 680 646 598 407 325 261 239 171 180 142  95  54  10   6  16  57 175
    ## [15029] 276 489 570 776 702 666 640 691 723 540 413 252 196 138 100  64  18
    ## [15046]   8   5   8  37 168 530 750 328 198 238 342 328 303 300 516 898 853
    ## [15063] 590 432 256 172  94  64  13  14   5   9  47 194 556 805 409 170 218
    ## [15080] 302 305 293 303 495 967 822 539 425 297 188  98  40  29  17  10  14
    ## [15097]  36 182 532 838 334 185 227 316 340 277 300 468 953 884 627 434 360
    ## [15114] 215 115  65  21  16   7   8  66 169 558 794 335 208 294 335 326 258
    ## [15131] 296 472 905 899 559 457 253  47  45  17  13  10   7  12  38 138 394
    ## [15148] 705 426 245 361 380 410 399 392 502 808 667 508 336 237 220 190 149
    ## [15165] 101  92  29  14  15  37  73 211 332 472 663 682 686 750 727 722 712
    ## [15182] 594 470 315 292 221 196 147 116  92  58   7  10  22  57 144 285 479
    ## [15199] 591 680 594 612 649 648 575 401 257 194 123  91  57  45  18  12   7
    ## [15216]  10  36 155 483 784 340 179 272 323 305 244 329 459 856 613 516 262
    ## [15233] 218 207 105  31  11   2   5  10  43 179 260 134  86  45  99 104 154
    ## [15250] 163 209 374 715 687 395 306 289 240  98  52  19   9   7  11  34 177
    ## [15267] 515 809 362 175 230 358 279 251 319 478 917 810 593 497 209 241 220
    ## [15284]  76  25  13  11  10  39 133 391 738 359 177 214 288 294 267 325 497
    ## [15301] 901 887 534 441 321 232 155 102  46  11  16  11  41 137 428 785 384
    ## [15318] 260 304 468 424 400 470 634 900 761 500 372 267 264 171 191 141  75
    ## [15335]  56  11  10  30  84 206 395 539 647 743 710 576 620 659 610 495 341
    ## [15352] 247 215 185 179 127 109  74  22  11  10  23  35  62  86 185 336 392
    ## [15369] 320 314 306 333 157 106 114 116 113  79  80  51  37  15  10   7  19
    ## [15386]  59 154 397 332 298 308 341 405 420 370 377 497 456 371 227 151 107
    ## [15403]  69  31  13   7   3   7  19 143 362 713 382 164 193 260 238 244 254
    ## [15420] 424 806 784 514 360 225 162  84  33   8   8   6   6  40 175 507 839
    ## [15437] 366 217 291 390 297 271 319 566 948 844 566 392 269 197 136  42  11
    ## [15454]  11   3  10  42 159 478 798 355 204 251 326 291 348 460 481 827 692
    ## [15471] 743 415 260 177 186  68  37  16   6   8  35 143 392 745 400 275 313
    ## [15488] 387 381 371 455 520 837 642 493 308 190 160 100 249 146  67  15  11
    ## [15505]  11  28  70 174 266 347 527 573 670 597 615 637 528 473 332 255 204
    ## [15522] 189 125 111 112  66  28  12  11  24  51 132 223 428 494 562 619 617
    ## [15539] 568 581 539 453 336 246 196 125 105  48  29   6   4  10  40 147 476
    ## [15556] 737 359 168 263 284 146 185 260 447 766 592 239 256 206 126  81  48
    ## [15573]  13   3   2   7  52 172 525 835 355 222 228 325 328 308 346 446 943
    ## [15590] 838 531 432 195 181 199  49  17  16   7   4  41 178 464 817 382 228
    ## [15607] 234 332 310 270 301 466 888 884 516 414 329 215  99  57  14  14   5
    ## [15624]   5  42 153 508 834 387 218 268 377 332 285 353 450 890 788 513 387
    ## [15641] 283 229 117  56  16  10   5   6  36 131 154 467 389 224 252 332 375
    ## [15658] 365 395 565 425 233 232 229 206 190 131 123  95  78  29  18  13  31
    ## [15675]  65 193 363 423 612 703 714 711 711 691 731 521 345 259 296 202 163
    ## [15692] 154 117 132  41  21  10  26  56 143 269 443 547 675 626 640 623 591
    ## [15709] 491 413 266 195 166 113  66  31  11   5   6   7  46 144 452 728 360
    ## [15726] 212 211 283 315 282 296 524 922 786 514 403 174 183 163  37  17   7
    ## [15743]   1   7  50 158 531 761 334 221 253 378 303 296 346 541 938 826 482
    ## [15760] 379 295 194 111  46  27   6   4   6  53 173 526 801 373 171 236 316
    ## [15777] 306 301 308 532 963 858 572 441 313 238 123  76  28  18   8   5  55
    ## [15794] 171 495 779 343 225 238 319 313 285 305 499 886 809 542 347 271 207
    ## [15811] 135  71  36  19  13   5  50 144 421 734 403 197 290 392 337 351 448
    ## [15828] 582 817 665 471 311 292 217 178 142 146  90  26  13   9  31  86 213
    ## [15845] 328 479 585 668 760 750 711 612 618 456 300 296 209 157 167 117 133
    ## [15862] 116  79  20  28  39  65 173 330 434 462 491 391 402 301 293 225 154
    ## [15879]  54  55  46  37  14  22 116 126 124  98 124 143 115  81  64  66  39
    ## [15896]  16   8   7   3   5  24 116 337 621 297 168 189 241 213 224 230 424
    ## [15913] 723 584 410 268 192 165 101  60  30  20  15  10  40 148 420 680 331
    ## [15930] 146 170 221 248 185 214 344 689 678 452 296 267 202 120  50  24  10
    ## [15947]   3   7  26 122 357 687 382 204 221 292 294 262 306 421 638 522 374
    ## [15964] 221 156 149 119 108  89  46  28  16   9  21  59 142 219 328 340 394
    ## [15981] 449 455 448 400 410 297 239 181 166 146 148 102 165  37  11   9   5
    ## [15998]  16  50 149 217 360 445 504 493 509 449 435 327 237 227 141  87  79
    ## [16015]  53  23   8   6   4  16  39 139 461 648 257 142 150 244 222 196 231
    ## [16032] 360 638 542 372 241 152 115  53  19   8   2   4   7  41 149 387 588
    ## [16049] 363 230 216 248 278 240 272 402 627 497 371 228 151 153 205 283  92
    ## [16066]  74  11   9  27 118 319 601 368 178 147 212 181 161 198 273 517 465
    ## [16083] 319 256 126  60  40  15  12   6  10  35 124 423 668 275 161 171 235
    ## [16100] 228 182 233 339 592 507 373 245 233 161  87  55  20  12   9  14  27
    ## [16117] 131 341 693 327 197 251 262 299 274 356 434 654 496 361 255 199 163
    ## [16134] 162 122  76  44  20   6  10  16  71 146 259 350 466 606 651 627 567
    ## [16151] 605 501 359 283 192 220 193 146 124 108  70  48  11  12  19  68 119
    ## [16168] 273 405 504 585 686 654 724 653 493 405 277 200 171 134 109  49  30
    ## [16185]  20   4   4  24  71 264 540 397 266 367 419 420 444 382 434 596 551
    ## [16202] 353 304 155 112  63  23   5  11   5   9  18  50 107 211 110  94 155
    ## [16219] 196 218 203 217 310 608 559 356 223 187 135  84  24  12   1   5   6
    ## [16236]  39 146 415 691 332 171 151 240 214 181 260 321 583 567 396 267 213
    ## [16253] 173  87  34  16   7   2   5  37 147 410 646 323 153 166 213 217 201
    ## [16270] 217 342 563 573 386 285 224 153 125  65  21   9   6   5  36 130 367
    ## [16287] 711 351 198 218 290 278 251 267 383 580 505 332 227 178 162 128 106
    ## [16304]  87  49  21   7  11  21  78 168 248 320 423 476 525 555 549 469 403
    ## [16321] 271 253 172 139 145 133 129  95  73  46  12  12  15  42 119 204 329
    ## [16338] 359 442 447 428 407 372 280 276 210 129 110  74  59  27  19   5   2
    ## [16355]  12  39 131 386 663 278 139 197 242 251 217 257 380 619 580 406 259
    ## [16372] 203 113  74  29  13   5   2   6  34 148 418 665 326 176 205 313 267
    ## [16389] 224 287 395 590 550 384 235 157 122  83  26  14   8   2  10  29 101
    ## [16406] 273 551 365 172 241 344 365 413 449 469 374 311 243 136 105  88  57
    ## [16423]  46  42  18   6   3   8  17  56  97 119 219 232 262 269 220 228 182
    ## [16440] 109  70  44  52  46  44  36  32  13   5   1   4  10  20  77  94 112
    ## [16457] 201 251 379 424 435 447 357 300 208 154 142  98  79  67  42  20  32
    ## [16474]  11   3   3  10  25  62 122 142 186 199 225 207 198 170 126 145  83
    ## [16491]  58  75  76  57  34  36  28   8   2   3  11  21  34  96 139 204 202
    ## [16508] 195 203 245 228 181 141 127 119  64  62  41  23  10   5   5  10  41
    ## [16525] 124 365 638 286 147 130 210 217 228 230 346 553 521 371 277 181 104
    ## [16542]  65  26   6   5   4   5  31 100 293 500 268  57  65  76  77  94 155
    ## [16559] 257 527 482 350 232 185  97  67  23  12   1   3   4  39 136 409 692
    ## [16576] 322 161 146 208 214 176 179 325 563 542 387 263 223 154  78  29  16
    ## [16593]   7   7  42 121 362 679 299 178 163 236 224 187 220 370 561 537 344
    ## [16610] 252 208 151 130  52  19  12   4   3  40 106 352 729 330 169 201 268
    ## [16627] 262 251 296 405 582 509 377 245 183 163 110 108  69  50  15   5  13
    ## [16644]  27  63 144 193 256 392 446 509 479 484 443 320 297 249 198 158 147
    ## [16661] 126 117  94  74  25   7   8  16  31  93 172 293 355 520 431 473 417
    ## [16678] 443 303 203 174 154  99  96  51  21  13  10   8   5  38 138 396 731
    ## [16695] 308 136 233 268 321 260 268 442 708 692 471 300 221 144 102  55  25
    ## [16712]   8   4   7  46 153 502 721 336 156 207 312 272 270 300 435 743 731
    ## [16729] 460 306 280 181  96  37  11   9   7  10  49 124 398 759 388 141 172
    ## [16746] 232 214 218 285 377 605 609 414 293 198 105  74  44  16   9   2   9
    ## [16763]  32 124 389 659 276 145 178 235 245 212 270 331 617 565 373 227 191
    ## [16780] 133  93  48  28  11   5  10  26  84 215 441 301 166 203 240 220 215
    ## [16797] 303 375 568 498 352 241 171 165 122 103 100  70  29  12   6  20  39
    ## [16814] 111 170 287 404 486 547 542 541 507 345 304 246 182 209 160 162 118
    ## [16831] 102  78  48  12   8   6  23  69 103 219 250 315 285 232 225 253 229
    ## [16848] 198 122 108  96  78  51  20   4   5   4  12  27 123 294 584 284 133
    ## [16865] 134 173 220 210 236 345 616 564 427 300 245 126  84  31   8   1   3
    ## [16882]   8  41 118 380 724 334 154 173 226 254 204 270 358 601 546 433 257
    ## [16899] 207 106  64  34  21   9  10   4  37 128 369 688 285 136 172 232 238
    ## [16916] 225 228 329 561 540 402 268 202 122  79  32  23   8   2   8  33 114
    ## [16933] 385 679 325 167 189 282 271 242 280 406 550 466 348 241 213 148 120
    ## [16950]  47  26   9  12  10  34 113 308 636 343 190 211 273 313 299 309 417
    ## [16967] 622 455 319 221 172 138 134  94  95  69  23   6   3  11  48 119 220
    ## [16984] 273 393 453 456 426 447 413 309 272 257 201 184 156 119 110  96  87
    ## [17001]  38  11   9   6  27  88 132 257 295 417 356 310 338 400 243 107 109
    ## [17018] 122 106  89  33  28  15   5   3   5  24 108 319 592 282 135 170 232
    ## [17035] 210 181 211 302 495 507 340 200 120  54  47  18  15   7   5   8  36
    ## [17052] 118 355 662 326 184 269 313 286 247 246 363 572 525 353 268 168 132
    ## [17069]  81  41  15   3   5   7  31 112 363 678 317 164 200 236 213 218 237
    ## [17086] 334 562 569 336 241 168 129  88  42  20   8   4   6  35 118 350 599
    ## [17103] 317 161 190 212 217 214 249 302 475 381 118  50  26  21  13  17   9
    ## [17120]   3   3   6  13  80 208 472 267 154 162 224 240 219 263 281 321 243
    ## [17137] 153 112  70  63  40  31  35  23   7   5   5  13  20  29  90 118 159
    ## [17154] 146 158 161 153 147 114  82  60  61  53  39  40  27  20  18   9   4
    ## [17171]   6   9  19  48  56  93 152 196 186 175 137 190 114  93  58  52  42
    ## [17188]  58  25  12  11   5   3   9  17  30  66  85 103 124 135  70  46  33
    ## [17205]  33  26  26  18  23  22  12  11  13  13   7   1   3   7   6  11  28
    ## [17222]  32  86 114 121 126 107  86  50  43  36  40  34  19  30   9   7   1
    ## [17239]   2   2  11  36  26  31  23   8  10  10  15  20  13  13  53  43  35
    ## [17256]  32  20  11  10   3   5   2   1   3  11  45  90 208 133  75 103 109
    ## [17273] 118 119 120 174 257 197 117  91  63  44  26  25   9   5   2   4  15
    ## [17290]  51 112 239 191 162 178 222 222 261 225 250 271 213 128  97  92  62
    ## [17307]  59  26  37  19   6   3   3   7  18  44  49  41  45  48  84  98 129
    ## [17324] 147 118 110  97  66  60  54  32  41  28  19  15   7   2   8  13  33
    ## [17341]  74 122 136 144 169 160 138 133 123 125 102  72  47  36  49  34  19
    ## [17358]  11   1   3   9  40  85 196 157 120 157 224 203 247 315 214 164 122
    ## [17375] 119  89  90  61  49

    BICIS$prediccion

    ## NULL

    BICIS$prediccion=predict(model_poisson,type="response")
    SCE=sum((BICIS$cnt-BICIS$prediccion)^2)
    STC=sum((BICIS$cnt-mean(BICIS$cnt))^2)
    R2=1-sum((BICIS$cnt-BICIS$prediccion)^2)/sum((BICIS$cnt-mean(BICIS$cnt))^2)

    R2

    ## [1] 0.7557846

-------------------------------------------------------------------------
-------------------------------------------------------------------------

COMO VALIDAS SI UN MODELO FUNCIONA:

Realidad: tengo un historico de variables, hago un modelo, lo estimo,
tengo la predicción, para gente nueva tengo mi predicción. A la gente
que tengo prediccion Si, le voy a hacer campaña. Como evaluo la
capacidad de la campaña? Clientes que tenías SI y le has hecho accion,
ves el % exito entre.. Clientes que tenias NO y no les has hecho acción,
ves el % exito esto me da 4,7 Mi modelo tiene propensos SI y propensos
NO, lo normal es hacer accion SI y accion NO. En este modelo no has
incentivado al que no es propenso. Hay que hacer acción sobre el no
propenso. El problema es el tamaño de no prepensos que incluyes.

Calculas propensos Si con accion si / propensos Si accion no + Prop Si
acc No/prop No Acc No

Pasa un año, como entrenas el modelo dentro de un año cuando no
funcione. Ya estan influenciado con la accion comercial puedo usar los
que no he aplicado accion pero eran propensos.

------------------------------------------------------------------------
