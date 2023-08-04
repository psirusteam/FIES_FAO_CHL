# Modelo de área para la estimación de la Escala de Experiencia de Inseguridad Alimentaria



El índice FIES (Escala de Experiencia de Inseguridad Alimentaria) se define como un sistema de medición para la inseguridad alimentaria a nivel individual y del hogar. La FIES se basa en cuestionarios que preguntan sobre las experiencias de los individuos y hogares en relación con el acceso a alimentos seguros y nutritivos. Estos cuestionarios se utilizan para definir un modelo probabilístico que vincule la medida (desconocida) de la inseguridad alimentaria con las respuestas (observables) a los cuestionarios basados en la experiencia. El modelo más sencillo utilizado para esto es el modelo de Rasch.

Los indicadores definidos son los porcentajes de individuos de la población con edades a partir de 15 años que experimentan niveles de inseguridad alimentaria moderada o grave (IA moderada+grave) y grave (IA grave). Estos indicadores se calculan utilizando una escala numérica que va desde 0 hasta 40, donde 0 indica seguridad alimentaria total y 40 indica inseguridad alimentaria extrema. Los puntajes más altos en la escala indican una mayor gravedad de la inseguridad alimentaria.


## Modelo de Rash 

El modelo de Rasch es un modelo matemático utilizado para el análisis de datos en la medición de habilidades y características de los individuos. Este modelo es ampliamente utilizado en la educación, psicología, medicina y otras áreas de investigación.

El modelo de Rasch se basa en la teoría de respuesta al ítem, que establece que la probabilidad de que un individuo responda correctamente a un ítem en particular depende de la habilidad del individuo y de la dificultad del ítem. El modelo de Rasch supone que la habilidad de un individuo y la dificultad de un ítem se pueden medir en una misma escala de medida, y que esta escala es unidimensional y lineal.

La ecuación del modelo de Rasch es la siguiente:

$$
P_i(X_j=1) = \frac{\exp(a_i -b_j)}{1 + \exp(a_i -b_j)}
$$

donde $P_i(X_j=1)$ es la probabilidad de que el individuo $i$ responda correctamente al ítem $j$, $a_i$ es el nivel de habilidad del individuo $i$, $b_j$ es la dificultad del ítem $j$, y $X_j$ es una variable indicadora que toma el valor de 1 si el individuo $i$ responde correctamente al ítem $j$, y 0 en caso contrario.

El proceso de estimación de los parámetros del modelo de Rasch se lleva a cabo mediante un procedimiento de máxima verosimilitud. Este proceso permite estimar tanto los niveles de habilidad de los individuos como las dificultades de los ítems. En el caso de las encuestas complejas, se utilizan métodos de estimación de la verosimilitud basados en muestras complejas para ajustar los parámetros del modelo. Es importante tener en cuenta el diseño de muestreo y la ponderación de las observaciones al utilizar el modelo de Rasch en encuestas complejas.


## Fuentes de error en la estimación del FIES

1. Error de muestreo: Este error se produce debido a la variabilidad natural en los datos recopilados a partir de una muestra de la población. Cuanto mayor sea el tamaño de la muestra, menor será este error.

2. Error de medición: Este error se produce debido a la variabilidad en las respuestas proporcionadas por los encuestados. El modelo de Rasch utilizado para estimar la inseguridad alimentaria tiene en cuenta este error y proporciona estimaciones precisas.


Debido a que los errores de muestreo y medición se consideran independientes, estos se combinan para obtener el error estándar de la prevalencia mundial de la siguiente forma: 

$$
\sigma^{2}_{total} = (Error de Muestreo)^2 + (Error de Medición)^2
$$

## Modelos de área 

La estimación de áreas pequeñas es un conjunto de técnicas que permiten la estimación de parámetros de interés para dominios donde los estimadores directos no pueden considerarse lo suficientemente confiables debido a que su varianza es demasiado alta para ser liberada. Las encuestas de oficinas estadísticas nacionales suelen planificarse a un nivel más alto, por lo que cuando se requiere información más detallada, el tamaño de la muestra puede no ser lo suficientemente grande como para garantizar la liberación de estimaciones directas y, en algunos casos, es posible que los dominios más pequeños no cuenten con unidades de muestra. Los métodos de estimación de áreas pequeñas aumentan la confiabilidad de la estimación "tomando fuerza prestada" de un conjunto de áreas en un dominio más grande para el cual el estimador directo es confiable. Esto significa que se utiliza información de otras áreas y / o se explota información adicional de diferentes fuentes.

Uno de los modelos más utilizados para la estimación de áreas pequeñas es el modelo de Fay-Herriot. El modelo de Fay-Herriot es un modelo lineal mixto que se utiliza para estimar parámetros de interés para pequeñas áreas en presencia de datos auxiliares. El modelo se utiliza para "prestar fuerza" de áreas más grandes a áreas más pequeñas y, por lo tanto, aumentar la precisión de las estimaciones para áreas pequeñas. 

En el modelo de Fay-Herriot, el parámetro de interés en cada área pequeña se modela como una combinación lineal de un estimador directo de la encuesta y un componente predicho basado en un modelo lineal mixto. El modelo relaciona el parámetro de interés con las variables auxiliares conocidas para cada uno de los dominios que constituyen la partición de la población completa. Se incluye un efecto para tener en cuenta la homogeneidad dentro de cada dominio. 


### Modelo de área de Fay-Herriot

Sea $P_d$ la probabilidad de encontrar un hogar con inseguridad alimentaria  en el $d-$ésimo dominio de la población. Entonces, el estimador directo de $P_d$ se puede escribir como:  

$$
\hat{P}^{DIR}_{d} = P_d + e_d
$$

Ahora bien, $P_d$ se puede modelar de la siguiente manera,  

$$
P_d = \boldsymbol{x}^{T}_{d}\boldsymbol{\beta} + u_d
$$
Luego, reescribiendo $\hat{P}^{DIR}_{d}$ en términos de las dos ecuaciones anteriores tenemos:  

$$
\hat{P}^{DIR}_{d} = \boldsymbol{x}^{T}_{d}\boldsymbol{\beta} + u_d + e_d
$$

Ahora, es posible suponer que $\hat{P}^{DIR}_d \sim N(\boldsymbol{x}^{T}_{d}\boldsymbol \beta, \sigma_u^2 +\sigma_{e_d}^2)$, $\hat{P}^{DIR}_d \mid u_d \sim N(\boldsymbol{x}^{T}_{d}\boldsymbol \beta + u_d,\sigma_{e_d}^2)$ y $u_d \sim N(0, \sigma^2_u)$

Luego, se asumen distribuciones previas para $\boldsymbol{\beta}$ y $\sigma^2_u$

$$
\begin{eqnarray*}
\beta_p & \sim   & N(0, 10000)\\
\sigma^2_u &\sim & IG(0.0001, 0.0001)
\end{eqnarray*}
$$

por tanto, el estimador bayesiano para $P_d$ esta dado como $\tilde{P}_d = E\left(P_d\mid\hat{P}_d^{DIR}\right)$



### Modelo de área de Fay-Herriot con tranformación arcoseno. 


En su concepción más básica, el modelo de **Fay-Herriot** es una combinación lineal de covariables. Sin embargo, el resultado de esta combinación pueden tomar valores que se salen del rango aceptable en el que puede estar una proporción; es decir, en general el estimador de Fay-Herriot $\theta \in R$, mientras que el estimador directo $\theta \in (0,1)$. La transformación arcoseno esta dada por: 

$$
\hat{z}_d = arcsin\left( \sqrt{ \hat{\theta}_d} \right)
$$ donde

$$
Var\left( \hat{z}_d \right) = \frac{\widehat{DEFF}_d}{4\times n_d} = \frac{1}{4\times n_{d,efectivo} }
$$

El modelo de Fay-Herriot  estaría definido de la siguiente forma:

$$
\begin{eqnarray*}
Z_d \mid \mu_d,\sigma^2_d &  \sim  & N(\mu_d, \sigma^2_d)\\
\mu_d & = & \boldsymbol{x}^{T}_{d}\boldsymbol{\beta} + u_d \\
\theta_d & = &  \left(sin(\mu_d)\right)^2
\end{eqnarray*}
$$ donde $u_d \sim N(0 , \sigma^2)$.


Suponga de las distribuciones previas para 
$\boldsymbol{\beta}$ y $\sigma_{u}^{2}$ son dadas por 
$$
\begin{eqnarray*}
\boldsymbol{\beta}	\sim	N\left(0,1000 \right)\\
\sigma_{u}^{2}	\sim	IG\left(0.0001,0.0001\right)
\end{eqnarray*}
$$

### Modelos de área con variable respuesta Beta. 


El modelo beta-logístico fue inicialmente considerado por Jiang y Lahiri (2006b) para un enfoque EBP en uno de sus ejemplos ilustrativos para estimar medias de dominio de población finita. 


El modelo de área beta-logístico estaría dado por las siguientes expresiones 
$$
\begin{eqnarray*}
\hat{p}_{d} \mid P_d & \sim & beta(a_d, b_d)\\
\end{eqnarray*}
$$
La función del enlace es 
$$
\begin{eqnarray*}
logit(P_{d}) \mid \boldsymbol{\beta}, \sigma^2_u  & \sim & N(\boldsymbol{x}_d^T\boldsymbol{\beta},\sigma^2_u)\\
\end{eqnarray*}
$$
Los parámetros $a_d$ y $b_d$ son estimados así: 
$$
\begin{eqnarray*}
a_d &=& P_d \times \phi_d\\
b_d &=& (1 - P_d) \times \phi_d\\
\end{eqnarray*}
$$ donde

$$\phi_d = \frac{n_d}{\widehat{DEFF}_d} -1 = n_{d,efecctivo} -1$$

Las distribuciones previas para $\boldsymbol{\beta}$ y $\sigma^2_u$

$$
\begin{eqnarray*}
\beta_k &\sim& N(0, 10000)\\
\sigma^2_u &\sim& IG(0.0001,0.0001)
\end{eqnarray*}
$$


### Modelos de área con variable respuesta Binomial.


El modelo lineal de Fay-Herriot puede ser reemplazado por un modelo mixto lineal generalizado (GLMM). Esto se puede hacer cuando los datos observados $Y_d$ son inherentemente discretos, como cuando son recuentos (no ponderados) de personas u hogares muestreados con ciertas características. Uno de estos modelos supone una distribución binomial para $Y_d$ con probabilidad de éxito $\theta_d$, y una logística modelo de regresión para $\theta_d$ con errores normales en la escala logit. El modelo resultante es


$$
\begin{eqnarray*}
Y_{d}\mid \theta_{d},n_{d} & \sim & Bin\left(n_{d},\theta_{d}\right)
\end{eqnarray*}
$$
para $d=1,\dots,D$ y 

$$
\begin{eqnarray*}
logit\left(\theta_{d}\right)=\log\left(\frac{\theta_{d}}{1-\theta_{d}}\right) & = & \boldsymbol{x}_{d}^{T}\boldsymbol{\beta}+u_{d}
\end{eqnarray*}
$$
donde $u_{d}\sim N\left(0,\sigma_{u}^{2}\right)$ y $n_{d}$ es el
tamaño de la muestra para el área $d$.

El modelo anterior se puede aplicar fácilmente a recuentos de muestras no ponderadas $Y_d$, pero esto ignora cualquier aspecto complejo del diseño de la encuesta. En muestras complejas donde las $Y_d$ son estimaciones ponderadas, surgen dos problemas. En primer lugar, los posibles valores de
el $Y_d$ no serán los números enteros $0, 1, \dots , n_d$ para cualquier definición directa de tamaño de muestra $n_d$. En su lugar, $Y_d$ tomará un valor de un conjunto finito de números desigualmente espaciados determinados por las ponderaciones de la encuesta que se aplican a los casos de muestra en el dominio  $d$. En segundo lugar, la varianza muestral de $Y_d$
implícito en la distribución Binomial, es decir,  $n_d \times \theta_d (1-\theta_d)$, será incorrecto. Abordamos estos dos problemas al definir un **tamaño de muestra efectivo** $\tilde{n}_d$, y un **número de muestra efectivo de éxitos** $\tilde{Y_d}$ determinó mantener: (i) la estimación directa  $\hat{\theta}_i$, de la pobreza y (ii) una estimación de la varianza de muestreo correspondiente,$\widehat{Var}(\hat{\theta}_d)$. 


Es posible suponer que 
$$
\begin{eqnarray*}
\tilde{n}_{d} & \sim & \frac{\check{\theta}_{d}\left(1-\check{\theta}_{d}\right)}{\widehat{Var}\left(\hat{\theta}_{d}\right)}
\end{eqnarray*}
$$
donde $\check{\theta}_{d}$ es una preliminar perdicción basada en el modelo para la proporción poblacional $\theta_d$ y $\widehat{Var}\left(\hat{\theta}_{d}\right)$ depende de$\check{\theta}_{d}$ a través de una función de varianza generalizada ajustada (FGV). Note que $\tilde{Y}_{d}=\tilde{n}_{d}\times\hat{\theta}_{d}$. 

Suponga de las distribuciones previas para 
$\boldsymbol{\beta}$ y $\sigma_{u}^{2}$ son dadas por 
$$
\begin{eqnarray*}
\boldsymbol{\beta}	\sim	N\left(0,10000\right)\\
\sigma_{u}^{2}	\sim	IG\left(0.0001,0.0001\right)
\end{eqnarray*}
$$

# Estimación de la Escala de Experiencia de Inseguridad Alimentaria

Luego de recopilar la información proporcionada por FAO, se procedió al ajuste de los 4 modelos anteriores utilizando un conjunto de covariables detalladas a continuación:

- region                                      
- accesibilidad_hospitales                    
- luces_nocturnas                             
- prop_b50median_afc_2020                     
- prop_fonasa_a_2019                          
- prop_fonasa_c_2019                          
- tasa_victimizacion_2019                     
- tasa_mort_infantil_2017                     
- log_ing_municipales_permanentes_pc_2018     
- prop_am_bajo_peso_2018                      
- prop_am_normal_2018                         
- prop_am_obeso_2018                          
- prop_obeso_sobrepeso_menores_2018           
- prop_riesgo_desnutricion_menores_2018       
- prop_obeso_sobrepeso_menores_2018_w         
- prop_isapre_2019                            
- prop_red_publica_2017                       
- prop_camion_aljibe_2017                     
- prop_rio_vertiente_estero_canal_2017        
- prop_deficit_habitacional_cuantitativo_2017 
- indice_de_vejez_2020                        
 

Posteriormente, se llevó a cabo un análisis de chequeo predictivo con el fin de seleccionar el modelo más adecuado. Cabe destacar que se realizaron 6500 iteraciones, descartando las primeras 5000 iteraciones como parte del proceso de quemado. Al evaluar el criterio de Rhat, se pudo constatar que todas las cadenas del modelo convergieron satisfactoriamente.  


### Resultado para el modelo de área de Fay-Herriot. {-}

<img src="Data/RecursosBook/02/1_ppc_normal.jpeg" width="200%" style="display: block; margin: auto;" />


### Resultado para el Modelo de área de Fay-Herriot con tranformación arcoseno. {-}

<img src="Data/RecursosBook/02/2_ppc_arcosin.jpeg" width="200%" style="display: block; margin: auto;" />

### Resultado para el modelo de área con variable respuesta Beta. {-}


<img src="Data/RecursosBook/02/3_ppc_beta.jpeg" width="200%" style="display: block; margin: auto;" />

### Resultado para el modelo de área con variable respuesta Binomial. {-}


<img src="Data/RecursosBook/02/4_ppc_binomial.jpeg" width="200%" style="display: block; margin: auto;" />

De acuerdo con los resultados obtenidos, se determinó que el modelo de área con variable respuesta binomial presenta un mejor ajuste en comparación con los otros modelos considerados. 


<img src="Data/RecursosBook/02/5_comparando.jpeg" width="200%" style="display: block; margin: auto;" />


A partir de este modelo, se procedió a realizar el proceso de benchmarking, el cual se describe a continuación:

## Proceso de Benchmark 

1. Leer estimaciones del modelo


```r
estimacionesPre <- readRDS("Data/estimacionesPre.rds") %>%
  transmute(dam2 = haven::as_factor(comuna, levels = "values"),
            dam2 = str_pad(width = 5, dam2, pad = "0"),
            dam = str_sub(dam2,1,2),
            theta_pred,thetaSyn)
```


2. Del censo extraer el total de personas por Región 



```r
total_hh <- readRDS(file = "Data/Total_Hogares.rds")
N_hh <- total_hh %>% group_by(dam = str_sub(dam2, 1, 2)) %>%
  mutate(dam_hh = sum(NN_Hogar)) 

tba(N_hh %>% data.frame() %>% slice(1:10))
```

<table class="table table-striped lightable-classic" style="width: auto !important; margin-left: auto; margin-right: auto; font-family: Arial Narrow; width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> dam2 </th>
   <th style="text-align:right;"> NN_Hogar </th>
   <th style="text-align:left;"> dam </th>
   <th style="text-align:right;"> dam_hh </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 01101 </td>
   <td style="text-align:right;"> 60226 </td>
   <td style="text-align:left;"> 01 </td>
   <td style="text-align:right;"> 97693 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 01107 </td>
   <td style="text-align:right;"> 29699 </td>
   <td style="text-align:left;"> 01 </td>
   <td style="text-align:right;"> 97693 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 01401 </td>
   <td style="text-align:right;"> 4188 </td>
   <td style="text-align:left;"> 01 </td>
   <td style="text-align:right;"> 97693 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 01402 </td>
   <td style="text-align:right;"> 484 </td>
   <td style="text-align:left;"> 01 </td>
   <td style="text-align:right;"> 97693 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 01403 </td>
   <td style="text-align:right;"> 488 </td>
   <td style="text-align:left;"> 01 </td>
   <td style="text-align:right;"> 97693 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 01404 </td>
   <td style="text-align:right;"> 965 </td>
   <td style="text-align:left;"> 01 </td>
   <td style="text-align:right;"> 97693 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 01405 </td>
   <td style="text-align:right;"> 1643 </td>
   <td style="text-align:left;"> 01 </td>
   <td style="text-align:right;"> 97693 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 02101 </td>
   <td style="text-align:right;"> 105863 </td>
   <td style="text-align:left;"> 02 </td>
   <td style="text-align:right;"> 174314 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 02102 </td>
   <td style="text-align:right;"> 3430 </td>
   <td style="text-align:left;"> 02 </td>
   <td style="text-align:right;"> 174314 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 02103 </td>
   <td style="text-align:right;"> 358 </td>
   <td style="text-align:left;"> 02 </td>
   <td style="text-align:right;"> 174314 </td>
  </tr>
</tbody>
</table>

3. Obtener las estimaciones directa por region o el nivel de agregación en el cual la encuesta es representativa. 


```r
directoDam <- readRDS("Data/FIES_region.rds") %>% 
  dplyr::select( dam, ModerateSevere = FIES)
```


4. Realizar el consolidando información obtenida en *1* y *2*.  


```r
temp <- estimacionesPre %>%
  inner_join(N_hh ) %>% 
  inner_join(directoDam )
```

5. Con la información organizada realizar el calculo de los pesos para el Benchmark


```r
R_dam2 <- temp %>% group_by(dam) %>%
  summarise(R_dam_RB = unique(ModerateSevere) / sum((NN_Hogar  / dam_hh) * theta_pred)) %>%
  left_join(directoDam, by = "dam")
```

calculando los pesos para cada comuna


```r
pesos <- temp %>% 
  mutate(W_i = NN_Hogar / dam_hh) %>% 
  select(dam2, W_i)
```


6. Realizar la estimación FH  Benchmark 



```r
estimacionesBench <- estimacionesPre %>%
  left_join(R_dam2, by = c("dam")) %>%
  mutate(theta_pred_RBench = R_dam_RB * theta_pred) %>%
  left_join(pesos) %>% 
   select(dam, dam2, W_i, theta_pred, thetaSyn, theta_pred_RBench)  
```

6. Validación: Estimación FH con Benchmark


```r
estimacionesBench %>% group_by(dam) %>%
  summarise(theta_reg_RB = sum(W_i * theta_pred_RBench)) %>%
  left_join(directoDam, by = "dam") %>% 
  data.frame()
```

## Validación de los resultados. 

La visualización resultante del siguiente código muestra puntos de diferentes formas y colores para representar los diferentes métodos de estimación, y dos líneas punteadas que representan los intervalos de confianza superior e inferior para los valores observados en la variable `theta_dir`.



```r
IC_dir <- readRDS("Data/FIES_region.rds") %>%
  dplyr::select(dam, FIES, var_hat) %>%
  transmute(dam,
            Ls = FIES + 1.96 * sqrt(var_hat),
            Li = FIES - 1.96 * sqrt(var_hat))
temp <- estimacionesBench %>% left_join( estimacionesPre ) %>% 
  group_by(dam) %>% 
  summarise(
            "FIES Modelo" = sum(W_i * theta_pred),
            "FIES Modelo Syn" = sum(W_i * thetaSyn),
            "FIES Modelo Bench" = sum(W_i * theta_pred_RBench)
  ) %>%   
  left_join(directoDam, by = "dam")  %>% 
  mutate(id = 1:n())

temp %<>% gather(key = "Metodo",value = "Estimacion",
                 -id, -dam)
temp <- inner_join(temp,IC_dir)
p_temp <- ggplot(data = temp, aes(x = id, y = Estimacion, shape = Metodo)) +
  geom_point(aes(color = Metodo), size = 2) +
  geom_line(aes(y = Li), linetype  = 2) +
  geom_line(aes(y = Ls),  linetype  = 2) +
  theme_bw(10) + 
  scale_x_continuous(breaks = temp$id,
                     labels =  temp$dam) +
  labs(y = "", x = "")
```


![](Data/RecursosBook/03/1_validacion_Bench.jpeg)<!-- -->

## Mapa con la estimación de la Escala de Experiencia de Inseguridad Alimentaria 


<img src="Data/RecursosBook/04/CHL_FIES3.jpeg" width="200%" />

### Tabla con las estimaciones por comuna. 

<table class="table table-striped lightable-classic" style="width: auto !important; margin-left: auto; margin-right: auto; font-family: Arial Narrow; width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> Region </th>
   <th style="text-align:left;"> Comuna </th>
   <th style="text-align:right;"> FIES </th>
   <th style="text-align:right;"> FIES_ee </th>
   <th style="text-align:right;"> FIES_cv(%) </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 16 </td>
   <td style="text-align:left;"> 16104 </td>
   <td style="text-align:right;"> 0.1293 </td>
   <td style="text-align:right;"> 0.0354 </td>
   <td style="text-align:right;"> 27.397 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 09 </td>
   <td style="text-align:left;"> 09205 </td>
   <td style="text-align:right;"> 0.1725 </td>
   <td style="text-align:right;"> 0.0448 </td>
   <td style="text-align:right;"> 25.972 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13108 </td>
   <td style="text-align:right;"> 0.0436 </td>
   <td style="text-align:right;"> 0.0089 </td>
   <td style="text-align:right;"> 20.486 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 16 </td>
   <td style="text-align:left;"> 16106 </td>
   <td style="text-align:right;"> 0.2242 </td>
   <td style="text-align:right;"> 0.0448 </td>
   <td style="text-align:right;"> 20.004 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 10107 </td>
   <td style="text-align:right;"> 0.1865 </td>
   <td style="text-align:right;"> 0.0370 </td>
   <td style="text-align:right;"> 19.812 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 10307 </td>
   <td style="text-align:right;"> 0.1026 </td>
   <td style="text-align:right;"> 0.0184 </td>
   <td style="text-align:right;"> 17.950 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 12 </td>
   <td style="text-align:left;"> 12301 </td>
   <td style="text-align:right;"> 0.0535 </td>
   <td style="text-align:right;"> 0.0091 </td>
   <td style="text-align:right;"> 17.066 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 14 </td>
   <td style="text-align:left;"> 14105 </td>
   <td style="text-align:right;"> 0.1469 </td>
   <td style="text-align:right;"> 0.0247 </td>
   <td style="text-align:right;"> 16.802 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 02 </td>
   <td style="text-align:left;"> 02302 </td>
   <td style="text-align:right;"> 0.1953 </td>
   <td style="text-align:right;"> 0.0327 </td>
   <td style="text-align:right;"> 16.758 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 10305 </td>
   <td style="text-align:right;"> 0.1065 </td>
   <td style="text-align:right;"> 0.0173 </td>
   <td style="text-align:right;"> 16.253 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 01 </td>
   <td style="text-align:left;"> 01402 </td>
   <td style="text-align:right;"> 0.1512 </td>
   <td style="text-align:right;"> 0.0239 </td>
   <td style="text-align:right;"> 15.833 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05 </td>
   <td style="text-align:left;"> 05201 </td>
   <td style="text-align:right;"> 0.3141 </td>
   <td style="text-align:right;"> 0.0497 </td>
   <td style="text-align:right;"> 15.815 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 06 </td>
   <td style="text-align:left;"> 06104 </td>
   <td style="text-align:right;"> 0.1379 </td>
   <td style="text-align:right;"> 0.0217 </td>
   <td style="text-align:right;"> 15.708 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 02 </td>
   <td style="text-align:left;"> 02102 </td>
   <td style="text-align:right;"> 0.1909 </td>
   <td style="text-align:right;"> 0.0295 </td>
   <td style="text-align:right;"> 15.441 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05 </td>
   <td style="text-align:left;"> 05301 </td>
   <td style="text-align:right;"> 0.1342 </td>
   <td style="text-align:right;"> 0.0206 </td>
   <td style="text-align:right;"> 15.344 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 16 </td>
   <td style="text-align:left;"> 16102 </td>
   <td style="text-align:right;"> 0.2381 </td>
   <td style="text-align:right;"> 0.0363 </td>
   <td style="text-align:right;"> 15.232 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13110 </td>
   <td style="text-align:right;"> 0.1360 </td>
   <td style="text-align:right;"> 0.0203 </td>
   <td style="text-align:right;"> 14.896 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05 </td>
   <td style="text-align:left;"> 05703 </td>
   <td style="text-align:right;"> 0.1336 </td>
   <td style="text-align:right;"> 0.0199 </td>
   <td style="text-align:right;"> 14.873 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 04 </td>
   <td style="text-align:left;"> 04304 </td>
   <td style="text-align:right;"> 0.1373 </td>
   <td style="text-align:right;"> 0.0203 </td>
   <td style="text-align:right;"> 14.785 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 10306 </td>
   <td style="text-align:right;"> 0.0731 </td>
   <td style="text-align:right;"> 0.0105 </td>
   <td style="text-align:right;"> 14.413 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 04 </td>
   <td style="text-align:left;"> 04303 </td>
   <td style="text-align:right;"> 0.1774 </td>
   <td style="text-align:right;"> 0.0255 </td>
   <td style="text-align:right;"> 14.403 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 01 </td>
   <td style="text-align:left;"> 01405 </td>
   <td style="text-align:right;"> 0.2074 </td>
   <td style="text-align:right;"> 0.0296 </td>
   <td style="text-align:right;"> 14.282 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13301 </td>
   <td style="text-align:right;"> 0.2254 </td>
   <td style="text-align:right;"> 0.0322 </td>
   <td style="text-align:right;"> 14.282 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 06 </td>
   <td style="text-align:left;"> 06116 </td>
   <td style="text-align:right;"> 0.1765 </td>
   <td style="text-align:right;"> 0.0251 </td>
   <td style="text-align:right;"> 14.203 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05 </td>
   <td style="text-align:left;"> 05402 </td>
   <td style="text-align:right;"> 0.1522 </td>
   <td style="text-align:right;"> 0.0216 </td>
   <td style="text-align:right;"> 14.197 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05 </td>
   <td style="text-align:left;"> 05105 </td>
   <td style="text-align:right;"> 0.1637 </td>
   <td style="text-align:right;"> 0.0232 </td>
   <td style="text-align:right;"> 14.167 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 10104 </td>
   <td style="text-align:right;"> 0.1983 </td>
   <td style="text-align:right;"> 0.0280 </td>
   <td style="text-align:right;"> 14.144 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13605 </td>
   <td style="text-align:right;"> 0.2451 </td>
   <td style="text-align:right;"> 0.0345 </td>
   <td style="text-align:right;"> 14.079 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 11 </td>
   <td style="text-align:left;"> 11401 </td>
   <td style="text-align:right;"> 0.0686 </td>
   <td style="text-align:right;"> 0.0097 </td>
   <td style="text-align:right;"> 14.064 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 10209 </td>
   <td style="text-align:right;"> 0.1977 </td>
   <td style="text-align:right;"> 0.0277 </td>
   <td style="text-align:right;"> 14.000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 06 </td>
   <td style="text-align:left;"> 06109 </td>
   <td style="text-align:right;"> 0.1683 </td>
   <td style="text-align:right;"> 0.0235 </td>
   <td style="text-align:right;"> 13.997 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 14 </td>
   <td style="text-align:left;"> 14103 </td>
   <td style="text-align:right;"> 0.2282 </td>
   <td style="text-align:right;"> 0.0318 </td>
   <td style="text-align:right;"> 13.918 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13504 </td>
   <td style="text-align:right;"> 0.1791 </td>
   <td style="text-align:right;"> 0.0249 </td>
   <td style="text-align:right;"> 13.888 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 04 </td>
   <td style="text-align:left;"> 04104 </td>
   <td style="text-align:right;"> 0.1760 </td>
   <td style="text-align:right;"> 0.0244 </td>
   <td style="text-align:right;"> 13.879 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13501 </td>
   <td style="text-align:right;"> 0.1797 </td>
   <td style="text-align:right;"> 0.0249 </td>
   <td style="text-align:right;"> 13.869 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 06 </td>
   <td style="text-align:left;"> 06306 </td>
   <td style="text-align:right;"> 0.1719 </td>
   <td style="text-align:right;"> 0.0236 </td>
   <td style="text-align:right;"> 13.725 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13116 </td>
   <td style="text-align:right;"> 0.1526 </td>
   <td style="text-align:right;"> 0.0209 </td>
   <td style="text-align:right;"> 13.717 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 06 </td>
   <td style="text-align:left;"> 06205 </td>
   <td style="text-align:right;"> 0.1713 </td>
   <td style="text-align:right;"> 0.0233 </td>
   <td style="text-align:right;"> 13.583 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 11 </td>
   <td style="text-align:left;"> 11202 </td>
   <td style="text-align:right;"> 0.1309 </td>
   <td style="text-align:right;"> 0.0178 </td>
   <td style="text-align:right;"> 13.565 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 10108 </td>
   <td style="text-align:right;"> 0.2183 </td>
   <td style="text-align:right;"> 0.0294 </td>
   <td style="text-align:right;"> 13.469 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 02 </td>
   <td style="text-align:left;"> 02203 </td>
   <td style="text-align:right;"> 0.2246 </td>
   <td style="text-align:right;"> 0.0302 </td>
   <td style="text-align:right;"> 13.423 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05 </td>
   <td style="text-align:left;"> 05604 </td>
   <td style="text-align:right;"> 0.1678 </td>
   <td style="text-align:right;"> 0.0225 </td>
   <td style="text-align:right;"> 13.389 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13118 </td>
   <td style="text-align:right;"> 0.1591 </td>
   <td style="text-align:right;"> 0.0212 </td>
   <td style="text-align:right;"> 13.330 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 04 </td>
   <td style="text-align:left;"> 04201 </td>
   <td style="text-align:right;"> 0.1858 </td>
   <td style="text-align:right;"> 0.0247 </td>
   <td style="text-align:right;"> 13.281 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 06 </td>
   <td style="text-align:left;"> 06203 </td>
   <td style="text-align:right;"> 0.1508 </td>
   <td style="text-align:right;"> 0.0199 </td>
   <td style="text-align:right;"> 13.224 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05 </td>
   <td style="text-align:left;"> 05304 </td>
   <td style="text-align:right;"> 0.1964 </td>
   <td style="text-align:right;"> 0.0260 </td>
   <td style="text-align:right;"> 13.217 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 16 </td>
   <td style="text-align:left;"> 16108 </td>
   <td style="text-align:right;"> 0.2360 </td>
   <td style="text-align:right;"> 0.0311 </td>
   <td style="text-align:right;"> 13.190 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05 </td>
   <td style="text-align:left;"> 05705 </td>
   <td style="text-align:right;"> 0.1605 </td>
   <td style="text-align:right;"> 0.0209 </td>
   <td style="text-align:right;"> 13.027 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 16 </td>
   <td style="text-align:left;"> 16302 </td>
   <td style="text-align:right;"> 0.2240 </td>
   <td style="text-align:right;"> 0.0292 </td>
   <td style="text-align:right;"> 13.019 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 07 </td>
   <td style="text-align:left;"> 07104 </td>
   <td style="text-align:right;"> 0.1692 </td>
   <td style="text-align:right;"> 0.0220 </td>
   <td style="text-align:right;"> 12.989 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13503 </td>
   <td style="text-align:right;"> 0.2018 </td>
   <td style="text-align:right;"> 0.0261 </td>
   <td style="text-align:right;"> 12.914 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 07 </td>
   <td style="text-align:left;"> 07305 </td>
   <td style="text-align:right;"> 0.2014 </td>
   <td style="text-align:right;"> 0.0259 </td>
   <td style="text-align:right;"> 12.877 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13106 </td>
   <td style="text-align:right;"> 0.1090 </td>
   <td style="text-align:right;"> 0.0139 </td>
   <td style="text-align:right;"> 12.745 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 09 </td>
   <td style="text-align:left;"> 09116 </td>
   <td style="text-align:right;"> 0.1863 </td>
   <td style="text-align:right;"> 0.0236 </td>
   <td style="text-align:right;"> 12.678 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13117 </td>
   <td style="text-align:right;"> 0.1860 </td>
   <td style="text-align:right;"> 0.0235 </td>
   <td style="text-align:right;"> 12.656 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 15 </td>
   <td style="text-align:left;"> 15201 </td>
   <td style="text-align:right;"> 0.2488 </td>
   <td style="text-align:right;"> 0.0313 </td>
   <td style="text-align:right;"> 12.584 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 04 </td>
   <td style="text-align:left;"> 04301 </td>
   <td style="text-align:right;"> 0.1847 </td>
   <td style="text-align:right;"> 0.0232 </td>
   <td style="text-align:right;"> 12.552 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05 </td>
   <td style="text-align:left;"> 05706 </td>
   <td style="text-align:right;"> 0.1860 </td>
   <td style="text-align:right;"> 0.0233 </td>
   <td style="text-align:right;"> 12.532 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 07 </td>
   <td style="text-align:left;"> 07407 </td>
   <td style="text-align:right;"> 0.1918 </td>
   <td style="text-align:right;"> 0.0239 </td>
   <td style="text-align:right;"> 12.471 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 12 </td>
   <td style="text-align:left;"> 12401 </td>
   <td style="text-align:right;"> 0.1635 </td>
   <td style="text-align:right;"> 0.0203 </td>
   <td style="text-align:right;"> 12.450 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 09 </td>
   <td style="text-align:left;"> 09201 </td>
   <td style="text-align:right;"> 0.2222 </td>
   <td style="text-align:right;"> 0.0277 </td>
   <td style="text-align:right;"> 12.448 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 09 </td>
   <td style="text-align:left;"> 09103 </td>
   <td style="text-align:right;"> 0.2470 </td>
   <td style="text-align:right;"> 0.0307 </td>
   <td style="text-align:right;"> 12.441 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13114 </td>
   <td style="text-align:right;"> 0.1523 </td>
   <td style="text-align:right;"> 0.0189 </td>
   <td style="text-align:right;"> 12.435 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 09 </td>
   <td style="text-align:left;"> 09113 </td>
   <td style="text-align:right;"> 0.2300 </td>
   <td style="text-align:right;"> 0.0285 </td>
   <td style="text-align:right;"> 12.387 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 06 </td>
   <td style="text-align:left;"> 06107 </td>
   <td style="text-align:right;"> 0.2025 </td>
   <td style="text-align:right;"> 0.0251 </td>
   <td style="text-align:right;"> 12.375 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 09 </td>
   <td style="text-align:left;"> 09111 </td>
   <td style="text-align:right;"> 0.2277 </td>
   <td style="text-align:right;"> 0.0282 </td>
   <td style="text-align:right;"> 12.367 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05 </td>
   <td style="text-align:left;"> 05405 </td>
   <td style="text-align:right;"> 0.2055 </td>
   <td style="text-align:right;"> 0.0254 </td>
   <td style="text-align:right;"> 12.348 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 09 </td>
   <td style="text-align:left;"> 09112 </td>
   <td style="text-align:right;"> 0.2383 </td>
   <td style="text-align:right;"> 0.0294 </td>
   <td style="text-align:right;"> 12.336 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05 </td>
   <td style="text-align:left;"> 05603 </td>
   <td style="text-align:right;"> 0.2068 </td>
   <td style="text-align:right;"> 0.0254 </td>
   <td style="text-align:right;"> 12.303 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 06 </td>
   <td style="text-align:left;"> 06206 </td>
   <td style="text-align:right;"> 0.2051 </td>
   <td style="text-align:right;"> 0.0252 </td>
   <td style="text-align:right;"> 12.279 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05 </td>
   <td style="text-align:left;"> 05506 </td>
   <td style="text-align:right;"> 0.1828 </td>
   <td style="text-align:right;"> 0.0224 </td>
   <td style="text-align:right;"> 12.232 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13111 </td>
   <td style="text-align:right;"> 0.1719 </td>
   <td style="text-align:right;"> 0.0210 </td>
   <td style="text-align:right;"> 12.190 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13603 </td>
   <td style="text-align:right;"> 0.2340 </td>
   <td style="text-align:right;"> 0.0284 </td>
   <td style="text-align:right;"> 12.147 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 03 </td>
   <td style="text-align:left;"> 03102 </td>
   <td style="text-align:right;"> 0.2214 </td>
   <td style="text-align:right;"> 0.0268 </td>
   <td style="text-align:right;"> 12.101 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 08 </td>
   <td style="text-align:left;"> 08109 </td>
   <td style="text-align:right;"> 0.1576 </td>
   <td style="text-align:right;"> 0.0191 </td>
   <td style="text-align:right;"> 12.101 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 03 </td>
   <td style="text-align:left;"> 03304 </td>
   <td style="text-align:right;"> 0.1790 </td>
   <td style="text-align:right;"> 0.0216 </td>
   <td style="text-align:right;"> 12.084 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 09 </td>
   <td style="text-align:left;"> 09108 </td>
   <td style="text-align:right;"> 0.2677 </td>
   <td style="text-align:right;"> 0.0323 </td>
   <td style="text-align:right;"> 12.067 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 10105 </td>
   <td style="text-align:right;"> 0.1780 </td>
   <td style="text-align:right;"> 0.0215 </td>
   <td style="text-align:right;"> 12.063 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 09 </td>
   <td style="text-align:left;"> 09115 </td>
   <td style="text-align:right;"> 0.2510 </td>
   <td style="text-align:right;"> 0.0302 </td>
   <td style="text-align:right;"> 12.050 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13115 </td>
   <td style="text-align:right;"> 0.1673 </td>
   <td style="text-align:right;"> 0.0201 </td>
   <td style="text-align:right;"> 12.039 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 07 </td>
   <td style="text-align:left;"> 07405 </td>
   <td style="text-align:right;"> 0.1823 </td>
   <td style="text-align:right;"> 0.0219 </td>
   <td style="text-align:right;"> 12.034 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 09 </td>
   <td style="text-align:left;"> 09206 </td>
   <td style="text-align:right;"> 0.2010 </td>
   <td style="text-align:right;"> 0.0241 </td>
   <td style="text-align:right;"> 12.010 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13121 </td>
   <td style="text-align:right;"> 0.1644 </td>
   <td style="text-align:right;"> 0.0197 </td>
   <td style="text-align:right;"> 11.996 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 09 </td>
   <td style="text-align:left;"> 09207 </td>
   <td style="text-align:right;"> 0.2746 </td>
   <td style="text-align:right;"> 0.0329 </td>
   <td style="text-align:right;"> 11.986 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 14 </td>
   <td style="text-align:left;"> 14106 </td>
   <td style="text-align:right;"> 0.2702 </td>
   <td style="text-align:right;"> 0.0322 </td>
   <td style="text-align:right;"> 11.912 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05 </td>
   <td style="text-align:left;"> 05404 </td>
   <td style="text-align:right;"> 0.2101 </td>
   <td style="text-align:right;"> 0.0249 </td>
   <td style="text-align:right;"> 11.872 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 08 </td>
   <td style="text-align:left;"> 08102 </td>
   <td style="text-align:right;"> 0.2273 </td>
   <td style="text-align:right;"> 0.0270 </td>
   <td style="text-align:right;"> 11.872 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 14 </td>
   <td style="text-align:left;"> 14104 </td>
   <td style="text-align:right;"> 0.2414 </td>
   <td style="text-align:right;"> 0.0285 </td>
   <td style="text-align:right;"> 11.800 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 03 </td>
   <td style="text-align:left;"> 03103 </td>
   <td style="text-align:right;"> 0.2025 </td>
   <td style="text-align:right;"> 0.0238 </td>
   <td style="text-align:right;"> 11.768 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05 </td>
   <td style="text-align:left;"> 05103 </td>
   <td style="text-align:right;"> 0.1816 </td>
   <td style="text-align:right;"> 0.0214 </td>
   <td style="text-align:right;"> 11.765 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 07 </td>
   <td style="text-align:left;"> 07408 </td>
   <td style="text-align:right;"> 0.1996 </td>
   <td style="text-align:right;"> 0.0234 </td>
   <td style="text-align:right;"> 11.739 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 11 </td>
   <td style="text-align:left;"> 11402 </td>
   <td style="text-align:right;"> 0.1936 </td>
   <td style="text-align:right;"> 0.0227 </td>
   <td style="text-align:right;"> 11.739 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 04 </td>
   <td style="text-align:left;"> 04203 </td>
   <td style="text-align:right;"> 0.2385 </td>
   <td style="text-align:right;"> 0.0280 </td>
   <td style="text-align:right;"> 11.726 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 16 </td>
   <td style="text-align:left;"> 16103 </td>
   <td style="text-align:right;"> 0.2169 </td>
   <td style="text-align:right;"> 0.0254 </td>
   <td style="text-align:right;"> 11.723 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 04 </td>
   <td style="text-align:left;"> 04103 </td>
   <td style="text-align:right;"> 0.2214 </td>
   <td style="text-align:right;"> 0.0259 </td>
   <td style="text-align:right;"> 11.709 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 10210 </td>
   <td style="text-align:right;"> 0.2788 </td>
   <td style="text-align:right;"> 0.0326 </td>
   <td style="text-align:right;"> 11.708 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05 </td>
   <td style="text-align:left;"> 05802 </td>
   <td style="text-align:right;"> 0.1823 </td>
   <td style="text-align:right;"> 0.0213 </td>
   <td style="text-align:right;"> 11.707 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05 </td>
   <td style="text-align:left;"> 05107 </td>
   <td style="text-align:right;"> 0.1997 </td>
   <td style="text-align:right;"> 0.0234 </td>
   <td style="text-align:right;"> 11.691 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 16 </td>
   <td style="text-align:left;"> 16301 </td>
   <td style="text-align:right;"> 0.2635 </td>
   <td style="text-align:right;"> 0.0308 </td>
   <td style="text-align:right;"> 11.675 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05 </td>
   <td style="text-align:left;"> 05606 </td>
   <td style="text-align:right;"> 0.2043 </td>
   <td style="text-align:right;"> 0.0238 </td>
   <td style="text-align:right;"> 11.667 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 06 </td>
   <td style="text-align:left;"> 06303 </td>
   <td style="text-align:right;"> 0.1958 </td>
   <td style="text-align:right;"> 0.0228 </td>
   <td style="text-align:right;"> 11.657 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05 </td>
   <td style="text-align:left;"> 05109 </td>
   <td style="text-align:right;"> 0.1697 </td>
   <td style="text-align:right;"> 0.0198 </td>
   <td style="text-align:right;"> 11.652 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 08 </td>
   <td style="text-align:left;"> 08106 </td>
   <td style="text-align:right;"> 0.1836 </td>
   <td style="text-align:right;"> 0.0214 </td>
   <td style="text-align:right;"> 11.628 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 06 </td>
   <td style="text-align:left;"> 06201 </td>
   <td style="text-align:right;"> 0.2275 </td>
   <td style="text-align:right;"> 0.0264 </td>
   <td style="text-align:right;"> 11.610 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13120 </td>
   <td style="text-align:right;"> 0.1871 </td>
   <td style="text-align:right;"> 0.0217 </td>
   <td style="text-align:right;"> 11.582 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 06 </td>
   <td style="text-align:left;"> 06105 </td>
   <td style="text-align:right;"> 0.2015 </td>
   <td style="text-align:right;"> 0.0233 </td>
   <td style="text-align:right;"> 11.581 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 10102 </td>
   <td style="text-align:right;"> 0.1399 </td>
   <td style="text-align:right;"> 0.0162 </td>
   <td style="text-align:right;"> 11.558 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 02 </td>
   <td style="text-align:left;"> 02104 </td>
   <td style="text-align:right;"> 0.1713 </td>
   <td style="text-align:right;"> 0.0198 </td>
   <td style="text-align:right;"> 11.554 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 06 </td>
   <td style="text-align:left;"> 06112 </td>
   <td style="text-align:right;"> 0.2075 </td>
   <td style="text-align:right;"> 0.0239 </td>
   <td style="text-align:right;"> 11.501 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13119 </td>
   <td style="text-align:right;"> 0.1584 </td>
   <td style="text-align:right;"> 0.0182 </td>
   <td style="text-align:right;"> 11.501 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 08 </td>
   <td style="text-align:left;"> 08304 </td>
   <td style="text-align:right;"> 0.2034 </td>
   <td style="text-align:right;"> 0.0234 </td>
   <td style="text-align:right;"> 11.488 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 03 </td>
   <td style="text-align:left;"> 03301 </td>
   <td style="text-align:right;"> 0.1536 </td>
   <td style="text-align:right;"> 0.0176 </td>
   <td style="text-align:right;"> 11.450 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 07 </td>
   <td style="text-align:left;"> 07102 </td>
   <td style="text-align:right;"> 0.1897 </td>
   <td style="text-align:right;"> 0.0217 </td>
   <td style="text-align:right;"> 11.419 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13122 </td>
   <td style="text-align:right;"> 0.1944 </td>
   <td style="text-align:right;"> 0.0222 </td>
   <td style="text-align:right;"> 11.415 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 10203 </td>
   <td style="text-align:right;"> 0.2514 </td>
   <td style="text-align:right;"> 0.0287 </td>
   <td style="text-align:right;"> 11.400 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 16 </td>
   <td style="text-align:left;"> 16303 </td>
   <td style="text-align:right;"> 0.2944 </td>
   <td style="text-align:right;"> 0.0335 </td>
   <td style="text-align:right;"> 11.390 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13112 </td>
   <td style="text-align:right;"> 0.2012 </td>
   <td style="text-align:right;"> 0.0229 </td>
   <td style="text-align:right;"> 11.377 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 07 </td>
   <td style="text-align:left;"> 07309 </td>
   <td style="text-align:right;"> 0.1579 </td>
   <td style="text-align:right;"> 0.0180 </td>
   <td style="text-align:right;"> 11.376 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 09 </td>
   <td style="text-align:left;"> 09209 </td>
   <td style="text-align:right;"> 0.1850 </td>
   <td style="text-align:right;"> 0.0210 </td>
   <td style="text-align:right;"> 11.374 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 16 </td>
   <td style="text-align:left;"> 16305 </td>
   <td style="text-align:right;"> 0.2861 </td>
   <td style="text-align:right;"> 0.0325 </td>
   <td style="text-align:right;"> 11.349 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 10208 </td>
   <td style="text-align:right;"> 0.2576 </td>
   <td style="text-align:right;"> 0.0292 </td>
   <td style="text-align:right;"> 11.316 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 09 </td>
   <td style="text-align:left;"> 09109 </td>
   <td style="text-align:right;"> 0.2421 </td>
   <td style="text-align:right;"> 0.0274 </td>
   <td style="text-align:right;"> 11.301 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05 </td>
   <td style="text-align:left;"> 05503 </td>
   <td style="text-align:right;"> 0.2282 </td>
   <td style="text-align:right;"> 0.0258 </td>
   <td style="text-align:right;"> 11.295 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13403 </td>
   <td style="text-align:right;"> 0.2251 </td>
   <td style="text-align:right;"> 0.0254 </td>
   <td style="text-align:right;"> 11.284 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 09 </td>
   <td style="text-align:left;"> 09107 </td>
   <td style="text-align:right;"> 0.2210 </td>
   <td style="text-align:right;"> 0.0249 </td>
   <td style="text-align:right;"> 11.280 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 06 </td>
   <td style="text-align:left;"> 06308 </td>
   <td style="text-align:right;"> 0.1783 </td>
   <td style="text-align:right;"> 0.0200 </td>
   <td style="text-align:right;"> 11.241 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 07 </td>
   <td style="text-align:left;"> 07109 </td>
   <td style="text-align:right;"> 0.2155 </td>
   <td style="text-align:right;"> 0.0242 </td>
   <td style="text-align:right;"> 11.218 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05 </td>
   <td style="text-align:left;"> 05701 </td>
   <td style="text-align:right;"> 0.2125 </td>
   <td style="text-align:right;"> 0.0238 </td>
   <td style="text-align:right;"> 11.207 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 09 </td>
   <td style="text-align:left;"> 09211 </td>
   <td style="text-align:right;"> 0.2197 </td>
   <td style="text-align:right;"> 0.0246 </td>
   <td style="text-align:right;"> 11.204 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 08 </td>
   <td style="text-align:left;"> 08201 </td>
   <td style="text-align:right;"> 0.2079 </td>
   <td style="text-align:right;"> 0.0233 </td>
   <td style="text-align:right;"> 11.199 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 08 </td>
   <td style="text-align:left;"> 08205 </td>
   <td style="text-align:right;"> 0.2221 </td>
   <td style="text-align:right;"> 0.0248 </td>
   <td style="text-align:right;"> 11.189 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05 </td>
   <td style="text-align:left;"> 05303 </td>
   <td style="text-align:right;"> 0.2067 </td>
   <td style="text-align:right;"> 0.0231 </td>
   <td style="text-align:right;"> 11.175 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 07 </td>
   <td style="text-align:left;"> 07110 </td>
   <td style="text-align:right;"> 0.1958 </td>
   <td style="text-align:right;"> 0.0218 </td>
   <td style="text-align:right;"> 11.152 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 07 </td>
   <td style="text-align:left;"> 07201 </td>
   <td style="text-align:right;"> 0.1861 </td>
   <td style="text-align:right;"> 0.0207 </td>
   <td style="text-align:right;"> 11.136 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 14 </td>
   <td style="text-align:left;"> 14107 </td>
   <td style="text-align:right;"> 0.1883 </td>
   <td style="text-align:right;"> 0.0209 </td>
   <td style="text-align:right;"> 11.094 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13602 </td>
   <td style="text-align:right;"> 0.2187 </td>
   <td style="text-align:right;"> 0.0242 </td>
   <td style="text-align:right;"> 11.081 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05 </td>
   <td style="text-align:left;"> 05601 </td>
   <td style="text-align:right;"> 0.1717 </td>
   <td style="text-align:right;"> 0.0190 </td>
   <td style="text-align:right;"> 11.066 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13132 </td>
   <td style="text-align:right;"> 0.2319 </td>
   <td style="text-align:right;"> 0.0257 </td>
   <td style="text-align:right;"> 11.066 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 03 </td>
   <td style="text-align:left;"> 03201 </td>
   <td style="text-align:right;"> 0.1707 </td>
   <td style="text-align:right;"> 0.0187 </td>
   <td style="text-align:right;"> 10.964 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 07 </td>
   <td style="text-align:left;"> 07403 </td>
   <td style="text-align:right;"> 0.1796 </td>
   <td style="text-align:right;"> 0.0196 </td>
   <td style="text-align:right;"> 10.928 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 03 </td>
   <td style="text-align:left;"> 03202 </td>
   <td style="text-align:right;"> 0.1777 </td>
   <td style="text-align:right;"> 0.0194 </td>
   <td style="text-align:right;"> 10.905 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 14 </td>
   <td style="text-align:left;"> 14204 </td>
   <td style="text-align:right;"> 0.2036 </td>
   <td style="text-align:right;"> 0.0222 </td>
   <td style="text-align:right;"> 10.905 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 03 </td>
   <td style="text-align:left;"> 03303 </td>
   <td style="text-align:right;"> 0.2060 </td>
   <td style="text-align:right;"> 0.0225 </td>
   <td style="text-align:right;"> 10.902 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 08 </td>
   <td style="text-align:left;"> 08309 </td>
   <td style="text-align:right;"> 0.1229 </td>
   <td style="text-align:right;"> 0.0134 </td>
   <td style="text-align:right;"> 10.886 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 09 </td>
   <td style="text-align:left;"> 09117 </td>
   <td style="text-align:right;"> 0.2018 </td>
   <td style="text-align:right;"> 0.0220 </td>
   <td style="text-align:right;"> 10.880 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 09 </td>
   <td style="text-align:left;"> 09105 </td>
   <td style="text-align:right;"> 0.2799 </td>
   <td style="text-align:right;"> 0.0304 </td>
   <td style="text-align:right;"> 10.866 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05 </td>
   <td style="text-align:left;"> 05302 </td>
   <td style="text-align:right;"> 0.1892 </td>
   <td style="text-align:right;"> 0.0205 </td>
   <td style="text-align:right;"> 10.859 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 06 </td>
   <td style="text-align:left;"> 06111 </td>
   <td style="text-align:right;"> 0.1708 </td>
   <td style="text-align:right;"> 0.0185 </td>
   <td style="text-align:right;"> 10.837 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 07 </td>
   <td style="text-align:left;"> 07308 </td>
   <td style="text-align:right;"> 0.1973 </td>
   <td style="text-align:right;"> 0.0213 </td>
   <td style="text-align:right;"> 10.816 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 09 </td>
   <td style="text-align:left;"> 09118 </td>
   <td style="text-align:right;"> 0.2439 </td>
   <td style="text-align:right;"> 0.0264 </td>
   <td style="text-align:right;"> 10.805 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 08 </td>
   <td style="text-align:left;"> 08203 </td>
   <td style="text-align:right;"> 0.2532 </td>
   <td style="text-align:right;"> 0.0273 </td>
   <td style="text-align:right;"> 10.792 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 06 </td>
   <td style="text-align:left;"> 06115 </td>
   <td style="text-align:right;"> 0.2042 </td>
   <td style="text-align:right;"> 0.0219 </td>
   <td style="text-align:right;"> 10.750 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13601 </td>
   <td style="text-align:right;"> 0.2036 </td>
   <td style="text-align:right;"> 0.0218 </td>
   <td style="text-align:right;"> 10.709 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05 </td>
   <td style="text-align:left;"> 05504 </td>
   <td style="text-align:right;"> 0.1570 </td>
   <td style="text-align:right;"> 0.0168 </td>
   <td style="text-align:right;"> 10.695 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 07 </td>
   <td style="text-align:left;"> 07108 </td>
   <td style="text-align:right;"> 0.2024 </td>
   <td style="text-align:right;"> 0.0216 </td>
   <td style="text-align:right;"> 10.688 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05 </td>
   <td style="text-align:left;"> 05803 </td>
   <td style="text-align:right;"> 0.3554 </td>
   <td style="text-align:right;"> 0.0379 </td>
   <td style="text-align:right;"> 10.666 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 06 </td>
   <td style="text-align:left;"> 06310 </td>
   <td style="text-align:right;"> 0.2191 </td>
   <td style="text-align:right;"> 0.0234 </td>
   <td style="text-align:right;"> 10.657 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13128 </td>
   <td style="text-align:right;"> 0.2197 </td>
   <td style="text-align:right;"> 0.0234 </td>
   <td style="text-align:right;"> 10.652 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 08 </td>
   <td style="text-align:left;"> 08111 </td>
   <td style="text-align:right;"> 0.1654 </td>
   <td style="text-align:right;"> 0.0176 </td>
   <td style="text-align:right;"> 10.651 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 09 </td>
   <td style="text-align:left;"> 09104 </td>
   <td style="text-align:right;"> 0.2634 </td>
   <td style="text-align:right;"> 0.0280 </td>
   <td style="text-align:right;"> 10.629 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 04 </td>
   <td style="text-align:left;"> 04202 </td>
   <td style="text-align:right;"> 0.1852 </td>
   <td style="text-align:right;"> 0.0196 </td>
   <td style="text-align:right;"> 10.606 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 08 </td>
   <td style="text-align:left;"> 08310 </td>
   <td style="text-align:right;"> 0.2313 </td>
   <td style="text-align:right;"> 0.0245 </td>
   <td style="text-align:right;"> 10.601 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 08 </td>
   <td style="text-align:left;"> 08107 </td>
   <td style="text-align:right;"> 0.1707 </td>
   <td style="text-align:right;"> 0.0181 </td>
   <td style="text-align:right;"> 10.595 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 08 </td>
   <td style="text-align:left;"> 08311 </td>
   <td style="text-align:right;"> 0.2039 </td>
   <td style="text-align:right;"> 0.0216 </td>
   <td style="text-align:right;"> 10.585 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05 </td>
   <td style="text-align:left;"> 05804 </td>
   <td style="text-align:right;"> 0.1835 </td>
   <td style="text-align:right;"> 0.0194 </td>
   <td style="text-align:right;"> 10.581 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 09 </td>
   <td style="text-align:left;"> 09102 </td>
   <td style="text-align:right;"> 0.2496 </td>
   <td style="text-align:right;"> 0.0264 </td>
   <td style="text-align:right;"> 10.572 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13102 </td>
   <td style="text-align:right;"> 0.1796 </td>
   <td style="text-align:right;"> 0.0190 </td>
   <td style="text-align:right;"> 10.562 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 02 </td>
   <td style="text-align:left;"> 02201 </td>
   <td style="text-align:right;"> 0.2191 </td>
   <td style="text-align:right;"> 0.0231 </td>
   <td style="text-align:right;"> 10.561 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13201 </td>
   <td style="text-align:right;"> 0.2137 </td>
   <td style="text-align:right;"> 0.0226 </td>
   <td style="text-align:right;"> 10.560 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 14 </td>
   <td style="text-align:left;"> 14203 </td>
   <td style="text-align:right;"> 0.2079 </td>
   <td style="text-align:right;"> 0.0220 </td>
   <td style="text-align:right;"> 10.559 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 06 </td>
   <td style="text-align:left;"> 06304 </td>
   <td style="text-align:right;"> 0.1540 </td>
   <td style="text-align:right;"> 0.0163 </td>
   <td style="text-align:right;"> 10.553 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 07 </td>
   <td style="text-align:left;"> 07101 </td>
   <td style="text-align:right;"> 0.1897 </td>
   <td style="text-align:right;"> 0.0200 </td>
   <td style="text-align:right;"> 10.527 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 09 </td>
   <td style="text-align:left;"> 09119 </td>
   <td style="text-align:right;"> 0.2347 </td>
   <td style="text-align:right;"> 0.0247 </td>
   <td style="text-align:right;"> 10.518 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 07 </td>
   <td style="text-align:left;"> 07304 </td>
   <td style="text-align:right;"> 0.1755 </td>
   <td style="text-align:right;"> 0.0185 </td>
   <td style="text-align:right;"> 10.515 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 08 </td>
   <td style="text-align:left;"> 08204 </td>
   <td style="text-align:right;"> 0.2144 </td>
   <td style="text-align:right;"> 0.0225 </td>
   <td style="text-align:right;"> 10.494 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 07 </td>
   <td style="text-align:left;"> 07404 </td>
   <td style="text-align:right;"> 0.2584 </td>
   <td style="text-align:right;"> 0.0270 </td>
   <td style="text-align:right;"> 10.464 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 16 </td>
   <td style="text-align:left;"> 16304 </td>
   <td style="text-align:right;"> 0.2664 </td>
   <td style="text-align:right;"> 0.0279 </td>
   <td style="text-align:right;"> 10.462 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13129 </td>
   <td style="text-align:right;"> 0.1986 </td>
   <td style="text-align:right;"> 0.0208 </td>
   <td style="text-align:right;"> 10.454 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 07 </td>
   <td style="text-align:left;"> 07106 </td>
   <td style="text-align:right;"> 0.2462 </td>
   <td style="text-align:right;"> 0.0257 </td>
   <td style="text-align:right;"> 10.442 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 08 </td>
   <td style="text-align:left;"> 08313 </td>
   <td style="text-align:right;"> 0.2103 </td>
   <td style="text-align:right;"> 0.0219 </td>
   <td style="text-align:right;"> 10.425 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 11 </td>
   <td style="text-align:left;"> 11301 </td>
   <td style="text-align:right;"> 0.1471 </td>
   <td style="text-align:right;"> 0.0153 </td>
   <td style="text-align:right;"> 10.397 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13202 </td>
   <td style="text-align:right;"> 0.2066 </td>
   <td style="text-align:right;"> 0.0214 </td>
   <td style="text-align:right;"> 10.375 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13104 </td>
   <td style="text-align:right;"> 0.2237 </td>
   <td style="text-align:right;"> 0.0232 </td>
   <td style="text-align:right;"> 10.367 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 08 </td>
   <td style="text-align:left;"> 08105 </td>
   <td style="text-align:right;"> 0.2105 </td>
   <td style="text-align:right;"> 0.0218 </td>
   <td style="text-align:right;"> 10.350 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 06 </td>
   <td style="text-align:left;"> 06301 </td>
   <td style="text-align:right;"> 0.2328 </td>
   <td style="text-align:right;"> 0.0241 </td>
   <td style="text-align:right;"> 10.335 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 06 </td>
   <td style="text-align:left;"> 06117 </td>
   <td style="text-align:right;"> 0.2092 </td>
   <td style="text-align:right;"> 0.0216 </td>
   <td style="text-align:right;"> 10.328 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05 </td>
   <td style="text-align:left;"> 05501 </td>
   <td style="text-align:right;"> 0.1245 </td>
   <td style="text-align:right;"> 0.0129 </td>
   <td style="text-align:right;"> 10.325 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 10302 </td>
   <td style="text-align:right;"> 0.2053 </td>
   <td style="text-align:right;"> 0.0211 </td>
   <td style="text-align:right;"> 10.287 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 01 </td>
   <td style="text-align:left;"> 01403 </td>
   <td style="text-align:right;"> 0.1831 </td>
   <td style="text-align:right;"> 0.0188 </td>
   <td style="text-align:right;"> 10.244 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 02 </td>
   <td style="text-align:left;"> 02103 </td>
   <td style="text-align:right;"> 0.1745 </td>
   <td style="text-align:right;"> 0.0179 </td>
   <td style="text-align:right;"> 10.228 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 08 </td>
   <td style="text-align:left;"> 08302 </td>
   <td style="text-align:right;"> 0.2138 </td>
   <td style="text-align:right;"> 0.0218 </td>
   <td style="text-align:right;"> 10.191 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 09 </td>
   <td style="text-align:left;"> 09210 </td>
   <td style="text-align:right;"> 0.1756 </td>
   <td style="text-align:right;"> 0.0178 </td>
   <td style="text-align:right;"> 10.167 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 07 </td>
   <td style="text-align:left;"> 07103 </td>
   <td style="text-align:right;"> 0.2258 </td>
   <td style="text-align:right;"> 0.0228 </td>
   <td style="text-align:right;"> 10.114 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 10301 </td>
   <td style="text-align:right;"> 0.2148 </td>
   <td style="text-align:right;"> 0.0217 </td>
   <td style="text-align:right;"> 10.109 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 16 </td>
   <td style="text-align:left;"> 16202 </td>
   <td style="text-align:right;"> 0.2749 </td>
   <td style="text-align:right;"> 0.0277 </td>
   <td style="text-align:right;"> 10.079 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 03 </td>
   <td style="text-align:left;"> 03302 </td>
   <td style="text-align:right;"> 0.2517 </td>
   <td style="text-align:right;"> 0.0254 </td>
   <td style="text-align:right;"> 10.077 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 10101 </td>
   <td style="text-align:right;"> 0.2083 </td>
   <td style="text-align:right;"> 0.0209 </td>
   <td style="text-align:right;"> 10.056 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 07 </td>
   <td style="text-align:left;"> 07406 </td>
   <td style="text-align:right;"> 0.2617 </td>
   <td style="text-align:right;"> 0.0263 </td>
   <td style="text-align:right;"> 10.048 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 16 </td>
   <td style="text-align:left;"> 16207 </td>
   <td style="text-align:right;"> 0.2987 </td>
   <td style="text-align:right;"> 0.0300 </td>
   <td style="text-align:right;"> 10.033 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 07 </td>
   <td style="text-align:left;"> 07107 </td>
   <td style="text-align:right;"> 0.2439 </td>
   <td style="text-align:right;"> 0.0245 </td>
   <td style="text-align:right;"> 10.026 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 10303 </td>
   <td style="text-align:right;"> 0.2845 </td>
   <td style="text-align:right;"> 0.0285 </td>
   <td style="text-align:right;"> 10.013 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 08 </td>
   <td style="text-align:left;"> 08108 </td>
   <td style="text-align:right;"> 0.1986 </td>
   <td style="text-align:right;"> 0.0199 </td>
   <td style="text-align:right;"> 10.003 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 02 </td>
   <td style="text-align:left;"> 02301 </td>
   <td style="text-align:right;"> 0.1866 </td>
   <td style="text-align:right;"> 0.0187 </td>
   <td style="text-align:right;"> 10.001 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 06 </td>
   <td style="text-align:left;"> 06106 </td>
   <td style="text-align:right;"> 0.1742 </td>
   <td style="text-align:right;"> 0.0174 </td>
   <td style="text-align:right;"> 9.993 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13107 </td>
   <td style="text-align:right;"> 0.2151 </td>
   <td style="text-align:right;"> 0.0215 </td>
   <td style="text-align:right;"> 9.984 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 10202 </td>
   <td style="text-align:right;"> 0.3234 </td>
   <td style="text-align:right;"> 0.0323 </td>
   <td style="text-align:right;"> 9.980 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 10403 </td>
   <td style="text-align:right;"> 0.2438 </td>
   <td style="text-align:right;"> 0.0243 </td>
   <td style="text-align:right;"> 9.947 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 07 </td>
   <td style="text-align:left;"> 07306 </td>
   <td style="text-align:right;"> 0.2335 </td>
   <td style="text-align:right;"> 0.0231 </td>
   <td style="text-align:right;"> 9.907 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 10201 </td>
   <td style="text-align:right;"> 0.2318 </td>
   <td style="text-align:right;"> 0.0230 </td>
   <td style="text-align:right;"> 9.903 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 09 </td>
   <td style="text-align:left;"> 09204 </td>
   <td style="text-align:right;"> 0.2490 </td>
   <td style="text-align:right;"> 0.0246 </td>
   <td style="text-align:right;"> 9.898 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 04 </td>
   <td style="text-align:left;"> 04302 </td>
   <td style="text-align:right;"> 0.2035 </td>
   <td style="text-align:right;"> 0.0201 </td>
   <td style="text-align:right;"> 9.871 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 07 </td>
   <td style="text-align:left;"> 07303 </td>
   <td style="text-align:right;"> 0.2237 </td>
   <td style="text-align:right;"> 0.0221 </td>
   <td style="text-align:right;"> 9.866 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 08 </td>
   <td style="text-align:left;"> 08301 </td>
   <td style="text-align:right;"> 0.1978 </td>
   <td style="text-align:right;"> 0.0195 </td>
   <td style="text-align:right;"> 9.864 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 01 </td>
   <td style="text-align:left;"> 01404 </td>
   <td style="text-align:right;"> 0.1521 </td>
   <td style="text-align:right;"> 0.0150 </td>
   <td style="text-align:right;"> 9.851 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 10205 </td>
   <td style="text-align:right;"> 0.3331 </td>
   <td style="text-align:right;"> 0.0327 </td>
   <td style="text-align:right;"> 9.804 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 08 </td>
   <td style="text-align:left;"> 08207 </td>
   <td style="text-align:right;"> 0.2254 </td>
   <td style="text-align:right;"> 0.0221 </td>
   <td style="text-align:right;"> 9.800 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13604 </td>
   <td style="text-align:right;"> 0.2359 </td>
   <td style="text-align:right;"> 0.0231 </td>
   <td style="text-align:right;"> 9.785 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 06 </td>
   <td style="text-align:left;"> 06102 </td>
   <td style="text-align:right;"> 0.2894 </td>
   <td style="text-align:right;"> 0.0283 </td>
   <td style="text-align:right;"> 9.782 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 10402 </td>
   <td style="text-align:right;"> 0.2430 </td>
   <td style="text-align:right;"> 0.0237 </td>
   <td style="text-align:right;"> 9.775 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 06 </td>
   <td style="text-align:left;"> 06108 </td>
   <td style="text-align:right;"> 0.2148 </td>
   <td style="text-align:right;"> 0.0209 </td>
   <td style="text-align:right;"> 9.745 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 14 </td>
   <td style="text-align:left;"> 14108 </td>
   <td style="text-align:right;"> 0.1660 </td>
   <td style="text-align:right;"> 0.0162 </td>
   <td style="text-align:right;"> 9.743 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05 </td>
   <td style="text-align:left;"> 05502 </td>
   <td style="text-align:right;"> 0.2268 </td>
   <td style="text-align:right;"> 0.0221 </td>
   <td style="text-align:right;"> 9.739 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13130 </td>
   <td style="text-align:right;"> 0.2283 </td>
   <td style="text-align:right;"> 0.0222 </td>
   <td style="text-align:right;"> 9.716 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05 </td>
   <td style="text-align:left;"> 05101 </td>
   <td style="text-align:right;"> 0.2033 </td>
   <td style="text-align:right;"> 0.0197 </td>
   <td style="text-align:right;"> 9.712 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 08 </td>
   <td style="text-align:left;"> 08110 </td>
   <td style="text-align:right;"> 0.2167 </td>
   <td style="text-align:right;"> 0.0210 </td>
   <td style="text-align:right;"> 9.711 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 08 </td>
   <td style="text-align:left;"> 08112 </td>
   <td style="text-align:right;"> 0.1972 </td>
   <td style="text-align:right;"> 0.0191 </td>
   <td style="text-align:right;"> 9.675 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 02 </td>
   <td style="text-align:left;"> 02202 </td>
   <td style="text-align:right;"> 0.2026 </td>
   <td style="text-align:right;"> 0.0195 </td>
   <td style="text-align:right;"> 9.626 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 09 </td>
   <td style="text-align:left;"> 09110 </td>
   <td style="text-align:right;"> 0.2245 </td>
   <td style="text-align:right;"> 0.0216 </td>
   <td style="text-align:right;"> 9.607 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 08 </td>
   <td style="text-align:left;"> 08306 </td>
   <td style="text-align:right;"> 0.2231 </td>
   <td style="text-align:right;"> 0.0214 </td>
   <td style="text-align:right;"> 9.587 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 08 </td>
   <td style="text-align:left;"> 08206 </td>
   <td style="text-align:right;"> 0.2160 </td>
   <td style="text-align:right;"> 0.0206 </td>
   <td style="text-align:right;"> 9.545 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 10106 </td>
   <td style="text-align:right;"> 0.1765 </td>
   <td style="text-align:right;"> 0.0168 </td>
   <td style="text-align:right;"> 9.524 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 14 </td>
   <td style="text-align:left;"> 14202 </td>
   <td style="text-align:right;"> 0.1883 </td>
   <td style="text-align:right;"> 0.0179 </td>
   <td style="text-align:right;"> 9.521 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 10304 </td>
   <td style="text-align:right;"> 0.3727 </td>
   <td style="text-align:right;"> 0.0354 </td>
   <td style="text-align:right;"> 9.504 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 09 </td>
   <td style="text-align:left;"> 09121 </td>
   <td style="text-align:right;"> 0.1603 </td>
   <td style="text-align:right;"> 0.0152 </td>
   <td style="text-align:right;"> 9.503 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 08 </td>
   <td style="text-align:left;"> 08308 </td>
   <td style="text-align:right;"> 0.2870 </td>
   <td style="text-align:right;"> 0.0272 </td>
   <td style="text-align:right;"> 9.476 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05 </td>
   <td style="text-align:left;"> 05801 </td>
   <td style="text-align:right;"> 0.2313 </td>
   <td style="text-align:right;"> 0.0219 </td>
   <td style="text-align:right;"> 9.462 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 16 </td>
   <td style="text-align:left;"> 16101 </td>
   <td style="text-align:right;"> 0.2758 </td>
   <td style="text-align:right;"> 0.0260 </td>
   <td style="text-align:right;"> 9.433 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 04 </td>
   <td style="text-align:left;"> 04105 </td>
   <td style="text-align:right;"> 0.2040 </td>
   <td style="text-align:right;"> 0.0192 </td>
   <td style="text-align:right;"> 9.413 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 06 </td>
   <td style="text-align:left;"> 06305 </td>
   <td style="text-align:right;"> 0.2027 </td>
   <td style="text-align:right;"> 0.0190 </td>
   <td style="text-align:right;"> 9.352 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 04 </td>
   <td style="text-align:left;"> 04102 </td>
   <td style="text-align:right;"> 0.1893 </td>
   <td style="text-align:right;"> 0.0176 </td>
   <td style="text-align:right;"> 9.281 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13103 </td>
   <td style="text-align:right;"> 0.2080 </td>
   <td style="text-align:right;"> 0.0192 </td>
   <td style="text-align:right;"> 9.235 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 16 </td>
   <td style="text-align:left;"> 16201 </td>
   <td style="text-align:right;"> 0.3251 </td>
   <td style="text-align:right;"> 0.0300 </td>
   <td style="text-align:right;"> 9.221 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13502 </td>
   <td style="text-align:right;"> 0.2506 </td>
   <td style="text-align:right;"> 0.0231 </td>
   <td style="text-align:right;"> 9.206 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 10401 </td>
   <td style="text-align:right;"> 0.2621 </td>
   <td style="text-align:right;"> 0.0241 </td>
   <td style="text-align:right;"> 9.195 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 08 </td>
   <td style="text-align:left;"> 08305 </td>
   <td style="text-align:right;"> 0.2047 </td>
   <td style="text-align:right;"> 0.0188 </td>
   <td style="text-align:right;"> 9.174 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 09 </td>
   <td style="text-align:left;"> 09202 </td>
   <td style="text-align:right;"> 0.2064 </td>
   <td style="text-align:right;"> 0.0189 </td>
   <td style="text-align:right;"> 9.171 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 11 </td>
   <td style="text-align:left;"> 11201 </td>
   <td style="text-align:right;"> 0.2239 </td>
   <td style="text-align:right;"> 0.0205 </td>
   <td style="text-align:right;"> 9.162 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13101 </td>
   <td style="text-align:right;"> 0.1888 </td>
   <td style="text-align:right;"> 0.0173 </td>
   <td style="text-align:right;"> 9.160 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 14 </td>
   <td style="text-align:left;"> 14201 </td>
   <td style="text-align:right;"> 0.2096 </td>
   <td style="text-align:right;"> 0.0192 </td>
   <td style="text-align:right;"> 9.149 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05 </td>
   <td style="text-align:left;"> 05602 </td>
   <td style="text-align:right;"> 0.1794 </td>
   <td style="text-align:right;"> 0.0164 </td>
   <td style="text-align:right;"> 9.130 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 07 </td>
   <td style="text-align:left;"> 07302 </td>
   <td style="text-align:right;"> 0.1492 </td>
   <td style="text-align:right;"> 0.0136 </td>
   <td style="text-align:right;"> 9.129 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 07 </td>
   <td style="text-align:left;"> 07401 </td>
   <td style="text-align:right;"> 0.1684 </td>
   <td style="text-align:right;"> 0.0153 </td>
   <td style="text-align:right;"> 9.096 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 06 </td>
   <td style="text-align:left;"> 06114 </td>
   <td style="text-align:right;"> 0.1906 </td>
   <td style="text-align:right;"> 0.0173 </td>
   <td style="text-align:right;"> 9.054 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 06 </td>
   <td style="text-align:left;"> 06101 </td>
   <td style="text-align:right;"> 0.1860 </td>
   <td style="text-align:right;"> 0.0168 </td>
   <td style="text-align:right;"> 9.034 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13401 </td>
   <td style="text-align:right;"> 0.2125 </td>
   <td style="text-align:right;"> 0.0189 </td>
   <td style="text-align:right;"> 8.914 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 06 </td>
   <td style="text-align:left;"> 06309 </td>
   <td style="text-align:right;"> 0.1977 </td>
   <td style="text-align:right;"> 0.0174 </td>
   <td style="text-align:right;"> 8.824 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 10109 </td>
   <td style="text-align:right;"> 0.2138 </td>
   <td style="text-align:right;"> 0.0188 </td>
   <td style="text-align:right;"> 8.813 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13302 </td>
   <td style="text-align:right;"> 0.1875 </td>
   <td style="text-align:right;"> 0.0165 </td>
   <td style="text-align:right;"> 8.807 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 08 </td>
   <td style="text-align:left;"> 08303 </td>
   <td style="text-align:right;"> 0.2377 </td>
   <td style="text-align:right;"> 0.0209 </td>
   <td style="text-align:right;"> 8.770 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 16 </td>
   <td style="text-align:left;"> 16204 </td>
   <td style="text-align:right;"> 0.3113 </td>
   <td style="text-align:right;"> 0.0273 </td>
   <td style="text-align:right;"> 8.762 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 07 </td>
   <td style="text-align:left;"> 07307 </td>
   <td style="text-align:right;"> 0.2349 </td>
   <td style="text-align:right;"> 0.0206 </td>
   <td style="text-align:right;"> 8.757 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 10207 </td>
   <td style="text-align:right;"> 0.2547 </td>
   <td style="text-align:right;"> 0.0223 </td>
   <td style="text-align:right;"> 8.751 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13113 </td>
   <td style="text-align:right;"> 0.2226 </td>
   <td style="text-align:right;"> 0.0193 </td>
   <td style="text-align:right;"> 8.682 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 07 </td>
   <td style="text-align:left;"> 07402 </td>
   <td style="text-align:right;"> 0.1609 </td>
   <td style="text-align:right;"> 0.0140 </td>
   <td style="text-align:right;"> 8.679 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 09 </td>
   <td style="text-align:left;"> 09101 </td>
   <td style="text-align:right;"> 0.2715 </td>
   <td style="text-align:right;"> 0.0235 </td>
   <td style="text-align:right;"> 8.646 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 01 </td>
   <td style="text-align:left;"> 01401 </td>
   <td style="text-align:right;"> 0.2207 </td>
   <td style="text-align:right;"> 0.0189 </td>
   <td style="text-align:right;"> 8.567 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 09 </td>
   <td style="text-align:left;"> 09114 </td>
   <td style="text-align:right;"> 0.1632 </td>
   <td style="text-align:right;"> 0.0140 </td>
   <td style="text-align:right;"> 8.567 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 04 </td>
   <td style="text-align:left;"> 04204 </td>
   <td style="text-align:right;"> 0.2142 </td>
   <td style="text-align:right;"> 0.0183 </td>
   <td style="text-align:right;"> 8.560 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 07 </td>
   <td style="text-align:left;"> 07105 </td>
   <td style="text-align:right;"> 0.1723 </td>
   <td style="text-align:right;"> 0.0147 </td>
   <td style="text-align:right;"> 8.547 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 06 </td>
   <td style="text-align:left;"> 06103 </td>
   <td style="text-align:right;"> 0.2631 </td>
   <td style="text-align:right;"> 0.0225 </td>
   <td style="text-align:right;"> 8.546 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 04 </td>
   <td style="text-align:left;"> 04106 </td>
   <td style="text-align:right;"> 0.1760 </td>
   <td style="text-align:right;"> 0.0150 </td>
   <td style="text-align:right;"> 8.495 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05 </td>
   <td style="text-align:left;"> 05102 </td>
   <td style="text-align:right;"> 0.1633 </td>
   <td style="text-align:right;"> 0.0139 </td>
   <td style="text-align:right;"> 8.494 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 07 </td>
   <td style="text-align:left;"> 07202 </td>
   <td style="text-align:right;"> 0.2263 </td>
   <td style="text-align:right;"> 0.0192 </td>
   <td style="text-align:right;"> 8.468 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 09 </td>
   <td style="text-align:left;"> 09120 </td>
   <td style="text-align:right;"> 0.2090 </td>
   <td style="text-align:right;"> 0.0176 </td>
   <td style="text-align:right;"> 8.417 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13131 </td>
   <td style="text-align:right;"> 0.1822 </td>
   <td style="text-align:right;"> 0.0153 </td>
   <td style="text-align:right;"> 8.416 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05 </td>
   <td style="text-align:left;"> 05401 </td>
   <td style="text-align:right;"> 0.2249 </td>
   <td style="text-align:right;"> 0.0189 </td>
   <td style="text-align:right;"> 8.388 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 08 </td>
   <td style="text-align:left;"> 08103 </td>
   <td style="text-align:right;"> 0.1731 </td>
   <td style="text-align:right;"> 0.0145 </td>
   <td style="text-align:right;"> 8.380 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 16 </td>
   <td style="text-align:left;"> 16205 </td>
   <td style="text-align:right;"> 0.3329 </td>
   <td style="text-align:right;"> 0.0279 </td>
   <td style="text-align:right;"> 8.366 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13109 </td>
   <td style="text-align:right;"> 0.1774 </td>
   <td style="text-align:right;"> 0.0148 </td>
   <td style="text-align:right;"> 8.335 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 10103 </td>
   <td style="text-align:right;"> 0.3146 </td>
   <td style="text-align:right;"> 0.0262 </td>
   <td style="text-align:right;"> 8.329 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 10206 </td>
   <td style="text-align:right;"> 0.3124 </td>
   <td style="text-align:right;"> 0.0260 </td>
   <td style="text-align:right;"> 8.312 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 06 </td>
   <td style="text-align:left;"> 06202 </td>
   <td style="text-align:right;"> 0.2540 </td>
   <td style="text-align:right;"> 0.0207 </td>
   <td style="text-align:right;"> 8.170 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 08 </td>
   <td style="text-align:left;"> 08202 </td>
   <td style="text-align:right;"> 0.1785 </td>
   <td style="text-align:right;"> 0.0145 </td>
   <td style="text-align:right;"> 8.113 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13124 </td>
   <td style="text-align:right;"> 0.1701 </td>
   <td style="text-align:right;"> 0.0138 </td>
   <td style="text-align:right;"> 8.106 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 08 </td>
   <td style="text-align:left;"> 08104 </td>
   <td style="text-align:right;"> 0.1229 </td>
   <td style="text-align:right;"> 0.0098 </td>
   <td style="text-align:right;"> 7.991 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 04 </td>
   <td style="text-align:left;"> 04305 </td>
   <td style="text-align:right;"> 0.2471 </td>
   <td style="text-align:right;"> 0.0196 </td>
   <td style="text-align:right;"> 7.936 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 10404 </td>
   <td style="text-align:right;"> 0.3792 </td>
   <td style="text-align:right;"> 0.0298 </td>
   <td style="text-align:right;"> 7.856 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 08 </td>
   <td style="text-align:left;"> 08314 </td>
   <td style="text-align:right;"> 0.1734 </td>
   <td style="text-align:right;"> 0.0136 </td>
   <td style="text-align:right;"> 7.825 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05 </td>
   <td style="text-align:left;"> 05104 </td>
   <td style="text-align:right;"> 0.1992 </td>
   <td style="text-align:right;"> 0.0154 </td>
   <td style="text-align:right;"> 7.729 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 04 </td>
   <td style="text-align:left;"> 04101 </td>
   <td style="text-align:right;"> 0.1936 </td>
   <td style="text-align:right;"> 0.0148 </td>
   <td style="text-align:right;"> 7.649 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05 </td>
   <td style="text-align:left;"> 05702 </td>
   <td style="text-align:right;"> 0.2389 </td>
   <td style="text-align:right;"> 0.0180 </td>
   <td style="text-align:right;"> 7.536 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 09 </td>
   <td style="text-align:left;"> 09208 </td>
   <td style="text-align:right;"> 0.3279 </td>
   <td style="text-align:right;"> 0.0245 </td>
   <td style="text-align:right;"> 7.484 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 10204 </td>
   <td style="text-align:right;"> 0.3254 </td>
   <td style="text-align:right;"> 0.0242 </td>
   <td style="text-align:right;"> 7.439 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 06 </td>
   <td style="text-align:left;"> 06113 </td>
   <td style="text-align:right;"> 0.2558 </td>
   <td style="text-align:right;"> 0.0190 </td>
   <td style="text-align:right;"> 7.435 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 09 </td>
   <td style="text-align:left;"> 09106 </td>
   <td style="text-align:right;"> 0.3511 </td>
   <td style="text-align:right;"> 0.0259 </td>
   <td style="text-align:right;"> 7.374 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05 </td>
   <td style="text-align:left;"> 05605 </td>
   <td style="text-align:right;"> 0.2371 </td>
   <td style="text-align:right;"> 0.0173 </td>
   <td style="text-align:right;"> 7.312 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05 </td>
   <td style="text-align:left;"> 05403 </td>
   <td style="text-align:right;"> 0.2584 </td>
   <td style="text-align:right;"> 0.0184 </td>
   <td style="text-align:right;"> 7.106 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 07 </td>
   <td style="text-align:left;"> 07203 </td>
   <td style="text-align:right;"> 0.2007 </td>
   <td style="text-align:right;"> 0.0142 </td>
   <td style="text-align:right;"> 7.095 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13123 </td>
   <td style="text-align:right;"> 0.1791 </td>
   <td style="text-align:right;"> 0.0126 </td>
   <td style="text-align:right;"> 7.054 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 08 </td>
   <td style="text-align:left;"> 08312 </td>
   <td style="text-align:right;"> 0.1359 </td>
   <td style="text-align:right;"> 0.0092 </td>
   <td style="text-align:right;"> 6.807 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 09 </td>
   <td style="text-align:left;"> 09203 </td>
   <td style="text-align:right;"> 0.2287 </td>
   <td style="text-align:right;"> 0.0154 </td>
   <td style="text-align:right;"> 6.755 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13402 </td>
   <td style="text-align:right;"> 0.2481 </td>
   <td style="text-align:right;"> 0.0164 </td>
   <td style="text-align:right;"> 6.624 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13203 </td>
   <td style="text-align:right;"> 0.1846 </td>
   <td style="text-align:right;"> 0.0122 </td>
   <td style="text-align:right;"> 6.585 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13125 </td>
   <td style="text-align:right;"> 0.1763 </td>
   <td style="text-align:right;"> 0.0114 </td>
   <td style="text-align:right;"> 6.446 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 14 </td>
   <td style="text-align:left;"> 14101 </td>
   <td style="text-align:right;"> 0.2577 </td>
   <td style="text-align:right;"> 0.0165 </td>
   <td style="text-align:right;"> 6.401 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 01 </td>
   <td style="text-align:left;"> 01107 </td>
   <td style="text-align:right;"> 0.2696 </td>
   <td style="text-align:right;"> 0.0163 </td>
   <td style="text-align:right;"> 6.056 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05 </td>
   <td style="text-align:left;"> 05704 </td>
   <td style="text-align:right;"> 0.2266 </td>
   <td style="text-align:right;"> 0.0136 </td>
   <td style="text-align:right;"> 6.013 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 08 </td>
   <td style="text-align:left;"> 08101 </td>
   <td style="text-align:right;"> 0.2297 </td>
   <td style="text-align:right;"> 0.0137 </td>
   <td style="text-align:right;"> 5.948 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 07 </td>
   <td style="text-align:left;"> 07301 </td>
   <td style="text-align:right;"> 0.2495 </td>
   <td style="text-align:right;"> 0.0143 </td>
   <td style="text-align:right;"> 5.726 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 08 </td>
   <td style="text-align:left;"> 08307 </td>
   <td style="text-align:right;"> 0.2420 </td>
   <td style="text-align:right;"> 0.0132 </td>
   <td style="text-align:right;"> 5.475 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 03 </td>
   <td style="text-align:left;"> 03101 </td>
   <td style="text-align:right;"> 0.1944 </td>
   <td style="text-align:right;"> 0.0106 </td>
   <td style="text-align:right;"> 5.456 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13105 </td>
   <td style="text-align:right;"> 0.1927 </td>
   <td style="text-align:right;"> 0.0104 </td>
   <td style="text-align:right;"> 5.405 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 02 </td>
   <td style="text-align:left;"> 02101 </td>
   <td style="text-align:right;"> 0.1882 </td>
   <td style="text-align:right;"> 0.0101 </td>
   <td style="text-align:right;"> 5.376 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 06 </td>
   <td style="text-align:left;"> 06307 </td>
   <td style="text-align:right;"> 0.1750 </td>
   <td style="text-align:right;"> 0.0091 </td>
   <td style="text-align:right;"> 5.210 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 06 </td>
   <td style="text-align:left;"> 06110 </td>
   <td style="text-align:right;"> 0.1290 </td>
   <td style="text-align:right;"> 0.0064 </td>
   <td style="text-align:right;"> 4.984 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13127 </td>
   <td style="text-align:right;"> 0.2502 </td>
   <td style="text-align:right;"> 0.0122 </td>
   <td style="text-align:right;"> 4.878 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13404 </td>
   <td style="text-align:right;"> 0.3050 </td>
   <td style="text-align:right;"> 0.0146 </td>
   <td style="text-align:right;"> 4.794 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 11 </td>
   <td style="text-align:left;"> 11101 </td>
   <td style="text-align:right;"> 0.2145 </td>
   <td style="text-align:right;"> 0.0093 </td>
   <td style="text-align:right;"> 4.338 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 06 </td>
   <td style="text-align:left;"> 06302 </td>
   <td style="text-align:right;"> 0.2057 </td>
   <td style="text-align:right;"> 0.0088 </td>
   <td style="text-align:right;"> 4.282 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 01 </td>
   <td style="text-align:left;"> 01101 </td>
   <td style="text-align:right;"> 0.1938 </td>
   <td style="text-align:right;"> 0.0082 </td>
   <td style="text-align:right;"> 4.221 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 06 </td>
   <td style="text-align:left;"> 06204 </td>
   <td style="text-align:right;"> 0.2294 </td>
   <td style="text-align:right;"> 0.0091 </td>
   <td style="text-align:right;"> 3.977 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13126 </td>
   <td style="text-align:right;"> 0.1862 </td>
   <td style="text-align:right;"> 0.0074 </td>
   <td style="text-align:right;"> 3.953 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 12 </td>
   <td style="text-align:left;"> 12101 </td>
   <td style="text-align:right;"> 0.1582 </td>
   <td style="text-align:right;"> 0.0036 </td>
   <td style="text-align:right;"> 2.278 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 15 </td>
   <td style="text-align:left;"> 15101 </td>
   <td style="text-align:right;"> 0.2165 </td>
   <td style="text-align:right;"> 0.0003 </td>
   <td style="text-align:right;"> 0.124 </td>
  </tr>
</tbody>
</table>




