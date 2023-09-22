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

De acuerdo con los resultados obtenidos, se determinó que el modelo de área con variable respuesta **beta** presenta un mejor ajuste en comparación con los otros modelos considerados. 

A partir de este modelo, se procedió a realizar el proceso de benchmarking, el cual se describe a continuación:

## Proceso de estimación y  Benchmark

1. Leer el modelo y el archivo que contiene el orden en que ingresaron las comunas al modelo. 


```r
model_FH_beta_logitic <- readRDS("Data/model_FH_beta_logitic.rds")
estimacionesPre <- readRDS("Data/id_Orden.rds") %>%
  transmute(dam2 = str_pad(width = 5, comuna, pad = "0"),
            dam = str_sub(dam2, 1, 2),
            id_Orden)
```

2. Obtener los valores posteriores tanto para las comunas observadas como para las no observadas. Organizar estos valores de manera que las filas representen cada comuna y las columnas correspondan a los valores posteriores.


```r
y_pred <- as.array(model_FH_beta_logitic,
                   pars = c("theta", "thetapred")) %>%
  as_draws_df() %>% select(matches("theta"))

y_pred <- data.frame(t(y_pred)) %>% as_tibble()
colnames(y_pred) <- paste0("iter_", 1:ncol(y_pred))
y_pred %<>% mutate(id_Orden = 1:n())
```


3. Asignar los  valores posteriores a la comuna correspondiente. 


```r
estimacionesPre <- inner_join(estimacionesPre, y_pred)
```


4. Del censo extraer el total de personas por Región 



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

5. Obtener las estimaciones directa por región o el nivel de agregación en el cual la encuesta es representativa. 


```r
directoDam <- readRDS("Data/FIES_region.rds") %>% 
  dplyr::select( dam, ModerateSevere = FIES)
```


6. Realizar el consolidando información obtenida hasta el momento.  


```r
temp <- estimacionesPre %>%
  inner_join(N_hh ) %>% 
  inner_join(directoDam )
```

7. Con la información organizada realizar de los ponderadores para cada uno de los valores posteriores


```r
R_dam2 <- temp %>% group_by(dam) %>%
  mutate(across(
    starts_with("iter_"),
    ~
      unique(ModerateSevere) / sum((NN_Hogar  / dam_hh) * .),
    .names = "gk_{.col}"
  ),
  across(starts_with("iter_"), ~NULL)
  ) 
tba(R_dam2[1:10,c(1,2,7:12)])
```

calculando los pesos para cada comuna


```r
pesos <- temp %>% 
  mutate(W_i = NN_Hogar / dam_hh) %>% 
  select(dam2, W_i)
```


8. Multiplicar cada ponderador ($gk_i$) por la respectiva realización posterior ($iter_i$). Esto generará una estimación de benchmarking para cada uno de los valores posteriores.



```r
names_iter <- grep(pattern = "iter_",names(temp),value = TRUE)
llave <- intersect(names(estimacionesPre),names(R_dam2))
paso <- estimacionesPre %>% left_join(R_dam2 , by = llave)

for(x in names_iter){
  cat(x,"\n")
  columna_prefijo <- sym(paste0("gk_", x))
  columna_resultado <- sym(paste0("RBench_", x))
  paso %<>%  mutate(!!columna_resultado := !!sym(x) * !!columna_prefijo)
  
}

saveRDS(paso, file = "Data/replicas_RBench.rds")
```

Los resultados obtenidos se muestra en la siguiente tabla. 


```r
paso <- readRDS("Data/replicas_RBench.rds")
paso_RBench <- paso %>% select(matches("RBench_"))

tba(paso_RBench[1:10,c(1:5)])
```

<table class="table table-striped lightable-classic" style="width: auto !important; margin-left: auto; margin-right: auto; font-family: Arial Narrow; width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:right;"> RBench_iter_1 </th>
   <th style="text-align:right;"> RBench_iter_2 </th>
   <th style="text-align:right;"> RBench_iter_3 </th>
   <th style="text-align:right;"> RBench_iter_4 </th>
   <th style="text-align:right;"> RBench_iter_5 </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 0.1764 </td>
   <td style="text-align:right;"> 0.1794 </td>
   <td style="text-align:right;"> 0.1795 </td>
   <td style="text-align:right;"> 0.1809 </td>
   <td style="text-align:right;"> 0.1807 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 0.2996 </td>
   <td style="text-align:right;"> 0.2915 </td>
   <td style="text-align:right;"> 0.2909 </td>
   <td style="text-align:right;"> 0.2931 </td>
   <td style="text-align:right;"> 0.2941 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 0.2137 </td>
   <td style="text-align:right;"> 0.2333 </td>
   <td style="text-align:right;"> 0.2343 </td>
   <td style="text-align:right;"> 0.2130 </td>
   <td style="text-align:right;"> 0.2137 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 0.2093 </td>
   <td style="text-align:right;"> 0.1996 </td>
   <td style="text-align:right;"> 0.2007 </td>
   <td style="text-align:right;"> 0.1818 </td>
   <td style="text-align:right;"> 0.1758 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 0.1939 </td>
   <td style="text-align:right;"> 0.1959 </td>
   <td style="text-align:right;"> 0.1959 </td>
   <td style="text-align:right;"> 0.1942 </td>
   <td style="text-align:right;"> 0.1979 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 0.2522 </td>
   <td style="text-align:right;"> 0.2686 </td>
   <td style="text-align:right;"> 0.2745 </td>
   <td style="text-align:right;"> 0.2710 </td>
   <td style="text-align:right;"> 0.2836 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 0.2200 </td>
   <td style="text-align:right;"> 0.2613 </td>
   <td style="text-align:right;"> 0.2626 </td>
   <td style="text-align:right;"> 0.2394 </td>
   <td style="text-align:right;"> 0.2444 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 0.1937 </td>
   <td style="text-align:right;"> 0.1867 </td>
   <td style="text-align:right;"> 0.1872 </td>
   <td style="text-align:right;"> 0.1943 </td>
   <td style="text-align:right;"> 0.1878 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 0.2287 </td>
   <td style="text-align:right;"> 0.2235 </td>
   <td style="text-align:right;"> 0.2277 </td>
   <td style="text-align:right;"> 0.2229 </td>
   <td style="text-align:right;"> 0.2278 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 0.2246 </td>
   <td style="text-align:right;"> 0.2174 </td>
   <td style="text-align:right;"> 0.2088 </td>
   <td style="text-align:right;"> 0.2056 </td>
   <td style="text-align:right;"> 0.1868 </td>
  </tr>
</tbody>
</table>

9. En proceso de estimación se hace calculando el promedio y desviación estándar para cada comuna a partir de l resultado anterior. 


```r
estimacionesBench <- data.frame(
  dam = paso$dam,
  dam2 = paso$dam2,
theta_pred_RBench_rep = apply(paso_RBench,MARGIN = 1,mean),
theta_pred_RBench_rep_sd = apply(paso_RBench,MARGIN = 1,sd)
) %>% left_join(pesos)

tba(estimacionesBench %>% slice(1:10))
```

<table class="table table-striped lightable-classic" style="width: auto !important; margin-left: auto; margin-right: auto; font-family: Arial Narrow; width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> dam </th>
   <th style="text-align:left;"> dam2 </th>
   <th style="text-align:right;"> theta_pred_RBench_rep </th>
   <th style="text-align:right;"> theta_pred_RBench_rep_sd </th>
   <th style="text-align:right;"> W_i </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 01 </td>
   <td style="text-align:left;"> 01101 </td>
   <td style="text-align:right;"> 0.1862 </td>
   <td style="text-align:right;"> 0.0062 </td>
   <td style="text-align:right;"> 0.6165 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 01 </td>
   <td style="text-align:left;"> 01107 </td>
   <td style="text-align:right;"> 0.2786 </td>
   <td style="text-align:right;"> 0.0123 </td>
   <td style="text-align:right;"> 0.3040 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 01 </td>
   <td style="text-align:left;"> 01401 </td>
   <td style="text-align:right;"> 0.2247 </td>
   <td style="text-align:right;"> 0.0143 </td>
   <td style="text-align:right;"> 0.0429 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 01 </td>
   <td style="text-align:left;"> 01405 </td>
   <td style="text-align:right;"> 0.2085 </td>
   <td style="text-align:right;"> 0.0173 </td>
   <td style="text-align:right;"> 0.0168 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 02 </td>
   <td style="text-align:left;"> 02101 </td>
   <td style="text-align:right;"> 0.1934 </td>
   <td style="text-align:right;"> 0.0047 </td>
   <td style="text-align:right;"> 0.6073 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 02 </td>
   <td style="text-align:left;"> 02102 </td>
   <td style="text-align:right;"> 0.2327 </td>
   <td style="text-align:right;"> 0.0219 </td>
   <td style="text-align:right;"> 0.0197 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 02 </td>
   <td style="text-align:left;"> 02104 </td>
   <td style="text-align:right;"> 0.2300 </td>
   <td style="text-align:right;"> 0.0235 </td>
   <td style="text-align:right;"> 0.0208 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 02 </td>
   <td style="text-align:left;"> 02201 </td>
   <td style="text-align:right;"> 0.1942 </td>
   <td style="text-align:right;"> 0.0093 </td>
   <td style="text-align:right;"> 0.2781 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 02 </td>
   <td style="text-align:left;"> 02203 </td>
   <td style="text-align:right;"> 0.2536 </td>
   <td style="text-align:right;"> 0.0240 </td>
   <td style="text-align:right;"> 0.0172 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 02 </td>
   <td style="text-align:left;"> 02301 </td>
   <td style="text-align:right;"> 0.2192 </td>
   <td style="text-align:right;"> 0.0207 </td>
   <td style="text-align:right;"> 0.0459 </td>
  </tr>
</tbody>
</table>


10. Validación: Estimación FH con Benchmark


```r
estimacionesBench %>% group_by(dam) %>%
  summarise(theta_reg_RB = sum(W_i * theta_pred_RBench_rep)) %>%
  left_join(directoDam, by = "dam") %>% 
  data.frame() %>% tba()
```

## Validación de los resultados. 

La visualización resultante del siguiente código muestra puntos de diferentes formas y colores para representar los diferentes métodos de estimación, y dos líneas punteadas que representan los intervalos de confianza superior e inferior para los valores observados en la variable `theta_dir`.

El siguiente código define los intervalos de confianza para las estimaciones directas. 


```r
IC_dir <- readRDS("Data/FIES_region.rds") %>%
  dplyr::select(dam, FIES, var_hat) %>%
  transmute(dam,
            Ls = FIES + 1.96 * sqrt(var_hat),
            Li = FIES - 1.96 * sqrt(var_hat))
```

Ahora, examinar las estimaciones del modelo de área sin incluir el benchmark. 


```r
estimacionesPre <- readRDS("Data/estimacionesPre.rds") %>%
  transmute(dam2 = comuna,
            dam = str_sub(dam2,1,2),
            theta_pred = pred_beta_log,
            theta_pred_EE = pred_beta_log_EE)
```

A continuación se consolida las base con las estimaciones por comuna. 


```r
temp <- estimacionesBench %>% left_join( estimacionesPre ) %>% 
  group_by(dam) %>% 
  summarise(
            "FIES Modelo" = sum(W_i * theta_pred),
            "FIES Modelo Bench" = sum(W_i * theta_pred_RBench_rep)
  ) %>%   
  left_join(directoDam, by = "dam")  %>% 
  mutate(id = 1:n())
```



```r
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

ggsave(
  plot = p_temp,
  filename = "Data/RecursosBook/03/1_validacion_Bench.jpeg",
  width = 12, 
  height = 7
)
```


<img src="Data/RecursosBook/03/1_validacion_Bench.jpeg" width="1800" />

## Mapa con la estimación de la Escala de Experiencia de Inseguridad Alimentaria 


<img src="Data/RecursosBook/04/CHL_FIES2.jpeg" width="200%" />

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
   <td style="text-align:left;"> 15 </td>
   <td style="text-align:left;"> 15202 </td>
   <td style="text-align:right;"> 0.2239 </td>
   <td style="text-align:right;"> 0.0698 </td>
   <td style="text-align:right;"> 31.155 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 15 </td>
   <td style="text-align:left;"> 15102 </td>
   <td style="text-align:right;"> 0.1315 </td>
   <td style="text-align:right;"> 0.0345 </td>
   <td style="text-align:right;"> 26.220 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 12 </td>
   <td style="text-align:left;"> 12303 </td>
   <td style="text-align:right;"> 0.1183 </td>
   <td style="text-align:right;"> 0.0286 </td>
   <td style="text-align:right;"> 24.145 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 12 </td>
   <td style="text-align:left;"> 12402 </td>
   <td style="text-align:right;"> 0.1002 </td>
   <td style="text-align:right;"> 0.0238 </td>
   <td style="text-align:right;"> 23.782 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 12 </td>
   <td style="text-align:left;"> 12103 </td>
   <td style="text-align:right;"> 0.1127 </td>
   <td style="text-align:right;"> 0.0266 </td>
   <td style="text-align:right;"> 23.643 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 12 </td>
   <td style="text-align:left;"> 12102 </td>
   <td style="text-align:right;"> 0.1825 </td>
   <td style="text-align:right;"> 0.0425 </td>
   <td style="text-align:right;"> 23.287 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 02 </td>
   <td style="text-align:left;"> 02202 </td>
   <td style="text-align:right;"> 0.2250 </td>
   <td style="text-align:right;"> 0.0514 </td>
   <td style="text-align:right;"> 22.835 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 01 </td>
   <td style="text-align:left;"> 01403 </td>
   <td style="text-align:right;"> 0.2505 </td>
   <td style="text-align:right;"> 0.0515 </td>
   <td style="text-align:right;"> 20.562 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05 </td>
   <td style="text-align:left;"> 05504 </td>
   <td style="text-align:right;"> 0.1010 </td>
   <td style="text-align:right;"> 0.0192 </td>
   <td style="text-align:right;"> 18.973 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 12 </td>
   <td style="text-align:left;"> 12104 </td>
   <td style="text-align:right;"> 0.1089 </td>
   <td style="text-align:right;"> 0.0200 </td>
   <td style="text-align:right;"> 18.358 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05 </td>
   <td style="text-align:left;"> 05104 </td>
   <td style="text-align:right;"> 0.1555 </td>
   <td style="text-align:right;"> 0.0278 </td>
   <td style="text-align:right;"> 17.897 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 12 </td>
   <td style="text-align:left;"> 12302 </td>
   <td style="text-align:right;"> 0.1690 </td>
   <td style="text-align:right;"> 0.0279 </td>
   <td style="text-align:right;"> 16.486 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 15 </td>
   <td style="text-align:left;"> 15201 </td>
   <td style="text-align:right;"> 0.2226 </td>
   <td style="text-align:right;"> 0.0343 </td>
   <td style="text-align:right;"> 15.404 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05 </td>
   <td style="text-align:left;"> 05506 </td>
   <td style="text-align:right;"> 0.1550 </td>
   <td style="text-align:right;"> 0.0234 </td>
   <td style="text-align:right;"> 15.089 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 08 </td>
   <td style="text-align:left;"> 08314 </td>
   <td style="text-align:right;"> 0.3296 </td>
   <td style="text-align:right;"> 0.0489 </td>
   <td style="text-align:right;"> 14.839 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 12 </td>
   <td style="text-align:left;"> 12201 </td>
   <td style="text-align:right;"> 0.1892 </td>
   <td style="text-align:right;"> 0.0260 </td>
   <td style="text-align:right;"> 13.749 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 06 </td>
   <td style="text-align:left;"> 06309 </td>
   <td style="text-align:right;"> 0.1990 </td>
   <td style="text-align:right;"> 0.0273 </td>
   <td style="text-align:right;"> 13.716 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05 </td>
   <td style="text-align:left;"> 05303 </td>
   <td style="text-align:right;"> 0.2021 </td>
   <td style="text-align:right;"> 0.0276 </td>
   <td style="text-align:right;"> 13.661 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 10103 </td>
   <td style="text-align:right;"> 0.2021 </td>
   <td style="text-align:right;"> 0.0276 </td>
   <td style="text-align:right;"> 13.649 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 12 </td>
   <td style="text-align:left;"> 12301 </td>
   <td style="text-align:right;"> 0.1742 </td>
   <td style="text-align:right;"> 0.0230 </td>
   <td style="text-align:right;"> 13.229 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 10401 </td>
   <td style="text-align:right;"> 0.1698 </td>
   <td style="text-align:right;"> 0.0213 </td>
   <td style="text-align:right;"> 12.531 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 06 </td>
   <td style="text-align:left;"> 06202 </td>
   <td style="text-align:right;"> 0.1499 </td>
   <td style="text-align:right;"> 0.0187 </td>
   <td style="text-align:right;"> 12.485 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 11 </td>
   <td style="text-align:left;"> 11302 </td>
   <td style="text-align:right;"> 0.2053 </td>
   <td style="text-align:right;"> 0.0256 </td>
   <td style="text-align:right;"> 12.460 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 01 </td>
   <td style="text-align:left;"> 01402 </td>
   <td style="text-align:right;"> 0.2506 </td>
   <td style="text-align:right;"> 0.0309 </td>
   <td style="text-align:right;"> 12.332 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 11 </td>
   <td style="text-align:left;"> 11102 </td>
   <td style="text-align:right;"> 0.1468 </td>
   <td style="text-align:right;"> 0.0176 </td>
   <td style="text-align:right;"> 11.989 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 02 </td>
   <td style="text-align:left;"> 02302 </td>
   <td style="text-align:right;"> 0.1493 </td>
   <td style="text-align:right;"> 0.0177 </td>
   <td style="text-align:right;"> 11.869 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05 </td>
   <td style="text-align:left;"> 05605 </td>
   <td style="text-align:right;"> 0.1557 </td>
   <td style="text-align:right;"> 0.0180 </td>
   <td style="text-align:right;"> 11.593 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 10204 </td>
   <td style="text-align:right;"> 0.1878 </td>
   <td style="text-align:right;"> 0.0214 </td>
   <td style="text-align:right;"> 11.413 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13115 </td>
   <td style="text-align:right;"> 0.0810 </td>
   <td style="text-align:right;"> 0.0089 </td>
   <td style="text-align:right;"> 10.953 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 09 </td>
   <td style="text-align:left;"> 09116 </td>
   <td style="text-align:right;"> 0.2443 </td>
   <td style="text-align:right;"> 0.0267 </td>
   <td style="text-align:right;"> 10.942 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13132 </td>
   <td style="text-align:right;"> 0.0468 </td>
   <td style="text-align:right;"> 0.0051 </td>
   <td style="text-align:right;"> 10.814 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05 </td>
   <td style="text-align:left;"> 05402 </td>
   <td style="text-align:right;"> 0.1706 </td>
   <td style="text-align:right;"> 0.0183 </td>
   <td style="text-align:right;"> 10.721 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 10404 </td>
   <td style="text-align:right;"> 0.1842 </td>
   <td style="text-align:right;"> 0.0197 </td>
   <td style="text-align:right;"> 10.716 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 10402 </td>
   <td style="text-align:right;"> 0.2318 </td>
   <td style="text-align:right;"> 0.0246 </td>
   <td style="text-align:right;"> 10.614 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 02 </td>
   <td style="text-align:left;"> 02103 </td>
   <td style="text-align:right;"> 0.2311 </td>
   <td style="text-align:right;"> 0.0242 </td>
   <td style="text-align:right;"> 10.491 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 11 </td>
   <td style="text-align:left;"> 11402 </td>
   <td style="text-align:right;"> 0.1977 </td>
   <td style="text-align:right;"> 0.0204 </td>
   <td style="text-align:right;"> 10.318 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 03 </td>
   <td style="text-align:left;"> 03302 </td>
   <td style="text-align:right;"> 0.2441 </td>
   <td style="text-align:right;"> 0.0251 </td>
   <td style="text-align:right;"> 10.296 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 16 </td>
   <td style="text-align:left;"> 16204 </td>
   <td style="text-align:right;"> 0.2191 </td>
   <td style="text-align:right;"> 0.0225 </td>
   <td style="text-align:right;"> 10.283 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 02 </td>
   <td style="text-align:left;"> 02104 </td>
   <td style="text-align:right;"> 0.2300 </td>
   <td style="text-align:right;"> 0.0235 </td>
   <td style="text-align:right;"> 10.224 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 09 </td>
   <td style="text-align:left;"> 09104 </td>
   <td style="text-align:right;"> 0.2876 </td>
   <td style="text-align:right;"> 0.0292 </td>
   <td style="text-align:right;"> 10.157 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 09 </td>
   <td style="text-align:left;"> 09121 </td>
   <td style="text-align:right;"> 0.2427 </td>
   <td style="text-align:right;"> 0.0244 </td>
   <td style="text-align:right;"> 10.067 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 11 </td>
   <td style="text-align:left;"> 11203 </td>
   <td style="text-align:right;"> 0.2219 </td>
   <td style="text-align:right;"> 0.0223 </td>
   <td style="text-align:right;"> 10.052 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 01 </td>
   <td style="text-align:left;"> 01404 </td>
   <td style="text-align:right;"> 0.2425 </td>
   <td style="text-align:right;"> 0.0244 </td>
   <td style="text-align:right;"> 10.048 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 04 </td>
   <td style="text-align:left;"> 04305 </td>
   <td style="text-align:right;"> 0.1757 </td>
   <td style="text-align:right;"> 0.0176 </td>
   <td style="text-align:right;"> 10.007 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05 </td>
   <td style="text-align:left;"> 05404 </td>
   <td style="text-align:right;"> 0.1847 </td>
   <td style="text-align:right;"> 0.0184 </td>
   <td style="text-align:right;"> 9.939 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 09 </td>
   <td style="text-align:left;"> 09205 </td>
   <td style="text-align:right;"> 0.2739 </td>
   <td style="text-align:right;"> 0.0271 </td>
   <td style="text-align:right;"> 9.895 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05 </td>
   <td style="text-align:left;"> 05201 </td>
   <td style="text-align:right;"> 0.2096 </td>
   <td style="text-align:right;"> 0.0207 </td>
   <td style="text-align:right;"> 9.883 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 04 </td>
   <td style="text-align:left;"> 04202 </td>
   <td style="text-align:right;"> 0.1791 </td>
   <td style="text-align:right;"> 0.0177 </td>
   <td style="text-align:right;"> 9.857 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 11 </td>
   <td style="text-align:left;"> 11303 </td>
   <td style="text-align:right;"> 0.2022 </td>
   <td style="text-align:right;"> 0.0199 </td>
   <td style="text-align:right;"> 9.850 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 04 </td>
   <td style="text-align:left;"> 04304 </td>
   <td style="text-align:right;"> 0.1949 </td>
   <td style="text-align:right;"> 0.0188 </td>
   <td style="text-align:right;"> 9.654 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13502 </td>
   <td style="text-align:right;"> 0.1826 </td>
   <td style="text-align:right;"> 0.0176 </td>
   <td style="text-align:right;"> 9.631 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13303 </td>
   <td style="text-align:right;"> 0.1631 </td>
   <td style="text-align:right;"> 0.0157 </td>
   <td style="text-align:right;"> 9.614 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13505 </td>
   <td style="text-align:right;"> 0.2128 </td>
   <td style="text-align:right;"> 0.0204 </td>
   <td style="text-align:right;"> 9.589 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 04 </td>
   <td style="text-align:left;"> 04104 </td>
   <td style="text-align:right;"> 0.1951 </td>
   <td style="text-align:right;"> 0.0186 </td>
   <td style="text-align:right;"> 9.529 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 02 </td>
   <td style="text-align:left;"> 02203 </td>
   <td style="text-align:right;"> 0.2536 </td>
   <td style="text-align:right;"> 0.0240 </td>
   <td style="text-align:right;"> 9.465 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 02 </td>
   <td style="text-align:left;"> 02301 </td>
   <td style="text-align:right;"> 0.2192 </td>
   <td style="text-align:right;"> 0.0207 </td>
   <td style="text-align:right;"> 9.425 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 02 </td>
   <td style="text-align:left;"> 02102 </td>
   <td style="text-align:right;"> 0.2327 </td>
   <td style="text-align:right;"> 0.0219 </td>
   <td style="text-align:right;"> 9.398 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 06 </td>
   <td style="text-align:left;"> 06103 </td>
   <td style="text-align:right;"> 0.1718 </td>
   <td style="text-align:right;"> 0.0161 </td>
   <td style="text-align:right;"> 9.382 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05 </td>
   <td style="text-align:left;"> 05606 </td>
   <td style="text-align:right;"> 0.1405 </td>
   <td style="text-align:right;"> 0.0131 </td>
   <td style="text-align:right;"> 9.347 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 07 </td>
   <td style="text-align:left;"> 07103 </td>
   <td style="text-align:right;"> 0.1783 </td>
   <td style="text-align:right;"> 0.0166 </td>
   <td style="text-align:right;"> 9.298 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 09 </td>
   <td style="text-align:left;"> 09204 </td>
   <td style="text-align:right;"> 0.3516 </td>
   <td style="text-align:right;"> 0.0323 </td>
   <td style="text-align:right;"> 9.178 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 10206 </td>
   <td style="text-align:right;"> 0.1810 </td>
   <td style="text-align:right;"> 0.0166 </td>
   <td style="text-align:right;"> 9.150 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05 </td>
   <td style="text-align:left;"> 05703 </td>
   <td style="text-align:right;"> 0.1955 </td>
   <td style="text-align:right;"> 0.0177 </td>
   <td style="text-align:right;"> 9.061 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13123 </td>
   <td style="text-align:right;"> 0.0638 </td>
   <td style="text-align:right;"> 0.0058 </td>
   <td style="text-align:right;"> 9.036 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 16 </td>
   <td style="text-align:left;"> 16202 </td>
   <td style="text-align:right;"> 0.1863 </td>
   <td style="text-align:right;"> 0.0167 </td>
   <td style="text-align:right;"> 8.976 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 08 </td>
   <td style="text-align:left;"> 08207 </td>
   <td style="text-align:right;"> 0.2858 </td>
   <td style="text-align:right;"> 0.0256 </td>
   <td style="text-align:right;"> 8.965 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 06 </td>
   <td style="text-align:left;"> 06205 </td>
   <td style="text-align:right;"> 0.1559 </td>
   <td style="text-align:right;"> 0.0140 </td>
   <td style="text-align:right;"> 8.952 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 07 </td>
   <td style="text-align:left;"> 07104 </td>
   <td style="text-align:right;"> 0.2565 </td>
   <td style="text-align:right;"> 0.0228 </td>
   <td style="text-align:right;"> 8.893 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 08 </td>
   <td style="text-align:left;"> 08310 </td>
   <td style="text-align:right;"> 0.2070 </td>
   <td style="text-align:right;"> 0.0183 </td>
   <td style="text-align:right;"> 8.832 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 08 </td>
   <td style="text-align:left;"> 08104 </td>
   <td style="text-align:right;"> 0.2195 </td>
   <td style="text-align:right;"> 0.0193 </td>
   <td style="text-align:right;"> 8.802 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05 </td>
   <td style="text-align:left;"> 05405 </td>
   <td style="text-align:right;"> 0.1555 </td>
   <td style="text-align:right;"> 0.0137 </td>
   <td style="text-align:right;"> 8.794 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05 </td>
   <td style="text-align:left;"> 05105 </td>
   <td style="text-align:right;"> 0.2023 </td>
   <td style="text-align:right;"> 0.0177 </td>
   <td style="text-align:right;"> 8.756 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05 </td>
   <td style="text-align:left;"> 05403 </td>
   <td style="text-align:right;"> 0.1659 </td>
   <td style="text-align:right;"> 0.0144 </td>
   <td style="text-align:right;"> 8.688 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13114 </td>
   <td style="text-align:right;"> 0.0592 </td>
   <td style="text-align:right;"> 0.0051 </td>
   <td style="text-align:right;"> 8.680 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13203 </td>
   <td style="text-align:right;"> 0.1587 </td>
   <td style="text-align:right;"> 0.0138 </td>
   <td style="text-align:right;"> 8.668 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 09 </td>
   <td style="text-align:left;"> 09117 </td>
   <td style="text-align:right;"> 0.2988 </td>
   <td style="text-align:right;"> 0.0253 </td>
   <td style="text-align:right;"> 8.460 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 12 </td>
   <td style="text-align:left;"> 12401 </td>
   <td style="text-align:right;"> 0.1780 </td>
   <td style="text-align:right;"> 0.0150 </td>
   <td style="text-align:right;"> 8.446 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 03 </td>
   <td style="text-align:left;"> 03303 </td>
   <td style="text-align:right;"> 0.1751 </td>
   <td style="text-align:right;"> 0.0147 </td>
   <td style="text-align:right;"> 8.414 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 10209 </td>
   <td style="text-align:right;"> 0.1975 </td>
   <td style="text-align:right;"> 0.0164 </td>
   <td style="text-align:right;"> 8.307 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 10306 </td>
   <td style="text-align:right;"> 0.3012 </td>
   <td style="text-align:right;"> 0.0250 </td>
   <td style="text-align:right;"> 8.298 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 01 </td>
   <td style="text-align:left;"> 01405 </td>
   <td style="text-align:right;"> 0.2085 </td>
   <td style="text-align:right;"> 0.0173 </td>
   <td style="text-align:right;"> 8.297 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 06 </td>
   <td style="text-align:left;"> 06108 </td>
   <td style="text-align:right;"> 0.1290 </td>
   <td style="text-align:right;"> 0.0107 </td>
   <td style="text-align:right;"> 8.295 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 10210 </td>
   <td style="text-align:right;"> 0.2421 </td>
   <td style="text-align:right;"> 0.0200 </td>
   <td style="text-align:right;"> 8.272 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 08 </td>
   <td style="text-align:left;"> 08204 </td>
   <td style="text-align:right;"> 0.2164 </td>
   <td style="text-align:right;"> 0.0179 </td>
   <td style="text-align:right;"> 8.251 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 07 </td>
   <td style="text-align:left;"> 07107 </td>
   <td style="text-align:right;"> 0.1827 </td>
   <td style="text-align:right;"> 0.0150 </td>
   <td style="text-align:right;"> 8.220 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 03 </td>
   <td style="text-align:left;"> 03103 </td>
   <td style="text-align:right;"> 0.2321 </td>
   <td style="text-align:right;"> 0.0191 </td>
   <td style="text-align:right;"> 8.215 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 11 </td>
   <td style="text-align:left;"> 11401 </td>
   <td style="text-align:right;"> 0.1955 </td>
   <td style="text-align:right;"> 0.0158 </td>
   <td style="text-align:right;"> 8.108 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13503 </td>
   <td style="text-align:right;"> 0.2028 </td>
   <td style="text-align:right;"> 0.0164 </td>
   <td style="text-align:right;"> 8.065 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 10207 </td>
   <td style="text-align:right;"> 0.2159 </td>
   <td style="text-align:right;"> 0.0172 </td>
   <td style="text-align:right;"> 7.987 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 08 </td>
   <td style="text-align:left;"> 08308 </td>
   <td style="text-align:right;"> 0.2389 </td>
   <td style="text-align:right;"> 0.0188 </td>
   <td style="text-align:right;"> 7.864 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 16 </td>
   <td style="text-align:left;"> 16304 </td>
   <td style="text-align:right;"> 0.2476 </td>
   <td style="text-align:right;"> 0.0194 </td>
   <td style="text-align:right;"> 7.846 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05 </td>
   <td style="text-align:left;"> 05704 </td>
   <td style="text-align:right;"> 0.2080 </td>
   <td style="text-align:right;"> 0.0162 </td>
   <td style="text-align:right;"> 7.791 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05 </td>
   <td style="text-align:left;"> 05602 </td>
   <td style="text-align:right;"> 0.1577 </td>
   <td style="text-align:right;"> 0.0123 </td>
   <td style="text-align:right;"> 7.786 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 09 </td>
   <td style="text-align:left;"> 09106 </td>
   <td style="text-align:right;"> 0.2708 </td>
   <td style="text-align:right;"> 0.0207 </td>
   <td style="text-align:right;"> 7.659 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 07 </td>
   <td style="text-align:left;"> 07203 </td>
   <td style="text-align:right;"> 0.1888 </td>
   <td style="text-align:right;"> 0.0144 </td>
   <td style="text-align:right;"> 7.652 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 09 </td>
   <td style="text-align:left;"> 09110 </td>
   <td style="text-align:right;"> 0.2278 </td>
   <td style="text-align:right;"> 0.0174 </td>
   <td style="text-align:right;"> 7.631 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 16 </td>
   <td style="text-align:left;"> 16206 </td>
   <td style="text-align:right;"> 0.1781 </td>
   <td style="text-align:right;"> 0.0135 </td>
   <td style="text-align:right;"> 7.589 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 07 </td>
   <td style="text-align:left;"> 07306 </td>
   <td style="text-align:right;"> 0.2098 </td>
   <td style="text-align:right;"> 0.0158 </td>
   <td style="text-align:right;"> 7.549 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13504 </td>
   <td style="text-align:right;"> 0.1839 </td>
   <td style="text-align:right;"> 0.0139 </td>
   <td style="text-align:right;"> 7.544 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 16 </td>
   <td style="text-align:left;"> 16207 </td>
   <td style="text-align:right;"> 0.2049 </td>
   <td style="text-align:right;"> 0.0154 </td>
   <td style="text-align:right;"> 7.523 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13301 </td>
   <td style="text-align:right;"> 0.1760 </td>
   <td style="text-align:right;"> 0.0132 </td>
   <td style="text-align:right;"> 7.497 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05 </td>
   <td style="text-align:left;"> 05702 </td>
   <td style="text-align:right;"> 0.2199 </td>
   <td style="text-align:right;"> 0.0164 </td>
   <td style="text-align:right;"> 7.448 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 16 </td>
   <td style="text-align:left;"> 16205 </td>
   <td style="text-align:right;"> 0.2188 </td>
   <td style="text-align:right;"> 0.0163 </td>
   <td style="text-align:right;"> 7.446 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 08 </td>
   <td style="text-align:left;"> 08302 </td>
   <td style="text-align:right;"> 0.1995 </td>
   <td style="text-align:right;"> 0.0148 </td>
   <td style="text-align:right;"> 7.408 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 14 </td>
   <td style="text-align:left;"> 14102 </td>
   <td style="text-align:right;"> 0.2373 </td>
   <td style="text-align:right;"> 0.0175 </td>
   <td style="text-align:right;"> 7.385 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 03 </td>
   <td style="text-align:left;"> 03202 </td>
   <td style="text-align:right;"> 0.1673 </td>
   <td style="text-align:right;"> 0.0123 </td>
   <td style="text-align:right;"> 7.377 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 06 </td>
   <td style="text-align:left;"> 06206 </td>
   <td style="text-align:right;"> 0.2214 </td>
   <td style="text-align:right;"> 0.0163 </td>
   <td style="text-align:right;"> 7.377 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 06 </td>
   <td style="text-align:left;"> 06304 </td>
   <td style="text-align:right;"> 0.1898 </td>
   <td style="text-align:right;"> 0.0140 </td>
   <td style="text-align:right;"> 7.351 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 11 </td>
   <td style="text-align:left;"> 11202 </td>
   <td style="text-align:right;"> 0.2004 </td>
   <td style="text-align:right;"> 0.0147 </td>
   <td style="text-align:right;"> 7.346 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 06 </td>
   <td style="text-align:left;"> 06203 </td>
   <td style="text-align:right;"> 0.1890 </td>
   <td style="text-align:right;"> 0.0138 </td>
   <td style="text-align:right;"> 7.282 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 10403 </td>
   <td style="text-align:right;"> 0.2609 </td>
   <td style="text-align:right;"> 0.0190 </td>
   <td style="text-align:right;"> 7.274 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 11 </td>
   <td style="text-align:left;"> 11301 </td>
   <td style="text-align:right;"> 0.2018 </td>
   <td style="text-align:right;"> 0.0147 </td>
   <td style="text-align:right;"> 7.271 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 14 </td>
   <td style="text-align:left;"> 14203 </td>
   <td style="text-align:right;"> 0.2528 </td>
   <td style="text-align:right;"> 0.0183 </td>
   <td style="text-align:right;"> 7.231 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13120 </td>
   <td style="text-align:right;"> 0.0828 </td>
   <td style="text-align:right;"> 0.0060 </td>
   <td style="text-align:right;"> 7.225 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 06 </td>
   <td style="text-align:left;"> 06306 </td>
   <td style="text-align:right;"> 0.1861 </td>
   <td style="text-align:right;"> 0.0134 </td>
   <td style="text-align:right;"> 7.174 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 07 </td>
   <td style="text-align:left;"> 07105 </td>
   <td style="text-align:right;"> 0.2113 </td>
   <td style="text-align:right;"> 0.0150 </td>
   <td style="text-align:right;"> 7.086 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05 </td>
   <td style="text-align:left;"> 05107 </td>
   <td style="text-align:right;"> 0.2122 </td>
   <td style="text-align:right;"> 0.0150 </td>
   <td style="text-align:right;"> 7.064 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 10304 </td>
   <td style="text-align:right;"> 0.2529 </td>
   <td style="text-align:right;"> 0.0178 </td>
   <td style="text-align:right;"> 7.041 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05 </td>
   <td style="text-align:left;"> 05705 </td>
   <td style="text-align:right;"> 0.2082 </td>
   <td style="text-align:right;"> 0.0146 </td>
   <td style="text-align:right;"> 7.012 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 07 </td>
   <td style="text-align:left;"> 07309 </td>
   <td style="text-align:right;"> 0.1698 </td>
   <td style="text-align:right;"> 0.0119 </td>
   <td style="text-align:right;"> 7.011 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 06 </td>
   <td style="text-align:left;"> 06308 </td>
   <td style="text-align:right;"> 0.2081 </td>
   <td style="text-align:right;"> 0.0145 </td>
   <td style="text-align:right;"> 6.992 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 04 </td>
   <td style="text-align:left;"> 04302 </td>
   <td style="text-align:right;"> 0.1987 </td>
   <td style="text-align:right;"> 0.0138 </td>
   <td style="text-align:right;"> 6.956 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 07 </td>
   <td style="text-align:left;"> 07202 </td>
   <td style="text-align:right;"> 0.2298 </td>
   <td style="text-align:right;"> 0.0159 </td>
   <td style="text-align:right;"> 6.914 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 07 </td>
   <td style="text-align:left;"> 07302 </td>
   <td style="text-align:right;"> 0.1757 </td>
   <td style="text-align:right;"> 0.0121 </td>
   <td style="text-align:right;"> 6.909 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 07 </td>
   <td style="text-align:left;"> 07303 </td>
   <td style="text-align:right;"> 0.1703 </td>
   <td style="text-align:right;"> 0.0118 </td>
   <td style="text-align:right;"> 6.909 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13501 </td>
   <td style="text-align:right;"> 0.2194 </td>
   <td style="text-align:right;"> 0.0151 </td>
   <td style="text-align:right;"> 6.902 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13113 </td>
   <td style="text-align:right;"> 0.0838 </td>
   <td style="text-align:right;"> 0.0058 </td>
   <td style="text-align:right;"> 6.889 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 06 </td>
   <td style="text-align:left;"> 06204 </td>
   <td style="text-align:right;"> 0.1816 </td>
   <td style="text-align:right;"> 0.0125 </td>
   <td style="text-align:right;"> 6.872 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 04 </td>
   <td style="text-align:left;"> 04105 </td>
   <td style="text-align:right;"> 0.2007 </td>
   <td style="text-align:right;"> 0.0138 </td>
   <td style="text-align:right;"> 6.862 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05 </td>
   <td style="text-align:left;"> 05302 </td>
   <td style="text-align:right;"> 0.1917 </td>
   <td style="text-align:right;"> 0.0131 </td>
   <td style="text-align:right;"> 6.842 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05 </td>
   <td style="text-align:left;"> 05304 </td>
   <td style="text-align:right;"> 0.1723 </td>
   <td style="text-align:right;"> 0.0118 </td>
   <td style="text-align:right;"> 6.835 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 07 </td>
   <td style="text-align:left;"> 07108 </td>
   <td style="text-align:right;"> 0.2114 </td>
   <td style="text-align:right;"> 0.0144 </td>
   <td style="text-align:right;"> 6.833 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 08 </td>
   <td style="text-align:left;"> 08109 </td>
   <td style="text-align:right;"> 0.1940 </td>
   <td style="text-align:right;"> 0.0132 </td>
   <td style="text-align:right;"> 6.813 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 03 </td>
   <td style="text-align:left;"> 03201 </td>
   <td style="text-align:right;"> 0.1944 </td>
   <td style="text-align:right;"> 0.0132 </td>
   <td style="text-align:right;"> 6.796 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 09 </td>
   <td style="text-align:left;"> 09118 </td>
   <td style="text-align:right;"> 0.2586 </td>
   <td style="text-align:right;"> 0.0176 </td>
   <td style="text-align:right;"> 6.795 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 09 </td>
   <td style="text-align:left;"> 09208 </td>
   <td style="text-align:right;"> 0.2607 </td>
   <td style="text-align:right;"> 0.0176 </td>
   <td style="text-align:right;"> 6.752 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13202 </td>
   <td style="text-align:right;"> 0.1601 </td>
   <td style="text-align:right;"> 0.0108 </td>
   <td style="text-align:right;"> 6.739 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 06 </td>
   <td style="text-align:left;"> 06113 </td>
   <td style="text-align:right;"> 0.2008 </td>
   <td style="text-align:right;"> 0.0135 </td>
   <td style="text-align:right;"> 6.712 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13302 </td>
   <td style="text-align:right;"> 0.2099 </td>
   <td style="text-align:right;"> 0.0141 </td>
   <td style="text-align:right;"> 6.701 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 10106 </td>
   <td style="text-align:right;"> 0.2720 </td>
   <td style="text-align:right;"> 0.0182 </td>
   <td style="text-align:right;"> 6.676 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 06 </td>
   <td style="text-align:left;"> 06307 </td>
   <td style="text-align:right;"> 0.2019 </td>
   <td style="text-align:right;"> 0.0134 </td>
   <td style="text-align:right;"> 6.640 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 08 </td>
   <td style="text-align:left;"> 08205 </td>
   <td style="text-align:right;"> 0.2114 </td>
   <td style="text-align:right;"> 0.0139 </td>
   <td style="text-align:right;"> 6.589 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 16 </td>
   <td style="text-align:left;"> 16203 </td>
   <td style="text-align:right;"> 0.1893 </td>
   <td style="text-align:right;"> 0.0125 </td>
   <td style="text-align:right;"> 6.580 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 10105 </td>
   <td style="text-align:right;"> 0.2290 </td>
   <td style="text-align:right;"> 0.0150 </td>
   <td style="text-align:right;"> 6.564 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 16 </td>
   <td style="text-align:left;"> 16104 </td>
   <td style="text-align:right;"> 0.2674 </td>
   <td style="text-align:right;"> 0.0175 </td>
   <td style="text-align:right;"> 6.552 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13107 </td>
   <td style="text-align:right;"> 0.1632 </td>
   <td style="text-align:right;"> 0.0107 </td>
   <td style="text-align:right;"> 6.542 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 06 </td>
   <td style="text-align:left;"> 06104 </td>
   <td style="text-align:right;"> 0.2144 </td>
   <td style="text-align:right;"> 0.0140 </td>
   <td style="text-align:right;"> 6.527 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05 </td>
   <td style="text-align:left;"> 05103 </td>
   <td style="text-align:right;"> 0.1310 </td>
   <td style="text-align:right;"> 0.0085 </td>
   <td style="text-align:right;"> 6.522 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 10302 </td>
   <td style="text-align:right;"> 0.2424 </td>
   <td style="text-align:right;"> 0.0156 </td>
   <td style="text-align:right;"> 6.441 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 09 </td>
   <td style="text-align:left;"> 09107 </td>
   <td style="text-align:right;"> 0.2262 </td>
   <td style="text-align:right;"> 0.0145 </td>
   <td style="text-align:right;"> 6.419 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 09 </td>
   <td style="text-align:left;"> 09207 </td>
   <td style="text-align:right;"> 0.2522 </td>
   <td style="text-align:right;"> 0.0161 </td>
   <td style="text-align:right;"> 6.394 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 10307 </td>
   <td style="text-align:right;"> 0.2176 </td>
   <td style="text-align:right;"> 0.0139 </td>
   <td style="text-align:right;"> 6.389 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 01 </td>
   <td style="text-align:left;"> 01401 </td>
   <td style="text-align:right;"> 0.2247 </td>
   <td style="text-align:right;"> 0.0143 </td>
   <td style="text-align:right;"> 6.383 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 07 </td>
   <td style="text-align:left;"> 07307 </td>
   <td style="text-align:right;"> 0.1952 </td>
   <td style="text-align:right;"> 0.0125 </td>
   <td style="text-align:right;"> 6.383 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 08 </td>
   <td style="text-align:left;"> 08201 </td>
   <td style="text-align:right;"> 0.2524 </td>
   <td style="text-align:right;"> 0.0161 </td>
   <td style="text-align:right;"> 6.366 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 06 </td>
   <td style="text-align:left;"> 06102 </td>
   <td style="text-align:right;"> 0.1968 </td>
   <td style="text-align:right;"> 0.0124 </td>
   <td style="text-align:right;"> 6.317 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13404 </td>
   <td style="text-align:right;"> 0.2304 </td>
   <td style="text-align:right;"> 0.0145 </td>
   <td style="text-align:right;"> 6.312 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 10104 </td>
   <td style="text-align:right;"> 0.2422 </td>
   <td style="text-align:right;"> 0.0153 </td>
   <td style="text-align:right;"> 6.304 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 04 </td>
   <td style="text-align:left;"> 04203 </td>
   <td style="text-align:right;"> 0.2066 </td>
   <td style="text-align:right;"> 0.0130 </td>
   <td style="text-align:right;"> 6.300 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 06 </td>
   <td style="text-align:left;"> 06302 </td>
   <td style="text-align:right;"> 0.2211 </td>
   <td style="text-align:right;"> 0.0139 </td>
   <td style="text-align:right;"> 6.284 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 07 </td>
   <td style="text-align:left;"> 07106 </td>
   <td style="text-align:right;"> 0.1878 </td>
   <td style="text-align:right;"> 0.0118 </td>
   <td style="text-align:right;"> 6.268 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 09 </td>
   <td style="text-align:left;"> 09206 </td>
   <td style="text-align:right;"> 0.2373 </td>
   <td style="text-align:right;"> 0.0149 </td>
   <td style="text-align:right;"> 6.263 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 10208 </td>
   <td style="text-align:right;"> 0.2598 </td>
   <td style="text-align:right;"> 0.0162 </td>
   <td style="text-align:right;"> 6.238 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 10107 </td>
   <td style="text-align:right;"> 0.2122 </td>
   <td style="text-align:right;"> 0.0132 </td>
   <td style="text-align:right;"> 6.237 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 04 </td>
   <td style="text-align:left;"> 04204 </td>
   <td style="text-align:right;"> 0.1742 </td>
   <td style="text-align:right;"> 0.0108 </td>
   <td style="text-align:right;"> 6.215 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 06 </td>
   <td style="text-align:left;"> 06112 </td>
   <td style="text-align:right;"> 0.1763 </td>
   <td style="text-align:right;"> 0.0109 </td>
   <td style="text-align:right;"> 6.207 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 10108 </td>
   <td style="text-align:right;"> 0.2554 </td>
   <td style="text-align:right;"> 0.0158 </td>
   <td style="text-align:right;"> 6.201 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 10109 </td>
   <td style="text-align:right;"> 0.1747 </td>
   <td style="text-align:right;"> 0.0107 </td>
   <td style="text-align:right;"> 6.136 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 04 </td>
   <td style="text-align:left;"> 04106 </td>
   <td style="text-align:right;"> 0.2187 </td>
   <td style="text-align:right;"> 0.0134 </td>
   <td style="text-align:right;"> 6.125 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 06 </td>
   <td style="text-align:left;"> 06111 </td>
   <td style="text-align:right;"> 0.1869 </td>
   <td style="text-align:right;"> 0.0114 </td>
   <td style="text-align:right;"> 6.122 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05 </td>
   <td style="text-align:left;"> 05803 </td>
   <td style="text-align:right;"> 0.2085 </td>
   <td style="text-align:right;"> 0.0128 </td>
   <td style="text-align:right;"> 6.116 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 03 </td>
   <td style="text-align:left;"> 03102 </td>
   <td style="text-align:right;"> 0.1848 </td>
   <td style="text-align:right;"> 0.0113 </td>
   <td style="text-align:right;"> 6.113 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 07 </td>
   <td style="text-align:left;"> 07110 </td>
   <td style="text-align:right;"> 0.1903 </td>
   <td style="text-align:right;"> 0.0116 </td>
   <td style="text-align:right;"> 6.100 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 04 </td>
   <td style="text-align:left;"> 04103 </td>
   <td style="text-align:right;"> 0.1630 </td>
   <td style="text-align:right;"> 0.0099 </td>
   <td style="text-align:right;"> 6.072 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 06 </td>
   <td style="text-align:left;"> 06305 </td>
   <td style="text-align:right;"> 0.2171 </td>
   <td style="text-align:right;"> 0.0131 </td>
   <td style="text-align:right;"> 6.051 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 16 </td>
   <td style="text-align:left;"> 16201 </td>
   <td style="text-align:right;"> 0.2107 </td>
   <td style="text-align:right;"> 0.0127 </td>
   <td style="text-align:right;"> 6.028 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13403 </td>
   <td style="text-align:right;"> 0.1734 </td>
   <td style="text-align:right;"> 0.0104 </td>
   <td style="text-align:right;"> 5.981 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13108 </td>
   <td style="text-align:right;"> 0.2316 </td>
   <td style="text-align:right;"> 0.0138 </td>
   <td style="text-align:right;"> 5.972 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 08 </td>
   <td style="text-align:left;"> 08312 </td>
   <td style="text-align:right;"> 0.2063 </td>
   <td style="text-align:right;"> 0.0123 </td>
   <td style="text-align:right;"> 5.968 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 10205 </td>
   <td style="text-align:right;"> 0.2368 </td>
   <td style="text-align:right;"> 0.0141 </td>
   <td style="text-align:right;"> 5.963 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 09 </td>
   <td style="text-align:left;"> 09105 </td>
   <td style="text-align:right;"> 0.2723 </td>
   <td style="text-align:right;"> 0.0162 </td>
   <td style="text-align:right;"> 5.945 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13128 </td>
   <td style="text-align:right;"> 0.2346 </td>
   <td style="text-align:right;"> 0.0139 </td>
   <td style="text-align:right;"> 5.929 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 16 </td>
   <td style="text-align:left;"> 16107 </td>
   <td style="text-align:right;"> 0.2339 </td>
   <td style="text-align:right;"> 0.0139 </td>
   <td style="text-align:right;"> 5.921 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 07 </td>
   <td style="text-align:left;"> 07305 </td>
   <td style="text-align:right;"> 0.2093 </td>
   <td style="text-align:right;"> 0.0124 </td>
   <td style="text-align:right;"> 5.918 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 06 </td>
   <td style="text-align:left;"> 06105 </td>
   <td style="text-align:right;"> 0.1891 </td>
   <td style="text-align:right;"> 0.0112 </td>
   <td style="text-align:right;"> 5.910 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13118 </td>
   <td style="text-align:right;"> 0.1525 </td>
   <td style="text-align:right;"> 0.0090 </td>
   <td style="text-align:right;"> 5.903 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 09 </td>
   <td style="text-align:left;"> 09115 </td>
   <td style="text-align:right;"> 0.2520 </td>
   <td style="text-align:right;"> 0.0148 </td>
   <td style="text-align:right;"> 5.887 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 08 </td>
   <td style="text-align:left;"> 08106 </td>
   <td style="text-align:right;"> 0.2199 </td>
   <td style="text-align:right;"> 0.0129 </td>
   <td style="text-align:right;"> 5.881 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 09 </td>
   <td style="text-align:left;"> 09210 </td>
   <td style="text-align:right;"> 0.2422 </td>
   <td style="text-align:right;"> 0.0142 </td>
   <td style="text-align:right;"> 5.859 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 06 </td>
   <td style="text-align:left;"> 06110 </td>
   <td style="text-align:right;"> 0.2069 </td>
   <td style="text-align:right;"> 0.0121 </td>
   <td style="text-align:right;"> 5.856 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 08 </td>
   <td style="text-align:left;"> 08309 </td>
   <td style="text-align:right;"> 0.2129 </td>
   <td style="text-align:right;"> 0.0124 </td>
   <td style="text-align:right;"> 5.834 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13602 </td>
   <td style="text-align:right;"> 0.2127 </td>
   <td style="text-align:right;"> 0.0124 </td>
   <td style="text-align:right;"> 5.830 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 09 </td>
   <td style="text-align:left;"> 09209 </td>
   <td style="text-align:right;"> 0.2465 </td>
   <td style="text-align:right;"> 0.0143 </td>
   <td style="text-align:right;"> 5.787 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 04 </td>
   <td style="text-align:left;"> 04201 </td>
   <td style="text-align:right;"> 0.1844 </td>
   <td style="text-align:right;"> 0.0107 </td>
   <td style="text-align:right;"> 5.781 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 04 </td>
   <td style="text-align:left;"> 04303 </td>
   <td style="text-align:right;"> 0.2342 </td>
   <td style="text-align:right;"> 0.0135 </td>
   <td style="text-align:right;"> 5.776 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 10203 </td>
   <td style="text-align:right;"> 0.2150 </td>
   <td style="text-align:right;"> 0.0124 </td>
   <td style="text-align:right;"> 5.759 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 14 </td>
   <td style="text-align:left;"> 14108 </td>
   <td style="text-align:right;"> 0.2713 </td>
   <td style="text-align:right;"> 0.0156 </td>
   <td style="text-align:right;"> 5.758 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 07 </td>
   <td style="text-align:left;"> 07408 </td>
   <td style="text-align:right;"> 0.2362 </td>
   <td style="text-align:right;"> 0.0136 </td>
   <td style="text-align:right;"> 5.748 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 03 </td>
   <td style="text-align:left;"> 03304 </td>
   <td style="text-align:right;"> 0.1777 </td>
   <td style="text-align:right;"> 0.0102 </td>
   <td style="text-align:right;"> 5.745 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 07 </td>
   <td style="text-align:left;"> 07304 </td>
   <td style="text-align:right;"> 0.2072 </td>
   <td style="text-align:right;"> 0.0119 </td>
   <td style="text-align:right;"> 5.734 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 07 </td>
   <td style="text-align:left;"> 07102 </td>
   <td style="text-align:right;"> 0.2089 </td>
   <td style="text-align:right;"> 0.0120 </td>
   <td style="text-align:right;"> 5.728 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05 </td>
   <td style="text-align:left;"> 05603 </td>
   <td style="text-align:right;"> 0.2184 </td>
   <td style="text-align:right;"> 0.0125 </td>
   <td style="text-align:right;"> 5.713 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 16 </td>
   <td style="text-align:left;"> 16108 </td>
   <td style="text-align:right;"> 0.2617 </td>
   <td style="text-align:right;"> 0.0149 </td>
   <td style="text-align:right;"> 5.688 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 09 </td>
   <td style="text-align:left;"> 09119 </td>
   <td style="text-align:right;"> 0.2698 </td>
   <td style="text-align:right;"> 0.0153 </td>
   <td style="text-align:right;"> 5.683 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 10305 </td>
   <td style="text-align:right;"> 0.2269 </td>
   <td style="text-align:right;"> 0.0129 </td>
   <td style="text-align:right;"> 5.682 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05 </td>
   <td style="text-align:left;"> 05102 </td>
   <td style="text-align:right;"> 0.1862 </td>
   <td style="text-align:right;"> 0.0106 </td>
   <td style="text-align:right;"> 5.667 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05 </td>
   <td style="text-align:left;"> 05503 </td>
   <td style="text-align:right;"> 0.2152 </td>
   <td style="text-align:right;"> 0.0122 </td>
   <td style="text-align:right;"> 5.655 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 08 </td>
   <td style="text-align:left;"> 08206 </td>
   <td style="text-align:right;"> 0.2393 </td>
   <td style="text-align:right;"> 0.0135 </td>
   <td style="text-align:right;"> 5.652 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13603 </td>
   <td style="text-align:right;"> 0.2100 </td>
   <td style="text-align:right;"> 0.0119 </td>
   <td style="text-align:right;"> 5.652 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 06 </td>
   <td style="text-align:left;"> 06107 </td>
   <td style="text-align:right;"> 0.2254 </td>
   <td style="text-align:right;"> 0.0127 </td>
   <td style="text-align:right;"> 5.647 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 08 </td>
   <td style="text-align:left;"> 08108 </td>
   <td style="text-align:right;"> 0.1813 </td>
   <td style="text-align:right;"> 0.0102 </td>
   <td style="text-align:right;"> 5.643 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 08 </td>
   <td style="text-align:left;"> 08203 </td>
   <td style="text-align:right;"> 0.2631 </td>
   <td style="text-align:right;"> 0.0148 </td>
   <td style="text-align:right;"> 5.627 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 16 </td>
   <td style="text-align:left;"> 16303 </td>
   <td style="text-align:right;"> 0.2270 </td>
   <td style="text-align:right;"> 0.0128 </td>
   <td style="text-align:right;"> 5.623 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 16 </td>
   <td style="text-align:left;"> 16109 </td>
   <td style="text-align:right;"> 0.1863 </td>
   <td style="text-align:right;"> 0.0105 </td>
   <td style="text-align:right;"> 5.621 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05 </td>
   <td style="text-align:left;"> 05604 </td>
   <td style="text-align:right;"> 0.2127 </td>
   <td style="text-align:right;"> 0.0119 </td>
   <td style="text-align:right;"> 5.616 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 10102 </td>
   <td style="text-align:right;"> 0.2411 </td>
   <td style="text-align:right;"> 0.0135 </td>
   <td style="text-align:right;"> 5.610 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 07 </td>
   <td style="text-align:left;"> 07405 </td>
   <td style="text-align:right;"> 0.2325 </td>
   <td style="text-align:right;"> 0.0130 </td>
   <td style="text-align:right;"> 5.604 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 06 </td>
   <td style="text-align:left;"> 06106 </td>
   <td style="text-align:right;"> 0.2049 </td>
   <td style="text-align:right;"> 0.0115 </td>
   <td style="text-align:right;"> 5.600 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05 </td>
   <td style="text-align:left;"> 05801 </td>
   <td style="text-align:right;"> 0.1648 </td>
   <td style="text-align:right;"> 0.0092 </td>
   <td style="text-align:right;"> 5.598 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 08 </td>
   <td style="text-align:left;"> 08107 </td>
   <td style="text-align:right;"> 0.2248 </td>
   <td style="text-align:right;"> 0.0126 </td>
   <td style="text-align:right;"> 5.594 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 16 </td>
   <td style="text-align:left;"> 16305 </td>
   <td style="text-align:right;"> 0.2193 </td>
   <td style="text-align:right;"> 0.0123 </td>
   <td style="text-align:right;"> 5.588 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 16 </td>
   <td style="text-align:left;"> 16105 </td>
   <td style="text-align:right;"> 0.2364 </td>
   <td style="text-align:right;"> 0.0131 </td>
   <td style="text-align:right;"> 5.542 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13125 </td>
   <td style="text-align:right;"> 0.2165 </td>
   <td style="text-align:right;"> 0.0118 </td>
   <td style="text-align:right;"> 5.469 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13102 </td>
   <td style="text-align:right;"> 0.1971 </td>
   <td style="text-align:right;"> 0.0107 </td>
   <td style="text-align:right;"> 5.445 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05 </td>
   <td style="text-align:left;"> 05502 </td>
   <td style="text-align:right;"> 0.2150 </td>
   <td style="text-align:right;"> 0.0117 </td>
   <td style="text-align:right;"> 5.428 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13117 </td>
   <td style="text-align:right;"> 0.2516 </td>
   <td style="text-align:right;"> 0.0136 </td>
   <td style="text-align:right;"> 5.410 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13402 </td>
   <td style="text-align:right;"> 0.1940 </td>
   <td style="text-align:right;"> 0.0105 </td>
   <td style="text-align:right;"> 5.410 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 09 </td>
   <td style="text-align:left;"> 09102 </td>
   <td style="text-align:right;"> 0.2624 </td>
   <td style="text-align:right;"> 0.0142 </td>
   <td style="text-align:right;"> 5.402 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 08 </td>
   <td style="text-align:left;"> 08313 </td>
   <td style="text-align:right;"> 0.2253 </td>
   <td style="text-align:right;"> 0.0122 </td>
   <td style="text-align:right;"> 5.394 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13601 </td>
   <td style="text-align:right;"> 0.1959 </td>
   <td style="text-align:right;"> 0.0106 </td>
   <td style="text-align:right;"> 5.391 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05 </td>
   <td style="text-align:left;"> 05706 </td>
   <td style="text-align:right;"> 0.2120 </td>
   <td style="text-align:right;"> 0.0114 </td>
   <td style="text-align:right;"> 5.384 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05 </td>
   <td style="text-align:left;"> 05301 </td>
   <td style="text-align:right;"> 0.1785 </td>
   <td style="text-align:right;"> 0.0096 </td>
   <td style="text-align:right;"> 5.376 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05 </td>
   <td style="text-align:left;"> 05802 </td>
   <td style="text-align:right;"> 0.1871 </td>
   <td style="text-align:right;"> 0.0101 </td>
   <td style="text-align:right;"> 5.376 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 08 </td>
   <td style="text-align:left;"> 08111 </td>
   <td style="text-align:right;"> 0.1913 </td>
   <td style="text-align:right;"> 0.0103 </td>
   <td style="text-align:right;"> 5.374 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 16 </td>
   <td style="text-align:left;"> 16106 </td>
   <td style="text-align:right;"> 0.2530 </td>
   <td style="text-align:right;"> 0.0136 </td>
   <td style="text-align:right;"> 5.370 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 07 </td>
   <td style="text-align:left;"> 07308 </td>
   <td style="text-align:right;"> 0.2374 </td>
   <td style="text-align:right;"> 0.0127 </td>
   <td style="text-align:right;"> 5.343 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 06 </td>
   <td style="text-align:left;"> 06114 </td>
   <td style="text-align:right;"> 0.2093 </td>
   <td style="text-align:right;"> 0.0111 </td>
   <td style="text-align:right;"> 5.297 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 08 </td>
   <td style="text-align:left;"> 08307 </td>
   <td style="text-align:right;"> 0.2317 </td>
   <td style="text-align:right;"> 0.0122 </td>
   <td style="text-align:right;"> 5.286 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13605 </td>
   <td style="text-align:right;"> 0.1982 </td>
   <td style="text-align:right;"> 0.0105 </td>
   <td style="text-align:right;"> 5.275 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13130 </td>
   <td style="text-align:right;"> 0.1235 </td>
   <td style="text-align:right;"> 0.0065 </td>
   <td style="text-align:right;"> 5.238 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13103 </td>
   <td style="text-align:right;"> 0.2600 </td>
   <td style="text-align:right;"> 0.0136 </td>
   <td style="text-align:right;"> 5.235 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13109 </td>
   <td style="text-align:right;"> 0.1681 </td>
   <td style="text-align:right;"> 0.0088 </td>
   <td style="text-align:right;"> 5.208 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 14 </td>
   <td style="text-align:left;"> 14103 </td>
   <td style="text-align:right;"> 0.2412 </td>
   <td style="text-align:right;"> 0.0125 </td>
   <td style="text-align:right;"> 5.175 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 14 </td>
   <td style="text-align:left;"> 14105 </td>
   <td style="text-align:right;"> 0.2372 </td>
   <td style="text-align:right;"> 0.0123 </td>
   <td style="text-align:right;"> 5.166 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 06 </td>
   <td style="text-align:left;"> 06201 </td>
   <td style="text-align:right;"> 0.2034 </td>
   <td style="text-align:right;"> 0.0105 </td>
   <td style="text-align:right;"> 5.158 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 08 </td>
   <td style="text-align:left;"> 08202 </td>
   <td style="text-align:right;"> 0.2036 </td>
   <td style="text-align:right;"> 0.0104 </td>
   <td style="text-align:right;"> 5.111 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 09 </td>
   <td style="text-align:left;"> 09103 </td>
   <td style="text-align:right;"> 0.2407 </td>
   <td style="text-align:right;"> 0.0123 </td>
   <td style="text-align:right;"> 5.099 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 08 </td>
   <td style="text-align:left;"> 08311 </td>
   <td style="text-align:right;"> 0.2377 </td>
   <td style="text-align:right;"> 0.0121 </td>
   <td style="text-align:right;"> 5.096 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13106 </td>
   <td style="text-align:right;"> 0.2194 </td>
   <td style="text-align:right;"> 0.0112 </td>
   <td style="text-align:right;"> 5.093 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 16 </td>
   <td style="text-align:left;"> 16302 </td>
   <td style="text-align:right;"> 0.2815 </td>
   <td style="text-align:right;"> 0.0143 </td>
   <td style="text-align:right;"> 5.087 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 10303 </td>
   <td style="text-align:right;"> 0.2243 </td>
   <td style="text-align:right;"> 0.0114 </td>
   <td style="text-align:right;"> 5.080 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 09 </td>
   <td style="text-align:left;"> 09203 </td>
   <td style="text-align:right;"> 0.2351 </td>
   <td style="text-align:right;"> 0.0119 </td>
   <td style="text-align:right;"> 5.074 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 08 </td>
   <td style="text-align:left;"> 08105 </td>
   <td style="text-align:right;"> 0.2390 </td>
   <td style="text-align:right;"> 0.0120 </td>
   <td style="text-align:right;"> 5.033 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 08 </td>
   <td style="text-align:left;"> 08112 </td>
   <td style="text-align:right;"> 0.1998 </td>
   <td style="text-align:right;"> 0.0100 </td>
   <td style="text-align:right;"> 5.028 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13131 </td>
   <td style="text-align:right;"> 0.2563 </td>
   <td style="text-align:right;"> 0.0129 </td>
   <td style="text-align:right;"> 5.022 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 08 </td>
   <td style="text-align:left;"> 08304 </td>
   <td style="text-align:right;"> 0.1994 </td>
   <td style="text-align:right;"> 0.0100 </td>
   <td style="text-align:right;"> 5.017 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 09 </td>
   <td style="text-align:left;"> 09202 </td>
   <td style="text-align:right;"> 0.2511 </td>
   <td style="text-align:right;"> 0.0126 </td>
   <td style="text-align:right;"> 5.010 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 06 </td>
   <td style="text-align:left;"> 06116 </td>
   <td style="text-align:right;"> 0.1993 </td>
   <td style="text-align:right;"> 0.0099 </td>
   <td style="text-align:right;"> 4.989 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 14 </td>
   <td style="text-align:left;"> 14104 </td>
   <td style="text-align:right;"> 0.2397 </td>
   <td style="text-align:right;"> 0.0119 </td>
   <td style="text-align:right;"> 4.979 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05 </td>
   <td style="text-align:left;"> 05401 </td>
   <td style="text-align:right;"> 0.2064 </td>
   <td style="text-align:right;"> 0.0103 </td>
   <td style="text-align:right;"> 4.974 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 14 </td>
   <td style="text-align:left;"> 14204 </td>
   <td style="text-align:right;"> 0.2357 </td>
   <td style="text-align:right;"> 0.0117 </td>
   <td style="text-align:right;"> 4.974 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05 </td>
   <td style="text-align:left;"> 05804 </td>
   <td style="text-align:right;"> 0.1867 </td>
   <td style="text-align:right;"> 0.0093 </td>
   <td style="text-align:right;"> 4.964 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 06 </td>
   <td style="text-align:left;"> 06109 </td>
   <td style="text-align:right;"> 0.2159 </td>
   <td style="text-align:right;"> 0.0107 </td>
   <td style="text-align:right;"> 4.960 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 07 </td>
   <td style="text-align:left;"> 07407 </td>
   <td style="text-align:right;"> 0.1974 </td>
   <td style="text-align:right;"> 0.0098 </td>
   <td style="text-align:right;"> 4.955 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 07 </td>
   <td style="text-align:left;"> 07402 </td>
   <td style="text-align:right;"> 0.2206 </td>
   <td style="text-align:right;"> 0.0109 </td>
   <td style="text-align:right;"> 4.942 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 08 </td>
   <td style="text-align:left;"> 08103 </td>
   <td style="text-align:right;"> 0.1832 </td>
   <td style="text-align:right;"> 0.0091 </td>
   <td style="text-align:right;"> 4.940 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 08 </td>
   <td style="text-align:left;"> 08110 </td>
   <td style="text-align:right;"> 0.1849 </td>
   <td style="text-align:right;"> 0.0091 </td>
   <td style="text-align:right;"> 4.940 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 09 </td>
   <td style="text-align:left;"> 09113 </td>
   <td style="text-align:right;"> 0.2476 </td>
   <td style="text-align:right;"> 0.0122 </td>
   <td style="text-align:right;"> 4.921 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 09 </td>
   <td style="text-align:left;"> 09114 </td>
   <td style="text-align:right;"> 0.2316 </td>
   <td style="text-align:right;"> 0.0114 </td>
   <td style="text-align:right;"> 4.902 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05 </td>
   <td style="text-align:left;"> 05701 </td>
   <td style="text-align:right;"> 0.2219 </td>
   <td style="text-align:right;"> 0.0108 </td>
   <td style="text-align:right;"> 4.858 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 14 </td>
   <td style="text-align:left;"> 14201 </td>
   <td style="text-align:right;"> 0.2086 </td>
   <td style="text-align:right;"> 0.0101 </td>
   <td style="text-align:right;"> 4.829 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13604 </td>
   <td style="text-align:right;"> 0.2038 </td>
   <td style="text-align:right;"> 0.0098 </td>
   <td style="text-align:right;"> 4.827 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05 </td>
   <td style="text-align:left;"> 05601 </td>
   <td style="text-align:right;"> 0.2150 </td>
   <td style="text-align:right;"> 0.0103 </td>
   <td style="text-align:right;"> 4.799 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 02 </td>
   <td style="text-align:left;"> 02201 </td>
   <td style="text-align:right;"> 0.1942 </td>
   <td style="text-align:right;"> 0.0093 </td>
   <td style="text-align:right;"> 4.770 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13116 </td>
   <td style="text-align:right;"> 0.2719 </td>
   <td style="text-align:right;"> 0.0129 </td>
   <td style="text-align:right;"> 4.757 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 08 </td>
   <td style="text-align:left;"> 08101 </td>
   <td style="text-align:right;"> 0.1740 </td>
   <td style="text-align:right;"> 0.0082 </td>
   <td style="text-align:right;"> 4.740 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 16 </td>
   <td style="text-align:left;"> 16103 </td>
   <td style="text-align:right;"> 0.2372 </td>
   <td style="text-align:right;"> 0.0112 </td>
   <td style="text-align:right;"> 4.723 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 06 </td>
   <td style="text-align:left;"> 06310 </td>
   <td style="text-align:right;"> 0.2086 </td>
   <td style="text-align:right;"> 0.0098 </td>
   <td style="text-align:right;"> 4.702 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05 </td>
   <td style="text-align:left;"> 05109 </td>
   <td style="text-align:right;"> 0.1724 </td>
   <td style="text-align:right;"> 0.0081 </td>
   <td style="text-align:right;"> 4.679 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 06 </td>
   <td style="text-align:left;"> 06303 </td>
   <td style="text-align:right;"> 0.2292 </td>
   <td style="text-align:right;"> 0.0107 </td>
   <td style="text-align:right;"> 4.669 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 08 </td>
   <td style="text-align:left;"> 08306 </td>
   <td style="text-align:right;"> 0.2079 </td>
   <td style="text-align:right;"> 0.0097 </td>
   <td style="text-align:right;"> 4.657 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13121 </td>
   <td style="text-align:right;"> 0.2317 </td>
   <td style="text-align:right;"> 0.0107 </td>
   <td style="text-align:right;"> 4.637 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 08 </td>
   <td style="text-align:left;"> 08305 </td>
   <td style="text-align:right;"> 0.2403 </td>
   <td style="text-align:right;"> 0.0111 </td>
   <td style="text-align:right;"> 4.634 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 16 </td>
   <td style="text-align:left;"> 16102 </td>
   <td style="text-align:right;"> 0.2412 </td>
   <td style="text-align:right;"> 0.0112 </td>
   <td style="text-align:right;"> 4.633 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 14 </td>
   <td style="text-align:left;"> 14202 </td>
   <td style="text-align:right;"> 0.2801 </td>
   <td style="text-align:right;"> 0.0130 </td>
   <td style="text-align:right;"> 4.624 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13201 </td>
   <td style="text-align:right;"> 0.2139 </td>
   <td style="text-align:right;"> 0.0099 </td>
   <td style="text-align:right;"> 4.612 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 10202 </td>
   <td style="text-align:right;"> 0.2341 </td>
   <td style="text-align:right;"> 0.0107 </td>
   <td style="text-align:right;"> 4.590 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 07 </td>
   <td style="text-align:left;"> 07404 </td>
   <td style="text-align:right;"> 0.2155 </td>
   <td style="text-align:right;"> 0.0099 </td>
   <td style="text-align:right;"> 4.583 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 10301 </td>
   <td style="text-align:right;"> 0.2047 </td>
   <td style="text-align:right;"> 0.0093 </td>
   <td style="text-align:right;"> 4.562 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 08 </td>
   <td style="text-align:left;"> 08301 </td>
   <td style="text-align:right;"> 0.2225 </td>
   <td style="text-align:right;"> 0.0101 </td>
   <td style="text-align:right;"> 4.545 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 07 </td>
   <td style="text-align:left;"> 07109 </td>
   <td style="text-align:right;"> 0.2240 </td>
   <td style="text-align:right;"> 0.0102 </td>
   <td style="text-align:right;"> 4.544 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 08 </td>
   <td style="text-align:left;"> 08303 </td>
   <td style="text-align:right;"> 0.2283 </td>
   <td style="text-align:right;"> 0.0104 </td>
   <td style="text-align:right;"> 4.538 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 07 </td>
   <td style="text-align:left;"> 07201 </td>
   <td style="text-align:right;"> 0.1944 </td>
   <td style="text-align:right;"> 0.0088 </td>
   <td style="text-align:right;"> 4.525 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 09 </td>
   <td style="text-align:left;"> 09109 </td>
   <td style="text-align:right;"> 0.2448 </td>
   <td style="text-align:right;"> 0.0111 </td>
   <td style="text-align:right;"> 4.516 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13129 </td>
   <td style="text-align:right;"> 0.2056 </td>
   <td style="text-align:right;"> 0.0093 </td>
   <td style="text-align:right;"> 4.504 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 06 </td>
   <td style="text-align:left;"> 06115 </td>
   <td style="text-align:right;"> 0.2378 </td>
   <td style="text-align:right;"> 0.0107 </td>
   <td style="text-align:right;"> 4.502 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13104 </td>
   <td style="text-align:right;"> 0.2182 </td>
   <td style="text-align:right;"> 0.0098 </td>
   <td style="text-align:right;"> 4.501 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13101 </td>
   <td style="text-align:right;"> 0.1832 </td>
   <td style="text-align:right;"> 0.0082 </td>
   <td style="text-align:right;"> 4.491 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 03 </td>
   <td style="text-align:left;"> 03301 </td>
   <td style="text-align:right;"> 0.1947 </td>
   <td style="text-align:right;"> 0.0087 </td>
   <td style="text-align:right;"> 4.474 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 10201 </td>
   <td style="text-align:right;"> 0.2115 </td>
   <td style="text-align:right;"> 0.0095 </td>
   <td style="text-align:right;"> 4.471 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 07 </td>
   <td style="text-align:left;"> 07403 </td>
   <td style="text-align:right;"> 0.2493 </td>
   <td style="text-align:right;"> 0.0111 </td>
   <td style="text-align:right;"> 4.465 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 09 </td>
   <td style="text-align:left;"> 09112 </td>
   <td style="text-align:right;"> 0.2807 </td>
   <td style="text-align:right;"> 0.0125 </td>
   <td style="text-align:right;"> 4.446 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 06 </td>
   <td style="text-align:left;"> 06301 </td>
   <td style="text-align:right;"> 0.1979 </td>
   <td style="text-align:right;"> 0.0088 </td>
   <td style="text-align:right;"> 4.439 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 14 </td>
   <td style="text-align:left;"> 14107 </td>
   <td style="text-align:right;"> 0.2381 </td>
   <td style="text-align:right;"> 0.0106 </td>
   <td style="text-align:right;"> 4.433 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 01 </td>
   <td style="text-align:left;"> 01107 </td>
   <td style="text-align:right;"> 0.2786 </td>
   <td style="text-align:right;"> 0.0123 </td>
   <td style="text-align:right;"> 4.432 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13105 </td>
   <td style="text-align:right;"> 0.2600 </td>
   <td style="text-align:right;"> 0.0115 </td>
   <td style="text-align:right;"> 4.426 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13124 </td>
   <td style="text-align:right;"> 0.2071 </td>
   <td style="text-align:right;"> 0.0091 </td>
   <td style="text-align:right;"> 4.411 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13127 </td>
   <td style="text-align:right;"> 0.2371 </td>
   <td style="text-align:right;"> 0.0103 </td>
   <td style="text-align:right;"> 4.342 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 09 </td>
   <td style="text-align:left;"> 09111 </td>
   <td style="text-align:right;"> 0.2585 </td>
   <td style="text-align:right;"> 0.0112 </td>
   <td style="text-align:right;"> 4.326 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13110 </td>
   <td style="text-align:right;"> 0.1709 </td>
   <td style="text-align:right;"> 0.0074 </td>
   <td style="text-align:right;"> 4.310 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13111 </td>
   <td style="text-align:right;"> 0.2535 </td>
   <td style="text-align:right;"> 0.0109 </td>
   <td style="text-align:right;"> 4.304 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13119 </td>
   <td style="text-align:right;"> 0.1680 </td>
   <td style="text-align:right;"> 0.0072 </td>
   <td style="text-align:right;"> 4.297 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 11 </td>
   <td style="text-align:left;"> 11201 </td>
   <td style="text-align:right;"> 0.2157 </td>
   <td style="text-align:right;"> 0.0092 </td>
   <td style="text-align:right;"> 4.275 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13112 </td>
   <td style="text-align:right;"> 0.3151 </td>
   <td style="text-align:right;"> 0.0134 </td>
   <td style="text-align:right;"> 4.241 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 09 </td>
   <td style="text-align:left;"> 09201 </td>
   <td style="text-align:right;"> 0.2236 </td>
   <td style="text-align:right;"> 0.0095 </td>
   <td style="text-align:right;"> 4.235 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 09 </td>
   <td style="text-align:left;"> 09120 </td>
   <td style="text-align:right;"> 0.2500 </td>
   <td style="text-align:right;"> 0.0105 </td>
   <td style="text-align:right;"> 4.210 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 06 </td>
   <td style="text-align:left;"> 06117 </td>
   <td style="text-align:right;"> 0.1913 </td>
   <td style="text-align:right;"> 0.0080 </td>
   <td style="text-align:right;"> 4.171 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13126 </td>
   <td style="text-align:right;"> 0.2094 </td>
   <td style="text-align:right;"> 0.0087 </td>
   <td style="text-align:right;"> 4.158 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 09 </td>
   <td style="text-align:left;"> 09108 </td>
   <td style="text-align:right;"> 0.2501 </td>
   <td style="text-align:right;"> 0.0103 </td>
   <td style="text-align:right;"> 4.129 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 08 </td>
   <td style="text-align:left;"> 08102 </td>
   <td style="text-align:right;"> 0.2270 </td>
   <td style="text-align:right;"> 0.0093 </td>
   <td style="text-align:right;"> 4.115 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 09 </td>
   <td style="text-align:left;"> 09211 </td>
   <td style="text-align:right;"> 0.2371 </td>
   <td style="text-align:right;"> 0.0097 </td>
   <td style="text-align:right;"> 4.087 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13122 </td>
   <td style="text-align:right;"> 0.1949 </td>
   <td style="text-align:right;"> 0.0079 </td>
   <td style="text-align:right;"> 4.067 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 14 </td>
   <td style="text-align:left;"> 14106 </td>
   <td style="text-align:right;"> 0.2582 </td>
   <td style="text-align:right;"> 0.0105 </td>
   <td style="text-align:right;"> 4.060 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05 </td>
   <td style="text-align:left;"> 05501 </td>
   <td style="text-align:right;"> 0.1957 </td>
   <td style="text-align:right;"> 0.0078 </td>
   <td style="text-align:right;"> 4.012 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 07 </td>
   <td style="text-align:left;"> 07301 </td>
   <td style="text-align:right;"> 0.2028 </td>
   <td style="text-align:right;"> 0.0080 </td>
   <td style="text-align:right;"> 3.968 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 16 </td>
   <td style="text-align:left;"> 16301 </td>
   <td style="text-align:right;"> 0.2302 </td>
   <td style="text-align:right;"> 0.0090 </td>
   <td style="text-align:right;"> 3.923 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 04 </td>
   <td style="text-align:left;"> 04101 </td>
   <td style="text-align:right;"> 0.1720 </td>
   <td style="text-align:right;"> 0.0067 </td>
   <td style="text-align:right;"> 3.913 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 04 </td>
   <td style="text-align:left;"> 04301 </td>
   <td style="text-align:right;"> 0.2048 </td>
   <td style="text-align:right;"> 0.0079 </td>
   <td style="text-align:right;"> 3.845 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05 </td>
   <td style="text-align:left;"> 05101 </td>
   <td style="text-align:right;"> 0.2110 </td>
   <td style="text-align:right;"> 0.0080 </td>
   <td style="text-align:right;"> 3.782 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 07 </td>
   <td style="text-align:left;"> 07101 </td>
   <td style="text-align:right;"> 0.1851 </td>
   <td style="text-align:right;"> 0.0069 </td>
   <td style="text-align:right;"> 3.718 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 13401 </td>
   <td style="text-align:right;"> 0.2428 </td>
   <td style="text-align:right;"> 0.0090 </td>
   <td style="text-align:right;"> 3.688 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 06 </td>
   <td style="text-align:left;"> 06101 </td>
   <td style="text-align:right;"> 0.1921 </td>
   <td style="text-align:right;"> 0.0071 </td>
   <td style="text-align:right;"> 3.679 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 07 </td>
   <td style="text-align:left;"> 07401 </td>
   <td style="text-align:right;"> 0.2026 </td>
   <td style="text-align:right;"> 0.0074 </td>
   <td style="text-align:right;"> 3.666 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 07 </td>
   <td style="text-align:left;"> 07406 </td>
   <td style="text-align:right;"> 0.2076 </td>
   <td style="text-align:right;"> 0.0076 </td>
   <td style="text-align:right;"> 3.642 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 10101 </td>
   <td style="text-align:right;"> 0.2175 </td>
   <td style="text-align:right;"> 0.0074 </td>
   <td style="text-align:right;"> 3.393 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 01 </td>
   <td style="text-align:left;"> 01101 </td>
   <td style="text-align:right;"> 0.1862 </td>
   <td style="text-align:right;"> 0.0062 </td>
   <td style="text-align:right;"> 3.314 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 04 </td>
   <td style="text-align:left;"> 04102 </td>
   <td style="text-align:right;"> 0.1973 </td>
   <td style="text-align:right;"> 0.0065 </td>
   <td style="text-align:right;"> 3.272 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 14 </td>
   <td style="text-align:left;"> 14101 </td>
   <td style="text-align:right;"> 0.2004 </td>
   <td style="text-align:right;"> 0.0064 </td>
   <td style="text-align:right;"> 3.202 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 09 </td>
   <td style="text-align:left;"> 09101 </td>
   <td style="text-align:right;"> 0.2148 </td>
   <td style="text-align:right;"> 0.0066 </td>
   <td style="text-align:right;"> 3.075 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 16 </td>
   <td style="text-align:left;"> 16101 </td>
   <td style="text-align:right;"> 0.2131 </td>
   <td style="text-align:right;"> 0.0065 </td>
   <td style="text-align:right;"> 3.042 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 03 </td>
   <td style="text-align:left;"> 03101 </td>
   <td style="text-align:right;"> 0.1832 </td>
   <td style="text-align:right;"> 0.0052 </td>
   <td style="text-align:right;"> 2.846 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 11 </td>
   <td style="text-align:left;"> 11101 </td>
   <td style="text-align:right;"> 0.1825 </td>
   <td style="text-align:right;"> 0.0048 </td>
   <td style="text-align:right;"> 2.620 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 02 </td>
   <td style="text-align:left;"> 02101 </td>
   <td style="text-align:right;"> 0.1934 </td>
   <td style="text-align:right;"> 0.0047 </td>
   <td style="text-align:right;"> 2.426 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 12 </td>
   <td style="text-align:left;"> 12101 </td>
   <td style="text-align:right;"> 0.1449 </td>
   <td style="text-align:right;"> 0.0029 </td>
   <td style="text-align:right;"> 1.973 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 15 </td>
   <td style="text-align:left;"> 15101 </td>
   <td style="text-align:right;"> 0.2151 </td>
   <td style="text-align:right;"> 0.0007 </td>
   <td style="text-align:right;"> 0.331 </td>
  </tr>
</tbody>
</table>




