################################################################################
## Title:        Modelo de área (\theta ~ beta)                             ##
## Authors:      Stalyn Guerrero- Andrés Gutiérrez                            ##
## Date:         05-2023                                                      ##
################################################################################

## Lectura de librerías 
rm(list = ls())
library(tidyverse)
library(magrittr)
library(patchwork)
library(thematic)
library(rstan)
library(rstanarm)
library(rstantools)
library(bayesplot)
library(posterior)

theme_set(theme_bw())
thematic_on(
  bg = "white", fg = "black", accent = "red",
  font = font_spec("Oxanium", scale = 1.25)
)
select <- dplyr::select
dam2 <- "comuna"
## Lectura de base de datos

model_FH_Binomial <- readRDS("Data/model_FH_Binomial2.rds")
id_orden <- readRDS("Data/id_Orden.rds")

y_pred <- as.array(model_FH_Binomial, pars = "thetaLP") %>% 
  as_draws_df() %>% select(matches("theta"))

y_pred <- data.frame(t(y_pred)) %>% as_tibble()
colnames(y_pred)<- paste0("iter_",1:ncol(y_pred))
y_pred %<>% mutate(id_Orden = 1:n()) 

# 0. Leer estimaciones del modelo

estimacionesPre <- readRDS("Data/id_Orden.rds") %>%
  transmute(dam2 = haven::as_factor(comuna, levels = "values"),
            dam2 = str_pad(width = 5, dam2, pad = "0"),
            dam = str_sub(dam2,1,2),
            id_Orden)
estimacionesPre <- inner_join(estimacionesPre,y_pred)


# 1. Del censo extraer el total de personas por DAM2 

total_hh <- readRDS(file = "Data/Total_Hogares.rds")
N_hh <- total_hh %>% group_by(dam = str_sub(dam2,1,2)) %>%  
  mutate(dam_hh = sum(NN_Hogar) ) 

# 2. Obtener las estimaciones directa por DAM o el nivel de 
#  agregación en el cual la encuesta es representativa. 

directoDam <- readRDS("Data/FIES_region.rds") %>% 
  dplyr::select( dam, ModerateSevere = FIES)

# 3. Realizar el consolidando información obtenida en *1* y *2*. 

temp <- estimacionesPre %>%
  inner_join(N_hh ) %>% 
  inner_join(directoDam )

#  4. Con la información organizada realizar el calculo de los pesos para el Benchmark

R_dam2 <- temp %>% group_by(dam) %>%
  mutate(across(
    starts_with("iter_"),
    ~
      unique(ModerateSevere) / sum((NN_Hogar  / dam_hh) * .),
    .names = "gk_{.col}"
  ),
  across(starts_with("iter_"), ~NULL)
  ) 

# calculando los pesos para cada dominio.

pesos <- temp %>% 
  mutate(W_i = NN_Hogar / dam_hh) %>% 
  select(dam2, W_i)


# 5. Realizar la estimación FH  Benchmark 

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
paso <- readRDS("Data/replicas_RBench.rds")
paso_RBench <- paso %>% select(matches("RBench_"))

estimacionesBench <- data.frame(
  dam = paso$dam,
  dam2 = paso$dam2,
theta_pred_RBench_rep = apply(paso_RBench,MARGIN = 1,mean),
theta_pred_RBench_rep_sd = apply(paso_RBench,MARGIN = 1,sd)
) %>% left_join(pesos)


# 6. Validación: Estimación FH con Benchmark

estimacionesBench %>% group_by(dam) %>%
  summarise(theta_reg_RB = sum(W_i * theta_pred_RBench_rep)) %>%
  left_join(directoDam, by = "dam") %>% 
  data.frame()


## Validación de los resultados. 

estimacionesPre <- readRDS("Data/estimacionesPre.rds") %>%
  transmute(dam2 = comuna,
            dam = str_sub(dam2,1,2),
            theta_pred,theta_pred_EE)



# La visualización resultante del siguiente código muestra puntos de 
# diferentes formas y colores para representar los diferentes métodos 
# de estimación, y dos líneas punteadas que representan los intervalos de 
# confianza superior e inferior para los valores observados en la variable 
# `theta_dir`.

IC_dir <- readRDS("Data/FIES_region.rds") %>%
  dplyr::select(dam, FIES, var_hat) %>%
  transmute(dam,
            Ls = FIES + 1.96 * sqrt(var_hat),
            Li = FIES - 1.96 * sqrt(var_hat))

temp <- estimacionesBench %>% left_join( estimacionesPre ) %>% 
  group_by(dam) %>% 
  summarise(
   # "FIES Modelo" = sum(W_i * theta_pred),
   # "FIES Modelo Syn" = sum(W_i * thetaSyn),
   # "FIES Modelo Bench" = sum(W_i * theta_pred_RBench),
    "FIES Modelo Bench" = sum(W_i * theta_pred_RBench_rep)
  ) %>%   
  left_join(directoDam, by = "dam")  %>% 
  mutate(id = 1:n()) %>% rename("FIES Directo"  = ModerateSevere)

temp %<>% gather(key = "Metodo",value = "Estimacion",
                 -id, -dam)
temp <- inner_join(temp,IC_dir)
p_temp <- ggplot(data = temp, aes(x = id, y = Estimacion, shape = Metodo)) +
  geom_jitter(aes(color = Metodo), size = 2) +
  geom_line(aes(y = Li), linetype  = 2) +
  geom_line(aes(y = Ls),  linetype  = 2) +
  theme_bw(20) + 
  scale_x_continuous(breaks = temp$id,
                     labels =  temp$dam) +
  labs(y = "", x = "")
   
ggsave(plot = p_temp,
       filename =  "Data/RecursosBook/03/2_validacion_Bench_rep2.jpeg", 
       scale = 1.5)

## Resultados del Benchmark

temp <- estimacionesBench %>% left_join( estimacionesPre ) 


saveRDS(object = estimacionesBench %>% select(-W_i),
        "Data/estimacionesBench_replicas.rds")

