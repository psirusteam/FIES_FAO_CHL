################################################################################
## Title:        Modelo de área (arcosin(\theta) ~ normal)                             ##
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
library(readxl)

theme_set(theme_bw())
thematic_on(
  bg = "white", fg = "black", accent = "red",
  font = font_spec("Oxanium", scale = 1.25)
)
select <- dplyr::select

## Lectura de base de datos
dam2 <- "comuna"

base_FH <-
  readRDS(
    "Data/SAE-FIES Chile/Material for bayesian application/data_SAE_complete_final_arcsin.rds"
  ) %>%
  mutate(comuna = str_pad(
    haven::as_factor(comuna, levels = "values"),
    width = 5,
    pad = "0"
  ))



base_FH <- base_FH %>% mutate(
   T_ModerateSevere = asin(sqrt(ModerateSevere)),  ## creando zd
  varhat = 1/(4*n_eff_fgv)            ## varianza para zd  
)

## Lectura de covariables 

table(is.na(base_FH$ModerateSevere))
table(is.na(base_FH$var.tot), is.na(base_FH$var.tot.smooth.corr) )

## Estimación directa 
data_dir <- base_FH %>% filter(!is.na(ModerateSevere))

data_syn <-
  base_FH %>% anti_join(data_dir %>% select(all_of(dam2)))

# names(data_syn)
names_cov <- data_dir %>% select(accesibilidad_hospitales:region) %>% 
  names()

formula_mod <- formula(paste("~",paste(names_cov),
                 collapse = " + "))

## Dominios observados
Xdat <- model.matrix(formula_mod, data = data_dir)

## Dominios no observados
Xs <- model.matrix(formula_mod, data = data_syn)

sample_data <- list(
  N1 = nrow(Xdat),       # Observados.
  N2 = nrow(Xs),         # NO Observados.
  p  = ncol(Xdat),       # Número de regresores.
  X  = as.matrix(Xdat),  # Covariables Observados.
  Xs = as.matrix(Xs),    # Covariables NO Observados
  # Hay que hacer la transformación antes de compilar el código
  y  = as.numeric(data_dir$T_ModerateSevere),  
  sigma_e = sqrt(data_dir$varhat)
)

fit_FH_arcoseno  <- "Data/modelosStan/15FH_arcsin_normal.stan"

options(mc.cores = parallel::detectCores())
rstan::rstan_options(auto_write = TRUE) 

model_FH_arcoseno  <- stan(
  file = fit_FH_arcoseno,  
  data = sample_data,   
  verbose = FALSE,
  warmup = 1000,         
  iter = 2000,            
  cores = 4              
)

saveRDS(object = model_FH_arcoseno,
        file = "Data/model_FH_arcoseno.rds")



paramtros <- summary(model_FH_arcoseno)$summary %>% data.frame()

mcmc_rhat(paramtros$Rhat)

paramtros %>% filter(Rhat>1.05)

y_pred_B <- as.array(model_FH_arcoseno, pars = "theta") %>% 
  as_draws_matrix()
rowsrandom <- sample(nrow(y_pred_B), 100)
y_pred2 <- y_pred_B[rowsrandom, ]
p_temp <- ppc_dens_overlay(y = as.numeric(data_dir$ModerateSevere), y_pred2)

ggsave(plot = p_temp,
       filename =  "Data/RecursosBook/02/2_ppc_arcosin.jpeg", 
       scale = 3)

posterior_sigma2_u <- as.array(model_FH_arcoseno, pars = "sigma2_u")
(mcmc_dens_chains(posterior_sigma2_u) +
    mcmc_areas(posterior_sigma2_u) ) / 
  mcmc_trace(posterior_sigma2_u)

theta_FH <-   summary(model_FH_arcoseno,pars =  "theta")$summary %>%
  data.frame()
data_dir %<>% mutate(pred_arcoseno = theta_FH$mean, 
                     pred_arcoseno_EE = theta_FH$sd,
                     Cv_pred = pred_arcoseno_EE/pred_arcoseno)

arcsin_freq <-
  read_xlsx("Data/SAE-FIES Chile/Outputs/hh/Indirect estimates/fh_arcsin.xlsx",
            sheet = 2) %>%
  transmute(Domain,
            comuna = str_pad(Domain, width = 5, pad = "0"), Direct, FH)

temp <-
  data_dir %>% 
  select(comuna, ModerateSevere, pred_arcoseno) %>%
  inner_join(arcsin_freq, by = dam2)

p_temp2 <- p_temp + 
  geom_density(data = temp, aes(x = FH),
               colour = "red" , linewidth = 1.5)

ggsave(plot = p_temp2,
       filename =  "Data/RecursosBook/02/2_ppc_arcosin.jpeg", 
       scale = 3)


p11 <- ggplot(temp, aes(x = ModerateSevere, y = Direct)) +
  geom_point() + 
  geom_abline(slope = 1,intercept = 0, colour = "red") +
  theme_bw(10) 

# Estimación con la ecuación ponderada de FH Vs estimación sintética
p12 <- ggplot(temp, aes(x = pred_arcoseno, y = FH)) +
  geom_point() + 
  geom_abline(slope = 1,intercept = 0, colour = "red") +
  theme_bw(10) 


