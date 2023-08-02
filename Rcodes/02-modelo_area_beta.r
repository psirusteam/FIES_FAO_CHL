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

## Lectura de base de datos
dam2 <- "comuna"

base_FH_antes <- readRDS("Data/data_v1/data_SAE_complete_final.rds") %>% 
  select(comuna:var.tot.smooth.corr) %>% 
  mutate(comuna = str_pad(haven::as_factor( comuna, levels = "values"), 
                          width = 5, pad = "0"))

base_cov_new <- readRDS("Data/data_SAE_complete_final_arcsin.rds") %>% 
  select(comuna,accesibilidad_hospitales:region) %>% 
  mutate(comuna = str_pad(comuna, width = 5, pad = "0"))

base_FH <- inner_join(base_FH_antes, base_cov_new)

dir_estim <- readRDS("Data/data_v1/dir_estim_comuna_CASEN_2020_hh.rds") %>% 
  transmute(
    comuna = str_pad(comuna, width = 5, pad = "0"), n, deff)

base_FH <- inner_join(
  base_FH, dir_estim, by = dam2
) %>% mutate(
  deff_FGV = var.tot.smooth.corr / (var.tot / deff),
  n_eff_FGV = n / deff_FGV, #Número efectivo de personas encuestadas
  n_effec = n_eff_FGV,              ## n efectivo
)
## Lectura de covariables 

table(is.na(base_FH$ModerateSevere))
table(is.na(base_FH$var.tot), is.na(base_FH$var.tot.smooth.corr) )

## Estimación directa 
data_dir <- base_FH %>% filter(!is.na(ModerateSevere))

data_syn <-
  base_FH %>% anti_join(data_dir %>% select(all_of(dam2)))

# names(data_syn)
colnames(base_cov_new)## Tomado del script main_V1.R

formula_mod <- formula(paste("~",paste(colnames(base_cov_new)[-1]),
                             collapse = " + "))
## Dominios observados
Xdat <- model.matrix(formula_mod, data = data_dir)

## Dominios no observados
Xs <- model.matrix(formula_mod, data = data_syn)

sample_data <- list(
  N1 = nrow(Xdat),   # Observados.
  N2 = nrow(Xs),   # NO Observados.
  p  = ncol(Xdat),       # Número de regresores.
  X  = as.matrix(Xdat),  # Covariables Observados.
  Xs = as.matrix(Xs),    # Covariables NO Observados
  y  = as.numeric(data_dir$ModerateSevere),
  phi = data_dir$n_eff_FGV - 1 
)

fit_FH_beta_logitic   <- "Data/modelosStan/16FH_beta_logitc.stan"

options(mc.cores = parallel::detectCores())
rstan::rstan_options(auto_write = TRUE) 

model_FH_beta_logitic  <- stan(
  file = fit_FH_beta_logitic ,  
  data = sample_data,   
  verbose = FALSE,
  warmup = 3000,         
  iter = 4000,            
  cores = 4              
)

saveRDS(object = model_FH_beta_logitic,
        file = "Data/model_FH_beta_logitic.rds")

paramtros <- summary(model_FH_beta_logitic)$summary %>% data.frame()

mcmc_rhat(paramtros$Rhat)

y_pred_B <- as.array(model_FH_beta_logitic, pars = "theta") %>% 
  as_draws_matrix()
rowsrandom <- sample(nrow(y_pred_B), 100)
y_pred2 <- y_pred_B[rowsrandom, ]
p_temp <- ppc_dens_overlay(y = as.numeric(data_dir$ModerateSevere), y_pred2)

ggsave(plot = p_temp,
       filename =  "Data/RecursosBook/02/3_ppc_beta.jpeg", 
       scale = 3)

posterior_sigma2_u <- as.array(model_FH_beta_logitic, pars = "sigma2_u")
(mcmc_dens_chains(posterior_sigma2_u) +
    mcmc_areas(posterior_sigma2_u) ) / 
  mcmc_trace(posterior_sigma2_u)

theta_FH <-   summary(model_FH_beta_logitic,pars =  "theta")$summary %>%
  data.frame()
data_dir %<>% mutate(pred_beta_log = theta_FH$mean, 
                     pred_beta_log_EE = theta_FH$sd,
                     Cv_pred = pred_beta_log_EE/pred_beta_log)

arcsin_freq <-
  readxl::read_xlsx("Data/SAE-FIES Chile/Outputs/hh/Indirect estimates/fh_arcsin.xlsx",
            sheet = 2) %>%
  transmute(Domain,
            comuna = str_pad(Domain, width = 5, pad = "0"), Direct, FH)

temp <-
  data_dir %>% 
  select(comuna, ModerateSevere, pred_beta_log) %>%
  inner_join(arcsin_freq, by = dam2)

p_temp2 <- p_temp + 
  geom_density(data = temp, aes(x = FH),
               colour = "red" , size = 1.5)

ggsave(plot = p_temp2,
       filename =  "Data/RecursosBook/02/3_ppc_beta.jpeg", 
       scale = 3)


p11 <- ggplot(temp, aes(x = ModerateSevere, y = Direct)) +
  geom_point() + 
  geom_abline(slope = 1,intercept = 0, colour = "red") +
  theme_bw(10) 

# Estimación con la ecuación ponderada de FH Vs estimación sintética
p12 <- ggplot(temp, aes(x = pred_beta_log, y = FH)) +
  geom_point() + 
  geom_abline(slope = 1,intercept = 0, colour = "red") +
  theme_bw(10) 




