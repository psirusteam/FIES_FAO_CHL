################################################################################
## Title:        Modelo de área (\theta ~ normal)                             ##
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

formula_mod <- formula(paste("~", paste0(names_cov,
                                         collapse = " + ")))

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
  y  = as.numeric(data_dir$ModerateSevere), # Estimación directa
  sigma_e = sqrt(data_dir$var.tot.smooth.corr)   # Error de estimación
)


fit_FH_normal <- "Data/modelosStan/17FH_normal.stan"

options(mc.cores = parallel::detectCores())
rstan::rstan_options(auto_write = TRUE) 

model_FH_normal <- stan(
  file = fit_FH_normal,  
  data = sample_data,   
  verbose = FALSE,
  warmup = 2000,         
  iter = 3000,            
  cores = 4              
)

saveRDS(object = model_FH_normal,
        file = "Data/model_FH_normal.rds")

model_FH_normal <- readRDS(file = "Data/model_FH_normal.rds")

paramtros <- summary(model_FH_normal)$summary %>% data.frame()

mcmc_rhat(paramtros$Rhat)
paramtros %>% filter(Rhat>1.05)

y_pred_B <- as.array(model_FH_normal, pars = "theta") %>% 
  as_draws_matrix()
rowsrandom <- sample(nrow(y_pred_B), 100)
y_pred2 <- y_pred_B[rowsrandom, ]
p_temp <- ppc_dens_overlay(y = as.numeric(data_dir$ModerateSevere), y_pred2)

ggsave(plot = p_temp,
       filename =  "Data/RecursosBook/02/1_ppc_normal.jpeg", 
       scale = 3)

posterior_sigma2_u <- as.array(model_FH_normal, pars = "sigma2_u")
(mcmc_dens_chains(posterior_sigma2_u) +
    mcmc_areas(posterior_sigma2_u) ) / 
  mcmc_trace(posterior_sigma2_u)


theta <-   summary(model_FH_normal,pars =  "theta")$summary %>%
  data.frame()
thetaSyn <-   summary(model_FH_normal,pars =  "thetaSyn")$summary %>%
  data.frame()
theta_FH <-   summary(model_FH_normal,pars =  "thetaFH")$summary %>%
  data.frame()

data_dir %<>% mutate(
  thetadir = ModerateSevere,
  theta_pred = theta$mean,
  thetaSyn = thetaSyn$mean,
  thetaFH = theta_FH$mean,
  theta_pred_EE = theta$sd,
  Cv_theta_pred = theta_pred_EE/theta_pred
) 
# Estimación predicción del modelo vs ecuación ponderada de FH 
p11 <- ggplot(data_dir, aes(x = theta_pred, y = thetaFH)) +
  geom_point() + 
  geom_abline(slope = 1,intercept = 0, colour = "red") +
  theme_bw(10) 

# Estimación con la ecuación ponderada de FH Vs estimación sintética
p12 <- ggplot(data_dir, aes(x = thetaSyn, y = thetaFH)) +
  geom_point() + 
  geom_abline(slope = 1,intercept = 0, colour = "red") +
  theme_bw(10) 

# Estimación con la ecuación ponderada de FH Vs estimación directa

p21 <- ggplot(data_dir, aes(x = thetadir, y = thetaFH)) +
  geom_point() + 
  geom_abline(slope = 1,intercept = 0, colour = "red") +
  theme_bw(10) 

# Estimación directa Vs estimación sintética

p22 <- ggplot(data_dir, aes(x = thetadir, y = thetaSyn)) +
  geom_point() + 
  geom_abline(slope = 1,intercept = 0, colour = "red") +
  theme_bw(10) 

(p11+p12)/(p21+p22)


arcsin_freq <-
  read_xlsx("Data/SAE-FIES Chile/Outputs/hh/Indirect estimates/fh_arcsin.xlsx",
            sheet = 2) %>%
  transmute(Domain,
            comuna = str_pad(Domain, width = 5, pad = "0"), Direct, FH)

temp <-
  data_dir %>%
  select(comuna, ModerateSevere, theta_pred) %>%
  inner_join(arcsin_freq, by = dam2)

p_temp2 <- p_temp + 
  geom_density(data = temp, aes(x = FH),
               colour = "red" , size = 1.5)

ggsave(plot = p_temp2,
       filename =  "Data/RecursosBook/02/1_ppc_normal.jpeg", 
       scale = 3)


p11 <- ggplot(temp, aes(x = ModerateSevere, y = Direct)) +
  geom_point() + 
  geom_abline(slope = 1,intercept = 0, colour = "red") +
  theme_bw(10) 

# Estimación con la ecuación ponderada de FH Vs estimación sintética
p12 <- ggplot(temp, aes(x = theta_pred, y = FH)) +
  geom_point() + 
  geom_abline(slope = 1,intercept = 0, colour = "red") +
  theme_bw(10) 



