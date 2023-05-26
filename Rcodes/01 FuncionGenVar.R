################################################################################
## Title:        Tranformación FGV para la varianza directa                   ##
## Authors:      Stalyn Guerrero- Andrés Gutiérrez                            ##
## Date:         05-2023                                                      ##
## Realiza la transformación FGV sobre las comunas que fueron seleccionadas   ##
################################################################################
rm(list = ls())
library(ggplot2)
library(dplyr)
library(patchwork)
library(thematic)
theme_set(theme_bw())
thematic_on(
  bg = "white", fg = "black", accent = "red",
  font = font_spec("Oxanium", scale = 1.25)
)
select <- dplyr::select

id_dominio <- "comuna"

## Lectura de indicadores estimados en el paso anterior. 
indicador_dom <- readRDS('Data/dir_estim_comuna_CASEN_2020_hh2.rds')

# Filtrar valores para obtener solo aquellos que puedan 

baseFGV <- indicador_dom %>% filter(calidad2 == 1) %>%
  transmute(
    comuna,
    FIES = ModerateSevere,
    n = n,
    var_FIES = var.tot,
    ln_sigma2 = log(var.tot)
  )

############Plots de la data#########
p1 <- ggplot(baseFGV, aes(x = FIES, y = ln_sigma2)) +
  geom_point() +
  geom_smooth(method = "loess") +
  xlab("Escala de Experiencia de Inseguridad Alimentaria")

p2 <- ggplot(baseFGV, aes(x = n, y = ln_sigma2)) + 
  geom_point() +
  geom_smooth(method = "loess") + 
  xlab("Tamaño de muestra")

p3 <- ggplot(baseFGV, 
             aes(x = FIES * n, y = ln_sigma2)) + 
  geom_point() +
  geom_smooth(method = "loess") + 
  xlab("Número de personas en\n Escala de Experiencia de Inseguridad Alimentaria")

p4 <- ggplot(baseFGV, 
             aes(x = sqrt(FIES), y = ln_sigma2)) + 
  geom_point() +
  geom_smooth(method = "loess") + 
  xlab("Raiz cuadrada de\nEscala de Experiencia de Inseguridad Alimentaria")


p_temp <- (p1 | p2) / (p3 | p4)
ggsave(plot = p_temp,
       filename =  "Data/RecursosBook/01 FGV/1_lnsigma2.jpeg", 
       scale = 3)
rm('p1','p2','p3','p4', 'p_temp')

#-----------------Función de Varianza-----------------
FGV1 <- lm(ln_sigma2 ~ n + 
             I(n^2) + I(sqrt(FIES)),
           data = baseFGV)

summary(FGV1)
##Resultados del summary
# Call:
#   lm(formula = ln_sigma2 ~ n + I(n^2) + I(sqrt(FIES)), data = baseFGV)
# Residual standard error: 0.6956 on 249 degrees of freedom
# Multiple R-squared:  0.4713,	Adjusted R-squared:  0.465 
# F-statistic:    74 on 3 and 249 DF,  p-value: < 2.2e-16

## Determinar el valor de la constante delta. 

delta.hat = sum(baseFGV$var_FIES) / sum(exp(fitted.values(FGV1)))
delta.hat

saveRDS(object = list(FGV1 = FGV1, delta.hat = delta.hat),
        "Data/RecursosBook/01 FGV/2_modelo.rds")

baseFGV <-
  baseFGV %>% mutate(hat_var = delta.hat * exp(fitted.values(FGV1)))

# =============Plot de las estimaciones=================
X11()
par(mfrow = c(2, 2))

#Plot1
plot(FGV1)
#Plot2
ggplot(baseFGV, 
       aes(x = var_FIES, y = hat_var)) + 
  geom_point() +
  geom_smooth(method = "loess")


#------------Unir las estimaciones con la data original-------------
base_sae <- left_join(indicador_dom,
                      baseFGV %>% select(all_of(id_dominio), hat_var), 
                      by = id_dominio) %>%
  mutate(
    var.tot = ifelse(is.na(hat_var), NA_real_, var.tot),
    deff = ifelse(is.na(hat_var), NA_real_, deff)
  )

######################################################

#----------Transformación de la data para consumo final-------------------
##
base_FH <- base_sae %>%
  mutate(
    deff = ifelse(is.nan(deff), 1, deff),
    deff_FGV = ifelse(var.tot == 0 ,
      1,
      hat_var / (var.tot / deff) #Fórmula del nuevo DEFF
    ),
    # Criterio MDS para regularizar el DeffFGV
    deff_FGV = ifelse(deff_FGV <= 1  & calidad2 == 1 , NA_real_, deff_FGV), #Deff estimado
    n_eff_FGV = n / deff_FGV, #Número efectivo de personas encuestadas
    hat_var = ifelse(deff_FGV <= 1 &  calidad2 == 1, NA_real_, hat_var), #Si no se estimó varianza para ese municipio, también excluir la estimación directa de este municipio, esto es relevante para el modelo FH 
    ModerateSevere = ifelse(is.na(hat_var), NA_real_, ModerateSevere) 
  )

table(base_FH$calidad2, is.na(base_FH$deff) )
table(base_FH$calidad2, is.na(base_FH$deff_FGV) )
table(base_FH$calidad2, is.na(base_FH$hat_var) )

saveRDS(object = base_FH, "Data/base_FH.Rds")


##### Análisis gráfico#####


ggplot(base_FH %>% filter(!is.na(hat_var)) %>% 
         arrange(n), aes(x = hat_var, y = var.tot)) + 
  geom_point() + 
  geom_smooth(method = "lm", col = 2) + 
  labs(x = "FGV", y = "VaRdirEst") +
  ylab("Varianza del Estimador Directo")

#########################3#########################
nDom <- sum(!is.na(base_FH$hat_var))
temp_FH <- base_FH %>% filter(!is.na(hat_var))
p_temp <- ggplot(temp_FH %>% 
         arrange(n), aes(x = 1:nDom)) +
  geom_line(aes(y = var.tot, color = "VarDirEst")) +
  geom_line(aes(y = hat_var, color = "FGV")) +
  labs(y = "Varianzas", x = "Tamaño muestral", color = " ") +
  scale_x_continuous(breaks = seq(1, nDom, by = 10),
                     labels = temp_FH$n[order(temp_FH$n)][seq(1, nDom, by = 10)]) +
  scale_color_manual(values = c("FGV" = "Blue", "VarDirEst" = "Red"))

ggsave(plot = p_temp,
       filename =  "Data/RecursosBook/01 FGV/3_FGV_vs_n.jpeg", 
       scale = 3)

###     Comparación del tamaño de muestra efectivo respecto al tamaño de     ###
###                             muestra                                      ###

p_temp <- ggplot(temp_FH %>% 
         arrange(n), aes(x = 1:nDom)) +
  geom_line(aes(y =  n / deff, color = "n_eff_DIR")) +
  geom_line(aes(y = n_eff_FGV, color = "n_eff_FGV")) +
  labs(y = "Tamaño de muestra efectivo", 
       x = "Tamaño muestral", color = " ") +
  scale_x_continuous(breaks = seq(1, nDom, by = 10),
                     labels = temp_FH$n[order(temp_FH$n)][seq(1, nDom, by = 10)]) +
  scale_color_manual(values = c("n_eff_FGV" = "Blue", "n_eff_DIR" = "red"))

ggsave(plot = p_temp,
       filename =  "Data/RecursosBook/01 FGV/3_neff_vs_n.jpeg", 
       scale = 3)
