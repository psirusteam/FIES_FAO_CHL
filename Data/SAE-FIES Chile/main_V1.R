##          Master script for direct estimation with CASEN 2020               ##

# 0. Setup ---------------------------------------------------------------------
## Set working directory -------------------------------------------------------
setwd("C:/Users/Dicandia/OneDrive - Food and Agriculture Organization/Desktop/2023/Latin America/A. Final/SAE-FIES Chile")

## Packages --------------------------------------------------------------------
library(haven)
library(dplyr)
library(nloptr)
library(readxl)
library(glmnet)
library(SAEval)
library(ggplot2)
library(sf)
library(openxlsx)

## Source scripts/functions ----------------------------------------------------
source("Scripts/source.R")

## Load Data -------------------------------------------------------------------
DATA = read_sav("Inputs/Casen en Pandemia 2020 SPSS.sav")

# A. HOUSEHOLD LEVEL analysis --------------------------------------------------
## 1. Food insecurity probabilities (no weights) -------------------------------
DATA$wt <- DATA$expc
FS_hh_CASEN2020_Whole <- fies_processing_CASEN2020_2(DATA,weights=F,HH=T,save=F) # without weights

weighted.mean(FS_hh_CASEN2020_Whole$prob.mod, 
              FS_hh_CASEN2020_Whole$wt,na.rm=T)*100

## 2. Trimming of weights (Potter Method) --------------------------------------
FS_hh_CASEN2020_Whole$wkx <- NA
FS_hh_CASEN2020_Whole$region_zona <- paste0(FS_hh_CASEN2020_Whole$region,
                                            FS_hh_CASEN2020_Whole$zona)

# ciclo para ajustar el método de potter a tozdas las región-zona
ValorC <- 5 # the square of any weight is less than 5 times the average of the squared weights
# Smaller or larger values of c will generate different trimming levels
for (j in unique(FS_hh_CASEN2020_Whole$region_zona)) {
  casenflt <- FS_hh_CASEN2020_Whole[FS_hh_CASEN2020_Whole$region_zona==j,]
  pos <- which(FS_hh_CASEN2020_Whole$region_zona == j)
  res <- nloptr(#x0 = ValorC,
    x0 = optimize_c_potter(casenflt, lower = 1, upper = 40, which_prob = "ModSev"), 
    eval_f = optimizador2, 
    eval_grad_f = NULL, 
    opts = list("algorithm" = "NLOPT_LN_BOBYQA", "xtol_rel" = 1.0e-4,
                "maxeval" = 500), 
    lb = 1, ub = 800, 
    encuesta = casenflt,
    which_prob= c("ModSev"))
  
  FS_hh_CASEN2020_Whole$wkx[pos] <- potter(weights = casenflt$wt, ValorC = res$solution)
  print(paste("region/zona: ", j, " - Potter óptimo: ", res$solution))
  print(summary(FS_hh_CASEN2020_Whole$wt[pos]))
  print(summary(FS_hh_CASEN2020_Whole$wkx[pos]))
}

# Renaming the columns of the trimmed weights and original weights
colnames(FS_hh_CASEN2020_Whole)[colnames(FS_hh_CASEN2020_Whole) %in% c("wt","wkx")] <- c("wt_old","wt")

summary(FS_hh_CASEN2020_Whole$wt) ### Pesos con recorte de Potter
summary(FS_hh_CASEN2020_Whole$wt_old)  ### Pesos Originales

FS_hh_CASEN2020_Whole %>% ggplot() + geom_point(aes(x = wt, y = wt_old)) +
  xlab("Trimmed weights") + ylab("Original weights") +
  geom_abline(intercept = 0, slope = 1, col = 2) +
  scale_x_continuous(breaks=seq(0,3000,500)) + scale_y_continuous(breaks=seq(0,16000,1000))

## 3. Food insecurity probabilities (with trimmed weights) ---------------------
dim(DATA)
dim(FS_hh_CASEN2020_Whole)
DATA_trim <- merge(DATA[,colnames(DATA)!="wt"],FS_hh_CASEN2020_Whole[,c("folio","wt")],all.x=T)
dim(DATA_trim)

FS_hh_CASEN2020_Whole_wt <- fies_processing_CASEN2020_2(DATA_trim,weights=T,HH=T,save=F)

weighted.mean(FS_hh_CASEN2020_Whole_wt$prob.mod, 
              FS_hh_CASEN2020_Whole_wt$wt,na.rm=T)*100

FS_hh_CASEN2020_Whole_wt <- merge(FS_hh_CASEN2020_Whole_wt,FS_hh_CASEN2020_Whole[,c("folio","wt_old")])

## 4. Direct estimates ---------------------------------------------------------

### National level -------------------------------------------------------------
direct_estimates_national_CASEN2020 <- DE_CASEN2020(data=FS_hh_CASEN2020_Whole_wt, 
                                                    disaggregation=c("national"),
                                                    HH=T,save=F)
### Region level -------------------------------------------------------------
direct_estimates_region_CASEN2020 <- DE_CASEN2020(data=FS_hh_CASEN2020_Whole_wt, 
                                                  disaggregation=c("region"),
                                                  HH=T,save=F)
direct_estimates_ModSev_region_CASEN2020 <- direct_estimates_region_CASEN2020[[1]]
### Comuna level ---------------------------------------------------------------
direct_estimates_comuna_CASEN2020 <- DE_CASEN2020(data=FS_hh_CASEN2020_Whole_wt,
                                                  disaggregation=c("comuna"),
                                                  HH=T,save=F)
direct_estimates_ModSev_comuna_CASEN2020 <- direct_estimates_comuna_CASEN2020[[1]]
#direct_estimates_Sev_comuna_CASEN2020 <- direct_estimates_comuna_CASEN2020[[2]]

## 5. Exclusion/Inclusion of comunas -------------------------------------------
table(direct_estimates_ModSev_comuna_CASEN2020$calidad2)
direct_estimates_ModSev_comuna_CASEN2020_filt <- 
  direct_estimates_ModSev_comuna_CASEN2020[direct_estimates_ModSev_comuna_CASEN2020$calidad2==1,]

## 6. Variable selection -------------------------------------------------------

### Load auxiliary variables ---------------------------------------------------
# Geospatial
geo_var <- read_excel("Inputs/satelitales_media.xlsx", col_types =rep("numeric",7))

# Other 
admin_var <-readRDS("Inputs/Auxiliary Data.rds")
dim(admin_var) 

# First filtering of admin_var based on discussions with the Ministry
filter_admin_var <- filter_var(aux_data= admin_var,which=1)
dim(filter_admin_var)
summary(filter_admin_var)

filter_admin_var$promedio_afc_2020 <- log(filter_admin_var$promedio_afc_2020)
filter_admin_var$promedio_pet_afc_2020 <- log(filter_admin_var$promedio_pet_afc_2020)
filter_admin_var$mediana_afc_2020 <- log(filter_admin_var$mediana_afc_2020)
filter_admin_var$tasa_victimizacion_2019 <- log(filter_admin_var$tasa_victimizacion_2019)

filter_admin_var$tasa_anios_perdidos_2014 <- log(filter_admin_var$tasa_anios_perdidos_2014)
filter_admin_var$promedio_simce_leng_8b_2019 <- log(filter_admin_var$promedio_simce_leng_8b_2019)
filter_admin_var$promedio_simce_mat_8b_2019 <- log(filter_admin_var$promedio_simce_mat_8b_2019)
filter_admin_var$promedio_simce_hist_8b_2019 <- log(filter_admin_var$promedio_simce_hist_8b_2019)
filter_admin_var$indice_de_vejez_2020 <- log(filter_admin_var$indice_de_vejez_2020)
filter_admin_var$tasa_dependencia_familiar_2020 <- log(filter_admin_var$tasa_dependencia_familiar_2020)

# Merge with direct estimates
data_SAE_complete <- direct_estimates_ModSev_comuna_CASEN2020_filt %>%
  dplyr::select(comuna, ModerateSevere) %>%
  right_join(filter_admin_var, by="comuna") %>%
  left_join(geo_var[,c("mpio","luces_nocturnas","accesibilidad_hospitales")], by= c("comuna"="mpio")) #%>%
#select(-comuna)

# Second filtering of admin_var due to collinearity and/or high correlation
data_SAE_complete_filt <- filter_var2(data_SAE_complete)[[1]]
dim(data_SAE_complete_filt)

### Stepwise selection ---------------------------------------------------------
data_SAE_complete_mod <- data_SAE_complete_filt[,-which(colnames(data_SAE_complete_filt)=="comuna")]

stepwise_sel_outputA <- stepwise_sel(data=data_SAE_complete_mod,transformation="no",criteria="AIC")
summary(stepwise_sel_outputA$model)
car::vif(stepwise_sel_outputA$model)
stepwise_sel_outputA$selected_variables

stepwise_sel_outputA_log <- stepwise_sel(data=data_SAE_complete_mod,transformation="log",criteria="AIC")
summary(stepwise_sel_outputA_log$model)
car::vif(stepwise_sel_outputA_log$model)
stepwise_sel_outputA_log$selected_variables

stepwise_sel_outputA_arcsin <- stepwise_sel(data=data_SAE_complete_mod,transformation="arcsin",criteria="AIC")
summary(stepwise_sel_outputA_arcsin$model)
car::vif(stepwise_sel_outputA_arcsin$model)
stepwise_sel_outputA_arcsin$selected_variables

## 7. Variance Smoothing -------------------------------------------------------
var_smooth_output <- gvf(direct_estimates_ModSev_comuna_CASEN2020_filt,"model1")
summary(var_smooth_output$model)
plot(var_smooth_output$model)
var_smooth_output$plot_prelim
var_smooth_output$plot_gvf_compare
summary(var_smooth_output$data$var.tot.smooth.corr)
summary(var_smooth_output$data$var.tot)

## 8. Area level model ---------------------------------------------------------
library(emdi)
### Combined data --------------------------------------------------------------
data_SAE_complete_final <- var_smooth_output$data %>%
  dplyr::select(comuna,ModerateSevere,var.tot,var.tot.smooth.corr) %>%
  right_join(
    data_SAE_complete[,unique(c("comuna","accesibilidad_hospitales","luces_nocturnas",
                         colnames(stepwise_sel_outputA$model$model)[-1]))] , 
    #names(stepwise_sel_outputA$selected_variables))] , 
    by="comuna") %>%
  mutate(region=factor(case_when(
    nchar(comuna)==4 ~ substring(comuna,first = 1,last = 1),
    nchar(comuna)==5 ~ substring(comuna,first = 1,last = 2),
  )))
data_SAE_complete_final <- as.data.frame(data_SAE_complete_final)
data_SAE_complete_final$accesibilidad_hospitales <- log(data_SAE_complete_final$accesibilidad_hospitales)

data_SAE_complete_final_log <- var_smooth_output$data %>%
  dplyr::select(comuna,ModerateSevere,var.tot,var.tot.smooth.corr) %>%
  right_join(
    data_SAE_complete[,unique(c("comuna","accesibilidad_hospitales","luces_nocturnas",
                         colnames(stepwise_sel_outputA_log$model$model)[-1]))] , 
    #names(stepwise_sel_outputA_log$selected_variables))] , 
    by="comuna") %>%
  mutate(region=factor(case_when(
    nchar(comuna)==4 ~ substring(comuna,first = 1,last = 1),
    nchar(comuna)==5 ~ substring(comuna,first = 1,last = 2),
  ))) 
data_SAE_complete_final_log <- as.data.frame(data_SAE_complete_final_log)
data_SAE_complete_final_log$accesibilidad_hospitales <- log(data_SAE_complete_final_log$accesibilidad_hospitales)

data_SAE_complete_final_arcsin <- var_smooth_output$data %>%
  mutate(deff_fgv = var.tot.smooth.corr/(var.tot/deff),
         deff_fgv = ifelse(deff_fgv < 1, 1, deff_fgv), # Comentario CEPAL: Criterio MDS para regularizar el DeffFGV
         n_eff_fgv = n/deff_fgv,
         n_eff = n/deff) %>%
  dplyr::select(comuna,ModerateSevere,var.tot,var.tot.smooth.corr,n_eff_fgv,n_eff) %>%
  right_join(
    data_SAE_complete[,unique(c("comuna","accesibilidad_hospitales","luces_nocturnas",
                         colnames(stepwise_sel_outputA_arcsin$model$model)[-1]))] , 
    #names(stepwise_sel_outputA_log$selected_variables))] , 
    by="comuna") %>%
  mutate(region=factor(case_when(
    nchar(comuna)==4 ~ substring(comuna,first = 1,last = 1),
    nchar(comuna)==5 ~ substring(comuna,first = 1,last = 2)))
  )
data_SAE_complete_final_arcsin <- as.data.frame(data_SAE_complete_final_arcsin)
data_SAE_complete_final_arcsin$accesibilidad_hospitales <- log(data_SAE_complete_final_arcsin$accesibilidad_hospitales)

### Fay-Herriot model (NO TRANSFORM + SMOOTHED VAR) ----------------------------
my_formula1 <- as.formula(                      
  paste("ModerateSevere ~ ", 
        paste(colnames(data_SAE_complete_final[5:dim(data_SAE_complete_final)[2]]), 
              collapse = " + ")))
my_formula1 
fh <- fh(fixed = my_formula1,              
         vardir = "var.tot.smooth.corr", domains = "comuna", method = "reml",
         combined_data = data_SAE_complete_final, 
         MSE = TRUE)
summary(fh) 

#### Get values, MSE and CV of the estimators ----------------------------------
fh_d <- estimators(fh, MSE=T, CV=T)
head(fh_d$ind)

#### Summary of CVs of direct and indirect estimates  --------------------------
summary(fh_d$ind$Direct_CV)
summary(fh_d$ind$FH_CV)

#### Diagnostic plots, tables, tests -------------------------------------------

# Q-Q plots and density plots of residuals and random effects 
plot(fh)

# Plots comparing point and existing MSE/CV estimates of direct and model-based estimation
compare_plot(fh, CV=T)

# Linear correlation between direct and indirect estimates
cor(fh$ind$Direct,fh$ind$FH,use ="complete.obs") # 0.7143096

# Table of CVs of direct and indirect estimates
SAEval_cvtable <- cv_table(data = fh_d$ind[,c("Direct_CV","FH_CV")],
                           cv = ~ Direct_CV+FH_CV)
SAEval_cvtable

#write.excel(fh, file = "excel_fh.xlsx", MSE = TRUE, CV = TRUE)

# Goodness of fit diagnostic 
SAEval.gof <-gof(data=fh_d$ind,
                 dir=~Direct,
                 sae=~FH,
                 v.dir=~Direct_MSE,
                 mse.sae=~FH_MSE)
SAEval.gof

### Fay-Herriot model (LOG TRANSFORM + SMOOTHED VAR) ---------------------------
my_formula1_log <- as.formula(                      
  paste("ModerateSevere ~ ", 
        paste(colnames(data_SAE_complete_final_log[5:dim(data_SAE_complete_final_log)[2]]), 
              collapse = " + ")))
my_formula1_log 
fh_log <- fh(my_formula1_log,             
             vardir = "var.tot.smooth.corr", domains = "comuna", method = "reml",
             combined_data = data_SAE_complete_final_log, 
             transformation="log",backtransformation ="bc_crude",
             MSE = TRUE) 
summary(fh_log) 

#### Get values, MSE and CV of the estimators ----------------------------------
fh_d_log <- estimators(fh_log, MSE=T, CV=T)
head(fh_d_log$ind)

#### Summary of CVs of direct and indirect estimates  --------------------------
summary(fh_d_log$ind$Direct_CV)
summary(fh_d_log$ind$FH_CV)

#### Diagnostic plots, tables, tests -------------------------------------------

# Q-Q plots and density plots of residuals and random effects 
plot(fh_log)

# Plots comparing point and existing MSE/CV estimates of direct and model-based estimation
compare_plot(fh_log, CV=T)

# Linear correlation between direct and indirect estimates
cor(fh_log$ind$Direct,fh_log$ind$FH,use ="complete.obs") # 0.6984607

# Table of CVs of direct and indirect estimates
SAEval_cvtable_log <- cv_table(data = fh_d_log$ind[,c("Direct_CV","FH_CV")],
                               cv = ~ Direct_CV+FH_CV)
SAEval_cvtable_log

#write.excel(fh, file = "excel_fh.xlsx", MSE = TRUE, CV = TRUE)

# Goodness of fit diagnostic 
SAEval.gof_log <-gof(data=fh_d_log$ind,
                     dir=~Direct,
                     sae=~FH,
                     v.dir=~Direct_MSE,
                     mse.sae=~FH_MSE)
SAEval.gof_log

### Fay-Herriot model (ARCSIN TRANSFORM + SMOOTHED VAR) ------------------------
my_formula1_arcsin <- as.formula(                      
  paste("ModerateSevere ~ ", 
        paste(colnames(data_SAE_complete_final_arcsin[7:dim(data_SAE_complete_final_arcsin)[2]]), 
              collapse = " + ")))
my_formula1_arcsin 
fh_arcsin <- fh(my_formula1_arcsin,             
                vardir = "var.tot.smooth.corr", domains = "comuna", method = "reml",
                combined_data = data_SAE_complete_final_arcsin, 
                transformation="arcsin", backtransformation = "bc",
                eff_smpsize = "n_eff_fgv", MSE = TRUE, mse_type = "boot",
                B = 500) 
summary(fh_arcsin) 

#### Get values, MSE and CV of the estimators ----------------------------------
fh_d_arcsin <- estimators(fh_arcsin, MSE=T, CV=T)
head(fh_d_arcsin$ind)

#### Summary of CVs of direct and indirect estimates  --------------------------
summary(fh_d_arcsin$ind$Direct_CV)
summary(fh_d_arcsin$ind$FH_CV)

#### Diagnostic plots, tables, tests  ------------------------------------------

# Q-Q plots and density plots of residuals and random effects 
plot(fh_arcsin)

# Plots comparing point and existing MSE/CV estimates of direct and model-based estimation
compare_plot(fh_arcsin, CV=T)

# Linear correlation between direct and indirect estimates
cor(fh_arcsin$ind$Direct,fh_arcsin$ind$FH,use ="complete.obs") # 0.7113556

# Table of CVs of direct and indirect estimates
SAEval_cvtable_arcsin <- cv_table(data = fh_d_arcsin$ind[,c("Direct_CV","FH_CV")],
                                  cv = ~ Direct_CV+FH_CV)
SAEval_cvtable_arcsin

#write.excel(fh, file = "excel_fh.xlsx", MSE = TRUE, CV = TRUE)

# Goodness of fit diagnostic 
SAEval.gof_arcsin <-gof(data=fh_d_arcsin$ind,
                        dir=~Direct,
                        sae=~FH,
                        v.dir=~Direct_MSE,
                        mse.sae=~FH_MSE)
SAEval.gof_arcsin

### Fay-Herriot model (NO TRANSFORM + ORIGINAL VAR) ----------------------------
my_formula1 
fh2 <- fh(fixed = my_formula1,              
          vardir = "var.tot", domains = "comuna", method = "reml",
          combined_data = data_SAE_complete_final, 
          MSE = TRUE)
summary(fh2) 

#### Get values, MSE and CV of the estimators ----------------------------------
fh_d2 <- estimators(fh2, MSE=T, CV=T)
head(fh_d2$ind)

#### Summary of CVs of direct and indirect estimates  --------------------------
summary(fh_d2$ind$Direct_CV)
summary(fh_d2$ind$FH_CV)

#### Diagnostic plots, tables, tests -------------------------------------------

# Q-Q plots and density plots of residuals and random effects 
plot(fh2)

# Plots comparing point and existing MSE/CV estimates of direct and model-based estimation
compare_plot(fh2, CV=T)

# Linear correlation between direct and indirect estimates
cor(fh2$ind$Direct,fh2$ind$FH,use ="complete.obs") # 0.7426435

# Table of CVs of direct and indirect estimates
SAEval_cvtable2 <- cv_table(data = fh_d2$ind[,c("Direct_CV","FH_CV")],
                            cv = ~ Direct_CV+FH_CV)
SAEval_cvtable2

#write.excel(fh, file = "excel_fh.xlsx", MSE = TRUE, CV = TRUE)

# Goodness of fit diagnostic 
SAEval.gof2 <-gof(data=fh_d2$ind,
                  dir=~Direct,
                  sae=~FH,
                  v.dir=~Direct_MSE,
                  mse.sae=~FH_MSE)
SAEval.gof2

### Fay-Herriot model (LOG TRANSFORM + ORIGINAL VAR) ---------------------------
fh_log2 <- fh(my_formula1_log,           
              vardir = "var.tot", domains = "comuna", method = "reml",
              combined_data = data_SAE_complete_final_log, 
              transformation="log",backtransformation ="bc_crude",
              MSE = TRUE)
summary(fh_log2) 

#### Get values, MSE and CV of the estimators ----------------------------------
fh_d_log2 <- estimators(fh_log2, MSE=T, CV=T)
head(fh_d_log2$ind)

#### Summary of CVs of direct and indirect estimates  --------------------------
summary(fh_d_log2$ind$Direct_CV)
summary(fh_d_log2$ind$FH_CV)

#### Diagnostic plots, tables, tests -------------------------------------------

# Q-Q plots and density plots of residuals and random effects 
plot(fh_log2)

# Plots comparing point and existing MSE/CV estimates of direct and model-based estimation
compare_plot(fh_log2, CV=T)

# Linear correlation between direct and indirect estimates
cor(fh_log2$ind$Direct,fh_log2$ind$FH,use ="complete.obs") # 0.7479231

# Table of CVs of direct and indirect estimates
SAEval_cvtable_log2 <- cv_table(data = fh_d_log2$ind[,c("Direct_CV","FH_CV")],
                                cv = ~ Direct_CV+FH_CV)
SAEval_cvtable_log2

#write.excel(fh, file = "excel_fh.xlsx", MSE = TRUE, CV = TRUE)

# Goodness of fit diagnostic 
SAEval.gof_log2 <-gof(data=fh_d_log2$ind,
                      dir=~Direct,
                      sae=~FH,
                      v.dir=~Direct_MSE,
                      mse.sae=~FH_MSE)
SAEval.gof_log2

### Fay-Herriot model (ARCSIN TRANSFORM + ORIGINAL VAR) ------------------------
fh_arcsin2 <- fh(my_formula1_arcsin,             
                 vardir = "var.tot", domains = "comuna", method = "reml",
                 combined_data = data_SAE_complete_final_arcsin, 
                 transformation="arcsin", backtransformation = "bc",
                 eff_smpsize = "n_eff", MSE = TRUE, mse_type = "boot",
                 B = 500) 
summary(fh_arcsin2) 

#### Get values, MSE and CV of the estimators ----------------------------------
fh_d_arcsin2 <- estimators(fh_arcsin2, MSE=T, CV=T)
head(fh_d_arcsin2$ind)

#### Summary of CVs of direct and indirect estimates  --------------------------
summary(fh_d_arcsin2$ind$Direct_CV)
summary(fh_d_arcsin2$ind$FH_CV)

#### Diagnostic plots, tables, tests -------------------------------------------

# Q-Q plots and density plots of residuals and random effects 
plot(fh_arcsin2)

# Plots comparing point and existing MSE/CV estimates of direct and model-based estimation
compare_plot(fh_arcsin2, CV=T)

# Linear correlation between direct and indirect estimates
cor(fh_arcsin2$ind$Direct,fh_arcsin2$ind$FH,use ="complete.obs") # 0.760923

# Table of CVs of direct and indirect estimates
SAEval_cvtable_arcsin2 <- cv_table(data = fh_d_arcsin2$ind[,c("Direct_CV","FH_CV")],
                                   cv = ~ Direct_CV+FH_CV)
SAEval_cvtable_arcsin2

#write.excel(fh, file = "excel_fh.xlsx", MSE = TRUE, CV = TRUE)

# Goodness of fit diagnostic 
SAEval.gof_arcsin2 <-gof(data=fh_d_arcsin2$ind,
                         dir=~Direct,
                         sae=~FH,
                         v.dir=~Direct_MSE,
                         mse.sae=~FH_MSE)
SAEval.gof_arcsin2

## 9. Choosen model ------------------------------------------------------------

summary(fh_log2)
compare(fh_log2)
View(fh_d_log2$ind)

summary(fh_arcsin) # Choosen one
compare(fh_arcsin)
View(fh_d_arcsin$ind)

summary(fh_arcsin2)
compare(fh_arcsin2)
View(fh_d_arcsin2$ind)

write.excel(fh_arcsin,
            file = "Outputs/hh/Indirect estimates/fh_arcsin.xlsx", indicator = "all",
            MSE = TRUE, CV = TRUE
)

## 10. Final model validations plots -------------------------------------------

### Standardised residuals -----------------------------------------------
ggplot(data.frame(Residuals = fh_arcsin$model$std_real_residuals)) +
  geom_point(col="blue", aes(y = Residuals, x = 1:length(Residuals))) +
  labs(y = "Standardised residuals", x = "Comunas") +
  geom_hline(yintercept = 0, col = "red") 

### Standardised predicted values ----------------------------------------
pred.est <- (fh_arcsin$model$fitted)/sd(fh_arcsin$model$std_real_residuals)
res <- data.frame(residuals=fh_arcsin$model$std_real_residuals,pred.est)

ggplot(res, aes(pred.est,residuals)) + theme_light() +
  geom_point(col = "blue") +
  geom_smooth(method = "lm", se = FALSE, aes(color = "regresión")) +
  geom_hline(aes(yintercept = 0, col = "y = 0"), size = 1) +
  labs(y = "Residuos estandarizados", x = "Valores predichos estandarizados") +
  scale_color_manual(name = "rectas", values = c("regresión" = "green", "y = 0" = "red"))

### Cook distance ----------------------------------------------------------------
Betas <-  as.matrix(fh_arcsin$model$coefficients[,1],ncol = 1)
rownames(Betas) <- rownames(fh_arcsin$model$coefficients)
data_SAE_complete_final_arcsin_insample <- data_SAE_complete_final_arcsin %>% filter(!is.na(ModerateSevere))
XS <- cbind(as.matrix(
  fastDummies::dummy_cols(data_SAE_complete_final_arcsin_insample, select_columns = c("region"),
                          remove_selected_columns=T) %>% 
    rename(region1=region_1,region2=region_2,region3=region_3,region4=region_4,
           region5=region_5,region6=region_6,region7=region_7,region8=region_8,
           region9=region_9,region10=region_10,region11=region_11,region12=region_12,
           region13=region_13,region14=region_14,region15=region_15,region16=region_16) %>%
    mutate(`(Intercept)` = 1) %>%
    select(rownames(Betas))))  
su2 <- fh_arcsin$model$variance
residuos <- XS %*% Betas - c(colMeans(XS) %*% Betas)
D <- dim(XS)[1]
q <- dim(XS)[2]
S2Beta <- sum(residuos^2)/(D-1)
su2 <- fh_arcsin$model$variance
(R2 <- 1 - (su2/(((D-q)/(D-1))*su2 + S2Beta)))

D <- dim(data_SAE_complete_final_arcsin_insample)[1]
CD = numeric(D)
V <- diag(1/(su2 + 1/(4 * data_SAE_complete_final_arcsin_insample$n_eff)))

for(i in 1:D){
  print(i)
  BetaModelo <- fh(fixed = my_formula1_arcsin, vardir = "var.tot.smooth.corr", 
                   combined_data = data_SAE_complete_final_arcsin_insample[-c(i), ], 
                   domains = "comuna", method = "reml", transformation = "arcsin", 
                   backtransformation = "bc", eff_smpsize = "n_eff_fgv",
                   MSE = FALSE, mse_type = "NULL")
  
  #--- Coeficientes del modelo ajustado ---#
  
  Betas_i = as.matrix(BetaModelo$model$coefficients[, 1], ncol = 1)
  betaDiff <- Betas - Betas_i
  CD[i] = (1/(q - 1)) * t(betaDiff) %*% (t(XS) %*% V %*% XS) %*% betaDiff
}

data.frame(cookDis = CD, comuna = data_SAE_complete_final_arcsin_insample$comuna) %>%
  ggplot(aes(y = cookDis, x = 1:D)) + 
  geom_point(col = "blue") + ylim(c(0,0.15)) + 
  geom_text(aes(label = ifelse(cookDis > 0.05, as.character(comuna),'')), hjust = 0, vjust = 0) + 
  labs(y = "Distancia de Cook", x = "Comunas")  

## 11. Benchmarking ------------------------------------------------------------

# estimación directa por región
head(direct_estimates_ModSev_region_CASEN2020)

# tamaño poblacional por región
Total_Hogares <- readRDS("Inputs/Total_Hogares.rds") %>% 
  rename(comuna=dam2,pob_comuna=NN_Hogar) %>%
  group_by(region = stringr::str_sub(comuna, 1, 2)) %>%
  mutate(pob_region = sum(pob_comuna),
         comuna=as.numeric(comuna),region=as.numeric(region)) 

# consolidación df: región comuna, estimación región, estimación FH comuna
r_reg <- fh_d_arcsin$ind %>% rename(comuna=Domain) %>%
  left_join(Total_Hogares, by = "comuna") %>%
  left_join(direct_estimates_ModSev_region_CASEN2020 %>% 
              select(region,ModerateSevere) %>% rename(Direct_region=ModerateSevere), 
            by = c("region"))

# pesos benchmark 
r_reg2 <- r_reg %>% group_by(region) %>% summarise(
  r_reg_rb = unique(Direct_region)/sum((pob_comuna/pob_region) * FH),
  r_reg_db = unique(Direct_region) - sum((pob_comuna/pob_region) * FH)) %>% 
  left_join(direct_estimates_ModSev_region_CASEN2020 %>% 
              select(region,ModerateSevere) %>% rename(Direct_region=ModerateSevere), 
            by = c("region"))

pesos <- r_reg %>% mutate(w_i = pob_comuna/pob_region) %>% select(comuna, w_i)

# estimación fh benchmark
estimaciones_bench <- fh_d_arcsin$ind %>% rename(comuna=Domain) %>%
  left_join(r_reg %>% select(region, comuna), by ="comuna") %>% 
  left_join(r_reg2, by = c("region")) %>% mutate(fh_bench = r_reg_rb * FH) %>%
  left_join(pesos, by = "comuna")

# Validación: estimación FH con benchmark:
# 1) comparación entre el valor reportado y el valor estimado FH benchmark a nivel de region
estimaciones_bench %>% group_by(region) %>% 
  summarise(theta_reg_rb = sum(w_i * fh_bench)) %>% 
  left_join(direct_estimates_ModSev_region_CASEN2020 %>% 
              select(region,ModerateSevere) %>% rename(Direct_region=ModerateSevere), 
            by = "region") %>% View()

# 2) comparación entre estimación FH y FH con benchmark por comuna
View(estimaciones_bench %>% transmute(comuna,  directo = Direct * 100, 
                                      fay_herriot = FH * 100, fh_bench = fh_bench * 100))

# 3) comparación gráfica entre estimación fh y fh con benchmark por comuna
ggplot(estimaciones_bench, aes(fh_bench,FH)) + geom_point() +
  geom_smooth(method = "lm") + labs(y = "Estimación Fay-Herriot", 
                                    x = "Estimación Fay-Herriot con Benchmark")

estimaciones_bench_export <- 
  estimaciones_bench %>% 
  select(comuna,colnames(fh_d_arcsin$ind)[-1],fh_bench) %>%
  rename(Domain=comuna,FH_BENCH=fh_bench)

wb <- loadWorkbook("Outputs/hh/Indirect estimates/fh_arcsin.xlsx")
addWorksheet(wb = wb, sheetName = "Estimates + Benchmarking")
writeDataTable(wb, sheet = "Estimates + Benchmarking", estimaciones_bench_export,
               tableStyle = "TablestyleMedium2")
removeWorksheet(wb, "Estimates")
saveWorkbook(wb,"Outputs/hh/Indirect estimates/fh_arcsin.xlsx",overwrite = T)

temp <- estimaciones_bench %>%
  group_by(region) %>% 
  summarise(#thetaSyn = sum(w_i * thetaSyn),
            fh_r = sum(w_i * FH),
            fh_rbench = sum(w_i * fh_bench)
  ) %>%   
  left_join(direct_estimates_ModSev_region_CASEN2020 %>% 
              select(region,ModerateSevere) %>% rename(Direct_region=ModerateSevere),by="region")  %>% 
  mutate(id = 1:n()) %>% 
  tidyr::gather(key = "Metodo",value = "Estimacion",-id, -region) %>%
  inner_join(direct_estimates_ModSev_region_CASEN2020 %>% select(region,CI_L,CI_U) )

ggplot(data = temp, aes(x = id, y = Estimacion, shape = Metodo)) +
  geom_point(aes(color = Metodo), size = 2) +
  geom_line(aes(y = CI_L), linetype  = 2) +
  geom_line(aes(y = CI_U),  linetype  = 2) +
  theme_bw(10) + 
  scale_x_continuous(breaks = temp$id,
                     labels = temp$region) +
  labs(y = "", x = "")

### Map ----
shape <- read_sf("Shapefiles/Comunas/comunas.shp")
head(shape)
dim(shape)

head(estimaciones_bench_export)
dim(estimaciones_bench_export)

fh_estimates_map <- merge(shape,estimaciones_bench_export,
                          by.x="cod_comuna",by.y="Domain")

ggplot() +
  geom_sf(data = fh_estimates_map,aes(fill = Direct)) +
  scale_fill_distiller(palette="Blues",direction=1) 

ggplot() +
  geom_sf(data = fh_estimates_map,aes(fill = FH_BENCH)) +
  scale_fill_distiller(palette="Blues",direction=1) 

# ***************************************** ------------------------------------
