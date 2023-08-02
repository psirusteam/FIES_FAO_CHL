
# * Fies processing --------------------------------------------------------------
fies_processing_CASEN2020_2 = function(data,weights=T,HH=T,save=F) {
  
  # Packages -------------------------------------------------------------------
  list.of.packages <- c("RM.weights", "survey", "openxlsx", "dplyr")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) {
    install.packages(new.packages)
  }
  for (i in list.of.packages) {
    library(i,character.only = TRUE)
  }
  rm(i,list.of.packages,new.packages)
  
  #data = read_sav("Casen en Pandemia 2020 SPSS.sav")
  dim(data)                    ## 185437  650
  length(unique(data$folio))   ## number of unique household id: 62911
  jefeHH=which((data$pco1)==1) ## household heads
  length(jefeHH)               ## number of household head = number of households
  
  # Calculate degrees of freedom for each comuna
  #data = data %>% 
  #  group_by(comuna) %>% 
  #  mutate(n_psu_comuna = as.numeric(n_distinct(cod_upm)),
  #         n_strata_comuna = as.numeric(n_distinct(estrato)),
  #         deg_free = n_psu_comuna - n_strata_comuna)
  
  # Keep only FIES items -------------------------------------------------------
  XX = as.data.frame(subset(data, select = c(r8a, r8b,r8c,r8d,r8e,r8f,r8g,r8h)))
  XX[] = lapply(XX, unclass)
  XX[XX > 2] = NA
  XX[XX == 2] = 0
  if (weights==T) {
    wt = data$wt    ##Sampling weight
    summary(wt)
    sum(wt)           # Population size in Chile
  }else{
    wt = NULL
  }
  
  # Compute raw scores ---------------------------------------------------------
  if (HH) {
    XX=XX[jefeHH,]
    dim(XX)
    wt=wt[jefeHH]  ##select weights corresponding to hh head in order to have a weight per hh
    sum(wt)        ##Number of households in Chile in 2020: 6560146 
  }
  
  rsXX = rowSums(XX) ##the row score corresponds to the number of positive answers
  table(rsXX)
  table(rsXX)/sum(table(rsXX)) ##relative frequency
  
  # Identify and impute missing items ------------------------
  apply(XX,2,function(i) sum (is.na(i)))
  sum(XX[is.na(XX[,5]),1:4])
  sum(XX[is.na(XX[,6]),1:4])
  sum(XX[is.na(XX[,7]),1:4])
  sum(XX[is.na(XX[,8]),1:4])
  
  #  in correspondence of missing values for items 5-8 we have all 0s for the first
  #  4 items. Hence, we set as 0 the missing items.
  items_5_8 = XX[,5:8]
  items_5_8[is.na(items_5_8[,1]),]=0
  XX[,5:8]=items_5_8
  apply(XX,2,function(i) sum (is.na(i)))
  
  # recompute the raw scores without missing values
  rsXX = rowSums(XX)
  table(rsXX)
  table(rsXX)/sum(table(rsXX))
  
  # Use the vector of global standards -----------------------------------------
  load("Inputs/FIES_glob_st.RData") # cargando el vector de Standard Global 
  # para compararlo con los parametros de items de CASEN
  b.tot=fies.global.st
  
  # estableciendo el pseudo puntaje brutos ( en el puntaje bruto 8) requeridos para la estimaci?n
  extr = sum(rowSums(XX)==8, na.rm=T)/sum(!is.na(rowSums(XX)) & !rowSums(XX)==0)
  extr = c(min(7.7, 7.5+extr))
  
  # Implement the Rasch Model --------------------------------------------------
  
  if (HH) {
    rr = RM.w(XX, wt, write.file = save, .d=c(0.5, extr), country="Chile_CASEN2020")
    if (save==T) {
      file.rename(from = "OutputChile_CASEN2020.csv",
                  to=ifelse(weights==T,"Outputs/hh/FIES/OutputChile_CASEN2020_wt.csv","Outputs/hh/FIES/OutputChile_CASEN2020.csv"))
    }
  }else{
    rr = RM.w(XX, wt, write.file = save, .d=c(0.5, extr),country="Chile_CASEN2020_total") 
    if (save==T) {
      file.rename(from ="OutputChile_CASEN2020_total.csv",
                  to=ifelse(weights==T,"Outputs/indiv/FIES/OutputChile_CASEN2020_total_wt.csv","Outputs/indiv/FIES/OutputChile_CASEN2020_total.csv"))
    }
  }
  
  ## explore Rasch output
  rr
  rr$infit
  rr$outfit
  
  # Remove item 1 due to bad infit ---------------------------------------------
  XX=XX[,-1]
  
  ## re-compute the pseudo raw scores
  extr = sum(rowSums(XX)==7, na.rm=T)/sum(!is.na(rowSums(XX)) & !rowSums(XX)==0)
  extr = c(min(6.7, 6.5+extr))
  
  ## implement rasch with 7 items
  
  if (HH) {
    rr_1 = RM.w(XX, wt, write.file = save, .d=c(0.5, extr),country="Chile_CASEN2020 Excluido item1")
    if (save==T) {
      file.rename(from ="OutputChile_CASEN2020 Excluido item1.csv",
                  to=ifelse(weights==T,"Outputs/hh/FIES/OutputChile_CASEN2020 Excluido item1_wt.csv","Outputs/hh/FIES/OutputChile_CASEN2020 Excluido item1.csv"))
    }
  }else{
    rr_1 = RM.w(XX, wt, write.file = save, .d=c(0.5, extr),country="Chile_CASEN2020 Excluido item1_total")
    if (save==T) {
      file.rename(from ="OutputChile_CASEN2020 Excluido item1_total.csv",
                  to=ifelse(weights==T,"Outputs/indiv/FIES/OutputChile_CASEN2020 Excluido item1_total_wt.csv","Outputs/indiv/FIES/OutputChile_CASEN2020 Excluido item1_total.csv"))
    }
  }
  
  rr_1$infit ##assess the infit again
  rr_1$outfit
  
  # Apply the "equating function" ----------------------------------------------
  if (HH) {
    ee1 = equating.fun(rr_1, st=b.tot, thres= b.tot[c(5,8)], plot=save,iterative=T,
                       spec.com1=c(1:7),spec.com2=c(2:8))
    if (save==T) {
      file.rename(from="Equating_plot.pdf",
                  to=ifelse(weights==T,"Outputs/hh/FIES/equating_plot_Allcommons_wt.pdf","Outputs/hh/FIES/equating_plot_Allcommons.pdf"))
    }
  }else{
    ee1 = equating.fun(rr_1, st=b.tot, thres= b.tot[c(5,8)], plot=save,iterative=T,
                       spec.com1=c(1:7),spec.com2=c(2:8))
    if (save==T) {
      file.rename(from="Equating_plot.pdf",
                  to=ifelse(weights==T,"Outputs/indiv/FIES/equating_plot_Allcommons_total_wt.pdf","Outputs/indiv/FIES/equating_plot_Allcommons_total.pdf"))
    }
  }
  
  ee1$prevs*100
  ee1
  
  #if (save==T) {
  #  if (HH) {
  ## Estimate household prevalence 
  #HH.prev=cbind(round(ee1$prevs*100,2))
  #rownames(HH.prev)=c("Moderada o severa", "Severa")
  #colnames(HH.prev)="CASEN2020_Prevalencias nacionales de inseguridad alimentaria_HOGARES "
  #write.csv(HH.prev, file = "Outputs/hh/FIES/CASEN2020_Tasa de prevalencias para hogares.csv")
  #  }else{
  ## Estimate indiv prevalence 
  #Indiv.prev=cbind(round(ee1$prevs*100,2))
  #rownames(Indiv.prev)=c("Moderada o severa", "Severa")
  #colnames(Indiv.prev)="CASEN2020_Prevalencias nacionales de inseguridad alimentaria_Individuals"
  #write.csv(Indiv.prev, file = "Outputs/indiv/FIES/CASEN2020_Tasa de prevalencias para individuos_total.csv")
  #  }
  #}
  
  # Probabilities  -------------------------------------------------------------
  if (HH) {
    rs=rowSums(rr_1$XX)
    prob.rs.mat=ee1$probs.rs                      # probabilities for each raw score
    prob.mod=prob.rs.mat[rs+1,1]
    prob.sev=prob.rs.mat[rs+1,2]
    dim(prob.rs.mat)
    head(prob.rs.mat)
    
    folio=data$folio[jefeHH]
    strata=data$estrato[jefeHH]
    conglo= data$cod_upm[jefeHH]
    wt = data$wt[jefeHH]
    
    # Add variables to produce disaggregated estimates later
    comuna=data$comuna[jefeHH] 
    region=data$region[jefeHH] 
    cod_upm=data$cod_upm[jefeHH] 
    zona=data$zona[jefeHH]
    sexo=data$sexo[jefeHH]
    edad=data$edad[jefeHH]
    quintil=data$qaut[jefeHH]
    indigena=data$r3[jefeHH]
    pobreza=data$pobreza[jefeHH]
    educacion=data$e6a[jefeHH]
    FS_CASEN2020_Whole=data.frame(folio,zona,sexo,educacion,edad,quintil,cod_upm,
                                  comuna,region,indigena,pobreza,rs,wt,strata,conglo,
                                  prob.mod,prob.sev)
    dim(FS_CASEN2020_Whole)
    if (save==T) {
      write.xlsx(FS_CASEN2020_Whole,
                 file=ifelse(weights==T,"Outputs/hh/FIES/FS_hogares_CASEN2020_Whole_wt.xlsx",
                             "Outputs/hh/FIES/FS_hogares_CASEN2020_Whole.xlsx"),
                 rowNames = T, 
                 overwrite = T)
    }
  }else{
    rs=rowSums(rr_1$XX)
    prob.rs.mat=ee1$probs.rs                      # probabilities for each raw score
    prob.mod=prob.rs.mat[rs+1,1]
    prob.sev=prob.rs.mat[rs+1,2]
    dim(prob.rs.mat)
    head(prob.rs.mat)
    
    folio=data$folio
    strata=data$estrato
    conglo= data$cod_upm
    wt = data$wt
    
    # Add variables to produce disaggregated estimates later
    comuna=data$comuna
    region=data$region
    cod_upm=data$cod_upm
    zona=data$zona
    sexo=data$sexo
    edad=data$edad
    quintil=data$qaut
    indigena=data$r3
    pobreza=data$pobreza
    educacion=data$e6a
    FS_CASEN2020_Whole=data.frame(folio,zona,sexo,educacion,edad,quintil,cod_upm,
                                  comuna,region,indigena,pobreza,rs,wt,strata,conglo,
                                  prob.mod,prob.sev)
    dim(FS_CASEN2020_Whole)
    if (save==T) {
      write.xlsx(FS_CASEN2020_Whole,
                 file=ifelse(weights==T,"Outputs/indiv/FIES/FS_indiv_CASEN2020_Whole_wt.xlsx",
                             "Outputs/indiv/FIES/FS_indiv_CASEN2020_Whole.xlsx"),
                 rowNames = T, 
                 overwrite = T) 
    }
    
  }
  
  # Final output ---------------------------------------------------------------
  return(FS_CASEN2020_Whole)
}

# * Filter comunas based on quality of direct estimates --------------------------
filter_comunas <- function(direct_estimates){
  
  #data2=read.xlsx("comunas2020.xlsx")
  #data2$comuna=as.numeric(data2$Grupos)
  #data2$deff <- data2$DEff
  #direct_estimates=merge(direct_estimates[,!colnames(direct_estimates)=="deff"],
  #                       data2[,c("comuna","deff")],by="comuna")
  
  # mean households by upm
  direct_estimates$promhupm <- direct_estimates$n/direct_estimates$n_psu_comuna
  
  # rho
  direct_estimates$rho <- (direct_estimates$deff-1)/(direct_estimates$promhupm-1)
  
  # limit of acceptable rho
  direct_estimates$coterho <- 1-(direct_estimates$n_psu_comuna/(direct_estimates$n_psu_comuna-1))
  
  # application of the criterias
  direct_estimates$calidad2[direct_estimates$n<50|
                              direct_estimates$deg_free<=2|
                              direct_estimates$rho<direct_estimates$coterho] <- 0
  direct_estimates$calidad2[!direct_estimates$n<50 &
                              !direct_estimates$deg_free<=2 &
                              !direct_estimates$rho<direct_estimates$coterho] <- 1
  direct_estimates$calidad2[direct_estimates$deg_free>=14] <- 1
  
  # factor of the criteria
  direct_estimates$calidadf <- factor(direct_estimates$calidad2,levels=c(1,0),
                                      labels = c("Incluir","Excluir"))
  table(direct_estimates$calidadf)
  
  # Variables by criteria (to check reason of inclusion/exclusion)
  direct_estimates$incdf14 <- direct_estimates$deg_free>=14
  direct_estimates$excn50 <- !direct_estimates$n<50
  # direct_estimates$excdeff1=!direct_estimates$DEff<1
  direct_estimates$excrho <- !direct_estimates$rho<direct_estimates$coterho
  direct_estimates$excdf2 <- !direct_estimates$deg_free<=2
  
  direct_estimates$incdf14 <- factor(direct_estimates$incdf14,levels = c(T,F),
                                     labels = c("Incluir", "Excluir"))
  direct_estimates$excn50 <- factor(direct_estimates$excn50,levels = c(T,F),
                                    labels = c( "Incluir","Excluir"))
  # direct_estimates$excdeff1=factor(direct_estimates$excdeff1,levels = c(T,F),
  #                                  labels = c( "Incluir","Excluir"))
  direct_estimates$excrho <- factor(direct_estimates$excrho,levels = c(T,F),
                                    labels = c( "Incluir","Excluir"))
  direct_estimates$excdf2 <- factor(direct_estimates$excdf2,levels = c(T,F),
                                    labels = c( "Incluir","Excluir"))
  
  return(direct_estimates)
}

# * Direct estimates -------------------------------------------------------------
DE_CASEN2020 = function(data, disaggregation=c("national","region","comuna"),HH=T,save=F) {
  
  source("Scripts/moe_complex survey design_rev2.R")
  
  # Packages -------------------------------------------------------------------
  list.of.packages <- c("survey","openxlsx", "dplyr")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) {
    install.packages(new.packages)
  }
  for (i in list.of.packages) {
    library(i,character.only = TRUE)
  }
  rm(i,list.of.packages,new.packages)
  
  # National level -------------------------------------------------------------
  if (disaggregation=="national") {
    ## Estimates+Moe+CI for ModSev Prevalence
    moes.national.mod = moe3(data$prob.mod, 
                             rs=data$rs, wt=data$wt, psu=data$conglo, 
                             strata=data$strata, conf.level = 0.9)
    
    info_mod <- as.data.frame(
      cbind(estim.national = weighted.mean(data$prob.mod, data$wt,na.rm=T),
            moes.national = as.numeric(moes.national.mod$moe),
            se.m.national = as.numeric(moes.national.mod$se_m),
            se.s.national = moes.national.mod$se_s,
            deff.national = survey::deff(svymean(~prob.mod,
                                                 svydesign(ids=~conglo,weights = ~ wt,strata=~strata,nest=T,
                                                           lonely.psu=getOption("survey.lonely.psu"="adjust"),data=data),  
                                                 deff = T,na.rm=T))))
    colnames(info_mod)[4] <- "se.s.national"
    
    info_mod$se.national <- sqrt(info_mod$se.s.national^2+info_mod$se.m.national^2)
    info_mod$var.national <- info_mod$se.national^2
    info_mod$cv.national <- (info_mod$se.national/info_mod$estim.national)
    rownames(info_mod) <- "ModerateSevere"
    info_mod$CI.L <-  (info_mod$estim.national-(info_mod$se.national*1.64)) ##Lower bound confidence interval
    info_mod$CI.U <-  (info_mod$estim.national+(info_mod$se.national*1.64)) ##Upper bound confidence interval
    info_mod
    
    ## Estimates+Moe+CI for Sev Prevalence
    moes.national.sev=moe3(data$prob.sev, 
                           rs=data$rs, wt=data$wt,psu=data$conglo, 
                           strata=data$strata, conf.level = 0.9)
    
    info_sev <- as.data.frame(
      cbind(estim.national = weighted.mean(data$prob.sev, data$wt,na.rm=T),
            moes.national = as.numeric(moes.national.sev$moe),
            se.m.national = as.numeric(moes.national.sev$se_m),
            se.s.national = moes.national.sev$se_s,
            deff.national = survey::deff(svymean(~prob.sev,
                                                 svydesign(ids=~conglo,weights = ~ wt,strata=~strata,nest=T,
                                                           lonely.psu=getOption("survey.lonely.psu"="adjust"), data=data), 
                                                 deff = T,na.rm=T))))
    colnames(info_sev)[4] <- "se.s.national"
    
    info_sev$se.national <- sqrt(info_sev$se.s.national^2+info_sev$se.m.national^2)
    info_sev$var.national <- info_sev$se.national^2
    info_sev$cv.national <- (info_sev$se.national/info_sev$estim.national)
    rownames(info_sev) <- "Severe"
    info_sev$CI.L <-  (info_sev$estim.national-(info_sev$se.national*1.64)) ##Lower bound confidence interval
    info_sev$CI.U <-  (info_sev$estim.national+(info_sev$se.national*1.64)) ##Upper bound confidence interval
    info_sev
    
    summary_info_casean <- rbind(info_mod, info_sev)
    
    if (HH) {
      if (save==T) {
        write.csv(summary_info_casean, 
                file = "Outputs/hh/Direct estimates/Direct estimates national_hh.csv")
        write.xlsx(summary_info_casean, 
                 file = "Outputs/hh/Direct estimates/Direct estimates national_hh.xlsx")
        }
      }else{
        if (save==T) {
          write.csv(summary_info_casean, 
                    file = "Outputs/indiv/Direct estimates/Direct estimates national_indiv.csv")
          write.xlsx(summary_info_casean,
                     file = "Outputs/indiv/Direct estimates/Direct estimates national_indiv.csv")
        }
      }
    return(summary_info_casean)
  }
  
  # Region level ----------
  if (disaggregation=="region") {
    
    ## Estimates+Moe+CI for ModSev Prevalence
    prevs.region <- data %>%
      group_by(region) %>% 
      summarise(ModerateSevere=weighted.mean(prob.mod,wt,na.rm=T),
                #Severe=weighted.mean(prob.sev,wt,na.rm=T),
                n = n(),
                n_psu_region = as.numeric(n_distinct(cod_upm)),
                n_strata_region = as.numeric(n_distinct(strata))) %>%
      mutate(deg_free = n_psu_region - n_strata_region)
    #write.csv(prevs.region,file="Prevalencia de hogares por region.csv") 
    
    moes.region.mod=sapply(sort(unique(data$region)), function(i)
      moe3(data$prob.mod[data$region==i], rs=data$rs[data$region==i], 
           wt=data$wt[data$region==i],psu=data$conglo[data$region==i],
           strata=data$strata[data$region==i], conf.level = 0.9))
    dim(moes.region.mod)
    
    prevs.region$moes<- unlist(moes.region.mod[1,])
    prevs.region$se.m<- unlist(moes.region.mod[2,])
    prevs.region$se.s<- unlist(moes.region.mod[3,])
    
    prevs.region$deff <- sapply(sort(unique(data$region)), function(i)
      survey::deff(svymean(~prob.mod, 
                           svydesign(ids=~conglo,weights = ~ wt,strata=~strata,nest=T,
                                     lonely.psu=getOption("survey.lonely.psu"="adjust"),
                                     data=data[data$region==i,]), 
                           deff = T,na.rm=T)))
    
    prevs.region$se.tot<-sqrt(prevs.region$se.m^2 + prevs.region$se.s^2)
    prevs.region$var.tot<-prevs.region$se.tot^2
    prevs.region$cv<-(prevs.region$se.tot/prevs.region$ModerateSevere)
    prevs.region$CI_L<-(prevs.region$ModerateSevere-(prevs.region$se.tot*1.64))
    prevs.region$CI_U<-(prevs.region$ModerateSevere+(prevs.region$se.tot*1.64))
    
    info_mod_region <- prevs.region
    
    if (HH) {
      if (save==T) {
        write.csv(info_mod_region, 
                  file = "Outputs/hh/Direct estimates/Direct estimates ModSev region_hh.csv")
        write.xlsx(info_mod_region, 
                   file = "Outputs/hh/Direct estimates/Direct estimates ModSev region_hh.xlsx")
        }
      }else{
        if (save==T) {
          write.csv(info_mod_region, 
                    file = "Outputs/indiv/Direct estimates/Direct estimates ModSev region_indiv.csv")
          write.xlsx(info_mod_region,
                     file = "Outputs/indiv/Direct estimates/Direct estimates ModSev region_indiv.csv")
          }
        }
    
    # Estimates+Moe+CI for Sev Prevalence 
    prevs.region <- data %>%
      group_by(region) %>% 
      summarise(Severe=weighted.mean(prob.sev,wt,na.rm=T),
                n = n(),
                n_psu_region = as.numeric(n_distinct(cod_upm)),
                n_strata_region = as.numeric(n_distinct(strata))) %>%
      mutate(deg_free = n_psu_region - n_strata_region)
    
    moes.region.sev=sapply(sort(unique(data$region)), function(i)
      moe3(data$prob.sev[data$region==i], rs=data$rs[data$region==i], 
           wt=data$wt[data$region==i],psu=data$conglo[data$region==i],
           strata=data$strata[data$region==i], conf.level = 0.9))
    dim(moes.region.sev)
    
    prevs.region$moes<- unlist(moes.region.sev[1,])
    prevs.region$se.m<- unlist(moes.region.sev[2,])
    prevs.region$se.s<- unlist(moes.region.sev[3,])
    
    prevs.region$deff <- sapply(sort(unique(data$region)), function(i)
      survey::deff(svymean(~prob.sev, 
                           svydesign(ids=~conglo,weights = ~ wt,strata=~strata,nest=T,
                                     lonely.psu=getOption("survey.lonely.psu"="adjust"), 
                                     data=data[data$region==i,]), 
                           deff = T,na.rm=T)))
    
    prevs.region$se.tot<-sqrt(prevs.region$se.m^2 + prevs.region$se.s^2)
    prevs.region$var.tot<-prevs.region$se.tot^2
    prevs.region$cv<-(prevs.region$se.tot/prevs.region$Severe)
    prevs.region$CI_L<-(prevs.region$Severe-(prevs.region$se.tot*1.64))
    prevs.region$CI_U<-(prevs.region$Severe+(prevs.region$se.tot*1.64))
    
    info_sev_region <- prevs.region
    
    if (HH) {
      if (save==T) {
        write.csv(info_sev_region,
                  file = "Outputs/hh/Direct estimates/Direct estimates Sev region_hh.csv")
        write.xlsx(info_sev_region, 
                   file = "Outputs/hh/Direct estimates/Direct estimates Sev region_hh.xlsx")
        }
      }else{
        if (save==T) {
          write.csv(info_sev_region,
                    file = "Outputs/indiv/Direct estimates/Direct estimates Sev region_indiv.csv")
          write.xlsx(info_sev_region, 
                     file = "Outputs/indiv/Direct estimates/Direct estimates Sev region_indiv.csv")
          }
        }
    return(list(info_mod_region,info_sev_region))
    
  }
  
  # Comuna level ----------
  if (disaggregation=="comuna") {
    ## Estimates+Moe+CI for ModSev Prevalence
    prevs.comuna <- data %>%
      group_by(comuna) %>% 
      summarise(ModerateSevere=weighted.mean(prob.mod,wt,na.rm=T),
                #Severe=weighted.mean(prob.sev,wt,na.rm=T),
                n = n(),
                n_psu_comuna = as.numeric(n_distinct(cod_upm)),
                n_strata_comuna = as.numeric(n_distinct(strata))) %>%
      mutate(deg_free = n_psu_comuna - n_strata_comuna)
    #write.csv(prevs.comuna,file="Prevalencia de hogares por comuna.csv") 
    
    moes.comuna.mod=sapply(sort(unique(data$comuna)), function(i)
      moe3(data$prob.mod[data$comuna==i], rs=data$rs[data$comuna==i], 
           wt=data$wt[data$comuna==i],psu=data$conglo[data$comuna==i],
           strata=data$strata[data$comuna==i], conf.level = 0.9))
    dim(moes.comuna.mod)
    
    prevs.comuna$se.m<- unlist(moes.comuna.mod[2,])
    prevs.comuna$se.s<- unlist(moes.comuna.mod[3,])
    
    prevs.comuna$deff <- sapply(sort(unique(data$comuna)), function(i)
      survey::deff(svymean(~prob.mod, 
                           svydesign(ids=~conglo,weights = ~ wt,strata=~strata,nest=T,
                                     lonely.psu=getOption("survey.lonely.psu"="adjust"),
                                     data=data[data$comuna==i,]), 
                           deff = T,na.rm=T)))
    
    prevs.comuna$se.tot<-sqrt(prevs.comuna$se.m^2 + prevs.comuna$se.s^2)
    prevs.comuna$var.tot<-prevs.comuna$se.tot^2
    prevs.comuna$cv<-(prevs.comuna$se.tot/prevs.comuna$ModerateSevere)
    prevs.comuna$CI_L<-(prevs.comuna$ModerateSevere-(prevs.comuna$se.tot*1.64))
    prevs.comuna$CI_U<-(prevs.comuna$ModerateSevere+(prevs.comuna$se.tot*1.64))
    
    info_mod_comuna <- prevs.comuna
    
    info_mod_comuna <- filter_comunas(info_mod_comuna)
    
    if (HH) {
      if (save==T) {
        write.csv(info_mod_comuna, 
                  file = "Outputs/hh/Direct estimates/Direct estimates ModSev comuna_hh.csv")
        write.xlsx(info_mod_comuna, 
                   file = "Outputs/hh/Direct estimates/Direct estimates ModSev comuna_hh.xlsx")
        }
      }else{
        if (save==T) {
          write.csv(info_mod_comuna,
                    file = "Outputs/indiv/Direct estimates/Direct estimates ModSev comuna_indiv.csv")
          write.xlsx(info_mod_comuna, 
                     file = "Outputs/indiv/Direct estimates/Direct estimates ModSev comuna_indiv.csv")
          }
        }
    
    # Estimates+Moe+CI for Sev Prevalence 
    prevs.comuna <- data %>%
      group_by(comuna) %>% 
      summarise(Severe=weighted.mean(prob.sev,wt,na.rm=T),
                n = n(),
                n_psu_comuna = as.numeric(n_distinct(cod_upm)),
                n_strata_comuna = as.numeric(n_distinct(strata))) %>%
      mutate(deg_free = n_psu_comuna - n_strata_comuna)
    
    moes.comuna.sev=sapply(sort(unique(data$comuna)), function(i)
      moe3(data$prob.sev[data$comuna==i], rs=data$rs[data$comuna==i], 
           wt=data$wt[data$comuna==i],psu=data$conglo[data$comuna==i],
           strata=data$strata[data$comuna==i], conf.level = 0.9))
    dim(moes.comuna.sev)
    
    prevs.comuna$se.m<- unlist(moes.comuna.sev[2,])
    prevs.comuna$se.s<- unlist(moes.comuna.sev[3,])
    
    prevs.comuna$deff <- sapply(sort(unique(data$comuna)), function(i)
      survey::deff(svymean(~prob.sev, 
                           svydesign(ids=~conglo,weights = ~ wt,strata=~strata,nest=T,
                                     lonely.psu=getOption("survey.lonely.psu"="adjust"), 
                                     data=data[data$comuna==i,]), 
                           deff = T,na.rm=T)))
    
    prevs.comuna$se.tot<-sqrt(prevs.comuna$se.m^2 + prevs.comuna$se.s^2)
    prevs.comuna$var.tot<-prevs.comuna$se.tot^2
    prevs.comuna$cv<-(prevs.comuna$se.tot/prevs.comuna$Severe)
    prevs.comuna$CI_L<-(prevs.comuna$Severe-(prevs.comuna$se.tot*1.64))
    prevs.comuna$CI_U<-(prevs.comuna$Severe+(prevs.comuna$se.tot*1.64))
    
    info_sev_comuna <- prevs.comuna
    
    info_sev_comuna <- filter_comunas(info_sev_comuna)
    
    if (HH) {
      if (save==T) {
        write.csv(info_sev_comuna,
                  file = "Outputs/hh/Direct estimates/Direct estimates Sev comuna_hh.csv")
        write.xlsx(info_sev_comuna, 
                   file = "Outputs/hh/Direct estimates/Direct estimates Sev comuna_hh.xlsx")
        }
      }else{
        if (save==T) {
          write.csv(info_sev_comuna, 
                    file = "Outputs/indiv/Direct estimates/Direct estimates Sev comuna_indiv.csv")
          write.xlsx(info_sev_comuna, 
                     file = "Outputs/indiv/Direct estimates/Direct estimates Sev comuna_indiv.csv")
          }
        }
    return(list(info_mod_comuna,info_sev_comuna))
  }
  
}

# * Potter Method ----------------------------------------------------------------
potter <- function(weights, ValorC) {
  
  n <- length(weights)
  K <- sqrt(ValorC * (sum(weights^2) / n))
  
  weights_trunc <- ifelse(weights >= K, K, weights)
  weights_trunc <- weights_trunc * (sum(weights)  / sum(weights_trunc))
  
  return(weights_trunc)
}

# * Function to optimize (Potter Method) -----------------------------------------
optimizador2 <- function(ValorC, encuesta, which_prob= c("ModSev","Sev")){
  
  source("Scripts/moe_complex survey design_rev2.R")
  encuesta$wt_new <- potter(encuesta$wt, ValorC)
  
  if (which_prob=="ModSev") {
    prob <- encuesta$prob.mod
  }else{
    prob <- encuesta$prob.sev
  }
  
  var_theta_i <- moe3(prob = prob, 
                      rs=encuesta$rs, 
                      wt= encuesta$wt_new,
                      psu=encuesta$conglo,
                      strata=encuesta$strata, 
                      conf.level = 0.9)
  
  MSE <- (var_theta_i$se_s^2+var_theta_i$se_m^2) + 
    (weighted.mean(prob,encuesta$wt_new, na.rm=T) -
       weighted.mean(prob,encuesta$wt, na.rm=T))^2
  dimnames(MSE) <-  NULL
  
  return(MSE)
}

# * Optimize C value
optimize_c_potter <- function(encuesta, lower = 1, upper = 50, which_prob =c("ModSev", "Sev")) {
  source("Scripts/moe_complex survey design_rev2.R")
  
  objective <- function(ValorC) {
    encuesta$wt_new <- potter(encuesta$wt, ValorC = ValorC)
    if (which_prob=="ModSev") {
      prob <- encuesta$prob.mod
    }else{
      prob <- encuesta$prob.sev
    }
    
    var_theta_i <- moe3(prob = prob, 
                        rs=encuesta$rs, 
                        wt= encuesta$wt_new,
                        psu=encuesta$conglo,
                        strata=encuesta$strata, 
                        conf.level = 0.9)
    
    MSE <- (var_theta_i$se_s^2+var_theta_i$se_m^2) + 
      (weighted.mean(prob,encuesta$wt_new, na.rm=T) -
         weighted.mean(prob,encuesta$wt, na.rm=T))^2
    dimnames(MSE) <-  NULL
    
    return(MSE)}
  
  result <- optimize(objective, interval = seq(lower, upper, 1))
  return(round(result$minimum, 0))
}

# * Variance smoothing -----------------------------------------------------------
gvf <- function(data,model){
  
  # Packages -------------------------------------------------------------------
  list.of.packages <- c("ggplot2","ggpubr")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) {
    install.packages(new.packages)
  }
  for (i in list.of.packages) {
    library(i,character.only = TRUE)
  }
  rm(i,list.of.packages,new.packages)
  
  # Preliminary plots ----------------------------------------------------------
  data$var.tot.log <- log(data$var.tot)
  
  plot_y_var <- ggplot(data = data, aes(x = ModerateSevere, y = var.tot.log)) + 
    geom_point() +
    geom_smooth(method = "lm", formula= y~x) +
    annotate("text", label = paste0("\n\nLinear correlation\n",
                                    as.character(
                                      round(cor(data$ModerateSevere,data$var.tot.log,
                                                use = "complete.obs"),2))), 
             x = max(data$ModerateSevere)/2, y = max(data$var.tot.log)/2)
  
  plot_n_var <- ggplot(data = data, aes(x = n, y = var.tot.log)) + 
    geom_point() +
    geom_smooth(method = "lm", formula= y~x) +
    annotate("text", label = paste0("\n\nLinear correlation\n",
                                    as.character(
                                      round(cor(data$n,data$var.tot.log,
                                                use = "complete.obs"),2))), 
             x = max(data$n)/2, y = max(data$var.tot.log)/2)
  
  plot_sqrtn_var <- ggplot(data = data, aes(x = sqrt(n), y = var.tot.log)) + 
    geom_point() +
    geom_smooth(method = "lm", formula= y~x) +
    annotate("text", label = paste0("\n\nLinear correlation\n",
                                    as.character(
                                      round(cor(sqrt(data$n),data$var.tot.log,
                                                use = "complete.obs"),2))), 
             x = max(sqrt(data$n))/2, y = max(data$var.tot.log)/2)
  
  plot_prelim <- ggarrange(ggarrange(plot_y_var, plot_n_var,ncol=2), 
                           ggarrange(NULL, plot_sqrtn_var, NULL, ncol=3, widths = c(1,2,1)), 
                           ncol = 1)
  
  #gvf0 <- lm(var.tot.log ~ ModerateSevere + n, data=data)
  #summary(gvf0)
  
  # GVF model1 -----------------------------------------------------------------
  if (model=="model1") {
    
    gvf <- lm(var.tot.log ~ ModerateSevere + n + I(n ^ 2) + I(ModerateSevere * n) + I(sqrt(ModerateSevere)) +
                I(sqrt(n)) + I(sqrt(ModerateSevere * n)),
              data=data)
    #summary(gvf)
    #plot(gvf)
    
    data$var.tot.smooth <- exp(predict(gvf))
    data$var.tot.smooth.corr <- data$var.tot.smooth*(sum(data$var.tot,na.rm=T)/sum(data$var.tot.smooth,na.rm=T))
    
    ncomunal <- nrow(data)
    plot_gvf_compare <- ggplot(data=data[order(data$n),],aes(x=1:ncomunal)) +
      geom_line(aes(y=var.tot,color="VarDirEst")) +
      geom_line(aes(y=var.tot.smooth.corr,color="GVF")) +
      scale_x_continuous(breaks = seq(1, ncomunal, by = 10),
                         labels = data$n[order(data$n)][seq(1,ncomunal, by = 10)]) +
      scale_colour_manual("", values = c("VarDirEst"="red","GVF"="blue")) +
      labs(y = "Variances", x = "n", color = " ")
    
    plot_gvf_compare2 <- data %>% arrange(n) %>% group_by(n) %>%
      summarise(var.tot.mean=mean(var.tot),
                var.tot.smooth.corr.mean=mean(var.tot.smooth.corr)) %>%
      ggplot(aes(x = n)) +
      geom_line(aes(y = var.tot.mean, color = "VarDirEst")) +
      geom_line(aes(y = var.tot.smooth.corr.mean, color = "GVF")) +
      scale_colour_manual("", 
                          values = c("VarDirEst"="red","GVF"="blue"))
  }
  
  # GVF model2 -----------------------------------------------------------------
  if(model=="model2"){
    
    data$var.rel <- data$var.tot/data$ModerateSevere^2
    data$ModerateSevere_inv <- 1/data$ModerateSevere
    
    gvf <- lm(log(var.rel) ~ log(ModerateSevere_inv)+log(n), data=data)
    #summary(gvf)
    #plot(gvf)
    
    data$var.tot.smooth <- exp(predict(gvf))*data$ModerateSevere**2
    data$var.tot.smooth.corr <- data$var.tot.smooth*(sum(data$var.tot,na.rm=T)/sum(data$var.tot.smooth,na.rm=T))
    
    ncomunal <- nrow(data)
    plot_gvf_compare <- ggplot(data=data[order(data$n),],aes(x=1:ncomunal)) +
      geom_line(aes(y=var.tot,color="VarDirEst")) +
      geom_line(aes(y=var.tot.smooth.corr,color="GVF")) +
      scale_x_continuous(breaks = seq(1, ncomunal, by = 10),
                         labels = data$n[order(data$n)][seq(1,ncomunal, by = 10)]) + 
      scale_colour_manual("",values = c("VarDirEst"="red","GVF"="blue"))
    
    plot_gvf_compare2 <- data %>% arrange(n) %>% group_by(n) %>%
      summarise(var.tot.mean=mean(var.tot),
                var.tot.smooth.corr.mean=mean(var.tot.smooth.corr)) %>%
      ggplot(aes(x = n)) +
      geom_line(aes(y = var.tot.mean, color = "VarDirEst")) +
      geom_line(aes(y = var.tot.smooth.corr.mean, color = "GVF")) +
      scale_colour_manual("",values = c("VarDirEst"="red","GVF"="blue")) +
      labs(y = "Variances", x = "n", color = " ")
  }
  
  # GVF model3 -----------------------------------------------------------------
  if (model=="model3") {
    
    #gvf <- lm(log(var.tot) ~ ModerateSevere + n + sqrt(deff),data=data)
    data$var.rel <- data$var.tot/data$ModerateSevere^2
    data$ModerateSevere_inv <- 1/data$ModerateSevere
    
    gvf <- lm(var.rel ~ ModerateSevere_inv,data=data, weights=ModerateSevere_inv**2)
    #summary(gvf)
    #plot(gvf)
    
    data$var.tot.smooth <- predict(gvf)*data$ModerateSevere**2
    data$var.tot.smooth.corr <- data$var.tot.smooth*(sum(data$var.tot,na.rm=T)/sum(data$var.tot.smooth,na.rm=T))
    
    ncomunal <- nrow(data)
    plot_gvf_compare <- ggplot(data=data[order(data$n),],aes(x=1:ncomunal)) +
      geom_line(aes(y=var.tot,color="VarDirEst")) +
      geom_line(aes(y=var.tot.smooth.corr,color="GVF")) +
      scale_x_continuous(breaks = seq(1, ncomunal, by = 10),
                         labels = data$n[order(data$n)][seq(1,ncomunal, by = 10)]) +
      scale_colour_manual("", values = c("VarDirEst"="red","GVF"="blue")) +
      labs(y = "Variances", x = "n", color = " ")
    
    plot_gvf_compare2 <- data %>% arrange(n) %>% group_by(n) %>%
      summarise(var.tot.mean=mean(var.tot),
                var.tot.smooth.corr.mean=mean(var.tot.smooth.corr)) %>%
      ggplot(aes(x = n)) +
      geom_line(aes(y = var.tot.mean, color = "VarDirEst")) +
      geom_line(aes(y = var.tot.smooth.corr.mean, color = "GVF")) +
      scale_colour_manual("", 
                          values = c("VarDirEst"="red","GVF"="blue"))
  }
  if (model=="model_new") {
    
    gvf <- lm(log(var.tot) ~ sqrt(ModerateSevere) + n + I(n^2),data=data)
    #summary(gvf)
    #plot(gvf)
    
    data$var.tot.smooth <- exp(predict(gvf))
    data$var.tot.smooth.corr <- data$var.tot.smooth*(sum(data$var.tot,na.rm=T)/sum(data$var.tot.smooth,na.rm=T))
    
    ncomunal <- nrow(data)
    plot_gvf_compare <- ggplot(data=data[order(data$n),],aes(x=1:ncomunal)) +
      geom_line(aes(y=var.tot,color="VarDirEst")) +
      geom_line(aes(y=var.tot.smooth.corr,color="GVF")) +
      scale_x_continuous(breaks = seq(1, ncomunal, by = 10),
                         labels = data$n[order(data$n)][seq(1,ncomunal, by = 10)]) +
      scale_colour_manual("", values = c("VarDirEst"="red","GVF"="blue")) +
      labs(y = "Variances", x = "n", color = " ")
    
    plot_gvf_compare2 <- data %>% arrange(n) %>% group_by(n) %>%
      summarise(var.tot.mean=mean(var.tot),
                var.tot.smooth.corr.mean=mean(var.tot.smooth.corr)) %>%
      ggplot(aes(x = n)) +
      geom_line(aes(y = var.tot.mean, color = "VarDirEst")) +
      geom_line(aes(y = var.tot.smooth.corr.mean, color = "GVF")) +
      scale_colour_manual("", 
                          values = c("VarDirEst"="red","GVF"="blue"))
  }
  return(list(data=data,model=gvf,
              plot_prelim=plot_prelim,plot_gvf_compare=plot_gvf_compare))
}

gvf_new <- function(data,model){
  
  # Packages -------------------------------------------------------------------
  list.of.packages <- c("ggplot2","ggpubr")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) {
    install.packages(new.packages)
  }
  for (i in list.of.packages) {
    library(i,character.only = TRUE)
  }
  rm(i,list.of.packages,new.packages)
  
  # Preliminary plots ----------------------------------------------------------
  data$var.s <- (data$se.s)^2
  data$var.s.log <- log(data$var.s)
  
  plot_y_var <- ggplot(data = data, aes(x = ModerateSevere, y = var.s.log)) + 
    geom_point() +
    geom_smooth(method = "lm", formula= y~x) +
    annotate("text", label = paste0("\n\nLinear correlation\n",
                                    as.character(
                                      round(cor(data$ModerateSevere,data$var.s.log,
                                                use = "complete.obs"),2))), 
             x = max(data$ModerateSevere)/2, y = max(data$var.s.log)/2)
  
  plot_n_var <- ggplot(data = data, aes(x = n, y = var.s.log)) + 
    geom_point() +
    geom_smooth(method = "lm", formula= y~x) +
    annotate("text", label = paste0("\n\nLinear correlation\n",
                                    as.character(
                                      round(cor(data$n,data$var.s.log,
                                                use = "complete.obs"),2))), 
             x = max(data$n)/2, y = max(data$var.s.log)/2)
  
  plot_sqrtn_var <- ggplot(data = data, aes(x = sqrt(n), y = var.s.log)) + 
    geom_point() +
    geom_smooth(method = "lm", formula= y~x) +
    annotate("text", label = paste0("\n\nLinear correlation\n",
                                    as.character(
                                      round(cor(sqrt(data$n),data$var.s.log,
                                                use = "complete.obs"),2))), 
             x = max(sqrt(data$n))/2, y = max(data$var.s.log)/2)
  
  plot_prelim <- ggarrange(ggarrange(plot_y_var, plot_n_var,ncol=2), 
                           ggarrange(NULL, plot_sqrtn_var, NULL, ncol=3, widths = c(1,2,1)), 
                           ncol = 1)
  
  #gvf0 <- lm(var.s.log ~ ModerateSevere + n, data=data)
  #summary(gvf0)
  
  # GVF model1 -----------------------------------------------------------------
  if (model=="model1") {
    
    gvf <- lm(var.s.log ~ ModerateSevere + n + I(n ^ 2) + I(ModerateSevere * n) + I(sqrt(ModerateSevere)) +
                I(sqrt(n)) + I(sqrt(ModerateSevere * n)),
              data=data)
    #summary(gvf)
    #plot(gvf)
    
    data$var.s.smooth <- exp(predict(gvf))
    data$var.s.smooth.corr <- data$var.s.smooth*(sum(data$var.s,na.rm=T)/sum(data$var.s.smooth,na.rm=T))
    
    ncomunal <- nrow(data)
    plot_gvf_compare <- ggplot(data=data[order(data$n),],aes(x=1:ncomunal)) +
      geom_line(aes(y=var.s,color="VarDirEst")) +
      geom_line(aes(y=var.s.smooth.corr,color="GVF")) +
      scale_x_continuous(breaks = seq(1, ncomunal, by = 10),
                         labels = data$n[order(data$n)][seq(1,ncomunal, by = 10)]) +
      scale_colour_manual("", values = c("VarDirEst"="red","GVF"="blue")) +
      labs(y = "Variances", x = "n", color = " ")
    
    plot_gvf_compare2 <- data %>% arrange(n) %>% group_by(n) %>%
      summarise(var.s.mean=mean(var.s),
                var.s.smooth.corr.mean=mean(var.s.smooth.corr)) %>%
      ggplot(aes(x = n)) +
      geom_line(aes(y = var.s.mean, color = "VarDirEst")) +
      geom_line(aes(y = var.s.smooth.corr.mean, color = "GVF")) +
      scale_colour_manual("", 
                          values = c("VarDirEst"="red","GVF"="blue"))
  }
  
  # GVF model2 -----------------------------------------------------------------
  if(model=="model2"){
    
    data$var.rel <- data$var.s/data$ModerateSevere^2
    data$ModerateSevere_inv <- 1/data$ModerateSevere
    
    gvf <- lm(log(var.rel) ~ log(ModerateSevere_inv)+log(n), data=data)
    #summary(gvf)
    #plot(gvf)
    
    data$var.s.smooth <- exp(predict(gvf))*data$ModerateSevere**2
    data$var.s.smooth.corr <- data$var.s.smooth*(sum(data$var.s,na.rm=T)/sum(data$var.s.smooth,na.rm=T))
    
    ncomunal <- nrow(data)
    plot_gvf_compare <- ggplot(data=data[order(data$n),],aes(x=1:ncomunal)) +
      geom_line(aes(y=var.s,color="VarDirEst")) +
      geom_line(aes(y=var.s.smooth.corr,color="GVF")) +
      scale_x_continuous(breaks = seq(1, ncomunal, by = 10),
                         labels = data$n[order(data$n)][seq(1,ncomunal, by = 10)]) + 
      scale_colour_manual("",values = c("VarDirEst"="red","GVF"="blue"))
    
    plot_gvf_compare2 <- data %>% arrange(n) %>% group_by(n) %>%
      summarise(var.s.mean=mean(var.s),
                var.s.smooth.corr.mean=mean(var.s.smooth.corr)) %>%
      ggplot(aes(x = n)) +
      geom_line(aes(y = var.s.mean, color = "VarDirEst")) +
      geom_line(aes(y = var.s.smooth.corr.mean, color = "GVF")) +
      scale_colour_manual("",values = c("VarDirEst"="red","GVF"="blue")) +
      labs(y = "Variances", x = "n", color = " ")
  }
  
  # GVF model3 -----------------------------------------------------------------
  if (model=="model3") {
    
    #gvf <- lm(log(var.s) ~ ModerateSevere + n + sqrt(deff),data=data)
    data$var.rel <- data$var.s/data$ModerateSevere^2
    data$ModerateSevere_inv <- 1/data$ModerateSevere
    
    gvf <- lm(var.rel ~ ModerateSevere_inv,data=data, weights=ModerateSevere_inv**2)
    #summary(gvf)
    #plot(gvf)
    
    data$var.s.smooth <- predict(gvf)*data$ModerateSevere**2
    data$var.s.smooth.corr <- data$var.s.smooth*(sum(data$var.s,na.rm=T)/sum(data$s.tot.smooth,na.rm=T))
    
    ncomunal <- nrow(data)
    plot_gvf_compare <- ggplot(data=data[order(data$n),],aes(x=1:ncomunal)) +
      geom_line(aes(y=var.s,color="VarDirEst")) +
      geom_line(aes(y=var.s.smooth.corr,color="GVF")) +
      scale_x_continuous(breaks = seq(1, ncomunal, by = 10),
                         labels = data$n[order(data$n)][seq(1,ncomunal, by = 10)]) +
      scale_colour_manual("", values = c("VarDirEst"="red","GVF"="blue")) +
      labs(y = "Variances", x = "n", color = " ")
    
    plot_gvf_compare2 <- data %>% arrange(n) %>% group_by(n) %>%
      summarise(var.s.mean=mean(var.tot),
                var.s.smooth.corr.mean=mean(var.s.smooth.corr)) %>%
      ggplot(aes(x = n)) +
      geom_line(aes(y = var.s.mean, color = "VarDirEst")) +
      geom_line(aes(y = var.s.smooth.corr.mean, color = "GVF")) +
      scale_colour_manual("", 
                          values = c("VarDirEst"="red","GVF"="blue"))
  }
  if (model=="model_new") {
    
    gvf <- lm(log(var.s) ~ sqrt(ModerateSevere) + n + I(n^2),data=data)
    #summary(gvf)
    #plot(gvf)
    
    data$var.s.smooth <- exp(predict(gvf))
    data$var.s.smooth.corr <- data$var.s.smooth*(sum(data$var.s,na.rm=T)/sum(data$var.s.smooth,na.rm=T))
    
    ncomunal <- nrow(data)
    plot_gvf_compare <- ggplot(data=data[order(data$n),],aes(x=1:ncomunal)) +
      geom_line(aes(y=var.s,color="VarDirEst")) +
      geom_line(aes(y=var.s.smooth.corr,color="GVF")) +
      scale_x_continuous(breaks = seq(1, ncomunal, by = 10),
                         labels = data$n[order(data$n)][seq(1,ncomunal, by = 10)]) +
      scale_colour_manual("", values = c("VarDirEst"="red","GVF"="blue")) +
      labs(y = "Variances", x = "n", color = " ")
    
    plot_gvf_compare2 <- data %>% arrange(n) %>% group_by(n) %>%
      summarise(var.s.mean=mean(var.s),
                var.s.smooth.corr.mean=mean(var.s.smooth.corr)) %>%
      ggplot(aes(x = n)) +
      geom_line(aes(y = var.s.mean, color = "VarDirEst")) +
      geom_line(aes(y = var.s.smooth.corr.mean, color = "GVF")) +
      scale_colour_manual("", 
                          values = c("VarDirEst"="red","GVF"="blue"))
  }
  return(list(data=data,model=gvf,
              plot_prelim=plot_prelim,plot_gvf_compare=plot_gvf_compare))
}

# * Filtering of administrative variables ----------------------------------------
filter_var <- function(aux_data,which){
  
  # Packages -------------------------------------------------------------------
  list.of.packages <- c("dplyr")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) {
    install.packages(new.packages)
  }
  for (i in list.of.packages) {
    library(i,character.only = TRUE)
  }
  rm(i,list.of.packages,new.packages)
  
  if (which==1) {
    aux_data_sub <- aux_data[,
                             c("comuna",
                               "tasa_matricula_0_a_24_anos_2020","tasa_rezago_poblacion_2020","tasa_rezago_matricula_2020",
                               "prop_tofd_2019","promedio_afc_2020","promedio_pet_afc_2020","prop_b50median_afc_2020",
                               "prop_ism_afc_2020","mediana_afc_2020",
                               "prop_fonasa_a_2019","prop_fonasa_b_2019","prop_fonasa_c_2019","prop_fonasa_d_2019","prop_fonasa_ab_2019",
                               "tasa_victimizacion_2019","tasa_mort_infantil_2017","tasa_anios_perdidos_2014",
                               #"total_ing_municipales_permanentes_pc_2018",#
                               "log_ing_municipales_permanentes_pc_2018",
                               "prop_am_bajo_peso_2018","prop_am_normal_2018","prop_am_obeso_2018", "prop_am_sobrepeso_2018",
                               "prop_ive_sinae_2020","promedio_simce_leng_8b_2019","promedio_simce_mat_8b_2019","promedio_simce_hist_8b_2019",
                               "prop_obeso_sobrepeso_menores_2018","prop_desnutrido_menores_2018",     
                               "prop_normal_menores_2018","prop_obeso_menores_2018",
                               "prop_riesgo_desnutricion_menores_2018","prop_sobrepeso_menores_2018",
                               "prop_obeso_sobrepeso_menores_2018_w","prop_isapre_2019",
                               "prop_poblacion_indigena_todos_2017","prop_poblacion_pueblos_originarios_ley_chile_2017",
                               "prop_poblacion_inmigrante_2017","promedio_anios_escolaridad18_2017",
                               "prop_viv_sin_hacinamiento_2017","prop_viv_con_hacinamiento_medio_2017",      
                               "prop_viv_con_hacinamiento_critico_2017","prop_viv_con_hacinamiento_2017",
                               "prop_hog_sin_hacinamiento_2017", "prop_hog_con_hacinamiento_medio_2017",
                               "prop_hog_con_hacinamiento_critico_2017","prop_hog_con_hacinamiento_2017",
                               "prop_indice_materialidad_aceptable_2017",#
                               "prop_indice_materialidad_recuperable_2017",  
                               "prop_indice_materialidad_irrecuperable_2017",
                               "prop_red_publica_2017","prop_pozo_o_noria_2017","prop_camion_aljibe_2017","prop_rio_vertiente_estero_canal_2017",
                               "prop_deficit_habitacional_cuantitativo_2017","tasa_dependencia_familiar_2020","indice_de_vejez_2020")]
  }
  
  if (which==2) {
    aux_data_sub <- aux_data[,
                             c("comuna",
                               "tasa_matricula_0_a_24_anos_2020","tasa_rezago_poblacion_2020","tasa_rezago_matricula_2020",
                               "tasa_victimizacion_2019","tasa_mort_infantil_2017","tasa_anios_perdidos_2014",
                               "prop_ive_sinae_2020","promedio_simce_leng_8b_2019","promedio_simce_mat_8b_2019","promedio_simce_hist_8b_2019",
                               "prop_poblacion_indigena_todos_2017","prop_poblacion_pueblos_originarios_ley_chile_2017",
                               "prop_poblacion_inmigrante_2017",
                               "prop_viv_sin_hacinamiento_2017","prop_viv_con_hacinamiento_medio_2017",      
                               "prop_viv_con_hacinamiento_critico_2017","prop_viv_con_hacinamiento_2017",
                               "prop_hog_sin_hacinamiento_2017", "prop_hog_con_hacinamiento_medio_2017",
                               "prop_hog_con_hacinamiento_critico_2017","prop_hog_con_hacinamiento_2017",
                               "prop_indice_materialidad_aceptable_2017",#
                               "prop_indice_materialidad_recuperable_2017",  
                               "prop_indice_materialidad_irrecuperable_2017",
                               "prop_red_publica_2017","prop_pozo_o_noria_2017","prop_camion_aljibe_2017","prop_rio_vertiente_estero_canal_2017",
                               "prop_deficit_habitacional_cuantitativo_2017")]
  }
  
  #View(cor(aux_data_sub[,-1],use = "complete.obs"))
  
  return(aux_data_sub)
  
}

filter_var2 <- function(aux_data){
  
  # Packages -------------------------------------------------------------------
  list.of.packages <- c("mctest")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) {
    install.packages(new.packages)
  }
  for (i in list.of.packages) {
    library(i,character.only = TRUE)
  }
  rm(i,list.of.packages,new.packages)
  
  # Check collinearity
  check_vif <- imcdiag(lm(ModerateSevere ~.  , data =aux_data[,-1]))
  check_vif_d <- as.data.frame(check_vif[["idiags"]])
  #View(check_vif_d)
  
  # Many variables are function of others 
  check_vif_d_col <- check_vif_d[check_vif_d$VIF==Inf,]
  var_col <- rownames(check_vif_d_col[order(check_vif_d_col$VIF,decreasing =T),])
  
  var_col[1:4]
  #summary(lm(prop_am_bajo_peso_2018~.,data=aux_data[,var_col[1:4]]))
  #summary(lm(ModerateSevere~.,data=aux_data[,c("ModerateSevere",var_col[1:4])]))
  #cor(aux_data[,c("ModerateSevere",var_col[1:4])],use="complete.obs")
  var_to_remove <- c("prop_am_sobrepeso_2018")
  
  var_col[5:7]
  #summary(lm(prop_obeso_sobrepeso_menores_2018~.,data=aux_data[,var_col[5:7]]))
  #summary(lm(ModerateSevere~.,data=aux_data[,c("ModerateSevere",var_col[5:7])]))
  #cor(aux_data[,c("ModerateSevere",var_col[5:7])],use="complete.obs")
  var_to_remove <- append(var_to_remove, "prop_sobrepeso_menores_2018")
  
  var_col[8:11]
  #summary(lm(prop_viv_con_hacinamiento_medio_2017~.,data=aux_data[,var_col[8:11]]))
  #summary(lm(ModerateSevere~.,data=aux_data[,c("ModerateSevere",var_col[8:11])]))
  #cor(aux_data[,c("ModerateSevere",var_col[8:11])],use="complete.obs")
  var_to_remove <- append(var_to_remove, c("prop_viv_sin_hacinamiento_2017",#"prop_hog_con_hacinamiento_critico_2017",
                                           "prop_viv_con_hacinamiento_2017"))
  
  var_col[12:15]
  #summary(lm(prop_hog_sin_hacinamiento_2017~.,data=aux_data[,var_col[12:15]]))
  #summary(lm(ModerateSevere~.,data=aux_data[,c("ModerateSevere",var_col[12:15])]))
  #cor(aux_data[,c("ModerateSevere",var_col[12:15])],use="complete.obs")
  var_to_remove <- append(var_to_remove, c("prop_hog_sin_hacinamiento_2017",#"prop_hog_con_hacinamiento_critico_2017",
                                           "prop_hog_con_hacinamiento_2017"))
  
  var_col[16:18]
  #summary(lm(prop_indice_materialidad_aceptable_2017~.,data=aux_data[,var_col[16:18]]))
  #summary(lm(ModerateSevere~.,data=aux_data[,c("ModerateSevere",var_col[16:18])]))
  #cor(aux_data[,c("ModerateSevere",var_col[16:18])],use="complete.obs")
  var_to_remove <- append(var_to_remove, c("prop_indice_materialidad_irrecuperable_2017",#"prop_indice_materialidad_aceptable_2017",
                                           "prop_indice_materialidad_recuperable_2017"))
  
  var_col[19:20]
  #summary(lm(prop_red_publica_2017~.,data=aux_data[,var_col[19:20]]))
  #summary(lm(ModerateSevere~.,data=aux_data[,c("ModerateSevere",var_col[19:20])]))
  #cor(aux_data[,c("ModerateSevere",var_col[19:20])],use="complete.obs")
  var_to_remove <- append(var_to_remove, c("prop_pozo_o_noria_2017"))
  
  
  check_vif2 <- imcdiag(lm(ModerateSevere ~.  , 
                           data =aux_data[,-c(1,which(colnames(aux_data) %in% 
                                                        var_to_remove))]))
  check_vif_d2 <- as.data.frame(check_vif2[["idiags"]])
  #View(check_vif_d2)
  
  # Many variables are very correlated
  check_vif_d_col2 <- check_vif_d2[check_vif_d2$VIF>25,]
  var_col2 <- rownames(check_vif_d_col2[order(check_vif_d_col2$VIF,decreasing =T),])
  
  var_col2[1:5]
  #summary(lm(prop_fonasa_ab_2019~.,data=aux_data[,var_col2[1:5]]))
  #summary(lm(ModerateSevere~.,data=aux_data[,c("ModerateSevere",var_col2[c(1:5)])]))
  #cor(aux_data[,c("ModerateSevere",var_col2[1:5])],use="complete.obs")
  var_to_remove2 <- c("prop_fonasa_ab_2019","prop_fonasa_b_2019","prop_fonasa_d_2019")
  
  var_col2[6:7]
  #summary(lm(tasa_rezago_matricula_2020~.,data=aux_data[,var_col2[6:7]]))
  #summary(lm(ModerateSevere~.,data=aux_data[,c("ModerateSevere",var_col2[c(6:7)])]))
  #cor(aux_data[,c("ModerateSevere",var_col2[6:7])],use="complete.obs")
  var_to_remove2 <- append(var_to_remove2, c("tasa_rezago_poblacion_2020"))
  
  var_col2[8:9]
  #summary(lm(prop_hog_con_hacinamiento_medio_2017~.,data=aux_data[,var_col2[8:9]]))
  #summary(lm(ModerateSevere~.,data=aux_data[,c("ModerateSevere",var_col2[c(8:9)])]))
  #cor(aux_data[,c("ModerateSevere",var_col2[8:9])],use="complete.obs")
  var_to_remove2 <- append(var_to_remove2, c("prop_hog_con_hacinamiento_medio_2017"))
  
  var_col2[10:11]
  #summary(lm(prop_poblacion_indigena_todos_2017~.,data=aux_data[,var_col2[10:11]]))
  #summary(lm(ModerateSevere~.,data=aux_data[,c("ModerateSevere",var_col2[10:11])]))
  #cor(aux_data[,c("ModerateSevere",var_col2[10:11])],use="complete.obs")
  var_to_remove2 <- append(var_to_remove2, c("prop_poblacion_indigena_todos_2017"))
  
  var_col2[c(12:13,16,17)]
  #summary(lm(promedio_pet_afc_2020~.,data=aux_data[,var_col2[c(12:13,16,17)]]))
  #summary(lm(ModerateSevere~.,data=aux_data[,c("ModerateSevere",var_col2[c(12:13,16,17)])]))
  #cor(aux_data[,c("ModerateSevere",var_col2[c(12:13,16,17)])],use="complete.obs")
  var_to_remove2 <- append(var_to_remove2, c("promedio_pet_afc_2020","promedio_afc_2020","prop_tofd_2019"))
  
  var_col2[c(14:15)]
  #summary(lm(prop_hog_con_hacinamiento_critico_2017~.,data=aux_data[,var_col2[c(14:15)]]))
  #summary(lm(ModerateSevere~.,data=aux_data[,c("ModerateSevere",var_col2[c(14:15)])]))
  #cor(aux_data[,c("ModerateSevere",var_col2[c(14:15)])],use="complete.obs")
  var_to_remove2 <- append(var_to_remove2, c("prop_hog_con_hacinamiento_critico_2017"))
  
  var_col2[18:19]
  #summary(lm(prop_obeso_sobrepeso_menores_2018~.,data=aux_data[,var_col2[18:19]]))
  #summary(lm(ModerateSevere~.,data=aux_data[,c("ModerateSevere",var_col2[18:19])]))
  #cor(aux_data[,c("ModerateSevere",var_col2[c(18:19)])],use="complete.obs")
  var_to_remove2 <- append(var_to_remove2, c("prop_normal_menores_2018"))
  
  check_vif3 <- imcdiag(lm(ModerateSevere ~.  , 
                           data =aux_data[,-c(1,which(colnames(aux_data) %in% 
                                                        c(var_to_remove,var_to_remove2)))]))
  check_vif_d3 <- as.data.frame(check_vif3[["idiags"]])
  #View(check_vif_d3)
  
  aux_data_filt <- aux_data[,-c(which(colnames(aux_data) %in% c(var_to_remove,var_to_remove2)))]
  
  return(list(aux_data_filt=aux_data_filt,
              var_to_remove_final=c(var_to_remove,var_to_remove2)))
}

# * Stepwise selection -----------------------------------------------------------
stepwise_sel <- function(data,transformation=c("log","arcsin","no"),criteria=c("AIC","BIC")){
  
  # Packages -------------------------------------------------------------------
  list.of.packages <- c("car","relaimpo","R.utils")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) {
    install.packages(new.packages)
  }
  for (i in list.of.packages) {
    library(i,character.only = TRUE)
  }
  rm(i,list.of.packages,new.packages)
  
  if(is.null(criteria))
    criteria <- "AIC"
  
  if (transformation=="log") {
    data$ModerateSevere <- log(data$ModerateSevere)
    model_all <- lm(ModerateSevere~.,data=data)
  }else if(transformation=="arcsin"){
    data$ModerateSevere <- asin(sqrt(data$ModerateSevere))
    model_all <- lm(ModerateSevere~.,data=data)
  }else{
    model_all <- lm(ModerateSevere~.,data=data)
  }
  #summary(model_all)
  #sort(vif(model_all))
  
  if(criteria=="AIC"){
    mod <- stats::step(model_all, direction = "both", trace = 0)
    #summary(mod)
    #sort(vif(mod))
    
    # Calculate a relative score of importance of the selected variables
    tryCatch( 
      {
        rel.imp.res <- NULL
        rel.imp <- withTimeout({calc.relimp(mod, type = c("lmg"), rela = TRUE)},timeout=30,
                               onTimeout="warning")
        if(is.null(rel.imp)){
          rel.imp.res <- "reached elapsed time limit"
        }else{
          rel.imp.res <- sort(round(rel.imp@lmg*100,1),decreasing = TRUE)
          variables_step <- names(rel.imp.res)
        }
      },
      error = function(e) {
        "Error in the calculation of relative scores"
      }
    )
    
  }else{
    mod <- stats::step(model_all, direction = "both", trace = 0, k=log(dim(data[!is.na(data$ModerateSevere),])[1]))
    summary(mod)
    sort(vif(mod))
    
    # Calculate a relative score of importance of the selected variables
    tryCatch( 
      {
        rel.imp.res <- NULL
        rel.imp <- withTimeout({calc.relimp(mod, type = c("lmg"), rela = TRUE)},timeout=30,
                               onTimeout="warning")
        if(is.null(rel.imp)){
          rel.imp.res <- "reached elapsed time limit"
        }else{
          rel.imp.res <- sort(round(rel.imp@lmg*100,1),decreasing = TRUE)
          variables_step <- names(rel.imp.res)
        }
        
      },
      error = function(e) {
        "Error in the calculation of relative scores"
      }
    )
  }
  
  detach("package:relaimpo", unload = TRUE)
  detach("package:MASS", unload = TRUE)
  
  return(list(model=mod,selected_variables=rel.imp.res))
}