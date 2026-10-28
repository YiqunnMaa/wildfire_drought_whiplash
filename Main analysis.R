### Rapid wet-to-dry hydroclimate transitions intensify the mortality impact of wildfire smoke exposure in the contiguous United States ###
######################### Authors: Yiqun Ma, Jennifer D. Stowell, Tarik Benmarhnia, Azar M. Abadi, Pin Wang ###############################
####################################################### Date: October 28, 2026 ############################################################

library(fixest)
library(splines)

## 1. define variables
exposure.vars <- c("smokePM.ndays5", "smokePM.ndays10", "smokePM.ndays15", "smokePM.ndays20", 
                   "smokePM.ndays25","smokePM.ndays30", "smokePM.ndays35")

drought.vars <- c("SPEI03.cat", "SPEI12.cat")

whiplash.vars <- c("wl5.SPEI03", "wl5.SPEI12")


## 2. smoke-mortality associations
result.all <- data.frame()

for (i in 1:length(exposure.vars)){
  exposure.var <- exposure.vars[i]
  print(exposure.var)
  
  formula.i <- c(paste0("total ~ ", exposure.var, " + ns(Tmean, df=3) | COUNTY + year_month + STATE^year + STATE^month"))
  model = feglm(as.formula(formula.i), 
                data = data.all,
                offset = log(data.all$population),
                cluster = data.all$COUNTY,
                family = "quasipoisson")
  
  coef <- model$coefficients[1]
  SE <- model$se[1]

  table <- data.frame(exposure = exposure.var,
                      coef = coef,
                      SE = SE)
  
  result.all <- rbind(result.all, table)
}

## 3. effect modification by drought
result.all <- data.frame()

for (i in 1:length(exposure.vars)){
  exposure.var <- exposure.vars[i]
  print(exposure.var)
  
  formula.i <- c(paste0("total ~ ", exposure.var, " + ns(Tmean, df=3) | COUNTY + year_month + STATE^year + STATE^month"))
  
  for (k in 1:length(drought.vars)){
    drought.var <- drought.vars[k]
    data.all[,"modifier"] <- data.all[,drought.var]
    
    data1 <- data.all %>% filter(modifier %in% c("No drought"))
    data2 <- data.all %>% filter(modifier %in% c("Mild drought", "Severe drought"))
    
    # no drought
    model1 = feglm(as.formula(formula.i), 
                   data = data1,
                   offset = log(data1$population),
                   cluster = data1$COUNTY,
                   family = "quasipoisson")
    
    coef1 <- model1$coefficients[1]
    SE1 <- model1$se[1]
    
    # mild or severe drought
    model2 = feglm(as.formula(formula.i), 
                   data = data2,
                   offset = log(data2$population),
                   cluster = data2$COUNTY,
                   family = "quasipoisson")
    
    coef2 <- model2$coefficients[1]
    SE2 <- model2$se[1]
    
    table <- data.frame(exposure = exposure.var,
                        modifier = drought.var,
                        level = c("No drought", "Mild or severe drought"),
                        coef = c(coef1, coef2),
                        SE = c(SE1, SE2))
    
    result.all <- rbind(result.all, table)
  }
}

## 4. effect modification whiplash during drought
result.all <- data.frame()

for (i in 1:length(exposure.vars)){
  exposure.var <- exposure.vars[i]
  print(exposure.var)
  
  formula.i <- c(paste0("total ~ ", exposure.var, " + ns(Tmean, df=3) | COUNTY + year_month + STATE^year + STATE^month"))
  
  for (k in 1:length(whiplash.vars)){
    whiplash.var <- whiplash.vars[k]
    drought.var <- drought.vars[k]
    data.all[,"modifier"] <- data.all[,whiplash.var]
    data.all[,"condition"] <- data.all[,drought.var]
    
    data1 <- data.all %>%
      filter(condition %in% c("Mild drought", "Severe drought")) %>%
      filter(modifier != "wet to dry")
    
    data2 <- data.all %>%
      filter(condition %in% c("Mild drought", "Severe drought")) %>%
      filter(modifier == "wet to dry")
    
    # no whiplash
    model1 = feglm(as.formula(formula.i), 
                   data = data1,
                   offset = log(data1$population),
                   cluster = data1$COUNTY,
                   family = "quasipoisson")
    
    coef1 <- model1$coefficients[1]
    SE1 <- model1$se[1]
    
    # whiplash
    model2 = feglm(as.formula(formula.i), 
                   data = data2,
                   offset = log(data2$population),
                   cluster = data2$COUNTY,
                   family = "quasipoisson")
    
    coef2 <- model2$coefficients[1]
    SE2 <- model2$se[1]
    
    table <- data.frame(exposure = exposure.var,
                        condition = drought.var,
                        modifier = whiplash.var,
                        level = c("no wet to dry whiplash", "wet to dry"),
                        coef = c(coef1, coef2),
                        SE = c(SE1, SE2))
    
    result.all <- rbind(result.all, table)
  }
}