##Recycled predictions for MSK disorders

library(haven)
library(survey)

##reproducibility
n <- 1000
set.seed(123)
##load and adjust data
load("//sciensano.be/fs/1140_DATA/WAIST/VANESSA/Analysis/LBP paper/Costs/Output/tot_healthexp.RData")

str(dta)

hisdesign <-
  svydesign(id = ~hh_cluster,
            strata = ~provw,
            weights = ~wfin,
            data = dta)

hisdesign$variables$LBP <-
  relevel(hisdesign$variables$LBP, "No")

hisdesign$variables$NKP <-
  relevel(hisdesign$variables$NKP, "No")

hisdesign$variables$OA <-
  relevel(hisdesign$variables$OA, "No")

hisdesign$variables$RA <-
  relevel(hisdesign$variables$RA, "No")

results_OA <- array(dim = c(n, n, n))
#results_RA <- array(dim = c(n, n, n))

#for OA
for (a in 1:n) {
  dta_boot <-
    dta[sample(nrow(dta), replace = TRUE), ]
  
  ## create hisdesign
  hisdesign_boot <-
    svydesign(id = ~hh_cluster,
              strata = ~provw,
              weights = ~wfin,
              data = dta_boot)
  
  hisdesign_boot$variables$LBP <-
    relevel(hisdesign_boot$variables$LBP, "No")
  
  hisdesign_boot$variables$NKP <-
    relevel(hisdesign_boot$variables$NKP, "No")
  
  hisdesign_boot$variables$OA <-
    relevel(hisdesign_boot$variables$OA, "No")
  
  hisdesign_boot$variables$RA <-
    relevel(hisdesign_boot$variables$RA, "No")
  
  
  reg_LBP <- svyglm(LBP ~ OA + RA + hc04 + hc_01 + ET_1 +
                      relevel(PA08_2, "No") +
                      relevel(TA07_3, "No") +
                      relevel(NS_2, "Normal"),
                    design = hisdesign_boot, 
                    family = quasibinomial(link = "logit"))
  
  reg_NKP <- svyglm(NKP ~ OA + RA + LBP + hc04 + hc_01 + ET_1 +
                      relevel(PA08_2, "No") +
                      relevel(TA07_3, "No") +
                      relevel(NS_2, "Normal"),
                    design = hisdesign_boot, 
                    family = quasibinomial(link = "logit"))
  
  reg_disease <-
    svyglm(Av_Tot_cost ~ LBP + NKP + OA + RA + hc04 + hc_01 + ET_1 +
             relevel(PA08_2, "No") +
             relevel(TA07_3, "No") +
             relevel(NS_2, "Normal"), design = hisdesign_boot, 
           family = quasipoisson(link = "log"))
  
  for (i in 1:n) {
    #change OA to No  
    newdata <- within(dta_boot, {OA <- "No"})
    
    prob_LBP <- predict(reg_LBP, newdata = newdata, type = "response")
    dta_boot$prob_LBP <- NA
    dta_boot[names(prob_LBP), "prob_LBP"] <- prob_LBP
    dta_boot$pred_LBP <- rbinom(9841, 1, dta_boot$prob_LBP)
    
    dta_boot$pred_LBP <- ifelse(dta_boot$pred_LBP == 1, "Yes", "No")
    
    #change LBP to predictions
    newdata <- within(dta_boot, {LBP <- pred_LBP}) 
    prob_NKP <- predict(reg_NKP, newdata = newdata, type = "response")
    
    dta_boot$prob_NKP <- NA
    dta_boot[names(prob_NKP), "prob_NKP"] <- prob_NKP
    dta_boot$pred_NKP <- rbinom(9841, 1, dta_boot$prob_NKP)
    
    dta_boot$pred_NKP <- ifelse(dta_boot$pred_NKP == 1, "Yes", "No")
    
    #change NKP to predictions
    newdata <- within(dta_boot, {NKP <- pred_NKP}) 
    
    pred_costs <- predict(reg_disease, newdata = newdata, type = "response")
    dta_boot$pred_costs <- NA
    dta_boot[names(pred_costs), "pred_costs"] <- pred_costs
    
    dta_boot$fit_costs <- NA
    dta_boot[names(fitted.values(reg_disease)), "fit_costs"] <-
      fitted.values(reg_disease)
    
    dta_boot$ac_disease <-
      dta_boot$fit_costs - dta_boot$pred_costs   
    
    ## recreate hisdesign object
    
    hisdesign <-
      svydesign(id = ~hh_cluster,
                strata = ~provw,
                weights = ~wfin,
                data = dta_boot)
    
    ## calculate survey weighted mean
    res_boot <- svyby(~ac_disease, ~OA, hisdesign, svymean, na.rm = TRUE)
    
    ## generate 1000 random average values
    results_OA[i,a,] <- rnorm(n, res_boot["Yes", "ac_disease"], res_boot["Yes", "se"])
    
  }
  results_OA
  
}

c(mean = mean(results_OA), quantile(results_OA, probs = c(0.025, 0.975))) 
