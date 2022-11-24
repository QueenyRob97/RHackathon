library(boot)
reg.fun <- function(dta, design){
  reg_LBP <- svyglm(Av_Tot_cost ~ OA ,
                  design = design)
  
  # reg_NKP <- svyglm(NKP ~ OA + RA + LBP + hc04 + hc_01 + ET_1 +
  #                     relevel(PA08_2, "No") +
  #                     relevel(TA07_3, "No") +
  #                     relevel(NS_2, "Normal"),
  #                   design = design, 
  #                   family = quasibinomial(link = "logit"))
  # 
  # reg_disease <-
  #   svyglm(Av_Tot_cost ~ LBP + NKP + OA + RA + hc04 + hc_01 + ET_1 +
  #            relevel(PA08_2, "No") +
  #            relevel(TA07_3, "No") +
  #            relevel(NS_2, "Normal"), design = design, 
  #          family = quasipoisson(link = "log"))
  # 

  newdata <- within(dta, {OA <- "No"})

  prob_LBP <- predict(reg_LBP, newdata = newdata, type = "response")
  dta$prob_LBP <- NA
  dta[names(prob_LBP), "prob_LBP"] <- prob_LBP
  dta$pred_LBP <- rbinom(9841, 1, dta$prob_LBP)

  dta$pred_LBP <- ifelse(dta$pred_LBP == 1, 1, 0)

  # #change LBP to predictions
  # newdata <- within(dta, {LBP <- pred_LBP}) 
  # prob_NKP <- predict(reg_NKP, newdata = newdata, type = "response")
  # 
  # dta$prob_NKP <- NA
  # dta[names(prob_NKP), "prob_NKP"] <- prob_NKP
  # dta$pred_NKP <- rbinom(9841, 1, dta$prob_NKP)
  # 
  # dta$pred_NKP <- ifelse(dta$pred_NKP == 1, "Yes", "No")
  # 
  # #change NKP to predictions
  # newdata <- within(dta, {NKP <- pred_NKP}) 
  # 
  # pred_costs <- predict(reg_disease, newdata = newdata, type = "response")
  # dta$pred_costs <- NA
  # dta[names(pred_costs), "pred_costs"] <- pred_costs
  # 
  # dta$fit_costs <- NA
  # dta[names(fitted.values(reg_disease)), "fit_costs"] <-
  #   fitted.values(reg_disease)
  # 
  # dta$ac_disease <-
  #   dta$fit_costs - dta$pred_costs  
}

nuke.boot <- boot(subdata, reg.fun, R = 5)



# The bootstrap prediction squared error would then be found by
mean(nuke.boot$t[, 8]^2)
# Basic bootstrap prediction limits would be
new.fit - sort(nuke.boot$t[, 8])[c(975, 25)]