library(boot)
library(dplyr)
load("tot_healthexp.RData")

n <- 100
subdata <- dta[, c("hh_cluster", "provw", "wfin", "LBP", "NKP", "OA", "RA",
                   "hc04", "hc_01", "ET_1", "PA08_2", "TA07_3", "NS_2", "Av_Tot_cost")]

sampled.data <- sample(subdata, 10, replace=TRUE)
dta_boot <-
  dta[sample(nrow(dta), replace = TRUE), ]


start_time <- Sys.time()
reg.fun <- function(formula, data, design){

  #dta_boot <-
    #dta[sample(nrow(dta), replace = TRUE), ]

  ## create hisdesign
  hisdesign_boot <-
    svydesign(id = ~hh_cluster,
              strata = ~provw,
              weights = ~wfin,
              data = subdata)

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


  newdata <- within(subdata, {OA <- "No"})

  prob_LBP <- predict(reg_LBP, newdata = newdata, type = "response")
  subdata$prob_LBP <- NA
  subdata[names(prob_LBP), "prob_LBP"] <- prob_LBP
  subdata$pred_LBP <- rbinom(9841, 1, subdata$prob_LBP)

  subdata$pred_LBP <- ifelse(subdata$pred_LBP == 1, "Yes", "No")

# #change LBP to predictions
newdata <- within(subdata, {LBP <- pred_LBP})
prob_NKP <- predict(reg_NKP, newdata = newdata, type = "response")

subdata$prob_NKP <- NA
subdata[names(prob_NKP), "prob_NKP"] <- prob_NKP
subdata$pred_NKP <- rbinom(9841, 1, subdata$prob_NKP)

subdata$pred_NKP <- ifelse(subdata$pred_NKP == 1, "Yes", "No")

#change NKP to predictions
newdata <- within(subdata, {NKP <- pred_NKP})

pred_costs <- predict(reg_disease, newdata = newdata, type = "response")
subdata$pred_costs <- NA
subdata[names(pred_costs), "pred_costs"] <- pred_costs

subdata$fit_costs <- NA
subdata[names(fitted.values(reg_disease)), "fit_costs"] <-
  fitted.values(reg_disease)

subdata$ac_disease <-
  subdata$fit_costs - subdata$pred_costs
}

nuke.boot <- boot(subdata, reg.fun, R = 100)
end_time <- Sys.time()
end_time - start_time


# The bootstrap prediction squared error would then be found by
mean(nuke.boot$t[, 8]^2)
# Basic bootstrap prediction limits would be
new.fit - sort(nuke.boot$t[, 8])[c(975, 25)]
