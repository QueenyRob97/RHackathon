library(boot)
library(dplyr)
load("tot_healthexp.RData")

n <- 5
n_boot <- 6
subdata <- dta[, c("hh_cluster", "provw", "wfin", "LBP", "NKP", "OA", "RA",
                   "hc04", "hc_01", "ET_1", "PA08_2", "TA07_3", "NS_2", "Av_Tot_cost")]

indices <- c("hh_cluster", "provw", "wfin", "LBP", "NKP", "OA", "RA",
             "hc04", "hc_01", "ET_1", "PA08_2", "TA07_3", "NS_2", "Av_Tot_cost")

bloc_len <- nrow(subdata)

boot_dta <- bind_rows(replicate(n, subdata %>% sample(), simplify=F))

boot_dta$id <- 
  rep(seq(1, 1 + nrow(boot_dta) %/% bloc_len), each = bloc_len, length.out = nrow(boot_dta))

start_time <- Sys.time()
reg.fun <- function(formula, data, design){
  
  ## create hisdesign
  hisdesign_boot <-
    svydesign(id = ~hh_cluster,
              strata = ~provw,
              weights = ~wfin,
              data = sp[[1]])

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


  newdata <- within(data, {OA <- "No"})

  prob_LBP <- predict(reg_LBP, newdata = newdata, type = "response")
  data$prob_LBP <- NA
  data[names(prob_LBP), "prob_LBP"] <- prob_LBP
  data$pred_LBP <- rbinom(9841, 1, data$prob_LBP)

  data$pred_LBP <- ifelse(data$pred_LBP == 1, "Yes", "No")

# #change LBP to predictions
newdata <- within(data, {LBP <- pred_LBP})
prob_NKP <- predict(reg_NKP, newdata = newdata, type = "response")

data$prob_NKP <- NA
data[names(prob_NKP), "prob_NKP"] <- prob_NKP
data$pred_NKP <- rbinom(9841, 1, data$prob_NKP)

data$pred_NKP <- ifelse(data$pred_NKP == 1, "Yes", "No")

#change NKP to predictions
newdata <- within(data, {NKP <- pred_NKP})

pred_costs <- predict(reg_disease, newdata = newdata, type = "response")
data$pred_costs <- NA
data[names(pred_costs), "pred_costs"] <- pred_costs

data$fit_costs <- NA
data[names(fitted.values(reg_disease)), "fit_costs"] <-
  fitted.values(reg_disease)

data$ac_disease <-
  data$fit_costs - data$pred_costs
  
}

results <- by(boot_dta, boot_dta[,"id"], summary)
              #, function(x) boot(boot_dta, reg.fun, R = n))

boot_dta %>%
  group_by(id) %>%
  purrr::map_dfr(
    function(x){
      boot(x, reg.fun, R = n)
    })

sp <- split(boot_dta, boot_dta$id)
results <- lapply(sp[[1]], function(x){
  boot(x, reg.fun, R = n_boot)
})


nuke.boot <- boot(sp[[1]], reg.fun, R = n)


end_time <- Sys.time()
end_time - start_time


# The bootstrap prediction squared error would then be found by
mean(nuke.boot$t[, 8]^2)
# Basic bootstrap prediction limits would be
new.fit - sort(nuke.boot$t[, 8])[c(975, 25)]
