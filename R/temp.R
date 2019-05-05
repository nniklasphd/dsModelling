# # formula <- as.formula(paste0("Surv(", survival_time, " + ", survival_event, ") ~ ", terms))
# # result  <- coxph(formula, data = dataTable)
# 
# formulatext     <- Reduce(paste, deparse(formula))
# originalFormula <- formulatext
# formula2use     <- as.formula(paste0(Reduce(paste, deparse(originalFormula)))) # here we need the formula as a 'call' object
# 
# #result <- coxph(Surv(survival_time, survival_event) ~ terms, data = dataTable)
# result <- coxph(formula2use, data = dataTable)
# #result <- coxph(survival.object ~ survival.object, data = dataTable)
# 
# #formula <- as.formula(paste0("Surv(", st, ",", se, ") ~ ", terms))
# #result <- coxph(formula, data = dataTable)