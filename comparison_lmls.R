# comparison lmls with hmc_location_scale_reg
#source("R/HMC_sim.R")
#source("R/one_function_model.R")
#source("R/hmc_location_scale_reg.R")
#source("R/methods.R")
#source("R/helper_functions.R")

#asp24hmc::hmc_simple_reg()
library(lmls)
#load_all()
par(mfrow = c(2,1))

lmls_result <- lmls(y ~ I(x) + I(x^2), scale = ~x ,data = abdom, light = F)
ci_unten <- lmls_result$fitted.values$location - 1.96 * lmls_result$fitted.values$scale
ci_oben <- lmls_result$fitted.values$location + 1.96 * lmls_result$fitted.values$scale
plot(abdom$x, abdom$y, col = "darkgrey", pch = 19, main =  "lmls")
lines(abdom$x, lmls_result$fitted.values$location, col = "darkblue", lwd = 2)
lines(abdom$x, ci_unten, col = "blue", lty = 2, lwd = 1.3)
lines(abdom$x, ci_oben, col = "blue", lty = 2, lwd = 1.3)
sum(lmls_result$residuals)
lmls_result$coefficients$location

hmc_result <- aps24hmc::hmc_simple_reg(y ~ I(x) + I(x^2), ~x, data = abdom, stepsize = 0.001, chain_length = 500, burn_in = 0 , thin = 1)
sum(hmc_result$residuals)
cat("Coefficients LMLS:", lmls_result$coefficients$location, "\n")
cat("Coefficients HMC Reg:",hmc_result$coefficients$location, "\n")
ci_unten <- hmc_result$fitted.values$location - 1.96 * hmc_result$fitted.values$scale
ci_oben <- hmc_result$fitted.values$location + 1.96 * hmc_result$fitted.values$scale
plot(abdom$x, abdom$y, col = "darkgrey", pch = 19, main = "hmc")
lines(abdom$x, hmc_result$fitted.values$location, col = "darkblue", lwd = 2)
lines(abdom$x, ci_unten, col = "blue", lty = 2, lwd = 1.3)
lines(abdom$x, ci_oben, col = "blue", lty = 2, lwd = 1.3)
sum(hmc_result$fitted.values$scale)
sum(lmls_result$fitted.values$scale)
summary(hmc_result)
