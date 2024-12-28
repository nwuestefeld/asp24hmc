library(asp24hmc)
library(lmls)
source("R/methods.R")
source("R/helper_methods.R")
data("abdom")

model <- GLSR_HMC(y ~ I(x) + I(x^2), ~x+I(x^2), data = abdom , NULL, NULL, chain_length = 5000, burn_in = 0, thin = 1)
a <-asp24hmc::GLSR_HMC(y ~ I(x) + I(x^2), ~x, data = abdom ,stepsize = 1e-04 , trajectory_length = 12, chain_length = 5000, burn_in = 0, thin = 20)
summary(a)
plot(a)

# test asp24hmc on iris data

data(iris)
model <- asp24hmc::GLSR_HMC(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, ~Sepal.Width, data = iris , 0.001, 20, chain_length = 10000, burn_in = 0, thin = 20)

summary(model)
plot(model)

b <- lmls::lmls(y ~ I(x) + I(x^2),  ~x, data = abdom, light = T)
summary(b)
plot(b)
par(mfrow = c(2,1))
ci_unten <- b$fitted.values$location - 1.96 * b$fitted.values$scale
ci_oben <- b$fitted.values$location + 1.96 * b$fitted.values$scale
plot(abdom$x, abdom$y, col = "darkgrey", pch = 19, main =  "lmls")
lines(abdom$x, b$fitted.values$location, col = "darkblue", lwd = 2)
lines(abdom$x, ci_unten, col = "blue", lty = 2, lwd = 1.3)
lines(abdom$x, ci_oben, col = "blue", lty = 2, lwd = 1.3)

ci_unten <- a$fitted$location - 1.96 * a$fitted$scale
ci_oben <- a$fitted$location + 1.96 * a$fitted$scale
plot(abdom$x, abdom$y, col = "darkgrey", pch = 19, main = "hmc")
lines(abdom$x, a$fitted$location, col = "darkblue", lwd = 2)
lines(abdom$x, ci_unten, col = "blue", lty = 2, lwd = 1.3)
lines(abdom$x, ci_oben, col = "blue", lty = 2, lwd = 1.3)





newdata <- data.frame(x = c(1, 2, 3, 4, 5))
predict(a, newdata)
predict(b, newdata)

set.seed(123)

rnorm(3)

