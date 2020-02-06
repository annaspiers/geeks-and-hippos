# 6 Feb 2020
# Anna Spiers
# Ch3 exercises 1-3

library(arm)

### 1 ###
dat <- read.table("data/pyth/exercise2.1.dat", header = TRUE)

# a # 
fit.1 <- lm(y ~ x1 + x2 , dat[1:40,]) # Predict y from x1, x2 using first 40 data points
display(fit.1) # Summarize the inferences
# AIS - how did you Check fit of the model? Anything other than just plotting the line with the data?

# b #
beta.hat <- coef(fit.1)
plot (dat[1:40,]$x1, dat[1:40,]$y, xlab="x", ylab="y", col="red", pch=20)
points(dat[1:40,]$x2, dat[1:40,]$y, col="blue", pch=20)
#curve (coef(fit.1)[1] + coef(fit.1)[2]*x + coef(fit.1)[3]*x, add=TRUE)
curve( cbind(1, x, mean(dat[1:40,]$x2)) %*% beta.hat, add=TRUE)

# c # Residual plot
residual.plot(fitted(fit.1), residuals(fit.1), sigma.hat(fit.1))

# d #
predict.dat <- data.frame(predict(fit.1, dat[41:60,], interval="prediction", level=0.95))
plot (dat[1:40,]$x1, dat[1:40,]$y, xlab="x", ylab="y", col="black", pch=20)
points(dat[1:40,]$x2, dat[1:40,]$y, col="darkgray", pch=20)
points(dat[41:60,]$x2, predict.dat$fit, col="red", pch=20)
#curve (coef(fit.1)[1] + coef(fit.1)[2]*x + coef(fit.1)[3]*x, add=TRUE)
curve( cbind(1, x, mean(dat[1:40,]$x2)) %*% beta.hat, add=TRUE)
# AIS Hmm, that doesn't look right



### 2 ###
# predict y=log earnings from x=log height... ln(y) = a + b*ln(x)
# 66" makes $30,000
# every 1% increase in height corresponds to predicted increase of .8% earning... 1.008*ln(y) is prop to 1.01
# earnings of 95% of ppl fall within a factor of 1.1 of predicted values - AIS what does that mean?

# a #
# ln(y) = 10.31726 + ln(1.008/1.01)*ln(x)

# b #
# R^2  = 0.09159696



### 3 ###
var1 <- rnorm(1000)
var2 <- rnorm(1000)

# a #
fit.2 <- lm(var1 ~ var2)
display(fit.2)
# If a coefficient estimate is > 2se away from 0, then it is statistically significant
# Since the slope coefficient is 0.01 and its se is 0.03, we determine that the slope coefficient is NOT stat sig

# b #
z.scores <- rep (NA, 100) 
for(k in 1:100){
  var1 <- rnorm (1000, 0, 1) #changed from (1000, 0.1)
  var2 <- rnorm (1000, 0, 1)
  lm3.3 <- lm (var2 ~ var1)
  z.scores[k] <- coef(lm3.3)[2] / se.coef(lm3.3)[2] 
}
sum ( abs( z.scores ) > 2)
# How many of these 100 z-scores are statistically significant? Ans: 7

