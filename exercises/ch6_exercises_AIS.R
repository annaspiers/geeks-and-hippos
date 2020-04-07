# Gelman & Hill Ch. 6 exercises
# 1-3, 8, 9, 11

library(foreign) #read.dta
library(arm)
library(dplyr)
library(AER) #dispersiontest

### 1 ###
rb <- read.dta('data/risky_behaviors.dta')
head(rb)
rb <- rb %>%
    mutate(treat = ifelse(couples == 1, "couples", ifelse(women_alone == 1, "women_alone", "control")) ) %>%
    select(-couples, -women_alone)
# what do the different column variables mean??

# a
fit1 <- glm(formula = fupacts ~ treat, data=rb, family=poisson)
display(fit1)
plot(fit1$fitted.values , fit1$residuals)
# yes, there is overdispersion

# b
fit2 <- glm(formula = fupacts ~ sex + bs_hiv + bupacts + treat, data=rb, family=poisson)
display(fit2)
plot(fit2$fitted.values , fit2$residuals)
# yes, there is overdispersion

# c (per top of p.115)
n <- 434
k <- 6
yhat <- predict(fit2, type="response")
z <- (rb$fupacts-yhat)/(yhat)
cat("overdispersion ratio is ", sum(z^2)/(n-k), "\n")
cat("p-value of overdispersion test is ", pchisq(sum(z^2), n-k), "\n")
# that p-value can't be right

dispersiontest(fit2, trafo=1)

# c #
fit.overdisp <- glm(formula = fupacts ~ sex + bs_hiv + bupacts+ treat, data=rb, family=quasipoisson) #used bupacts as baseline exposure
display(fit.overdisp)
# Since both treatment coefficients are negative and significant, we find that the treatments ahd a negative impact on the number of unprotdcted sex acts

# d #
# Our modeling assumptions aren't equal because we don't have a "male only" treatment group
# See Gianluca's answer... I don't get it


### 2 ###
nes_df <- read.dta("data/nes5200_processed_voters_realideo.dta")
head(nes_df)

# thin df to only those with party ID
nes_df_party <- nes_df %>%
    filter(!is.na(partyid7)) %>% 
    filter(partyid7 =! "0. dk/ na/ other/ refused to answer") %>% 
    filter(partyid7 =! "9. apolitical (1966 only: and dk)") 
    
    
    filter(partyid7 %in% c("6. weak republican", "5. independent-republican", "4. independent-independent",  "7. strong republican", "3. independent-democrat", "2. weak democrat", "1. strong democrat"))
#c("6. weak republican", "5. independent-republican", "4. independent-independent",  "7. strong republican", "3. independent-democrat", "2. weak democrat", "1. strong democrat")

# a #
# predict party ID (5pt scale) using idology and demographics

multinom <- polr(formula = partyid7 ~ ideo7 + age + gender + race + income + religion , data = nes_df_party, Hess=TRUE)
display(multinom)
summary(multinom)

# ?????
    
# 3
fit.logit <- glm(vote ~ income, data = nes_df, family = binomial(link="logit")) #copied from p.79
display(fit.logit)

fit.probit <- glm(vote ~ income, data = nes_df, family = binomial(link="probit")) 
display(fit.probit)

# probit coefficients should be logit coefficients/1.6
fit.probit$coefficients
fit.logit$coefficients/1.6
# What's the point of these?

### 8 ###
nes_df_RD <- nes_df %>%
    filter(partyid3_b %in% c("1. democrats (including leaners)", "3. republicans (including leaners)"))
fit.partychoice <- glm(formula = partyid3_b ~ ideo + black, data = nes_df_RD, family=binomial(link="logit"))
display(fit.partychoice)
# how to interpret this in choice context?

### 9 ####
nes_df_RDna <- nes_df %>%
    filter(partyid3_b %in% c("0. dk/ na/ other/ refused to answer/ no", "1. democrats (including leaners)", "3. republicans (including leaners)"))
multinom.choice <- polr(formula = partyid3_b ~ ideo + black , data = nes_df_RDna, Hess=TRUE)
display(multinom.choice)
# how to interpret this in choice context?

# 11