#### Econometrics Project - 12/4/2015 #####
options(warn=-1)
suppressMessages(library(dplyr))
suppressMessages(library(magrittr))
suppressMessages(library(car))

# **** place the file path to the dataset here ****
file_path <- "~/Desktop/full_dataset_final.csv"

# Load, clean dataset ####
fdi <- read.csv(file_path, stringsAsFactors=FALSE)
fdi[,-1] <- apply(fdi[,-1], 2, as.numeric)
fdi %<>% rename(electricity = electricitiy)

# replaced education with mean for: Australia, Bahrain, Bosnia and Herzegovina,
# Brazil, France, United Kingdom, Singapore, New Zealand
# imputed development vars for: Bahamas (2) & Qatar (1)
fdi$Dev.DV[fdi$Country == 'Qatar'] <- 1
fdi$Dev.DV[fdi$Country == 'Bahamas, The'] <- 2
fdi %<>% group_by(Dev.DV) %>%
  summarise(edu = mean(education, na.rm = TRUE)) %>%
  inner_join(fdi, .) %>%
  mutate(education = ifelse(is.na(education), edu, education)) %>%
  select(-edu)

# save full file 
full <- fdi
fdi <- fdi[complete.cases(fdi),]

# make Dummy Variables factors
DVs <- which(grepl('DV$', colnames(fdi)))
fdi[,DVs] <- apply(fdi[,DVs], 2, as.factor)
rm(DVs)

# Model 1 (all variables - no logs) ####
fit1 <- lm(FDI.sum ~ tax + warehouse + tariff + broadband + education + electricity +
             corrupt + law + politic + REER.vol + REER.avg + REER.chg + resource +
             infl + GDP.avg + GDP.PPP.chg + GDP.pc.lvl.avg + Openness + Credit +
             Dev.DV + ER.DV + FDI.ctrl.DV, data = fdi)
summary(fit1)
summary(step(fit1, trace = F))

# Model 2 (Logs & Select variables) ####
# add DV for each individual levels Development and Exch rate DVs
fdi %<>% mutate(ER.float.DV = ER.DV == '3', ER.mngd.DV = ER.DV == '2',
                Dev2.DV = Dev.DV == '2', Dev3.DV = Dev.DV == '3')
fit2 <- lm(log(FDI.sum+1) ~ log(tax+1) + log(warehouse+1) + tariff + broadband +
             I((education > 100)*log(education)) + I((education <= 100)*education) +
             log(101 - electricity) + corrupt + law + politic + log(REER.vol+1) + REER.avg + REER.chg + resource +
             infl + GDP.avg + log(GDP.pc.lvl.avg+1) + log(Openness+1) + Credit +
             ER.float.DV + ER.mngd.DV + FDI.ctrl.DV, data = fdi)
summary(fit2)
summary(step(fit2, trace = F))

# paring down the stepwise ####
f <- 'log(FDI.sum+1) ~ log(warehouse + 1) + resource +
             I((education > 100)*log(education + 1)) +
             I((education <= 100)*education) +
             politic + ER.float.DV + gdp.growth.2002 +
             log(GDP.pc.lvl.2002 + 1) + log(Openness + 1)'
f <- as.formula(f)
fit3 <- lm(f, fdi)
summary(fit3)
summary(update(fit3, .~. -resource))
summary(update(fit3, .~. -resource -log(warehouse+1)))
summary(update(fit3, .~. -resource -log(warehouse+1) -I((education <= 100) * education)))

# go back to full dataset and get records where our explanatory variables are populated
# increases sample size significantly
full %<>% select(Country, FDI.sum, education, politic, ER.DV,
                 gdp.growth.2002, GDP.pc.lvl.2002, Openness) %>%
  mutate(ER.float.DV = ER.DV == 3) %>%
  select(-ER.DV) %>%
  filter(complete.cases(.))
mod <- lm(log(FDI.sum+1) ~ I((education > 100)*log(education + 1)) +
                  politic + ER.float.DV + gdp.growth.2002 +
                  log(GDP.pc.lvl.2002 + 1) + log(Openness + 1), full)
summary(mod)
# covariance matrix
options(digits = 2)
vcov(mod)
# anova table
anova(mod)

## Distribution of Residuals ####
## leaving out studentized
resid <- residuals(mod)
rs <- e1071::skewness(resid)

#histogram
hist(resid, col = 'grey', 10, main = 'Distribution of Residuals',
     xlab = paste('Skewness:', round(rs, 4)))
# qqPlot
qqPlot(mod)

# kernel density
d <- density(resid)
plot(d, main="Kernel Density of Residuals")
polygon(d, col="red", border="black")

# Tests ####

## Heteroskedasticity
white = function(model) {
  u <- residuals(model)^2
  yh <- fitted(model)
  ru2 <- summary(lm(u ~ yh + I(yh^2)))$r.squared
  l <- nrow(model$model)*ru2
  p <- 1-pchisq(l, length(coef(mod))-1)
  return(p)
}
white(mod) # White
lmtest::bptest(mod)$p.value # Breusch-Pagan
lmtest::gqtest(mod)$p.value # Goldfeld–Quandt

## Multi-Collinearity
vif(mod) # variance inflation factors
cor(mod$model)

## Model Specification (Ramsey Reset test)
lmtest::reset(mod)

## Endogeneity
mod_new <- update(mod, .~. -log(GDP.pc.lvl.avg+1) + log(GDP.pc.lvl.2002+1) )
r <- Matrix::rankMatrix(vcov(mod_new) - vcov(mod))[1]
# Hausman-Wu test
hw <- t(coef(mod) - coef(mod_new)) %*%
  MASS::ginv(vcov(mod_new) - vcov(mod)) %*%
  t(t(coef(mod) - coef(mod_new)))
1-pchisq(hw, r) # not finding endogeneity

# histograms, plots ####
suppressMessages(require(ggplot2))
suppressMessages(require(reshape2))
# histogram of explanatory variables (pre-tranformations)
full %>%
  select(-Country, -ER.float.DV) %>%
  melt(.) %>%
  ggplot(., aes(x = value)) + 
  facet_wrap(~variable, scales = "free") + 
  geom_histogram()  + 
  xlab("Variable Values") +
  ylab("Frequency") +
  ggtitle("Explanatory Variable Distributions")

# histogram of explanatory variables (post-transformation)
mod$model %>%
  select(-ER.float.DV) %>%
  melt(.) %>%
  filter(!(grepl('education', variable) & value == 0)) %>%
  ggplot(., aes(x = value, color = ifelse(grepl('log', variable), 'red', NA),
                fill = ifelse(grepl('log', variable), 'red', NA))) + 
  facet_wrap(~variable, scales = "free") + 
  geom_histogram()  + 
  xlab("Post Transformation Values") +
  ylab("Frequency") +
  ggtitle(expression(atop("Explanatory Variable Distributions", atop(italic("Post Transformation"), "")))) +
  theme(legend.position = 'none')

# plot of explanatory variables vs FDI (post-transformation)
mod$model %>%
  select(-ER.float.DV) %>%
  mutate(Country = full$Country) %>%
  melt(., id.vars = 'Country') %>%
  filter(!(grepl('education', variable) & value == 0)) %>%
  inner_join(., select(fdi, Country, FDI.sum)) %>%
  filter(variable != 'log(FDI.sum + 1)') %>%
  ggplot(., aes(x = value, y = log(FDI.sum +1))) +
  facet_wrap(~variable, scales = 'free') +
  geom_point() +
  geom_smooth(method = 'loess') + xlab("") +
  ggtitle(expression(atop("X vs FDI Scatter Plots", atop(italic("with Conditional Mean"), ""))))

# histogram of coefficients after repeated sampling
n <- nrow(full)
p <- 1 # size of sample
c <- replicate(5000, coef(update(mod, subset = sample(n, n*p, replace = TRUE))))
c <- as.data.frame(t(c))
c %>% melt(.) %>%
  ggplot(., aes(x = value)) +
  facet_wrap(~variable, scales = "free") + 
  geom_histogram() + xlab("") +
  ggtitle(expression(atop("Reapeated Sampling", atop(italic("Coefficient Distributions"), ""))))

# mean, stdev, skew of each beta distribution
c %>% melt(.) %>%
  group_by(variable) %>%
  summarise(beta_mean = round(mean(value), 3),
            beta_sd = round(sd(value), 3),
            beta_skew = round(e1071::skewness(value), 3)) %>%
  mutate(conf_int_95 = paste0('(', round(beta_mean - beta_sd*1.96, 2), ', ',
                              round(beta_mean + beta_sd*1.96, 2), ')')) %>%
  as.data.frame
