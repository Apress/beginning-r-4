## ----setup, include=FALSE, results='hide', message = FALSE, warning = FALSE, cache=FALSE----
  options(
    width = 70,
    digits = 4,
    datatable.print.nrows = 5)


## -------------------------------------------------------------------
library(data.table)
library(ggplot2)
library(palmerpenguins)
library(visreg)
library(emmeans)

library(JWileymisc)


## -------------------------------------------------------------------
acesData <- as.data.table(aces_daily)[SurveyDay == "2017-03-03" & SurveyInteger == 3]


## -------------------------------------------------------------------
range(acesData$STRESS, na.rm = TRUE)


## -------------------------------------------------------------------
acesData[, StressCat := factor(fifelse(STRESS >= 2,
                                       "2+",
                                       as.character(STRESS)))]


## -------------------------------------------------------------------
str(acesData[, .(UserID, STRESS, StressCat)])


## -------------------------------------------------------------------
unique(acesData$StressCat)


## -------------------------------------------------------------------
penguinsData <- as.data.table(penguins)
str(penguinsData)


## -------------------------------------------------------------------
m <- lm(formula = PosAff ~ STRESS,
        data = acesData)

summary(m)


## ----include=FALSE, echo=FALSE, results='hide'----------------------
## store some results to make it easier.
sm <- summary(m)
csm <- coef(sm)
cim <- confint(m)


## -------------------------------------------------------------------
confint(m)


## -------------------------------------------------------------------
qt(p = .025, df = 185)

qt(p = .975, df = 185)


## ----echo=FALSE, include=FALSE--------------------------------------
m1summary <- sprintf(
"There %s a statistically significant association between stress and
positive affect. A one unit higher stress score was associated with
a %0.2f %s positive affect score [95\\%% CI %0.2f to %0.2f], %s.
For someone with a stress score of zero, positive affect was expected to
be %0.2f [%0.2f, %0.2f], %s (the intercept).
Overall, stress explained %0.1f\\%% of the variance in positive affect.
",
fifelse(csm["STRESS", "Pr(>|t|)"] < .05, "was", "was not"),
round(csm["STRESS", "Estimate"], 2),
fifelse(csm["STRESS", "Estimate"] < 0, "lower", "higher"),
round(cim["STRESS", "2.5 %"], 2),
round(cim["STRESS", "97.5 %"], 2),
formatPval(csm["STRESS", "Pr(>|t|)"], d = 3, sd = 3, includeP = TRUE),
round(csm["(Intercept)", "Estimate"], 2),
round(cim["(Intercept)", "2.5 %"], 2),
round(cim["(Intercept)", "97.5 %"], 2),
formatPval(csm["(Intercept)", "Pr(>|t|)"], d = 3, sd = 3, includeP = TRUE),
round(sm$adj.r.squared * 100, 1))
m1summary <- gsub("p < .001", "$p < .001$", m1summary)


## ----mr_pastress1, warnings = FALSE, fig.width=5, fig.height=4, out.width='.6\\linewidth', fig.pos="!ht", fig.cap = c("Visualizing a regression model of positive affect predicted by stress using the visreg() function.")----
visreg(m, xvar = "STRESS", gg = TRUE)


## ----mr_pastress2, warnings = FALSE, fig.width=5, fig.height=4, out.width='.6\\linewidth', fig.pos="!ht", fig.cap = c("Customizing the regression model visualization from visreg().")----

visreg(m, xvar = "STRESS", partial = FALSE, rug = FALSE, gg = TRUE) +
  ggtitle("Linear regression of Positive Affect on Stress",
          subtitle = "Shaded region shows 95% confidence interval.")



## ----mr_pastress3, warnings = FALSE, fig.width=5, fig.height=4, out.width='.6\\linewidth', fig.pos="!ht", fig.cap = c("Adding annotations to show the slope in regression model visualization.")----

visreg(m, xvar = "STRESS", partial = FALSE, rug = FALSE, gg = TRUE) +
   ggtitle("Linear regression of Positive Affect on Stress",
          subtitle = "Shaded region shows 95% confidence interval.")+
  annotate("text", x = 6, y = 3, label = "b = -0.14, p < .001", size = 5) +
  xlab("Perceived Stress") + ylab("Positive Affect (Mood)")



## -------------------------------------------------------------------
m2 <- lm(formula = PosAff ~ STRESS + SUPPORT,
         data = acesData)


## ----mr_pasupstress, warning = FALSE, message = FALSE, fig.width=6, fig.height=4, out.width='1\\linewidth', fig.pos="!ht", fig.cap = c("Multiple regression model diagnostics.")----
m2d <- modelDiagnostics(m2, ev.perc = .005)
plot(m2d, ncol = 2, ask = FALSE)


## -------------------------------------------------------------------
summary(m2)

confint(m2)


## ----include=FALSE, echo=FALSE, results='hide'----------------------
## store some results to make it easier.
sm2 <- summary(m2)
csm2 <- coef(sm2)
cim2 <- confint(m2)


## ----mr_3d, echo = FALSE, warning = FALSE, fig.width=4, fig.height=4, out.width='.49\\linewidth', fig.pos="!ht", fig.cap = c("3D graph of a multiple regression surface."), fig.show="hold", fig.align = "center"----

Stress <- seq(from = min(acesData$STRESS, na.rm=TRUE), to = max(acesData$STRESS, na.rm=TRUE),
                length.out = 20)
Support <- seq(from = min(acesData$SUPPORT, na.rm=TRUE), to = max(acesData$SUPPORT, na.rm=TRUE),
                length.out = 20)
PositiveAffect <- expand.grid(STRESS = Stress, SUPPORT = Support)
PositiveAffect$PositiveAffect <- predict(m2, newdata = PositiveAffect)
PositiveAffect <- as.matrix(
  reshape(PositiveAffect, v.names = "PositiveAffect", timevar = "SUPPORT",
          idvar = "STRESS", direction = "wide")[, -1])

palette("R4")
par(mar = c(.5, .5, .5, .5))
persp(Stress, Support, PositiveAffect,
      col = colorRampPalette(palette()[2:4])(19),
      theta = 0, phi = 15)

persp(Stress, Support, PositiveAffect,
      col = colorRampPalette(palette()[2:4])(19),
      theta = -60, phi = 30)



## ----mr_pasupstress1, warnings = FALSE, fig.width=6, fig.height=4, out.width='.8\\linewidth', fig.pos="!ht", fig.cap = c("Multiple regression of stress and social support predicting positive affect. This figure shows only the unique stress and positive affect association.")----

visreg(m2, xvar = "STRESS", partial = FALSE, rug = FALSE, gg = TRUE) +
  ggtitle("Association of stress and positive affect, adjusted for social support")



## ----mr_pasupstress2, warnings = FALSE, fig.width=6, fig.height=4, out.width='.8\\linewidth', fig.pos="!ht", fig.cap = c("Multiple regression of stress and social support predicting positive affect. This figure shows only the unique social support and positive affect association.")----

visreg(m2, xvar = "SUPPORT", partial = FALSE, rug = FALSE, gg = TRUE) +
  ggtitle("Association of support and positive affect, adjusted for stress")



## ----echo=FALSE, include=FALSE--------------------------------------
m2summary <- sprintf(
"A multiple linear regression model with positive affect as the outcome
and stress and social support as predictors.
There %s a statistically significant association between stress and
positive affect, controlling for social support (see \\figref{mr_pasupstress1}).
Independent of social support, a one unit higher stress score was associated with
a %0.2f %s positive affect score [95\\%% CI %0.2f to %0.2f], %s.
Likewise, there %s a statistically significant association between social support and
positive affect, controlling for stress (see \\figref{mr_pasupstress2}).
Independent of stress, a one unit higher social support score was associated with
a %0.2f %s positive affect score [95\\%% CI %0.2f to %0.2f], %s.
For someone with a stress and social support scores of zero, positive affect was expected to
be %0.2f [%0.2f, %0.2f], %s (the intercept).
Overall, stress and social support explained %0.1f\\%% of the variance in positive affect.
",
fifelse(csm2["STRESS", "Pr(>|t|)"] < .05, "was", "was not"),
round(csm2["STRESS", "Estimate"], 2),
fifelse(csm2["STRESS", "Estimate"] < 0, "lower", "higher"),
round(cim2["STRESS", "2.5 %"], 2),
round(cim2["STRESS", "97.5 %"], 2),
formatPval(csm2["STRESS", "Pr(>|t|)"], d = 3, sd = 3, includeP = TRUE),

fifelse(csm2["SUPPORT", "Pr(>|t|)"] < .05, "was", "was not"),
round(csm2["SUPPORT", "Estimate"], 2),
fifelse(csm2["SUPPORT", "Estimate"] < 0, "lower", "higher"),
round(cim2["SUPPORT", "2.5 %"], 2),
round(cim2["SUPPORT", "97.5 %"], 2),
formatPval(csm2["SUPPORT", "Pr(>|t|)"], d = 3, sd = 3, includeP = TRUE),

round(csm2["(Intercept)", "Estimate"], 2),
round(cim2["(Intercept)", "2.5 %"], 2),
round(cim2["(Intercept)", "97.5 %"], 2),
formatPval(csm2["(Intercept)", "Pr(>|t|)"], d = 3, sd = 3, includeP = TRUE),
round(sm2$adj.r.squared * 100, 1))

m2summary <- gsub("<", "\\\\textless", m2summary)


## -------------------------------------------------------------------

model_a <- lm(PosAff ~ SUPPORT, data = acesData)

R2(m2) - R2(model_a)



## -------------------------------------------------------------------
(R2(m2) - R2(model_a)) / (1 - R2(m2))


## -------------------------------------------------------------------
m2test <- modelTest(m2)
APAStyler(m2test)


## -------------------------------------------------------------------
print(APAStyler(m2test), nrow = 100)


## -------------------------------------------------------------------
m2Penguin <- lm(formula = body_mass_g ~ flipper_length_mm + bill_depth_mm,
                data = penguinsData)


## ----mr_bmflipbill1, warning = FALSE, message = FALSE, fig.width=6, fig.height=4, out.width='1\\linewidth', fig.pos="!ht", fig.cap = c("Multiple regression model diagnostics for penguins.")----
m2Penguind <- modelDiagnostics(m2Penguin, ev.perc = 0.005)
plot(m2Penguind, ncol = 2, ask = FALSE)


## -------------------------------------------------------------------
summary(m2Penguin)


## ----include=FALSE, echo=FALSE, results='hide'----------------------
## store some results to make it easier.
sm2Penguin <- summary(m2Penguin)
csm2Penguin <- coef(sm2Penguin)
cim2Penguin <- confint(m2Penguin)


## ----mr_3dPenguin, echo = FALSE, warning = FALSE, fig.width=4, fig.height=4, out.width='.49\\linewidth', fig.pos="!ht", fig.cap = c("3D graph of a multiple regression surface on penguins."), fig.show="hold", fig.align = "center"----

flipper <- seq(from = min(penguinsData$flipper_length_mm, na.rm=TRUE), to = max(penguinsData$flipper_length_mm, na.rm=TRUE),
                length.out = 20)
bill <- seq(from = min(penguinsData$bill_depth_mm, na.rm=TRUE), to = max(penguinsData$bill_depth_mm, na.rm=TRUE),
                length.out = 20)
bodyMass <- expand.grid(flipper_length_mm = flipper, bill_depth_mm = bill)
bodyMass$bodyMass <- predict(m2Penguin, newdata = bodyMass)
bodyMass <- as.matrix(
  reshape(bodyMass, v.names = "bodyMass", timevar = "bill_depth_mm",
          idvar = "flipper_length_mm", direction = "wide")[, -1])

palette("R4")
par(mar = c(.5, .5, .5, .5))

persp(flipper, bill, bodyMass,
      col = colorRampPalette(palette()[2:4])(19),
      theta = 30, phi = -5)

persp(flipper, bill, bodyMass,
      col = colorRampPalette(palette()[2:4])(19),
      theta = -60, phi = 30)



## ----mr_bmflipbill2, warnings = FALSE, fig.width=6, fig.height=4, out.width='1\\linewidth', fig.pos="!ht", fig.cap = c("Multiple regression of flipper length and bill depth predicting penguin body mass. This figure shows only the unique bill depth and body mass association.")----
visreg(m2Penguin, xvar = "bill_depth_mm", partial = FALSE, rug = FALSE, gg = TRUE) +
  ggtitle("Association of bill depth and body mass, adjusted for flipper length")


## ----mr_bmflipbill3, warnings = FALSE, fig.width=6, fig.height=4, out.width='1\\linewidth', fig.pos="!ht", fig.cap = c("Multiple regression of flipper length and bill depth predicting penguin body mass. This figure shows only the unique flipper length and body mass association.")----

visreg(m2Penguin, xvar = "flipper_length_mm", partial = FALSE, rug = FALSE, gg = TRUE) +
  ggtitle("Association of flipper length and body mass, adjusted for bill depth")



## -------------------------------------------------------------------
m2PenguinTest <- modelTest(m2Penguin)
print(APAStyler(m2PenguinTest), nrow = 100)



## -------------------------------------------------------------------
acesData[, cor(STRESS, SUPPORT, use = "pairwise.complete.obs")]


## ----mr_pastressd, warning = FALSE, message = FALSE, fig.width=6, fig.height=4, out.width='1\\linewidth', fig.pos="!ht", fig.cap = c("Regression model diagnostics. Left panel shows a plot of the residuals to assess normality and they will be highlighted black if an extreme value. The right panel shows a plot of the predicted versus residual values to help assess the homogeneity of variance assumption.")----

## assess model diagnostics
m2d <- modelDiagnostics(m2, ev.perc = .005)
plot(m2d, ncol = 2, ask = FALSE)


## ----mr_nastressd, warning = FALSE, message = FALSE, fig.width=6, fig.height=4, out.width='1\\linewidth', fig.pos="!ht", fig.cap = c("Regression model diagnostics. Left panel shows a plot of the residuals to assess normality and they will be highlighted black if an extreme value. The right panel shows a plot of the predicted versus residual values to help assess the homogeneity of variance assumption.")----

malt <- lm(NegAff ~ STRESS + SUPPORT, data = acesData)

## assess model diagnostics
maltd <- modelDiagnostics(malt, ev.perc = .005)
plot(maltd, ncol = 2, ask = FALSE)



## ----mr_lnastressd, warning = FALSE, message = FALSE, fig.width=6, fig.height=4, out.width='1\\linewidth', fig.pos="!ht", fig.cap = c("Regression model diagnostics after log transforming the outcome.")----

malt2 <- lm(log(NegAff) ~ STRESS + SUPPORT, data = acesData)

## assess model diagnostics
malt2d <- modelDiagnostics(malt2, ev.perc = .005)
plot(malt2d, ncol = 2, ask = FALSE)



## ----mr_lnastressd2, warning = FALSE, message = FALSE, fig.width=6, fig.height=4, out.width='1\\linewidth', fig.pos="!ht", fig.cap = c("Regression model diagnostics after log transforming the outcome and removing extreme values.")----

malt2d$extremeValues

malt3 <- lm(formula = log(NegAff) ~ STRESS,
            data = acesData[-malt2d$extremeValues$Index])

## assess model diagnostics
malt3d <- modelDiagnostics(malt3, ev.perc = .005)
plot(malt3d, ncol = 2, ask = FALSE)



## ----echo = FALSE---------------------------------------------------

tmpdat <- mtcars
tmpdat$dummy_cyl <- factor(tmpdat$cyl)

head(model.matrix(~ 0 + cyl + dummy_cyl, data = tmpdat))



## ----mr_paeducat, warning = FALSE, message = FALSE, fig.width=6, fig.height=4, out.width='1\\linewidth', fig.pos="!ht", fig.cap = c("Simple linear regression plot from visreg with a categorical predictor, education, showing the predicted positive affect and 95\\% confidence intervals for each education level.")----

acesData[, EDU := factor(EDU)]

mcat1 <- lm(formula = PosAff ~ EDU,
            data = acesData)

APAStyler(modelTest(mcat1))

visreg(mcat1, xvar = "EDU", partial = FALSE, rug = FALSE, gg = TRUE) +
  ggtitle("EDU as a categorical predictor")



## -------------------------------------------------------------------
t.test(PosAff ~ EDU,
       data = acesData,
       var.equal = TRUE)


## ----mr_pastresscat, warning = FALSE, message = FALSE, fig.width=6, fig.height=4, out.width='1\\linewidth', fig.pos="!ht", fig.cap = c("Regression diagnostic plot when there is only a categorical predictor.")----

mcat2 <- lm(formula = PosAff ~ StressCat,
            data = acesData)

plot(modelDiagnostics(mcat2), ncol = 2)



## -------------------------------------------------------------------
APAStyler(modelTest(mcat2))


## -------------------------------------------------------------------
mcat2.means <- emmeans(object = mcat2, specs = "StressCat")
mcat2.means



## -------------------------------------------------------------------
pairs(mcat2.means)


## ----mr_pastresscat3, warning = FALSE, message = FALSE, fig.width=6, fig.height=4, out.width='1\\linewidth', fig.pos="!ht", fig.cap = c("Regression diagnostic plot when there is a categorical predictor and a continuous predictor.")----

mcat3 <- lm(formula = PosAff ~ StressCat + SUPPORT,
            data = acesData)

plot(modelDiagnostics(mcat3), ncol = 2)



## ----mr_pastresscat3visreg, warning = FALSE, message = FALSE, fig.width=6, fig.height=4, out.width='1\\linewidth', fig.pos="!ht", fig.cap = c("Simple linear regression plot from visreg with a categorical predictor, education, showing the predicted positive affect and 95\\% confidence intervals for each education level.")----
visreg(mcat3, xvar = "SUPPORT", by = "StressCat",
       partial = FALSE, rug = FALSE, overlay = TRUE,
       gg = TRUE) +
  ggtitle("SUPPORT and StressCat as predictors")



## -------------------------------------------------------------------
APAStyler(modelTest(mcat3))


## -------------------------------------------------------------------
mcat3.means <- emmeans(object = mcat3,
                       specs = "StressCat")
mcat3.means

pairs(mcat3.means)


## -------------------------------------------------------------------
summary(m2Penguin)


## ----echo=TRUE, eval=FALSE, results="hide", message=FALSE, warning=FALSE----
## m3Penguind <- modelDiagnostics(m3Penguin, ev.perc = 0.005)
## plot(m3Penguind, ncol = 2, ask = FALSE)


## ----echo=TRUE, eval=FALSE, results="hide", message=FALSE, warning=FALSE----
## visreg(m3Penguin, xvar = "sex", partial = FALSE, rug = FALSE, gg = TRUE) +
##   ggtitle("Association of sex and body mass, adjusted for flipper length")


## ----echo=TRUE, eval=FALSE, results="hide", message=FALSE, warning=FALSE----
## m3PenguinTest <- modelTest(m3Penguin)
## print(APAStyler(m3PenguinTest), nrow = 100)

