## ----setup, include=FALSE, results='hide', message = FALSE, warning = FALSE, cache=FALSE----
  options(
    width = 70,
    digits = 4,
    datatable.print.nrows = 5)


## -------------------------------------------------------------------
library(data.table)
library(ggplot2)
library(visreg)
library(emmeans)
library(palmerpenguins)

library(JWileymisc)


## -------------------------------------------------------------------
data(aces_daily)
acesData <- as.data.table(aces_daily)[SurveyDay == "2017-03-03" & SurveyInteger == 3]
acesData[, StressCat := factor(fifelse(STRESS >= 2,
                                       "2+",
                                       as.character(STRESS)))]

penguinsData <- as.data.table(penguins)


## -------------------------------------------------------------------
mPmul <- lm(formula = body_mass_g ~ flipper_length_mm + bill_depth_mm,
         data = penguinsData)


## -------------------------------------------------------------------
summary(mPmul)


## -------------------------------------------------------------------
mPmod <- lm(formula = body_mass_g ~ flipper_length_mm * bill_depth_mm,
         data = penguinsData)


## -------------------------------------------------------------------
summary(mPmod)


## ----modReg_contcat, warning = FALSE, message = FALSE, fig.width=6, fig.height=4, out.width='1\\linewidth', fig.pos="!ht", fig.cap = c("Moderated regression with an interaction between stress category and social support predicting positive affect. Lines are predictions in each group with 95\\% confidence intervals.")----
mint1 <- lm(formula = PosAff ~ StressCat * SUPPORT,
            data = acesData)

visreg(mint1, xvar = "SUPPORT", by = "StressCat",
       partial = FALSE, rug = FALSE, overlay = TRUE,
       gg = TRUE) +
  ggtitle("SUPPORT and StressCat as predictors")


## -------------------------------------------------------------------
mint1Test <- modelTest(mint1)
print(APAStyler(mint1Test), nrow = 100)


## -------------------------------------------------------------------
print(APAStyler(mint1Test), nrow = 100)


## -------------------------------------------------------------------
print(APAStyler(modelTest(mint1),
  pcontrol = list(digits = 3, stars = FALSE, includeP = TRUE,
                  includeSign = TRUE, dropLeadingZero = TRUE)),
  nrow = 100)


## -------------------------------------------------------------------
m1int.slopes <- emtrends(
  object = mint1,
  specs = "StressCat",
  var = "SUPPORT")

summary(m1int.slopes, infer = TRUE)


## -------------------------------------------------------------------
## calculate M - 1 SD, M, and M + 1 SD

m1.MeanSDlow <- mean(acesData$SUPPORT, na.rm = TRUE) -
                    sd(acesData$SUPPORT, na.rm = TRUE)
  
m1.MeanSD <- mean(acesData$SUPPORT, na.rm = TRUE)


m1.MeanSDhigh <- mean(acesData$SUPPORT, na.rm = TRUE) +
                    sd(acesData$SUPPORT, na.rm = TRUE)


## -------------------------------------------------------------------
m1int.means <- emmeans(
  object = mint1,
  specs = "StressCat",
  by = "SUPPORT",
  at = list(SUPPORT = c(m1.MeanSDlow, m1.MeanSD, m1.MeanSDhigh)))



## -------------------------------------------------------------------
pairs(m1int.means)


## ----modReg_int1, warning = FALSE, message = FALSE, fig.width=6, fig.height=4, out.width='1\\linewidth', fig.pos="!ht", fig.cap = c("Regression diagnostic plot for a moderated regression model.")----

mint1d <- modelDiagnostics(mint1)

plot(mint1d, ncol = 2, ask = FALSE)


## -------------------------------------------------------------------
mint2 <- lm(formula = PosAff ~ STRESS * Age,
            data = acesData)


## ----modReg_3d, echo = FALSE, warnings = FALSE, fig.width=4, fig.height=4, out.width='.49\\linewidth', fig.pos="!ht", fig.cap = c("3D graph of a moderated multiple regression surface."), fig.show="hold", fig.align = "center"----

Stress <- seq(from = min(acesData$STRESS, na.rm=TRUE), to = max(acesData$STRESS, na.rm=TRUE),
                length.out = 20)
Age <- seq(from = min(acesData$Age, na.rm=TRUE), to = max(acesData$Age, na.rm=TRUE),
                length.out = 20)
PositiveAffect <- expand.grid(STRESS = Stress, Age = Age)

PositiveAffect$PositiveAffect <- predict(mint2, newdata = PositiveAffect)
PositiveAffect <- as.matrix(
  reshape(PositiveAffect, v.names = "PositiveAffect", timevar = "Age",
          idvar = "STRESS", direction = "wide")[, -1])

palette("R4")
par(mar = c(.5, .5, .5, .5))
persp(Stress, Age, PositiveAffect,
      col = colorRampPalette(palette()[2:4])(19),
      theta = 60, phi = 35)
persp(Stress, Age, PositiveAffect,
      col = colorRampPalette(palette()[2:4])(19),
      theta = -60, phi = 50)



## -------------------------------------------------------------------
mint2Test <- modelTest(mint2)
APAStyler(mint2Test)


## -------------------------------------------------------------------
range(acesData$Age,
      na.rm = TRUE)

summary(acesData$Age)


## -------------------------------------------------------------------
acesData[, Agec22 := Age - 22]


## -------------------------------------------------------------------
mint2b <- lm(formula = PosAff ~ STRESS * Agec22,
             data = acesData)

mint2bTest <- modelTest(mint2b)
APAStyler(mint2bTest)


## -------------------------------------------------------------------
m2.MeanSDlow <- mean(acesData$STRESS, na.rm = TRUE) - 
                  sd(acesData$STRESS, na.rm = TRUE)
  
m2.MeanSD <- mean(acesData$STRESS, na.rm = TRUE)

m2.MeanSDhigh <- mean(acesData$STRESS, na.rm = TRUE) +
                  sd(acesData$STRESS, na.rm = TRUE)
  
m2.MeanSDlow <- 0


## -------------------------------------------------------------------
m2int.slopes <- emtrends(
  object = mint2,
  specs = "STRESS",
  var = "Age",
  at = list(STRESS = c(m2.MeanSDlow,
                       m2.MeanSD,
                       m2.MeanSDhigh)))
  
summary(m2int.slopes, infer = TRUE)


## -------------------------------------------------------------------
m2.MeanSDAgelow <- round(mean(acesData$Age, na.rm = TRUE) -
  sd(acesData$Age, na.rm = TRUE), 0)

m2.MeanSDAge <- round(mean(acesData$Age, na.rm = TRUE), 0)

m2.MeanSDAgehigh <- round(mean(acesData$Age, na.rm = TRUE) +
  sd(acesData$Age, na.rm = TRUE), 0)


## -------------------------------------------------------------------
m2intAge.slopes <- emtrends(
  object = mint2,
  specs = "Age",
  var = "STRESS",
  at = list(Age = c(m2.MeanSDAgelow,
                    m2.MeanSDAge,
                    m2.MeanSDAgehigh))
)

summary(m2intAge.slopes,
        infer = TRUE)


## ----modReg_contcont1, warning = FALSE, message = FALSE, fig.width=6, fig.height=4, out.width='1\\linewidth', fig.pos="!ht", fig.cap = c("Moderated regression with an interaction between stress  and age predicting positive affect. Lines are predictions at M - 1 SD, Mean, and M + 1 SD of age with 95\\% confidence intervals.")----

visreg(mint2, xvar = "STRESS", by = "Age",
       breaks = c(m2.MeanSDAgelow, m2.MeanSDAge, m2.MeanSDAgehigh),
       partial = FALSE, rug = FALSE, overlay = TRUE,
       gg = TRUE) +
  ggtitle("STRESS and Age as predictors")



## ----modReg_contcont1b, warning = FALSE, message = FALSE, fig.width=6, fig.height=4, out.width='1\\linewidth', fig.pos="!ht", fig.cap = c("Moderated regression with an interaction between stress and age predicting positive affect. Lines are predictions at M - 1 SD and M + 1 SD of age with 95\\% confidence intervals.")----

visreg(mint2, xvar = "STRESS", by = "Age",
       breaks = c(19, 24),
       partial = FALSE, rug = FALSE, overlay = TRUE,
       gg = TRUE) +
  ggtitle("Stress x Age interaction on Positive Affect") +
  annotate("text", x = 5, y = 3, label = "Age M - 1 SD (19): b = -.05, p = .302") +
  annotate("text", x = 5, y = 1.6, label = "Age M + 1 SD (24): b = -.21, p < .001") +
  ylab("Positive Affect")



## ----modReg_contcont2, warning = FALSE, message = FALSE, fig.width=6, fig.height=4, out.width='1\\linewidth', fig.pos="!ht", fig.cap = c("Moderated regression with an interaction between stress and age predicting positive affect. Lines are predictions at M - 1 SD, Mean, and M + 1 SD of stress with 95\\% confidence intervals.")----

visreg(mint2, xvar = "Age", by = "STRESS",
       breaks = c(m2.MeanSDlow,
                  m2.MeanSD,
                  m2.MeanSDhigh),
       partial = FALSE, rug = FALSE, overlay = TRUE,
       gg = TRUE) +
  ggtitle("STRESS and Age as predictors")



## -------------------------------------------------------------------
mPmod <- lm(formula = body_mass_g ~ flipper_length_mm * bill_depth_mm,
             data = penguinsData)

summary(mPmod)


## ----echo=TRUE, eval=FALSE, results="hide", message=FALSE, warning=FALSE----
## #exercise 3
## mPmod.MeanSDlowBill <- mean(penguinsData$ , na.rm = TRUE) -
##                   sd(penguinsData$ , na.rm = TRUE)
## 
## mPmod.MeanSDBill <- mean(penguinsData$ , na.rm = TRUE)
## 
## mPmod.MeanSDhighBill <- mean(penguinsData$ , na.rm = TRUE) +
##                   sd(penguinsData$ , na.rm = TRUE)


## ----include=FALSE, results='hide', message = FALSE, warning = FALSE, cache=FALSE----
#exercise 3
mPmod.MeanSDlowBill <- mean(penguinsData$bill_depth_mm , na.rm = TRUE) - 
                  sd(penguinsData$bill_depth_mm , na.rm = TRUE)
  
mPmod.MeanSDBill <- mean(penguinsData$bill_depth_mm , na.rm = TRUE)

mPmod.MeanSDhighBill <- mean(penguinsData$bill_depth_mm , na.rm = TRUE) +
                  sd(penguinsData$bill_depth_mm , na.rm = TRUE)


## -------------------------------------------------------------------
mPmod.slopes <- emtrends(
  object = mPmod,
  specs = "bill_depth_mm",
  var = "flipper_length_mm",
  at = list(bill_depth_mm = c(mPmod.MeanSDlowBill,
                       mPmod.MeanSDBill,
                       mPmod.MeanSDhighBill)))
  
summary(mPmod.slopes, infer = TRUE)


## ----modReg_exercise, warning = FALSE, message = FALSE, fig.width=6, fig.height=4, out.width='1\\linewidth', fig.pos="!ht", fig.cap = c("Moderated regression with an interaction between flipper length and bill depth predicting body mass. Lines are predictions at M - 1 SD, Mean, and M + 1 SD of bill depth with 95\\% confidence intervals.")----
visreg(
  mPmod,
  xvar = "flipper_length_mm",
  by = "bill_depth_mm",
  breaks = c(mPmod.MeanSDlowBill,
             mPmod.MeanSDBill,
             mPmod.MeanSDhighBill),
  partial = FALSE,
  rug = FALSE,
  overlay = TRUE,
  gg = TRUE
) +
  ggtitle("flipper_length_mm and bill_depth_mm as predictors")

