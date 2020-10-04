## ----setup, include=FALSE, results='hide', message = FALSE, warning = FALSE, cache=FALSE----
  options(
    width = 70,
    digits = 4,
    datatable.print.nrows = 5)


## -------------------------------------------------------------------
library(data.table)
library(ggplot2)
library(visreg)
library(palmerpenguins)

library(JWileymisc)


## -------------------------------------------------------------------
acesData <- as.data.table(aces_daily)[SurveyDay == "2017-03-03" & SurveyInteger == 3]


## -------------------------------------------------------------------
penguinsData <- as.data.table(penguins)
mtcarsData <- as.data.table(mtcars, keep.rownames = TRUE)


## -------------------------------------------------------------------
summary(penguinsData[species == "Adelie"]$flipper_length_mm)


## -------------------------------------------------------------------
confidenceLevel <- 0.95
alpha <- 1 - confidenceLevel

beta <- 0.20
powerOfTest <- 1 - beta

#here we choose medium effect level.
cohensD <- 0.5

power.t.test(n = NULL ,
             delta = cohensD,
             sd = 1,
             sig.level = alpha,
             power = powerOfTest,
             type = "one.sample",
             alternative = "two.sided")


## -------------------------------------------------------------------
#here we choose small effect level.
cohensD <- 0.2

power.t.test(n = NULL ,
             delta = cohensD,
             sd = 1,
             sig.level = alpha,
             power = powerOfTest,
             type = "one.sample",
             alternative = "two.sided")


## -------------------------------------------------------------------
penguinsData[species == "Adelie",
             .N,
             by = island]

mean(penguinsData[species == "Adelie"]$flipper_length_mm, na.rm = TRUE)


## -------------------------------------------------------------------
t.test(x = penguinsData[species == "Adelie" & island == "Biscoe"]$flipper_length_mm,
       alternative = "two.sided",
       mu = 190,
       conf.level = 0.95)


## ----hyp_mtcarscorplot, warnings = FALSE, fig.width=5, fig.height=4, out.width='.6\\linewidth', fig.pos="!ht", fig.cap = c("Scatter plot of miles per gallon (mpg) and horsepower (hp) in the mtcars dataset.")----
ggplot(data = mtcarsData,
       mapping = aes(x = mpg,
                     y = hp)) +
  geom_point()


## -------------------------------------------------------------------
cor.test(x = mtcarsData$mpg,
         y = mtcarsData$hp,
         alternative = "less",
         method = "pearson",
         conf.level = 0.90)


## ----hyp_acescorplot, fig.width=5, fig.height=4, out.width='.6\\linewidth', fig.pos="!ht", fig.cap = c("Scatter plot of stress and negative affect (mood) in the ACES daily dataset.")----

ggplot(data = acesData,
       aes(x = STRESS,
           y = NegAff)) +
  geom_point()



## -------------------------------------------------------------------
acesData[, cor.test(x = STRESS,
               y = NegAff,
               alternative = "greater",
               method = "kendall",
               conf.level = 0.95
               )]

