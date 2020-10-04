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
str(acesData)


## -------------------------------------------------------------------
penguinsData <- as.data.table(penguins)
mtcarsData <- as.data.table(mtcars, keep.rownames = TRUE)


## -------------------------------------------------------------------
set.seed(1234)
muP <- 2.5
sigmaP <- 0.75
perfectData <- data.table(Data = rnorm(n = 10000,
                                       mean = muP,
                                       sd = sigmaP),
                          Group = 1:100)


## -------------------------------------------------------------------
perfectData[, mean(Data),
            by = Group]


## -------------------------------------------------------------------
perfectData[,
            .(groupMean = mean(Data)),
            by = Group]


## -------------------------------------------------------------------
perfectMeans <- perfectData[,
                            .(groupMean = mean(Data)),
                            by = Group]


## ----ci_perfectSamples, fig.width=5, fig.height=4, out.width='.6\\linewidth',fig.pos="!ht", fig.cap = c("sample means with a vertical line for the population mean, mu = 2.5")----
ggplot(data = perfectMeans,
       mapping = aes(x = groupMean, y = Group))+
  geom_point()+
  geom_vline(xintercept = muP)


## -------------------------------------------------------------------
standardErrorP <- sigmaP / sqrt(100)


## -------------------------------------------------------------------
ConfidenceLevel <- 0.95
alpha <- 1 - ConfidenceLevel

qnorm(alpha/2)


## -------------------------------------------------------------------
round(qnorm(alpha/2), 2)


## -------------------------------------------------------------------
round(qnorm(alpha/2,
            lower.tail = FALSE),
      2)


## -------------------------------------------------------------------
ConfidenceLevel <- 0.99
alpha <- 1 - ConfidenceLevel

qnorm(alpha/2,
      lower.tail = FALSE)


## -------------------------------------------------------------------
ebmP <- 1.96*standardErrorP
ebmP


## -------------------------------------------------------------------
perfectMeans[,
            lowerEBM := groupMean - ebmP]

perfectMeans[,
            upperEBM := groupMean + ebmP]


## ----ci_perfectEBM, fig.width=5, fig.height=4, out.width='.6\\linewidth',fig.pos="!ht", fig.cap = c("sample means with confidence intervals")----
ggplot(data = perfectMeans,
       mapping = aes(x = groupMean, y = Group)) +
  geom_point() +
  geom_vline(xintercept = muP) +
  geom_segment( mapping = aes( x = lowerEBM, xend = upperEBM,
                               y = Group, yend = Group),
                alpha = 0.4)


## -------------------------------------------------------------------
perfectMeans[muP < lowerEBM|
               muP > upperEBM]


## -------------------------------------------------------------------
tTestResults <- t.test(perfectData[Group == 1]$Data,
                       conf.level = ConfidenceLevel)

tTestResults


## ----ci_perfectEBMT, echo=FALSE, fig.width=7, fig.height=6, out.width='.8\\linewidth',fig.pos="!ht", fig.cap = c("sample means with confidence intervals. solid lines are z-values and blue, dashed lines are t-values")----

perfectMeansT <- perfectData[,
                             .(
                               tTestMean = t.test(Data,
                                                  conf.level = ConfidenceLevel)$estimate,
                               tTestlower = t.test(Data,
                                                   conf.level = ConfidenceLevel)$conf.int[1],
                               tTestupper = t.test(Data,
                                                   conf.level = ConfidenceLevel)$conf.int[2]),
                             by = Group]

comp <- merge(perfectMeans, perfectMeansT,
      by = "Group")

ggplot(data = comp,
       mapping = aes(x = groupMean, y = Group)) +
  geom_point() +
  geom_vline(xintercept = muP) +
  geom_segment( mapping = aes( x = lowerEBM, xend = upperEBM,
                               y = Group, yend = Group),
                alpha = 0.3, linetype = 1, size = 1.2) +
  geom_segment( mapping = aes( x = tTestlower, xend = tTestupper,
                               y = Group, yend = Group),
                alpha = 0.6, linetype = 3, color = "blue")



## -------------------------------------------------------------------
t.test(penguinsData$flipper_length_mm,
       conf.level = 0.98)


## -------------------------------------------------------------------
mPenguin <- lm(formula = body_mass_g ~ flipper_length_mm,
               data = penguinsData)

summary(mPenguin)


## ----ci_penguinFBplot, fig.width = 5, fig.height = 5, out.width='.7\\linewidth', fig.pos="!ht", fig.cap = "Linear regression visual for penguinData with confidence interval."----
visreg(fit = mPenguin)


## -------------------------------------------------------------------
range(penguinsData$flipper_length_mm,
      na.rm = TRUE)


## -------------------------------------------------------------------
t.test(penguinsData[species == "Adelie"]$flipper_length_mm,
       conf.level = 0.95)

t.test(penguinsData[species == "Chinstrap"]$flipper_length_mm,
       conf.level = 0.95)


## -------------------------------------------------------------------
range(penguinsData[species == "Adelie"]$flipper_length_mm,
      na.rm = TRUE)

range(penguinsData[species == "Chinstrap"]$flipper_length_mm,
      na.rm = TRUE)


## -------------------------------------------------------------------
t.test(acesData[BornAUS == 0]$Age)

t.test(acesData[BornAUS == 1]$Age)

