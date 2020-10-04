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


## ----cr_mtcarscorplot, warnings = FALSE, fig.width=5, fig.height=4, out.width='.6\\linewidth', fig.pos="!ht", fig.cap = c("Scatter plot of miles per gallon (mpg) and horsepower (hp) in the mtcars dataset.")----

ggplot(data = mtcarsData,
       mapping = aes(x = mpg,
                     y = hp)) +
  geom_point()


## ----cr_acescorplot, fig.width=5, fig.height=4, out.width='.6\\linewidth', fig.pos="!ht", fig.cap = c("Scatter plot of stress and negative affect (mood) in the ACES daily dataset.")----

ggplot(data = acesData,
       aes(x = STRESS,
           y = NegAff)) +
  geom_point()


## -------------------------------------------------------------------
cor(x = mtcarsData$mpg,
    y = mtcarsData$hp)


## -------------------------------------------------------------------
acesData[ ,
          cor(x = STRESS,
              y = NegAff,
              use = "pair")]


## -------------------------------------------------------------------
cubicData <- data.table(x = -100:100)
cubicData[, y := x^3]


## ----cr_cubecorplot, fig.width=5, fig.height=4, out.width='.6\\linewidth', fig.pos="!ht", fig.cap = c("Scatter plot of stress and negative affect (mood) in the ACES daily dataset.")----
ggplot(data = cubicData,
       mapping = aes(x = x,
                     y = y)) +
  geom_point()


## -------------------------------------------------------------------
cubicData[,
          cor(x = x,
              y = y)]


## -------------------------------------------------------------------
cor(x = mtcarsData$mpg,
    y = mtcarsData$hp,
    method = "spearman")


## -------------------------------------------------------------------
acesData[, cor(x = STRESS,
               y = NegAff,
               use = "pair",
               method = "spearman")]


## -------------------------------------------------------------------
toy <- data.frame(toy = c("rabbit", "bear", "dog"),
                ReviewerA = c(1, 2, 3),
                ReviewerB = c(1, 3, 2))

toy


## -------------------------------------------------------------------
cor(x = toy$ReviewerA,
    y = toy$ReviewerB,
    method = "kendall")


## -------------------------------------------------------------------
cor(x = mtcarsData$mpg,
    y = mtcarsData$hp,
    method = "kendall")


## -------------------------------------------------------------------
acesData[, cor(x = STRESS,
               y = NegAff,
               use = "pair",
               method = "kendall")]



## ----cr_regex1, echo = FALSE, fig.width = 5, fig.height = 4, out.width='.6\\linewidth', fig.pos="!ht", fig.cap = "Straight line graphed from a simple, linear regression model."----

int <- .50

ggplot() +
  geom_hline(yintercept = int, linetype = 3) +
  geom_vline(xintercept = 0, linetype = 3) +   
  geom_abline(intercept = int, slope = 0.5) +
  scale_x_continuous("x (predictor/explanatory variable)",
                     breaks = c(-3, -2, -1, 0, 1, 2, 3)) +
  scale_y_continuous("y (outcome variable)",
                     breaks = c(-1.5, -1, -0.5, 0, 0.5, 1, 1.5)) +  
  coord_cartesian(xlim = c(-3, 3),
                  ylim = c(-1.5, 1.5)) +
  theme_classic() +
  annotate("point", x = 0, y = int, colour = "black") +  
  annotate("text", x = -.1, y = .1 + int, label = "b[0]", parse = TRUE) + 
  annotate("segment", x = 0, xend = 1, y = int, yend = int, colour = "blue") +
  annotate("segment", x = 1, xend = 1, y = int, yend = int + 0.5, colour = "blue") +    
  annotate("text", x = 1.1, y = int + 0.25, label = "b[1]", parse = TRUE, colour = "blue")



## ----cr_regex2, echo = FALSE, fig.width = 5, fig.height = 4, out.width='.6\\linewidth', fig.pos="!ht", fig.cap = "Straight line graphed from a simple, linear regression model with another intercept value."----

int <- -.50

ggplot() +
  geom_hline(yintercept = int, linetype = 3) +
  geom_vline(xintercept = 0, linetype = 3) +   
  geom_abline(intercept = int, slope = 0.5) +
  scale_x_continuous("x (predictor/explanatory variable)",
                     breaks = c(-3, -2, -1, 0, 1, 2, 3)) +
  scale_y_continuous("y (outcome variable)",
                     breaks = c(-1.5, -1, -0.5, 0, 0.5, 1, 1.5)) +  
  coord_cartesian(xlim = c(-3, 3),
                  ylim = c(-1.5, 1.5)) +
  theme_classic() +
  annotate("point", x = 0, y = int, colour = "black") +  
  annotate("text", x = -.1, y = .1 + int, label = "b[0]", parse = TRUE) + 
  annotate("segment", x = 0, xend = 1, y = int, yend = int, colour = "blue") +
  annotate("segment", x = 1, xend = 1, y = int, yend = int + 0.5, colour = "blue") +    
  annotate("text", x = 1.1, y = int + 0.25, label = "b[1]", parse = TRUE, colour = "blue")



## ----cr_regex3, echo = FALSE, fig.width = 5, fig.height = 4, out.width='.6\\linewidth', fig.pos="!ht", fig.cap = "Example image of a simple, linear regression model with a different slope."----

int <- .50

ggplot() +
  geom_hline(yintercept = int, linetype = 3) +
  geom_vline(xintercept = 0, linetype = 3) +   
  geom_abline(intercept = int, slope = -0.5) +
  scale_x_continuous("x (predictor/explanatory variable)",
                     breaks = c(-3, -2, -1, 0, 1, 2, 3)) +
  scale_y_continuous("y (outcome variable)",
                     breaks = c(-1.5, -1, -0.5, 0, 0.5, 1, 1.5)) +  
  coord_cartesian(xlim = c(-3, 3),
                  ylim = c(-1.5, 1.5)) +
  theme_classic() +
  annotate("point", x = 0, y = int, colour = "black") +  
  annotate("text", x = -.1, y = .1 + int, label = "b[0]", parse = TRUE) + 
  annotate("segment", x = 0, xend = 1, y = int, yend = int, colour = "blue") +
  annotate("segment", x = 1, xend = 1, y = int, yend = int - 0.5, colour = "blue") +    
  annotate("text", x = 1.1, y = int - 0.25, label = "b[1]", parse = TRUE, colour = "blue")



## ----cr_regres, echo = FALSE, fig.width = 5, fig.height = 4, out.width='.6\\linewidth', fig.pos="!ht", fig.cap = "Example showing data points (black points), linear regression line (black line) and residuals (blue, dashed lines)."----

m <- lm(hp ~ mpg, data = mtcarsData)
mtcarsData$yhat <- predict(m)

ggplot(mtcarsData, aes(x = mpg, y = hp)) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, colour = "black") +
  geom_point() +  
  geom_segment(aes(xend = mpg, yend = yhat), colour = "blue", linetype = 2) +
  theme_classic() +
  ggtitle("mtcarsData")



## ----cr_linearity, echo = FALSE, fig.width = 6, fig.height = 3, out.width='.9\\linewidth', fig.pos="!ht", fig.cap = "Example showing a very non-linear association (Panel A) and an approximately linear association (Panel B) between a predictor, x, and outcome, y. Linear regression lines are shown in the blue lines."----

set.seed(12345)
d <- data.frame(
  x = runif(100, min = -2, max = +2))
d$y1 <- rnorm(100, mean = d$x^2, sd = .5)
d$y2 <- rnorm(100, mean = d$x + .25 * (d$x^2), sd = .5)

ggpubr::ggarrange(
ggplot(d, aes(x, y1)) +
  geom_point() + theme_classic() +
stat_smooth(
  method = "lm", se = FALSE, size = 1,
  formula = y ~ x) + 
  xlab("x - predictor") + ylab("y - outcome") +
  ggtitle("A. Non-Linear"),
ggplot(d, aes(x, y2)) +
  geom_point() + theme_classic() +
stat_smooth(
  method = "lm", se = FALSE, size = 1,
  formula = y ~ x) + 
  xlab("x - predictor") + ylab("y - outcome") +
ggtitle("B. About Linear"),
ncol = 2)



## ----cr_scedasticity, echo = FALSE, fig.width = 6, fig.height = 4, out.width='1\\linewidth', fig.pos="!ht", fig.cap = "Example showing inconsistent residual variance, heteroscedasiticty, (Panel A) and concsistent or equal residual variance, homoscedasticity, (Panel B)."----

set.seed(12345)
d <- data.frame(
  x = runif(100, min = -2, max = +2))
d$y1 <- rnorm(100, mean = d$x, sd = d$x + abs(min(d$x)))
d$y2 <- rnorm(100, mean = d$x, sd = mean(d$x + abs(min(d$x))))

ggpubr::ggarrange(
ggplot(d, aes(x, y1)) +
  geom_point() + theme_classic() +
stat_smooth(
  method = "lm", se = FALSE, size = 1,
  formula = y ~ x) + 
  xlab("x - predictor") + ylab("y - outcome") +
  ggtitle("A. Heteroscedasticity"),
ggplot(d, aes(x, y2)) +
  geom_point() + theme_classic() +
stat_smooth(
  method = "lm", se = FALSE, size = 1,
  formula = y ~ x) + 
  xlab("x - predictor") + ylab("y - outcome") +
ggtitle("B. Homoscedasticity"),
ncol = 2)



## ----cr_penguinFBsp, fig.width = 5, fig.height = 5, out.width='.6\\linewidth', fig.pos="!ht", fig.cap = "Penguins Data. Flipper lengths versus body mass."----
ggplot(data = penguinsData,
       mapping = aes(x = flipper_length_mm,
                     y = body_mass_g)) +
  geom_point()


## -------------------------------------------------------------------
mPenguin <- lm(formula = body_mass_g ~ flipper_length_mm,
               data = penguinsData)


## ----cr_penguinFBtest, fig.width = 10, fig.height = 5, out.width='1\\linewidth', fig.pos="!ht", fig.cap = "Penguins Data test for normalicy of residuals and homoscedasticity."----
mPenguinTests <- modelDiagnostics(mPenguin)

plot(mPenguinTests,
     ncol = 2)


## -------------------------------------------------------------------
penguinsData[order(island, species),
             .N,
             by = .(island, species)]


## -------------------------------------------------------------------
summary(mPenguin)


## -------------------------------------------------------------------
range(penguinsData$flipper_length_mm,
      na.rm = TRUE)

xiPenguin <- 201
yiPenguin <- -5780.83 + (49.69*xiPenguin)
yiPenguin


## ----cr_penguinFBplot, fig.width = 5, fig.height = 5, out.width='.6\\linewidth', fig.pos="!ht", fig.cap = "Linear regression visual for penguinData."----
visreg(fit = mPenguin,
       band = FALSE)


## ----cr_acesSNsp, fig.width = 5, fig.height = 5, out.width='.6\\linewidth', fig.pos="!ht", fig.cap = "Aces Data. Stress versus Negative Affect."----
ggplot(data = acesData,
       mapping = aes(x = STRESS,
                     y = NegAff)) +
  geom_point()


## -------------------------------------------------------------------
mAces <- lm(formula = NegAff ~ STRESS,
               data = acesData)


## ----cr_acesSNtest, fig.width = 6, fig.height = 4, out.width='1\\linewidth', fig.pos="!ht", fig.cap = "Aces one day Data test for normalcy of residuals and homoscedasticity."----
mAcesTests <- modelDiagnostics(mAces)

plot(mAcesTests,
     ncol = 2)


## -------------------------------------------------------------------
summary(mAces)


## ----cr_acesSNplot, fig.width = 5, fig.height = 5, out.width='.6\\linewidth', fig.pos="!ht", fig.cap = "Linear regression visual for aces Data."----
visreg(fit = mAces,
       band = FALSE)

