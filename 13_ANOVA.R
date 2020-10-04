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
library(ez)

library(JWileymisc)


## -------------------------------------------------------------------
acesData <- as.data.table(aces_daily)[SurveyDay == "2017-03-03" & SurveyInteger == 3]
acesData[, StressCat := factor(fifelse(STRESS >= 2,
                                       "2+",
                                       as.character(STRESS)))]

penguinsData <- as.data.table(penguins)


## ----ANOVA_ss, echo = FALSE, warning = FALSE, message = FALSE, fig.width=5, fig.height=10, out.width='.6\\linewidth', fig.pos="!ht", fig.cap = c("Visual explanation of the different kinds of sums of squares in an ANOVA.")----

d <- data.table(
  ID = 1:15,
  y = c(1:5, 8:12, 21:25),
  group = rep(LETTERS[1:3], each = 5))
d[, ybar := mean(y)]
d[, ybarj := mean(y), by = group]

cowplot::plot_grid(
ggplot(d, aes(ID, y, colour = group)) +
  geom_hline(yintercept = d$ybar[1]) +
  geom_segment(aes(x = ID, xend = ID, y = y, yend = ybar, colour = NA)) +
  geom_point(size = 2) +
  scale_x_continuous("Participant", breaks = 1:15) +
  theme_classic() +
  theme(legend.position = c(.15, .85)) +
  ggtitle("A. Sum of Squares Total (SST)"),
ggplot(d, aes(ID, y, colour = group)) +
  geom_hline(yintercept = d$ybar[1]) +
  geom_segment(aes(x = 1, xend = 5, y = 3, yend = 3, colour = "A")) + 
  geom_segment(aes(x = 6, xend = 10, y = 10, yend = 10, colour = "B")) + 
  geom_segment(aes(x = 11, xend = 15, y = 23, yend = 23, colour = "C")) +
  geom_segment(aes(x = ID, xend = ID, y = ybar, yend = ybarj, colour = NA)) +
  geom_point(size = 2) +
  scale_x_continuous("Participant", breaks = 1:15) +
  theme_classic() +
  theme(legend.position = c(.15, .85)) +
  ggtitle("B. Sum of Squares Model (SSM)"),
ggplot(d, aes(ID, y, colour = group)) +
  geom_segment(aes(x = 1, xend = 5, y = 3, yend = 3, colour = "A")) + 
  geom_segment(aes(x = 6, xend = 10, y = 10, yend = 10, colour = "B")) + 
  geom_segment(aes(x = 11, xend = 15, y = 23, yend = 23, colour = "C")) +
  geom_segment(aes(x = ID, xend = ID, y = y, yend = ybarj, colour = NA)) +
  geom_point(size = 2) +
  scale_x_continuous("Participant", breaks = 1:15) +
  theme_classic() +
  theme(legend.position = c(.15, .85)) +
ggtitle("C. Sum of Squares Residual (SSR)"),
ncol = 1)



## -------------------------------------------------------------------
groups <- LETTERS[1:4]
combn(groups,
      m = 2)


## -------------------------------------------------------------------
aovdata <- acesData[!is.na(PosAff) & !is.na(StressCat)]
aovdata[, UserID := factor(UserID)]


## -------------------------------------------------------------------
str(aovdata[, .(UserID, StressCat, PosAff)])


## -------------------------------------------------------------------
anova1 <- ezANOVA(
  data = aovdata,
  dv = PosAff,
  wid = UserID,
  between = StressCat,
  type = 3,
  detailed = TRUE,
  return_aov = TRUE)

print(anova1)



## ----ANOVA_diag1, warning = FALSE, message = FALSE, fig.width=6, fig.height=4, out.width='1\\linewidth', fig.pos="!ht", fig.cap = c("Diagnostic plots for an ANOVA.")----

plot(modelDiagnostics(anova1$aov), ncol = 2)



## -------------------------------------------------------------------
anova1.means <- emmeans(object = anova1$aov,
                        specs = "StressCat")


## -------------------------------------------------------------------
summary(anova1.means, infer = TRUE)


## ----ANOVA_means1, warning = FALSE, message = FALSE, fig.width=6, fig.height=4, out.width='1\\linewidth', fig.pos="!ht", fig.cap = c("ANOVA means plot.")----
plot(anova1.means) +
  xlab("Positive Affect") +
  ylab("Stress Category") +
  coord_flip() +
  ggtitle("Estimated Means and 95% Confidence Intervals from One-Way ANOVA")



## -------------------------------------------------------------------
pairs(anova1.means)


## -------------------------------------------------------------------
penguinsData[order(species, island),
             .N,
             by = .(species, island)]


## -------------------------------------------------------------------
aovPenguins <- penguinsData[ species == "Adelie" &
                               !is.na(body_mass_g) &
                               !is.na(island)]


## -------------------------------------------------------------------
aovPenguins[ , ID := 1:.N]
aovPenguins[, ID := factor(ID)]

str(aovPenguins[, .(ID, island,body_mass_g)])


## -------------------------------------------------------------------
anovaP1 <- ezANOVA(
  data = aovPenguins,
  dv = body_mass_g,
  wid = ID,
  between = island,
  type = 3,
  detailed = TRUE,
  return_aov = TRUE)

print(anovaP1)



## -------------------------------------------------------------------
anovaP1.means <- emmeans(object = anovaP1$aov,
                        specs = "island")

summary(anovaP1.means, infer = TRUE)


## ----ANOVA_meansP1, warning = FALSE, message = FALSE, fig.width=6, fig.height=4, out.width='1\\linewidth', fig.pos="!ht", fig.cap = c("ANOVA means plot.")----
plot(anovaP1.means) +
  xlab("body mass in grams") +
  ylab("islands") +
  coord_flip() +
  ggtitle("Estimated Means and 95% Confidence Intervals from One-Way ANOVA")



## -------------------------------------------------------------------
pairs(anovaP1.means)


## -------------------------------------------------------------------
aovdata[, Female := factor(Female)]


## -------------------------------------------------------------------
aovdata[order(StressCat, Female),
        .N,
        by = .(StressCat, Female)]


## -------------------------------------------------------------------
xtabs(formula = ~ StressCat + Female,
      data = aovdata)


## -------------------------------------------------------------------
anova2 <- ezANOVA(
  data = aovdata,
  dv = PosAff,
  wid = UserID,
  between = .(StressCat, Female),
  type = 3,
  detailed = TRUE,
  return_aov = TRUE)

print(anova2)


## ----ANOVA_diag2, warning = FALSE, message = FALSE, fig.width=6, fig.height=4, out.width='1\\linewidth', fig.pos="!ht", fig.cap = c("Diagnostic plots for an ANOVA.")----
plot(modelDiagnostics(anova2$aov), ncol = 2)


## -------------------------------------------------------------------
anova2.meansA <- emmeans(object = anova2$aov,
                         specs = "StressCat",
                         by = "Female")

summary(anova2.meansA, infer = TRUE)


## ----ANOVA_means2A, warning = FALSE, message = FALSE, fig.width=6, fig.height=4, out.width='1\\linewidth', fig.pos="!ht", fig.cap = c("Factorial ANOVA means plot - Option A.")----
plot(anova2.meansA) +
  xlab("Positive Affect") +
  ylab("Stress Category") +
  coord_flip() +
  ggtitle("Estimated Means and 95% Confidence Intervals from Factorial ANOVA")


## -------------------------------------------------------------------
pairs(anova2.meansA)


## -------------------------------------------------------------------
## calculate the estimated means
anova2.meansB <- emmeans(object = anova2$aov,
                         specs = "Female",
                         by = "StressCat")

## print the means
summary(anova2.meansB, infer = TRUE)


## ----ANOVA_means2B, warning = FALSE, message = FALSE, fig.width=4, fig.height=6, out.width='.7\\linewidth', fig.pos="!ht", fig.cap = c("Factorial ANOVA means plot - Option B.")----
plot(anova2.meansB) +
  xlab("Positive Affect") +
  ylab("Female") +
  coord_flip() +
  ggtitle("Estimated Means and 95% Confidence Intervals from Factorial ANOVA")


## -------------------------------------------------------------------
pairs(anova2.meansB)


## -------------------------------------------------------------------
anova2.meansC <- emmeans(object = anova2$aov,
                         specs = "StressCat",
                         by = NULL)


summary(anova2.meansC, infer = TRUE)


## ----ANOVA_means2C, warning = FALSE, message = FALSE, fig.width=6, fig.height=4, out.width='1\\linewidth', fig.pos="!ht", fig.cap = c("Factorial ANOVA means plot - Option C.")----
plot(anova2.meansC) +
  xlab("Positive Affect") +
  ylab("Stress Category") +
  coord_flip() +
  ggtitle("Estimated Means and 95% Confidence Intervals, averaged across sex")


## -------------------------------------------------------------------
pairs(anova2.meansC)


## -------------------------------------------------------------------
anova2.meansD <- emmeans(object = anova2$aov,
                         specs = "Female",
                         by = NULL)

summary(anova2.meansD, infer = TRUE)


## ----ANOVA_means2D, warning = FALSE, message = FALSE, fig.width=6, fig.height=4, out.width='1\\linewidth', fig.pos="!ht", fig.cap = c("Factorial ANOVA means plot - Option CD")----
plot(anova2.meansD) +
  xlab("Positive Affect") +
  ylab("Female") +
  coord_flip() +
  ggtitle("Estimated Means and 95% Confidence Intervals, averaged across stress category")


## -------------------------------------------------------------------
pairs(anova2.meansD)


## -------------------------------------------------------------------
str(penguinsData)


## -------------------------------------------------------------------
aovPenguinsFull  <- penguinsData[!is.na(body_mass_g) &
                               !is.na(species) &
                                 !is.na(sex)]


## -------------------------------------------------------------------
aovPenguinsFull[ , ID := 1:.N]
aovPenguinsFull[, ID := factor(ID)]


## -------------------------------------------------------------------
xtabs(formula = ~ species + sex,
      data = aovPenguinsFull)


## -------------------------------------------------------------------
anovaP2 <- ezANOVA(
  data = aovPenguinsFull,
  dv = body_mass_g,
  wid = ID,
  between = .(species, sex),
  type = 3,
  detailed = TRUE,
  return_aov = TRUE)

print(anovaP2)


## ----ANOVA_Pdiag2, warning = FALSE, message = FALSE, fig.width=6, fig.height=4, out.width='1\\linewidth', fig.pos="!ht", fig.cap = c("Diagnostic plots for an ANOVA.")----
plot(modelDiagnostics(anovaP2$aov), ncol = 2)


## -------------------------------------------------------------------
anovaP2.meansA <- emmeans(object = anovaP2$aov,
                         specs = "species",
                         by = "sex")

summary(anovaP2.meansA, infer = TRUE)


## ----ANOVA_Pmeans2A, warning = FALSE, message = FALSE, fig.width=6, fig.height=4, out.width='1\\linewidth', fig.pos="!ht", fig.cap = c("Factorial ANOVA means plot - Option A.")----
plot(anovaP2.meansA) +
  xlab("Body Mass") +
  ylab("species") +
  coord_flip() +
  ggtitle("Estimated Means and 95% Confidence Intervals from Factorial ANOVA")


## -------------------------------------------------------------------
pairs(anovaP2.meansA)

