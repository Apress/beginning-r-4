## ----setup, include=FALSE, results='hide', message = FALSE, warning = FALSE, cache=FALSE----
  options(
    width = 70,
    digits = 4,
    datatable.print.nrows = 5)


## -------------------------------------------------------------------
library(data.table)
library(ggplot2)
library(palmerpenguins)

library(JWileymisc)
library(extraoperators)


## -------------------------------------------------------------------
acesData <- as.data.table(aces_daily)
penguinsData <- as.data.table(penguins)
mtcarsData <- as.data.table(mtcars, keep.rownames = TRUE)


## ----ds_penguinHist, fig.width=4, fig.height=4, out.width='.5\\linewidth',fig.pos="!ht", fig.cap = c("penguinsData Histogram")----
hist(x = penguinsData$flipper_length_mm)


## ----ds_acesAgeDup, fig.width=5, fig.height=4, out.width='.5\\linewidth',fig.pos="!ht", fig.cap = c("Duplicated aces Age Histogram")----
hist(x = acesData$Age)


## ----ds_acesAgeUndup, fig.width=5, fig.height=4, out.width='.5\\linewidth',fig.pos="!ht", fig.cap = c("Unduplicated aces Age Histogram")----
unduplicatedAcesAge <- acesData[! is.na(Age),
                                .(UserID, Age)]

unduplicatedAcesAge <- unique(unduplicatedAcesAge)

hist(x = unduplicatedAcesAge$Age)


## -------------------------------------------------------------------
setdiff(x = 1:191,
        y = unduplicatedAcesAge$UserID)


## -------------------------------------------------------------------
setdiff(x = c(107, 108),
        y = c(108))

setdiff(x = c(107, 108),
        y = c(108, 500))


## -------------------------------------------------------------------
1:191

letters

LETTERS


## -------------------------------------------------------------------
setdiff(x = 1:191,
        y = unduplicatedAcesAge$UserID)


## ----ds_penguinDot1, fig.width=5, fig.height=4, out.width='.6\\linewidth',fig.pos="!ht", fig.cap = c("penguin Dot Plot")----
dotchart(x = penguinsData$flipper_length_mm)


## ----ds_penguinDot2, fig.width=5, fig.height=4, out.width='.6\\linewidth',fig.pos="!ht", fig.cap = c("penguin Dot Plot by species")----
dotchart(x = penguinsData$flipper_length_mm,
         groups = penguinsData$species)


## ----ds_baseGG, fig.width=5, fig.height=4, out.width='.6\\linewidth',fig.pos="!ht", fig.cap = c("ggplot() base settings, no geometry")----
ggplot(data = penguinsData,
       mapping = aes(x = flipper_length_mm))


## ----ds_baseGGhist, fig.width=5, fig.height=4, out.width='.6\\linewidth',fig.pos="!ht", fig.cap = c("ggplot() base settings, histogram")----
ggplot(data = penguinsData,
       mapping = aes(x = flipper_length_mm))+
  geom_histogram()


## ----ds_baseGGdot, fig.width=5, fig.height=4, out.width='.6\\linewidth',fig.pos="!ht", fig.cap = c("ggplot() base settings, dot")----
ggplot(data = penguinsData,
       mapping = aes(x = flipper_length_mm))+
  geom_dotplot()


## ----ds_penguinGGfill, fig.width=5, fig.height=4, out.width='.6\\linewidth',fig.pos="!ht", fig.cap = c("penguin Dot Plot with fill = species")----
ggplot(data = penguinsData,
       mapping = aes(x = flipper_length_mm,
                     fill = species))+
  geom_dotplot()


## ----ds_penguinGGfillfac, fig.width=9, fig.height=4, out.width='1\\linewidth',fig.pos="!ht", fig.cap = c("penguin Dot Plot by species"), echo=FALSE----
ggplot(data = penguinsData,
       mapping = aes(x = flipper_length_mm,
                     fill = species))+
  geom_dotplot() +
  facet_grid(cols = vars(species))+
  scale_y_continuous(NULL, breaks = NULL)


## ----ds_perfect, fig.width=6, fig.height=4, out.width='1\\linewidth',fig.pos="!ht", fig.cap = c("Nearly Perfect Normal and Uniform Histograms"), echo=FALSE, warning=FALSE, message=FALSE----

set.seed(1234)
perfect <- data.table(
  normal = rnorm(n = 10000,
                 mean = 2.5,
                 sd = 0.75),
  uniform = runif(n = 10000,
                  min = 0,
                  max = 5)
)
perfect <- melt(perfect)

ggplot(data = perfect,
       mapping = aes(x = value))+
  geom_histogram()+
  facet_grid(cols = vars(variable))




## ----ds_acesHist, fig.width=5, fig.height=4, out.width='.6\\linewidth',fig.pos="!ht", fig.cap = c("Histogram of COPEExp")----
ggplot(data = acesData[SurveyInteger == 3],
       mapping = aes(x = COPEExp))+
  geom_histogram()


## ----ds_acesHistTrim, fig.width=5, fig.height=4, out.width='.6\\linewidth',fig.pos="!ht", fig.cap = c("Trimmed Histogram of COPEExp")----
ggplot(data = acesData[SurveyInteger == 3 & 
                         COPEExp %gl% c(1,4)],
       mapping = aes(x = COPEExp))+
  geom_histogram()


## -------------------------------------------------------------------
(0 + 100000)/ 2


## -------------------------------------------------------------------
salaries <- c(0, 100000)
mean(salaries)


## -------------------------------------------------------------------
(0 + 100000 + 57364)/3

salaries <- c(0, 100000, 57364)
mean(salaries)


## -------------------------------------------------------------------
mean(penguinsData$flipper_length_mm)
mean(penguinsData$flipper_length_mm, na.rm = TRUE)


## ----ds_penguinHistMean, fig.width=5, fig.height=4, out.width='.6\\linewidth',fig.pos="!ht", fig.cap = c("penguin histogram with arithmetic mean")----
ggplot(data = penguinsData,
       mapping = aes(x = flipper_length_mm))+
  geom_histogram()+
  geom_vline(mapping = aes(xintercept = mean(flipper_length_mm, na.rm = TRUE)))


## -------------------------------------------------------------------
mean(acesData[SurveyInteger == 3]$COPEExp)
mean(acesData[SurveyInteger == 3]$COPEExp,
     na.rm = TRUE)


## ----ds_acesHistExpMean, fig.width=5, fig.height=4, out.width='.6\\linewidth',fig.pos="!ht", fig.cap = c("Histogram of COPEExp with arithmetic mean")----
ggplot(data = acesData[SurveyInteger == 3],
       mapping = aes(x = COPEExp))+
  geom_histogram()+
  geom_vline(mapping = aes(xintercept = mean(COPEExp, na.rm = TRUE)))


## -------------------------------------------------------------------
salaries


## -------------------------------------------------------------------
mean(salaries)
median(salaries)


## -------------------------------------------------------------------
salariesEven <- c(0, 100000, 57364, 20000)

sort(salariesEven)
median(salariesEven)
mean(c(57364, 20000))


## -------------------------------------------------------------------
mean(penguinsData$flipper_length_mm, na.rm = TRUE)
median(penguinsData$flipper_length_mm, na.rm = TRUE)


## ----ds_penguinHistMedian, fig.width=5, fig.height=4, out.width='.6\\linewidth',fig.pos="!ht", fig.cap = c("penguin histogram with arithmetic mean and dashed line median")----
ggplot(data = penguinsData,
       mapping = aes(x = flipper_length_mm))+
  geom_histogram()+
  geom_vline(mapping = aes(xintercept = mean(flipper_length_mm, na.rm = TRUE)))+
  geom_vline(mapping = aes(xintercept = median(flipper_length_mm, na.rm = TRUE)),
                           linetype = "dashed")


## -------------------------------------------------------------------
mean(acesData[SurveyInteger == 3]$COPEExp,
     na.rm = TRUE)
median(acesData[SurveyInteger == 3]$COPEExp,
     na.rm = TRUE)


## ----ds_acesHistExpMedian, fig.width=5, fig.height=4, out.width='.6\\linewidth',fig.pos="!ht", fig.cap = c("Histogram of COPEExp with arithmetic mean")----
ggplot(data = acesData[SurveyInteger == 3],
       mapping = aes(x = COPEExp))+
  geom_histogram()+
  geom_vline(mapping = aes(xintercept = mean(COPEExp, na.rm = TRUE)))+
  geom_vline(mapping = aes(xintercept = median(COPEExp, na.rm = TRUE)),
             linetype = "dashed")


## -------------------------------------------------------------------
quartiles <- seq(0, 1, 0.25)
deciles <- seq(0, 1, 0.10)
grades <- seq(0.6, 1, 0.10)
percentiles <- seq(0, 1, 0.01)


## -------------------------------------------------------------------
quantile(salaries, quartiles)
median(salaries)

quantile(salariesEven, quartiles)
median(salariesEven)


## ----ds_penguinJitter, fig.width=5, fig.height=4, out.width='.6\\linewidth',fig.pos="!ht", fig.cap = c("penguin jitter plot")----
ggplot(data = penguinsData,
       mapping = aes(x = flipper_length_mm, y = 0))+
  geom_jitter()


## -------------------------------------------------------------------
quantile(penguinsData$flipper_length_mm, quartiles)
quantile(penguinsData$flipper_length_mm, quartiles, na.rm = TRUE)


## ----ds_penguinBox, fig.width=5, fig.height=4, out.width='.6\\linewidth',fig.pos="!ht", fig.cap = c("penguin boxplot")----
ggplot(data = penguinsData,
       mapping = aes(x = flipper_length_mm, y = 0))+
  geom_jitter()+
  geom_boxplot(alpha = 0.5)


## -------------------------------------------------------------------
acesData[order(Age), .N, by = Age]


## -------------------------------------------------------------------
acesData[, meanAge := mean(Age, na.rm = TRUE), by = UserID]


## -------------------------------------------------------------------
acesData[order(meanAge), .N, by = meanAge]


## ----ds_acesmeanAgeBox, fig.width=5, fig.height=4, out.width='.6\\linewidth',fig.pos="!ht", fig.cap = c("meanAge boxplot")----
ggplot(data = acesData,
       mapping = aes(x = meanAge, y = 0))+
  geom_jitter(alpha = 0.4)+
  geom_boxplot(alpha = 0.85)


## ----ds_boxOutliers, fig.width=7, fig.height=4, out.width='1\\linewidth',fig.pos="!ht", fig.cap = c("outliers in boxplot"), echo=FALSE, warning=FALSE, message=FALSE----
ggplot(data = perfect,
       mapping = aes(x = value, y = 0))+
  geom_jitter(alpha = 0.2)+
  geom_boxplot(alpha = 0.85, outlier.size = 5)+
  facet_grid(cols = vars(variable))


## ----echo=FALSE, warning=FALSE, message=FALSE-----------------------
perfect[,.(Variance = var(value),
           StandardDeviation = sd(value),
           ArithmeticMean = mean(value),
           Median = median(value)),
        by = variable]


## ----ds_perfectSD, fig.width=6, fig.height=5, out.width='.8\\linewidth',fig.pos="!ht", fig.cap = c("bars captured by the standard deviation ranges."), echo=FALSE, warning=FALSE, message=FALSE----
ggplot(data = perfect[variable == "normal"],
       mapping = aes(x = value))+
  geom_histogram(fill = "white", col = "black")+
  annotate(x = 2.5 , y = 950, geom = "text", label = "2.5 +/- 1s (68%)", size = 5)+
  annotate(x = 2.5 , y = 350, geom = "text", label = "2.5 +/- 2s (95%)", size = 5)+
  annotate(x = 2.5 , y = 50, geom = "text", label = "2.5 +/- 3s (99.7%)", size = 5)+
  geom_segment(x = (2.5-0.75), xend = (2.5 + .75), y = 900, yend = 900, size = 2)+
  geom_segment(x = (2.5-2*0.75), xend = (2.5 + 2*.75), y = 300, yend = 300, size = 2)+
  geom_segment(x = (2.5-3*0.75), xend = (2.5 + 3*.75), y = 10, yend = 10, size = 2)


## -------------------------------------------------------------------
summary(penguinsData$flipper_length_mm)


## -------------------------------------------------------------------
var(penguinsData$flipper_length_mm, na.rm = TRUE)
sd(penguinsData$flipper_length_mm, na.rm = TRUE)


## -------------------------------------------------------------------
count <- penguinsData[flipper_length_mm %between% c(187, 215),
         .N]
count

count/nrow(penguinsData)


## -------------------------------------------------------------------
sqrt(var(penguinsData$flipper_length_mm, na.rm = TRUE)) ==
  sd(penguinsData$flipper_length_mm, na.rm = TRUE)


## -------------------------------------------------------------------
summary(acesData$meanAge)


## -------------------------------------------------------------------
var(acesData$meanAge, na.rm = TRUE)
sd(acesData$meanAge, na.rm = TRUE)


## ----ds_acesmeanAgeHist, fig.width=5, fig.height=4, out.width='.6\\linewidth',fig.pos="!ht", fig.cap = c("meanAge histogram")----
ggplot(data = acesData,
       mapping = aes(x = meanAge))+
  geom_histogram()


## -------------------------------------------------------------------
count <- acesData[meanAge %between% c(19.5, 23.9) &
                    !is.na(meanAge),
                  .N]
count

count/nrow(acesData[!is.na(meanAge)])


## -------------------------------------------------------------------
#first you get a data.table with only lengths
standardDeviationpenguin <- penguinsData[,.(flipper_length_mm)]

#you need to know the mean length
standardDeviationpenguin[, Mean := mean(flipper_length_mm, na.rm = TRUE)]

#so far we have one entry per penguin flipper length and the overall mean.
head(standardDeviationpenguin)

#this is the residual difference between each length and the mean
standardDeviationpenguin[ , x_iMinusMean := (flipper_length_mm - Mean)]

#this lets us see three penguins (these happen to be from each species by visual inspection)
standardDeviationpenguin[c(1, 153, 277)]


## -------------------------------------------------------------------
standardDeviationpenguin[ , x_iMinusMeanSquared := (x_iMinusMean^2)]

#this lets us see three penguins (these happen to be from each species by visual inspection)
standardDeviationpenguin[c(1, 153, 277)]


## -------------------------------------------------------------------
variance <- sum(standardDeviationpenguin$x_iMinusMeanSquared, na.rm = TRUE) / (nrow(standardDeviationpenguin) - 1)


## -------------------------------------------------------------------
sqrt(variance)
sd(standardDeviationpenguin$flipper_length_mm, na.rm = TRUE)

