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


## -------------------------------------------------------------------
1/8


## -------------------------------------------------------------------
(1/2)*(1/2)*(1/2)


## -------------------------------------------------------------------
1 - (1/8)

7/8 == 1 - (1/8)


## ----pd_nfam, fig.width=6, fig.height=6, out.width='.6\\linewidth',fig.pos="!ht", fig.cap = c("Normal distributions with different means and standard deviations."), echo=FALSE, warning=FALSE, message=FALSE----
ggplot(data.frame(x = seq(-4, 4, by = .05)), aes(x)) +  
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1.5),
                colour = "#E69F00", size = 0.5, linetype = 2) +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1),
                colour = "#000000", size = 3, linetype = 1) +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 0.6),
                colour = "#56B4E9", size = 1, linetype = 3) +     
  stat_function(fun = dnorm, args = list(mean = 1, sd = 0.6),
                colour = "#009E73", size = 1.5, linetype = 4) +
  xlab("x") +
  ylab("Density of the normal distribution")



## ----pd_perfectnorm, fig.width=6, fig.height=5, out.width='.6\\linewidth',fig.pos="!ht", fig.cap = c("Nearly Perfect Normal Histogram with normal distribution superimposed."), echo=FALSE, warning=FALSE, message=FALSE----
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

ggplot(data = perfect[variable == "normal"],
       mapping = aes(x = value))+
  geom_histogram(alpha = 0.2, aes(y=..density..))+     
  stat_function(fun = dnorm, args = list(mean = mean(perfect[variable == "normal"]$value, na.rm = TRUE),
                                         sd = sd(perfect[variable == "normal"]$value, na.rm = TRUE)))



## -------------------------------------------------------------------
mean(penguinsData$flipper_length_mm, na.rm = TRUE)
sd(penguinsData$flipper_length_mm, na.rm = TRUE)


## ----pd_penguinhistnorm, fig.width=6, fig.height=5, out.width='.6\\linewidth',fig.pos="!ht", fig.cap = c("penguin flipper histogram with normal curve"), echo=FALSE,warning=FALSE, message=FALSE----
ggplot(data = penguinsData,
       mapping = aes(x = flipper_length_mm)) +
  geom_histogram(alpha = 0.2, aes(y = ..density..)) +
  stat_function(fun = dnorm, args = list(
    mean = mean(penguinsData$flipper_length_mm, na.rm = TRUE),
    sd = sd(penguinsData$flipper_length_mm, na.rm = TRUE)
  )) +
  scale_y_continuous("Density",
                     sec.axis = sec_axis(
                       trans = ~ . * 2 * 342,
                       name = "Counts",
                       breaks = seq(0, 30, 5)
                     ))


## -------------------------------------------------------------------
penguinsData[, mean(flipper_length_mm, na.rm = TRUE),
             by = species]
penguinsData[, sd(flipper_length_mm, na.rm = TRUE), by = species]


## ----pd_Adeliehistnorm, fig.width=6, fig.height=5, out.width='.6\\linewidth',fig.pos="!ht", fig.cap = c("Adelie flipper histogram with normal curve"), echo=FALSE, message=FALSE, warning=FALSE----
ggplot(data = penguinsData[species == "Adelie"],
       mapping = aes(x = flipper_length_mm)) +
  geom_histogram(alpha = 0.2, aes(y = ..density..)) +
  stat_function(fun = dnorm, args = list(
    mean = mean(penguinsData[species == "Adelie"]$flipper_length_mm, na.rm = TRUE),
    sd = sd(penguinsData[species == "Adelie"]$flipper_length_mm, na.rm = TRUE)
  ))


## ----pd_perfecttD, fig.width=6, fig.height=5, out.width='.6\\linewidth',fig.pos="!ht", fig.cap = c("Nearly Perfect Normal Data in testDistribution() plot"), echo=FALSE, warning=FALSE, message=FALSE----

testN <- testDistribution(perfect[variable == "normal"]$value,
                          distr = "normal")

plot(testN)


## ----pd_perfectUtD, fig.width=6, fig.height=5, out.width='.6\\linewidth',fig.pos="!ht", fig.cap = c("Nearly Perfect Uniform Data in testDistribution() plot"), echo=FALSE, warning=FALSE, message=FALSE----

testU <- testDistribution(perfect[variable == "uniform"]$value,
                          distr = "normal")

plot(testU)


## ----pd_penguinFtD, fig.width=6, fig.height=5, out.width='.6\\linewidth',fig.pos="!ht", fig.cap = c("All Penguins testDistribution() plot"), echo=TRUE, warning=FALSE, message=FALSE----

testF <- testDistribution(penguinsData$flipper_length_mm,
                 distr = "normal")

plot(testF)


## ----pd_soloPenguin, fig.width=10, fig.height=5, out.width='1\\linewidth',fig.pos="!ht", fig.cap = c("exploded species flipper histograms (density)")----
ggplot(data = penguinsData,
       mapping = aes(x = flipper_length_mm)) +
  geom_histogram(aes(y = ..density..)) +
  facet_grid(cols = vars(species))


## -------------------------------------------------------------------
testA <- testDistribution(penguinsData[species == "Adelie"]$flipper_length_mm,
                 distr = "normal")

testC <- testDistribution(penguinsData[species == "Chinstrap"]$flipper_length_mm,
                 distr = "normal")

testG <- testDistribution(penguinsData[species == "Gentoo"]$flipper_length_mm,
                 distr = "normal")


## ----pd_testA, fig.width=6, fig.height=5, out.width='.6\\linewidth',fig.pos="!ht", fig.cap = c("Adelie Penguins testDistribution() plot"), echo=TRUE, warning=FALSE, message=FALSE----
plot(testA)


## ----pd_testC, fig.width=6, fig.height=5, out.width='.6\\linewidth',fig.pos="!ht", fig.cap = c("Chinstrap Penguins testDistribution() plot"), echo=TRUE, warning=FALSE, message=FALSE----
plot(testC)


## ----pd_testG, fig.width=6, fig.height=5, out.width='.6\\linewidth',fig.pos="!ht", fig.cap = c("Gentoo Penguins testDistribution() plot"), echo=TRUE, warning=FALSE, message=FALSE----
plot(testG)


## -------------------------------------------------------------------
penguinsData[,
             .N,
             by = .(species, sex)]


## ----pd_perfectSD, fig.width=5, fig.height=4, out.width='.6\\linewidth',fig.pos="!ht", fig.cap = c("bars captured by the standard deviation ranges."), echo=FALSE, warning=FALSE, message=FALSE----
ggplot(data = perfect[variable == "normal"],
       mapping = aes(x = value))+
  geom_histogram(fill = "white", col = "black")+
  annotate(x = 2.5 , y = 950, geom = "text", label = "1s", size = 5)+
  annotate(x = 2.5 , y = 350, geom = "text", label = "2s", size = 5)+
  annotate(x = 2.5 , y = 50, geom = "text", label = "3s", size = 5)+
  geom_segment(x = (2.5-0.75), xend = (2.5 + .75), y = 900, yend = 900, size = 2)+
  geom_segment(x = (2.5-2*0.75), xend = (2.5 + 2*.75), y = 300, yend = 300, size = 2)+
  geom_segment(x = (2.5-3*0.75), xend = (2.5 + 3*.75), y = 10, yend = 10, size = 2)


## -------------------------------------------------------------------
perfectZscore <- (2-2.5)/0.75
perfectZscore


## -------------------------------------------------------------------
perfectZscore <- (4.5-2.5)/0.75
perfectZscore


## -------------------------------------------------------------------
pnorm(4.5,
      mean = 2.5,
      sd = 0.75,
      lower.tail = TRUE)


## -------------------------------------------------------------------
chinstrapData <- penguinsData[species == "Chinstrap"]
chinstrapMean <- mean(chinstrapData$flipper_length_mm)
chinstrapSD <- sd(chinstrapData$flipper_length_mm)


## -------------------------------------------------------------------
chinstrapData[, zScoreFlipper := scale(flipper_length_mm)]
head(chinstrapData)
(192-chinstrapMean)/chinstrapSD


## -------------------------------------------------------------------
pnorm(191, chinstrapMean, chinstrapSD, lower.tail = TRUE)


## -------------------------------------------------------------------
1 - pnorm(191, chinstrapMean, chinstrapSD, lower.tail = TRUE)
pnorm(191, chinstrapMean, chinstrapSD, lower.tail = FALSE)


## -------------------------------------------------------------------
pnorm(210, chinstrapMean, chinstrapSD, lower.tail = FALSE)


## -------------------------------------------------------------------
adelieData <- penguinsData[species == "Adelie"]
adelieMean <- mean(adelieData$flipper_length_mm, na.rm = TRUE)
adelieSD <- sd(adelieData$flipper_length_mm, na.rm = TRUE)


## -------------------------------------------------------------------
pnorm(202, # x > 202mm
      mean = adelieMean, #sample mean
      sd = adelieSD, # sample sd
      lower.tail = FALSE) #GREATER than is upper/right tail


## -------------------------------------------------------------------
set.seed(1234)
sampleA <- rnorm(n = 5, mean = 2.5, sd = 0.75)
sampleB <- rnorm(n = 5, mean = 2.5, sd = 0.75)

summary(sampleA)
summary(sampleB)


## -------------------------------------------------------------------
set.seed(1234)
sampleC <- runif(n = 5, min = 0, max = 5)
sampleD <- runif(n = 5, min = 0, max = 5)

summary(sampleC)
summary(sampleD)


## -------------------------------------------------------------------
set.seed(1234)
perfectUniform <- data.table(uniform = runif(n = 100000,
                                             min = 0,
                                             max = 5),
                             sample = 1:1000)
perfectUniform


## ----pd_histPerfectUniform, fig.width=6, fig.height=5, out.width='.6\\linewidth',fig.pos="!ht", fig.cap = c("Uniform Distribution Population N = 100,000"), echo=TRUE, warning=FALSE, message=FALSE----
hist(perfectUniform$uniform)


## -------------------------------------------------------------------
sampleMeans <- perfectUniform[,
               .(SampleMean = mean(uniform)),
               by = sample]
sampleMeans


## ----pd_histUniformMeans, fig.width=6, fig.height=5, out.width='.6\\linewidth',fig.pos="!ht", fig.cap = c("1,000 sample means from a uniform distribution"), echo=TRUE, warning=FALSE, message=FALSE----
hist(sampleMeans$SampleMean)


## -------------------------------------------------------------------
mu <- mean(perfectUniform$uniform)
sigma <- sd(perfectUniform$uniform)
n <- 100000/1000

standardError <- sigma/sqrt(n)
standardError


## -------------------------------------------------------------------
mean(sampleMeans$SampleMean)
sd(sampleMeans$SampleMean)


## -------------------------------------------------------------------
sampleMeansF <- data.table(sampleID = 1:1000,
                           sampleMean = 0)


## -------------------------------------------------------------------
set.seed(1234) #ensures your results match ours; not used in real life


## ----eval=FALSE-----------------------------------------------------
## for(i in startInteger:endInteger){ #notice this starting bracket
## 
##   #code inside here gets repeated from startInteger to endInteger.
## 
## 
## } #notice this ending bracket!


## -------------------------------------------------------------------
for(i in 1:1000){#for loop cycles 1 to 1000 times, getting a random sample each time.
  
  #each iteration of the loop, a NEW, with-replacement sample of flipper lengths
  randomSample <- sample(x = penguinsData$flipper_length_mm,
       size = 30, #there are 30 flipper lengths in each sample
       replace = TRUE) #with replacement!
  
  sampleMeansF[sampleID == i, #row selection to CURRENT sample ID
               sampleMean := mean(randomSample, #assign sample mean 
                                  na.rm = TRUE)]
  
}


## ----pd_testMF, fig.width=6, fig.height=5, out.width='.6\\linewidth',fig.pos="!ht", fig.cap = c("distribution of sample means in all penguins"), echo=TRUE, warning=FALSE, message=FALSE----
testMF <- testDistribution(x = sampleMeansF$sampleMean,
                           distr = "normal")
plot(testMF)


## -------------------------------------------------------------------
xbar <- mean(penguinsData$flipper_length_mm, na.rm = TRUE)
stdDev <- sd(penguinsData$flipper_length_mm, na.rm = TRUE)
n <- 30

standardError <- stdDev/sqrt(n)
standardError


## -------------------------------------------------------------------
xbar
mean(sampleMeansF$sampleMean)

standardError
sd(sampleMeansF$sampleMean)


## -------------------------------------------------------------------
pnorm(195,
      mean = mean(sampleMeansF$sampleMean),
      sd = sd(sampleMeansF$sampleMean),
      lower.tail = TRUE)


## -------------------------------------------------------------------
unduplicatedAcesAge <- acesData[! is.na(Age),
                                .(UserID, Age)]
unduplicatedAcesAge <- unique(unduplicatedAcesAge)


## -------------------------------------------------------------------
sampleMeansA <- data.table(sampleID = 1:1000,
                           sampleMean = 0)

set.seed(1234) #ensures your results match ours; not used in real life

for(i in 1:1000){#for loop cycles 1 to 1000 times, getting a random sample each time.
  
  #each iteration of the loop, a NEW, with-replacement sample of ages
  randomSample <- sample(x = unduplicatedAcesAge$Age,
       size = 30, #there are 30 ages in each sample
       replace = TRUE) #with replacement!
  
  sampleMeansA[sampleID == i, #row selection to CURRENT sample ID
               sampleMean := mean(randomSample, #assign sample mean 
                                  na.rm = TRUE)]
  
}


## ----pd_testAces, fig.width=6, fig.height=5, out.width='.6\\linewidth',fig.pos="!ht", fig.cap = c("distribution of sample means of ages in aces daily data"), echo=TRUE, warning=FALSE, message=FALSE----
testAces <- testDistribution(x = sampleMeansA$sampleMean,
                           distr = "normal")
plot(testAces)


## -------------------------------------------------------------------
xbar <- mean(unduplicatedAcesAge$Age, na.rm = TRUE)
stdDev <- sd(unduplicatedAcesAge$Age, na.rm = TRUE)
n <- 30

standardError <- stdDev/sqrt(n)
standardError


## -------------------------------------------------------------------
xbar
mean(sampleMeansA$sampleMean)

standardError
sd(sampleMeansA$sampleMean)


## -------------------------------------------------------------------
pnorm(24,
      mean = mean(sampleMeansA$sampleMean),
      sd = sd(sampleMeansA$sampleMean),
      lower.tail = FALSE) #more than 24


## -------------------------------------------------------------------
unduplicatedAcesAge[Age >= 24, .N]

