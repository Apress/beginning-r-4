## ----setup, include=FALSE, results='hide', message = FALSE, warning = FALSE, cache=FALSE----
  options(
    width = 70,
    digits = 4,
    datatable.print.nrows = 5)


## -------------------------------------------------------------------
library(data.table)
library(extraoperators)
library(JWileymisc)
library(palmerpenguins)


## -------------------------------------------------------------------
texasData <- fread(file = "data/ch04/Counties_in_Texas.csv",
                  header = TRUE,
                  skip = 0)


## -------------------------------------------------------------------
data(aces_daily)
acesData <- as.data.table(aces_daily)
rm(aces_daily)


## -------------------------------------------------------------------
penguinsData <- as.data.table(penguins)


## -------------------------------------------------------------------
colnames(acesData)


## -------------------------------------------------------------------
unique(acesData$EDU)


## -------------------------------------------------------------------
unique(acesData$SurveyDay)


## -------------------------------------------------------------------
head(acesData)


## -------------------------------------------------------------------
tail(penguinsData)


## -------------------------------------------------------------------
penguinsData[order(flipper_length_mm)]


## -------------------------------------------------------------------
head(penguinsData)


## -------------------------------------------------------------------
penguinsData <- penguinsData[order(flipper_length_mm)]


## -------------------------------------------------------------------
penguinsData


## -------------------------------------------------------------------
penguinsData[order(-species)]


## -------------------------------------------------------------------
penguinsData[order(-species, flipper_length_mm)]


## -------------------------------------------------------------------
texasData[CountyName == "Victoria"]


## -------------------------------------------------------------------
penguinsData[species != "Adelie"]


## -------------------------------------------------------------------
penguinsData[species == "Gentoo" &
           flipper_length_mm == 230]

penguinsData[species == "Gentoo" |
           flipper_length_mm == 230]


## -------------------------------------------------------------------
acesData[UserID %like% "19" &
           STRESS %gel% c(4, 6) &
           NegAff %l% 2 &
           SurveyInteger == 2]


## -------------------------------------------------------------------
acesData[EDU == "NA"]
acesData[EDU == NA]


## -------------------------------------------------------------------
acesData[is.na(EDU)]


## -------------------------------------------------------------------
acesData[, EDU]


## -------------------------------------------------------------------

unique(acesData$EDU)

unique(acesData[, EDU])



## -------------------------------------------------------------------
acesData[, .(UserID, STRESS, NegAff, SurveyInteger, EDU, SES_1)]


## -------------------------------------------------------------------
acesData[UserID %like% "19" &
           STRESS %gel% c(4, 6) &
           NegAff %l% 2 &
           SurveyInteger == 2, #Pay Attention to the comma!
         .(UserID, STRESS, NegAff, SurveyInteger, EDU, SES_1)]


## -------------------------------------------------------------------
acesData[, .N]


## -------------------------------------------------------------------
acesData[UserID %like% "19" &
           STRESS %gel% c(4, 6) &
           NegAff %l% 2 &
           SurveyInteger == 2, #Pay Attention to the comma!
         .N]


## -------------------------------------------------------------------
acesData[UserID %like% "19" &
           STRESS %gel% c(4, 6) &
           NegAff %l% 2 &
           SurveyInteger == 2, #Pay Attention to the comma!
         uniqueN(UserID)]


## -------------------------------------------------------------------
acesData[STRESS %gele% c(0, 3),
         STRESS_3 := "low"]



## ----eval = FALSE---------------------------------------------------
## 
## View(acesData) ## no output, opens RStudio data viewer
## 


## -------------------------------------------------------------------

acesData[STRESS %gle% c(3, 6),
         STRESS_3 := "medium"]

acesData[STRESS %gle% c(6, 10),
         STRESS_3 := "high"]



## ----eval = FALSE---------------------------------------------------
## 
## View(acesData) ## no output, opens RStudio data viewer
## 


## -------------------------------------------------------------------
acesData[UserID %like% "19" &
           STRESS %gel% c(4, 6) &
           NegAff %l% 2 &
           SurveyInteger == 2, #Pay Attention to the comma!
         .(UserID, STRESS, STRESS_3, NegAff, SurveyInteger, EDU, SES_1)]


## -------------------------------------------------------------------
acesData[, .N, by = STRESS_3]


## -------------------------------------------------------------------
acesData[is.na(STRESS_3),
         .N,
         by = STRESS]


## -------------------------------------------------------------------
acesData[STRESS_3 == "low",
         .N,
         by = STRESS]


## -------------------------------------------------------------------
texasData[order(MetroArea),
          uniqueN(CountyNumber),
          by = MetroArea]


## -------------------------------------------------------------------
texasData[order(MetroArea, NCHSUrbanRuralClassification_2013),
          uniqueN(CountyNumber),
          by = .(MetroArea, NCHSUrbanRuralClassification_2013)]


## -------------------------------------------------------------------
texasData[NCHSUrbanRuralClassification_2013 %like% "Small",
          uniqueN(CountyNumber),
          by = .(MetropolitanStatisticalArea_MSA)]


## -------------------------------------------------------------------
acesData[STRESS > 3,
         .N,
         by = .(EDU)]

