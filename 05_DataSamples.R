## ----setup, include=FALSE, results='hide', message = FALSE, warning = FALSE, cache=FALSE----
  options(
    width = 70,
    digits = 4,
    datatable.print.nrows = 5)


## -------------------------------------------------------------------
library(data.table)
library(palmerpenguins)
library(JWileymisc)
library(extraoperators)


## -------------------------------------------------------------------
acesData <- as.data.table(aces_daily)
str(acesData)


## -------------------------------------------------------------------
unique(acesData$SurveyInteger)
unique(acesData$SES_1)



## -------------------------------------------------------------------
unique(acesData$SurveyDay)


## -------------------------------------------------------------------
unique(acesData$Age)
summary(acesData$SOLs)
unique(acesData$WASONs)


## -------------------------------------------------------------------
head(mtcars)


## -------------------------------------------------------------------
str(penguins)


## -------------------------------------------------------------------
#this comment reminds us we pretend acesData is a population
colnames(acesData)


## -------------------------------------------------------------------
convenienceAge <- acesData[UserID <= 20, Age]
unique(convenienceAge)


## -------------------------------------------------------------------
#convenience sample selection of first 20 participants
convenienceAge <- acesData[UserID <= 20, Age]

#each participant takes many surveys, just need age once
convenienceAge <-  unique(convenienceAge)

# sort the ages for better readability 
sort(convenienceAge)


## -------------------------------------------------------------------
sort(convenienceAge, na.last = TRUE)
sort(convenienceAge, na.last = FALSE)


## -------------------------------------------------------------------
?sort()


## -------------------------------------------------------------------
penguinsData <- as.data.table(penguins)

#convenience sample selection of first 20 penguins
convenienceFlippers <- penguinsData[1:20, flipper_length_mm]

#Just need each length once each; only need range
convenienceFlippers <-  unique(convenienceFlippers)

# sort the lengths for better readability 
sort(convenienceFlippers, na.last = TRUE)


## -------------------------------------------------------------------
#the modulo output of 0 is the 'remainder'
6 %% 2
8 %% 2
#the modulo output of 1 is the 'remainder'
5 %% 2
7 %% 2


## -------------------------------------------------------------------
13 %% 13
26 %% 13


## -------------------------------------------------------------------
# Assign every 13th UserID to idCheck
idCheck <- acesData[UserID %% 13 == 0, UserID]
# Make sure we got the correct UserIDs
unique(idCheck)


## -------------------------------------------------------------------
#kth sample selection of every 13 participants
kthAge <- acesData[UserID %% 13 == 0, Age]

#each participant takes many surveys, just need age once
kthAge <-  unique(kthAge)

# sort the ages for better readability 
sort(kthAge, na.last = TRUE)


## -------------------------------------------------------------------
#observation-rows in penguins
nrow(penguinsData)

#sequence of every 13th row needed
seq(13, nrow(penguinsData), 13)


## -------------------------------------------------------------------
#kth sample selection of every 13th penguin
penguinsData[seq(13, nrow(penguinsData), 13)]


## -------------------------------------------------------------------
#assign every 13th sample to kthFlipper variable
kthFlipper <- penguinsData[seq(13, nrow(penguinsData), 13), flipper_length_mm]

#Just need each length once each
kthFlipper <-  unique(kthFlipper)

# the lengths for better readability
sort(kthFlipper, na.last = TRUE)


## -------------------------------------------------------------------
#assign to a well-named variable
clusterDate <- c(as.Date("2017-02-23"))

acesData[SurveyDay %in% clusterDate,
         .(UserID, Age)]


## -------------------------------------------------------------------
#cluster sample of all participants on 27 Feb 2017
clusterAge <- acesData[SurveyDay %in% clusterDate,
                       Age]

#each participant takes many surveys, just need age once
clusterAge <-  unique(clusterAge)

# sort the ages for better readability 
sort(clusterAge, na.last = TRUE)


## -------------------------------------------------------------------
mtcarsData <- as.data.table(mtcars, keep.rownames = TRUE)

mtcarsData[rn %like% "Merc" |
                           rn %like% "Toyota"]



## -------------------------------------------------------------------
#cluster sample of hp
clusterCar <- mtcarsData[rn %like% "Merc" |
                           rn %like% "Toyota", hp]

#some cars may share hp, we still are focused on range
clusterCar <-  unique(clusterCar)

# sort the hp for better readability 
sort(clusterCar, na.last = TRUE)


## -------------------------------------------------------------------
unique(acesData$SES_1)
unique(acesData$Female)


## -------------------------------------------------------------------
acesData[1, , by = .(SES_1, Female)]


## -------------------------------------------------------------------
acesData[ ,
          .SD[1],
          by = .(SES_1, Female) ]


## -------------------------------------------------------------------
#strata sample of age by SES and sex
strataAge <- acesData[ ,
          .SD[1],
          by = .(SES_1, Female) ][, Age]

#each participant takes many surveys, just need age once
strataAge <-  unique(strataAge)

# sort the ages for better readability 
sort(strataAge, na.last = TRUE)


## -------------------------------------------------------------------
# recall : can be used as a shortcut for sequence
1:3

# make one tiny change to .SD to get first three rows.
penguinsData[,
         .SD[1:3],
         by = species]


## -------------------------------------------------------------------
#strata sample of penguin flipper lengths by species
strataFlippers <- penguinsData[,
         .SD[1:3],
         by = species][, flipper_length_mm]

#Just need each length once each
strataFlippers <-  unique(strataFlippers)

#sort the lengths for better readability
sort(strataFlippers, na.last = TRUE)


## -------------------------------------------------------------------
set.seed(1234)
sample(x = 1:191, size = 20)


## -------------------------------------------------------------------
#remember, do not use this in general
set.seed(1234)

#random sample of age
randomIDs <- sample(x = 1:191, size = 20)
randomAge <- acesData[UserID %in% randomIDs,
                      Age]

#each participant takes many surveys, just need age once
randomAge <-  unique(randomAge)

# sort the ages for better readability 
sort(randomAge, na.last = TRUE)


## -------------------------------------------------------------------
#remember, do not use this in general
set.seed(1234)

#random sample for penguins
randomRows <- sample(x = 1:nrow(penguinsData), size = 20)

#random sample of penguins
randomFlippers <- penguinsData[randomRows, flipper_length_mm]

#Just need each length once each
randomFlippers <-  unique(randomFlippers)

#sort the lengths for better readability
sort(randomFlippers, na.last = TRUE)


## ----freqIntro, include=FALSE---------------------------------------
acesData[order(Age),
         .(N = uniqueN(UserID)),
         by = Age]


## -------------------------------------------------------------------
acesData[,
         uniqueN(UserID),
         by = Age]


## -------------------------------------------------------------------
acesData[,
         .( N = uniqueN(UserID)),
         by = Age]


## -------------------------------------------------------------------
acesData[order(Age),
         .( N = uniqueN(UserID)),
         by = Age]


## -------------------------------------------------------------------
acesAgeFrequency <- acesData[order(Age),
                                .(Freq = uniqueN(UserID)),
                                by = Age]


## -------------------------------------------------------------------
acesAgeFrequency[, CumulaFreq := cumsum(Freq)]
acesAgeFrequency


## -------------------------------------------------------------------
penguinsData[order(flipper_length_mm),
         .N,
         by = flipper_length_mm]


## -------------------------------------------------------------------
minLength <- min(penguinsData$flipper_length_mm, na.rm = TRUE)
maxLength <- max(penguinsData$flipper_length_mm, na.rm = TRUE)

numberOfBins <- 5

binWidth <- (maxLength - minLength)/numberOfBins


## -------------------------------------------------------------------
minLength
maxLength
numberOfBins
binWidth


## -------------------------------------------------------------------
cutOffs <- seq(from = minLength,
               to = maxLength,
               by = binWidth)
cutOffs


## -------------------------------------------------------------------
penguinsData[ , BinOrdinal := findInterval(flipper_length_mm, cutOffs)]
head(penguinsData)


## -------------------------------------------------------------------
flipperLengthFreq <- penguinsData[order(BinOrdinal) ,
                           .(Freq = .N),
                           by = BinOrdinal]

flipperLengthFreq


## -------------------------------------------------------------------
flipperLengthFreq[, cumulaFreq := cumsum(Freq)]
flipperLengthFreq


## -------------------------------------------------------------------
penguinsData[order(flipper_length_mm),
         .N,
         by = flipper_length_mm]


## -------------------------------------------------------------------
sample(x = 1:10, size = 1)
sample(x = 1:10, size = 1)


## -------------------------------------------------------------------
set.seed(1234)
sample(x = 1:10, size = 1)
set.seed(NULL) #this cancels the use of set.seed()

