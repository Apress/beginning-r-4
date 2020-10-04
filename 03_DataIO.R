## ----setup, include=FALSE, results='hide', message = FALSE, warning = FALSE, cache=FALSE----
options(
  width = 70,
  digits = 4)


## -------------------------------------------------------------------
academicYear <- 2020


## -------------------------------------------------------------------
academicYear


## -------------------------------------------------------------------
mtcarsWeight <- mtcars$wt
mtcarsWeight


## -------------------------------------------------------------------
library(data.table)
csvData <- fread(file = "data/ch03/Counties_in_Texas.csv",
                  header = TRUE,
                  skip = 0)


## -------------------------------------------------------------------
head(csvData)


## -------------------------------------------------------------------
library(readxl)
excelData <- read_excel(path = "data/ch03/Counties_in_Texas.xlsx",
                        sheet = 1, #or sheet = "Counties_in_Texas"
                        col_names = TRUE,
                        skip = 0
                        )


## -------------------------------------------------------------------
head(excelData)


## -------------------------------------------------------------------
rData <- readRDS(file = "data/ch03/Counties_in_Texas.rds")


## -------------------------------------------------------------------
tail(rData)


## -------------------------------------------------------------------
library(haven)
spssData <- read_spss(file = "data/ch03/Counties_in_Texas.sav",
                      skip = 0)


## -------------------------------------------------------------------
stataData <- read_stata("data/ch03/Counties_in_Texas.dta",
                        skip = 0)


## -------------------------------------------------------------------
sasData <- read_sas("data/ch03/ACS_1yr_Seq_Table_Number_Lookup.sas7bdat",
                    skip = 0)
head(sasData)


## -------------------------------------------------------------------
fwrite(x = rData,
       file = "output/ch3_txCounties.csv")


## -------------------------------------------------------------------
library(writexl)
write_xlsx(x = rData,
           path = "output/ch3_txCounties.xlsx",
           col_names = TRUE,
           format_headers = TRUE)


## -------------------------------------------------------------------
saveRDS(object = rData,
        file = "output/ch3_txCounties.RDS")

