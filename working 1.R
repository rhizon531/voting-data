## packages

library(dplyr)
library(pmdplyr)
library(readxl)
library(haven)
library(tidyr)
library(sjlabelled)


## attempt 1

setwd("~/Desktop/voting data")
ZA3521_v2_0_1 <- read_sav("ZA3521_v2-0-1.sav")
dataset2 <- ZA3521_v2_0_1

ZA3648 <- read_data(
  "~/Desktop/voting data/ZA3648.sav",
  atomic.to.fac = FALSE,
  drop.labels = FALSE,
  enc = NULL,
  verbose = FALSE
)

dataset1 <- ZA3648


## keeping variables ZA3521

dataset2$nation2 <- factor(dataset2$nation2, levels = c(1:17), labels = c("France", "Belgium", "Netherlands", "Germany", "Italy", "Luxembourg", "Denmark", "Ireland", "UK", "Greece", "Spain", "Portugal", "Norway", "Finland", "Sweden", "Austria", "Switzerland"))
dataset2$country_year <- paste(dataset2$nation2, dataset2$year, sep="_")

dataset2 <- dataset2 %>% select(country_year, id, split, eb, better, econpast, finapast, satisdmo, closepty, feelclo, voteint, inclvote, lastvote, particip, lrs, party, married, educ, sex, age, sizehh, occup, soclass, income, regionat)
View(dataset2)

## ZA3648 - i will come back to this like it's stressed me out

dataset1$V3 <- factor(dataset1$V3, levels = c(1:22), labels = c("Albania", "Armenia", "Belarus", "Bulgaria", "Croatia", "CzechRepublic", "Slovakia", "Estonia", "Georgia", "Hungary", "Latvia", "Lithuania", "Macedonia", "Moldova", "Poland", "Romania", "Russia", "Slovenia", "Ukraine", "Kazakhstan", "Yugoslavia", "GDR"))
dataset1$country_year <- paste(dataset1$V3, dataset1$V4,sep="_")

datasubset1 <-  dataset1 %>% select(country_year, V80, V81, V82, V83, V84, V85, V86, V87, V88, V89, V90, V91, V92, V93, V94, V95, V96, V97, V98, V99, V100)

datasubset1[is.na(datasubset1)] <- 0

datasubset1 %>% mutate(sum = rowSums(.[V80:V100]))

datasubset1$voteint <- paste(datasubset1$V80, datasubset1$V81, datasubset1$V82, datasubset1$V83, datasubset1$V84, datasubset1$V85, datasubset1$V86, datasubset1$V87, datasubset1$V88, datasubset1$V89,datasubset1$V90, datasubset1$V91, datasubset1$V92, datasubset1$V93, datasubset1$V94, datasubset1$V95, datasubset1$V96, datasubset1$V97, datasubset1$V98, datasubset1$V99, datasubset1$V100)

dataset1 <-merge(dataset1, datasubset1, by="country_year")

dataset1 <- dataset1 %>% select(country_year, V3V80:V100)


dataset1$voteint <- paste(dataset1$V80+ dataset1$V81+ dataset1$V82+dataset1$V83+ dataset1$V84+ dataset1$V85+ dataset1$V86+ dataset1$V87+ dataset1$V88+dataset1$V89+ dataset1$V90+ dataset1$V91+ dataset1$V92+ dataset1$V93+dataset1$V94+ dataset1$V95+dataset1$V96+ dataset1$V97+ dataset1$V98+ dataset1$V99+ dataset1$V100, na.rm = TRUE)


dataset1 %>% unite("voteint", V80:V100, na.rm = TRUE, remove = FALSE)

save(dataset1, file="EAEurobarometer.RData")






