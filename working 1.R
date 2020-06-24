rm(list = ls())


## packages

library(dplyr)
library(pmdplyr)
library(tidyr)
library(sjlabelled)
library(haven)


## attempt 1

setwd("~/Desktop/voting data")
dataset2 <- read_sav("ZA3521_v2-0-1.sav")


dataset1 <- read_data(
  "~/Desktop/voting data/ZA3648.sav",
  atomic.to.fac = FALSE,
  drop.labels = FALSE,
  enc = NULL,
  verbose = FALSE
)




## ZA3521 dataset 2

dataset2$nation2 <- factor(dataset2$nation2, levels = c(1:17), labels = c("France", "Belgium", "Netherlands", "Germany", "Italy", "Luxembourg", "Denmark", "Ireland", "UK", "Greece", "Spain", "Portugal", "Norway", "Finland", "Sweden", "Austria", "Switzerland"))
dataset2$country_year <- paste(dataset2$nation2, dataset2$year, sep="_")

dataset2 <- dataset2 %>% select(country_year, country=nation2, year, id, id2=split, id3=eb, better, econpast, finapast, satisdmo, closepty, feelclo, voteint, inclvote, lastvote, particip, lrs, party, married, educ, sex, age, sizehh, secoccup, occup, soclass, income, region=regionat)


View(dataset2)


## dataset 2 missing cases
dataset2$voteint<-replace(dataset2$voteint, is.na(dataset2$voteint), 0)
dataset2$inclvote<-replace(dataset2$inclvote, is.na(dataset2$inclvote), 0)
dataset2$voteint[dataset2$voteint<=99] <- NA
dataset2$voteint[dataset2$voteint>=990] <- NA
dataset2$inclvote[dataset2$inclvote<=99] <- NA
dataset2$inclvote[dataset2$inclvote>=990] <- NA

table(dataset2$country, dataset2$voteint)

## ZA3648 - 

dataset1$country <- factor(dataset1$V3, levels = c(1:22), labels = c("Albania", "Armenia", "Belarus", "Bulgaria", "Croatia", "CzechRepublic", "Slovakia", "Estonia", "Georgia", "Hungary", "Latvia", "Lithuania", "Macedonia", "Moldova", "Poland", "Romania", "Russia", "Slovenia", "Ukraine", "Kazakhstan", "Yugoslavia", "GDR"))
dataset1$country_year <- paste(dataset1$country, dataset1$V4,sep="_")


#start by creating new variable for each V79:V100

dataset1<-dataset1 %>% mutate(vote_Albania=V79)
dataset1<-dataset1 %>% mutate(vote_Armenia=V80)
dataset1<-dataset1 %>% mutate(vote_Belarus=V81)
dataset1<-dataset1 %>% mutate(vote_Bulgaria=V82)
dataset1<-dataset1 %>% mutate(vote_Croatia=V83)
dataset1<-dataset1 %>% mutate(vote_Czech=V84)
dataset1<-dataset1 %>% mutate(vote_Slovakia=V85)
dataset1<-dataset1 %>% mutate(vote_Estonia=V86)
dataset1<-dataset1 %>% mutate(vote_Hungary=V87)
dataset1<-dataset1 %>% mutate(vote_Latvia=V88)
dataset1<-dataset1 %>% mutate(vote_Lithuania=V89)
dataset1<-dataset1 %>% mutate(vote_Macedonia=V90)
dataset1<-dataset1 %>% mutate(vote_Poland=V91)
dataset1<-dataset1 %>% mutate(vote_Romania=V92)
dataset1<-dataset1 %>% mutate(vote_Russia=V93)
dataset1<-dataset1 %>% mutate(vote_Slovenia=V94)
dataset1<-dataset1 %>% mutate(vote_Ukraine=V95)
dataset1<-dataset1 %>% mutate(vote_Georgia=V96)
dataset1<-dataset1 %>% mutate(vote_Kazachstan=V97)
dataset1<-dataset1 %>% mutate(vote_Yugoslavia=V98)
dataset1<-dataset1 %>% mutate(vote_GDR=V99)
dataset1<-dataset1 %>% mutate(vote_Moldova=V100)


## Replace NAs to 0
dataset1$vote_Albania<-replace(dataset1$vote_Albania, is.na(dataset1$vote_Albania), 0)
dataset1$vote_Armenia<-replace(dataset1$vote_Armenia, is.na(dataset1$vote_Armenia), 0)
dataset1$vote_Belarus<-replace(dataset1$vote_Belarus, is.na(dataset1$vote_Belarus), 0)
dataset1$vote_Bulgaria<-replace(dataset1$vote_Bulgaria, is.na(dataset1$vote_Bulgaria), 0)
dataset1$vote_Croatia<-replace(dataset1$vote_Croatia, is.na(dataset1$vote_Croatia), 0)
dataset1$vote_Czech<-replace(dataset1$vote_Czech, is.na(dataset1$vote_Czech), 0)
dataset1$vote_Slovakia<-replace(dataset1$vote_Slovakia, is.na(dataset1$vote_Slovakia), 0)
dataset1$vote_Estonia<-replace(dataset1$vote_Estonia, is.na(dataset1$vote_Estonia), 0)
dataset1$vote_Hungary<-replace(dataset1$vote_Hungary, is.na(dataset1$vote_Hungary), 0)
dataset1$vote_Latvia<-replace(dataset1$vote_Latvia, is.na(dataset1$vote_Latvia), 0)
dataset1$vote_Lithuania<-replace(dataset1$vote_Lithuania, is.na(dataset1$vote_Lithuania), 0)
dataset1$vote_Macedonia<-replace(dataset1$vote_Macedonia, is.na(dataset1$vote_Macedonia), 0)
dataset1$vote_Poland<-replace(dataset1$vote_Poland, is.na(dataset1$vote_Poland), 0)
dataset1$vote_Romania<-replace(dataset1$vote_Romania, is.na(dataset1$vote_Romania), 0)
dataset1$vote_Russia<-replace(dataset1$vote_Russia, is.na(dataset1$vote_Russia), 0)
dataset1$vote_Slovenia<-replace(dataset1$vote_Slovenia, is.na(dataset1$vote_Slovenia), 0)
dataset1$vote_Ukraine<-replace(dataset1$vote_Ukraine, is.na(dataset1$vote_Ukraine), 0)
dataset1$vote_Georgia<-replace(dataset1$vote_Georgia, is.na(dataset1$vote_Georgia), 0)
dataset1$vote_Kazachstan<-replace(dataset1$vote_Kazachstan, is.na(dataset1$vote_Kazachstan), 0)
dataset1$vote_Yugoslavia<-replace(dataset1$vote_Yugoslavia, is.na(dataset1$vote_Yugoslavia), 0)
dataset1$vote_GDR<-replace(dataset1$vote_GDR, is.na(dataset1$vote_GDR), 0)
dataset1$vote_Moldova<-replace(dataset1$vote_Moldova, is.na(dataset1$vote_Moldova), 0)


# Add all relevant columns to get the sum; if NA then you get 0 otherwise the value of the party
#dataset1$vote_int <- (dataset1$vote_Albania+dataset1$vote_Armenia+dataset1$vote_Belarus+dataset1$V82+dataset1$V83+dataset1$V84+dataset1$V85+dataset1$V86+dataset1$V87+dataset1$V88+dataset1$V89+dataset1$V90+dataset1$V91+dataset1$V92+dataset1$V93+dataset1$V94+dataset1$V95+dataset1$V96+dataset1$V97+dataset1$V98+dataset1$V99+dataset1$V100)


dataset1$vote_int = (dataset1$vote_Albania+dataset1$vote_Armenia+dataset1$vote_Belarus+dataset1$vote_Bulgaria+dataset1$vote_Croatia+dataset1$vote_Czech+dataset1$vote_Slovakia+dataset1$vote_Estonia+dataset1$vote_Hungary+dataset1$vote_Latvia+dataset1$vote_Lithuania+dataset1$vote_Macedonia+dataset1$vote_Poland+dataset1$vote_Romania+dataset1$vote_Russia+dataset1$vote_Slovenia+dataset1$vote_Ukraine+dataset1$vote_Georgia+dataset1$vote_Kazachstan+dataset1$vote_Yugoslavia+dataset1$vote_GDR+dataset1$vote_Moldova) 


#Replace 0s to NAs and no vote/spoil. no answer

dataset1$vote_int[dataset1$vote_int==0] <- NA
dataset1$vote_int[dataset1$vote_Albania>=24] <- NA
dataset1$vote_int[dataset1$vote_Armenia>=19] <- NA
dataset1$vote_int[dataset1$vote_Belarus>=28] <- NA
dataset1$vote_int[dataset1$vote_Bulgaria>=21] <- NA
dataset1$vote_int[dataset1$vote_Croatia >=14] <- NA
dataset1$vote_int[dataset1$vote_Czech>=31] <- NA
dataset1$vote_int[dataset1$vote_Slovakia>=28] <- NA
dataset1$vote_int[dataset1$vote_Estonia>=38] <- NA
dataset1$vote_int[dataset1$vote_Hungary==17] <- NA
dataset1$vote_int[dataset1$vote_Hungary>=19] <- NA
dataset1$vote_int[dataset1$vote_Latvia==27] <- NA
dataset1$vote_int[dataset1$vote_Latvia>=29] <- NA
dataset1$vote_int[dataset1$vote_Lithuania==26] <- NA
dataset1$vote_int[dataset1$vote_Lithuania>=28] <- NA
dataset1$vote_int[dataset1$vote_Macedonia>=27] <- NA
dataset1$vote_int[dataset1$vote_Poland>=39] <- NA
dataset1$vote_int[dataset1$vote_Romania>=30] <- NA
dataset1$vote_int[dataset1$vote_Russia==39] <- NA
dataset1$vote_int[dataset1$vote_Russia>=41] <- NA
dataset1$vote_int[dataset1$vote_Slovenia==22] <- NA 
dataset1$vote_int[dataset1$vote_Slovenia>=24] <- NA 
dataset1$vote_int[dataset1$vote_Ukraine>=25] <- NA
dataset1$vote_int[dataset1$vote_Georgia>=28] <- NA
dataset1$vote_int[dataset1$vote_Kazachstan>=23] <- NA
dataset1$vote_int[dataset1$vote_Yugoslavia>=17] <- NA
dataset1$vote_int[dataset1$vote_GDR>=17] <- NA
dataset1$vote_int[dataset1$vote_Moldova>=17] <- NA





table(dataset1$vote_int)
table(dataset1$country,dataset1$vote_int)





# mutating vote incl variables

dataset1<-dataset1 %>% mutate(voteinc_Albania=V101)
dataset1<-dataset1 %>% mutate(voteinc_Belarus=V102)
dataset1<-dataset1 %>% mutate(voteinc_Bulgaria=V103)
dataset1<-dataset1 %>% mutate(voteinc_Czech=V104)
dataset1<-dataset1 %>% mutate(voteinc_Slovakia=V105)
dataset1<-dataset1 %>% mutate(voteinc_Estonia=V106)
dataset1<-dataset1 %>% mutate(voteinc_Hungary=V107)
dataset1<-dataset1 %>% mutate(voteinc_Latvia=V108)
dataset1<-dataset1 %>% mutate(voteinc_Lithuania=V109)
dataset1<-dataset1 %>% mutate(voteinc_Macedonia=V110)
dataset1<-dataset1 %>% mutate(voteinc_Poland=V111)
dataset1<-dataset1 %>% mutate(voteinc_Romania=V112)
dataset1<-dataset1 %>% mutate(voteinc_Russia=V113)
dataset1<-dataset1 %>% mutate(voteinc_Slovenia=V114)
dataset1<-dataset1 %>% mutate(voteinc_Ukraine=V115)
dataset1<-dataset1 %>% mutate(voteinc_Georgia=V116)
dataset1<-dataset1 %>% mutate(voteinc_Moldova=V117)


# Replace NAs to 0s

dataset1$voteinc_Albania<-replace(dataset1$voteinc_Albania, is.na(dataset1$voteinc_Albania), 0)
dataset1$voteinc_Belarus<-replace(dataset1$voteinc_Belarus, is.na(dataset1$voteinc_Belarus), 0)
dataset1$voteinc_Bulgaria<-replace(dataset1$voteinc_Bulgaria, is.na(dataset1$voteinc_Bulgaria), 0)
dataset1$voteinc_Czech<-replace(dataset1$voteinc_Czech, is.na(dataset1$voteinc_Czech), 0)
dataset1$voteinc_Slovakia<-replace(dataset1$voteinc_Slovakia, is.na(dataset1$voteinc_Slovakia), 0)
dataset1$voteinc_Estonia<-replace(dataset1$voteinc_Estonia, is.na(dataset1$voteinc_Estonia), 0)
dataset1$voteinc_Hungary<-replace(dataset1$voteinc_Hungary, is.na(dataset1$voteinc_Hungary), 0)
dataset1$voteinc_Latvia<-replace(dataset1$voteinc_Latvia, is.na(dataset1$voteinc_Latvia), 0)
dataset1$voteinc_Lithuania<-replace(dataset1$voteinc_Lithuania, is.na(dataset1$voteinc_Lithuania), 0)
dataset1$voteinc_Macedonia<-replace(dataset1$voteinc_Macedonia, is.na(dataset1$voteinc_Macedonia), 0)
dataset1$voteinc_Poland<-replace(dataset1$voteinc_Poland, is.na(dataset1$voteinc_Poland), 0)
dataset1$voteinc_Romania<-replace(dataset1$voteinc_Romania, is.na(dataset1$voteinc_Romania), 0)
dataset1$voteinc_Russia<-replace(dataset1$voteinc_Russia, is.na(dataset1$voteinc_Russia), 0)
dataset1$voteinc_Slovenia<-replace(dataset1$voteinc_Slovenia, is.na(dataset1$voteinc_Slovenia), 0)
dataset1$voteinc_Ukraine<-replace(dataset1$voteinc_Ukraine, is.na(dataset1$voteinc_Ukraine), 0)
dataset1$voteinc_Georgia<-replace(dataset1$voteinc_Georgia, is.na(dataset1$voteinc_Georgia), 0)
dataset1$voteinc_Moldova<-replace(dataset1$voteinc_Moldova, is.na(dataset1$voteinc_Moldova), 0)


# thingy

dataset1$voteinc = (dataset1$voteinc_Albania+dataset1$voteinc_Belarus+dataset1$voteinc_Bulgaria+dataset1$voteinc_Czech+dataset1$voteinc_Estonia+dataset1$voteinc_Hungary+dataset1$voteinc_Latvia+dataset1$voteinc_Lithuania+dataset1$voteinc_Macedonia+dataset1$voteinc_Poland+dataset1$voteinc_Romania+dataset1$voteinc_Russia+dataset1$voteinc_Slovenia+dataset1$voteinc_Ukraine+dataset1$voteinc_Georgia+dataset1$voteinc_Moldova)


#Replace 0s to NAs and no vote/spoil. no answer

dataset1$voteinc[dataset1$voteinc==0] <- NA
dataset1$voteinc[dataset1$voteinc_Albania>=24] <- NA
dataset1$voteinc[dataset1$voteinc_Belarus>=28] <- NA
dataset1$voteinc[dataset1$voteinc_Bulgaria>=21] <- NA
dataset1$voteinc[dataset1$voteinc_Czech>=31] <- NA
dataset1$voteinc[dataset1$voteinc_Slovakia>=28] <- NA
dataset1$voteinc[dataset1$voteinc_Estonia>=38] <- NA
dataset1$voteinc[dataset1$voteinc_Hungary==17] <- NA
dataset1$voteinc[dataset1$voteinc_Hungary>=19] <- NA
dataset1$voteinc[dataset1$voteinc_Latvia==27] <- NA
dataset1$voteinc[dataset1$voteinc_Latvia>=29] <- NA
dataset1$voteinc[dataset1$voteinc_Lithuania==26] <- NA
dataset1$voteinc[dataset1$voteinc_Lithuania>=28] <- NA
dataset1$voteinc[dataset1$voteinc_Macedonia>=27] <- NA
dataset1$voteinc[dataset1$voteinc_Poland>=39] <- NA
dataset1$voteinc[dataset1$voteinc_Romania>=30] <- NA
dataset1$voteinc[dataset1$voteinc_Russia==39] <- NA
dataset1$voteinc[dataset1$voteinc_Russia>=41] <- NA
dataset1$voteinc[dataset1$voteinc_Slovenia==22] <- NA 
dataset1$voteinc[dataset1$voteinc_Slovenia>=24] <- NA 
dataset1$voteinc[dataset1$voteinc_Ukraine>=25] <- NA
dataset1$voteinc[dataset1$voteinc_Georgia>=28] <- NA
dataset1$voteinc[dataset1$voteinc_Moldova>=17] <- NA

table(dataset1$voteinc)
table(dataset1$country,dataset1$voteinc)

# dataset1 income merging

dataset1$V135<-replace(dataset1$V135, is.na(dataset1$V135), 0)
dataset1$V136<-replace(dataset1$V136, is.na(dataset1$V136), 0)
dataset1$V137<-replace(dataset1$V137, is.na(dataset1$V137), 0)
dataset1$V138<-replace(dataset1$V138, is.na(dataset1$V138), 0)
dataset1$V139<-replace(dataset1$V139, is.na(dataset1$V139), 0)
dataset1$V140<-replace(dataset1$V140, is.na(dataset1$V140), 0)
dataset1$V141<-replace(dataset1$V141, is.na(dataset1$V141), 0)
dataset1$V142<-replace(dataset1$V142, is.na(dataset1$V142), 0)
dataset1$V143<-replace(dataset1$V143, is.na(dataset1$V143), 0)
dataset1$V144<-replace(dataset1$V144, is.na(dataset1$V144), 0)
dataset1$V145<-replace(dataset1$V145, is.na(dataset1$V145), 0)
dataset1$V146<-replace(dataset1$V146, is.na(dataset1$V146), 0)
dataset1$V147<-replace(dataset1$V147, is.na(dataset1$V147), 0)
dataset1$V148<-replace(dataset1$V148, is.na(dataset1$V148), 0)
dataset1$V149<-replace(dataset1$V149, is.na(dataset1$V149), 0)
dataset1$V150<-replace(dataset1$V150, is.na(dataset1$V150), 0)
dataset1$V151<-replace(dataset1$V151, is.na(dataset1$V151), 0)
dataset1$V152<-replace(dataset1$V152, is.na(dataset1$V152), 0)
dataset1$V153<-replace(dataset1$V153, is.na(dataset1$V153), 0)
dataset1$V154<-replace(dataset1$V154, is.na(dataset1$V154), 0)
dataset1$V155<-replace(dataset1$V155, is.na(dataset1$V155), 0)
dataset1$V156<-replace(dataset1$V156, is.na(dataset1$V156), 0)
dataset1$V157<-replace(dataset1$V157, is.na(dataset1$V157), 0)
dataset1$V158<-replace(dataset1$V158, is.na(dataset1$V158), 0)
dataset1$V159<-replace(dataset1$V159, is.na(dataset1$V159), 0)
dataset1$V160<-replace(dataset1$V160, is.na(dataset1$V160), 0)
dataset1$V161<-replace(dataset1$V161, is.na(dataset1$V161), 0)
dataset1$V162<-replace(dataset1$V162, is.na(dataset1$V162), 0)
dataset1$V163<-replace(dataset1$V163, is.na(dataset1$V163), 0)
dataset1$V164<-replace(dataset1$V164, is.na(dataset1$V164), 0)
dataset1$V165<-replace(dataset1$V165, is.na(dataset1$V165), 0)
dataset1$V166<-replace(dataset1$V166, is.na(dataset1$V166), 0)
dataset1$V167<-replace(dataset1$V167, is.na(dataset1$V167), 0)
dataset1$V168<-replace(dataset1$V168, is.na(dataset1$V168), 0)
dataset1$V169<-replace(dataset1$V169, is.na(dataset1$V169), 0)
dataset1$V170<-replace(dataset1$V170, is.na(dataset1$V170), 0)
dataset1$V171<-replace(dataset1$V171, is.na(dataset1$V171), 0)
dataset1$V172<-replace(dataset1$V172, is.na(dataset1$V172), 0)
dataset1$V173<-replace(dataset1$V173, is.na(dataset1$V173), 0)
dataset1$V174<-replace(dataset1$V174, is.na(dataset1$V174), 0)
dataset1$V175<-replace(dataset1$V175, is.na(dataset1$V175), 0)
dataset1$V176<-replace(dataset1$V176, is.na(dataset1$V176), 0)
dataset1$V177<-replace(dataset1$V177, is.na(dataset1$V177), 0)
dataset1$V178<-replace(dataset1$V178, is.na(dataset1$V178), 0)
dataset1$V179<-replace(dataset1$V179, is.na(dataset1$V179), 0)
dataset1$V180<-replace(dataset1$V180, is.na(dataset1$V180), 0)
dataset1$V181<-replace(dataset1$V181, is.na(dataset1$V181), 0)
dataset1$V182<-replace(dataset1$V182, is.na(dataset1$V182), 0)
dataset1$V183<-replace(dataset1$V183, is.na(dataset1$V183), 0)
dataset1$V184<-replace(dataset1$V184, is.na(dataset1$V184), 0)
dataset1$V185<-replace(dataset1$V185, is.na(dataset1$V185), 0)
dataset1$V186<-replace(dataset1$V186, is.na(dataset1$V186), 0)
dataset1$V187<-replace(dataset1$V187, is.na(dataset1$V187), 0)
dataset1$V188<-replace(dataset1$V188, is.na(dataset1$V188), 0)

dataset1$income = dataset1$V135+dataset1$V136+dataset1$V137+dataset1$V138+dataset1$V139+dataset1$V140+dataset1$V141+dataset1$V142+dataset1$V143+dataset1$V144+dataset1$V145+dataset1$V146+dataset1$V147+dataset1$V148+dataset1$V149+dataset1$V150+dataset1$V151+dataset1$V152+dataset1$V153+dataset1$V154+dataset1$V155+dataset1$V156+dataset1$V157+dataset1$V158+dataset1$V159+dataset1$V160+dataset1$V161+dataset1$V162+dataset1$V163+dataset1$V164+dataset1$V165+dataset1$V166+dataset1$V167+dataset1$V168+dataset1$V169+dataset1$V170+dataset1$V171+dataset1$V172+dataset1$V173+dataset1$V174+dataset1$V175+dataset1$V176+dataset1$V177+dataset1$V178+dataset1$V179+dataset1$V180+dataset1$V181+dataset1$V182+dataset1$V183+dataset1$V184+dataset1$V185+dataset1$V186+dataset1$V187+dataset1$V188

dataset1$income[dataset1$income==0] <- NA
dataset1$income[dataset1$income>=97] <-NA


# region merging

dataset1$V215<-replace(dataset1$V215, is.na(dataset1$V215), 0)
dataset1$V216<-replace(dataset1$V216, is.na(dataset1$V216), 0)
dataset1$V217<-replace(dataset1$V217, is.na(dataset1$V217), 0)
dataset1$V218<-replace(dataset1$V218, is.na(dataset1$V218), 0)
dataset1$V219<-replace(dataset1$V219, is.na(dataset1$V219), 0)
dataset1$V220<-replace(dataset1$V220, is.na(dataset1$V220), 0)
dataset1$V221<-replace(dataset1$V221, is.na(dataset1$V221), 0)
dataset1$V222<-replace(dataset1$V222, is.na(dataset1$V222), 0)
dataset1$V223<-replace(dataset1$V223, is.na(dataset1$V223), 0)
dataset1$V224<-replace(dataset1$V224, is.na(dataset1$V224), 0)
dataset1$V225<-replace(dataset1$V225, is.na(dataset1$V225), 0)
dataset1$V226<-replace(dataset1$V226, is.na(dataset1$V226), 0)
dataset1$V227<-replace(dataset1$V227, is.na(dataset1$V227), 0)
dataset1$V228<-replace(dataset1$V228, is.na(dataset1$V228), 0)
dataset1$V229<-replace(dataset1$V228, is.na(dataset1$V229), 0)
dataset1$V230<-replace(dataset1$V230, is.na(dataset1$V230), 0)
dataset1$V231<-replace(dataset1$V231, is.na(dataset1$V231), 0)
dataset1$V232<-replace(dataset1$V232, is.na(dataset1$V232), 0)
dataset1$V233<-replace(dataset1$V233, is.na(dataset1$V233), 0)
dataset1$V234<-replace(dataset1$V234, is.na(dataset1$V234), 0)
dataset1$V235<-replace(dataset1$V235, is.na(dataset1$V235), 0)
dataset1$V236<-replace(dataset1$V236, is.na(dataset1$V236), 0)

dataset1$region = (dataset1$V215+dataset1$V216+dataset1$V217+dataset1$V218+dataset1$V219+dataset1$V220+dataset1$V221+dataset1$V222+dataset1$V223+dataset1$V224+dataset1$V225+dataset1$V226+dataset1$V227+dataset1$V228+dataset1$V229+dataset1$V230+dataset1$V231+dataset1$V232+dataset1$V233+dataset1$V234+dataset1$V235+dataset1$V236)

dataset1$region[dataset1$region==0]<-NA


## occupation sector

dataset1$V130<-replace(dataset1$V130, is.na(dataset1$V130), 0)
dataset1$V131<-replace(dataset1$V131, is.na(dataset1$V131), 0)

dataset1$secoccup = dataset1$V130+dataset1$V131

dataset1$secoccup[dataset1$secoccup==0]<-NA
dataset1$secoccup[dataset1$secoccup>=98]<-NA

table(dataset1$country,dataset1$secoccup)

# condense dataset1

dataset1 <- dataset1 %>% select(country_year, country=V3, year=V4, id=V6, better=V11, econpast=V10, finapast=V12, satisdmo=V19, voteint=vote_int, inclvote=voteinc, lrs=V76, married=V125, educ=V128, sex=V119, age=V120, sizehh=V126, secoccup=secoccup, occup=V132, income=income, region=region, econfuture=V11, finafuture=V13, freemarket=V14, reform=V17)


# merge dataset
datasetfull <- merge(dataset1, dataset2, by="country_year", all=TRUE)
save(datasetfull, file="EAEurobarometer.RData")


## annex bc i might need this

# c("country_year", "country", "year", "id", "better", "econpast", "finapast", "satisdmo","voteint", "inclvote", "lrs", "married", "educ", "sex", "age", "sizehh", "secoccup", "occup", "income", "region")

