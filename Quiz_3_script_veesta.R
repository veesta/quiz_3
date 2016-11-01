library(tidyverse)
library(cocor)
library(predictionInterval)
library(apaTables)

bfi2 <- read_csv("bfi2.csv")
glimpse(bfi2)


apa.cor.table(bfi2)

cocor(~A1+C1|E1+O1, data=as.data.frame(bfi2))

cocor(~A1+C1|A1+E1, data=as.data.frame(bfi2))

bfi2_men <- bfi2 %>% filter(gender==1) %>% select(-gender)
bfi2_women <- bfi2 %>% filter(gender==2) %>% select(-gender)

glimpse(bfi2_men)

glimpse(bfi2_women)

glimpse(bfi2)

bfi2_men_dataframe <- as.data.frame(bfi2_men)
bfi2_women_dataframe <- as.data.frame(bfi2_women)

cocor(~A1+E1|A1+E1, data=list(bfi2_men_dataframe, bfi2_women_dataframe))

#extent rating raises differs from rating critical correlation - apa style in reading 

library(cocor)

?cocor.dep.groups.overlap

cocor.dep.groups.overlap(.59, .16, .38, 30, alternative= "two.sided",
                         test ="all", alpha = 0.05, conf.level = 0.95, null.value = 0,
                         data.name = NULL, var.labels = NULL, return.htest = FALSE)

?cocor.dep.groups.nonoverlap


#rating raises vs. complaints critical 

cocor.dep.groups.nonoverlap(.59, .19, .83, .16, .67, .38, 30,
                            alternative = "two.sided", test="all", alpha= 0.05,
                            var.labels=NULL, return.htest = FALSE)

library(predictionInterval)


#A new paper examines the rating-privileges correlation. It used a sample size of 1000 and got a correlation of 
#.1. Is this correlation different - use cocor.indep.groups

#?cocor.indep.groups

cocor.indep.groups(.59, .03, 30, 3000, alternative= "two.sided",
                   test = "all", alpha = 0.05, conf.level = 0.95, null.value = 0,
                   data.name = NULL, var.labels  = NULL, return.htest = FALSE)



