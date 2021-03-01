library(readxl)
library(readr)
library(dplyr)
library(caret)
library(caTools)

cub <- read.csv('C:/Users/Florecita/Documents/SKRIPSI/fixdataset_nv_edit(uji).csv')

glimpse(cub)
cub$mhsNpm = as.factor(cub$mhsNpm)
str(cub$mhsNpm)

set.seed(123)
split = sample.split(cibi$mhsNpm, SplitRatio = 0.70)

train = subset(cub, split == TRUE)
test = subset(cub, split == FALSE)

dim(train); dim(test)



write.csv(train, "uji_train.csv", row.names = FALSE)
write.csv(test, "uji_test.csv", row.names = FALSE)

train_data <- read.transactions('C:/Users/Florecita/Documents/SKRIPSI/ProcessingData/shineandrise/uji_train.csv', format = 'basket', sep=',')
test_data <- read.transactions('C:/Users/Florecita/Documents/SKRIPSI/ProcessingData/shineandrise/uji_test.csv', format = 'basket', sep=',')



t1<-proc.time() 
v_rule <- apriori(train_data, parameter = list(sup=0.1))
v_rule2 <- apriori(test_data, parameter = list(sup=0.1))
proc.time() - t1


rules.sort1 = sort(v_rule, by ="lift")
subset.matrix = is.subset(rules.sort1, rules.sort1, sparse = FALSE)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant = colSums(subset.matrix, na.rm=T) >= 1

#remove redundant rules
rules.pruni <- rules.sort1[!redundant]
summary(rules.pruni)
interdata2 <- inspect(rules.pruni)





t2<-proc.time()

proc.time() - t2

proc.time() - t1

t3<-Sys.time(); v_rule <- apriori(train_data, parameter = list(sup=0.100)); v_rule2 <- apriori(test_data, parameter = list(sup=0.100)); t4 <- Sys.time()

difftime(t4,t3)

t5<-Sys.time(); v_rule3 <- apriori(train_data, parameter = list(sup=0.050)); v_rule4 <- apriori(test_data, parameter = list(sup=0.050)); t6 <- Sys.time()

difftime(t6,t5)


t7<-Sys.time(); v_rule5 <- apriori(train_data, parameter = list(sup=0.010)); v_rule6 <- apriori(test_data, parameter = list(sup=0.010)); t8 <- Sys.time()

difftime(t8,t7)

t9<-Sys.time(); v_rule7 <- apriori(train_data, parameter = list(sup=0.005)); v_rule8 <- apriori(test_data, parameter = list(sup=0.005)); t10 <- Sys.time()

difftime(t10,t9)


quality(v_rule)$kappa <- interestMeasure(v_rule,measure='kappa',transactions = train_data)
quality(v_rule)$leastContradiction <- interestMeasure(v_rule, measure='leastContradiction',transactions = train_data)
try <- as(v_rule,'data.frame')
summary(try$kappa)

v_rule <- apriori(train_data, parameter = list(sup=0.100))
v_rule2 <- apriori(test_data, parameter = list(sup=0.100))

v_rule3 <- apriori(train_data, parameter = list(sup=0.050))
v_rule4 <- apriori(test_data, parameter = list(sup=0.050))

v_rule5 <- apriori(train_data, parameter = list(sup=0.010))
v_rule6 <- apriori(test_data, parameter = list(sup=0.010))

v_rule7 <- apriori(train_data, parameter = list(sup=0.005))
v_rule8 <- apriori(test_data, parameter = list(sup=0.005))




v_rule8 <- apriori(train_data, parameter = list(sup=0.010, maxlen=2))
v_rule9 <- apriori(test_data, parameter = list(sup=0.010, maxlen=2))


v_rule8 <- apriori(train_data, parameter = list(sup=0.010, maxlen=3))
v_rule9 <- apriori(test_data, parameter = list(sup=0.010, maxlen=3))

v_rule8 <- apriori(train_data, parameter = list(sup=0.010, maxlen=4))
v_rule9 <- apriori(test_data, parameter = list(sup=0.010, maxlen=4))

v_rule8 <- apriori(train_data, parameter = list(sup=0.10, conf=0.4))
v_rule9 <- apriori(test_data, parameter = list(sup=0.10, conf=0.4))


-0.02594  0.02365  0.04618  0.06079  0.07787  0.26990 
>


k_rule <- apriori(train_data, parameter = list(supp=0.05))
quality(k_rule)$kappa <- interestMeasure(k_rule,measure='kappa',transactions = train_data)
quality(k_rule)$leastContradiction <- interestMeasure(k_rule,measure='leastContradiction',transactions = train_data)
try <- as(k_rule,'data.frame')
summary(try$kappa)
summary(try$leastContradiction)
