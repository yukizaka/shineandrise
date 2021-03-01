library(arules)
library(arulesViz)
library(readxl)
library(readr)
library(dplyr)
library(caret)
library(caTools)
install.packages("xlsx")
library(xlsx)

vald <- read.csv('C:/Users/Florecita/Documents/SKRIPSI/fixdataset_nv_edit.csv')
testapriori1 <- apriori(vald, parameter = list(supp=0.05, conf=0.6))
tp <- interestMeasure(r4prune1, c("rulePowerFactor"), 
                        transactions = vald)
tp
write.csv(tp, "tp.csv", row.names = FALSE)
rs4_r1 = sort(testapriori1, by ="lift")
subset.matrix = is.subset(rs4_r1, rs4_r1, sparse = FALSE)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant = colSums(subset.matrix, na.rm=T) >= 1

#remove redundant rules
r4prune1 <- rs4_r1[!redundant]
inspect(r4prune1)

vald <- read.transactions('C:/Users/Florecita/Documents/SKRIPSI/fixdataset_nv_edit.csv', format = 'basket', sep=',')

vold <- read.csv('C:/Users/Florecita/Documents/SKRIPSI/fixdataset_nv_edit.csv')
glimpse(vold)
vold$mhsNpm = as.factor(vold$mhsNpm)
str(vold$mhsNpm)

set.seed(123)
split = sample.split(vold$mhsNpm, SplitRatio = 0.70)

train = subset(vold, split == TRUE)
test = subset(vold, split == FALSE)

dim(train); dim(test)

write.csv(train, "uji_train.csv", row.names = FALSE)
write.csv(test, "uji_test.csv", row.names = FALSE)

train_rule1 <- apriori(train_data, parameter = list(sup=0.1, conf=0.5))
tr1 <- interestMeasure(train_rule1, c("rulePowerFactor"), 
                        transactions = train_data)

train_data <- read.transactions('C:/Users/Florecita/Documents/SKRIPSI/ProcessingData/shineandrise/uji_train.csv', format = 'basket', sep=',')
test_data <- read.transactions('C:/Users/Florecita/Documents/SKRIPSI/ProcessingData/shineandrise/uji_test.csv', format = 'basket', sep=',')


#mincof 50% and minsup (1o%,20%,30%,40%,50%)

val_r1 <- apriori(vald, parameter = list(sup=0.1, conf=0.5, target = "frequent itemsets"))
inspect(val_r1)

val_r2 <- apriori(vald, parameter = list(sup=0.2, conf=0.5, target = "frequent itemsets"))
inspect(val_r2)

val_r3 <- apriori(vald, parameter = list(sup=0.3, conf=0.5, target = "frequent itemsets"))
inspect(val_r3)

val_r4 <- apriori(vald, parameter = list(sup=0.4, conf=0.5, target = "frequent itemsets"))
inspect(val_r4)

val_r5 <- apriori(vald, parameter = list(sup=0.5, conf=0.5, target = "frequent itemsets"))
inspect(val_r5)


#mincof 60% and minsup (1o%,20%,30%,40%,50%)

val2_r1 <- apriori(vald, parameter = list(sup=0.1, conf=0.6, target = "frequent itemsets"))
inspect(val2_r1)

val2_r2 <- apriori(vald, parameter = list(sup=0.2, conf=0.6, target = "frequent itemsets"))
inspect(val2_r2)

val2_r3 <- apriori(vald, parameter = list(sup=0.3, conf=0.6, target = "frequent itemsets"))
inspect(val2_r3)

val2_r4 <- apriori(vald, parameter = list(sup=0.4, conf=0.6, target = "frequent itemsets"))
inspect(val2_r4)

val2_r5 <- apriori(vald, parameter = list(sup=0.5, conf=0.6, target = "frequent itemsets"))
inspect(val2_r5)

#mincof 70% and minsup (1o%,20%,30%,40%,50%)

val3_r1 <- apriori(vald, parameter = list(sup=0.1, conf=0.7, target = "frequent itemsets"))
inspect(val3_r1)

val3_r2 <- apriori(vald, parameter = list(sup=0.2, conf=0.7, target = "frequent itemsets"))
inspect(val3_r2)

val3_r3 <- apriori(vald, parameter = list(sup=0.3, conf=0.7, target = "frequent itemsets"))
inspect(val3_r3)

val3_r4 <- apriori(vald, parameter = list(sup=0.4, conf=0.7, target = "frequent itemsets"))
inspect(val3_r4)

val3_r5 <- apriori(vald, parameter = list(sup=0.5, conf=0.7, target = "frequent itemsets"))
inspect(val3_r5)

########################################Seacrh For Rule####################################

#mincof 50% and minsup (1o%,20%,30%,40%,50%)

val_r1 <- apriori(vald, parameter = list(sup=0.1, conf=0.5))

val_r2 <- apriori(vald, parameter = list(sup=0.2, conf=0.5))

val_r3 <- apriori(vald, parameter = list(sup=0.3, conf=0.5))

val_r4 <- apriori(vald, parameter = list(sup=0.4, conf=0.5))

val_r5 <- apriori(vald, parameter = list(sup=0.5, conf=0.5))


#mincof 60% and minsup (1o%,20%,30%,40%,50%)

val2_r1 <- apriori(vald, parameter = list(sup=0.1, conf=0.6))

val2_r2 <- apriori(vald, parameter = list(sup=0.2, conf=0.6))

val2_r3 <- apriori(vald, parameter = list(sup=0.3, conf=0.6))

val2_r4 <- apriori(vald, parameter = list(sup=0.4, conf=0.6))

val2_r5 <- apriori(vald, parameter = list(sup=0.5, conf=0.6))


#mincof 70% and minsup (1o%,20%,30%,40%,50%)

val3_r1 <- apriori(vald, parameter = list(sup=0.1, conf=0.7))

val3_r2 <- apriori(vald, parameter = list(sup=0.2, conf=0.7))

val3_r3 <- apriori(vald, parameter = list(sup=0.3, conf=0.7))

val3_r4 <- apriori(vald, parameter = list(sup=0.4, conf=0.7))

val3_r5 <- apriori(vald, parameter = list(sup=0.5, conf=0.7))


#akurasi data training
va1_r1 <- apriori(test_data, parameter = list(sup=0.1, conf=0.5))
val4_r1 <- apriori(train_data, parameter = list(sup=0.1, conf=0.5))
options(max.print=100000)
inspect(val4_r1)
summary(val4_r1)

rfdt <- interestMeasure(va1_r1, c("rulePowerFactor"), 
                       transactions = test_data)
write.csv(rfdt, "rfdt.csv", row.names = FALSE)
abc <- interestMeasure(r4prune1, c("rulePowerFactor"), 
                     transactions = train_data)
rs4_r1 = sort(val4_r1, by ="lift")
subset.matrix = is.subset(rs4_r1, rs4_r1, sparse = FALSE)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant = colSums(subset.matrix, na.rm=T) >= 1

#remove redundant rules
r4prune1 <- rs4_r1[!redundant]
inspect(r4prune1)

head(abc)
abc
write.csv(abc, "abc.csv", row.names = FALSE)

val4_r2 <- apriori(train_data, parameter = list(sup=0.1, conf=0.6))
abc1 <- interestMeasure(val4_r2, c("rulePowerFactor"), 
                       transactions = train_data)
write.csv(abc1, "abc1.csv", row.names = FALSE)

val4_r4 <- apriori(train_data, parameter = list(sup=0.1, conf=0.7))
abc4 <- interestMeasure(val4_r4, c("rulePowerFactor"),
                        transactions = train_data)
abc4

write.csv(abc4, "abc4.csv", row.names = FALSE)
#find redundant rules
# rs_r1 = sort(val_r1, by ="lift")
# subset.matrix = is.subset(rs_r1, rs_r1, sparse = FALSE)
# subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
# redundant = colSums(subset.matrix, na.rm=T) >= 1
# 
# #remove redundant rules
# rprune1 <- rs_r1[!redundant]
# interdata <- inspect(rprune1)
