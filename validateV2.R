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
vald$mhsNpm = as.factor(vald$mhsNpm)
glimpse(vald)
summary(vald)

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
write(val_r1,
      file = "val_r1.csv",
      sep = ";",
      quote = TRUE,
      row.names = FALSE)

val_r2 <- apriori(vald, parameter = list(sup=0.2, conf=0.5))
write(val_r2,
      file = "val_r2.csv",
      sep = ";",
      quote = TRUE,
      row.names = FALSE)

val_r3 <- apriori(vald, parameter = list(sup=0.3, conf=0.5))
write(val_r3,
      file = "val_r3.csv",
      sep = ";",
      quote = TRUE,
      row.names = FALSE)

val_r4 <- apriori(vald, parameter = list(sup=0.4, conf=0.5))
write(val_r4,
      file = "val_r4.csv",
      sep = ";",
      quote = TRUE,
      row.names = FALSE)

val_r5 <- apriori(vald, parameter = list(sup=0.5, conf=0.5))
write(val_r5,
      file = "val_r5.csv",
      sep = ";",
      quote = TRUE,
      row.names = FALSE)


#mincof 60% and minsup (1o%,20%,30%,40%,50%)

val2_r1 <- apriori(vald, parameter = list(sup=0.1, conf=0.6))
write(val2_r1,
      file = "val2_r1.csv",
      sep = ";",
      quote = TRUE,
      row.names = FALSE)

val2_r2 <- apriori(vald, parameter = list(sup=0.2, conf=0.6))
write(val2_r2,
      file = "val2_r2.csv",
      sep = ";",
      quote = TRUE,
      row.names = FALSE)

val2_r3 <- apriori(vald, parameter = list(sup=0.3, conf=0.6))
write(val2_r3,
      file = "val2_r3.csv",
      sep = ";",
      quote = TRUE,
      row.names = FALSE)

val2_r4 <- apriori(vald, parameter = list(sup=0.4, conf=0.6))
write(val2_r4,
      file = "val2_r4.csv",
      sep = ";",
      quote = TRUE,
      row.names = FALSE)

val2_r5 <- apriori(vald, parameter = list(sup=0.5, conf=0.6))
write(val2_r5,
      file = "val2_r5.csv",
      sep = ";",
      quote = TRUE,
      row.names = FALSE)

#mincof 70% and minsup (1o%,20%,30%,40%,50%)

val3_r1 <- apriori(vald, parameter = list(sup=0.1, conf=0.7))
write(val3_r1,
      file = "val3_r1.csv",
      sep = ";",
      quote = TRUE,
      row.names = FALSE)

val3_r2 <- apriori(vald, parameter = list(sup=0.2, conf=0.7))
write(val3_r2,
      file = "val3_r2.csv",
      sep = ";",
      quote = TRUE,
      row.names = FALSE)

val3_r3 <- apriori(vald, parameter = list(sup=0.3, conf=0.7))
write(val3_r3,
      file = "val3_r3.csv",
      sep = ";",
      quote = TRUE,
      row.names = FALSE)
val3_r4 <- apriori(vald, parameter = list(sup=0.4, conf=0.7))
write(val3_r4,
      file = "val3_r4.csv",
      sep = ";",
      quote = TRUE,
      row.names = FALSE)

val3_r5 <- apriori(vald, parameter = list(sup=0.5, conf=0.7))
write(val3_r5,
      file = "val3_r5.csv",
      sep = ";",
      quote = TRUE,
      row.names = FALSE)

#akurasi data training
train_rule1 <- apriori(train_data, parameter = list(sup=0.1, conf=0.5))

write(train_rule1,
      file = "traindata1.csv",
      sep = ";",
      quote = TRUE,
      row.names = FALSE)

train_rule2 <- apriori(train_data, parameter = list(sup=0.2, conf=0.5))

write(train_rule2,
      file = "traindata2.csv",
      sep = ";",
      quote = TRUE,
      row.names = FALSE)

train_rule3 <- apriori(train_data, parameter = list(sup=0.3, conf=0.5))
write(train_rule3,
      file = "traindata3.csv",
      sep = ";",
      quote = TRUE,
      row.names = FALSE)

train_rule4 <- apriori(train_data, parameter = list(sup=0.4, conf=0.5))
write(train_rule4,
      file = "traindata4.csv",
      sep = ";",
      quote = TRUE,
      row.names = FALSE)

train_rule5<- apriori(train_data, parameter = list(sup=0.5, conf=0.5))
write(train_rule5,
      file = "traindata5.csv",
      sep = ";",
      quote = TRUE,
      row.names = FALSE)

###########################################################


train_rule6 <- apriori(train_data, parameter = list(sup=0.1, conf=0.6))

write(train_rule6,
      file = "traindata6.csv",
      sep = ";",
      quote = TRUE,
      row.names = FALSE)

train_rule7 <- apriori(train_data, parameter = list(sup=0.2, conf=0.6))

write(train_rule7,
      file = "traindata7.csv",
      sep = ";",
      quote = TRUE,
      row.names = FALSE)

train_rule8 <- apriori(train_data, parameter = list(sup=0.3, conf=0.6))
write(train_rule8,
      file = "traindata8.csv",
      sep = ";",
      quote = TRUE,
      row.names = FALSE)

train_rule9 <- apriori(train_data, parameter = list(sup=0.4, conf=0.6))
write(train_rule9,
      file = "traindata9.csv",
      sep = ";",
      quote = TRUE,
      row.names = FALSE)

train_rule10<- apriori(train_data, parameter = list(sup=0.5, conf=0.6))
write(train_rule10,
      file = "traindata10.csv",
      sep = ";",
      quote = TRUE,
      row.names = FALSE)

###############################################################################

train_rule11 <- apriori(train_data, parameter = list(sup=0.1, conf=0.7))

write(train_rule11,
      file = "traindata11.csv",
      sep = ";",
      quote = TRUE,
      row.names = FALSE)

train_rule12 <- apriori(train_data, parameter = list(sup=0.2, conf=0.7))

write(train_rule12,
      file = "traindata12.csv",
      sep = ";",
      quote = TRUE,
      row.names = FALSE)

train_rule13 <- apriori(train_data, parameter = list(sup=0.3, conf=0.7))
write(train_rule13,
      file = "traindata13.csv",
      sep = ";",
      quote = TRUE,
      row.names = FALSE)

train_rule14 <- apriori(train_data, parameter = list(sup=0.4, conf=0.7))
write(train_rule14,
      file = "traindata14.csv",
      sep = ";",
      quote = TRUE,
      row.names = FALSE)

train_rule15<- apriori(train_data, parameter = list(sup=0.5, conf=0.7))
write(train_rule15,
      file = "traindata15.csv",
      sep = ";",
      quote = TRUE,
      row.names = FALSE)

##############################Akurasi Data Test##########

test_rule1 <- apriori(test_data, parameter = list(sup=0.1, conf=0.5))

write(test_rule1,
      file = "testdata1.csv",
      sep = ";",
      quote = TRUE,
      row.names = FALSE)

test_rule2 <- apriori(test_data, parameter = list(sup=0.2, conf=0.5))

write(test_rule2,
      file = "testdata2.csv",
      sep = ";",
      quote = TRUE,
      row.names = FALSE)

test_rule3 <- apriori(test_data, parameter = list(sup=0.3, conf=0.5))
write(test_rule3,
      file = "testdata3.csv",
      sep = ";",
      quote = TRUE,
      row.names = FALSE)

test_rule4 <- apriori(test_data, parameter = list(sup=0.4, conf=0.5))
write(test_rule4,
      file = "testdata4.csv",
      sep = ";",
      quote = TRUE,
      row.names = FALSE)

test_rule5<- apriori(test_data, parameter = list(sup=0.5, conf=0.5))
write(test_rule5,
      file = "testdata5.csv",
      sep = ";",
      quote = TRUE,
      row.names = FALSE)

###########################################################


test_rule6 <- apriori(test_data, parameter = list(sup=0.1, conf=0.6))

write(test_rule6,
      file = "testdata6.csv",
      sep = ";",
      quote = TRUE,
      row.names = FALSE)

test_rule7 <- apriori(test_data, parameter = list(sup=0.2, conf=0.6))

write(test_rule7,
      file = "testdata7.csv",
      sep = ";",
      quote = TRUE,
      row.names = FALSE)

test_rule8 <- apriori(test_data, parameter = list(sup=0.3, conf=0.6))
write(test_rule8,
      file = "testdata8.csv",
      sep = ";",
      quote = TRUE,
      row.names = FALSE)

test_rule9 <- apriori(test_data, parameter = list(sup=0.4, conf=0.6))
write(test_rule9,
      file = "testdata9.csv",
      sep = ";",
      quote = TRUE,
      row.names = FALSE)

test_rule10<- apriori(test_data, parameter = list(sup=0.5, conf=0.6))
write(test_rule10,
      file = "testdata10.csv",
      sep = ";",
      quote = TRUE,
      row.names = FALSE)

###############################################################################

test_rule11 <- apriori(test_data, parameter = list(sup=0.1, conf=0.7))

write(test_rule11,
      file = "testdata11.csv",
      sep = ";",
      quote = TRUE,
      row.names = FALSE)

test_rule12 <- apriori(test_data, parameter = list(sup=0.2, conf=0.7))

write(test_rule12,
      file = "testdata12.csv",
      sep = ";",
      quote = TRUE,
      row.names = FALSE)

test_rule13 <- apriori(test_data, parameter = list(sup=0.3, conf=0.7))
write(test_rule13,
      file = "testdata13.csv",
      sep = ";",
      quote = TRUE,
      row.names = FALSE)

test_rule14 <- apriori(test_data, parameter = list(sup=0.4, conf=0.7))
write(test_rule14,
      file = "testdata14.csv",
      sep = ";",
      quote = TRUE,
      row.names = FALSE)

test_rule15<- apriori(test_data, parameter = list(sup=0.5, conf=0.7))
write(test_rule15,
      file = "testdata15.csv",
      sep = ";",
      quote = TRUE,
      row.names = FALSE)










write.csv(rfdt, "rfdt.csv", row.names = FALSE)
abc <- interestMeasure(r4prune1, c("rulePowerFactor"), 
                     transactions = train_data)
rs4_r1 = sort(val4_r1, by ="lift")

uio <- apriori(vald, parameter = list(sup=0.05, conf=0.6))
rs4_r1 = sort(uio, by ="lift")
subset.matrix = is.subset(rs4_r1, rs4_r1, sparse = FALSE)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant = colSums(subset.matrix, na.rm=T) >= 1

#remove redundant rules
r4prune1 <- rs4_r1[!redundant]
inspect(r4prune1)


write(r4prune1,
      file = "r4prune1.csv",
      sep = ";",
      quote = TRUE,
      row.names = FALSE)

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
