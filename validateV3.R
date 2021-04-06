library(arules)
library(arulesViz)
library(readxl)
library(readr)
library(dplyr)
library(caret)
library(caTools)
install.packages("xlsx")
library(xlsx)



datasetbaru<- read_excel('C:/Users/Florecita/Documents/SKRIPSI/fixdatasetV2.xlsx')

datasetbaru <- datasetbaru[complete.cases(datasetbaru), ]

write.csv(datasetbaru,"C:/Users/Florecita/Documents/SKRIPSI/fixdatasetV2.csv", quote = FALSE, row.names = FALSE)

dst <- read.csv('C:/Users/Florecita/Documents/SKRIPSI/fixdatasetV2.csv')

glimpse(dst)
dst$Id = as.factor(dst$Id)
str(dst$Id)

dst <- read.transactions('C:/Users/Florecita/Documents/SKRIPSI/fixdatasetV2.csv', format = 'basket', sep=',')

newapriori <- apriori(dst, parameter = list(sup=0.01, conf=0.6, minlen=2))
rs4_r1 = sort(newapriori, by ="lift")
subset.matrix = is.subset(rs4_r1, rs4_r1, sparse = FALSE)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant = colSums(subset.matrix, na.rm=T) >= 1
r4prune1 <- rs4_r1[!redundant]
inspect(r4prune1)

write(r4prune1,
      file = "newapriori1.csv",
      sep = ";",
      quote = TRUE,
      row.names = FALSE)



vild <- read.csv('C:/Users/Florecita/Documents/SKRIPSI/fixdatasetV2.csv')
glimpse(vild)
vild$Id = as.factor(vild$Id)
str(vild$Id)

vild <- read.transactions('C:/Users/Florecita/Documents/SKRIPSI/fixdatasetV2.csv', format = 'basket', sep=',')

set.seed(123)
split = sample.split(vild$Id, SplitRatio = 0.70)

train2 = subset(vild, split == TRUE)
test2 = subset(vild, split == FALSE)

dim(train2); dim(test2)

write.csv(train2, "uji_train2.csv", row.names = FALSE)
write.csv(test2, "uji_test2.csv", row.names = FALSE)

train_data2 <- read.transactions('C:/Users/Florecita/Documents/SKRIPSI/ProcessingData/shineandrise/uji_train2.csv', format = 'basket', sep=',')
test_data2 <- read.transactions('C:/Users/Florecita/Documents/SKRIPSI/ProcessingData/shineandrise/uji_test2.csv', format = 'basket', sep=',')

#########################################################################################################

vild_r1 <- apriori(vild, parameter = list(sup=0.1, conf=0.5, target = "frequent itemsets"))
inspect(vild_r1)

vild_r2 <- apriori(vild, parameter = list(sup=0.2, conf=0.5, target = "frequent itemsets"))
inspect(vild_r2)

vild_r3 <- apriori(vild, parameter = list(sup=0.3, conf=0.5, target = "frequent itemsets"))
inspect(vild_r3)

vild_r4 <- apriori(vild, parameter = list(sup=0.4, conf=0.5, target = "frequent itemsets"))
inspect(vild_r4)

vild_r5 <- apriori(vild, parameter = list(sup=0.5, conf=0.5, target = "frequent itemsets"))
inspect(vild_r5)


#mincof 60% and minsup (1o%,20%,30%,40%,50%)

vild2_r1 <- apriori(vild, parameter = list(sup=0.1, conf=0.6, target = "frequent itemsets"))
inspect(vild2_r1)

vild2_r2 <- apriori(vild, parameter = list(sup=0.2, conf=0.6, target = "frequent itemsets"))
inspect(vild2_r2)

vild2_r3 <- apriori(vild, parameter = list(sup=0.3, conf=0.6, target = "frequent itemsets"))
inspect(vild2_r3)

vild2_r4 <- apriori(vild, parameter = list(sup=0.4, conf=0.6, target = "frequent itemsets"))
inspect(vild2_r4)

vild2_r5 <- apriori(vild, parameter = list(sup=0.5, conf=0.6, target = "frequent itemsets"))
inspect(vild2_r5)

#mincof 70% and minsup (1o%,20%,30%,40%,50%)

vild3_r1 <- apriori(vild, parameter = list(sup=0.1, conf=0.7, target = "frequent itemsets"))
inspect(vild3_r1)

vild3_r2 <- apriori(vild, parameter = list(sup=0.2, conf=0.7, target = "frequent itemsets"))
inspect(vild3_r2)

vild3_r3 <- apriori(vild, parameter = list(sup=0.3, conf=0.7, target = "frequent itemsets"))
inspect(vild3_r3)

vild3_r4 <- apriori(vild, parameter = list(sup=0.4, conf=0.7, target = "frequent itemsets"))
inspect(vild3_r4)

vild3_r5 <- apriori(vild, parameter = list(sup=0.5, conf=0.7, target = "frequent itemsets"))
inspect(vild3_r5)

#mincof 80% and minsup (1o%,20%,30%,40%,50%)

vild4_r1 <- apriori(vild, parameter = list(sup=0.1, conf=0.8, target = "frequent itemsets"))
inspect(vild4_r1)

vild4_r2 <- apriori(vild, parameter = list(sup=0.2, conf=0.8, target = "frequent itemsets"))
inspect(vild4_r2)

vild4_r3 <- apriori(vild, parameter = list(sup=0.3, conf=0.8, target = "frequent itemsets"))
inspect(vild4_r3)

vild4_r4 <- apriori(vild, parameter = list(sup=0.4, conf=0.8, target = "frequent itemsets"))
inspect(vild4_r4)

vild4_r5 <- apriori(vild, parameter = list(sup=0.5, conf=0.8, target = "frequent itemsets"))
inspect(vild4_r5)

##############################################################################################
val_r1 <- apriori(vild, parameter = list(sup=0.1, conf=0.5))
write(val_r1,
      file = "val_r1.csv",
      sep = ";",
      quote = TRUE,
      row.names = FALSE)

val_r2 <- apriori(vild, parameter = list(sup=0.2, conf=0.5))
write(val_r2,
      file = "val_r2.csv",
      sep = ";",
      quote = TRUE,
      row.names = FALSE)

val_r3 <- apriori(vild, parameter = list(sup=0.3, conf=0.5))
write(val_r3,
      file = "val_r3.csv",
      sep = ";",
      quote = TRUE,
      row.names = FALSE)

val_r4 <- apriori(vild, parameter = list(sup=0.4, conf=0.5))
write(val_r4,
      file = "val_r4.csv",
      sep = ";",
      quote = TRUE,
      row.names = FALSE)

val_r5 <- apriori(vild, parameter = list(sup=0.5, conf=0.5))
write(val_r5,
      file = "val_r5.csv",
      sep = ";",
      quote = TRUE,
      row.names = FALSE)


#mincof 60% and minsup (1o%,20%,30%,40%,50%)

val2_r1 <- apriori(vild, parameter = list(sup=0.1, conf=0.6))
write(val2_r1,
      file = "val2_r1.csv",
      sep = ";",
      quote = TRUE,
      row.names = FALSE)

val2_r2 <- apriori(vild, parameter = list(sup=0.2, conf=0.6))
write(val2_r2,
      file = "val2_r2.csv",
      sep = ";",
      quote = TRUE,
      row.names = FALSE)

val2_r3 <- apriori(vild, parameter = list(sup=0.3, conf=0.6))
write(val2_r3,
      file = "val2_r3.csv",
      sep = ";",
      quote = TRUE,
      row.names = FALSE)

val2_r4 <- apriori(vild, parameter = list(sup=0.4, conf=0.6))
write(val2_r4,
      file = "val2_r4.csv",
      sep = ";",
      quote = TRUE,
      row.names = FALSE)

val2_r5 <- apriori(vild, parameter = list(sup=0.5, conf=0.6))
write(val2_r5,
      file = "val2_r5.csv",
      sep = ";",
      quote = TRUE,
      row.names = FALSE)

#mincof 70% and minsup (1o%,20%,30%,40%,50%)

val3_r1 <- apriori(vild, parameter = list(sup=0.1, conf=0.7))
write(val3_r1,
      file = "val3_r1.csv",
      sep = ";",
      quote = TRUE,
      row.names = FALSE)

val3_r2 <- apriori(vild, parameter = list(sup=0.2, conf=0.7))
write(val3_r2,
      file = "val3_r2.csv",
      sep = ";",
      quote = TRUE,
      row.names = FALSE)

val3_r3 <- apriori(vild, parameter = list(sup=0.3, conf=0.7))
write(val3_r3,
      file = "val3_r3.csv",
      sep = ";",
      quote = TRUE,
      row.names = FALSE)
val3_r4 <- apriori(vild, parameter = list(sup=0.4, conf=0.7))
write(val3_r4,
      file = "val3_r4.csv",
      sep = ";",
      quote = TRUE,
      row.names = FALSE)

val3_r5 <- apriori(vild, parameter = list(sup=0.5, conf=0.7))
write(val3_r5,
      file = "val3_r5.csv",
      sep = ";",
      quote = TRUE,
      row.names = FALSE)

#mincof 80% and minsup (1o%,20%,30%,40%,50%)

val4_r1 <- apriori(vild, parameter = list(sup=0.1, conf=0.8))
write(val4_r1,
      file = "val4_r1.csv",
      sep = ";",
      quote = TRUE,
      row.names = FALSE)

val4_r2 <- apriori(vild, parameter = list(sup=0.2, conf=0.8))
write(val4_r2,
      file = "val4_r2.csv",
      sep = ";",
      quote = TRUE,
      row.names = FALSE)

val4_r3 <- apriori(vild, parameter = list(sup=0.3, conf=0.8))
write(val4_r3,
      file = "val4_r3.csv",
      sep = ";",
      quote = TRUE,
      row.names = FALSE)
val4_r4 <- apriori(vild, parameter = list(sup=0.4, conf=0.8))
write(val4_r4,
      file = "val4_r4.csv",
      sep = ";",
      quote = TRUE,
      row.names = FALSE)

val4_r5 <- apriori(vild, parameter = list(sup=0.5, conf=0.8))
write(val4_r5,
      file = "val4_r5.csv",
      sep = ";",
      quote = TRUE,
      row.names = FALSE)

#akurasi data training
train_rule1 <- apriori(train_data2, parameter = list(sup=0.1, conf=0.5))
tr1 <- interestMeasure(train_rule1, c("Conviction"), 
                       transactions = vild)
write.csv(tr1, "tr1.csv", row.names = FALSE)

train_rule2 <- apriori(train_data2, parameter = list(sup=0.2, conf=0.5))
tr2 <- interestMeasure(train_rule2, c("Conviction"), 
                       transactions = vild)
write.csv(tr2, "tr2.csv", row.names = FALSE)


train_rule3 <- apriori(train_data2, parameter = list(sup=0.3, conf=0.5))
tr3 <- interestMeasure(train_rule3, c("Conviction"), 
                       transactions = vild)
write.csv(tr3, "tr3.csv", row.names = FALSE)


train_rule4 <- apriori(train_data2, parameter = list(sup=0.4, conf=0.5))
tr4 <- interestMeasure(train_rule4, c("Conviction"), 
                       transactions = vild)
write.csv(tr4, "tr4.csv", row.names = FALSE)

train_rule5<- apriori(train_data2, parameter = list(sup=0.5, conf=0.5))
tr5 <- interestMeasure(train_rule5, c("Conviction"), 
                       transactions = vild)
write.csv(tr5, "tr5.csv", row.names = FALSE)

###########################################################


train_rule6 <- apriori(train_data2, parameter = list(sup=0.1, conf=0.6))
tr6 <- interestMeasure(train_rule6, c("Conviction"), 
                       transactions = vild)
write.csv(tr6, "tr6.csv", row.names = FALSE)


train_rule7 <- apriori(train_data2, parameter = list(sup=0.2, conf=0.6))
tr7 <- interestMeasure(train_rule7, c("Conviction"), 
                       transactions = vild)
write.csv(tr7, "tr7.csv", row.names = FALSE)


train_rule8 <- apriori(train_data2, parameter = list(sup=0.3, conf=0.6))
tr8 <- interestMeasure(train_rule8, c("Conviction"), 
                       transactions = vild)
write.csv(tr8, "tr8.csv", row.names = FALSE)


train_rule9 <- apriori(train_data2, parameter = list(sup=0.4, conf=0.6))
tr9 <- interestMeasure(train_rule9, c("Conviction"), 
                       transactions = vild)
write.csv(tr9, "tr9.csv", row.names = FALSE)


train_rule10<- apriori(train_data2, parameter = list(sup=0.5, conf=0.6))
inspect(train_rule10)
tr10 <- interestMeasure(train_rule10, c("Conviction"), 
                       transactions = vild)
write.csv(tr10, "tr10.csv", row.names = FALSE)

###############################################################################

train_rule11 <- apriori(train_data2, parameter = list(sup=0.1, conf=0.7))
tr11 <- interestMeasure(train_rule11, c("Conviction"), 
                        transactions = vild)
write.csv(tr11, "tr11.csv", row.names = FALSE)


train_rule12 <- apriori(train_data2, parameter = list(sup=0.2, conf=0.7))
tr12 <- interestMeasure(train_rule12, c("Conviction"), 
                        transactions = vild)
write.csv(tr12, "tr12.csv", row.names = FALSE)


train_rule13 <- apriori(train_data2, parameter = list(sup=0.3, conf=0.7))
tr13 <- interestMeasure(train_rule13, c("Conviction"), 
                        transactions = vild)
write.csv(tr13, "tr13.csv", row.names = FALSE)


train_rule14 <- apriori(train_data2, parameter = list(sup=0.4, conf=0.7))
tr14 <- interestMeasure(train_rule14, c("Conviction"), 
                        transactions = vild)
write.csv(tr14, "tr14.csv", row.names = FALSE)



train_rule15<- apriori(train_data2, parameter = list(sup=0.5, conf=0.7))
tr15 <- interestMeasure(train_rule15, c("Conviction"), 
                        transactions = vild)
write.csv(tr15, "tr15.csv", row.names = FALSE)
################################################################################
train_rule16 <- apriori(train_data2, parameter = list(sup=0.1, conf=0.8))
tr16 <- interestMeasure(train_rule16, c("Conviction"), 
                        transactions = vild)
write.csv(tr16, "tr16.csv", row.names = FALSE)


train_rule17 <- apriori(train_data2, parameter = list(sup=0.2, conf=0.8))
tr17 <- interestMeasure(train_rule17, c("Conviction"), 
                        transactions = vild)
tr17
write.csv(tr17, "tr17.csv", row.names = FALSE)


train_rule18 <- apriori(train_data2, parameter = list(sup=0.3, conf=0.8, minlen=2))
tr18 <- interestMeasure(train_rule18, c("Conviction"), 
                        transactions = vild)
tr18
write.csv(tr18, "tr18.csv", row.names = FALSE)


train_rule19 <- apriori(train_data2, parameter = list(sup=0.4, conf=0.8))
tr19 <- interestMeasure(train_rule19, c("Conviction"), 
                        transactions = vild)
tr19
inspect(train_rule19)
write.csv(tr19, "tr19.csv", row.names = FALSE)


train_rule20<- apriori(train_data2, parameter = list(sup=0.5, conf=0.8))
tr20 <- interestMeasure(train_rule20, c("Conviction"), 
                        transactions = vild)
tr20
write.csv(tr20, "tr20.csv", row.names = FALSE)
##############################Akurasi Data Test##########

test_rule1 <- apriori(test_data2, parameter = list(sup=0.1, conf=0.5))
ts1 <- interestMeasure(test_rule1, c("Conviction"), 
                       transactions = vild)
write.csv(ts1, "ts1.csv", row.names = FALSE)


test_rule2 <- apriori(test_data2, parameter = list(sup=0.2, conf=0.5))
ts2 <- interestMeasure(test_rule2, c("Conviction"), 
                       transactions = vild)
write.csv(ts2, "ts2.csv", row.names = FALSE)


test_rule3 <- apriori(test_data2, parameter = list(sup=0.3, conf=0.5))
ts3 <- interestMeasure(test_rule3, c("Conviction"), 
                       transactions = vild)
write.csv(ts3, "ts3.csv", row.names = FALSE)


test_rule4 <- apriori(test_data2, parameter = list(sup=0.4, conf=0.5))
ts4 <- interestMeasure(test_rule4, c("Conviction"), 
                       transactions = vild)
write.csv(ts4, "ts4.csv", row.names = FALSE)


test_rule5<- apriori(test_data2, parameter = list(sup=0.5, conf=0.5))
ts5 <- interestMeasure(test_rule5, c("Conviction"), 
                       transactions = vild)
ts5
inspect(test_rule5)
write.csv(ts5, "ts5.csv", row.names = FALSE)

###########################################################


test_rule6 <- apriori(test_data2, parameter = list(sup=0.1, conf=0.6))
ts6 <- interestMeasure(test_rule6, c("Conviction"), 
                       transactions = vild)
write.csv(ts6, "ts6.csv", row.names = FALSE)

test_rule7 <- apriori(test_data2, parameter = list(sup=0.2, conf=0.6))
ts7 <- interestMeasure(test_rule7, c("Conviction"), 
                       transactions = vild)
write.csv(ts7, "ts7.csv", row.names = FALSE)

test_rule8 <- apriori(test_data2, parameter = list(sup=0.3, conf=0.6))
ts8 <- interestMeasure(test_rule8, c("Conviction"), 
                       transactions = vild)
write.csv(ts8, "ts8.csv", row.names = FALSE)

test_rule9 <- apriori(test_data2, parameter = list(sup=0.4, conf=0.6))
ts9 <- interestMeasure(test_rule9, c("Conviction"), 
                       transactions = vild)
write.csv(ts9, "ts9.csv", row.names = FALSE)

test_rule10<- apriori(test_data2, parameter = list(sup=0.5, conf=0.6))
ts10 <- interestMeasure(test_rule10, c("Conviction"), 
                       transactions = vild)
write.csv(ts10, "ts10.csv", row.names = FALSE)

###############################################################################

test_rule11 <- apriori(test_data2, parameter = list(sup=0.1, conf=0.7))
ts11 <- interestMeasure(test_rule11, c("Conviction"), 
                        transactions = vild)
write.csv(ts11, "ts11.csv", row.names = FALSE)

test_rule12 <- apriori(test_data2, parameter = list(sup=0.2, conf=0.7))
ts12 <- interestMeasure(test_rule12, c("Conviction"), 
                        transactions = vild)
write.csv(ts12, "ts12.csv", row.names = FALSE)

test_rule13 <- apriori(test_data2, parameter = list(sup=0.3, conf=0.7))
ts13 <- interestMeasure(test_rule13, c("Conviction"), 
                        transactions = vild)
write.csv(ts13, "ts13.csv", row.names = FALSE)

test_rule14 <- apriori(test_data2, parameter = list(sup=0.4, conf=0.7))
ts14 <- interestMeasure(test_rule14, c("Conviction"), 
                        transactions = vild)
write.csv(ts14, "ts14.csv", row.names = FALSE)

test_rule15<- apriori(test_data2, parameter = list(sup=0.5, conf=0.7))
ts15 <- interestMeasure(test_rule15, c("Conviction"), 
                        transactions = vild)
write.csv(ts15, "ts15.csv", row.names = FALSE)


##############################################################################

test_rule16 <- apriori(test_data2, parameter = list(sup=0.1, conf=0.8))
ts16 <- interestMeasure(test_rule16, c("Conviction"), 
                        transactions = vild)
write.csv(ts16, "ts16.csv", row.names = FALSE)

test_rule17 <- apriori(test_data2, parameter = list(sup=0.2, conf=0.8))
ts17 <- interestMeasure(test_rule17, c("Conviction"), 
                        transactions = vild)
write.csv(ts17, "ts17.csv", row.names = FALSE)

test_rule18 <- apriori(test_data2, parameter = list(sup=0.3, conf=0.8))
ts18 <- interestMeasure(test_rule18, c("Conviction"), 
                        transactions = vild)
write.csv(ts18, "ts18.csv", row.names = FALSE)

test_rule19 <- apriori(test_data2, parameter = list(sup=0.4, conf=0.8))
ts19 <- interestMeasure(test_rule19, c("Conviction"), 
                        transactions = vild)
write.csv(ts19, "ts19.csv", row.names = FALSE)

test_rule20<- apriori(test_data2, parameter = list(sup=0.5, conf=0.8))
ts20 <- interestMeasure(test_rule20, c("Conviction"), 
                        transactions = vild)
write.csv(ts20, "ts20.csv", row.names = FALSE)
