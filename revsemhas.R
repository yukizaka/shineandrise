
cibi <- read.csv('C:/Users/Florecita/Documents/SKRIPSI/fixdatasetV3.csv')

tele <- read.transactions('C:/Users/Florecita/Documents/SKRIPSI/fixdatasetV3.csv', format = 'basket', sep=',')
vpn <- read.transactions('C:/Users/Florecita/Documents/SKRIPSI/fixdatasetV3.csv', format = 'basket', sep=',')
inspect(vpn)

apriori1 <- apriori(vpn, parameter = list(sup=0.02, conf=0.6, minlen=2))
rs4_r1 = sort(apriori1, by ="lift")
subset.matrix = is.subset(rs4_r1, rs4_r1, sparse = FALSE)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant = colSums(subset.matrix, na.rm=T) >= 1
r4prune1 <- rs4_r1[!redundant]
inspect(r4prune1)

write(r4prune1,
      file = "rulebaru.csv",
      sep = ";",
      quote = TRUE,
      row.names = FALSE)

cibi1 <- read.csv('C:/Users/Florecita/Documents/SKRIPSI/fixdatasetV3.csv')

tele1 <- read.transactions('C:/Users/Florecita/Documents/SKRIPSI/fixdatasetV4.csv', format = 'basket', sep=',')
vpn1 <- read.transactions('C:/Users/Florecita/Documents/SKRIPSI/fixdatasetV5.csv', format = 'basket', sep=',')
inspect(vpn1)

apriori2 <- apriori(vpn1, parameter = list(sup=0.035, conf=0.6, minlen=2))
rs4_r2 = sort(apriori2, by ="lift")
subset.matrix = is.subset(rs4_r2, rs4_r2, sparse = FALSE)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant = colSums(subset.matrix, na.rm=T) >= 1
r4prune2 <- rs4_r2[!redundant]
inspect(r4prune2)

write(r4prune2,
      file = "rulebaruV5.csv",
      sep = ";",
      quote = TRUE,
      row.names = FALSE)


vpn2 <- read.transactions('C:/Users/Florecita/Documents/SKRIPSI/fixdatasetV6.csv', format = 'basket', sep=',')
inspect(vpn2)

apriori3 <- apriori(vpn2, parameter = list(sup=0.03, conf=0.6, minlen=2))
rs4_r3 = sort(apriori3, by ="lift")
subset.matrix = is.subset(rs4_r3, rs4_r3, sparse = FALSE)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant = colSums(subset.matrix, na.rm=T) >= 1
r4prune3 <- rs4_r3[!redundant]
inspect(r4prune3)

write(r4prune3,
      file = "rulebaruV3.csv",
      sep = ";",
      quote = TRUE,
      row.names = FALSE)




