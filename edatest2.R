library(readxl)
library(readr)
library(dplyr)
library(caret)
library(caTools)

cibs <- read.csv('C:/Users/Florecita/Documents/SKRIPSI/fixdatasetV5.csv')
tly1 <- read.transactions('C:/Users/Florecita/Documents/SKRIPSI/fixdatasetV5.csv', format = 'basket', sep=',')
tly1


cibs$Id <- as.factor(cibs$id)
cibs$JenisKelamin <- factor(cibs$JenisKelamin, levels = c('Lk','Pr'))
cibs$Ipk <- factor(cibs$Ipk, levels = c('P','SM', 'M', 'TP'))
cibs$LamaStudi <- factor(cibs$LamaStudi, levels = c('A', 'B', 'C', 'D', 'E'))
cibs$JalurMasuk <- factor(cibs$JalurMasuk, levels = c('SNMPTN', 'SBMPTN', 'SPMU'))
cibs$Prodi <- factor(cibs$Prodi, levels = c('Informatika', 'Teknik Sipil', 'Teknik Elektro', 'Teknik Mesin'))
cibs$AsalSekolah <- factor(cibs$AsalSekolah, levels = c('A1', 'A2', 'A3'))


pte = cibs[cibs$Prodi == 'Teknik Elektro',]
hlk = cibs[cibs$JenisKelamin == 'Lk',]
pte
head(pte)
cibs_fq1 = cibs %>% select(Ipk, JenisKelamin) %>% group_by(JenisKelamin)  %>% distinct()
head(cibi_prodiipk)
top_15 = head(cibi_prodiipk, 15)
top_15
cibi_prodiipk
summary(cibi_prodiipk$Prodi)
options(scipen = 10000)



top_sekolah = cibi %>% count(AsalSekolah)
head(top_sekolah)
top_sekolah
top_10 = head(top_sekolah, 10)
top_10

best_sekolah = dataset[dataset$AsalSekolah == 'SMKN 2 KOTA BENGKULU',]
head(best_sekolah)
genderDist_aj = ggplot(data = pte) + 
  geom_bar(mapping = aes(x = JenisKelamin, y = ..count.., fill = Ipk)) + 
  labs(title = 'Ipk dan Jenis Kelamin', x='IPK', fill='Jenis Kelamin') +
  geom_bar(position="dodge", stat="identity") +
  scale_fill_brewer(palette = 'Paired')+
  facet_wrap(~JenisKelamin)
  print(genderDist_aj)

cibi_jkjmls = cibi %>% select(Ipk, JenisKelamin, best_sekolah) %>% group_by(JenisKelamin) %>% count(Ipk, best_sekolah) %>% distinct()

cibi_jkjmls2 = dataset %>% select(Ipk, JenisKelamin, best_sekolah) %>% group_by(JenisKelamin) %>% count(Ipk, best_sekolah) %>% distinct()

genderbestsekolah = ggplot(data= best_sekolah, aes(fill=JenisKelamin, x=Ipk, y = ..count.., legend = TRUE )) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_viridis(discrete = T, option = "E") +
  ggtitle("Ipk dan Jenis Kelamin pada SMKN 2 Kota Bengkulu") +
  geom_text(hjust = 0.5, size = 4, position = position_dodge(width = .75), vjust = 2) +
  facet_wrap(~JenisKelamin) +
  theme_ipsum() +
  theme(legend.position="bottom") +
  xlab("")

print(genderbestsekolah)

cibi_jklama = cibi %>% select(JenisKelamin, LamaStudi) %>% group_by(LamaStudi) %>% count(JenisKelamin) %>% arrange(desc(JenisKelamin)) %>% distinct()
cibi_jklama

genderDist_jklama = ggplot(data = cibi_jklama) + 
  geom_bar(mapping = aes(x = LamaStudi, fill = JenisKelamin)) + 
  labs(title = 'Jenis Kelamin') + 
  scale_fill_brewer(palette = 'Paired')

print(genderDist_jklama)

genderDist_jklama = ggplot(data = cibi_jklama, aes(x = LamaStudi))+
  geom_bar(stat = 'count', fill = 'mediumseagreen', 
           width = 0.5)+
  stat_count(geom = 'text', size = 4,
             aes(label = ..count..),
             position = position_stack(vjust = 1.03))+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 16, face="bold"))+
  labs(title = sprintf('Frecuency plot of the variable '),
       x = LamaStudi, y = 'Count')

cibi_edals = cibi %>% select(LamaStudi, Ipk, JenisKelamin) %>% group_by(Ipk) %>% count(JenisKelamin, LamaStudi) %>% distinct()
cibi_edals

# ggplot(data=cibi_edals, aes(x = Ipk, y = n)) +
#   geom_bar(
#     aes(fill = LamaStudi), stat = "identity", color = "white",
#     position = position_dodge(0.9)
#   ) +
#   facet_wrap(~JenisKelamin) + 
#   fill_palette("jco")

genderDist_eda= ggplot(data=cibi_edals, aes(fill=Ipk, y=n, x=JenisKelamin, label = n, legend = TRUE)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_viridis(discrete = T, option = "E") +
  scale_fill_brewer(palette = 'Paired') +
  ggtitle("Lama Studi berdasarkan IPK dan Jenis Kelamin Mahasiswa") +
  geom_text(hjust = 0.5, size = 4, position = position_dodge(width = .75), vjust = 2) +
  facet_wrap(~LamaStudi) +
  theme_ipsum() +
  theme(legend.position="bottom") +
  xlab("")

print(genderDist_eda)

cibi_jm = cibi %>% select(LamaStudi, Ipk, JalurMasuk) %>% group_by(JalurMasuk) %>% count(Ipk, LamaStudi) %>% distinct()
cibi_jm

genderDist_lsjmipk= ggplot(data=cibi_jm, aes(fill=JalurMasuk, y=n, x=LamaStudi, label = n, legend = TRUE)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_viridis(discrete = T, option = "E") +
  scale_fill_brewer(palette = 'Paired') +
  ggtitle("Lama Studi berdasarkan IPK dan Jalur Masuk Mahasiswa") +
  geom_text(hjust = 0.5, size = 4, position = position_dodge(width = .75), vjust = 2) +
  facet_wrap(~Ipk) +
  theme_ipsum() +
  theme(legend.position="bottom") +
  xlab("")

print(genderDist_lsjmipk)

cibi_jkjmls = cibi %>% select(LamaStudi, JenisKelamin, JalurMasuk) %>% group_by(JenisKelamin) %>% count(JalurMasuk, LamaStudi) %>% distinct()
cibi_jkjmls

genderDist_jkjmls= ggplot(data=cibi_jkjmls, aes(fill=JalurMasuk, x=LamaStudi, y = n, label = n, legend = TRUE )) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_viridis(discrete = T, option = "E") +
  scale_fill_brewer(palette = 'Paired') +
  ggtitle("Lama Studi berdasarkan JenisKelamin dan Jalur Masuk Mahasiswa") +
  geom_text(hjust = 0.5, size = 4, position = position_dodge(width = .75), vjust = 2) +
  facet_wrap(~JenisKelamin) +
  theme_ipsum() +
  theme(legend.position="bottom") +
  xlab("")

print(genderDist_jkjmls)


cibi_prodiipk = cibi %>% select(Ipk, Prodi) %>% group_by(Prodi) %>% count(Ipk) %>% arrange(desc(Ipk)) %>% distinct()





