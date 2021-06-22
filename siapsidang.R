library(readxl)
library(readr)
library(dplyr)
library(caret)
library(caTools)

theme_set(theme_pubr())
lala1 <- read.csv('C:/Users/Florecita/Documents/SKRIPSI/fixdatasetV5.csv')

cubi = lala1 %>% select(JenisKelamin, LamaStudi) %>% group_by(LamaStudi) %>% count(JenisKelamin) %>% arrange(desc(JenisKelamin)) %>% distinct()
cubi

ggplot(data = cubi, aes(x = LamaStudi, y = n, label = n))+
  geom_bar(
    aes(fill = JenisKelamin), stat = "identity", color = "white",
    position = position_dodge(0.9)
  )+
  xlab("Lama Studi") + 
  ylab("Jumlah Mahasiswa") + 
  labs(fill = "Jenis Kelamin") +
  geom_text(aes(label = n), position = position_dodge(-1), size=6) +
  fill_palette("jco")

cubi = lala1 %>% select(JenisKelamin, LamaStudi) %>% group_by(LamaStudi) %>% count(JenisKelamin) %>% arrange(desc(JenisKelamin)) %>% distinct()

tpi = cibs[cibs$Prodi == 'Teknik Elektro',]
tpi2 = cibs[cibs$JenisKelamin == 'Lk',]

cibs_ptm = tpi %>% select(JenisKelamin, Ipk) %>% group_by(JenisKelamin) %>% count(Ipk) %>% distinct()
cibs_ptm
gencibs_ptm = ggplot(data=cibs_ptm, aes(fill=Ipk, x=JenisKelamin, y = n, label = n, legend = TRUE )) + 
  geom_bar(position="dodge", stat="identity") +
  labs(x='Jenis Kelamin') +
  theme_minimal() +
  ggtitle("Predikat IPK Mahasiswa Prodi Teknik Elektro") +
  geom_text(hjust = 0.5, size = 4, position = position_dodge(width = .75), vjust = 2) +
  theme(legend.position="top") +
  xlab("Jenis Kelamin")+
  ylab("Jumlah Mahasiswa") + 
  labs(fill = "IPK") 


print(gencibs_ptm)



tpi2 = cibs[cibs$JenisKelamin == 'Lk',]

cibs_ptm1 = tpi2 %>% select(LamaStudi, Ipk) %>% group_by(LamaStudi) %>% count(Ipk) %>% distinct()
cibs_ptm1

gencibs_ptm1 = ggplot(data=cibs_ptm1, aes(fill=LamaStudi, x=Ipk, y = n, label = n, legend = TRUE )) + 
  geom_bar(position="dodge", stat="identity") +
  theme_minimal() +
  ggtitle("Predikat IPK dan Lama Studi Mahasiswa Laki-Laki") +
  geom_text(hjust = 0.5, size = 4, position = position_dodge(width = .75), vjust = 2) +
  theme(legend.position="top") +
  xlab("Predikat IPK")+
  ylab("Jumlah Mahasiswa") + 
  labs(fill = "Lama Studi") 


print(gencibs_ptm1)

tpi2 = cibs[cibs$JenisKelamin == 'Lk',]

cibs_ptm2 = tpi2 %>% select(AsalSekolah, Ipk) %>% group_by(AsalSekolah) %>% count(Ipk) %>% distinct()
cibs_ptm2

gencibs_ptm2 = ggplot(data=cibs_ptm2, aes(fill=AsalSekolah, x=Ipk, y = n, label = n, legend = TRUE )) + 
  geom_bar(position="dodge", stat="identity") +
  theme_minimal() +
  ggtitle("Predikat IPK dan Asal Sekolah Mahasiswa Laki-Laki") +
  geom_text(hjust = 0.5, size = 4, position = position_dodge(width = .75), vjust = 2) +
  theme(legend.position="top") +
  xlab("Predikat IPK")+
  ylab("Jumlah Mahasiswa") + 
  labs(fill = "Asal Sekolah") 


print(gencibs_ptm2)


tpi3 = cibs[cibs$JenisKelamin == 'Lk',]

cibs_ptm3 = tpi3 %>% select(AsalSekolah, Ipk, JalurMasuk) %>% group_by(AsalSekolah, JalurMasuk) %>% count(Ipk) %>% distinct()
cibs_ptm3

gencibs_ptm3 = ggplot(data=cibs_ptm3,aes(fill=AsalSekolah, x=Ipk, y = n, label = n, legend = TRUE )) + 
  geom_bar(position="dodge", stat="identity") +
  theme_minimal() +
  ggtitle("Predikat IPK, Asal Sekolah, dan Jalur Masuk Mahasiswa Laki-Laki") +
  geom_text(hjust = 0.5, size = 4, position = position_dodge(width = .75), vjust = 2) +
  theme(legend.position="top") +
  xlab("Predikat IPK")+
  ylab("Jumlah Mahasiswa") + 
  facet_wrap(~JalurMasuk) +
  labs(fill = "Asal Sekolah") 


print(gencibs_ptm3)
