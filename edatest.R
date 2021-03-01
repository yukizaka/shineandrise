library(readxl)
library(readr)
library(dplyr)
library(caret)
library(caTools)
cibi <- read.csv('C:/Users/Florecita/Documents/SKRIPSI/fixdataset_nv_edit.csv')

tly1 <- read.transactions('C:/Users/Florecita/Documents/SKRIPSI/fixdataset_nv_edit.csv', format = 'basket', sep=',')

cibi$TID <- as.factor(cibi$TID)
cibi$JenisKelamin <- factor(cibi$JenisKelamin, levels = c('L','P'))
cibi$Ipk <- factor(cibi$Ipk, levels = c('B1','B2', 'B3'))
cibi$LamaStudi <- factor(cibi$LamaStudi, levels = c('SSR', 'SR', 'S'))
cibi$JalurMasuk <- factor(cibi$JalurMasuk, levels = c('SNMPTN', 'SBMPTN', 'SPMU'))
cibi$Prodi <- factor(cibi$Prodi, levels = c('Informatika', 'Teknik Sipil', 'Teknik Elektro', 'Teknik Mesin'))
cibi$AsalSekolah <- factor(cibi$AsalSekolah, levels = c(
  
                    'SMAN 2 KOTA BENGKULU', 'SMKN 2 KOTA BENGKULU', 'SMAN 5 KOTA BENGKULU',
                    'SMAN 7 KOTA BENGKULU', 'SMAN 6 KOTA BENGKULU', 'SMAN 1 KOTA BENGKULU',
                    'SMAN 4 KOTA BENGKULU', 'SMAN 3 KOTA BENGKULU', 'SMAN 1 ARGA MAKMUR', 
                    'SMA MUHAMMADIYAH 4 KOTA BENGKULU', 'SMAN 1 KEPAHIANG', 'SMAN 1 CURUP',
                    
                    'SMKN 2 ARGA MAKMUR', 'SMAN 4 CURUP', 'SMAN 8 KOTA BENGKULU',
                    'SMAN 2 CURUP', 'SMAN 2 MANNA', 'SMKN 1 CURUP',
                    'SMKS 8 GRAKARSA KOTA BENGKULU', 'MAN 1 MODEL KOTA BENGKULU','SMAN 10 KOTA BENGKULU',
                    'SMKN 1 KETAHUN','SMAN 1 LUBUKLINGGAU','SMAN 1 MANNA',
                    
                    'SMAN 1 PADANG JAYA','SMKN 4 KOTA BENGKULU','SMAN 1 PUTRI HIJAU',
                    'SMAN 1 SELUMA','SMKN 1 KOTA BENGKULU','SMAN 1 KAUR',
                    'SMAN 1 LEBONG UTARA','SMAN 1 PAGARALAM','SMAN 1 PONDOK KELAPA',
                    'SMAN 3 KAUR','SMAN 3 MANNA','SMAN 5 CURUP',
                    
                    'SMAN 5 MANNA','MAN 2 KOTA BENGKULU','SMAN 1 TUGU MULYO',
                    'SMAN 3 LUBUKLINGGAU','SMAN 3 SELUMA','SMAN 4 MANNA',
                    'SMAN 9 KOTA BENGKULU','SMKN 3 LUBUKLINGGAU','SMKS 9 MUHAMMADIYAH KOTA BENGKULU',
                    'MAN 2 CURUP','SMA MUHAMMADIYAH  4 KOTA BENGKULU','SMA XAVERIUS CURUP',
                    
                    'SMAN 1 BENGKULU TENGAH','SMAN 1 KERKAP','SMAN 1 KETAHUN',
                    'SMAN 1 PURWODADI','SMAN 1 SEKAYU','SMAN 1 SELUPU REJANG',
                    'SMAN 1 SIDIKALANG','SMAN 1 TARUTUNG','SMAN 2 ARGA MAKMUR',
                    'SMAN 2 MUKOMUKO','SMAN 3 CURUP','SMAN 3 LAHAT',
                    
                    'SMAN 3 MUKOMUKO','SMAN 4 KAUR','SMAN 5 KAUR',
                    'SMAN 6 MANNA','SMAN 8 MANNA','SMAN 9 MANNA',
                    'SMKN 1 LEBONG','SMKN 2 KEPAHIANG','SMKN 3 KOTA BENGKULU',
                    'MA AL FALAAH LEKISREJO OGAN KOMERING ULU','MAN 1 KOTA BENGKULU','MAN 1 LUBUKLINGGAU',
                    
                    'MAN 1 MODEL BENGKULU','MAN 2 KEPAHIANG','MAN 2 PALEMBANG',
                    'MAN MUKOMUKO','PONDOK MODERN DARUSSALAM GONTOR PUTRI','SMA 1 BINDURIANG',
                    'SMA 1 RUPIT MUSI RAWAS','SMA KHATOLIK CINTA KASIH TEBING TINGGI','SMA METHODIST 1 PALEMBANG',
                    'SMA MUHAMMADIYAH 1 KOTA BENGKULU','SMA MUHAMMADIYAH 1 LUBUKLINGGAU','SMA MUHAMMADIYAH 1 PURBOLINGGO',
                    
                    'SMA PALLAWA KOTA BENGKULU','SMA PANCASILA KOTA BENGKULU','SMA PEMBANGUNAN KOTA BENGKULU',
                    'SMA PMT HAMKA YWII PASAR USANG','SMA PRISMA SERANG','SMA SANTO PETRUS SIDIKALANG',
                    'SMA SWASTA AN-NIZAM MEDAN','SMA TENERA PT AGRICINAL BENGKULU UTARA','SMA TRISUKSES LAMPUNG SELATAN',
                    'SMAIT ASSYIFA BOARDING SCHOOL','SMAN 1 ARGAMAKMUR','SMAN 1 BAGAN SINEMBAH',
                    
                    'SMAN 1 BATUSANGKAR','SMAN 1 BINDURIANG','SMAN 1 DOLOKSANGGUL',
                    'SMAN 1 GIRI MULYA','SMAN 1 INDRALAYA UTARA','SMAN 1 KABANJAHE KARO',
                    'SMAN 1 KEC. HARAU','SMAN 1 KIKIM TIMUR LAHAT','SMAN 1 LEBONG SAKTI',
                    'SMAN 1 LEBONG SELATAN','SMAN 1 LENGAYANG','SMAN 1 MERBAU',
                    
                    'SMAN 1 MUARA PINANG','SMAN 1 MUKOMUKO UTARA','SMAN 1 OGAN KOMERING ULU',
                    'SMAN 1 PADANG','SMAN 1 PANGKALPINANG','SMAN 1 PANGURURAN',
                    'SMAN 1 PANYABUNGAN SELATAN','SMAN 1 PURBOLINGGO','SMAN 1 RANAH PESISIR',
                    'SMAN 1 RANTAU SELATAN','SMAN 1 RETEH INDRAGIRI HILIR','SMAN 1 SAROLANGUN',
                    
                    'SMAN 1 SEI PENUH','SMAN 1 SOLOK','SMAN 1 SUNGAI AUR',
                    'SMAN 1 TANJUNG BATU','SMAN 1 TEBING TINGGI','SMAN 1 UJAN MAS',
                    'SMAN 14 OKU','SMAN 2 BINJAI','SMAN 2 BUKIT TINGGI',
                    'SMAN 2 LAHAT','SMAN 2 LUBUK PAKAM','SMAN 2 LUBUKLINGGAU',
                    
                  'SMAN 2 PADANGSIDIMPUAN','SMAN 2 PARIAMAN','SMAN 2 PLUS PANYABUNGAN',
                    'SMAN 2 RANTAU UTARA LABUHAN RATU','SMAN 2 SEI PENUH','SMAN 2 SIBORONGBORONG',
                    'SMAN 2 SUNGAI LIMAU','SMAN 2 TEBING TINGGI','SMAN 29 JAKARTA',
                    'SMAN 3 PAGAR ALAM','SMAN 3 SOLOK','SMAN 3 SUMEDANG',
                  
                  'SMAN 3 TEBING TINGGI','SMAN 36 JAKARTA','SMAN 4 LAHAT',
                    'SMAN 4 LUBUKLINGGAU','SMAN 5 BEKASI','SMAN 5 LUBUKLINGGAU',
                  'SMAN 5 MUKOMUKO','SMAN 6 SELUMA','SMAN 7 TANGERANG',
                    'SMAN 8 MEDAN','SMAN 9 BEKASI','SMAN 9 PADANG',
                  
                  'SMAN GUNUNG TALANG','SMAN KARANG DAPO MUARO JAMBI','SMAN SURULANGUN',
                    'SMAN SURULANGUN MUSI RAWAS','SMAS RAKSANA MEDAN','SMAS S KAT BUDI MURNI 1 MEDAN',
                  'SMK (STM) LEONARDO KLATEN','SMK PANCAKARYA TANGERANG','SMK TIARA LAHAT',
                    'SMK VETERAN 1 TULUNGAGUNG','SMKN 1 BALIGE TOBA SAMOSIR','SMKN 1 KAUR',
                  
                  'SMKN 1 KEPAHIANG','SMKN 1 LAHAT','SMKN 1 MUKO MUKO',
                    'SMKN 1 PALIPI SAMOSIR','SMKN 1 PRABUMULIH','SMKN 1 SIBORONGBORONG',
                  'SMKN 2 RANTAU UTARA','SMKN 3 SELUMA','SMKN 4 KAUR',
                    'SMKN MUKO MUKO','SMKN TUGU MULYO','SMKN UJAN MAS KEPAHIANG',
                  'SMKS 1 PEMBANGUNAN KOTA BENGKULU','SMKS 13 YAMA KARYA BENGKULU','SMKS 16 FARMASI KOTA BENGKULU','SMKS 8 PERTIWI CURUP'
                          ))



cibi_id = cibi %>% select(TID, JenisKelamin) %>% group_by(TID) %>% distinct()
head(cibi_id)
cibi_id
summary(cibi_id$JenisKelamin)
options(scipen = 10000)
genderDist = ggplot(data = cibi_id) + 
  geom_bar(mapping = aes(x = JenisKelamin, fill = JenisKelamin)) + 
  labs(title = 'Jenis Kelamin') + 
  scale_fill_brewer(palette = 'Paired') #+
#geom_text(aes(label=count), vjust=1.6, color="black", size=10)#
print(genderDist)


cibi_prodiipk = cibi %>% select(Ipk, Prodi) %>% group_by(Prodi) %>% count(Ipk) %>% arrange(desc(Ipk)) %>% distinct()
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
genderDist_aj = ggplot(data = best_sekolah) + 
  geom_bar(mapping = aes(x = Ipk, y = ..count.., fill = JenisKelamin)) + 
  labs(title = 'Ipk dan Jenis Kelamin (SMKN 2 Kota Bengkulu)') +
  scale_fill_brewer(palette = 'Paired') +
print(genderDist_aj)

cibi_jkjmls = cibi %>% select(Ipk, JenisKelamin, best_sekolah) %>% group_by(JenisKelamin) %>% count(Ipk, best_sekolah) %>% distinct()

cibi_jkjmls2 = dataset %>% select(Ipk, JenisKelamin, best_sekolah) %>% group_by(JenisKelamin) %>% count(Ipk, best_sekolah) %>% distinct()

genderbestsekolah = ggplot(data= best_sekolah, aes(fill=JenisKelamin, x=Ipk, y = ..count.., legend = TRUE )) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_viridis(discrete = T, option = "E") +
  scale_fill_brewer(palette = 'Paired') +
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





