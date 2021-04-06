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

pte = cibs[cibs$Prodi == 'Teknik Elektro',]
ptm = cibs[cibs$Prodi == 'Teknik Mesin',]

cibs_pt = ptm %>% select(Ipk) %>% count(Ipk) %>% distinct()
cibs_pt
genderDist_aj = ggplot(data = cibs_pt, aes(x = Ipk, y = n)) +
  geom_bar(mapping = aes(x = Ipk, y = n), position="dodge", stat="identity", fill="steelblue", width = 0.5) + 
  labs(title = 'Ipk lulusan Prodi Teknik Elektro', x='IPK') +
  geom_text(aes(label=n), color="black", vjust=1.5,
            position = position_dodge(1.5), size=5) +
  theme_minimal()
print(genderDist_aj)



ptm = cibs[cibs$Prodi == 'Teknik Mesin',]
ptm

cibs_ptm = ptm %>% select(LamaStudi, JenisKelamin, Ipk) %>% group_by(JenisKelamin) %>% count(Ipk, LamaStudi) %>% distinct()
cibs_ptm
genderDist_aj = ggplot(data = cibs_ptm, aes(x = Ipk, y = n, fill=LamaStudi)) +
  geom_bar(mapping = aes(x = Ipk, y = n), position="dodge", stat="identity", fill="steelblue", width = 0.5) + 
  labs(title = 'Ipk lulusan Prodi Teknik Elektro', x='IPK') +
  geom_text(aes(label=n), color="black", vjust=1.5,
            position = position_dodge(1.5), size=5) +
  theme_minimal()


gencibs_ptm = ggplot(data=cibs_ptm, aes(fill=Ipk, x=LamaStudi, y = n, label = n, legend = TRUE )) + 
  geom_bar(position="dodge", stat="identity") +
  labs(x='Lama Studi') +
  theme_minimal() +
  ggtitle("Lama Studi dan IPK mahasiswa Prodi Teknik Mesin") +
  geom_text(hjust = 0.5, size = 4, position = position_dodge(width = .75), vjust = 2) +
  facet_wrap(~JenisKelamin) +
  theme_ipsum() +
  theme(legend.position="bottom") +
  xlab("")

print(gencibs_ptm)



ptu = cibs[cibs$JalurMasuk == 'SPMU',]
ptu

cibs_ptu = ptu %>% select(LamaStudi, JenisKelamin, Ipk) %>% group_by(JenisKelamin) %>% count(Ipk, LamaStudi) %>% distinct()
cibs_ptu


gencibs_ptu = ggplot(data=cibs_ptu, aes(fill=Ipk, x=LamaStudi, y = n, label = n, legend = TRUE )) + 
  geom_bar(position="dodge", stat="identity") +
  labs(x='Lama Studi') +
  theme_minimal() +
  ggtitle("Lama Studi dan IPK mahasiswa Jalur Masuk SPMU") +
  geom_text(hjust = 0.5, size = 4, position = position_dodge(width = .75), vjust = 2) +
  facet_wrap(~JenisKelamin) +
  theme_ipsum() +
  theme(legend.position="bottom") +
  xlab("")

print(gencibs_ptu)


pta = cibs[cibs$JalurMasuk == 'SNMPTN',]
pta

cibs_pta = pta %>% select(LamaStudi, JenisKelamin, Ipk, AsalSekolah) %>% group_by(JenisKelamin, AsalSekolah) %>% count(Ipk, LamaStudi) %>% distinct()
cibs_pta


gencibs_pta = ggplot(data=cibs_pta, aes(fill=Ipk, x=LamaStudi, y = n, label = n, legend = TRUE )) + 
  geom_bar(position="dodge", stat="identity") +
  theme_minimal() +
  ggtitle("Lama Studi, Asal Sekolah, dan IPK mahasiswa Jalur Masuk SNMPTN") +
  geom_text(size = 3, position = position_dodge(width = .75)) +
  facet_wrap(JenisKelamin~AsalSekolah) +
  theme_ipsum() +
  theme(legend.position="bottom") +
  xlab("")

print(gencibs_pta)
