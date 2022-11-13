# ANALISIS DATA KELULUSAN MAHASISWA FAKULTAS TEKNIK UNIVERSITAS BENGKULU DENGAN ALGORITMA APRIORI DAN ECLAT
<p align="justify">Jumlah mahasiswa yang diterima dan jumlah mahasiswa yang telah lulus mengalami 
penurunan drastis lima tahun berturut-turut akan dilakukan peninjauan kembali akreditasi berdasarkan
poin kedua pokok-pokok Merdeka Belajar: Kampus Merdeka Kementerian Pendidikan dan Kebudayaan. 
Penelitian ini menganalisis data kelulusan mahasiswa Fakultas Teknik tahun 2016-2021 menggunakan 
algoritma apriori dan algoritma eclat untuk data analisis dengan minimum support 0.035 dan minimum 
confidence 0.6 (60%). Hasil akhir algoritma eclat digunakan untuk analisis data yang diimplementasikan 
melalui grafik dan hasil akhir algoritma apriori yaitu dalam bentuk association rules diurutkan 
berdasarkan nilai confidence tertinggi, nilai count tertinggi, dan nilai lift >1 yang rules ini dapat 
digunakan sebagai referensi untuk meningkatkan input potensi siswa baru di Fakultas Teknik.</p>


## Metode Pengembangan Sistem
![image](https://user-images.githubusercontent.com/14053225/198309912-75b7197f-a404-42dc-ac29-bcae0d7e4755.png)

<p align="justify">Metode pengembangan untuk penelitian ini adalah metode-metode CRISP-DM (Cross Industry Standard Process for Data Mining) yang dikembangkan pada 
tahun 1996 menyediakan standar proses dalam pemecahan masalah untuk data mining melalui 6 Fase (Larose, 2005) : </p>

### 1. Fase Business Understanding Phase <br>
<p align="justify">Pada penelitian ini data yang diperlukan adalah data kelulusan mahasiswa Fakultas Teknik Universitas Bengkulu tahun 2016 – 2021 kemudian menganalisa data yang didapat atribut apa yang harus diambil lalu memprosesnya dengan algoritma Apriori untuk mendapatkan association rules antar atribut dan menganalisa frequent itemset atau nilai yang sering muncul dengan algoritma Eclat.</p>

### 2. Fase Data Understanding <br>
Pada tahap ini pengumpulan data didapatkan dua data yaitu :
 * Data peserta wisuda mahasiswa Universitas Bengkulu dari tahun 2016 – 2021
 * Data akademik mahasiswa Universitas Bengkulu tahun 2005-2017. 
 <p align="justify">Sebagian besar jenis nilai data bersifat categorical variable atau variabel yang nilainya tidak dinyatakan dengan angka seperti variabel nama mahasiswa, alamat, tempat lahir, asal sekolah, dan lainnya pada data peserta wisuda mahasiswa Universitas Bengkulu tahun 2016-2021. Beberapa variabel ada yang bersifat simbolis seperti jenis kelamin, agama, id_prodi, dan fakultas pada data akademik mahasiswa Universitas Bengkulu tahun 2005-2017. Variabel IPK bersifat numerical variable karena nilai variabelnya dinyatakan dengan angka namun banyaknya IPK yang berbeda membuat nilainya semakin bervariasi sehingga diperlukan transformasi data. Pada categorical variable fakultas ada ketidaksamaan data. Banyaknya data yang kosong dikarenakan proses penyalinan data dari database (sql) ke dalam bentuk excel (.xml) membuat data harus dihapus atau dilakukan crosscheck manual pada data di database.</p>

### 3. Fase Data Preparation <br>
* Selecting Data <br>
    <p align="justify">Pada tahap ini dilakukan pemilihan data dan atribut yang akan digunakan. Data yang akan digunakan yaitu data kelulusan mahasiswa Fakultas Teknik Universitas Bengkulu tahun 2016 – 2020. Atribut yang akan digunakan adalah jenis kelamin, asal sekolah, jalur masuk, IPK dan lama studi.</p>
* Cleaning Data <br>
    <p align="justify">Pada dataset Data Peserta Wisuda Mahasiswa UNIB Angkatan 2013-2017 dilakukan pembersihan data. Data dibersihkan berdasarkan fakultas. Data mahasiswa Fakultas Teknik yang diambil yaitu Teknik Sipil, Informatika, Teknik Mesin, dan Teknik Elektro. Data pada atribut jalur masuk yaitu “Lama” dihapus karena data tidak bisa ditransformasi berdasarkan jalur masuk yang telah ada. Atribut selain jenis kelamin, asal sekolah, jalur masuk, dan IPK dihapus.</p>
* Constructing New Data <br>
    <p align="justify">Tahap ini membuat atribut yang diperlukan namun tidak ada pada dataset seperti atribut lama studi. Pada dataset Data Peserta Wisuda Mahasiswa UNIB Angkatan 2013-2017 terdapat atribut yang berisi data tanggal lulus mahasiswa dan tahun atau periode wisuda. Untuk mendapatkan atribut lama studi yang dilakukan adalah dengan cara menghitung tahun Angkatan mahasiswa dan tahun atau periode wisuda mahasiswa sehingga didapatkan lama studi mahasiswa contohnya “mahasiswa angkatan 2014 yang wisuda pada periode 85 atau pada tanggal 06-Agustus-2018 berarti mahasiswa tersebut memiliki lama studi selama 4 tahun 0 bulan”.</p>
* Integrating Data <br>
    <p align="justify">Tahap ini adalah tahap menggabungkan data atau menyatukan data yang berbeda seperti data pada atribut asal sekolah. Data asal sekolah yang terdapat pada dataset Data Akademik Mahasiswa UNIB Angkatan 2005 – 2017 banyak yang tidak jelas asal daerahnya seperti “SMAN 5 “ yang mana tidak diketahui SMAN 5 daerah mana mahasiswa ini berasal. Jadi harus dilakukan crosscheck pada database untuk mengetahui daerah asal sekolah. Data pada dataset ada yang tidak sinkron dengan data pada database seperti pada dataset terdapat data asal sekolah “SMAN 1 CURUP SELATAN” di mana pada database mahasiswa itu berasal dari SMAN 4 CURUP.</p>
* Formatting Data <br>
    <p align="justify">Tahap ini adalah mengubah format data sehingga data tersebut bisa digunakan untuk tahap modeling. Atribut yang perlu dilakukan formatting data adalah IPK dan lama studi. Banyaknya data IPK yang bervariasi membuat proses modelling nantinya akan rumit dan proses pemahaman data analisis yang hasilnya tidak maksimal.</p>

### 4. Fase Modelling <br>
<p align="justify">Proses modelling algoritma yang digunakan untuk tahap modelling ini adalah algoritma apriori dan algoritma Eclat. Nilai minimum support merupakan hasil perhitungan data terkecil yaitu 19 dihitungan dengan rumus support untuk 1 item maka nilai minimum support yang digunakan adalah 0,035. Nilai minimum confidence yang digunakan adalah 0,6 atau 60%, nilai minimum confidence ini untuk menyakinkan kuatnya hubungan antara item yang satu dengan yang lainnya. Nilai minimum length (minlen) yang digunakan adalah 2 karena nilai minimum length (minlen) = 1 maka hasil yang didapatkan adalah rule yang memiliki nilai lift = 1, dimana jika nilai lift = 1 tidak dapat dijadikan acuan untuk menganalisis data kelulusan mahasiswa Fakultas Teknik Universitas Bengkulu.</p>

### 5. Fase Evaluation <br>
<p align="justify">Terdapat 2 faktor yang akan dievaluasi dalam penelitian ini, yaitu ukuran generalitas (generality) dan ukuran reliabilitas (reliability) dari association rules yang dihasilkan (Gunadi & Sensuse, 2012)</p>

### 6. Fase Deployment <br>
<p align="justify">Tahap deployment pada penelitian ini adalah aplikasi web dengan menggunakan open source package pada R yaitu Shiny.</p>


### Daftar Pustaka

Gunadi, G., & Sensuse, D. I. (2012). Penerapan Metode Data Mining Market Basket Analysis Terhadap Data Penjualan Produk Buku Dengan Menggunakan Algoritma Apriori Dan Frequent Pattern Growth ( Fp-Growth ) : Telematika, 4(1), 118–132.

Larose, D. T. (2005). Discovering Knowledge in Data: An Introduction to Data Mining (Illustrate). John Wiley & Sons.