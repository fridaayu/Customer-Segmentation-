#Customer Segmentation
datapelanggan <- read.csv("https://academy.dqlab.id/dataset/customer_segments.txt",sep="\t")
View(datapelanggan)
colnames(datapelanggan)
#menampilkan kolom tertentu
datapelanggan[c('Customer_ID','NilaiBelanjaSetahun')]
#Vector untuk menyimpan nama field
field_used <- c('Jenis.Kelamin','Umur','Profesi')
#nampilkan field_used
datapelanggan[field_used]
#Konversi data type kategorik ke bentuk numerik 
datapelanggan_matrix <- data.matrix(datapelanggan[c("Jenis.Kelamin",'Profesi','Tipe.Residen')])
View(datapelanggan_matrix)
#Menggabungkan data frame 
datapelangganbaru <- data.frame(datapelanggan,datapelanggan_matrix)
View(datapelangganbaru)
#Normalisasi kolom NilaiBelanjaSetahun ke bentuk yang lebih sederhana agar 
#sum of squared errors tidak besar
datapelangganbaru$NilaiBelanjaSetahun <- datapelangganbaru$NilaiBelanjaSetahun/1000000
#Membuta data Master > data master adalah list dari data kategorik yang diubah jadi numerik
Profesi <- unique(datapelangganbaru[c("Profesi","Profesi.1")])
Jenis.Kelamin <- unique(datapelangganbaru[c("Jenis.Kelamin","Jenis.Kelamin.1")])
Tipe.Residen <- unique(datapelangganbaru[c('Tipe.Residen','Tipe.Residen.1')])
#Clustering dengan kmeans
set.seed(100)
datapelanggan_clust <- c('Jenis.Kelamin.1','Umur','Profesi.1',
                         'Tipe.Residen.1','NilaiBelanjaSetahun')
segmentasi <- kmeans(datapelangganbaru[c(datapelanggan_clust)],centers=5,nstart=25)
segmentasi$centers #berisis mean dari masing2 cluster
#penggabungan hasil cluster
datapelangganbaru$cluster <- segmentasi$cluster
str(datapelangganbaru)
#Cek anggota cluster
which(datapelangganbaru$cluster==1)
which(datapelangganbaru$cluster==2)
which(datapelangganbaru$cluster==3)
which(datapelangganbaru$cluster==4)
which(datapelangganbaru$cluster==5)
#Menampilkan data hasil clustering
datapelangganbaru[which(datapelangganbaru$cluster==1),]
datapelangganbaru[which(datapelangganbaru$cluster==2),]
datapelangganbaru[which(datapelangganbaru$cluster==3),]
datapelangganbaru[which(datapelangganbaru$cluster==4),]
datapelangganbaru[which(datapelangganbaru$cluster==5),]
#membandingkan sum squared error dengan cara membuat clustering baru dengan k=3
segmentasi3 <- kmeans(datapelangganbaru[c(datapelanggan_clust)],centers = 3,nstart = 25)
segmentasi$tot.withinss
segmentasi3$tot.withinss
segmentasi$withinss
segmentasi3$withinss
#menentukan jumlah cluster terbaik bisa dari sum squared (SS atau SSE)
#Secara teori mengatakan bahwa semnakin banyak kluster nilai SS makin kecil dan sebaliknya
#Simulasi jumlah cluster dan SSE
sse <- sapply(1:10, function(param_k){
  kmeans(datapelangganbaru[c(datapelanggan_clust)],param_k,nstart = 25)$tot.withinss})
#Grafik SSE (ELBOW METHOD)
library(ggplot2)
jumlah_cluster_max <- 10
ssdata <- data.frame(cluster=c(1:jumlah_cluster_max),sse)
ggplot(ssdata,aes(x=cluster,y=sse))+geom_line(color='green')+geom_point()+
  ylab("SSE")+xlab("Cluster")+geom_text(aes(label=format(round(sse,2),nsmall = 2)),
                                      hjust=-0.2,vjust=-0.5)+ scale_x_discrete(limits=c(1:jumlah_cluster_max))
segmentasi$centers[,2] 
#cluster 1 : Diamond Senior Member
#Cluster 2 : Gold Young Professional
#Cluster 3 :Silver Youth Gals
#Cluster 4 : Diamond Profesional
#cluster 5 : Silver Mid Professional

Nama.Segmen <- c("Diamond Senior Member","Gold Young Professional",
                 "Silver Youth Gals","Diamond Profesional",
                 "Silver Mid Professional")
Nomor.cluster <- c(1,2,3,4,5)
custumer.segmentation <- data.frame(Nomor.cluster,Nama.Segmen)

#menggabungkan referensi
identitas.cluster <- list(Profesi=Profesi,Jenis.Kelamin=Jenis.Kelamin,
                          Tipe.Residen=Tipe.Residen,segmentasi=segmentasi,
                          custumer.segmentation=custumer.segmentation,
                          datapelanggan_clust=datapelanggan_clust)

#menyimpang objek dalam bentuk file
saveRDS(identitas.cluster,'cluster.rds')
#Memasukkan data baru
databaru <- data.frame(Customer_ID="CUST-100", Nama.Pelanggan="Rudi Wilamar",Umur=32,Jenis.Kelamin="Wanita",Profesi="Pelajar",Tipe.Residen="Cluster",NilaiBelanjaSetahun=3.5)
databaru
#Memuat objek clustering dari file
identitas.cluster <- readRDS(file = "cluster.rds")
#Merge dengan data referensi
databaru <- merge(databaru,identitas.cluster$Profesi)
databaru <- merge(databaru,identitas.cluster$Jenis.Kelamin)
databaru <- merge(databaru,identitas.cluster$Tipe.Residen)
#Menentukan data baru masuk cluster mana
which.min(sapply(1:5, function(x)sum(databaru[identitas.cluster$datapelanggan_clust]-
                                       identitas.cluster$segmentasi$centers[x,])^2))
identitas.cluster$custumer.segmentation[which.min(sapply(1:5, function(x)sum(databaru[identitas.cluster$datapelanggan_clust]-
                                                                                identitas.cluster$segmentasi$centers[x,])^2)),]
