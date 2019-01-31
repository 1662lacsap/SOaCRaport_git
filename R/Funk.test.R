#' Wyszukiwanie odchylen i grupowanie danych - Search Outliers and Clustering Raport
#'
#' Funkcja Funk.test() wyszukuje odchylenia metoda LOF lub COF
#' i nastepnie usuwa je z badanego zbioru, po usunieciu grupuje dane 
#' algorytmem hierarchicznym. Za kazdym razem obliczny jest wspolczynnik 
#' Jaacarda, ktory okresla jakosc zbiorow po grupowaniu.
#' 
#' @param file Zbior danych
#' @param nameColumns Nazwy kolumn
#' @param qSet Liczba grup
#' @param methodDist Metoda odleglosci
#' @param methodHclust Metoda laczenia
#' @param k.neighbors Wspolczynnik k-sasiadow
#' @param o.outliers Ilosc usuwanych odchylen
#' @param o.algorithm Metoda wykrywania odchylen (LOF lub COF)
#' @author Czeslaw Horyn choryn@us.edu.pl
#' @return Podsumowanie i dendrogram
#'
#' @examples
#' Funk.test <- function("iris.data",c('ClusterNumber','Sepal length','Sepal width','Petal length','Petal width'),c(1:3),"canberra","ward.D",4,1,"COF")
#' 
#' @export

 Funk.test <- function(file, nameColumns, qSet, methodDist,
 methodHclust, k.neighbors, o.outliers, o.algorithm){}
 #Funk.test <- function("iris.data",c('ClusterNumber','Sepal length','Sepal width','Petal length','Petal width'),
       #c(1:3),"canberra","ward.D",4,1,"COF")
     
# Potrzebne pakiety i programy, aby uruchomic pakiet:

# Uwaga na MAC OS zainstaluj https://www.xquartz.org/ XQuartz-2.7.11.dmg

# Pakiety:
# install.packages(c("genefilter"))
# Instalacja pakietu "genefilter" wymaga uruchomienia ponizszego kodu:
# if (!requireNamespace("BiocManager", quietly = TRUE))
# install.packages("BiocManager")
# BiocManager::install("genefilter", version = "3.8")

# install.packages(c("cluster"))
# install.packages(c("MASS"))
# install.packages(c("dendextend"))
# install.packages(c("DMwR"))
# install.packages(c("xlsx"))
# install.packages(c("DDoutlier"))
# install.packages(c("colorspace"))
# install.packages(c("rJava"))
# install.packages(c("dplyr"))
# install.packages(c("philentropy"))
# install.packages(c("clusterSim")) 
# install.packages(c("factoextra"))
# install.packages(c("NbClust"))
# install.packages(c("fpc"))
# install.packages(c("BiocManager"))
# install.packages(c("ggplot2"))

# Pomocne do tworzenia pakietu
# install.packages(c("devtools"))
# install.packages(c("roxygen2")) 

# Zapis danych do Excela przy pomocy rJava - instrukcja:
# Here is the easy steps for it:
# remove the rJava package: remove.packages(rJava)
# close R
# install latest Java on you mac
# open terminal and type this command: sudo R CMD javareconf
# Open R and install rJava with this command:
# install.packages("rJava", dependencies=TRUE, type="source")

# Windows rJava - https://www.java.com/en/download/manual.jsp - zainstaluj Windows Offline (64-bit)

# Linki do zbiorow z Machine Learning Databases
# Wine
# file <-  'https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data'
# Iris
# file <-  'https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data'
# Breast_tissue
# file <-  'http://archive.ics.uci.edu/ml/machine-learning-databases/00192/BreastTissue.xls'

# Funkcja wymaga wczytania nazwa kolumn dla poszczegolnych zbiorow danych
# przykladowe nameColumns - numer grupy jako pierwszy zawsze - 'ClusterNumber'
# Wine
# nameColumns <- c('ClusterNumber','Alcohol','Malic.acid','Ash','Alcalinity.of.ash','Magnesium','Total.phenol','Flavanoids','NonFlavanoid.phenols','Proanthocyanin','Color.intensity','Hue','OD280.OD315.of.diluted.wines','Proline')
# Iris
# nameColumns <- c('ClusterNumber','Sepal length','Sepal width','Petal length','Petal width') 
# Breast_tissue
# nameColumns <- c('ClusterNumber','I0','PA500','HFS','DA','Area','A/DA','Max','IP','P')

  #LIBRARY
  library(cluster)
  library(clusterSim)
  #dist oblicznie 46 przypadkow
  library(philentropy)  
  #do dendogramu wykres
  library(dendextend)
  library(colorspace)
  #pakiet dla LOF
  library(DMwR)
  #zapis do Excela
  library(rJava)
  library(xlsx)
  #narzedzie do pracy z ramka danych np: filter
  library(dplyr)
  #Connectivity-based Outlier Factor (COF) algorithm - pakiet
  library (DDoutlier)
  #pakiet graficzny
  library(ggplot2)
  library(factoextra) 
  library(NbClust)
  library(fpc)
  #library(genefilter)
  #library(BiocManager)
  library(MASS)
  
if (!requireNamespace("BiocManager", quietly = TRUE))
install.packages("BiocManager")
BiocManager::install("genefilter", version = "3.8")


  #drukuj na konsoli
  options(max.print = 1000000)


Funk.test <- function(file, nameColumns, qSet, methodDist, methodHclust, k.neighbors, o.outliers, o.algorithm)
{
    
  #Kontener do wczytywania zbiorow po podziale
  totalAfterDivision<-list()
  #Kontener do wczytywania zbiorow przed podzialem
  totalCollection <- list()
  #data.frame
  df_total <- data.frame()
  

        # Funkcja_4 - funkcja usuwajaca odchylenia przed grupowaniem
    # przygotowujemy zbior data_infTrain do dalszego procesu obrobki w kazdej petli usuwajac odpowiednia liczbe odchylen
  
            delete.outliers <- function(data_inf, o)          
            {
              if (o == 0)
            {
              data_infTrain <- data_inf[  ,which(names(data_inf) != "ClusterNumber")]                          
             }
             if (o > 0)
              {
                #p przyjmuje odchylenia od 1 do aktualnej wartosci o
              p <- outliers[1:o]       
              #usuwamy outliers-y
            data_infTrain <- data_inf[-p,which(names(data_inf) != "ClusterNumber")]                         
              }
              return(data_infTrain)
              }
    
       
      # Funkcja_7 - funkcja zliczajaca zbiory po usunieciu outliers i po nowym podziale

            CollectionAfterDivision <- function(m)
             {
    
            for(i in 1:length(hc_data_infTrainCutree))
            {																		                                        
            if (hc_data_infTrainCutree[i] == m)
             x[i] <- i
             }
             x[!is.na(x)]
             }
  
      # Funkcja_8 - funkcja obliczajaca indeksy Jaccarda(J), okreslajaca podobienstwo miedzy zbiorami przed i po podziale (analizujaca wszystkie przypadki)
      # i wybierajaca maksymalna sume, wartosc J = J_XYZA_MAX

      # Indeks Jaccard , znany rowniez jako Intersection over Union i wspolczynnik podobienstwa Jaccard
      # (pierwotnie uksztaltowany wspolczynnik autorstwa Paula Jaccarda),
      # to statystyka uzywana do porownywania podobienstwa i roznorodnosci zestawow probek.
      # Wspolczynnik Jaccard mierzy podobienstwo miedzy skonczonymi zbiorami probek i jest definiowany
      # jako rozmiar przeciecia podzielony przez wielkosc sumy zestawow probek.

        J<-function(){
      
      # A
      #indeks JACCARD A-X1
      J.A_X1<- round((length(intersect(totalCollection[["Col1"]],totalAfterDivision[["X1"]])))/(length(union(totalCollection[["Col1"]],totalAfterDivision[["X1"]]))),digits = 4)
      #indeks JACCARD A-Y2
      J.A_Y2<- round((length(intersect(totalCollection[["Col1"]],totalAfterDivision[["X2"]])))/(length(union(totalCollection[["Col1"]],totalAfterDivision[["X2"]]))),digits = 4)
      #indeks JACCARD A-Z3
      J.A_Z3<- round((length(intersect(totalCollection[["Col1"]],totalAfterDivision[["X3"]])))/(length(union(totalCollection[["Col1"]],totalAfterDivision[["X3"]]))),digits = 4)
      #indeks JACCARD A-A4
      J.A_A4<- round((length(intersect(totalCollection[["Col1"]],totalAfterDivision[["X4"]])))/(length(union(totalCollection[["Col1"]],totalAfterDivision[["X4"]]))),digits = 4)           

      # B
      #indeks JACCARD B-X1
      J.B_X1<- round((length(intersect(totalCollection[["Col2"]],totalAfterDivision[["X1"]])))/(length(union(totalCollection[["Col2"]],totalAfterDivision[["X1"]]))),digits = 4)
      #indeks JACCARD B-Y2
      J.B_Y2<- round((length(intersect(totalCollection[["Col2"]],totalAfterDivision[["X2"]])))/(length(union(totalCollection[["Col2"]],totalAfterDivision[["X2"]]))),digits = 4)
      #indeks JACCARD B-Y2
      J.B_Z3<- round((length(intersect(totalCollection[["Col2"]],totalAfterDivision[["X3"]])))/(length(union(totalCollection[["Col2"]],totalAfterDivision[["X3"]]))),digits = 4)
      #indeks JACCARD B-A4
      J.B_A4<- round((length(intersect(totalCollection[["Col2"]],totalAfterDivision[["X4"]])))/(length(union(totalCollection[["Col2"]],totalAfterDivision[["X4"]]))),digits = 4) 
      
      # C
      #indeks JACCARD C-X1
      J.C_X1<- round((length(intersect(totalCollection[["Col3"]],totalAfterDivision[["X1"]])))/(length(union(totalCollection[["Col3"]],totalAfterDivision[["X1"]]))),digits =4)
      #indeks JACCARD C-Y2
      J.C_Y2<- round((length(intersect(totalCollection[["Col3"]],totalAfterDivision[["X2"]])))/(length(union(totalCollection[["Col3"]],totalAfterDivision[["X2"]]))),digits =4)
      #indeks JACCARD C-Z3
      J.C_Z3<- round((length(intersect(totalCollection[["Col3"]],totalAfterDivision[["X3"]])))/(length(union(totalCollection[["Col3"]],totalAfterDivision[["X3"]]))),digits =4)
      #indeks JACCARD C-A4
      J.C_A4<- round((length(intersect(totalCollection[["Col3"]],totalAfterDivision[["X4"]])))/(length(union(totalCollection[["Col3"]],totalAfterDivision[["X4"]]))),digits = 4) 
          
      # D
      #indeks JACCARD D-X1
      J.D_X1<- round((length(intersect(totalCollection[["Col4"]],totalAfterDivision[["X1"]])))/(length(union(totalCollection[["Col4"]],totalAfterDivision[["X1"]]))),digits =4)
      #indeks JACCARD D-Y2
      J.D_Y2<- round((length(intersect(totalCollection[["Col4"]],totalAfterDivision[["X2"]])))/(length(union(totalCollection[["Col4"]],totalAfterDivision[["X2"]]))),digits =4)
      #indeks JACCARD D-Z3
      J.D_Z3<- round((length(intersect(totalCollection[["Col4"]],totalAfterDivision[["X3"]])))/(length(union(totalCollection[["Col4"]],totalAfterDivision[["X3"]]))),digits =4)
      #indeks JACCARD D-A4
      J.D_A4<- round((length(intersect(totalCollection[["Col4"]],totalAfterDivision[["X4"]])))/(length(union(totalCollection[["Col4"]],totalAfterDivision[["X4"]]))),digits = 4) 
         
      #24 przypadki dla 4 grup sumy indeksow JACCARD-a
      
      J.XYZA_I_w <- c(J.A_X1, J.B_Y2, J.C_Z3, J.D_A4)
      J.XYZA_I <- round(sum(J.XYZA_I_w, na.rm=TRUE), digits = 4)
      J.XYAZ_II_w <- c(J.A_X1, J.B_Y2, J.C_A4, J.D_Z3)
      J.XYAZ_II <- round(sum(J.XYAZ_II_w, na.rm=TRUE), digits = 4)
      J.XZYA_III_w <- c(J.A_X1, J.B_Z3, J.C_Y2, J.D_A4)
      J.XZYA_III <- round(sum(J.XZYA_III_w, na.rm=TRUE) , digits = 4)
      J.XZAY_IV_w <- c(J.A_X1, J.B_Z3, J.C_A4, J.D_Y2)
      J.XZAY_IV <- round(sum(J.XZAY_IV_w, na.rm=TRUE), digits = 4)
      J.XAYZ_V_w <- c(J.A_X1, J.B_A4, J.C_Y2, J.D_Z3)
      J.XAYZ_V <- round(sum(J.XAYZ_V_w, na.rm=TRUE), digits = 4)
      J.XAZY_VI_w <- c(J.A_X1, J.B_A4, J.C_Z3, J.D_Y2)
      J.XAZY_VI <- round(sum(J.XAZY_VI_w, na.rm=TRUE), digits = 4)
       
      J.YXZA_VII_w <- c(J.A_Y2, J.B_X1, J.C_Z3, J.D_A4)
      J.YXZA_VII <- round(sum(J.YXZA_VII_w , na.rm=TRUE), digits = 4)
      J.YXAZ_VIII_w <- c(J.A_Y2, J.B_X1, J.C_A4, J.D_Z3)
      J.YXAZ_VIII <- round(sum(J.YXAZ_VIII_w, na.rm=TRUE), digits = 4)
      J.YZXA_IX_w <- c(J.A_Y2, J.B_Z3, J.C_X1, J.D_A4)
      J.YZXA_IX <- round(sum(J.YZXA_IX_w, na.rm=TRUE), digits = 4)
      J.YZAX_X_w <- c(J.A_Y2, J.B_Z3, J.C_A4, J.D_X1)
      J.YZAX_X <- round(sum(J.YZAX_X_w, na.rm=TRUE), digits = 4)
      J.YAXZ_XI_w <- c(J.A_Y2, J.B_A4, J.C_X1, J.D_Z3)
      J.YAXZ_XI <- round (sum(J.YAXZ_XI_w, na.rm=TRUE), digits = 4)
      J.YAZX_XII_w <- c(J.A_Y2, J.B_A4, J.C_Z3, J.D_X1)
      J.YAZX_XII <- round(sum(J.YAZX_XII_w, na.rm=TRUE), digits = 4)
      
      J.ZXYA_XIII_w <- c(J.A_Z3, J.B_X1, J.C_Y2, J.D_A4)
      J.ZXYA_XIII <- round(sum(J.ZXYA_XIII_w, na.rm=TRUE), digits = 4)
      J.ZXAY_XIV_w <- c(J.A_Z3, J.B_X1, J.C_A4, J.D_Y2)
      J.ZXAY_XIV <- round(sum(J.ZXAY_XIV_w, na.rm=TRUE), digits = 4)
      J.ZYXA_XV_w <- c(J.A_Z3, J.B_Y2, J.C_X1, J.D_A4)
      J.ZYXA_XV <- round(sum(J.ZYXA_XV_w, na.rm=TRUE), digits = 4)
      J.ZYAX_XVI_w <- c(J.A_Z3, J.B_Y2, J.C_A4, J.D_X1)
      J.ZYAX_XVI <- round(sum(J.ZYAX_XVI_w, na.rm=TRUE), digits = 4)
      J.ZAXY_XVII_w <- c(J.A_Z3, J.B_A4, J.C_X1, J.D_Y2)
      J.ZAXY_XVII <- round(sum(J.ZAXY_XVII_w, na.rm=TRUE), digits = 4)
      J.ZAYX_XVIII_w <- c(J.A_Z3, J.B_A4, J.C_Y2, J.D_X1)
      J.ZAYX_XVIII <- round(sum(J.ZAYX_XVIII_w, na.rm=TRUE), digits = 4)
      
      
      J.AXYZ_XIX_w <- c(J.A_A4, J.B_X1, J.C_Y2, J.D_Z3)
      J.AXYZ_XIX <- round(sum(J.AXYZ_XIX_w, na.rm=TRUE), digits = 4)
      J.AXZY_XX_w <- c(J.A_A4, J.B_X1, J.C_Z3, J.D_Y2)
      J.AXZY_XX <- round(sum(J.AXZY_XX_w, na.rm=TRUE), digits = 4)
      J.AYXZ_XXI_w <- c(J.A_A4, J.B_Y2, J.C_X1, J.D_Z3)
      J.AYXZ_XXI <- round(sum(J.AYXZ_XXI_w, na.rm=TRUE), digits = 4)
      J.AYZX_XXII_w <- c(J.A_A4, J.B_Y2, J.C_Z3, J.D_X1)
      J.AYZX_XXII <- round(sum(J.AYZX_XXII_w, na.rm=TRUE), digits = 4)
      J.AZXY_XXIII_w <- c(J.A_A4, J.B_Z3, J.C_X1, J.D_Y2)
      J.AZXY_XXIII <- round(sum(J.AZXY_XXIII_w, na.rm=TRUE), digits = 4)
      J.AZYX_XXIV_w <- c(J.A_A4, J.B_Z3, J.C_Y2, J.D_X1)
      J.AZYX_XXIV <- round(sum(J.AZYX_XXIV_w, na.rm=TRUE), digits = 4)
      
      J.XYZA_MAX = max(J.XYZA_I, J.XYAZ_II, J.XZYA_III, J.XZAY_IV, J.XAYZ_V, J.XAZY_VI,  J.YXZA_VII, J.YXAZ_VIII,  J.YZXA_IX, J.YZAX_X,
                       J.YAXZ_XI, J.YAZX_XII, J.ZXYA_XIII, J.ZXAY_XIV ,J.ZYXA_XV, J.ZYAX_XVI, J.ZAXY_XVII, J.ZAYX_XVIII, J.AXYZ_XIX,
                     J.AXZY_XX, J.AYXZ_XXI, J.AYZX_XXII, J.AZXY_XXIII, J.AZYX_XXIV, na.rm=TRUE)
      
   
       return (J.XYZA_MAX)
    }


  # START I PETLA
  #k-najblizszych sasiadow na start 0
  k <- 0
  
  while (k < k.neighbors)   #parametr, ktory podajemy w fun.test (k.neighbors)
  {
    
    #1# Funkcja wczytujaca zbior danych
    data_inf <- read.data(file, nameColumns) 
    data_infTrain <- read.data_noClusterNumber (data_inf)
    
    #2# Petla liczaca ilosc obserwacji w poszczegolnych grupach     
    for (l in qSet)
    {
      name <- paste('Col',l,sep='')
      totalCollection[[name]] <- Collection(data_inf,l) + length(Collection(data_inf,l-1))+length(Collection(data_inf,l-2))  + length(Collection(data_inf,l-3))                          
    }
    
       
    #3# Funkcja wyszukujaca odchylenia (outliers)
    
    #k - najblizszych sasiadow zwieksza sie o 1 po kazdym przebiegu
    k <- k + 1 #zaczyna sie od k = 1 (w LOF i COF k nie moze byc 0)
    
    
    #Znajdz wartosci odstajace, ustawiajac opcjonalnie k
    #COF
    if (o.algorithm == "COF")
    {
      outliers <- COF.outliers (data_infTrain,k)
    }
    
    #LOF - PARAMETR od k = 1 (nie moze byc k = 0) 
    if (o.algorithm == "LOF")
    {
      outliers <- LOF.outliers(data_infTrain, k)
    }
    

   # START II PETLA
    #o ilosc usuwanych wartosci odstajacych na start ustawiono -1 przed II petla
    o <- -1
    
    while(o < o.outliers) #parametr, ktory podajemy w Funk.test (o.outliers)                                                                                 
    {                                                                                   
      o <- o + 1  #o odstajace zwiekszane o 1 w kazdej petli - zaczyna sie od o = 0
      

     #4# Funkcja usuwajaca odchylenia przed grupowaniem (outliers)
    #przygotowujemy zbior data_infTrain do dalszego procesu obrobki w kazdej petli usuwajac odpowiednia liczbe odchylen
    data_infTrain <- delete.outliers(data_inf, o)           
                  
    #5# Funkcja obliczajaca odleglosci miedzy obiektami
    #w kazdej petli po usunieciu odpowiednich wierszy z obserwacjami odstajacymi (powstaje plik data_infTrain)
    #obliczamy (wybrana metoda w funkcji Funk.test) odleglosci miedzy obserwacjami podajac metode (parametr methodDist)
    #otrzymujemy (d_data_infTrain)       
    d_data_infTrain <- fun.dist(data_infTrain,methodDist)
        
      #6# Funkcja laczaca dwa najblizsze skupienia i przycinajaca drzewo  
    #w kazdej petli po obliczeniu odleglosci przekazujemy (d_data_infTrain) i liczymy wybrana metoda (parametr methodHclust) w fun.test
    #tak otrzymane (hc_data_infTrain) przycinamy wg dlugosci parametru qSet otrzymujac (hc_data_infTrainCutree) 
    hc_data_infTrainCutree <- linkage_cutree(d_data_infTrain, methodHclust, qSet) 
        
     #7# Petla zliczajaca zbiory po usunieciu odchylen i po nowym podziale, wykorzystujaca funkcje CollectionAfterDivision
    x <- NULL    
            
    for (t in qSet)
    {
      name <- paste('X', t, sep = '')
      totalAfterDivision[[name]] <- CollectionAfterDivision(t)
    }
      
     #8 Funkcja obliczajca indeksy Jaccarda (J), okreslajace poodobienstwo miedzy zbiorami
    #przed i po podziale (analizujaca wszystkie przypadki) i wybierajaca maksymalna wartosc J = J_XYZA_MAX               
    J.XYZA_MAX<-J()
      
    #8a Funkcje zliczajace ilosc w kazdym klastrze
    sumaCluster1<-numberInCluster_sumaCluster1(hc_data_infTrainCutree)
    sumaCluster2<-numberInCluster_sumaCluster2(hc_data_infTrainCutree)
    sumaCluster3<-numberInCluster_sumaCluster3(hc_data_infTrainCutree)
    sumaCluster4<-numberInCluster_sumaCluster4(hc_data_infTrainCutree)
    sumaALL<-numberInCluster_sumaALL(hc_data_infTrainCutree)
      

   #8b Szczegolowo w kazdej grupie

      sumaCluster1_1p <- 0
      sumaCluster1_2p <- 0
      sumaCluster1_3p <- 0
      sumaCluster1_4p <- 0
      
      sumaCluster2_1p <- 0
      sumaCluster2_2p <- 0
      sumaCluster2_3p <- 0
      sumaCluster2_4p <- 0
      
      sumaCluster3_1p <- 0
      sumaCluster3_2p <- 0
      sumaCluster3_3p <- 0
      sumaCluster3_4p <- 0
      
      sumaCluster4_1p <- 0
      sumaCluster4_2p <- 0
      sumaCluster4_3p <- 0
      sumaCluster4_4p <- 0
      
      for(i in 1:length(hc_data_infTrainCutree))
      {
        if (hc_data_infTrainCutree[i] == 1 && data_inf[i,]$ClusterNumber == 1)
          sumaCluster1_1p <- sumaCluster1_1p + hc_data_infTrainCutree[i]
        if (hc_data_infTrainCutree[i] == 1 && data_inf[i,]$ClusterNumber == 2)
          sumaCluster1_2p <- sumaCluster1_2p + hc_data_infTrainCutree[i]
        if (hc_data_infTrainCutree[i] == 1 && data_inf[i,]$ClusterNumber == 3)
          sumaCluster1_3p <- sumaCluster1_3p + hc_data_infTrainCutree[i]
        if (hc_data_infTrainCutree[i] == 1 && data_inf[i,]$ClusterNumber == 4)
          sumaCluster1_4p <- sumaCluster1_4p + hc_data_infTrainCutree[i]
        
        if (hc_data_infTrainCutree[i] == 2 && data_inf[i,]$ClusterNumber == 1)
          sumaCluster2_1p <- sumaCluster2_1p + hc_data_infTrainCutree[i]
        if (hc_data_infTrainCutree[i] == 2 && data_inf[i,]$ClusterNumber == 2)
          sumaCluster2_2p <- sumaCluster2_2p + hc_data_infTrainCutree[i]
        if (hc_data_infTrainCutree[i] == 2 && data_inf[i,]$ClusterNumber == 3)
          sumaCluster2_3p <- sumaCluster2_3p + hc_data_infTrainCutree[i]
        if (hc_data_infTrainCutree[i] == 2 && data_inf[i,]$ClusterNumber == 4)
          sumaCluster2_4p <- sumaCluster2_4p + hc_data_infTrainCutree[i]
        
        if (hc_data_infTrainCutree[i] == 3 && data_inf[i,]$ClusterNumber == 1)
          sumaCluster3_1p <- sumaCluster3_1p + hc_data_infTrainCutree[i]
        if (hc_data_infTrainCutree[i] == 3 && data_inf[i,]$ClusterNumber == 2)
          sumaCluster3_2p <- sumaCluster3_2p + hc_data_infTrainCutree[i]
        if (hc_data_infTrainCutree[i] == 3 && data_inf[i,]$ClusterNumber == 3)
          sumaCluster3_3p <- sumaCluster3_3p + hc_data_infTrainCutree[i]
        if (hc_data_infTrainCutree[i] == 3 && data_inf[i,]$ClusterNumber == 4)
          sumaCluster3_4p <- sumaCluster3_4p + hc_data_infTrainCutree[i]
        
        if (hc_data_infTrainCutree[i] == 4 && data_inf[i,]$ClusterNumber == 1)
          sumaCluster4_1p <- sumaCluster4_1p + hc_data_infTrainCutree[i]
        if (hc_data_infTrainCutree[i] == 4 && data_inf[i,]$ClusterNumber == 2)
          sumaCluster4_2p <- sumaCluster4_2p + hc_data_infTrainCutree[i]
        if (hc_data_infTrainCutree[i] == 4 && data_inf[i,]$ClusterNumber == 3)
          sumaCluster4_3p <- sumaCluster4_3p + hc_data_infTrainCutree[i]
        if (hc_data_infTrainCutree[i] == 4 && data_inf[i,]$ClusterNumber == 4)
          sumaCluster4_4p <- sumaCluster4_4p + hc_data_infTrainCutree[i]
      }
      
      sumaCluster1_1 <- sumaCluster1_1p / 1
      sumaCluster1_2 <- sumaCluster1_2p / 1
      sumaCluster1_3 <- sumaCluster1_3p / 1
      sumaCluster1_4 <- sumaCluster1_4p / 1
      
      sumaCluster2_1 <- sumaCluster2_1p / 2
      sumaCluster2_2 <- sumaCluster2_2p / 2
      sumaCluster2_3 <- sumaCluster2_3p / 2
      sumaCluster2_4 <- sumaCluster2_4p / 2
      
      sumaCluster3_1 <- sumaCluster3_1p / 3
      sumaCluster3_2 <- sumaCluster3_2p / 3
      sumaCluster3_3 <- sumaCluster3_3p / 3
      sumaCluster3_4 <- sumaCluster3_4p / 3
      
      sumaCluster4_1 <- sumaCluster4_1p / 4
      sumaCluster4_2 <- sumaCluster4_2p / 4
      sumaCluster4_3 <- sumaCluster4_3p / 4
      sumaCluster4_4 <- sumaCluster4_4p / 4


    C1 <- as.numeric( c (k, o, sumaALL, sumaCluster1, sumaCluster2, sumaCluster3, sumaCluster4, J.XYZA_MAX, sumaCluster1_1, sumaCluster1_2, sumaCluster1_3, sumaCluster1_4, 
      sumaCluster2_1, sumaCluster2_2,sumaCluster2_3,sumaCluster2_4, sumaCluster3_1, sumaCluster3_2, sumaCluster3_3, sumaCluster3_4, sumaCluster4_1, sumaCluster4_2, sumaCluster4_3,sumaCluster4_4)) 
    print(C1)
      
    e <- rbind(C1)
    df_total <- rbind(df_total,e)
      
    closeAllConnections()
      
    } # END I PETLA
    
  }  # END II PETLA
  
  writeInExcel(df_total)
  
   # Caly wiersz maksymalnej wartosci Jaccarda
  wynik <- df_total[which.max(df_total$V8), ]
  k.max <- df_total[which.max(df_total$V8), 1]
  o.max <- df_total[which.max(df_total$V8), 2]
  

  
    #Rysujemy DENDROGRAM - przygotowanie start
  
  #wczytanie danych  
  data_inf <- read.data(file, nameColumns) 
  data_infTrain <- read.data_noClusterNumber (data_inf)

  #obliczenia pomocnicze
  ilosc_obserwacji <-length(data_inf$ClusterNumber) 
  ilosc_obserwacji_1 <-length(which(data_inf$ClusterNumber == 1))
  ilosc_obserwacji_2 <-length(which(data_inf$ClusterNumber == 2))
  ilosc_obserwacji_3 <-length(which(data_inf$ClusterNumber == 3))
  ilosc_obserwacji_4 <-length(which(data_inf$ClusterNumber == 4))

  procent_usnietych_odchylen<-round(((o.max/ilosc_obserwacji)*100), digits = 2)

  #outliers
  #COF
    if (o.algorithm == "COF")
    {
      outliers <- COF.outliers (data_infTrain,k)
    }
    
    #LOF
    if (o.algorithm == "LOF")
    {
      outliers <- LOF.outliers(data_infTrain, k)
    }
    
  #o ilosc usuwanych wartosci odstajacych 
   data_infTrain <- delete.outliers(data_inf, o.max)
   
    #Rysujemy DENDROGRAM - przygotowanie end

   # WLASCIWY DENDROGRAM - RYSUJ - START
   #draw_dend(data_inf,data_infTrain,methodDist,methodHclust,qSet) - nie wykorzystujemy tej funkcji

if (o.max == 0)

{

# WLASCIWY DENDROGRAM START
 
  d_data_infTrain <- dist(data_infTrain, method = methodDist)            
  hc_data_infTrain <- hclust(d_data_infTrain , method = methodHclust)
  hc_data_infTrainCutree <- cutree(hc_data_infTrain, k = length(qSet))
  hc_data_infTrainCutree_num <- as.numeric(hc_data_infTrainCutree)
  
  Train_species <- data_inf[,1]
  #library(dendextend)
  dend <- as.dendrogram(hc_data_infTrain)
  dend %>% head
  # Koloruj galezie w oparciu o klastry:
  dend <- color_branches(dend, k=length(qSet))
  groupLabels = Train_species
  # Dopasuj etykiety, w miare mozliwosci, do prawdziwej klasyfikacji:
  labels_colors(dend) <-rainbow_hcl(length(qSet))[sort_levels_values(as.numeric(data_inf[,1])[order.dendrogram(dend)])]
  # Dodamy zbior do etykiet:
  labels(dend) <- paste(as.character(data_inf[,1])[order.dendrogram(dend)],"(",labels(dend),")", (hc_data_infTrainCutree_num)[order.dendrogram(dend)], sep = "")
  # Zawiesilismy troche dendrogram:
  dend <- hang.dendrogram(dend,hang_height=0.1)
  # zmniejszyc rozmiar etykiet:
  # dend <- assign_values_to_leaves_nodePar (dend, 0.5, "lab.cex")
  dend <- set(dend, "labels_cex", 0.6)
  # i wykres:
  

  #plot(dend,main = "Dendrogram", horiz =  FALSE,  nodePar = list(cex = 0.007))
  
  #rect.hclust(hc_wineTrain, k=3, border="black") #ramka na skupieniach!!!
  #legend("topleft", legend = c(setosa="1",versicolor="2",virginica="3") , fill = rainbow_hcl(3))
  #legend("topleft", legend = c(1,2,3) , fill = rainbow_hcl(3)) #legenda poprawic!!!!!


plot(dend,main = paste("Podsumowanie i dendrogram: ",file,"(","Obiekty:",ilosc_obserwacji," Atrybuty:",(ncol(data_inf)-1)," Grupy kolejno:","{",ilosc_obserwacji_1,";",ilosc_obserwacji_2,";",ilosc_obserwacji_3,";",ilosc_obserwacji_4,"}",")"),   horiz =  FALSE,  nodePar = list(cex = 0.007), col.main = "blue", 
  sub = paste("Po usunieciu odchylen: ", (df_total[which.max(df_total$V8), 3])), col.sub = "#AA4371")

  mtext(c("k-odleglosc"),side=3,line=0,adj = 1, col = "#ff7f00")
  mtext(c(0),side=3,line=-1,adj = 1,col = "#37004D")
  
  mtext(c("Usunieto odchylen"),side=3,line=-2,adj = 1, col = "#ff7f00")
  mtext(paste(o.max,"-> (",procent_usnietych_odchylen,"% )"),side=3,line=-3,adj = 1,col = "#37004D")
  #mtext(c(o.max),side=3,line=-3,adj = 1,col = "#37004D")
  
  mtext(paste("Miara odleglosci: ",methodDist),side=1,line=3,adj = 0, col = "red", at =c(0))
  mtext(paste("Metoda laczenia: ",methodHclust),side=1,line=4,adj = 0, col = "blue", at =c(0))
  
 mtext(paste("Szczegolowo po grupowaniu: " ),side=1,line=-30,adj = 1, col = "#1629e7", cex = 0.8)


  mtext(paste("Grupa 1 ->",(df_total[which.max(df_total$V8), 4])),side=1,line=-29,adj = 1, col = "#003f00", cex = 0.8)
  mtext(paste("( I_",(df_total[which.max(df_total$V8), 9]),",II_",(df_total[which.max(df_total$V8), 10]),",III_",(df_total[which.max(df_total$V8), 11]),",IV_",(df_total[which.max(df_total$V8), 12]),")"),side=1,line=-28,adj = 1, col = "#084f06", cex = 0.7)
 

  mtext(paste("Grupa 2 ->",(df_total[which.max(df_total$V8), 5])),side=1,line=-27,adj = 1, col = "#003f00",cex = 0.8)
  mtext(paste("( I_",(df_total[which.max(df_total$V8), 13]),",II_",(df_total[which.max(df_total$V8), 14]),",III_",(df_total[which.max(df_total$V8), 15]),",IV_",(df_total[which.max(df_total$V8), 16]),")"),side=1,line=-26,adj = 1, col = "#084f06",cex = 0.7)



  mtext(paste("Grupa 3 ->",(df_total[which.max(df_total$V8), 6])),side=1,line=-25,adj = 1, col = "#003f00",cex = 0.8)
  mtext(paste("( I_",(df_total[which.max(df_total$V8), 17]),",II_",(df_total[which.max(df_total$V8), 18]),",III_",(df_total[which.max(df_total$V8), 19]),",IV_",(df_total[which.max(df_total$V8), 20]),")"),side=1,line=-24,adj = 1, col = "#084f06",cex = 0.7)


  mtext(paste("Grupa 4 ->",(df_total[which.max(df_total$V8), 7])),side=1,line=-23,adj = 1, col = "#003f00",cex = 0.8)
  mtext(paste("( I_",(df_total[which.max(df_total$V8), 21]),",II_",(df_total[which.max(df_total$V8), 22]),",III_",(df_total[which.max(df_total$V8), 23]),",IV_",(df_total[which.max(df_total$V8), 24]),")"),side=1,line=-22,adj = 1, col = "#084f06",cex = 0.7)

  
  mtext(paste("J(max sum) = ",(df_total[which.max(df_total$V8), 8]),"(",o.algorithm,")"),side=3,line=0,adj = 0, col = "#ff7f00")
  
  #WLASCIWY DENDROGRAM END


}

else

{

# WLASCIWY DENDROGRAM START
 
  d_data_infTrain <- dist(data_infTrain, method = methodDist)            
  hc_data_infTrain <- hclust(d_data_infTrain , method = methodHclust)
  hc_data_infTrainCutree <- cutree(hc_data_infTrain, k = length(qSet))
  hc_data_infTrainCutree_num <- as.numeric(hc_data_infTrainCutree)
  
  Train_species <- data_inf[,1]
  #library(dendextend)
  dend <- as.dendrogram(hc_data_infTrain)
  dend %>% head
  # Koloruj galezie w oparciu o klastry:
  dend <- color_branches(dend, k=length(qSet))
  groupLabels = Train_species
  # Dopasuj etykiety, w miare mozliwosci, do prawdziwej klasyfikacji:
  labels_colors(dend) <-rainbow_hcl(length(qSet))[sort_levels_values(as.numeric(data_inf[,1])[order.dendrogram(dend)])]
  # Dodamy zbior do etykiet:
  labels(dend) <- paste(as.character(data_inf[,1])[order.dendrogram(dend)],"(",labels(dend),")", (hc_data_infTrainCutree_num)[order.dendrogram(dend)], sep = "")
  # Zawiesilismy troche dendrogram:
  dend <- hang.dendrogram(dend,hang_height=0.1)
  # zmniejszyc rozmiar etykiet:
  # dend <- assign_values_to_leaves_nodePar (dend, 0.5, "lab.cex")
  dend <- set(dend, "labels_cex", 0.6)
  # i wykres:
  

  #plot(dend,main = "Dendrogram", horiz =  FALSE,  nodePar = list(cex = 0.007))
  
  #rect.hclust(hc_wineTrain, k=3, border="black") #ramka na skupieniach!!!
  #legend("topleft", legend = c(setosa="1",versicolor="2",virginica="3") , fill = rainbow_hcl(3))
  #legend("topleft", legend = c(1,2,3) , fill = rainbow_hcl(3)) #legenda poprawic!!!!!


 
 #inna wersja main
 #plot(dend,main = paste("Dendrogram: ",file,"[","Obiektow:",ilosc_obserwacji,"(Gr1:",ilosc_obserwacji_1,", Gr2:",ilosc_obserwacji_2,", Gr3:",ilosc_obserwacji_3,", Gr4:",ilosc_obserwacji_4,")"," Atrybutow:",(ncol(data_inf)-1),"]"),   horiz =  FALSE,  nodePar = list(cex = 0.007), col.main = "blue", 
  #sub = paste("Po usunieciu odchylen: ", (df_total[which.max(df_total$V8), 3])), col.sub = "#AA4371")

 plot(dend,main = paste("Podsumowanie i dendrogram: ",file,"(","Obiekty:",ilosc_obserwacji," Atrybuty:",(ncol(data_inf)-1)," Grupy kolejno:","{",ilosc_obserwacji_1,";",ilosc_obserwacji_2,";",ilosc_obserwacji_3,";",ilosc_obserwacji_4,"}",")"),   horiz =  FALSE,  nodePar = list(cex = 0.007), col.main = "blue", 
  sub = paste("Po usunieciu odchylen: ", (df_total[which.max(df_total$V8), 3])), col.sub = "#AA4371")

  mtext(c("k-odleglosc"),side=3,line=0,adj = 1, col = "#ff7f00")
  mtext(c(k.max),side=3,line=-1,adj = 1,col = "#37004D")
  
  mtext(c("Usunieto odchylen"),side=3,line=-2,adj = 1, col = "#ff7f00")
  mtext(paste(o.max,"-> (",procent_usnietych_odchylen,"% )"),side=3,line=-3,adj = 1,col = "#37004D")
  #mtext(c(o.max),side=3,line=-3,adj = 1,col = "#37004D")
  
  mtext(paste("Miara odleglosci: ",methodDist),side=1,line=3,adj = 0, col = "red", at =c(0))
  mtext(paste("Metoda laczenia: ",methodHclust),side=1,line=4,adj = 0, col = "blue", at =c(0))
  
 mtext(paste("Szczegolowo po grupowaniu: " ),side=1,line=-30,adj = 1, col = "#1629e7", cex = 0.8)

  mtext(paste("Grupa 1 ->",(df_total[which.max(df_total$V8), 4])),side=1,line=-29,adj = 1, col = "#003f00", cex = 0.8)
  mtext(paste("( I_",(df_total[which.max(df_total$V8), 9]),",II_",(df_total[which.max(df_total$V8), 10]),",III_",(df_total[which.max(df_total$V8), 11]),",IV_",(df_total[which.max(df_total$V8), 12]),")"),side=1,line=-28,adj = 1, col = "#084f06", cex = 0.7)
 

  mtext(paste("Grupa 2 ->",(df_total[which.max(df_total$V8), 5])),side=1,line=-27,adj = 1, col = "#003f00",cex = 0.8)
  mtext(paste("( I_",(df_total[which.max(df_total$V8), 13]),",II_",(df_total[which.max(df_total$V8), 14]),",III_",(df_total[which.max(df_total$V8), 15]),",IV_",(df_total[which.max(df_total$V8), 16]),")"),side=1,line=-26,adj = 1, col = "#084f06",cex = 0.7)



  mtext(paste("Grupa 3 ->",(df_total[which.max(df_total$V8), 6])),side=1,line=-25,adj = 1, col = "#003f00",cex = 0.8)
  mtext(paste("( I_",(df_total[which.max(df_total$V8), 17]),",II_",(df_total[which.max(df_total$V8), 18]),",III_",(df_total[which.max(df_total$V8), 19]),",IV_",(df_total[which.max(df_total$V8), 20]),")"),side=1,line=-24,adj = 1, col = "#084f06",cex = 0.7)


  mtext(paste("Grupa 4 ->",(df_total[which.max(df_total$V8), 7])),side=1,line=-23,adj = 1, col = "#003f00",cex = 0.8)
  mtext(paste("( I_",(df_total[which.max(df_total$V8), 21]),",II_",(df_total[which.max(df_total$V8), 22]),",III_",(df_total[which.max(df_total$V8), 23]),",IV_",(df_total[which.max(df_total$V8), 24]),")"),side=1,line=-22,adj = 1, col = "#084f06",cex = 0.7)

  
  mtext(paste("J(max sum) = ",(df_total[which.max(df_total$V8), 8]),"(",o.algorithm,")"),side=3,line=0,adj = 0, col = "#ff7f00")
  
   #WLASCIWY DENDROGRAM END

}


  #Wyswietl
  SumaALL_max <- (df_total[which.max(df_total$V8), 3])
  print(SumaALL_max)
  
  sumaCluster1_max<- (df_total[which.max(df_total$V8), 4])
  print(sumaCluster1_max)
  sumaCluster2_max<- (df_total[which.max(df_total$V8), 5])
  print(sumaCluster2_max)
  sumaCluster3_max<- (df_total[which.max(df_total$V8), 6])
  print(sumaCluster3_max)
  sumaCluster4_max<- (df_total[which.max(df_total$V8), 7])
  print(sumaCluster4_max)

  sumaCluster1_1_max<- (df_total[which.max(df_total$V8), 9])
  print(sumaCluster1_1_max)
  sumaCluster1_2_max<- (df_total[which.max(df_total$V8), 10])
  print(sumaCluster1_2_max)
  sumaCluster1_3_max<- (df_total[which.max(df_total$V8), 11])
  print(sumaCluster1_3_max)
  sumaCluster1_4_max<- (df_total[which.max(df_total$V8), 12])
  print(sumaCluster1_4_max)

  sumaCluster2_1_max<- (df_total[which.max(df_total$V8), 13])
  print(sumaCluster2_1_max)
  sumaCluster2_2_max<- (df_total[which.max(df_total$V8), 14])
  print(sumaCluster2_2_max)
  sumaCluster2_3_max<- (df_total[which.max(df_total$V8), 15])
  print(sumaCluster2_3_max)
  sumaCluster2_4_max<- (df_total[which.max(df_total$V8), 16])
  print(sumaCluster2_4_max)

  sumaCluster3_1_max<- (df_total[which.max(df_total$V8), 17])
  print(sumaCluster3_1_max)
  sumaCluster3_2_max<- (df_total[which.max(df_total$V8), 18])
  print(sumaCluster3_2_max)
  sumaCluster3_3_max<- (df_total[which.max(df_total$V8), 19])
  print(sumaCluster3_3_max)
  sumaCluster3_4_max<- (df_total[which.max(df_total$V8), 20])
  print(sumaCluster3_4_max)

  sumaCluster4_1_max<- (df_total[which.max(df_total$V8), 21])
  print(sumaCluster4_1_max)
  sumaCluster4_2_max<- (df_total[which.max(df_total$V8), 22])
  print(sumaCluster4_2_max)
  sumaCluster4_3_max<- (df_total[which.max(df_total$V8), 23])
  print(sumaCluster4_3_max)
  sumaCluster4_4_max<- (df_total[which.max(df_total$V8), 24])
  print(sumaCluster4_4_max)

  print(ncol(data_inf)-1)
  print(ilosc_obserwacji)
     print(ilosc_obserwacji_1)
     print(ilosc_obserwacji_2)
     print(ilosc_obserwacji_3)
     print(ilosc_obserwacji_4)
  print(file)
  print(procent_usnietych_odchylen)
  print(methodDist)
  print(methodHclust)
  print(k.max)
  print(o.max)
  print (o.algorithm)
  print (df_total[which.max(df_total$V8), 8])
  print(outliers)
  print("Usuniete odchylenia")
  print(outliers[1:o.max])

  return (wynik)
  
  }

# Przydatne do wywolania funkcji:
# Funk.test <- function(file, nameColumns, qSet, methodDist, methodHclust, k.neighbors, o.outliers, o.algorithm)


# Funk.test ("10_iris.data",c('ClusterNumber','Sepal length','Sepal width'),
#         c(1:3),"canberra","complete",2,1,"LOF")

# Funk.test ("iris.data",c('ClusterNumber','Sepal length','Sepal width','Petal length','Petal width'),
#       c(1:3),"canberra","ward.D",4,1,"COF")

# Funk.test ("wine.data",c('ClusterNumber','Alcohol','Malic.acid','Ash','Alcalinity.of.ash','Magnesium','Total.phenol','Flavanoids','NonFlavanoid.phenols','Proanthocyanin','Color.intensity','Hue','OD280.OD315.of.diluted.wines','Proline'), 
#         c(1:3),"canberra","ward.D",5,1,"LOF")

# Funk.test ("breast_tissue.data",c('ClusterNumber','I0','PA500','HFS','DA','Area','A/DA','Max','IP','P'),
#         c(1:4),"canberra","mcquitty",5,4,"LOF")

#methodDist:
# "euclidean"
# "maximum" -> "Czybyszewa"
# "manhattan"
# "canberra"

#methodHclust:
# "single"
# "mcquitty"
# "average"
# "centroid"
# "median"
# "complete"
# "ward.D"





       









