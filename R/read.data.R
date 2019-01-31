#' Funkcja read.data - wczytanie danych (A)
#' 
#' Funkcja read.data() - file -> przygotowane pliki (zbiory danych) nalezy umiescic w folderze 
#' przygotowanego pakietu R "SOaCRaport (ang. Search Outliers and Clustering Raport)"
#' Przygotowanie pliku: plik (file,zbior danych) powinien zawierac ponumerowana pierwsza kolumne ClusterNumber i dalej pozostale atrybuty
#' 
#' @param file_data Zbior danych
#' @param nameColumns Nazwy kolumn
#' @author Czeslaw Horyn choryn@us.edu.pl
#' @export
  
  read.data <- function (file_data,nameColumns) 
  {
    data_inf <-  read.table(file = file_data, header=FALSE, sep=',', stringsAsFactors=FALSE, col.names = nameColumns)
    return (data_inf) 
  }