#' Funkcja read.data_noClusterNumber - wczytanie danych bez numerow grup (B)
#' 
#' Funkcja read.data_noClusterNumber() - wczytanie danych bez numerow grup (B)
#' 
#' @param data_inf_ Zbior danych
#' @author Czeslaw Horyn choryn@us.edu.pl
#' @export
  
  read.data_noClusterNumber <- function (data_inf_) 
  {
    data_infTrain <- data_inf_[, which(names(data_inf_) != "ClusterNumber")]
    return (data_infTrain) 
  }