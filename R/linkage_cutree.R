#' Funkcja linkage_cutree - funkcja laczaca dwa najblizsze skupienia i przycinajaca drzewo (dendrogram)
#' 
#' Funkcja linkage_cutree() w kazdej petli po obliczeniu odleglosci przekazujemy (d_data_infTrain)
#' i liczymy wybrana metoda (parametr methodHclust) w Funk.test(),
#' tak otrzymane (hc_data_infTrain) przycinamy wg dlugosci parametru qSet otrzymujac (hc_data_infTrainCutree)
#' 
#' @param d_data_infTrain Zbior danych
#' @param methodHclust Metoda laczenia
#' @param qSet Liczba grup
#' @author Czeslaw Horyn choryn@us.edu.pl
#' @export
  
  linkage_cutree <- function (d_data_infTrain, methodHclust, qSet) 
  {
    hc_data_infTrain <- hclust(d_data_infTrain , method = methodHclust)                                           
    hc_data_infTrainCutree <- cutree(hc_data_infTrain, k=length(qSet)) 
    return (hc_data_infTrainCutree)
  }