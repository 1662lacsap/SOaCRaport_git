#' Funkcja fun.dist - funkcja obliczajaca odleglosci miedzy obiektami
#' 
#' Funkcja fun.dist() w kazdej petli po usunieciu odpowiednich wierszy z obserwacjami odstajacymi (powstaje plik data_infTrain).
#' Obliczamy wybrana metoda w funkcji Funk.test odleglosc miedzy obserwacjami podajac metode (parametr methodDist),
#' otrzymujemy (d_data_infTrain)
#'
#' @param data_infTrain Zbior danych
#' @param methodDist Metoda odleglosci
#' @author Czeslaw Horyn choryn@us.edu.pl
#' @export
  
  fun.dist <- function(data_infTrain, methodDist)
  {
    d_data_infTrain <- dist(data_infTrain, method = methodDist) 
    return (d_data_infTrain)
  }