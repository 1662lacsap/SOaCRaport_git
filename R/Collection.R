#' Funkcja Collection - funkcja pomocnicza sluzaca do obliczen 
#' 
#' Funkcja pomocnicza wykorzystywana w petli zapisuje poszczegolne zbiory
#' 
#' @param data_inf2 Zbior danych
#' @param c Numer grupy
#' @author Czeslaw Horyn choryn@us.edu.pl
#' @export
  
  Collection <- function(data_inf2,c)
  {
    result <- filter(data_inf2,data_inf2$ClusterNumber == c)
    C <- as.integer(row.names(result))
    return(C)
  }