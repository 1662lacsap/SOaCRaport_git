#' Funkcja LOF.outliers -  znajduje wartosci odstajace ustawiajac opcjonalnie k
#' 
#' Funkcja LOF.outliers() dla okreslonego zbioru danych i podanego
#' wspolczynnika k-sasiadow znajduje wartosci odstajace (outliers)
#' Funkcja LOF - parametr od k = 1 (nie moze byc 0)
#'
#' @param data_set Zbior danych
#' @param k k-sasiadow
#' @author Czeslaw Horyn choryn@us.edu.pl
#' @export
  
  LOF.outliers <- function (data_set, k)
  {
    outlier.scores <- lofactor(data_set, k)                                
    outliers <- order(outlier.scores, decreasing=T)
    return (outliers)
  }