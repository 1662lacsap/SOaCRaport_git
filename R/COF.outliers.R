#' Funkcja COF.outliers -  znajduje wartosci odstajace dla danego k
#' 
#' Funkcja COF.outliers() dla okreslonego zbioru danych i podanego
#' wspolczynnika k-sasiadow znajduje wartosci odstajace (outliers)
#' 
#' @param data_set Zbior danych
#' @param k k-sasiadow
#' @author Czeslaw Horyn choryn@us.edu.pl
#' @export
    
  COF.outliers <- function (data_set, k)
  {
    outlier_score <- COF(data_set, k)							                       
    names(outlier_score) <- 1:nrow(data_set)
    outliers_sort <- sort(outlier_score, decreasing = TRUE)
    outliers <- c(as.numeric(names(outliers_sort)))
    return (outliers)
  }