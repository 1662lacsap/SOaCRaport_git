#' Funkcja delete.outliers2 - funkcja usuwajaca odchylenia przed grupowaniem
#' 
#' Przygotowujemy zbior data_infTrain do dalszego procesu obrobki w kazdej petli usuwajac odpowiednia liczbe odchylen
#' 
#' @param data_inf Zbior danych
#' @param o Ilosc odchylen
#' @author Czeslaw Horyn choryn@us.edu.pl
#' @export
   
    delete.outliers2 <- function(data_inf, o)          
        {
          if (o == 0)
        {
         data_infTrain <- data_inf[  ,which(names(data_inf) != "ClusterNumber")]                          
        }
          if (o > 0)
          {
           #p przyjmuje odchylenia od 1 do aktualnej wartosci o
            p <- outliers[1:o]       
           #usuwamy odchylenia
            data_infTrain <- data_inf[-p,which(names(data_inf) != "ClusterNumber")]                         
          }
          return(data_infTrain)
          }