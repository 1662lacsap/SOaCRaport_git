#' Funkcja writeInExcel() zapisujaca dane do pliku Excel o nazwie ZRZUT.xlsx w Arkuszu1
#' 
#' Funkcja writeInExcel() zapisujaca dane do pliku Excel o nazwie ZRZUT.xlsx w Arkuszu1
#' 
#' @param df_total_ Dane do zapisu
#' @author Czeslaw Horyn choryn@us.edu.pl
#' @export
 
 writeInExcel<-function(df_total_) 
{
 write.xlsx(df_total_, file="ZRZUT.xlsx", sheetName = "Arkusz1",
             col.names = TRUE, row.names = TRUE, append = FALSE)
}
