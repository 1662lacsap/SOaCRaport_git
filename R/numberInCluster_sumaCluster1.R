#' Funkcja zliczajaca ilosc obserwacji w grupie 1
#' 
#' Funkcja numberInCluster_sumaALL(hc_data_infTrainCutree_) - oblicza ilosc obserwacji w grupie 1
#' 
#' @param hc_data_infTrainCutree_ Zbior danych
#' @author Czeslaw Horyn choryn@us.edu.pl
#' @export

 numberInCluster_sumaCluster1<-function(hc_data_infTrainCutree_)
 {
      
      sumaCluster1p <- 0
      sumaCluster2p <- 0
      sumaCluster3p <- 0
      sumaCluster4p <- 0
      for(i in 1:length(hc_data_infTrainCutree_))
      {
        if (hc_data_infTrainCutree_[i]==1)
          sumaCluster1p <-sumaCluster1p+hc_data_infTrainCutree_[i]
        if (hc_data_infTrainCutree_[i]==2)
          sumaCluster2p <-sumaCluster2p+hc_data_infTrainCutree_[i]
        if (hc_data_infTrainCutree_[i]==3)
          sumaCluster3p <-sumaCluster3p+hc_data_infTrainCutree_[i]
        if (hc_data_infTrainCutree_[i]==4)
          sumaCluster4p <-sumaCluster4p+hc_data_infTrainCutree_[i]
      }
      sumaCluster1 <- sumaCluster1p/1
      sumaCluster2 <- sumaCluster2p/2
      sumaCluster3 <- sumaCluster3p/3
      sumaCluster4 <- sumaCluster4p/4
      
      sumaALL <- sumaCluster1+sumaCluster2+sumaCluster3+sumaCluster4
      

return ( sumaCluster1)

    }