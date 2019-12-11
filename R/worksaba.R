fpath <- "C:/Rfiles/ALK2/masabadata.csv"

sabadata <-
  read.csv(fpath) %>%
  dplyr::as_tibble() %>%
  dplyr::mutate(Yearclass = as.factor(Yearclass))

saba06 <- sabadata %>% dplyr::filter(Yearclass == 2006)

#graphcheck
ggplot(saba06, aes(x=Age2, y=FL)) + geom_point()

fit_qvb(saba06$FL, saba06$Age2, 350, 0.8, 1.2, 3, 0)

p06 <- p

fit_vb(saba06$FL, saba06$Age2, 450, 0.6, 0)

vp06 <- vbp

grapher <-function(data, qvbp=list(NA), vbp=list(NA)){
  g <- ggplot2::ggplot(data) +
  ggplot2::geom_point(aes(x=Age2, y=FL, colour = as.factor(Year)), alpha = 0.5)
  
    if(!is.na(qvbp)==TRUE){
      qvbp <- unlist(qvbp)
    g <- 
      g +
      stat_function(
        fun=function(x)qvbp[1] * qvbp[4]^qvbp[2] * (1- (pmax(0,1-(1-qvbp[3])*(x-qvbp[5])/qvbp[4]))^(1/(1-qvbp[3])) )^qvbp[2],
        color="dodgerblue4", size=1.5)
    }
  
  if(!is.na(vbp)==TRUE){
    vbp <- unlist(vbp)
    g<- g+
  stat_function(
    fun=function(x) vbp[1] * (1-exp(-vbp[2]*(x-vbp[3]))),
    color="red", size=1
  )
  }
  
  return(g)
}


###### Hrd code ------

sabadata <-
  read.csv(fpath) %>%
  dplyr::as_tibble() %>%
  dplyr::mutate(Yearclass = as.factor(Yearclass))

ydat <- filter(sabadata, Yearclass == 2006)

fit_qvb(ydat$FL, ydat$Age2, 350, 0.8, 1.2, 3, 0); p

fit_vb(ydat$FL, ydat$Age2, 450, 0.6, 0); vbp

grapher(ydat, qvbp=list(p), vbp = list(vbp))
