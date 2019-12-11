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
  
  data <- data %>% mutate(Year = as.factor(Year))
  
  g <- ggplot2::ggplot(data) +
  ggplot2::geom_point(aes(x=Age2, y=FL, colour = Year), alpha = 0.5)
  
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
  
  g <-
    g + 
    theme_classic() +
    theme(legend.position = c(0.9, 0.3))+
    
    theme(legend.key = element_rect(fill = "white", color = NA),
          text = element_text(size=20)) + 
    xlab("Age (Years)") +
    ylab("Fork length (mm)")
    # theme(legend.background = element_rect(fill="white"),
    #       legend.box.background = element_rect(colour = "white"))
  
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


# allsaba

sabadata <-
  read.csv(fpath) %>%
  dplyr::as_tibble() %>%
  dplyr::mutate(Yearclass = as.factor(Yearclass))

ydat <- filter(sabadata, Yearclass == 2006)

fit_qvb(ydat$FL, ydat$Age2, 350, 0.8, 1.2, 3, 0); p

pars <- matrix(nrow=11, ncol=5)
pars <- cbind(c(2006:2016),pars)

for(i in 2006:2016){
  ydat <- filter(sabadata, Yearclass == i)
  fit_qvb(ydat$FL, ydat$Age2, 350, 0.8, 1.2, 3, 0)
  pars[i-2005,2:6]<-p
}


#####


allgrapher <-function(data, pars){
  
  data <- sabadata %>% mutate(Year = as.factor(Yearclass))
  
  g <- ggplot2::ggplot(data) +
    ggplot2::geom_point(aes(x=Age2, y=FL, colour = Yearclass), alpha = 0.5)
  

  p06 <- pars[1,2:6]
  p07 <- pars[2,2:6]
  p08 <- pars[3,2:6]
  p09 <- pars[4,2:6]
  p10 <- pars[5,2:6]
  p11 <- pars[6,2:6]
  p12 <- pars[7,2:6]
  p13 <- pars[8,2:6]
  p14 <- pars[9,2:6]
  p15 <- pars[10,2:6]
  p16 <- pars[11,2:6]
  
  g <-
    g +
    stat_function(fun=function(x)p06[1] * p06[4]^p06[2] * (1- (pmax(0,1-(1-p06[3])*(x-p06[5])/p06[4]))^(1/(1-p06[3])) )^p06[2],color="dodgerblue4", size=1.5) +
    stat_function(fun=function(x)p07[1] * p07[4]^p07[2] * (1- (pmax(0,1-(1-p07[3])*(x-p07[5])/p07[4]))^(1/(1-p07[3])) )^p07[2],color="dodgerblue4", size=1.5) +
    stat_function(fun=function(x)p08[1] * p08[4]^p08[2] * (1- (pmax(0,1-(1-p08[3])*(x-p08[5])/p08[4]))^(1/(1-p08[3])) )^p08[2],color="dodgerblue4", size=1.5) +
    stat_function(fun=function(x)p09[1] * p09[4]^p09[2] * (1- (pmax(0,1-(1-p09[3])*(x-p09[5])/p09[4]))^(1/(1-p09[3])) )^p09[2],color="dodgerblue4", size=1.5) +
    stat_function(fun=function(x)p10[1] * p10[4]^p10[2] * (1- (pmax(0,1-(1-p10[3])*(x-p10[5])/p10[4]))^(1/(1-p10[3])) )^p10[2],color="dodgerblue4", size=1.5) +
    stat_function(fun=function(x)p11[1] * p11[4]^p11[2] * (1- (pmax(0,1-(1-p11[3])*(x-p11[5])/p11[4]))^(1/(1-p11[3])) )^p11[2],color="dodgerblue4", size=1.5) +
    stat_function(fun=function(x)p12[1] * p12[4]^p12[2] * (1- (pmax(0,1-(1-p12[3])*(x-p12[5])/p12[4]))^(1/(1-p12[3])) )^p12[2],color="dodgerblue4", size=1.5) +
    stat_function(fun=function(x)p13[1] * p13[4]^p13[2] * (1- (pmax(0,1-(1-p13[3])*(x-p13[5])/p13[4]))^(1/(1-p13[3])) )^p13[2],color="dodgerblue4", size=1.5) +
    stat_function(fun=function(x)p14[1] * p14[4]^p14[2] * (1- (pmax(0,1-(1-p14[3])*(x-p14[5])/p14[4]))^(1/(1-p14[3])) )^p14[2],color="dodgerblue4", size=1.5) 
  
    # stat_function(fun=function(x)p15[1] * p15[4]^p15[2] * (1- (pmax(0,1-(1-p15[3])*(x-p15[5])/p15[4]))^(1/(1-p15[3])) )^p15[2],color="dodgerblue4", size=1.5) +
    # stat_function(fun=function(x)p16[1] * p16[4]^p16[2] * (1- (pmax(0,1-(1-p16[3])*(x-p16[5])/p16[4]))^(1/(1-p16[3])) )^p16[2],color="dodgerblue4", size=1.5) 
    
  qvbp <- pars[2,2:6]
  
  g <-
    g +
    stat_function(fun=function(x)qvbp[1] * qvbp[4]^qvbp[2] * (1- (pmax(0,1-(1-qvbp[3])*(x-qvbp[5])/qvbp[4]))^(1/(1-qvbp[3])) )^qvbp[2],color="dodgerblue4", size=1.5)
  g
  
  return(g)
}
