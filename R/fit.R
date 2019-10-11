#fit growth funciton (vb)

#### optim2 --- residual sumof squares

fitvb <- function(age, L, p){

fit2 <- function(age, L, p){

  #define function
  vbp <- function(p){
    t <- age
    p[1]*(1-exp(-p[2]*(t-p[3])))
  }

  #calculate rss
  rss <- function(p){sum((vbp(p)-L)^2)}

  #optimize
  res <- optim(p, rss)

  return(res)

}

res1 <- fit2(age = dat1$age, L = dat1$TL, p = c(200,0.4, 0))

newdat <- data.frame(dat1, estimL = vbp(res1$par))

grapher <- function(){
  ggplot2::ggplot(newdat)+
    geom_point(aes(x=age, y=TL), alpha = 0.5, col = "black",pch = 1, size = 2)+
    geom_line(aes(x=age, y=estimL), color = "blue", size = 1.2)
}

return(list(res1, grapher()))

}
