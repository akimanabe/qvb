fpath <- "C:/Rfiles/ALK2/masabadata.csv"

sabadata <-
  read.csv(fpath) %>%
  dplyr::as_tibble() %>%
  dplyr::mutate(Yearclass = as.factor(Yearclass))