complete <- function(directory, id = 1:332) {
  
  direct <- list.files(directory, pattern="*.csv", full.names=TRUE)
  x <- lapply(direct, read.csv)
  z <- x[id]
  t <- 0
  acumul <- matrix(nrow=0, ncol=2)
  
   for(i in 1:length(id)) {
    y <- z[i]
    dataf <- as.data.frame(y)
    teste <- cbind(dataf$sulfate,dataf$nitrate)
  
    t <- sum(complete.cases(teste))
    
    acumul <- rbind(acumul, c(id[i],t))
    
    t <- 0
  }
  colnames(acumul) <- c("id", "nobs")
  as.data.frame(acumul)
  
}