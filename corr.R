corr <- function(directory, threshold=0) {
     
  direct <- list.files(directory, pattern="*.csv", full.names=TRUE)
  x <- lapply(direct, read.csv)
  id <- 1:332
    t <- 0
  corre <- numeric()
  
  for(i in 1:length(id)) {
  
   y <- x[i]
    dataf <- as.data.frame(y)
    teste <- cbind(dataf$sulfate,dataf$nitrate)
    
    t <- sum(complete.cases(teste))
    
    if(t > threshold) {
            
    teste <- teste[complete.cases(teste),]
    
      corre[i] <- cor(teste[,1], teste[,2])
    }
  
  else {
    corre[i] <- NA
    
  }
  
    t <- 0
  }

 corre[!is.na(corre)]


  
}