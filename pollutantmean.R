pollutantmean <- function(directory, pollutant, id = 1:332) {
        
  
      direct <- list.files(directory, pattern="*.csv", full.names=TRUE)
      x <- lapply(direct, read.csv)
      z <- x[id]
      somatot <- 0
      nlinhtot <- 0
      
      for(i in 1:length(id)) {
        y <- z[i]
        dataf <- as.data.frame(y)
        
        if(pollutant == "nitrate") {
          
          somaid <- sum(dataf$nitrate, na.rm = TRUE)
          nlinhid <- sum(!is.na(dataf$nitrate))
        }
        
        else {
          
          somaid <- sum(dataf$sulfate, na.rm = TRUE)
          nlinhid <- sum(!is.na(dataf$sulfate))
          
        }
        
          somatot <- somaid+somatot
        nlinhtot <- nlinhid + nlinhtot
        
              }
        meanid <- somatot/nlinhtot
        meanid
  
}
