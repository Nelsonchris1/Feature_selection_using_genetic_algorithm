parseData <- function(data, firstcolumn, noRuns){
    col <- firstcolumn
    
    allstats <- (ncol(data)-1)/noRuns   #how many stats were collected. Omit the first column (Generations)
    cols <- seq(col,noRuns*allstats, by=allstats)
    subdata <- data[,cols]
    noGens <- nrow(data)
    pdata <- matrix(nrow = noGens, ncol = 3)
    for (i in 1:noGens){
      pdata[i,1] = i
      pdata[i,2] = mean(subdata[i,])
      pdata[i,3] = 1.96*sd((subdata[i,]))/sqrt(noRuns)   #compute the length of error bar. 
    }
  
    return (pdata)
}

statest <- function(p1, p2, p3){
  stat_test <- data.frame(Uniform = p1[,2],
                          single_point = p2[,2],
                          two_point = p3[,3])
  library(reshape2)
  data_long <- melt(stat_test)
  
  model <- aov(value ~ variable, data = data_long)
  return (summary(model))
}

