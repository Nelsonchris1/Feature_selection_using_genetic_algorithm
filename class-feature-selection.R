read_data <- function(){
  svm_df = read.csv('df_train.csv', header = TRUE)
  svm_df$RainTomorrow <- factor(svm_df$RainTomorrow)
  return (svm_df)
  }

classfitness <- function(string, xx, yy) {
  inc <- which(string == 1)
  svm_df <- read_data()
  if(sum(inc) == 0)
    return(0)
  
  outcome <- "RainTomorrow"
  inputs <- paste(names(xx)[inc], collapse = " + ")
  fRpart <- as.formula(paste(outcome, inputs, sep=" ~ "))
  
  svm_model <- svm(formula = fRpart, 
                   data = svm_df , type = 'C-classification',
                   kernel = "linear",
                   cost = 1)
  
  t_pred = predict(svm_model,svm_df, type='class')
  return(mean(svm_df$RainTomorrow == t_pred))
}



classfeatureFitness <- function(string,xx,yy) {
  df <- getData()
  inc <- which(string == 1)  #'inc' includes those features/variables for which 'string' contains 1
 
  if(sum(inc)==0)                          
    return(0)                          
  
   
  outcome <-"y"
  inputs <- paste(names(xx)[inc], collapse =" + ")
  
  fRpart <- as.formula(paste(outcome, inputs, sep=" ~ "))
  
  DT <- rpart(formula = fRpart, method="class", control = rpart.control(minsplit = 3),
               data = df)
  
  t_pred = predict(DT,df, type='class')
  
  #Maximise accuracy
  return(mean(df$y == t_pred))
  
  #Maximise accuracy and minimise the number of features
  #return(mean(dataDT$Species == t_pred) * (1 - sum(string == 1)/length(string)))
}

