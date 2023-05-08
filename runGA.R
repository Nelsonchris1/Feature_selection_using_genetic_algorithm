library(GA)

#This function is used by the GA to compute or report the statistics of your interest after every generation.
#This function overrides the default functionality provided by gaMonitor().
monitor <- function(obj){
  # gaMonitor(obj)                      #call the default gaMonitor to print the usual messages during evolution
  iter <- obj@iter                      #get the current iternation/generation number 
  if (iter <= maxGenerations){          #some error checking
    fitness <- obj@fitness              #get the array of all the fitness values in the present population
    #<<- assigns a value to the global variable declared outside the scope of this function.    
    thisRunResults[iter,1] <<- max(fitness)
    thisRunResults[iter,2] <<- mean(fitness)    
    thisRunResults[iter,3] <<- median(fitness)
    cat(paste("\rGA | generation =", obj@iter, "Mean =", thisRunResults[iter,2], "| Best =", thisRunResults[iter,1], "\n"))
    flush.console()
  }  
  else{                               #print error messages
    cat("ERROR: iter = ", iter, "exceeds maxGenerations = ", maxGenerations, ".\n")
    cat("Ensure maxGenerations == nrow(thisRunResults)")
  }
}
ga_control <<- gaControl(crossoverProbability = 0.8, crossoverType = "uniform", uniformCrossoverProbability = 0.5)


two_point <- function(object, parents){
  fitness <- object@fitness[parents]
  parents <- object@population[parents, ,drop=FALSE]
  n <- ncol(parents)
  children <- matrix(as.double(NA), nrow=2, ncol=n)
  fitnessChildren <- rep(NA,2)
  
  pt1 <- sample(1:(n-1), size = 1)
  pt2 <- sample((pt1+1):n, size = 1)
  
  temp_a <- parents[1, ][pt1:pt2]
  temp_b <- parents[2, ][pt1:pt2]
  parents[1, ][pt1:pt2] <- temp_b
  children[1, ] <- parents[1, ]
  parents[2, ][pt1:pt2] <- temp_a
  children[2, ] <- parents[2, ]
  out <- list(children = children, fitness = fitnessChildren)
  return(out)
}




runGA <- function(noRuns = 30, problem = "feature"){
  #Specify GA parameter values; using the default values below. 
  if (problem == "feature"){
    maxGenerations <<- 25    #<<- makes it a global variable. So it will be visible to other functions e.g. monitor()
    popSize = 100
    pcrossover = 0.8
    gaControl  = ga_control
    pmutation = 0.2
    type = "binary"
    svm_df <- getData()
    xx <- svm_df[,-ncol(svm_df)]
    yy <- svm_df[,ncol(svm_df)]
    fitness = classfeatureFitness #fitness function defined in feature-selection.R
  }
  else if (problem == "tsp"){
    maxGenerations <<- 50
  
    popSize = 100
    pcrossover = 0.8
    pmutation = 0.2
    type = "permutation"
    data = getData()
    min = 1                             #minimum is city indexed 1
    max = nrow(getData())               #maximum is the number of cities in the data set
    fitness = tspFitness                #fitness function defined in TSP.R
  }
  else {
    cat("invalid problem specified. Exiting ... \n")
    return()
  }
  
  
  #Set up what stats you wish to note.    
  statnames = c("best", "mean", "median")
  thisRunResults <<- matrix(nrow=maxGenerations, ncol = length(statnames)) #stats of a single run
  resultsMatrix = matrix(1:maxGenerations, ncol = 1)  #stats of all the runs
  
  
  resultNames = character(length(statnames)*noRuns)
  resultNames[1] = "Generation"
  
  bestFitness <<- -Inf
  bestSolution <<- NULL
  my_list <<- c()
  for (i in 1:noRuns){
    cat(paste("Starting Run ", i, "\n"))
    if (problem == "feature")
      GA <- ga(type=type, fitness = fitness, xx=xx, yy=yy, nBits = ncol(xx), 
               names = colnames(xx), seed=i, popSize = popSize, 
               pcrossover = pcrossover, crossover = gabin_spCrossover, pmutation = pmutation, 
               maxiter = maxGenerations, monitor= monitor)
    else if (problem == "tsp")
      GA <- ga(type = type, fitness = fitness, distMatrix = data, 
                     min = min, max = max, popSize = popSize, maxiter = maxGenerations,
                     pcrossover=pcrossover, pmutation = pmutation, monitor= monitor, seed = i )
    else if (problem == "vehicle")
      GA <- ga(type = type, fitness = fitness, 
               min = min, max = max, popSize = popSize, maxiter = maxGenerations,
               pcrossover=pcrossover, pmutation = pmutation, monitor= monitor, seed = i)
    
    
      
    resultsMatrix = cbind(resultsMatrix, thisRunResults)
    
    if (GA@fitnessValue > bestFitness){
      bestFitness <<- GA@fitnessValue
      bestSolution <<- GA@solution
    }
    #Create column names for the resultsMatrix
    for (j in 1:length(statnames)) resultNames[1+(i-1)*length(statnames)+j] = paste(statnames[j],i)
  }
  colnames(resultsMatrix) = resultNames
  return (list(resultsMatrix, bestFitness, bestSolution, sum(bestSolution[1,])))
}

getBestFitness<-function(){
  return(bestFitness)
}

getBestSolution<-function(){
  return(bestSolution)
}