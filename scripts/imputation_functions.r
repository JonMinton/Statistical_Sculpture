###############################################################################################################
###############################################################################################################
### graph_impute function

catch_error <- function(input){
  checker <- try(input)
  if (class(checker)=="try-error"){
    output <- NA
  } else {
    output <- input
  }
  return(output)
}

impute_matrix <- function(X, uselog=T, repeat.it=F){
  
  ifelse(uselog==T, input <- log(X), input <- X )
  
  output <- input
  
  dim.x <- dim(input)[1]
  dim.y <- dim(input)[2]
  
  imputationMatrix <- matrix(0, dim.x, dim.y)
  imputationCoords_na <- which(is.na(X), T)
  imputationCoords_inf <- which(is.infinite(X), T)
  imputationCoords <- rbind(
    imputationCoords_na, 
    imputationCoords_inf
    )
  
  numValsToImpute <- dim(imputationCoords)[1]
  
  for (i in 1:numValsToImpute){
    this.x <- imputationCoords[i,1]
    this.y <- imputationCoords[i,2]
    imputationMatrix[
      this.x, 
      this.y
      ] <- 1
  }
  
  
  for (i in 1:numValsToImpute){
    this.x <- imputationCoords[i,1]
    this.y <- imputationCoords[i,2]
    
    
    output[
      this.x, 
      this.y
      ] <- mean(
        c(
          catch_error(input[this.x - 1, this.y - 1]),
          catch_error(input[this.x - 1, this.y    ]),
          catch_error(input[this.x - 1, this.y + 1]),
          catch_error(input[this.x    , this.y - 1]),
          catch_error(input[this.x    , this.y    ]),
          catch_error(input[this.x    , this.y + 1]),
          catch_error(input[this.x + 1, this.y - 1]),
          catch_error(input[this.x + 1, this.y    ]),
          catch_error(input[this.x + 1, this.y + 1])
        ),
        na.rm=T
      )
  }
  
  remainingValsToImpute<- dim(which(is.na(output), T))[1]
  
  if(repeat.it==T & remainingValsToImpute > 0){
    tmp <- impute_matrix(output, uselog=F, repeat.it=T)
    input <- tmp[["input"]]
    imputed <- tmp[["imputationCoords"]]
    output <- tmp[["output"]]
  }
  
  return(
    list(
      input=input,
      imputed=imputationCoords,
      output=output,
      numValsToImpute=numValsToImpute
    )
  )
}
