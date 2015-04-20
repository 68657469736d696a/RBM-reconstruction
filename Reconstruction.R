devtools::install_github('zachmayer/rbm@master')
library('rbm')

### Reconstruction from a single RBM ###
reconstruct.RBM <- function(model, predict, type = 'probs'){
  newdata <- rbind(predict)
  newdata <- cBind(Bias_Unit=rep(1, nrow(newdata)), newdata)
  nm <- rownames(t(model$rotation))
  newdata <- newdata[, nm, drop = FALSE]
  
  hidden_activations <- newdata %*% t(model$rotation)
  rows <- nrow(newdata)
  hidden_probs <- model$activation_function(hidden_activations)
  if(type == 'probs'){
    return(hidden_probs[, -1])
  }
  
  hidden_states <- hidden_probs > Matrix(runif(rows*ncol(t(model$rotation))), nrow=rows, ncol=ncol(t(model$rotation)))
  return(hidden_states[ ,-1])
}

### Reconstruction from stacked RBM's ###
reconstruct.stacked.RBM <- function(stack, predict, type = 'probs'){
  for(i in length(stack$layers):1){
    layer <- list('rotation' = stack$rbm_list[[i]]$rotation, 'activation_function' = stack$activation_function)
    if(i == 1){
      predict <- reconstruct.RBM(layer, predict, type = type)
    }else{
      predict <- reconstruct.RBM(layer, predict)    
    }
  }
  return(predict)
}


### Example data ###
set.seed(10)
Alice <- c('Harry_Potter' = 1, Avatar = 1, 'LOTR3' = 1, Gladiator = 0, Titanic = 0, Glitter = 0) #Big SF/fantasy fan.
Bob <- c('Harry_Potter' = 1, Avatar = 0, 'LOTR3' = 1, Gladiator = 0, Titanic = 0, Glitter = 0) #SF/fantasy fan, but doesn't like Avatar.
Carol <- c('Harry_Potter' = 1, Avatar = 1, 'LOTR3' = 1, Gladiator = 0, Titanic = 0, Glitter = 0) #Big SF/fantasy fan.
David <- c('Harry_Potter' = 0, Avatar = 0, 'LOTR3' = 1, Gladiator = 1, Titanic = 1, Glitter = 0) #Big Oscar winners fan.
Eric <- c('Harry_Potter' = 0, Avatar = 0, 'LOTR3' = 1, Gladiator = 1, Titanic = 0, Glitter = 0) #Oscar winners fan, except for Titanic.
Fred <- c('Harry_Potter' = 0, Avatar = 0, 'LOTR3' = 1, Gladiator = 1, Titanic = 1, Glitter = 0) #Big Oscar winners fan.
dat <- rbind(Alice, Bob, Carol, David, Eric, Fred)


### Example single RBM ###
RBM <- rbm(dat, num_hidden = 2)
predict <- predict(RBM, rbind(dat[1, ]))
reconstruct.RBM(RBM, predict, type = 'states')


### Example stacked RBM's ###
stack <- stacked_rbm(dat, layers = c(4,2))
predict <- predict(stack)[3, ]
reconstruct.stacked.RBM(stack, predict, type = 'states')
