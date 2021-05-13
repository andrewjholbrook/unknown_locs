setwd("~/unknown_locs/")

# simulates 2d spat-temp Hawkes process
# Note: trimodal normal spatial bg rate, bimodal normal temporal bg rate
# Note: exponential temporal trigger (1/omega lengthscale)
# Note: Gaussian spatial trigger (sigma lengthscale)

generate_bg_events <- function (N=100,sigma_bg=1,tau_bg=1) {
  bg_events <- matrix(0,N,3) 
  for (i in 1:N) {
    bg_events[i,] <- c(rnd_bg_event_location(sigma_bg),rnd_bg_event_time(tau_bg))
  }
  return(bg_events)
}

rnd_bg_event_location <- function (sigma_bg) {
  Mode <- sample(1:3,size=1)
  Means <- 50/sqrt(2)*matrix(c(0,1,1,0,(1+sqrt(3))/2,(1+sqrt(3))/2),3,2,byrow=TRUE) # equidistant points
  location <- rnorm(n=2,mean = Means[Mode,],sd=sigma_bg)
  return(location)
} 

rnd_bg_event_time <- function (tau_bg) {
  Mode <- sample(1:2,size=1)
  Means <- c(30,80) # equidistant points
  time <- truncnorm::rtruncnorm(n=1,mean = Means[Mode],sd=tau_bg,a=0)
  return(time)
}

rnd_offspring <- function(m=20,Gl) { # Gi coordinate vector (x,y,t)
  count <- rpois(n=1,lambda = m)
  if (count==0) {
    return(rep(NA,3)) 
  } else if (count==1) {
    locations <- rnorm(n=2,mean = Gl[1:2],sd=0.5)
    times <- rexp(n=1,rate = 5) + Gl[3]
    return(c(locations,times)) 
  } else {
    locations <- matrix(0,count,2)
    times <- rep(0,count)
    for(i in 1:count) {
      locations[i,] <- rnorm(n=2,mean = Gl[1:2],sd=0.5)
      times[i] <- rexp(n=1,rate = 5) + Gl[3]
    }
    return(cbind(locations,times))
  }
}

next_gen <- function(prev_gen,m=20) {
  if(is.vector(prev_gen)) {
    children <- rnd_offspring(m=m,Gl=as.vector(prev_gen))
    return(children)
  } else if( !is.null(nrow(prev_gen)) ) {
    children <- rnd_offspring(m=m,Gl=as.vector(prev_gen[1,]))
    if(nrow(prev_gen)>1) {
      for(i in 2:nrow(prev_gen)) {
        newChildren <- rnd_offspring(m=m,Gl=as.vector(prev_gen[i,]))
        if(!any(is.na(newChildren))) children <- rbind(children, newChildren)
        
      }
    }
    return(children)
  } else {
    return(vector())
  }
}

generate_sample <- function(mean_N_bg_points=40,m=0.5) {
  N_bg_points <- rpois(n=1,lambda = mean_N_bg_points)
  
  generations <- list()
  generations[[1]] <- generate_bg_events(N=N_bg_points,sigma_bg = 10,tau_bg = 10)
  i <- 1
  while( sum(complete.cases(generations[[i]]))>0 ) {
    i <- i + 1
    generations[[i]] <- next_gen(generations[[i-1]],m=m)
    if(is.matrix(generations[[i]])) {
      generations[[i]] <- generations[[i]][complete.cases(generations[[i]]),]
    } else if (is.vector(generations[[i]]) & any(is.na(generations[[i]]))) {
      generations[[i]] <- NA
    }
  }
  
  finalOut <- generations[[1]]
  for(i in 2:length(generations)) {
    finalOut <- rbind(finalOut,generations[[i]])
  }
  return(finalOut[complete.cases(finalOut),]) 
}




