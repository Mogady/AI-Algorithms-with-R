q.learn <- function(R, N, alpha, gamma, final_state) {
  #initalize Q matrix with zero values
  Q <- matrix(rep(0,length(R)), nrow=nrow(R))
  #iterate with number of iteration determind
  for (i in 1:N) {
    #select state random
    cs <- sample(1:nrow(R), 1)
    while (TRUE) {
      #get the possible action and there rewards from this state
      next.states <- which(R[cs,] > -1)
      #if it hve one action take it and get next state 
      if (length(next.states)==1)
        ns <- next.states
      #else select one random
      else
        ns <- sample(next.states,1)
      #evaluate Q value from the equation 
        Q[cs,ns] <- (1-alpha)*Q[cs,ns] + alpha*(R[cs,ns] + gamma*max(Q[ns, which(R[ns,] > -1)]))
      if (ns == final_state) break
      cs <- ns
    }
  }
  #normalize the Q value
  return(100*Q/max(Q))
}
#inialize the problem
main.function<-function(){
  N <- 1000
  alpha <- 0.5
  gamma <- 0.8
  tgt.state <- 6
  iter<-100
  R <- matrix(c(-1,-1,-1,-1,0,-1,-1,-1,-1,0,-1,0,-1,-1,-1,0,-1,-1,-1,0,0,-1,0,-1,0,-1,-1,0,-1,0,-1,100,-1,-1,100,100),nrow=6)
  q.learn(R,iter,alpha,gamma,tgt.state)
  
  
}
main.function()
