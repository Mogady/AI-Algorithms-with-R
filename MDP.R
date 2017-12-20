mdp.function<-function(){
#formulate the problem by defining the {states,actions,transtion matrix,
#instantaneous reward matrix,starting Utality}

states<-c('cool','warm','overheated')
actions<-c('slow','fast')
pair1<-c('cool','slow','cool',1)
pair2<-c('cool','fast','warm',0.5)
pair3<-c('cool','fast','cool',0.5)
pair4<-c('warm','slow','warm',0.5)
pair5<-c('warm','slow','cool',0.5)
pair6<-c('warm','fast','overheated',0.5)
mdp<-matrix(c(pair1,pair2,pair3,pair4,pair5,pair6),nrow = 6,ncol = 4,byrow = TRUE)
reward1<-c('cool','slow',1)
reward2<-c('cool','fast',2)
reward3<-c('warm','slow',1)
reward4<-c('warm','fast',-10)
rewards<-matrix(c(reward1,reward2,reward3,reward4),nrow = 4,ncol = 3,byrow = TRUE)
U<-list()
for(i in states){
  U[i]<-0
}
return(valueIteration.function(U,rewards,mdp,states,actions))
}
#copy function to use old utality values
copy.function<-function(list){
  U1<-list()
  for (i in names(list)){
    U1[i]<-list[i]
  }
  return(U1)
}
#transition function takes state and action and return next state S` and probability
T.function<-function(s,a,mdp){
  x<-mdp[mdp[,1]==s&mdp[,2]==a,c(3,4)]
  return(matrix(x,ncol = 2))
}
#Rewards funcion takes state and action and return instantaneous reward
R.function<-function(s,a,rewards){
  x<-rewards[rewards[,1]==s&rewards[,2]==a,c(3)]
  return(x)
}
#value iteration function return the utality matrix after calculations and optimal actions
valueIteration.function<-function(U,rewards,mdp,states,actions){
  optimalAc<-list()
  counter<-3
  while(counter>0){
  U1<-copy.function(U)
  for(s in states[1:2]){
    #select state 
    m<-0
    for (a in actions){
      #for every action in this state computer the value for it and then max ver all actions
      total<-0
      x<-T.function(s,a,mdp)
      for(i in 1:nrow(x)){
        total<-total+(as.numeric(x[i,2])*U1[[x[i,1]]])
      }
      total<-as.numeric(R.function(s,a,rewards))+total
      if(total>m){
      m<-total
      optimalAc[s]<-a
      }
    }
    U[[s]]<-m
    #uncomment to print optimal actions
    #print(optimalAc)
    
  }
  counter<-counter-1
  }
  return(U)
}
U<-mdp.function()
print(U)
