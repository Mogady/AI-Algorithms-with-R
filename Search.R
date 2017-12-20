# problem:Alfie was a prisoner in mythland. Though Alfie was a witty and intelligent guy.
# He was confident of escaping prison.After few days of observation,
# He figured out that the prison consists of (N×N) cells.i.e The shape of prison was (N×N) matrix. 
# Few of the cells of the prison contained motion detectors.
# So Alfie planned that while escaping the prison he will avoid those cells containing motion detectors.
# Yet before executing his plan,Alfie wants to know the total number of unique possible paths which he can take to escape the prison.
# Initially Alfie is in cell 
# (1,1) while the exit of the cell (N,N).
# 0 0 0 1 
# 0 0 0 0 
# 1 1 1 0 
# 1 0 0 0 
library(rstack)
graph<-vector(mode="list", length=15)
#construct matrix as graph 
names(graph)<-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
graph[[1]]<-c(2,5)
graph[[2]]<-c(3,6)
graph[[3]]<-c(4,7)
graph[[4]]<-c(8)
graph[[5]]<-c(6,9)
graph[[6]]<-c(7,10)
graph[[7]]<-c(8,11)
graph[[8]]<-c(12)
graph[[9]]<-c(10,13)
graph[[10]]<-c(11,14)
graph[[11]]<-c(12,15)
graph[[12]]<-c(16)
graph[[13]]<-c(14)
graph[[14]]<-c(15)
graph[[15]]<-c(16)
#inialize the problem 
c1<-c(0,0,0,1)
c2<-c(0,0,0,0)
c3<-c(1,1,1,0)
c4<-c(1,0,0,0)
prison<-matrix(c(c1,c2,c3,c4),nrow = 4,ncol = 4)
#chech the cell value 
check.function<-function(index){
  if (prison[index]==1){
    return(TRUE)
  }
  else{return(FALSE)}
  
}
dfs.function<-function(grap,start,end){
  checked<-vector()
  cells<-vector()
  paths<-list()
  S<-stack$new()
  i<-1
  S$push(c(start))
  while(!(S$is_empty())){
    #pop the path and cehck the last cell in it 
    vertex<-S$pop()
    node<-tail(vertex,n=1)
    #if the node is the goal write this path and continue to next possible path 
    if(node==end){
      print(vertex)
      write(x = vertex,file = 'data',append = TRUE)
      next
    }
      #append the node to check list
      checked[i]<-node
      i<-i+1
    #iterate for each successor in the graph and push the new paths
    for(j in graph[[node]]){
        if(!check.function(j)){
          path<-vector()
          k<-1
          for (i in vertex){
            path[k]<-i
            k<-k+1
          }
          path[k]<-j
          S$push(path)
          }

    }
    }

}

dfs.function(graph,1,16)
