##here i'mworking on coloring map problem 
main.function<-function(){
  ##intialize the proplem and construct matrix holds the map 
  WA<-c('NT','SA')
  SA<-c('WA','NT','Q','NSW','V')
  NT<-c('WA','SA','Q')
  Q<-c('SA','NT','NSW')
  NSW<-c('SA','Q','V')
  V<-c('SA','NSW')
  map<-list(WA,SA,NT,Q,NSW,V)
  names(map)<-c('WA','SA','NT','Q','NSW','V')
  vaiables<-c('WA','NT','Q','NSW','V','SA','T')
  domains<-c('R','G','B')
  domain<-list()
  assignment<-list()
  for (i in vaiables){
    ##inialize the assignment list with -1 means no variable is assigned 
    domain[i]<-list(domains)
    assignment[i]<--1
  }
  backtrack.function(assignment,domain,map)
}
##check to see if every variable assigned or not
complete.function<-function(assignment){
  for (var in names(assignment)){
    if (assignment[[var]]==-1){
      return(0)
    }
  }
  return(TRUE)
  
  
}
##select unassigned variable from the list
select.function<-function(assignment){
  for (var in names(assignment)){
    if (assignment[[var]]==-1){
      return(var)
    }
  }
  
}
##return the domain of every variable
domain.function<-function(domain,var){
  return(domain[[var]])
  
  
}
##check the constrain of map coloring problem 
check.function<-function(var,map,assignment){
  neigh<-map[[var]]
  for (ne in neigh){
    if((assignment[[ne]]!=-1)&(assignment[[var]]==assignment[[ne]])){return(FALSE)}
    next
  } 
  return(TRUE)
}
##the backtrack function 
backtrack.function<-function(assignment,domain,map){
  ##check to see if the vaiables assigned stop recursing
   if(complete.function(assignment)){
    return(assignment)
   }
  var<-select.function(assignment)
  #try to assing a variable and check the constrain
  for (value in domain.function(domain,var)){
    assignment[[var]]<-value
    if (check.function(var,map,assignment)){
      ##if succeded recuse to the next variable 
      result<-backtrack.function(assignment,domain,map)
      if (length(result)>1){return(result)}
      #if not un assign it 
      assignment[[var]]<--1
    }
  }
  return(0)  
}

main.function()

