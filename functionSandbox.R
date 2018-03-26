myExecute = function(rapper = parent.frame()){
  print(paste(a,b,c,x,y))
}

myInit = function(...){
  modelParams = list(...)
  mapply(assign, x = names(modelParams), value = modelParams, MoreArgs = list(envir = parent.frame()))
  return(T)
}

# mapply(assign, x = names(modelParams), value = modelParams, MoreArgs = list(envir = rapper))
