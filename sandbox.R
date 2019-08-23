myDrivingValues = data.frame(a = 1:10, b = 10:1, c = runif(10), row.names = as.character(0:9))

myExecute = function(rapper = parent.frame()){
  print(paste(a,b,c,x,y))
}

myRapper = rapper(execute = myExecute,
                  drivingValues = myDrivingValues,
                  x = "x",
                  y = "y")
executeRapper(myRapper)

