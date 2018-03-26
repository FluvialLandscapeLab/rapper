source('~/R Projects/rapper/functionSandbox.R')

myDrivingValues = data.frame(a = 1:10, b = 10:1, c = runif(10), row.names = as.character(0:9))

myRapper = rapper(myDrivingValues, myInit, myExecute)
initRapper(myRapper, x = 1, y= 2)
executeRapper(myRapper)
