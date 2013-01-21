library('RUnit')
source("R/bottomup.R")
source("R/topdown.R")
 
test.suite <- defineTestSuite("example",
				dirs = file.path("tests"),
				testFileRegexp = '^runit_.*\\.R')
 
test.result <- runTestSuite(test.suite)
 
printTextProtocol(test.result)
