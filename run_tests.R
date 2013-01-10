library('RUnit')
 
test.suite <- defineTestSuite("example",
				dirs = file.path("tests"),
				testFileRegexp = '^test_.*\\.R')
 
test.result <- runTestSuite(test.suite)
 
printTextProtocol(test.result)
