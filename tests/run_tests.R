library(popReconstruct)
source('test_functions.R')

cat("\nTESTING Burkina Faso females reconstruction.")
test.sampler()

cat("\nTESTING Thailand two-sex reconstruction.")
test.sampler.two.sex()
