# The file is used to setup a replicable R environment

if(!require(renv)) install.packages("renv")
renv::init()
renv::snapshot()
