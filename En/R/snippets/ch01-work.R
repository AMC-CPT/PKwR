# R evaluates an expression into an object, which stays in the workspace under a name.
CL <- 5.2; V <- 40                            # two objects. <- assigns in R
ls()                                          # what is in the workspace
c(exists = exists("CL"), class = class(CL))

# args shows which arguments a function takes and their defaults; ? gives the details
args(round)                                   # also ?round and example(round)
args(seq.default)

# Naming: case matters, and avoid names that are already in use
c(t = is.function(t), c = is.function(c), df = is.function(df))
T <- FALSE                                    # T and F are ordinary variables
c(TRUE. = TRUE, T. = T)                       # so always write TRUE/FALSE
rm(T); ls()
