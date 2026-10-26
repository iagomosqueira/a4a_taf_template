# Prepare data, write CSV data tables

# Before:
# After:

library(TAF)
mkdir("data")

library(FLCore)

# LOAD example data

data(ple4)
data(ple4.indices)

# SUBSET
stk <- window(ple4, start=1990)
idx <- window(ple4.indices[c(2,3,5)], start=1990)

# SAVE

save(stk, idx, file="data/data.rda", compress="xz")
