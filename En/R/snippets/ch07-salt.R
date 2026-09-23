# Salt factor S: parent-drug fraction of the salt molecular weight (mind stoichiometry)
MW <- c(phenytoin  = 252.27, Na.phenytoin    = 274.25,   # 1:1 salt
        metoprolol = 267.36, metoprolol.tart = 684.81)   # 2:1 salt (tartrate)
S  <- c(Na.phenytoin    =   MW[["phenytoin"]]/MW[["Na.phenytoin"]],
        metoprolol.tart = 2*MW[["metoprolol"]]/MW[["metoprolol.tart"]])
round(S, 3)

# Effective dose (mg) of 300 mg phenytoin sodium given orally (assuming F = 0.9)
round(S[["Na.phenytoin"]]*0.9*300, 1)
