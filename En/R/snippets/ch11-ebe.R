EBE <- get("EBE", envir = e)            # FOCE already has the EBEs
round(head(EBE, 3), 3)

# eta-shrinkage: how far the SD of the EBEs has shrunk below the estimated omega
shr <- 1 - apply(EBE[, 2:4], 2, sd)/sqrt(diag(OM))
round(100*shr, 1)
