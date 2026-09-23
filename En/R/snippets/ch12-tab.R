TAB <- TabStep()                        # the table corresponding to NONMEM's sdtab
head(TAB[, c("ID", "TIME", "DV", "PRED", "CIPREDI", "CWRES")], 4)
