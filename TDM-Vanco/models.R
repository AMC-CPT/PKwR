# =====================================================================
# models.R  -  Population PK models + dosing targets for the Vanco TDM app
# ---------------------------------------------------------------------
# Single source of truth for the parameter sets.  Both the Inje block
# (formerly hand-copied into Start3.R AND Start4.R) and the AMC block now
# live here exactly once, removing the transcription-error hazard where a
# stray digit in one copy would silently change every EBE for one app.
#
# Each model is list(TH, OM, SG) where
#   TH = c(CL, V1, V2, Q) population typical values (CL is CLcr-scaled:
#        CL = TH[1] * CLcr/100 * exp(ETA1), CLcr capped at 150 in the engine)
#   OM = OMEGA, the ETA variance-covariance matrix (4x4; a zero diagonal
#        entry means that parameter has no IIV and is pinned near 0)
#   SG = SIGMA, residual error c(proportional^2, 0, 0, additive^2)
# =====================================================================

PARSETS = list(
  # AMC  - 3 ETAs (V2 has no IIV), proportional error
  AMC = list(
    TH = c(3.96, 33.1, 48.3, 6.99),
    OM = diag(c(0.149, 0.12, 0, 0.416)),
    SG = matrix(c(0.231^2, 0, 0, 0), nrow = 2)),

  # Inje - 4 ETAs (full OMEGA block), combined error
  Inje = list(
    TH = c(3.8135955291021233, 39.889510090195238, 44.981835351176571, 2.0055189192561507),
    OM = matrix(c( 0.10855133849022583,     -1.52445093837639736E-002, -0.19698189309256298,       0.11914555131547180,
                  -1.52445093837639736E-002, 3.01016276351715687E-003,  2.31285256338822770E-002, -6.61597586201947800E-003,
                  -0.19698189309256298,      2.31285256338822770E-002,  1.0420667710781930,       -0.19223488058085114,
                   0.11914555131547180,     -6.61597586201947800E-003, -0.19223488058085114,       0.25741541019610537), nrow = 4),
    SG = matrix(c(0.14019731106615912^2, 0, 0, 1.8662549226759475^2), nrow = 2)))

AUC_TARGET = 410       # target daily AUC (mg*h/L)
TINF       = 1         # default infusion duration for the recommended dose (h)
