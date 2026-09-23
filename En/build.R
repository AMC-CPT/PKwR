# =====================================================================
#  En/build.R  -  regenerate the English edition's frozen R output and figures
#
#  Run from the repository root:   Rscript En/build.R      (about 12-15 min)
#
#  It reuses the Korean edition's machinery (R/_freeze.R and the freeze list
#  of R/build.R, so the snippet order and the shared sessions of chapters
#  5-6, 7-9 and 11-12 are exactly the Korean edition's) and redirects the
#  three paths and the figure font through options:
#      snippets  R/snippets/  ->  En/R/snippets/   (translated)
#      output    output/      ->  En/output/       (regenerated)
#      figures   figures/     ->  En/figures/      (regenerated, Latin face)
#
#  All 201 snippets must be translated before this runs: the Korean freeze
#  list is sourced as it is, and a missing translated snippet would stop it
#  halfway through a shared session. En/check_chapter.py verifies that the
#  translated code is identical to the Korean code outside comments and
#  string literals, and En/numcheck.py verifies afterwards that every number
#  in En/output/ equals the Korean edition's.
# =====================================================================
if (!dir.exists("R/snippets"))
  stop("Run from the repository root (where R/snippets/ lives).")

dir.create("En/output",  showWarnings = FALSE, recursive = TRUE)
dir.create("En/figures", showWarnings = FALSE, recursive = TRUE)

ko <- sub("\\.R$", "", list.files("R/snippets", "\\.R$"))
en <- sub("\\.R$", "", list.files("En/R/snippets", "\\.R$"))
miss <- setdiff(ko, en)
if (length(miss))
  stop("untranslated snippet(s): ", paste(miss, collapse = ", "))

options(pkwr.snipdir = "En/R/snippets",
        pkwr.outdir  = "En/output",
        pkwr.figdir  = "En/figures",
        pkwr.figfont = "Arial")

#  R/build.R sources R/_freeze.R itself, creates output/ and figures/ at the
#  root (they exist) and then runs the freeze list; with the options above
#  every freeze() writes into En/.
source("R/build.R")

message(sprintf("English edition: %d outputs, %d figures in En/",
                length(list.files("En/output", "\\.txt$")),
                length(list.files("En/figures", "\\.pdf$"))))
