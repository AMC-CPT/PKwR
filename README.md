# Pharmacokinetics with R — code repository

*한국어 설명은 [README.ko.md](README.ko.md) 에 있습니다.*

The R code of the textbook **『약동학 with R — 이론과 계산을 R로 잇는다』**
(*Pharmacokinetics with R: joining the theory to the computation*) by
Kyun-Seop Bae.

The code printed in the book **is** the file in this repository, and the console
output and the figures printed in the book were produced by running it. So the
code in the book cannot drift away from the code that actually ran. Changing a
value and running it again is the way this book asks to be studied.

**The book is written in Korean, and so are the comments in nearly all of the
code files.** Everything else is not language-bound: the file names, the chapter
numbering, the console output and the figures are the same in any language, and
the `TDM-Vanco` web app below has an English interface. An English edition of
the book is in preparation; when it is ready its translated code will be added
here as `En/`, as it already has been for volume 4.

## What is here

| Folder | Contents |
|:--|:--|
| `R/snippets/` | 201 snippets, one file per name. The `NN` of `chNN-name.R` is the chapter number. The order they appear in the book is the order in `R/build.R`, not alphabetical |
| `output/` | the console output of each snippet, 200 files. `ch11-ctl` is a NONMEM control stream shown as an example and is not run |
| `figures/` | the 82 figures the snippets make |
| `R/build.R` | remakes all of it. The session boundaries between chapters are in its comments |
| `R/_freeze.R` | the tool that runs one snippet and freezes its output and figure |
| `ACRE/` | the syntax-highlighting definitions (`Syntax/`) of the ACRE editor that §1.7 introduces, and its README |
| `TDM-Vanco/` | the vancomycin TDM Shiny app of §13.7 (below) |

## Running it

```sh
Rscript R/build.R
```

Run from the repository root, this remakes `output/` and `figures/` from
scratch. It takes 12–15 minutes, most of it the FOCE-I estimation of Chapter 11
and the randomization test and bootstrap of Chapter 12. To look at one snippet,
open that file and just run it.

The packages needed are these. A star marks one the author wrote.

| Package | Chapters | What it does |
|:--|:--|:--|
| `NonCompart`* | 5, 9, 10 | non-compartmental analysis (NCA) |
| `wnl`* | 4, 7, 8, 16, 17 | nonlinear regression and error models |
| `nmw`* | 11, 13 | NONMEM's estimation steps, reproduced in R |
| `BE`* | 14 | bioequivalence analysis |
| `sasLM`* | 14 | linear models in the form of SAS PROC GLM |
| `LBI`* | 3 | likelihood intervals |
| `mathr`* | 2 | numerical tools (machine epsilon, quadrature, numerical differentiation) |
| `deSolve` | 2, 5, 7, 8, 10, 13, 17 | numerical solution of differential equations |
| `PowerTOST` | 14 | power and sample size for equivalence trials |
| `nlme` | 5, 14 | example data and mixed-effects models |

```r
install.packages(c("NonCompart", "wnl", "nmw", "BE", "sasLM", "LBI", "mathr",
                   "deSolve", "PowerTOST", "nlme"))
```

## Things to know

**Some chapters share a session.** Most snippets run in a fresh R session per
chapter, but at three places a chapter inherits the objects of the one before
it. That is deliberate: it lets the book look at the same data two ways, and
saves printing the same function twice.

- Ch 5 → Ch 6: `Cpo`
- Ch 7 → Ch 8 → Ch 9: `dat2`, `C2iv`, `th`, `D`, `CL`, `tobs` (Chapter 8 runs inside Chapter 7's session)
- Ch 11 → Ch 12: `DATA`, `PRED`, `TH`, `OM`, `SG`, `EBE`, `r.fo`, `r.foce`, `cov.fo`

Start a new session in between and the later chapter breaks. The boundaries are
marked in the comments of `R/build.R`.

**The random seeds are fixed.** Every snippet that simulates carries its own
`set.seed()`, so a rerun gives the numbers printed in the book.

**Why the output and the figures are committed.** Normally one records the code
and not what it produced. An exception is made here so that someone who has not
installed R can still see the results while reading the code. The code that
remakes those files is in the same repository, so nothing is taken on trust.

## TDM-Vanco: a vancomycin TDM web app

`TDM-Vanco/` is a Shiny app that runs the TDM engine described in §13.7 of the
book. Its point is that swapping the population model's output (THETA, OMEGA,
SIGMA) is all it takes to move it to another drug.

Its only external dependency is **`shiny`** (`commonmark` is used to render the
markdown of the help tab, and without it the help shows as plain text).
Everything else is base R.

![TDM-Vanco screen](TDM-Vanco/screenshot.png)

*The screen with synthetic data (`TDM-Vanco/tests/fixtures/synthetic_patient.csv`)
loaded. Two observed concentrations give a MAP estimate of the individual
parameters, and the next dose is proposed with a prediction interval.*

### Three ways to run it

**1. Without downloading anything (if you have R)**

```r
shiny::runGitHub("PKwR", "AMC-CPT", subdir = "TDM-Vanco")
```

One line. It fetches from GitHub, unpacks into a temporary folder and starts.

**2. From a clone**

```r
shiny::runApp("TDM-Vanco")     # from the repository root
```

**3. In a browser, with no R at all**

Open <https://amc-cpt.github.io/PKwR/>. No installation and no server.

GitHub cannot run R, and GitHub Pages serves static files only. But
[shinylive](https://posit-dev.github.io/r-shinylive/) exports the app to
WebAssembly, so the R runtime runs inside the browser and static hosting is
enough. Both `shiny` and `commonmark`, which this app uses, are in the webR
repository, so it works unchanged.

```r
install.packages("shinylive")
shinylive::export("TDM-Vanco", "docs")   # builds the static site into docs/
```

The exported site is 66 MB, nearly all of it the R runtime. Putting that on
`main` would add the same weight to the default-branch tarball that
`runGitHub()` downloads in method 1, so it lives on a separate **`gh-pages`
branch** here (Pages source: `gh-pages` / `(root)`). The runtime is fetched on
the first visit and cached by the browser afterwards.

The two differ in one way that matters: in the browser version R runs inside
your own browser, so **patient data you load is never transmitted anywhere.**
The cost is a slow first load.

If a server is wanted instead, this repository can be connected to
shinyapps.io or Posit Connect Cloud.

### Verification

`tests/regression.R` runs a regression check on the **synthetic** data in
`tests/fixtures/` and compares it against `golden_baseline.rds`. No real patient
data is in this repository.

```r
source("TDM-Vanco/tests/regression.R")
```

## The book

『약동학 with R — 이론과 계산을 R로 잇는다』, Kyun-Seop Bae (University of Ulsan
College of Medicine · Asan Medical Center). Three parts, seventeen chapters.
The book is published separately.

For the solutions to the exercises, or to report an error, write to
**ksbae@acr.kr**.

## Licence

The **R code in this repository and what it produces** are under GPL-3
([LICENSE](LICENSE)). The text of the book and its figure captions are not
included here and are separately reserved.

## The other books in the series

| | |
|---|---|
| 1 Scientific Computation with R | <https://github.com/AMC-CPT/SciCompR> |
| 2 Scientific Inference in Clinical Trials with R | <https://github.com/AMC-CPT/CTDA> |
| 4 Pharmacometrics with NONMEM and R | <https://github.com/AMC-CPT/PMx> |
| 5 Essentials of Clinical Drug Development (online appendix) | <https://github.com/AMC-CPT/CDD> |
