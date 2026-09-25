## Submission

This is the first submission of ggextreme.

## Test environments

* Local: macOS 27.0, R 4.6.0.
* GitHub Actions: macOS (R release), Windows (R release), Ubuntu (R release
  and R oldrel-1).

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

## Notes for the reviewers

* The words flagged as possibly misspelled in DESCRIPTION are method and
  data names (CINeMA, Kaplan, Meier, nomograms, choropleth, E-values).
* The CRAN status and downloads badges in README.md link to the package's
  CRAN page, which will exist once the package is accepted.
* The package bundles the Lato font (SIL Open Font License 1.1) and country
  flag artwork from the flag-icons project (MIT License). Their copyright
  holders and licenses are listed in inst/COPYRIGHTS, which the Copyright
  field in DESCRIPTION points to, and the license texts are installed with
  them.
* Examples that fit a network meta-analysis, run the proportional hazards
  tests or write animations are wrapped in \donttest{} because they take more
  than a few seconds. Animation examples, tests and vignettes use a single
  core, and everything they write goes to tempdir().
* Locally, some examples took more than 5 seconds of elapsed time because the
  machine was busy with other work; none used more than 2.6 seconds of CPU.
* The package's many interactive articles are on its website rather than
  shipped as vignettes, to keep the package small.
