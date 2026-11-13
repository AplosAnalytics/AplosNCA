# CRAN submission comments

## AplosNCA 1.1.0

This is a minor release adding automatic JWT refresh and SSO refresh-token
import. The existing username/password and JWT authentication paths are
unchanged, and the package remains backward compatible.

## Test environments

- Local macOS 15, R 4.5.3: `devtools::check()` — 0 errors, 0 warnings, 1 note.
- GitHub Actions, Ubuntu 24.04, R 4.4: `R-CMD-check` — 0 errors, 0 warnings,
  0 notes.

## R CMD check results

0 errors | 0 warnings | 1 note

The note is `unable to verify current time`. It comes from R's clock check,
which cannot reach a time source in the check environment; it is not a package
issue.

## Notes

- The package communicates with the Aplos NCA API. Examples and vignettes are
  self-contained or use `\dontrun{}`; no network access is required during
  `R CMD check`.
- No reverse dependencies.
