# AplosNCA 1.1.0

## Authentication

* JWT tokens are now refreshed automatically. Before each authenticated API
  call the package checks the token's expiry and, when less than 20 minutes
  remain, exchanges the Cognito refresh token for a new ID token. Scripts keep
  using the same token object; no re-authentication is required.
* `aplos_execution_status()` re-checks the token on every poll, so long-running
  analyses survive token expiry.
* New `aplos_import_jwt()` reads a JWT from a hidden file (`~/.token` by
  default, or `APLOS_JWT_FILE`) for single sign-on (SSO) users, who cannot
  authenticate with a username and password. Imported tokens are re-read when
  they are close to expiry, so externally refreshed token files are picked up
  automatically.
* Documentation now states that tokens expire one hour after issue, replacing
  the previous "~15 minutes, extended by each API call" description.

## Internal

* Authentication state lives in a package-local registry (`R/auth-state.R`).
  The refresh window is configurable with
  `options(AplosNCA.refresh_window_seconds = ...)` for testing.
* `options(AplosNCA.debug = TRUE)` reports when a refresh happens and which
  mechanism was used (Cognito refresh token or token file re-read).
