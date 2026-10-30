# AplosNCA 1.1.0

## Authentication

* JWT tokens are now refreshed automatically. Before each authenticated API
  call the package checks the token's expiry and, when less than 20 minutes
  remain, exchanges the Aplos NCA Refresh Token for a new JWT. Scripts keep
  using the same token object; no re-authentication is required.
* `aplos_execution_status()` re-checks the token on every poll, so long-running
  analyses survive token expiry.
* `aplos_get_jwt()` now captures the Aplos NCA Refresh Token that comes back with
  the JWT. `return_refresh = TRUE` returns both, for callers that want to
  persist the Aplos NCA Refresh Token (for example in a token file).
* New `aplos_import_jwt()` authenticates single sign-on (SSO) users from an
  Aplos NCA Refresh Token rather than a JWT. The file holds `REFRESH_TOKEN`,
  `COGNITO_CLIENT_KEY` and `COGNITO_REGION`; the package exchanges the Aplos NCA Refresh
  Token for a JWT at import and renews it automatically, exactly like a
  password login. A bare JWT file is still accepted for backwards
  compatibility, but it cannot be renewed.
* Documentation now states that tokens expire one hour after issue, replacing
  the previous "~15 minutes, extended by each API call" description.

## Internal

* Authentication state lives in a package-local registry (`R/auth-state.R`).
  The refresh window is configurable with
  `options(AplosNCA.refresh_window_seconds = ...)` for testing.
* `options(AplosNCA.debug = TRUE)` reports when a refresh happens and which
  mechanism was used (Aplos NCA Refresh Token or token file re-read).
