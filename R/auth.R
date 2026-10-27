
#' Obtain Aplos NCA API JWT Token
#'
#' @description
#' Authenticates with the Aplos NCA API using AWS Cognito. This function sends a POST request to retrieve a JWT token for subsequent API calls. All API calls require a valid JWT.
#'
#' The JWT expires one hour after it is issued. The package checks the token
#' before every API call and, when less than 20 minutes remain, transparently
#' refreshes it using the Cognito refresh token returned at login. This refresh
#' happens inside the API functions, so a script can keep using the token
#' object returned here without authenticating again.
#'
#' If the refresh token is rejected or unavailable, the API call stops with a
#' message asking you to authenticate again; the package never retries with a
#' stored password.
#'
#' Users who sign in through single sign-on (SSO) cannot use username/password
#' authentication. Those users should obtain a JWT from the Aplos NCA web
#' interface and use [aplos_import_jwt()] instead.
#'
#' Note: Store credentials securely; do not hardcode them in scripts. It is recommended that you use a hidden text file (e.g. ".aplos_creds") stored on your local computer. Read that file in and then parse it to update the required information for authentication. An example credential file may look like the following:
#' APLOS_API_URL="https://api.app.aplos-nca.com"
#' COGNITO_CLIENT_KEY="your_cognito_client_id/key"
#' COGNITO_USER_NAME="your_username"
#' COGNITO_PASSWORD="your_password"
#' COGNITO_REGION="your_aws_region"
#' All of this information can be found in your user profile for your Aplos NCA account under "API Settings".
#'
#' @param client_id The Cognito client ID.
#' @param username Your Aplos NCA username.
#' @param password Your Aplos NCA password.
#' @param region The AWS region (e.g., "us-east-1").
#' @param return_refresh Logical; if `TRUE`, return a list with the JWT and the
#'   Cognito refresh token instead of just the JWT (default `FALSE`). The
#'   refresh token is captured either way and used for automatic renewal.
#' @return A character string containing the JWT token, or, when
#'   `return_refresh = TRUE`, a list with elements `token` and `refresh_token`.
#' @seealso [aplos_import_jwt()] for SSO users.
#' @export
#' @importFrom AzureAuth decode_jwt
#' @examples
#' \dontrun{
#'   token <- aplos_get_jwt(client_id = "your_client_id", username = "user",
#'                          password = "pass", region = "us-east-1")
#' }
aplos_get_jwt <- function(client_id, username, password, region,
                          return_refresh = FALSE) {
  if (missing(client_id) || missing(username) || missing(password) || missing(region)) {
    stop("All parameters (client_id, username, password, region) are required.")
  }

  headers <- c("Content-Type" = "application/x-amz-json-1.1",
               "X-Amz-Target" = "AWSCognitoIdentityProviderService.InitiateAuth")
  body <- list(
    AuthFlow = "USER_PASSWORD_AUTH",
    AuthParameters = list("USERNAME" = username, "PASSWORD" = password),
    ClientId = client_id,
    method_type = "post"
  )

  response <- tryCatch({
    httr::POST(url = paste0("https://cognito-idp.", region, ".amazonaws.com/"),
               httr::add_headers(.headers = headers),
               body = jsonlite::toJSON(body, auto_unbox = TRUE))
  }, error = function(e) stop("JWT request failed: ", e$message))

  if (httr::http_error(response)) {
    stop("JWT authentication failed: ", httr::content(response, "text"))
  }

  result <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))
  auth <- result$AuthenticationResult
  jwt <- auth$IdToken
  if (is.null(jwt) || !nzchar(jwt)) {
    stop("JWT authentication failed: the response did not include an ID token.")
  }

  # Remember the refresh token so that near-expiry tokens can be renewed
  # without asking for the password again.
  .aplos_session_register(
    jwt,
    refresh_token = auth$RefreshToken,
    client_id = client_id,
    region = region
  )

  if (isTRUE(return_refresh)) {
    return(list(token = jwt, refresh_token = auth$RefreshToken %||% NULL))
  }

  return(jwt)
}

#' Import an Aplos NCA JWT Token from a File
#'
#' @description
#' Reads a JWT from a hidden text file so that it can be used with the other
#' Aplos NCA API functions. This is the authentication path for users who sign
#' in through single sign-on (SSO), because SSO accounts cannot authenticate
#' with a username and password through [aplos_get_jwt()].
#'
#' Obtain the token from the Aplos NCA web interface and save it on a single
#' line in a file in your home directory, for example `~/.token`. A leading
#' `Bearer ` prefix is allowed. Restrict the file so that only you can read it
#' (`chmod 600 ~/.token` on macOS and Linux). The token is never printed by the
#' package and is not written anywhere else.
#'
#' The file may hold just the JWT, or the JWT plus a Cognito refresh token so
#' that the package can renew it automatically. Two layouts are accepted:
#'
#' \preformatted{
#' # Positional: line 1 = token, optional line 2 = refresh token
#' eyJraWQiOi...
#' eyJjdHkiOi...
#'
#' # Key/value
#' JWT="eyJraWQiOi..."
#' REFRESH_TOKEN="eyJjdHkiOi..."
#' COGNITO_CLIENT_KEY="your_cognito_client_id"
#' COGNITO_REGION="us-east-1"
#' }
#'
#' Tokens expire one hour after they are issued. When the file includes a
#' refresh token (and a client ID and region), the package exchanges it for a
#' new JWT when less than 20 minutes remain, exactly as it does for
#' [aplos_get_jwt()]. Without a refresh token, it re-reads the file before each
#' API call so a token refreshed by another tool is picked up automatically; if
#' the file still contains an expired token, the call stops with a message
#' asking you to update it.
#'
#' @param path Path to the file containing the JWT. Defaults to the value of
#'   the `APLOS_JWT_FILE` environment variable when set, otherwise `~/.token`.
#' @param client_id Optional Cognito client ID used to refresh the token.
#'   Defaults to the `COGNITO_CLIENT_KEY` value in the file, then the
#'   `APLOS_COGNITO_CLIENT_ID` environment variable.
#' @param region Optional AWS region used to refresh the token. Defaults to the
#'   `COGNITO_REGION` value in the file, then the `APLOS_COGNITO_REGION`
#'   environment variable.
#' @return The JWT token, invisibly.
#' @seealso [aplos_get_jwt()] for username/password authentication.
#' @export
#' @examples
#' \dontrun{
#'   # SSO users: save the token from the web interface to ~/.token first.
#'   token <- aplos_import_jwt()
#'
#'   # With a refresh token in the file or supplied here:
#'   token <- aplos_import_jwt("~/.token", client_id = "your_client_id",
#'                             region = "us-east-1")
#'
#'   result <- aplos_get_upload_url("data.csv",
#'                                  "https://api.app.aplos-nca.com", token)
#' }
aplos_import_jwt <- function(path = NULL, client_id = NULL, region = NULL) {
  if (is.null(path) || !nzchar(path)) {
    path <- Sys.getenv("APLOS_JWT_FILE", "")
    if (!nzchar(path)) {
      path <- "~/.token"
    }
  }
  path <- normalizePath(path.expand(path), winslash = "/", mustWork = FALSE)

  config <- .aplos_read_token_config(path)
  token <- config$token
  claims <- .aplos_token_claims(token)

  if (is.null(claims$tenant_id) || is.null(claims$user_id)) {
    warning("The imported token is missing the Aplos NCA tenant or user claim. ",
            "API calls may fail; check that the whole token was copied.",
            call. = FALSE)
  }

  if (!is.null(claims$exp) && claims$exp <= .aplos_now()) {
    stop("The token in '", path, "' expired at ",
         format(as.POSIXct(claims$exp, origin = "1970-01-01", tz = "UTC"),
                "%Y-%m-%d %H:%M:%S UTC"),
         ". Download a new token and replace the file contents.",
         call. = FALSE)
  }

  if (is.null(client_id) || !nzchar(client_id)) {
    client_id <- config$client_id
  }
  if (is.null(client_id) || !nzchar(client_id)) {
    client_id <- Sys.getenv("APLOS_COGNITO_CLIENT_ID", "")
  }
  if (is.null(region) || !nzchar(region)) {
    region <- config$region
  }
  if (is.null(region) || !nzchar(region)) {
    region <- Sys.getenv("APLOS_COGNITO_REGION", "")
  }
  if (!is.null(client_id) && !nzchar(client_id)) client_id <- NULL
  if (!is.null(region) && !nzchar(region)) region <- NULL

  refresh_token <- config$refresh_token
  if (!is.null(refresh_token) && (is.null(client_id) || is.null(region))) {
    warning("The token file contains a refresh token, but no client ID or region ",
            "was found, so automatic renewal is disabled. Add COGNITO_CLIENT_KEY ",
            "and COGNITO_REGION to the file, or pass client_id and region.",
            call. = FALSE)
    refresh_token <- NULL
  }

  .aplos_session_register(
    token,
    refresh_token = refresh_token,
    client_id = client_id,
    region = region,
    token_file = path
  )

  expiry_text <- if (is.null(claims$exp)) {
    "expiry unknown"
  } else {
    paste0("expires ",
           format(as.POSIXct(claims$exp, origin = "1970-01-01", tz = "UTC"),
                  "%Y-%m-%d %H:%M:%S UTC"))
  }
  renewal_text <- if (is.null(refresh_token)) {
    "no refresh token"
  } else {
    "refresh token available"
  }
  message("Imported Aplos NCA token from ", path, " (", expiry_text, "; ",
          renewal_text, ").")

  invisible(token)
}
