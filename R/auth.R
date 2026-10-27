
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
#' @return A character string containing the JWT token.
#' @seealso [aplos_import_jwt()] for SSO users.
#' @export
#' @importFrom AzureAuth decode_jwt
#' @examples
#' \dontrun{
#'   token <- aplos_get_jwt(client_id = "your_client_id", username = "user",
#'                          password = "pass", region = "us-east-1")
#' }
aplos_get_jwt <- function(client_id, username, password, region) {
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
#' Tokens expire one hour after they are issued. When an imported token has
#' less than 20 minutes remaining, the package re-reads the file before each
#' API call, so a token refreshed by another tool is picked up automatically.
#' If the file still contains an expired token, the API call stops with a
#' message asking you to update the file.
#'
#' @param path Path to the file containing the JWT. Defaults to the value of
#'   the `APLOS_JWT_FILE` environment variable when set, otherwise `~/.token`.
#' @return The JWT token, invisibly.
#' @seealso [aplos_get_jwt()] for username/password authentication.
#' @export
#' @examples
#' \dontrun{
#'   # SSO users: save the token from the web interface to ~/.token first.
#'   token <- aplos_import_jwt()
#'
#'   result <- aplos_get_upload_url("data.csv",
#'                                  "https://api.app.aplos-nca.com", token)
#' }
aplos_import_jwt <- function(path = NULL) {
  if (is.null(path) || !nzchar(path)) {
    path <- Sys.getenv("APLOS_JWT_FILE", "")
    if (!nzchar(path)) {
      path <- "~/.token"
    }
  }
  path <- normalizePath(path.expand(path), winslash = "/", mustWork = FALSE)

  token <- .aplos_read_token_file(path)
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

  .aplos_session_register(token, token_file = path)

  expiry_text <- if (is.null(claims$exp)) {
    "expiry unknown"
  } else {
    paste0("expires ",
           format(as.POSIXct(claims$exp, origin = "1970-01-01", tz = "UTC"),
                  "%Y-%m-%d %H:%M:%S UTC"))
  }
  message("Imported Aplos NCA token from ", path, " (", expiry_text, ").")

  invisible(token)
}
