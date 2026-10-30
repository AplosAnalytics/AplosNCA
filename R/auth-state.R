# Internal authentication state -------------------------------------------
#
# JWT tokens are passed to the exported functions as plain character strings.
# A string cannot be modified in place, so seamless refresh works like this:
#
#   * Every token the package knows about is registered in a session record
#     stored in the package environment.
#   * Each authenticated API call resolves the token to its session, refreshes
#     it when it is close to expiring, and then registers the new token against
#     the same session.
#   * Old token strings keep resolving to the session, so `token` held in the
#     user's script transparently becomes the current token.

.aplos_state <- new.env(parent = emptyenv())
.aplos_state$sessions <- list()
.aplos_state$lookup <- character(0)

`%||%` <- function(x, y) if (is.null(x)) y else x

# Number of seconds before expiry at which a token is refreshed. Can be
# overridden with options(AplosNCA.refresh_window_seconds = ...).
.aplos_refresh_window <- function() {
  as.numeric(getOption("AplosNCA.refresh_window_seconds", 20 * 60))
}

.aplos_now <- function() as.numeric(Sys.time())

# Testing helper: forget every registered session.
.aplos_state_reset <- function() {
  .aplos_state$sessions <- list()
  .aplos_state$lookup <- character(0)
  invisible(NULL)
}

# Register or update the session that owns `token`. Returns the session list.
.aplos_session_register <- function(token, refresh_token = NULL, client_id = NULL,
                                    region = NULL, token_file = NULL) {
  if (!is.character(token) || length(token) != 1L || is.na(token) || !nzchar(token)) {
    stop("A valid Aplos NCA JWT is required.", call. = FALSE)
  }

  id <- unname(.aplos_state$lookup[token])
  if (length(id) != 1L || is.na(id) || !nzchar(id)) {
    id <- paste0("session-", length(.aplos_state$sessions) + 1L)
    .aplos_state$sessions[[id]] <- list(
      id = id,
      token = token,
      refresh_token = NULL,
      client_id = NULL,
      region = NULL,
      token_file = NULL,
      warned_expiring = FALSE
    )
  }

  session <- .aplos_state$sessions[[id]]
  if (!is.null(refresh_token) && nzchar(refresh_token)) session$refresh_token <- refresh_token
  if (!is.null(client_id) && nzchar(client_id)) session$client_id <- client_id
  if (!is.null(region) && nzchar(region)) session$region <- region
  if (!is.null(token_file) && nzchar(token_file)) session$token_file <- token_file

  .aplos_state$sessions[[id]] <- session
  .aplos_state$lookup[[token]] <- id
  session
}

# Find the session for a token, creating an unrefreshable record for tokens the
# package did not issue (for example a token pasted in from elsewhere).
.aplos_session_get <- function(token) {
  id <- unname(.aplos_state$lookup[token])
  if (length(id) == 1L && !is.na(id) && nzchar(id)) {
    return(.aplos_state$sessions[[id]])
  }
  .aplos_session_register(token)
}

.aplos_session_update <- function(session, token, refresh_token = NULL) {
  session$token <- token
  if (!is.null(refresh_token) && nzchar(refresh_token)) session$refresh_token <- refresh_token
  session$warned_expiring <- FALSE
  .aplos_state$sessions[[session$id]] <- session
  .aplos_state$lookup[[token]] <- session$id
  invisible(session)
}

# JWT claims used by the package. Returns NULLs instead of failing so that
# opaque or mocked tokens keep working.
.aplos_token_claims <- function(token) {
  payload <- tryCatch(AzureAuth::decode_jwt(token)$payload, error = function(e) NULL)
  if (is.null(payload)) {
    return(list(exp = NULL, tenant_id = NULL, user_id = NULL))
  }
  exp <- payload$exp
  list(
    exp = if (is.null(exp)) NULL else suppressWarnings(as.numeric(exp)),
    tenant_id = payload[["custom:aplos_user_tenant_id"]] %||% NULL,
    user_id = payload[["custom:aplos_user_id"]] %||% NULL
  )
}

# Human-readable time remaining, used in messages.
.aplos_time_left <- function(exp) {
  minutes <- ceiling((exp - .aplos_now()) / 60)
  if (minutes <= 0) {
    "has expired"
  } else if (minutes == 1L) {
    "expires in 1 minute"
  } else {
    paste0("expires in ", format(minutes, scientific = FALSE), " minutes")
  }
}

.aplos_refresh_hint <- function(session) {
  if (!is.null(session$token_file)) {
    paste0("Update the Aplos NCA Refresh Token in '", session$token_file, "' and run aplos_import_jwt(\"",
           session$token_file, "\").")
  } else {
    "Run aplos_get_jwt() to start a new session."
  }
}

.aplos_expiring_message <- function(session, exp, detail = NULL) {
  msg <- paste0("The Aplos NCA JWT ", .aplos_time_left(exp),
                " and cannot be refreshed automatically.")
  if (!is.null(detail) && nzchar(detail)) msg <- paste0(msg, " Refresh failed: ", detail, ".")
  paste(msg, .aplos_refresh_hint(session))
}

.aplos_expired_message <- function(session, exp, detail = NULL) {
  msg <- paste0("The Aplos NCA JWT ", .aplos_time_left(exp),
                " and could not be refreshed.")
  if (!is.null(detail) && nzchar(detail)) msg <- paste0(msg, " Refresh failed: ", detail, ".")
  paste(msg, .aplos_refresh_hint(session))
}

# Extract a useful error message from a Cognito error response.
.aplos_cognito_error <- function(response) {
  body <- tryCatch(httr::content(response, "text", encoding = "UTF-8"),
                   error = function(e) "")
  message_text <- tryCatch({
    parsed <- jsonlite::fromJSON(body)
    parsed[["message"]] %||% parsed[["__type"]] %||% body
  }, error = function(e) body)
  if (is.null(message_text) || !nzchar(message_text)) {
    message_text <- paste0("HTTP ", httr::status_code(response))
  }
  message_text
}

# Exchange an Aplos NCA Refresh Token for a new Aplos NCA JWT. Returns
# list(token = <id token>, refresh_token = <rotated token or NULL>).
.aplos_exchange_refresh_token <- function(refresh_token, client_id, region) {
  if (is.null(refresh_token) || !nzchar(refresh_token)) {
    stop("An Aplos NCA Refresh Token is required.", call. = FALSE)
  }
  if (is.null(client_id) || !nzchar(client_id)) {
    stop("A Cognito client ID is required to use an Aplos NCA Refresh Token.", call. = FALSE)
  }
  if (is.null(region) || !nzchar(region)) {
    stop("A Cognito region is required to use an Aplos NCA Refresh Token.", call. = FALSE)
  }

  headers <- c("Content-Type" = "application/x-amz-json-1.1",
               "X-Amz-Target" = "AWSCognitoIdentityProviderService.InitiateAuth")
  body <- list(
    AuthFlow = "REFRESH_TOKEN_AUTH",
    AuthParameters = list("REFRESH_TOKEN" = refresh_token),
    ClientId = client_id
  )

  response <- tryCatch(
    httr::POST(
      url = paste0("https://cognito-idp.", region, ".amazonaws.com/"),
      httr::add_headers(.headers = headers),
      body = jsonlite::toJSON(body, auto_unbox = TRUE)
    ),
    error = function(e) stop("Aplos NCA JWT refresh request failed: ", e$message, call. = FALSE)
  )

  if (httr::http_error(response)) {
    stop("Cognito rejected the Aplos NCA Refresh Token: ", .aplos_cognito_error(response),
         call. = FALSE)
  }

  result <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))
  if (!is.null(result$ChallengeName)) {
    stop(paste0("Cognito requested an additional challenge (", result$ChallengeName, ")."),
         call. = FALSE)
  }
  auth <- result$AuthenticationResult
  if (is.null(auth) || is.null(auth$IdToken) || !nzchar(auth$IdToken)) {
    stop("Cognito did not return a new Aplos NCA JWT.", call. = FALSE)
  }

  list(token = auth$IdToken, refresh_token = auth$RefreshToken %||% NULL)
}

.aplos_cognito_refresh <- function(session) {
  .aplos_exchange_refresh_token(
    refresh_token = session$refresh_token,
    client_id = session$client_id,
    region = session$region
  )
}

# Read and validate a JWT from a token file.
.aplos_validate_jwt <- function(token) {
  if (!is.character(token) || length(token) != 1L || is.na(token) || !nzchar(token)) {
    stop("No Aplos NCA JWT was found in the file.", call. = FALSE)
  }
  parts <- strsplit(token, ".", fixed = TRUE)[[1]]
  if (length(parts) != 3L || any(!nzchar(parts))) {
    stop("The value is not a valid Aplos NCA JWT (expected three dot-separated parts).",
         call. = FALSE)
  }
  payload <- tryCatch(AzureAuth::decode_jwt(token)$payload, error = function(e) NULL)
  if (is.null(payload)) {
    stop("The Aplos NCA JWT could not be decoded; check that the file contains a complete Aplos NCA JWT.",
         call. = FALSE)
  }
  token
}

.aplos_strip_token_prefix <- function(value) {
  value <- sub("^Bearer[[:space:]]+", "", value, ignore.case = TRUE)
  value <- sub("^(JWT|ID_TOKEN|TOKEN|REFRESH_TOKEN)[[:space:]]*=[[:space:]]*", "",
               value, ignore.case = TRUE)
  gsub("^[\"']|[\"']$", "", value)
}

# A Aplos NCA Refresh Token is opaque and can contain dots, so do not rely on the
# number of segments alone: only treat a value as a JWT if it decodes and has an
# expiry claim.
.aplos_looks_like_jwt <- function(value) {
  if (!is.character(value) || length(value) != 1L || is.na(value)) {
    return(FALSE)
  }
  parts <- strsplit(value, ".", fixed = TRUE)[[1]]
  if (length(parts) != 3L || any(!nzchar(parts))) {
    return(FALSE)
  }
  payload <- tryCatch(AzureAuth::decode_jwt(value)$payload, error = function(e) NULL)
  !is.null(payload) && !is.null(payload$exp)
}

# Read a token file. Two layouts are supported:
#
#   * positional:  line 1 = Aplos NCA Refresh Token (or a legacy Aplos NCA JWT),
#                  optional line 2 = Aplos NCA Refresh Token
#   * key/value:   JWT=..., REFRESH_TOKEN=..., COGNITO_CLIENT_KEY=..., COGNITO_REGION=...
#
# Returns list(token, refresh_token, client_id, region); absent values are NULL.
# `token` is not validated here because it may be an Aplos NCA Refresh Token.
.aplos_read_token_config <- function(path, warn_perm = TRUE) {
  path <- normalizePath(path.expand(path), winslash = "/", mustWork = FALSE)
  if (!file.exists(path)) {
    stop("Token file not found: ", path, call. = FALSE)
  }
  if (file.access(path, 4L) != 0L) {
    stop("Token file is not readable: ", path, call. = FALSE)
  }
  if (warn_perm && .Platform$OS.type != "windows") {
    mode <- file.info(path)$mode
    if (!is.na(mode) && bitwAnd(as.integer(mode), 63L) != 0L) {
      warning("Token file '", path,
              "' is readable by other users; restrict it with chmod 600.",
              call. = FALSE)
    }
  }

  lines <- trimws(readLines(path, warn = FALSE))
  lines <- lines[nzchar(lines) & !grepl("^#", lines)]
  if (length(lines) == 0L) {
    stop("Token file is empty: ", path, call. = FALSE)
  }

  config <- list(token = NULL, refresh_token = NULL, client_id = NULL, region = NULL)

  if (any(grepl("^[A-Za-z_][A-Za-z0-9_]*[[:space:]]*=", lines))) {
    key_map <- c(
      JWT = "token", ID_TOKEN = "token", TOKEN = "token", APLOS_JWT = "token",
      REFRESH_TOKEN = "refresh_token", COGNITO_REFRESH_TOKEN = "refresh_token",
      COGNITO_CLIENT_KEY = "client_id", COGNITO_CLIENT_ID = "client_id",
      CLIENT_ID = "client_id",
      COGNITO_REGION = "region", REGION = "region", AWS_REGION = "region"
    )
    for (line in lines) {
      key <- toupper(trimws(sub("[[:space:]]*=.*$", "", line)))
      if (!key %in% names(key_map)) next
      value <- trimws(sub("^[^=]*=", "", line))
      value <- gsub("^[\"']|[\"']$", "", value)
      field <- unname(key_map[[key]])
      if (is.null(config[[field]]) && nzchar(value)) {
        config[[field]] <- value
      }
    }
  } else {
    config$token <- .aplos_strip_token_prefix(lines[[1L]])
    if (length(lines) >= 2L) {
      config$refresh_token <- .aplos_strip_token_prefix(lines[[2L]])
    }
  }

  if (!is.null(config$refresh_token) && !nzchar(config$refresh_token)) {
    config$refresh_token <- NULL
  }
  if (!is.null(config$token) && !nzchar(config$token)) {
    config$token <- NULL
  }
  if (is.null(config$token) && is.null(config$refresh_token)) {
    stop("No Aplos NCA JWT or Aplos NCA Refresh Token found in: ", path, call. = FALSE)
  }
  config
}

.aplos_read_token_file <- function(path, warn_perm = TRUE) {
  .aplos_read_token_config(path, warn_perm = warn_perm)$token
}

# Return a token that is safe to use for the next API call, refreshing it when
# it is inside the refresh window.
.aplos_ensure_token <- function(session) {
  claims <- .aplos_token_claims(session$token)
  exp <- claims$exp
  if (is.null(exp)) {
    return(session$token)  # expiry unknown; leave the token alone
  }

  remaining <- exp - .aplos_now()
  if (remaining > .aplos_refresh_window()) {
    return(session$token)
  }

  refreshed <- NULL
  failure <- NULL

  if (!is.null(session$refresh_token) && !is.null(session$client_id) &&
      !is.null(session$region)) {
    refreshed <- tryCatch(.aplos_cognito_refresh(session),
                          error = function(e) {
                            failure <<- conditionMessage(e)
                            NULL
                          })
  } else if (!is.null(session$token_file)) {
    refreshed <- tryCatch({
      # Permissions were already checked when the file was imported; do not
      # warn again on every refreshed call.
      new_token <- .aplos_read_token_file(session$token_file, warn_perm = FALSE)
      if (identical(new_token, session$token) && remaining <= 0) {
        failure <<- "the file still contains the expired token"
        NULL
      } else {
        list(token = new_token, refresh_token = NULL)
      }
    }, error = function(e) {
      failure <<- conditionMessage(e)
      NULL
    })
  } else {
    failure <- "no Aplos NCA Refresh Token or credential file is associated with this Aplos NCA JWT"
  }

  if (is.null(refreshed)) {
    if (remaining <= 0) {
      stop(.aplos_expired_message(session, exp, failure), call. = FALSE)
    }
    if (!isTRUE(session$warned_expiring)) {
      warning(.aplos_expiring_message(session, exp, failure), call. = FALSE)
      session$warned_expiring <- TRUE
      .aplos_state$sessions[[session$id]] <- session
    }
    return(session$token)
  }

  .aplos_session_update(session, refreshed$token, refreshed$refresh_token)

  if (isTRUE(getOption("AplosNCA.debug"))) {
    source <- if (!is.null(session$refresh_token)) "Aplos NCA Refresh Token" else "Aplos NCA credential file re-read"
    message("Aplos NCA JWT refreshed (", source, ").")
  }

  refreshed$token
}

# Build everything an authenticated request needs. This is the single place
# where the JWT is decoded and refreshed.
.aplos_auth_context <- function(token, content_type = "application/json") {
  if (missing(token) || is.null(token) || length(token) != 1L ||
      is.na(token) || !nzchar(token)) {
    stop("A valid Aplos NCA JWT is required.", call. = FALSE)
  }

  session <- .aplos_session_get(token)
  token <- .aplos_ensure_token(session)
  claims <- .aplos_token_claims(token)

  if (is.null(claims$tenant_id) || is.null(claims$user_id)) {
    stop("The Aplos NCA JWT is missing the tenant or user claim. ",
         "Authenticate again with aplos_get_jwt() or import a current token with aplos_import_jwt().",
         call. = FALSE)
  }

  list(
    token = token,
    tenant_id = claims$tenant_id,
    user_id = claims$user_id,
    headers = c("Content-Type" = content_type,
                "Authorization" = paste0("Bearer ", token))
  )
}
