library(testthat)
library(jsonlite)
library(withr)

# Build a minimal httr response object for mocking.
mock_response <- function(status_code = 200L, body = "{}") {
  structure(
    list(
      url = "https://cognito-idp.us-east-1.amazonaws.com/",
      status_code = status_code,
      headers = structure(
        list(`content-type` = "application/x-amz-json-1.1"),
        class = c("insensitive", "list")
      ),
      content = charToRaw(body),
      date = structure(0, class = c("POSIXct", "POSIXt"), tzone = "GMT"),
      times = c(redirect = 0, namelookup = 0, connect = 0, pretransfer = 0,
                starttransfer = 0, total = 0)
    ),
    class = "response"
  )
}

# decode_jwt mock: returns claims and expiry based on the token string.
mock_claims <- function(exp, tenant = "tenant-1", user = "user-1") {
  list(payload = list(
    exp = exp,
    `custom:aplos_user_tenant_id` = tenant,
    `custom:aplos_user_id` = user
  ))
}

test_that("aplos_import_jwt reads a token file and registers the token", {
  AplosNCA:::.aplos_state_reset()
  token <- "header.payload.signature"
  path <- local_tempfile(fileext = ".token")
  writeLines(paste0("Bearer ", token), path)
  Sys.chmod(path, mode = "0600")

  local_mocked_bindings(
    decode_jwt = function(...) mock_claims(as.numeric(Sys.time()) + 3600),
    .package = "AzureAuth"
  )

  expect_message(imported <- aplos_import_jwt(path), "Imported Aplos NCA token")
  expect_identical(imported, token)

  context <- AplosNCA:::.aplos_auth_context(token)
  expect_equal(context$token, token)
  expect_equal(context$tenant_id, "tenant-1")
  expect_equal(context$user_id, "user-1")
  expect_equal(context$headers[["Authorization"]], paste0("Bearer ", token))
})

test_that("aplos_import_jwt uses APLOS_JWT_FILE when no path is given", {
  AplosNCA:::.aplos_state_reset()
  path <- local_tempfile(fileext = ".token")
  writeLines("a.b.c", path)

  local_mocked_bindings(
    decode_jwt = function(...) mock_claims(as.numeric(Sys.time()) + 3600),
    .package = "AzureAuth"
  )
  local_envvar(APLOS_JWT_FILE = path)

  expect_message(token <- aplos_import_jwt(), "Imported Aplos NCA token")
  expect_identical(token, "a.b.c")
})

test_that("aplos_import_jwt rejects expired, missing, and malformed tokens", {
  AplosNCA:::.aplos_state_reset()

  expired_path <- local_tempfile(fileext = ".token")
  writeLines("a.b.c", expired_path)
  local_mocked_bindings(
    decode_jwt = function(...) mock_claims(as.numeric(Sys.time()) - 60),
    .package = "AzureAuth"
  )
  expect_error(aplos_import_jwt(expired_path), "expired at")

  expect_error(aplos_import_jwt(tempfile()), "Token file not found")

  bad_path <- local_tempfile(fileext = ".token")
  writeLines("not-a-jwt", bad_path)
  expect_error(aplos_import_jwt(bad_path), "not a valid JWT")
})

test_that("tokens inside the refresh window are refreshed transparently", {
  AplosNCA:::.aplos_state_reset()
  old_token <- "old.token.value"
  new_token <- "new.token.value"
  old_expiry <- as.numeric(Sys.time()) + 600  # 10 minutes left

  local_mocked_bindings(
    decode_jwt = function(token, ...) {
      if (identical(token, new_token)) {
        mock_claims(as.numeric(Sys.time()) + 3600)
      } else {
        mock_claims(old_expiry)
      }
    },
    .package = "AzureAuth"
  )

  AplosNCA:::.aplos_session_register(
    old_token,
    refresh_token = "refresh-1",
    client_id = "client-1",
    region = "us-east-1"
  )

  refresh_calls <- 0L
  local_mocked_bindings(
    POST = function(...) {
      refresh_calls <<- refresh_calls + 1L
      mock_response(200L, toJSON(list(
        AuthenticationResult = list(
          IdToken = new_token,
          AccessToken = "access-token",
          ExpiresIn = 3600,
          TokenType = "Bearer"
        )
      ), auto_unbox = TRUE))
    },
    .package = "httr"
  )

  context <- AplosNCA:::.aplos_auth_context(old_token)
  expect_equal(context$token, new_token)
  expect_equal(refresh_calls, 1L)

  # The old string still works and now resolves to the refreshed token without
  # another network call.
  context2 <- AplosNCA:::.aplos_auth_context(old_token)
  expect_equal(context2$token, new_token)
  expect_equal(refresh_calls, 1L)
})

test_that("tokens outside the refresh window are left alone", {
  AplosNCA:::.aplos_state_reset()
  token <- "steady.token.value"

  local_mocked_bindings(
    decode_jwt = function(...) mock_claims(as.numeric(Sys.time()) + 1800),
    .package = "AzureAuth"
  )

  AplosNCA:::.aplos_session_register(
    token,
    refresh_token = "refresh-1",
    client_id = "client-1",
    region = "us-east-1"
  )

  refresh_calls <- 0L
  local_mocked_bindings(
    POST = function(...) {
      refresh_calls <<- refresh_calls + 1L
      mock_response(200L, "{}")
    },
    .package = "httr"
  )

  context <- AplosNCA:::.aplos_auth_context(token)
  expect_equal(context$token, token)
  expect_equal(refresh_calls, 0L)
})

test_that("a failed refresh warns while the token is still valid", {
  AplosNCA:::.aplos_state_reset()
  token <- "near.expiry.value"

  local_mocked_bindings(
    decode_jwt = function(...) mock_claims(as.numeric(Sys.time()) + 600),
    .package = "AzureAuth"
  )

  AplosNCA:::.aplos_session_register(
    token,
    refresh_token = "stale-refresh",
    client_id = "client-1",
    region = "us-east-1"
  )

  local_mocked_bindings(
    POST = function(...) mock_response(
      400L,
      toJSON(list(
        `__type` = "NotAuthorizedException",
        message = "Refresh Token has expired"
      ), auto_unbox = TRUE)
    ),
    .package = "httr"
  )

  expect_warning(
    context <- AplosNCA:::.aplos_auth_context(token),
    "cannot be refreshed automatically"
  )
  expect_equal(context$token, token)
})

test_that("a failed refresh on an expired token raises an actionable error", {
  AplosNCA:::.aplos_state_reset()
  token <- "expired.token.value"

  local_mocked_bindings(
    decode_jwt = function(...) mock_claims(as.numeric(Sys.time()) - 60),
    .package = "AzureAuth"
  )

  AplosNCA:::.aplos_session_register(
    token,
    refresh_token = "stale-refresh",
    client_id = "client-1",
    region = "us-east-1"
  )

  local_mocked_bindings(
    POST = function(...) mock_response(
      400L,
      toJSON(list(
        `__type` = "NotAuthorizedException",
        message = "Refresh Token has expired"
      ), auto_unbox = TRUE)
    ),
    .package = "httr"
  )

  expect_error(AplosNCA:::.aplos_auth_context(token), "Run aplos_get_jwt\\(\\)")
})

test_that("tokens imported from a file are re-read when close to expiry", {
  AplosNCA:::.aplos_state_reset()
  old_token <- "old.file.token"
  new_token <- "new.file.token"
  old_expiry <- as.numeric(Sys.time()) + 300

  path <- local_tempfile(fileext = ".token")
  writeLines(old_token, path)

  local_mocked_bindings(
    decode_jwt = function(token, ...) {
      if (identical(token, new_token)) {
        mock_claims(as.numeric(Sys.time()) + 3600)
      } else {
        mock_claims(old_expiry)
      }
    },
    .package = "AzureAuth"
  )

  AplosNCA:::.aplos_session_register(old_token, token_file = path)

  # Simulate another tool refreshing the token file.
  writeLines(new_token, path)

  context <- AplosNCA:::.aplos_auth_context(old_token)
  expect_equal(context$token, new_token)
})

test_that("a stale token file produces a message telling the user what to do", {
  AplosNCA:::.aplos_state_reset()
  token <- "stale.file.token"

  path <- local_tempfile(fileext = ".token")
  writeLines(token, path)

  local_mocked_bindings(
    decode_jwt = function(...) mock_claims(as.numeric(Sys.time()) - 60),
    .package = "AzureAuth"
  )

  AplosNCA:::.aplos_session_register(token, token_file = path)

  expect_error(AplosNCA:::.aplos_auth_context(token), "aplos_import_jwt")
})

test_that("aplos_get_jwt stores the refresh token for later use", {
  AplosNCA:::.aplos_state_reset()
  local_mocked_bindings(
    decode_jwt = function(...) mock_claims(as.numeric(Sys.time()) + 3600),
    .package = "AzureAuth"
  )
  local_mocked_bindings(
    POST = function(...) mock_response(200L, toJSON(list(
      AuthenticationResult = list(
        IdToken = "login.id.token",
        AccessToken = "access-token",
        RefreshToken = "login.refresh.token",
        ExpiresIn = 3600,
        TokenType = "Bearer"
      )
    ), auto_unbox = TRUE)),
    .package = "httr"
  )

  token <- aplos_get_jwt("client-1", "user", "pass", "us-east-1")
  expect_equal(token, "login.id.token")

  session <- AplosNCA:::.aplos_session_get(token)
  expect_equal(session$refresh_token, "login.refresh.token")
  expect_equal(session$client_id, "client-1")
  expect_equal(session$region, "us-east-1")
})

test_that("token files readable by other users produce a warning", {
  skip_on_os("windows")
  AplosNCA:::.aplos_state_reset()
  path <- local_tempfile(fileext = ".token")
  writeLines("a.b.c", path)
  Sys.chmod(path, mode = "0644")

  local_mocked_bindings(
    decode_jwt = function(...) mock_claims(as.numeric(Sys.time()) + 3600),
    .package = "AzureAuth"
  )

  expect_warning(aplos_import_jwt(path), "readable by other users")
})

test_that("imported token file paths are stored as absolute paths", {
  AplosNCA:::.aplos_state_reset()
  token <- "relative.path.token"
  path <- local_tempfile(fileext = ".token")
  writeLines(token, path)

  local_mocked_bindings(
    decode_jwt = function(...) mock_claims(as.numeric(Sys.time()) + 3600),
    .package = "AzureAuth"
  )

  local_dir(dirname(path))
  expect_message(aplos_import_jwt(basename(path)), "Imported Aplos NCA token")

  session <- AplosNCA:::.aplos_session_get(token)
  expect_equal(session$token_file, normalizePath(path, winslash = "/"))
})

test_that("refreshing an imported token does not repeat the permission warning", {
  skip_on_os("windows")
  AplosNCA:::.aplos_state_reset()
  token <- "perm.warning.token"
  path <- local_tempfile(fileext = ".token")
  writeLines(token, path)
  Sys.chmod(path, mode = "0644")

  local_mocked_bindings(
    decode_jwt = function(...) mock_claims(as.numeric(Sys.time()) + 3600),
    .package = "AzureAuth"
  )
  local_options(list(AplosNCA.refresh_window_seconds = 7200))

  expect_warning(
    expect_message(aplos_import_jwt(path), "Imported Aplos NCA token"),
    "chmod 600"
  )
  expect_silent(AplosNCA:::.aplos_auth_context(token))
})
