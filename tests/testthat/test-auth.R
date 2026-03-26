library(testthat)
library(httptest2)

test_that("aplos_get_jwt authenticates successfully with mock", {
  # Mock based on redacted_get_jwt.R
  mock_response <- structure(list(url = "https://cognito-idp.us-east-1.amazonaws.com/", 
 status_code = 200L, headers = structure(list(date = "fake_date", 
 `content-type` = "application/x-amz-json-1.1", `content-length` = "fake_length", 
 `x-amzn-requestid` = "fake_request_id"), class = c("insensitive", 
 "list")), all_headers = list(list(status = 200L, version = "HTTP/2", 
 headers = structure(list(date = "fake_date", 
 `content-type` = "application/x-amz-json-1.1", `content-length` = "fake_length", 
 `x-amzn-requestid` = "fake_request_id"), class = c("insensitive", 
 "list")))), cookies = structure(list(domain = logical(0), 
 flag = logical(0), path = logical(0), secure = logical(0), 
 expiration = structure(numeric(0), class = c("POSIXct", 
 "POSIXt")), name = logical(0), value = logical(0)), row.names = integer(0), class = "data.frame"), 
 content = charToRaw('{"AuthenticationResult":{"AccessToken":"fake_token","ExpiresIn":3600,"IdToken":"fake_id_token","RefreshToken":"fake_refresh_token","TokenType":"Bearer"},"ChallengeParameters":{}}'), 
 date = structure(0, class = c("POSIXct", "POSIXt"), tzone = "GMT"), 
 times = c(redirect = 0, namelookup = 0, connect = 0, pretransfer = 0, starttransfer = 0, total = 0)), class = "response")
  
  local_mocked_bindings(
    POST = function(...) mock_response,
    .package = "httr"
  )
  
  token <- aplos_get_jwt(client_id = "test_id", username = "test_user", password = "test_pass", region = "us-east-1")
  expect_type(token, "character")
  expect_equal(token, "fake_id_token")
})

test_that("aplos_get_jwt errors on missing params", {
  expect_error(aplos_get_jwt(client_id = "id", username = "user", password = "pass"), "All parameters.*required")
})
