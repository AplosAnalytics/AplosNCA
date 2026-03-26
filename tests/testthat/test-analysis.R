library(testthat)
library(httptest2)
library(jsonlite)  # For JSON in mocks
library(AzureAuth)  # For mocking decode_jwt

test_that("aplos_execute_analysis submits successfully with mock", {
  # Mock response based on redacted_execute_analysis.R (full httr structure with status_code)
  mock_result <- list(data = list(executionId = "fake_execution_id"))
  mock_response <- structure(
    list(
      url = "https://api.dev.aplos-nca.com/v3/tenants/fake_tenant/users/fake_user/analysis/queue",
      status_code = 201L,  # Fixed: Added status_code to avoid http_status error
      headers = structure(
        list(
          date = "fake_date",
          `content-type` = "application/json",
          `content-length` = "fake_length",
          `x-amzn-requestid` = "fake_request_id"
        ),
        class = c("insensitive", "list")
      ),
      all_headers = list(
        list(
          status = 201L,
          version = "HTTP/2",
          headers = structure(
            list(
              date = "fake_date",
              `content-type` = "application/json",
              `content-length` = "fake_length",
              `x-amzn-requestid` = "fake_request_id"
            ),
            class = c("insensitive", "list")
          )
        )
      ),
      cookies = structure(
        list(
          domain = logical(0),
          flag = logical(0),
          path = logical(0),
          secure = logical(0),
          expiration = structure(numeric(0), class = c("POSIXct", "POSIXt")),
          name = logical(0),
          value = logical(0)
        ),
        row.names = integer(0),
        class = "data.frame"
      ),
      content = charToRaw(toJSON(mock_result, auto_unbox = TRUE)),
      date = structure(0, class = c("POSIXct", "POSIXt"), tzone = "GMT"),
      times = c(redirect = 0, namelookup = 0, connect = 0, pretransfer = 0, starttransfer = 0, total = 0)
    ),
    class = "response"
  )

  # Mock decode_jwt for tenant/user IDs
  local_mocked_bindings(
    decode_jwt = function(...) list(payload = list(`custom:aplos_user_tenant_id` = "fake_tenant", `custom:aplos_user_id` = "fake_user")),
    .package = "AzureAuth"
  )

  # Mock POST
  local_mocked_bindings(
    POST = function(...) mock_response,
    .package = "httr"
  )

  # Mock upload result (as per function requirement)
  mock_upload <- list(data = list(fileId = "fake_file_id"))

  exec_id <- aplos_execute_analysis(
    result = mock_upload,
    analysis = '{"key": "value"}',
    url = "https://api.example.com",
    token = "fake_token"
  )
  expect_equal(exec_id, "fake_execution_id")
})

test_that("aplos_execute_analysis errors on missing required params", {
  expect_error(aplos_execute_analysis(result = list(), url = "api.com"), "Required parameters.*missing")
})

test_that("aplos_execute_analysis errors if no optional params provided", {
  mock_upload <- list(data = list(fileId = "fake_file_id"))
  expect_error(aplos_execute_analysis(result = mock_upload, url = "api.com", token = "fake_token"), "At least one of.*must be provided")
})
