library(testthat)
library(httptest2)
library(jsonlite)  # For JSON in mocks
library(AzureAuth)  # For mocking decode_jwt

test_that("aplos_execution_status polls until succeeded with mock", {
  # Mock response based on redacted_execution_status.json (full httr structure)
  mock_result <- list(data = list(status = "succeeded"))
  mock_response <- structure(
    list(
      url = "https://api.example.com/v3/tenants/fake_tenant/users/fake_user/executions/fake_id/status",  # Fixed: Valid character url
      status_code = 200L,  # Added for http_status checks if present
      headers = structure(
        list(
          date = "fake_date",
          `content-type` = "application/json",
          `content-length` = "fake_length",
          `x-amzn-requestid` = "fake_request_id"
        ),
        class = c("insensitive", "list")
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
  
  # Mock GET (simulates polling by always returning "succeeded")
  local_mocked_bindings(
    GET = function(...) mock_response,
    .package = "httr"
  )
  
  result <- aplos_execution_status(url = "https://api.example.com", token = "fake_token", execution_id = "fake_id")
  expect_type(result, "list")
  expect_equal(result$data$status, "succeeded")
})

test_that("aplos_execution_status errors on missing params", {
  expect_error(aplos_execution_status(url = "api.com", token = "token"), "All parameters.*required")
})
