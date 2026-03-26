library(testthat)
library(httptest2)
library(jsonlite)  # For JSON in mocks

test_that("aplos_get_upload_url retrieves URL with mock", {
  # Mock response based on redacted_get_upload_url.R (proper httr structure)
  mock_response <- structure(
    list(
      url = "https://api.dev.aplos-nca.com/v3/tenants/fake_tenant/users/fake_user/files",
      status_code = 200L,
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
          status = 200L,
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
      content = charToRaw(toJSON(list(
        data = list(
          url = "fake_upload_url",
          fields = list(
            key = "fake_key",
            xAmzAlgorithm = "fake_algorithm",
            xAmzCredential = "fake_credential",
            xAmzDate = "fake_date",
            xAmzSecurityToken = "fake_token",
            policy = "fake_policy",
            xAmzSignature = "fake_signature"
          )
        )
      ), auto_unbox = TRUE)),
      date = structure(0, class = c("POSIXct", "POSIXt"), tzone = "GMT"),
      times = c(redirect = 0, namelookup = 0, connect = 0, pretransfer = 0, starttransfer = 0, total = 0)
    ),
    class = "response"
  )

  local_mocked_bindings(
    POST = function(...) mock_response,
    .package = "httr"
  )

  result <- aplos_get_upload_url(
    input_file = "fake_file.csv",
    url = "https://api.example.com",
    token = "fake_token"
  )
  expect_type(result, "list")
  expect_equal(result$data$url, "fake_upload_url")
  expect_true("fields" %in% names(result$data))
})

test_that("aplos_upload_file uploads with mock", {
  # Create a real temp file for the test (to satisfy file.exists)
  temp_file <- tempfile(fileext = ".csv")
  writeLines("dummy,data\n1,2", temp_file)
  on.exit(unlink(temp_file))  # Clean up after test

  # Mock result from get_upload_url
  mock_result <- list(
    data = list(
      url = "fake_upload_url",
      fields = list(key = "fake_key")  # Minimal fields
    )
  )

  # Mock POST to simulate successful upload (returns 204)
  local_mocked_bindings(
    POST = function(url, body, ...) {
      expect_equal(url, "fake_upload_url")
      structure(list(status_code = 204L), class = "response")  # Success, no content
    },
    .package = "httr"
  )

  expect_silent(aplos_upload_file(input_file = temp_file, result = mock_result, token = "fake_token"))
})

test_that("aplos_get_upload_url errors on missing params", {
  expect_error(aplos_get_upload_url(input_file = "test.csv", url = "api.com"), "All parameters.*required")
})
